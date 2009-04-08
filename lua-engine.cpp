#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <string.h>
//#include <unistd.h> // for unlink
#include <ctype.h>
#include <assert.h>
#include <math.h>
#include <direct.h>

#ifdef __linux
#include <sys/types.h>
#include <sys/wait.h>
#endif


#include "port.h"
#include "snes9x.h"
#include "memmap.h"
#include "display.h"
#include "messages.h"
#include "movie.h"
#include "snapshot.h"
#include "gfx.h"
#include "ppu.h"

extern "C" {
	#include "lua.h"
	#include "lauxlib.h"
	#include "lualib.h"
	#include "lstate.h"
}

#include "s9xlua.h"
#include "luasav.h"

static lua_State *LUA;

// Are we running any code right now?
static char *luaScriptName = NULL;

// Are we running any code right now?
static bool8 luaRunning = FALSE;

// True at the frame boundary, false otherwise.
static bool8 frameBoundary = FALSE;


// The execution speed we're running at.
static enum {SPEED_NORMAL, SPEED_NOTHROTTLE, SPEED_TURBO, SPEED_MAXIMUM} speedmode = SPEED_NORMAL;

// Rerecord count skip mode
static bool8 skipRerecords = FALSE;

// Used by the registry to find our functions
static const char *frameAdvanceThread = "S9X.FrameAdvance";
static const char *memoryWatchTable = "S9X.Memory";
static const char *guiCallbackTable = "S9X.GUI";

// True if there's a thread waiting to run after a run of frame-advance.
static bool8 frameAdvanceWaiting = FALSE;

// We save our pause status in the case of a natural death.
static bool8 wasPaused = FALSE;

// Transparency strength. 255=opaque, 0=so transparent it's invisible
static uint8 transparencyModifier = 255;

// Our joypads.
static uint32 lua_joypads[5];
static uint8 lua_joypads_used = 0;


// NON-static. This is a listing of memory addresses we're watching as bitfields
// in order to speed up emulation, at the cost of memory usage. the SNES has
// 128 KB of main RAM, 128 * 1024 / 8 = 16384 entries with bit-level precision.
unsigned char lua_watch_bitfield[16384];


static bool8 gui_used = FALSE;
static bool8 gui_enabled = TRUE;
static uint8 *gui_data = NULL; // BGRA


// Protects Lua calls from going nuts.
// We set this to a big number like 1000 and decrement it
// over time. The script gets knifed once this reaches zero.
static int numTries;


// Look in snes9x.h for macros named like SNES9X_UP_MASK to determine the order.
static const char *button_mappings[] = {
	"","b1","b2","b3", // These are normally unused, but supported for custom input display
	
	"R", "L", "X", "A", "right", "left", "down", "up", "start", "select", "Y", "B"
};

static const char* luaCallIDStrings [] =
{
	"CALL_BEFOREEMULATION",
	"CALL_AFTEREMULATION",
	"CALL_BEFOREEXIT",
};
static const int _makeSureWeHaveTheRightNumberOfStrings [sizeof(luaCallIDStrings)/sizeof(*luaCallIDStrings) == LUACALL_COUNT ? 1 : 0];

INLINE void S9xSetDWord (uint32 DWord, uint32 Address);
INLINE uint32 S9xGetDWord (uint32 Address);

INLINE uint32 S9xGetDWord (uint32 Address)
{
	bool free = true;

	return S9xGetWord(Address, free) | (S9xGetWord(Address+2, free) << 16);
}

INLINE void S9xSetDWord (uint32 DWord, uint32 Address)
{
	bool free = true;

	S9xSetWordWrapped(DWord & 0xffff, Address, free);
	S9xSetWordWrapped(DWord >> 16, Address+2, free);
	S9xLuaWriteInform(Address);
}


/**
 * Resets emulator speed / pause states after script exit.
 */
static void S9xLuaOnStop() {
	luaRunning = FALSE;
	lua_joypads_used = 0;
	gui_used = false;
	if (wasPaused)
		Settings.Paused = TRUE;
	memset(lua_watch_bitfield, 0, sizeof(lua_watch_bitfield));
}

/**
 * Asks Lua if it wants control of the emulator's speed.
 * Returns 0 if no, 1 if yes. If yes, we also tamper with the
 * IPPU's settings for speed ourselves, so the calling code
 * need not do anything.
 */
int S9xLuaSpeed() {
	if (!LUA || !luaRunning)
		return 0;

	//printf("%d\n", speedmode);

	switch (speedmode) {
	case SPEED_NORMAL:
		return 0;
	case SPEED_NOTHROTTLE:
		IPPU.RenderThisFrame = TRUE;
		return 1;

	case SPEED_TURBO:
		IPPU.SkippedFrames++;
		if (IPPU.SkippedFrames >= 40) {
			IPPU.SkippedFrames = 0;
			IPPU.RenderThisFrame = TRUE;
		}
		else
			IPPU.RenderThisFrame = FALSE;
		return 1;

	// In mode 3, SkippedFrames is set to zero so that the frame
	// skipping code doesn't try anything funny.
	case SPEED_MAXIMUM:
		IPPU.SkippedFrames=0;
		IPPU.RenderThisFrame = FALSE;
		return 1;

	default:
		assert(false);
		return 0;
	
	}
}

/**
 * When code determines that we want to inform Lua that a write has occurred,
 * we call this to invoke code.
 *
 */
void S9xLuaWrite(uint32 addr) {
	// Nuke the stack, just in case.
	lua_settop(LUA,0);

	lua_getfield(LUA, LUA_REGISTRYINDEX, memoryWatchTable);
	lua_pushinteger(LUA,addr);
	lua_gettable(LUA, 1);

	// Invoke the Lua
	numTries = 1000;
	int res = lua_pcall(LUA, 0, 0, 0);
	if (res) {
		const char *err = lua_tostring(LUA, -1);
		
#ifdef __WIN32__
		MessageBox(GUI.hWnd, err, "Lua Engine", MB_OK);
#else
		fprintf(stderr, "Lua error: %s\n", err);
#endif
		
	}
	lua_settop(LUA,0);
}

extern "C" { 

void S9xLuaWriteCheck(uint32 address) {
	// Crazy problem: memory maps are pretty f**ked up.
	
	int block = address >> MEMMAP_SHIFT;
	int newaddr;
	if (Memory.Map[block] == Memory.Map[0x7e0])
		newaddr = address & 0xffff;
	else if (Memory.Map[block] == Memory.Map[0x7f0])
		newaddr = (address & 0xffff) + 65536;
	else
		// Not in RAM
		return;
	
	// These might be better using binary arithmatic -- a shift and an AND
	int slot = newaddr / 8;
	int bits = newaddr % 8;
	

	
	extern unsigned char lua_watch_bitfield[16384];
	
	// Check the slot
	if (lua_watch_bitfield[slot] & (1 << bits))
		S9xLuaWrite(newaddr + 0x7e0000);


}

}
///////////////////////////



// snes9x.speedmode(string mode)
//
//   Takes control of the emulation speed
//   of the system. Normal is normal speed (60fps, 50 for PAL),
//   nothrottle disables speed control but renders every frame,
//   turbo renders only a few frames in order to speed up emulation,
//   maximum renders no frames
static int snes9x_speedmode(lua_State *L) {
	const char *mode = luaL_checkstring(L,1);
	
	if (strcasecmp(mode, "normal")==0) {
		speedmode = SPEED_NORMAL;
	} else if (strcasecmp(mode, "nothrottle")==0) {
		speedmode = SPEED_NOTHROTTLE;
	} else if (strcasecmp(mode, "turbo")==0) {
		speedmode = SPEED_TURBO;
	} else if (strcasecmp(mode, "maximum")==0) {
		speedmode = SPEED_MAXIMUM;
	} else
		luaL_error(L, "Invalid mode %s to snes9x.speedmode",mode);
	
	//printf("new speed mode:  %d\n", speedmode);

	return 0;

}


// snes9x.frameadvnace()
//
//  Executes a frame advance. Occurs by yielding the coroutine, then re-running
//  when we break out.
static int snes9x_frameadvance(lua_State *L) {
	// We're going to sleep for a frame-advance. Take notes.

	if (frameAdvanceWaiting) 
		return luaL_error(L, "can't call snes9x.frameadvance() from here");

	frameAdvanceWaiting = TRUE;

	// Don't do this! The user won't like us sending their emulator out of control!
//	Settings.FrameAdvance = TRUE;
	
	// Now we can yield to the main 
	return lua_yield(L, 0);


	// It's actually rather disappointing...
}


// snes9x.pause()
//
//  Pauses the emulator, function "waits" until the user unpauses.
//  This function MAY be called from a non-frame boundary, but the frame
//  finishes executing anwyays. In this case, the function returns immediately.
static int snes9x_pause(lua_State *L) {
	
	Settings.Paused = TRUE;
	speedmode = SPEED_NORMAL;

	// Return control if we're midway through a frame. We can't pause here.
	if (frameAdvanceWaiting) {
		return 0;
	}

	// If it's on a frame boundary, we also yield.	
	frameAdvanceWaiting = TRUE;
	return lua_yield(L, 0);
	
}

static int snes9x_registerbefore(lua_State *L) {
	if (!lua_isnil(L,1))
		luaL_checktype(L, 1, LUA_TFUNCTION);
	lua_settop(L,1);
	lua_getfield(L, LUA_REGISTRYINDEX, luaCallIDStrings[LUACALL_BEFOREEMULATION]);
	lua_insert(L,1);
	lua_setfield(L, LUA_REGISTRYINDEX, luaCallIDStrings[LUACALL_BEFOREEMULATION]);
	//StopScriptIfFinished(luaStateToUIDMap[L]);
	return 1;
}

static int snes9x_registerafter(lua_State *L) {
	if (!lua_isnil(L,1))
		luaL_checktype(L, 1, LUA_TFUNCTION);
	lua_settop(L,1);
	lua_getfield(L, LUA_REGISTRYINDEX, luaCallIDStrings[LUACALL_AFTEREMULATION]);
	lua_insert(L,1);
	lua_setfield(L, LUA_REGISTRYINDEX, luaCallIDStrings[LUACALL_AFTEREMULATION]);
	//StopScriptIfFinished(luaStateToUIDMap[L]);
	return 1;
}

static int snes9x_registerexit(lua_State *L) {
	if (!lua_isnil(L,1))
		luaL_checktype(L, 1, LUA_TFUNCTION);
	lua_settop(L,1);
	lua_getfield(L, LUA_REGISTRYINDEX, luaCallIDStrings[LUACALL_BEFOREEXIT]);
	lua_insert(L,1);
	lua_setfield(L, LUA_REGISTRYINDEX, luaCallIDStrings[LUACALL_BEFOREEXIT]);
	//StopScriptIfFinished(luaStateToUIDMap[L]);
	return 1;
}

// snes9x.message(string msg)
//
//  Displays the given message on the screen.
static int snes9x_message(lua_State *L) {

	const char *msg = luaL_checkstring(L,1);
	static char msgBuf[1024];
	strcpy(msgBuf, msg);
	S9xMessage(S9X_INFO, S9X_MOVIE_INFO, msgBuf);
	
	return 0;

}


static int memory_readbyte(lua_State *L)
{
	lua_pushinteger(L, S9xGetByte(luaL_checkinteger(L,1), true));
	return 1;
}

static int memory_readbytesigned(lua_State *L) {
	signed char c = (signed char) S9xGetByte(luaL_checkinteger(L,1), true);
	lua_pushinteger(L, c);
	return 1;
}

static int memory_readword(lua_State *L)
{
	lua_pushinteger(L, S9xGetWord(luaL_checkinteger(L,1), true));
	return 1;
}

static int memory_readwordsigned(lua_State *L) {
	signed short c = (signed short) S9xGetWord(luaL_checkinteger(L,1), true);
	lua_pushinteger(L, c);
	return 1;
}

static int memory_readdword(lua_State *L)
{
	uint32 addr = luaL_checkinteger(L,1);
	uint32 val = S9xGetDWord(addr);

	// lua_pushinteger doesn't work properly for 32bit system, does it?
	if (val >= 0x80000000 && sizeof(int) <= 4)
		lua_pushnumber(L, val);
	else
		lua_pushinteger(L, val);
	return 1;
}

static int memory_readdwordsigned(lua_State *L) {
	uint32 addr = luaL_checkinteger(L,1);
	int32 val = (signed) S9xGetDWord(addr);

	lua_pushinteger(L, val);
	return 1;
}

static int memory_readbyterange(lua_State *L) {
	uint32 address = luaL_checkinteger(L,1);
	int length = luaL_checkinteger(L,2);

	if(length < 0)
	{
		address += length;
		length = -length;
	}

	// push the array
	lua_createtable(L, abs(length), 0);

	// put all the values into the (1-based) array
	for(int a = address, n = 1; n <= length; a++, n++)
	{
		unsigned char value = S9xGetByte(a, true);
		lua_pushinteger(L, value);
		lua_rawseti(L, -2, n);
	}

	return 1;
}

static int memory_writebyte(lua_State *L)
{
	S9xSetByte(luaL_checkinteger(L,2), luaL_checkinteger(L,1), true);
	return 0;
}

static int memory_writeword(lua_State *L)
{
	S9xSetWord(luaL_checkinteger(L,2), luaL_checkinteger(L,1), true);
	return 0;
}

static int memory_writedword(lua_State *L)
{
	S9xSetDWord(luaL_checkinteger(L,2), luaL_checkinteger(L,1));
	return 0;
}


// memory.registerwrite(int address, function func)
//
//  Calls the given function when the indicated memory address is
//  written to. No args are given to the function. The write has already
//  occurred, so the new address is readable.
static int memory_registerwrite(lua_State *L) {

	// Check args
	unsigned int addr = luaL_checkinteger(L, 1);
	if (lua_type(L,2) != LUA_TNIL && lua_type(L,2) != LUA_TFUNCTION)
		luaL_error(L, "function or nil expected in arg 2 to memory.register");
	
	
	// Check the address range
	if (addr < 0x7e0000 || addr > 0x7fffff)	
		luaL_error(L, "arg 1 should be between 0x7e0000 and 0x7fffff");

	// Commit it to the registery
	lua_getfield(L, LUA_REGISTRYINDEX, memoryWatchTable);
	lua_pushvalue(L,1);
	lua_pushvalue(L,2);
	lua_settable(L, -3);
	
	addr -= 0x7e0000;

	// Now, set the bitfield accordingly
	if (lua_type(L,2) == LUA_TNIL) {
		lua_watch_bitfield[addr / 8] &= ~(1 << (addr & 7));	
	} else {
		lua_watch_bitfield[addr / 8] |= 1 << (addr & 7);
	
	}
	
	return 0;
}


// table joypad.get(int which = 1)
//
//  Reads the joypads as inputted by the user.
//  This is really the only way to get input to the system.
static int joypad_get(lua_State *L) {

	// Reads the joypads as inputted by the user
	int which = luaL_checkinteger(L,1);
	
	if (which < 1 || which > 5) {
		luaL_error(L,"Invalid input port (valid range 1-5, specified %d)", which);
	}
	
	extern uint16 S9xReadJoypadNext(int which);
	uint32 buttons = S9xReadJoypadNext(which - 1);
	
	lua_newtable(L);
	
	int i;
	for (i = 1; i < 16; i++) {
		if (buttons & (1<<i)) {
			lua_pushinteger(L,1);
			lua_setfield(L, -2, button_mappings[i]);
		}
	}
	
	return 1;
}


// joypad.set(int which, table buttons)
//
//   Sets the given buttons to be pressed during the next
//   frame advance. The table should have the right 
//   keys (no pun intended) set.
static int joypad_set(lua_State *L) {

	// Which joypad we're tampering with
	int which = luaL_checkinteger(L,1);
	if (which < 1 || which > 5) {
		luaL_error(L,"Invalid output port (valid range 1-5, specified %d)", which);

	}

	// And the table of buttons.
	luaL_checktype(L,2,LUA_TTABLE);

	// Set up for taking control of the indicated controller
	lua_joypads_used |= 1 << (which-1);
	lua_joypads[which-1] = 0;

	int i;
	for (i=1; i < 16; i++) {
		lua_getfield(L, 2, button_mappings[i]);
		if (!lua_isnil(L,-1))
			lua_joypads[which-1] |= 1 << i;
		lua_pop(L,1);
	}
	
	return 0;
}




// Helper function to convert a savestate object to the filename it represents.
static const char *savestateobj2filename(lua_State *L, int offset) {
	
	// First we get the metatable of the indicated object
	int result = lua_getmetatable(L, offset);

	if (!result)
		luaL_error(L, "object not a savestate object");
	
	// Also check that the type entry is set
	lua_getfield(L, -1, "__metatable");
	if (strcmp(lua_tostring(L,-1), "snes9x Savestate") != 0)
		luaL_error(L, "object not a savestate object");
	lua_pop(L,1);
	
	// Now, get the field we want
	lua_getfield(L, -1, "filename");
	
	// Return it
	return lua_tostring(L, -1);
}


// Helper function for garbage collection.
static int savestate_gc(lua_State *L) {
	// The object we're collecting is on top of the stack
	lua_getmetatable(L,1);
	
	// Get the filename
	const char *filename;
	lua_getfield(L, -1, "filename");
	filename = lua_tostring(L,-1);

	// Delete the file
	remove(filename);

	// Also delete the .luasav file if we saved one of those with it
	char luaSaveFilename [512];
	strncpy(luaSaveFilename, filename, 512);
	luaSaveFilename[512-(1+7/*strlen(".luasav")*/)] = '\0';
	strcat(luaSaveFilename, ".luasav");
	remove(luaSaveFilename);

	// We exit, and the garbage collector takes care of the rest.
	return 0;
}

// object savestate.create(int which = nil)
//
//  Creates an object used for savestates.
//  The object can be associated with a player-accessible savestate
//  ("which" between 1 and 12) or not (which == nil).
static int savestate_create(lua_State *L) {
	int which = -1;
	if (lua_gettop(L) >= 1) {
		which = luaL_checkinteger(L, 1);
		if (which < 1 || which > 12) {
			luaL_error(L, "invalid player's savestate %d", which);
		}
	}
	

	char *filename;

	if (which > 0) {
		// Find an appropriate filename. This is OS specific, unfortunately.
		// So I turned the filename selection code into my bitch. :)
		// Numbers are 0 through 9 though.
		filename = S9xGetFreezeFilename(which - 1);
	}
	else {
		filename = tempnam(NULL, "snlua");
	}
	
	// Our "object". We don't care about the type, we just need the memory and GC services.
	lua_newuserdata(L,1);
	
	// The metatable we use, protected from Lua and contains garbage collection info and stuff.
	lua_newtable(L);
	
	// First, we must protect it
	lua_pushstring(L, "snes9x Savestate");
	lua_setfield(L, -2, "__metatable");
	
	
	// Now we need to save the file itself.
	lua_pushstring(L, filename);
	lua_setfield(L, -2, "filename");
	
	// If it's an anonymous savestate, we must delete the file from disk should it be gargage collected
	if (which < 0) {
		lua_pushcfunction(L, savestate_gc);
		lua_setfield(L, -2, "__gc");
		
	}
	
	// Set the metatable
	lua_setmetatable(L, -2);

	// The filename was allocated using malloc. Do something about that.
	free(filename);
	
	// Awesome. Return the object
	return 1;
	
}


// savestate.save(object state)
//
//   Saves a state to the given object.
static int savestate_save(lua_State *L) {

	const char *filename = savestateobj2filename(L,1);

//	printf("saving %s\n", filename);

	// Save states are very expensive. They take time.
	numTries--;

	bool8 retvalue = S9xFreezeGame(filename);
	if (!retvalue) {
		// Uh oh
		luaL_error(L, "savestate failed");
	}
	return 0;

}

// savestate.load(object state)
//
//   Loads the given state
static int savestate_load(lua_State *L) {

	const char *filename = savestateobj2filename(L,1);

	numTries--;

//	printf("loading %s\n", filename);
	bool8 retvalue = S9xUnfreezeGame(filename);
	if (!retvalue) {
		// Uh oh
		luaL_error(L, "loadstate failed");
	}
	return 0;

}

static int savestate_registersave(lua_State *L) {
 
	lua_settop(L,1);
	if (!lua_isnil(L,1))
		luaL_checktype(L, 1, LUA_TFUNCTION);
	lua_getfield(L, LUA_REGISTRYINDEX, LUA_SAVE_CALLBACK_STRING);
	lua_pushvalue(L,1);
	lua_setfield(L, LUA_REGISTRYINDEX, LUA_SAVE_CALLBACK_STRING);
	return 1;
}
static int savestate_registerload(lua_State *L) {

	lua_settop(L,1);
	if (!lua_isnil(L,1))
		luaL_checktype(L, 1, LUA_TFUNCTION);
	lua_getfield(L, LUA_REGISTRYINDEX, LUA_LOAD_CALLBACK_STRING);
	lua_pushvalue(L,1);
	lua_setfield(L, LUA_REGISTRYINDEX, LUA_LOAD_CALLBACK_STRING);
	return 1;
}

static int savestate_loadscriptdata(lua_State *L) {

	const char *filename = savestateobj2filename(L,1);

	{
		LuaSaveData saveData;

		char luaSaveFilename [512];
		strncpy(luaSaveFilename, filename, 512);
		luaSaveFilename[512-(1+7/*strlen(".luasav")*/)] = '\0';
		strcat(luaSaveFilename, ".luasav");
		FILE* luaSaveFile = fopen(luaSaveFilename, "rb");
		if(luaSaveFile)
		{
			saveData.ImportRecords(luaSaveFile);
			fclose(luaSaveFile);

			lua_settop(L, 0);
			saveData.LoadRecord(L, LUA_DATARECORDKEY, (unsigned int)-1);
			return lua_gettop(L);
		}
	}
	return 0;
}

// int snes9x.framecount()
//
//   Gets the frame counter for the movie, or the number of frames since last reset.
int snes9x_framecount(lua_State *L) {
	if (!S9xMovieActive()) {
		lua_pushinteger(L, IPPU.TotalEmulatedFrames);
	}
	else {
		lua_pushinteger(L, S9xMovieGetFrameCounter());
	}
	return 1;
}

// int snes9x.lagcount()
int snes9x_lagcount(lua_State *L) {
	lua_pushinteger(L, IPPU.LagCounter);
	return 1;
}

// boolean snes9x.lagged()
int snes9x_lagged(lua_State *L) {
	extern bool8 pad_read_last; // from ppu.cpp
	lua_pushboolean(L, !pad_read_last);
	return 1;
}

// string movie.mode()
//
//   "record", "playback" or nil
int movie_mode(lua_State *L) {
	if (!S9xMovieActive()) {
		lua_pushnil(L);
		return 1;
	}
	
	if (S9xMovieRecording())
		lua_pushstring(L, "record");
	else
		lua_pushstring(L, "playback");
	return 1;
}


static int movie_rerecordcounting(lua_State *L) {
	if (lua_gettop(L) == 0)
		luaL_error(L, "no parameters specified");

	skipRerecords = lua_toboolean(L,1);
	return 0;
}

// movie.stop()
//
//   Stops movie playback/recording. Bombs out if movie is not running.
static int movie_stop(lua_State *L) {
	if (!S9xMovieActive())
		luaL_error(L, "no movie");
	
	S9xMovieStop(FALSE);
	return 0;

}




// Common code by the gui library: make sure the screen array is ready
static void gui_prepare() {
	if (!gui_data)
		gui_data = (uint8*) malloc(256*239*4);
	if (!gui_used)
		memset(gui_data, 0, 256*239*4);
	gui_used = TRUE;
}

// pixform for lua graphics
#define BUILD_PIXEL_ARGB8888(A,R,G,B) (((int) (A) << 24) | ((int) (R) << 16) | ((int) (G) << 8) | (int) (B))
#define DECOMPOSE_PIXEL_ARGB8888(PIX,A,R,G,B) { (A) = ((PIX) >> 24) & 0xff; (R) = ((PIX) >> 16) & 0xff; (G) = ((PIX) >> 8) & 0xff; (B) = (PIX) & 0xff; }
#define LUA_BUILD_PIXEL BUILD_PIXEL_ARGB8888
#define LUA_DECOMPOSE_PIXEL DECOMPOSE_PIXEL_ARGB8888

template <class T> static void swap(T &one, T &two) {
	T temp = one;
	one = two;
	two = temp;
}

// write a pixel to buffer
static inline void blend32(uint32 *dstPixel, uint32 colour)
{
	uint8 *dst = (uint8*) dstPixel;
	int a, r, g, b;
	LUA_DECOMPOSE_PIXEL(colour, a, r, g, b);

	if (a == 255 || dst[3] == 0) {
		// direct copy
		*(uint32*)(dst) = colour;
	}
	else if (a == 0) {
		// do not copy
	}
	else {
		// alpha-blending
		int a_dst = ((255 - a) * dst[3] + 128) / 255;
		int a_new = a + a_dst;

		dst[0] = (uint8) ((( dst[0] * a_dst + b * a) + (a_new / 2)) / a_new);
		dst[1] = (uint8) ((( dst[1] * a_dst + g * a) + (a_new / 2)) / a_new);
		dst[2] = (uint8) ((( dst[2] * a_dst + r * a) + (a_new / 2)) / a_new);
		dst[3] = (uint8) a_new;
	}
}
// check if a pixel is in the lua canvas
static inline bool gui_check_boundary(int x, int y) {
	return !(x < 0 || x >= 256 || y < 0 || y >= 239);
}

// write a pixel to gui_data (do not check boundaries for speedup)
static inline void gui_drawpixel_fast(int x, int y, uint32 colour) {
	//gui_prepare();
	blend32((uint32*) &gui_data[(y*256+x)*4], colour);
}

// write a pixel to gui_data (check boundaries)
static inline void gui_drawpixel_internal(int x, int y, uint32 colour) {
	//gui_prepare();
	if (gui_check_boundary(x, y))
		gui_drawpixel_fast(x, y, colour);
}

// draw a line on gui_data (checks boundaries)
static void gui_drawline_internal(int x1, int y1, int x2, int y2, bool lastPixel, uint32 colour) {

	//gui_prepare();

	// Note: New version of Bresenham's Line Algorithm
	// http://groups.google.co.jp/group/rec.games.roguelike.development/browse_thread/thread/345f4c42c3b25858/29e07a3af3a450e6?show_docid=29e07a3af3a450e6

	int swappedx = 0;
	int swappedy = 0;

	int xtemp = x1-x2;
	int ytemp = y1-y2;
	if (xtemp == 0 && ytemp == 0) {
		gui_drawpixel_internal(x1, y1, colour);
		return;
	}
	if (xtemp < 0) {
		xtemp = -xtemp;
		swappedx = 1;
	}
	if (ytemp < 0) {
		ytemp = -ytemp;
		swappedy = 1;
	}

	int delta_x = xtemp << 1;
	int delta_y = ytemp << 1;

	signed char ix = x1 > x2?1:-1;
	signed char iy = y1 > y2?1:-1;

	if (lastPixel)
		gui_drawpixel_internal(x2, y2, colour);

	if (delta_x >= delta_y) {
		int error = delta_y - (delta_x >> 1);

		while (x2 != x1) {
			if (error == 0 && !swappedx)
				gui_drawpixel_internal(x2+ix, y2, colour);
			if (error >= 0) {
				if (error || (ix > 0)) {
					y2 += iy;
					error -= delta_x;
				}
			}
			x2 += ix;
			gui_drawpixel_internal(x2, y2, colour);
			if (error == 0 && swappedx)
				gui_drawpixel_internal(x2, y2+iy, colour);
			error += delta_y;
		}
	}
	else {
		int error = delta_x - (delta_y >> 1);

		while (y2 != y1) {
			if (error == 0 && !swappedy)
				gui_drawpixel_internal(x2, y2+iy, colour);
			if (error >= 0) {
				if (error || (iy > 0)) {
					x2 += ix;
					error -= delta_y;
				}
			}
			y2 += iy;
			gui_drawpixel_internal(x2, y2, colour);
			if (error == 0 && swappedy)
				gui_drawpixel_internal(x2+ix, y2, colour);
			error += delta_x;
		}
	}
}

// draw a rect on gui_data
static void gui_drawbox_internal(int x1, int y1, int x2, int y2, uint32 colour) {

	if (x1 > x2) 
		swap<int>(x1, x2);
	if (y1 > y2) 
		swap<int>(y1, y2);
	if (x1 < 0)
		x1 = -1;
	if (y1 < 0)
		y1 = -1;
	if (x2 >= 256)
		x2 = 256;
	if (y2 >= 239)
		y2 = 239;

	//gui_prepare();

	gui_drawline_internal(x1, y1, x2, y1, true, colour);
	gui_drawline_internal(x1, y2, x2, y2, true, colour);
	gui_drawline_internal(x1, y1, x1, y2, true, colour);
	gui_drawline_internal(x2, y1, x2, y2, true, colour);
}

// draw a circle on gui_data
static void gui_drawcircle_internal(int x0, int y0, int radius, uint32 colour) {

	//gui_prepare();

	if (radius < 0)
		radius = -radius;
	if (radius == 0)
		return;
	if (radius == 1) {
		gui_drawpixel_internal(x0, y0, colour);
		return;
	}

	// http://en.wikipedia.org/wiki/Midpoint_circle_algorithm

	int f = 1 - radius;
	int ddF_x = 1;
	int ddF_y = -2 * radius;
	int x = 0;
	int y = radius;

	gui_drawpixel_internal(x0, y0 + radius, colour);
	gui_drawpixel_internal(x0, y0 - radius, colour);
	gui_drawpixel_internal(x0 + radius, y0, colour);
	gui_drawpixel_internal(x0 - radius, y0, colour);
 
	// same pixel shouldn't be drawed twice,
	// because each pixel has opacity.
	// so now the routine gets ugly.
	while(true)
	{
		assert(ddF_x == 2 * x + 1);
		assert(ddF_y == -2 * y);
		assert(f == x*x + y*y - radius*radius + 2*x - y + 1);
		if(f >= 0) 
		{
			y--;
			ddF_y += 2;
			f += ddF_y;
		}
		x++;
		ddF_x += 2;
		f += ddF_x;
		if (x < y) {
			gui_drawpixel_internal(x0 + x, y0 + y, colour);
			gui_drawpixel_internal(x0 - x, y0 + y, colour);
			gui_drawpixel_internal(x0 + x, y0 - y, colour);
			gui_drawpixel_internal(x0 - x, y0 - y, colour);
			gui_drawpixel_internal(x0 + y, y0 + x, colour);
			gui_drawpixel_internal(x0 - y, y0 + x, colour);
			gui_drawpixel_internal(x0 + y, y0 - x, colour);
			gui_drawpixel_internal(x0 - y, y0 - x, colour);
		}
		else if (x == y) {
			gui_drawpixel_internal(x0 + x, y0 + y, colour);
			gui_drawpixel_internal(x0 - x, y0 + y, colour);
			gui_drawpixel_internal(x0 + x, y0 - y, colour);
			gui_drawpixel_internal(x0 - x, y0 - y, colour);
			break;
		}
		else
			break;
	}
}

// draw fill rect on gui_data
static void gui_fillbox_internal(int x1, int y1, int x2, int y2, uint32 colour) {

	if (x1 > x2) 
		swap<int>(x1, x2);
	if (y1 > y2) 
		swap<int>(y1, y2);
	if (x1 < 0)
		x1 = 0;
	if (y1 < 0)
		y1 = 0;
	if (x2 >= 256)
		x2 = 256 - 1;
	if (y2 >= 239)
		y2 = 239 - 1;

	//gui_prepare();

	int ix, iy;
	for (iy = y1; iy <= y2; iy++) {
		for (ix = x1; ix <= x2; ix++) {
			gui_drawpixel_fast(ix, iy, colour);
		}
	}
}

// fill a circle on gui_data
static void gui_fillcircle_internal(int x0, int y0, int radius, uint32 colour) {

	//gui_prepare();

	if (radius < 0)
		radius = -radius;
	if (radius == 0)
		return;
	if (radius == 1) {
		gui_drawpixel_internal(x0, y0, colour);
		return;
	}

	// http://en.wikipedia.org/wiki/Midpoint_circle_algorithm

	int f = 1 - radius;
	int ddF_x = 1;
	int ddF_y = -2 * radius;
	int x = 0;
	int y = radius;

	gui_drawline_internal(x0, y0 - radius, x0, y0 + radius, true, colour);
 
	while(true)
	{
		assert(ddF_x == 2 * x + 1);
		assert(ddF_y == -2 * y);
		assert(f == x*x + y*y - radius*radius + 2*x - y + 1);
		if(f >= 0) 
		{
			y--;
			ddF_y += 2;
			f += ddF_y;
		}
		x++;
		ddF_x += 2;
		f += ddF_x;

		if (x < y) {
			gui_drawline_internal(x0 + x, y0 - y, x0 + x, y0 + y, true, colour);
			gui_drawline_internal(x0 - x, y0 - y, x0 - x, y0 + y, true, colour);
			if (f >= 0) {
				gui_drawline_internal(x0 + y, y0 - x, x0 + y, y0 + x, true, colour);
				gui_drawline_internal(x0 - y, y0 - x, x0 - y, y0 + x, true, colour);
			}
		}
		else if (x == y) {
			gui_drawline_internal(x0 + x, y0 - y, x0 + x, y0 + y, true, colour);
			gui_drawline_internal(x0 - x, y0 - y, x0 - x, y0 + y, true, colour);
			break;
		}
		else
			break;
	}
}

// Helper for a simple hex parser
static int hex2int(lua_State *L, char c) {
	if (c >= '0' && c <= '9')
		return c-'0';
	if (c >= 'a' && c <= 'f')
		return c - 'a' + 10;
	if (c >= 'A' && c <= 'F')
		return c - 'A' + 10;
	return luaL_error(L, "invalid hex in colour");
}

static const struct ColorMapping
{
	const char* name;
	int value;
}
s_colorMapping [] =
{
	{"white",     0xFFFFFFFF},
	{"black",     0x000000FF},
	{"clear",     0x00000000},
	{"gray",      0x7F7F7FFF},
	{"grey",      0x7F7F7FFF},
	{"red",       0xFF0000FF},
	{"orange",    0xFF7F00FF},
	{"yellow",    0xFFFF00FF},
	{"chartreuse",0x7FFF00FF},
	{"green",     0x00FF00FF},
	{"teal",      0x00FF7FFF},
	{"cyan" ,     0x00FFFFFF},
	{"blue",      0x0000FFFF},
	{"purple",    0x7F00FFFF},
	{"magenta",   0xFF00FFFF},
};

/**
 * Converts an integer or a string on the stack at the given
 * offset to a RGB32 colour. Several encodings are supported.
 * The user may construct their own RGB value, given a simple colour name,
 * or an HTML-style "#09abcd" colour. 16 bit reduction doesn't occur at this time.
 */
static inline bool str2colour(uint32 *colour, lua_State *L, const char *str) {
	if (str[0] == '#') {
		int color;
		sscanf(str+1, "%X", &color);
		int len = strlen(str+1);
		int missing = max(0, 8-len);
		color <<= missing << 2;
		if(missing >= 2) color |= 0xFF;
		*colour = color;
		return true;
	}
	else {
		if(!strnicmp(str, "rand", 4)) {
			*colour = ((rand()*255/RAND_MAX) << 8) | ((rand()*255/RAND_MAX) << 16) | ((rand()*255/RAND_MAX) << 24) | 0xFF;
			return true;
		}
		for(int i = 0; i < sizeof(s_colorMapping)/sizeof(*s_colorMapping); i++) {
			if(!stricmp(str,s_colorMapping[i].name)) {
				*colour = s_colorMapping[i].value;
				return true;
			}
		}
	}
	return false;
}
static inline uint32 gui_getcolour_wrapped(lua_State *L, int offset, bool hasDefaultValue, uint32 defaultColour) {
	switch (lua_type(L,offset)) {
	case LUA_TSTRING:
		{
			const char *str = lua_tostring(L,offset);
			uint32 colour;

			if (str2colour(&colour, L, str))
				return colour;
			else {
				if (hasDefaultValue)
					return defaultColour;
				else
					return luaL_error(L, "unknown colour %s", str);
			}
		}
	case LUA_TNUMBER:
		{
			uint32 colour = (uint32) lua_tointeger(L,offset);
			return colour;
		}
	default:
		if (hasDefaultValue)
			return defaultColour;
		else
			return luaL_error(L, "invalid colour");
	}
}
static uint32 gui_getcolour(lua_State *L, int offset) {
	uint32 colour;
	uint8 a, r, g, b;

	colour = gui_getcolour_wrapped(L, offset, false, 0);
	a = ((colour & 0xff) * transparencyModifier) / 255;
	if (a > 255) a = 255;
	b = (colour >> 8) & 0xff;
	g = (colour >> 16) & 0xff;
	r = (colour >> 24) & 0xff;
	return LUA_BUILD_PIXEL(a, r, g, b);
}
static uint32 gui_optcolour(lua_State *L, int offset, uint32 defaultColour) {
	uint32 colour;
	uint8 a, r, g, b;
	uint8 defA, defB, defG, defR;

	LUA_DECOMPOSE_PIXEL(defaultColour, defA, defR, defG, defB);
	defaultColour = (defR << 24) | (defG << 16) | (defB << 8) | defA;

	colour = gui_getcolour_wrapped(L, offset, true, defaultColour);
	a = ((colour & 0xff) * transparencyModifier) / 255;
	if (a > 255) a = 255;
	b = (colour >> 8) & 0xff;
	g = (colour >> 16) & 0xff;
	r = (colour >> 24) & 0xff;
	return LUA_BUILD_PIXEL(a, r, g, b);
}

// gui.drawpixel(x,y,colour)
static int gui_drawpixel(lua_State *L) {

	int x = luaL_checkinteger(L, 1);
	int y = luaL_checkinteger(L,2);

	uint32 colour = gui_getcolour(L,3);

//	if (!gui_check_boundary(x, y))
//		luaL_error(L,"bad coordinates");

	gui_prepare();

	gui_drawpixel_internal(x, y, colour);

	return 0;
}

// gui.drawline(x1,y1,x2,y2,type colour)
static int gui_drawline(lua_State *L) {

	int x1,y1,x2,y2;
	uint32 colour;
	x1 = luaL_checkinteger(L,1);
	y1 = luaL_checkinteger(L,2);
	x2 = luaL_checkinteger(L,3);
	y2 = luaL_checkinteger(L,4);
	colour = gui_getcolour(L,5);

//	if (!gui_check_boundary(x1, y1))
//		luaL_error(L,"bad coordinates");
//
//	if (!gui_check_boundary(x2, y2))
//		luaL_error(L,"bad coordinates");

	gui_prepare();

	gui_drawline_internal(x1, y1, x2, y2, true, colour);

	return 0;
}

// gui.drawbox(x1, y1, x2, y2, colour)
static int gui_drawbox(lua_State *L) {

	int x1,y1,x2,y2;
	uint32 colour;

	x1 = luaL_checkinteger(L,1);
	y1 = luaL_checkinteger(L,2);
	x2 = luaL_checkinteger(L,3);
	y2 = luaL_checkinteger(L,4);
	colour = gui_getcolour(L,5);

//	if (!gui_check_boundary(x1, y1))
//		luaL_error(L,"bad coordinates");
//
//	if (!gui_check_boundary(x2, y2))
//		luaL_error(L,"bad coordinates");

	gui_prepare();

	gui_drawbox_internal(x1, y1, x2, y2, colour);

	return 0;
}

// gui.drawcircle(x0, y0, radius, colour)
static int gui_drawcircle(lua_State *L) {

	int x, y, r;
	uint32 colour;

	x = luaL_checkinteger(L,1);
	y = luaL_checkinteger(L,2);
	r = luaL_checkinteger(L,3);
	colour = gui_getcolour(L,4);

	gui_prepare();

	gui_drawcircle_internal(x, y, r, colour);

	return 0;
}

// gui.fillbox(x1, y1, x2, y2, colour)
static int gui_fillbox(lua_State *L) {

	int x1,y1,x2,y2;
	uint32 colour;

	x1 = luaL_checkinteger(L,1);
	y1 = luaL_checkinteger(L,2);
	x2 = luaL_checkinteger(L,3);
	y2 = luaL_checkinteger(L,4);
	colour = gui_getcolour(L,5);

//	if (!gui_check_boundary(x1, y1))
//		luaL_error(L,"bad coordinates");
//
//	if (!gui_check_boundary(x2, y2))
//		luaL_error(L,"bad coordinates");

	gui_prepare();

	gui_fillbox_internal(x1, y1, x2, y2, colour);

	return 0;
}

// gui.fillcircle(x0, y0, radius, colour)
static int gui_fillcircle(lua_State *L) {

	int x, y, r;
	uint32 colour;

	x = luaL_checkinteger(L,1);
	y = luaL_checkinteger(L,2);
	r = luaL_checkinteger(L,3);
	colour = gui_getcolour(L,4);

	gui_prepare();

	gui_fillcircle_internal(x, y, r, colour);

	return 0;
}

// gui.gdscreenshot()
//
//  Returns a screen shot as a string in gd's v1 file format.
//  This allows us to make screen shots available without gd installed locally.
//  Users can also just grab pixels via substring selection.
//
//  I think...  Does lua support grabbing byte values from a string?
//  Well, either way, just install gd and do what you like with it.
//  It really is easier that way.
static int gui_gdscreenshot(lua_State *L) {

	// Eat the stack
	lua_settop(L,0);
	
	// This is QUITE nasty...
	
	const int width=256, height=224;
	
	// Stack allocation
	unsigned char *buffer = (unsigned char*)alloca(2+2+2+1+4 + (width*height*4));
	unsigned char *pixels = (buffer + 2+2+2+1+4);

	// Truecolour image
	buffer[0] = 255;
	buffer[1] = 254;
	
	// Width
	buffer[2] = width >> 8;
	buffer[3] = width & 0xFF;
	
	// height
	buffer[4] = height >> 8;
	buffer[5] = height & 0xFF;
	
	// Make it truecolour... AGAIN?
	buffer[6] = 1;
	
	// No transparency
	buffer[7] = buffer[8] = buffer[9] = buffer[10] = 255;
	
	// Now we can actually save the image data
	int i = 0;
	int x,y;
	for (y=0; y < height; y++) {
		for (x=0; x < width; x++) {
			// This section is long and verbose for the sake of readability by you.
			// I hope the compiler can make it a little bit faster than it looks!
		
			unsigned short rgb16 = *((unsigned short*)(GFX.Screen + GFX.Pitch*y) + x);
			int red = (rgb16 >> 11) & 31;
			
			// Diagram:   000RRrrr -> RRrrr000 -> RRrrrRRr
			red = ((red << 3) & 0xff) | ((red >> 5) & 0xff);

			int green = (rgb16 >> 5) & 63;
			// 00Gggggg -> Gggggg00 -> GgggggGg
			green = ((green << 2) & 0xff) | ((green >> 6) & 0xff);

			int blue = (rgb16) & 31;
			
			// Diagram:   000RRrrr -> RRrrr000 -> RRrrrRRr
			blue = ((blue << 3) & 0xff) | ((blue >> 5) & 0xff);

			// Alpha
			pixels[i++] = 0;
			// R
			pixels[i++] = red;
			// G
			pixels[i++] = green;
			// B
			pixels[i++] = blue;
		}
	}
	
	// Ugh, ugh, ugh. Don't call this more than once a frame, for god's sake!
	
	lua_pushlstring(L, (char*)buffer, 2+2+2+1+4 + (width*height*4));
	
	// Buffers allocated with alloca are freed by the function's exit, since they're on the stack.
	
	return 1;
}


// gui.opacity(number alphaValue)
// sets the transparency of subsequent draw calls
// 0.0 is completely transparent, 1.0 is completely opaque
// non-integer values are supported and meaningful, as are values greater than 1.0
// it is not necessary to use this function to get transparency (or the less-recommended gui.transparency() either),
// because you can provide an alpha value in the color argument of each draw call.
// however, it can be convenient to be able to globally modify the drawing transparency
static int gui_setopacity(lua_State *L) {
	double opacF = luaL_checknumber(L,1);
	if (opacF < 0.0 || opacF > 1.0) {
		luaL_error(L, "alphaValue out of range");
	}

	transparencyModifier = (uint8) (opacF * 255);
	return 0;
}

// gui.transparency(int strength)
//
//  0 = solid, 
static int gui_transparency(lua_State *L) {
	double trans = luaL_checknumber(L,1);
	if (trans < 0.0 || trans > 4.0) {
		luaL_error(L, "transparency out of range");
	}

	transparencyModifier = (uint8) ((4.0 - trans) / 4.0 * 255);
	return 0;
}


static const uint32 Small_Font_Data[] =
{
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,			// 32	 
	0x00000000, 0x00000300, 0x00000400, 0x00000500, 0x00000000, 0x00000700, 0x00000000,			// 33	!
	0x00000000, 0x00040002, 0x00050003, 0x00000000, 0x00000000, 0x00000000, 0x00000000,			// 34	"
	0x00000000, 0x00040002, 0x00050403, 0x00060004, 0x00070605, 0x00080006, 0x00000000,			// 35	#
	0x00000000, 0x00040300, 0x00000403, 0x00000500, 0x00070600, 0x00000706, 0x00000000,			// 36	$
	0x00000000, 0x00000002, 0x00050000, 0x00000500, 0x00000005, 0x00080000, 0x00000000,			// 37	%
	0x00000000, 0x00000300, 0x00050003, 0x00000500, 0x00070005, 0x00080700, 0x00000000,			// 38	&
	0x00000000, 0x00000300, 0x00000400, 0x00000000, 0x00000000, 0x00000000, 0x00000000,			// 39	'
	0x00000000, 0x00000300, 0x00000003, 0x00000004, 0x00000005, 0x00000700, 0x00000000,			// 40	(
	0x00000000, 0x00000300, 0x00050000, 0x00060000, 0x00070000, 0x00000700, 0x00000000,			// 41	)
	0x00000000, 0x00000000, 0x00000400, 0x00060504, 0x00000600, 0x00080006, 0x00000000,			// 42	*
	0x00000000, 0x00000000, 0x00000400, 0x00060504, 0x00000600, 0x00000000, 0x00000000,			// 43	+
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000600, 0x00000700, 0x00000007,			// 44	,
	0x00000000, 0x00000000, 0x00000000, 0x00060504, 0x00000000, 0x00000000, 0x00000000,			// 45	-
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000700, 0x00000000,			// 46	.
	0x00030000, 0x00040000, 0x00000400, 0x00000500, 0x00000005, 0x00000006, 0x00000000,			// 47	/
	0x00000000, 0x00000300, 0x00050003, 0x00060004, 0x00070005, 0x00000700, 0x00000000,			// 48	0
	0x00000000, 0x00000300, 0x00000403, 0x00000500, 0x00000600, 0x00000700, 0x00000000,			// 49	1
	0x00000000, 0x00000302, 0x00050000, 0x00000500, 0x00000005, 0x00080706, 0x00000000,			// 50	2
	0x00000000, 0x00000302, 0x00050000, 0x00000504, 0x00070000, 0x00000706, 0x00000000,			// 51	3
	0x00000000, 0x00000300, 0x00000003, 0x00060004, 0x00070605, 0x00080000, 0x00000000,			// 52	4
	0x00000000, 0x00040302, 0x00000003, 0x00000504, 0x00070000, 0x00000706, 0x00000000,			// 53	5
	0x00000000, 0x00000300, 0x00000003, 0x00000504, 0x00070005, 0x00000700, 0x00000000,			// 54	6
	0x00000000, 0x00040302, 0x00050000, 0x00000500, 0x00000600, 0x00000700, 0x00000000,			// 55	7
	0x00000000, 0x00000300, 0x00050003, 0x00000500, 0x00070005, 0x00000700, 0x00000000,			// 56	8
	0x00000000, 0x00000300, 0x00050003, 0x00060500, 0x00070000, 0x00000700, 0x00000000,			// 57	9
	0x00000000, 0x00000000, 0x00000400, 0x00000000, 0x00000000, 0x00000700, 0x00000000,			// 58	:
	0x00000000, 0x00000000, 0x00000000, 0x00000500, 0x00000000, 0x00000700, 0x00000007,			// 59	;
	0x00000000, 0x00040000, 0x00000400, 0x00000004, 0x00000600, 0x00080000, 0x00000000,			// 60	<
	0x00000000, 0x00000000, 0x00050403, 0x00000000, 0x00070605, 0x00000000, 0x00000000,			// 61	=
	0x00000000, 0x00000002, 0x00000400, 0x00060000, 0x00000600, 0x00000006, 0x00000000,			// 62	>
	0x00000000, 0x00000302, 0x00050000, 0x00000500, 0x00000000, 0x00000700, 0x00000000,			// 63	?
	0x00000000, 0x00000300, 0x00050400, 0x00060004, 0x00070600, 0x00000000, 0x00000000,			// 64	@
	0x00000000, 0x00000300, 0x00050003, 0x00060504, 0x00070005, 0x00080006, 0x00000000,			// 65	A
	0x00000000, 0x00000302, 0x00050003, 0x00000504, 0x00070005, 0x00000706, 0x00000000,			// 66	B
	0x00000000, 0x00040300, 0x00000003, 0x00000004, 0x00000005, 0x00080700, 0x00000000,			// 67	C
	0x00000000, 0x00000302, 0x00050003, 0x00060004, 0x00070005, 0x00000706, 0x00000000,			// 68	D
	0x00000000, 0x00040302, 0x00000003, 0x00000504, 0x00000005, 0x00080706, 0x00000000,			// 69	E
	0x00000000, 0x00040302, 0x00000003, 0x00000504, 0x00000005, 0x00000006, 0x00000000,			// 70	F
	0x00000000, 0x00040300, 0x00000003, 0x00060004, 0x00070005, 0x00080700, 0x00000000,			// 71	G
	0x00000000, 0x00040002, 0x00050003, 0x00060504, 0x00070005, 0x00080006, 0x00000000,			// 72	H
	0x00000000, 0x00000300, 0x00000400, 0x00000500, 0x00000600, 0x00000700, 0x00000000,			// 73	I
	0x00000000, 0x00040000, 0x00050000, 0x00060000, 0x00070005, 0x00000700, 0x00000000,			// 74	J
	0x00000000, 0x00040002, 0x00050003, 0x00000504, 0x00070005, 0x00080006, 0x00000000,			// 75	K
	0x00000000, 0x00000002, 0x00000003, 0x00000004, 0x00000005, 0x00080706, 0x00000000,			// 76	l
	0x00000000, 0x00040002, 0x00050403, 0x00060004, 0x00070005, 0x00080006, 0x00000000,			// 77	M
	0x00000000, 0x00000302, 0x00050003, 0x00060004, 0x00070005, 0x00080006, 0x00000000,			// 78	N
	0x00000000, 0x00040302, 0x00050003, 0x00060004, 0x00070005, 0x00080706, 0x00000000,			// 79	O
	0x00000000, 0x00000302, 0x00050003, 0x00000504, 0x00000005, 0x00000006, 0x00000000,			// 80	P
	0x00000000, 0x00040302, 0x00050003, 0x00060004, 0x00070005, 0x00080706, 0x00090000,			// 81	Q
	0x00000000, 0x00000302, 0x00050003, 0x00000504, 0x00070005, 0x00080006, 0x00000000,			// 82	R
	0x00000000, 0x00040300, 0x00000003, 0x00000500, 0x00070000, 0x00000706, 0x00000000,			// 83	S
	0x00000000, 0x00040302, 0x00000400, 0x00000500, 0x00000600, 0x00000700, 0x00000000,			// 84	T
	0x00000000, 0x00040002, 0x00050003, 0x00060004, 0x00070005, 0x00080706, 0x00000000,			// 85	U
	0x00000000, 0x00040002, 0x00050003, 0x00060004, 0x00000600, 0x00000700, 0x00000000,			// 86	V
	0x00000000, 0x00040002, 0x00050003, 0x00060004, 0x00070605, 0x00080006, 0x00000000,			// 87	W
	0x00000000, 0x00040002, 0x00050003, 0x00000500, 0x00070005, 0x00080006, 0x00000000,			// 88	X
	0x00000000, 0x00040002, 0x00050003, 0x00000500, 0x00000600, 0x00000700, 0x00000000,			// 89	Y
	0x00000000, 0x00040302, 0x00050000, 0x00000500, 0x00000005, 0x00080706, 0x00000000,			// 90	Z
	0x00000000, 0x00040300, 0x00000400, 0x00000500, 0x00000600, 0x00080700, 0x00000000,			// 91	[
	0x00000000, 0x00000002, 0x00000400, 0x00000500, 0x00070000, 0x00080000, 0x00000000,			// 92	'\'
	0x00000000, 0x00000302, 0x00000400, 0x00000500, 0x00000600, 0x00000706, 0x00000000,			// 93	]
	0x00000000, 0x00000300, 0x00050003, 0x00000000, 0x00000000, 0x00000000, 0x00000000,			// 94	^
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00080706, 0x00000000,			// 95	_
	0x00000000, 0x00000002, 0x00000400, 0x00000000, 0x00000000, 0x00000000, 0x00000000,			// 96	`
	0x00000000, 0x00000000, 0x00050400, 0x00060004, 0x00070005, 0x00080700, 0x00000000,			// 97	a
	0x00000000, 0x00000002, 0x00000003, 0x00000504, 0x00070005, 0x00000706, 0x00000000,			// 98	b
	0x00000000, 0x00000000, 0x00050400, 0x00000004, 0x00000005, 0x00080700, 0x00000000,			// 99	c
	0x00000000, 0x00040000, 0x00050000, 0x00060500, 0x00070005, 0x00080700, 0x00000000,			// 100	d
	0x00000000, 0x00000000, 0x00050400, 0x00060504, 0x00000005, 0x00080700, 0x00000000,			// 101	e
	0x00000000, 0x00040300, 0x00000003, 0x00000504, 0x00000005, 0x00000006, 0x00000000,			// 102	f
	0x00000000, 0x00000000, 0x00050400, 0x00060004, 0x00070600, 0x00080000, 0x00000807,			// 103	g
	0x00000000, 0x00000002, 0x00000003, 0x00000504, 0x00070005, 0x00080006, 0x00000000,			// 104	h
	0x00000000, 0x00000300, 0x00000000, 0x00000500, 0x00000600, 0x00000700, 0x00000000,			// 105	i
	0x00000000, 0x00000300, 0x00000000, 0x00000500, 0x00000600, 0x00000700, 0x00000007,			// 106	j
	0x00000000, 0x00000002, 0x00000003, 0x00060004, 0x00000605, 0x00080006, 0x00000000,			// 107	k
	0x00000000, 0x00000300, 0x00000400, 0x00000500, 0x00000600, 0x00080000, 0x00000000,			// 108	l
	0x00000000, 0x00000000, 0x00050003, 0x00060504, 0x00070005, 0x00080006, 0x00000000,			// 109	m
	0x00000000, 0x00000000, 0x00000403, 0x00060004, 0x00070005, 0x00080006, 0x00000000,			// 110	n
	0x00000000, 0x00000000, 0x00000400, 0x00060004, 0x00070005, 0x00000700, 0x00000000,			// 111	o
	0x00000000, 0x00000000, 0x00000400, 0x00060004, 0x00000605, 0x00000006, 0x00000007,			// 112	p
	0x00000000, 0x00000000, 0x00000400, 0x00060004, 0x00070600, 0x00080000, 0x00090000,			// 113	q
	0x00000000, 0x00000000, 0x00050003, 0x00000504, 0x00000005, 0x00000006, 0x00000000,			// 114	r
	0x00000000, 0x00000000, 0x00050400, 0x00000004, 0x00070600, 0x00000706, 0x00000000,			// 115	s
	0x00000000, 0x00000300, 0x00050403, 0x00000500, 0x00000600, 0x00080000, 0x00000000,			// 116	t
	0x00000000, 0x00000000, 0x00050003, 0x00060004, 0x00070005, 0x00080700, 0x00000000,			// 117	u
	0x00000000, 0x00000000, 0x00050003, 0x00060004, 0x00070005, 0x00000700, 0x00000000,			// 118	v
	0x00000000, 0x00000000, 0x00050003, 0x00060004, 0x00070605, 0x00080006, 0x00000000,			// 119	w
	0x00000000, 0x00000000, 0x00050003, 0x00000500, 0x00070005, 0x00080006, 0x00000000,			// 120	x
	0x00000000, 0x00000000, 0x00050003, 0x00060004, 0x00000600, 0x00000700, 0x00000007,			// 121	y
	0x00000000, 0x00000000, 0x00050403, 0x00000500, 0x00000005, 0x00080706, 0x00000000,			// 122	z
	0x00000000, 0x00040300, 0x00000400, 0x00000504, 0x00000600, 0x00080700, 0x00000000,			// 123	{
	0x00000000, 0x00000300, 0x00000400, 0x00000000, 0x00000600, 0x00000700, 0x00000000,			// 124	|
	0x00000000, 0x00000302, 0x00000400, 0x00060500, 0x00000600, 0x00000706, 0x00000000,			// 125	}
	0x00000000, 0x00000302, 0x00050000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,			// 126	~
	0x00000000, 0x00000000, 0x00000400, 0x00060004, 0x00070605, 0x00000000, 0x00000000,			// 127	
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
};

static void PutTextInternal (const char *str, int len, short x, short y, int color, int backcolor)
{
	int Opac = (color >> 24) & 0xFF;
	int backOpac = (backcolor >> 24) & 0xFF;
	int origX = x;

	if(!Opac && !backOpac)
		return;

	while(*str && len && y < 239)
	{
		int c = *str++;
		while (x > 256 && c != '\n') {
			c = *str;
			if (c == '\0')
				break;
			str++;
		}
		if(c == '\n')
		{
			x = origX;
			y += 8;
			continue;
		}
		else if(c == '\t') // just in case
		{
			const int tabSpace = 8;
			x += (tabSpace-(((x-origX)/4)%tabSpace))*4;
			continue;
		}
		if((unsigned int)(c-32) >= 96)
			continue;
		const unsigned char* Cur_Glyph = (const unsigned char*)&Small_Font_Data + (c-32)*7*4;

		for(int y2 = 0; y2 < 8; y2++)
		{
			unsigned int glyphLine = *((unsigned int*)Cur_Glyph + y2);
			for(int x2 = -1; x2 < 4; x2++)
			{
				int shift = x2 << 3;
				int mask = 0xFF << shift;
				int intensity = (glyphLine & mask) >> shift;

				if(intensity && x2 >= 0 && y2 < 7)
				{
					//int xdraw = max(0,min(256 - 1,x+x2));
					//int ydraw = max(0,min(239 - 1,y+y2));
					//gui_drawpixel_fast(xdraw, ydraw, color);
					gui_drawpixel_internal(x+x2, y+y2, color);
				}
				else if(backOpac)
				{
					for(int y3 = max(0,y2-1); y3 <= min(6,y2+1); y3++)
					{
						unsigned int glyphLine = *((unsigned int*)Cur_Glyph + y3);
						for(int x3 = max(0,x2-1); x3 <= min(3,x2+1); x3++)
						{
							int shift = x3 << 3;
							int mask = 0xFF << shift;
							intensity |= (glyphLine & mask) >> shift;
							if (intensity)
								goto draw_outline; // speedup?
						}
					}
draw_outline:
					if(intensity)
					{
						//int xdraw = max(0,min(256 - 1,x+x2));
						//int ydraw = max(0,min(239 - 1,y+y2));
						//gui_drawpixel_fast(xdraw, ydraw, backcolor);
						gui_drawpixel_internal(x+x2, y+y2, backcolor);
					}
				}
			}
		}

		x += 4;
		len--;
	}
}

static int strlinelen(const char* string)
{
	const char* s = string;
	while(*s && *s != '\n')
		s++;
	if(*s)
		s++;
	return s - string;
}

static void LuaDisplayString (const char *string, int y, int x, uint32 color, uint32 outlineColor)
{
	if(!string)
		return;

	gui_prepare();

	PutTextInternal(string, strlen(string), x, y, color, outlineColor);
/*
	const char* ptr = string;
	while(*ptr && y < 239)
	{
		int len = strlinelen(ptr);
		int skip = 0;
		if(len < 1) len = 1;

		// break up the line if it's too long to display otherwise
		if(len > 63)
		{
			len = 63;
			const char* ptr2 = ptr + len-1;
			for(int j = len-1; j; j--, ptr2--)
			{
				if(*ptr2 == ' ' || *ptr2 == '\t')
				{
					len = j;
					skip = 1;
					break;
				}
			}
		}

		int xl = 0;
		int yl = 0;
		int xh = (256 - 1 - 1) - 4*len;
		int yh = 239 - 1;
		int x2 = min(max(x,xl),xh);
		int y2 = min(max(y,yl),yh);

		PutTextInternal(ptr,len,x2,y2,color,outlineColor);

		ptr += len + skip;
		y += 8;
	}
*/
}

// gui.text(int x, int y, string msg)
//
//  Displays the given text on the screen, using the same font and techniques as the
//  main HUD.
static int gui_text(lua_State *L) {

	extern int font_height;
	const char *msg;
	int x, y;
	uint32 colour, borderColour;

	x = luaL_checkinteger(L,1);
	y = luaL_checkinteger(L,2);
	msg = luaL_checkstring(L,3);

//	if (x < 0 || x >= 256 || y < 0 || y >= (239 - font_height))
//		luaL_error(L,"bad coordinates");

	uint8 displayColorR = (Settings.DisplayColor >> 11) << 3;
	uint8 displayColorG = ((Settings.DisplayColor >> 6) & 0x1f) << 3;
	uint8 displayColorB = (Settings.DisplayColor & 0x1f) << 3;
	colour = gui_optcolour(L,4,LUA_BUILD_PIXEL(255, displayColorR, displayColorG, displayColorB));
	borderColour = gui_optcolour(L,5,LUA_BUILD_PIXEL(255, 0, 0, 0));

	gui_prepare();

	LuaDisplayString(msg, y, x, colour, borderColour);

	return 0;

}


// gui.gdoverlay(string str)
//
//  Overlays the given image on the screen.
static int gui_gdoverlay(lua_State *L) {

	int baseX, baseY;
	int width, height;
	size_t size;

	baseX = luaL_checkinteger(L,1);
	baseY = luaL_checkinteger(L,2);
	const uint8 *data = (const uint8*) luaL_checklstring(L, 3, &size);
	
	if (size < 11)
		luaL_error(L, "bad image data");
	
	if (data[0] != 255 || data[1] != 254)
		luaL_error(L, "bad image data or not truecolour");
		
	width = data[2] << 8 | data[3];
	height = data[4] << 8 | data[5];
	
	if (!data[6])
		luaL_error(L, "bad image data or not truecolour");
	
	// Don't care about transparent colour
	if ((int)size < (11+ width*height*4))
		luaL_error(L, "bad image data");
	
	const uint8* pixels = data + 11;
	
	// Run image

	gui_prepare();

	// These coordinates are relative to the image itself.
	int x,y;
	
	// These coordinates are relative to the screen
	int sx, sy;
	
	if (baseY < 0) {
		// Above the top of the screen
		sy = 0;
		y = -baseY;
	} else {
		// It starts on the screen itself
		sy = baseY;
		y = 0; 
	}	
	
	for (; y < height && sy < 239; y++, sy++) {
	
		if (baseX < 0) {
			x = -baseX;
			sx = 0;
		} else {
			x = 0;
			sx = baseX;
		}

		for (; x < width && sx < 256; x++, sx++) {
			if (pixels[4 * (y*height+x)] == 127)
				continue;

			gui_drawpixel_fast(sx, sy, LUA_BUILD_PIXEL(255,
				pixels[4 * (y*width+x)+1], pixels[4 * (y*width+x)+2],
				pixels[4 * (y*width+x)+3]));
		}
	
	}

	return 0;
}


// function gui.register(function f)
//
//  This function will be called just before a graphical update.
//  More complicated, but doesn't suffer any frame delays.
//  Nil will be accepted in place of a function to erase
//  a previously registered function, and the previous function
//  (if any) is returned, or nil if none.
static int gui_register(lua_State *L) {

	// We'll do this straight up.


	// First set up the stack.
	lua_settop(L,1);
	
	// Verify the validity of the entry
	if (!lua_isnil(L,1))
		luaL_checktype(L, 1, LUA_TFUNCTION);

	// Get the old value
	lua_getfield(L, LUA_REGISTRYINDEX, guiCallbackTable);
	
	// Save the new value
	lua_pushvalue(L,1);
	lua_setfield(L, LUA_REGISTRYINDEX, guiCallbackTable);
	
	// The old value is on top of the stack. Return it.
	return 1;

}

// string gui.popup(string message, [string type = "ok"])
//
//  Popup dialog!
int gui_popup(lua_State *L) {
	const char *message = luaL_checkstring(L, 1);
	const char *type = luaL_optstring(L, 2, "ok");
	
#ifdef __WIN32__
	int t;
	if (strcmp(type, "ok") == 0)
		t = MB_OK;
	else if (strcmp(type, "yesno") == 0)
		t = MB_YESNO;
	else if (strcmp(type, "yesnocancel") == 0)
		t = MB_YESNOCANCEL;
	else
		return luaL_error(L, "invalid popup type \"%s\"", type);

	int result = MessageBox(GUI.hWnd, message, "Lua Script Pop-up", t);
	
	lua_settop(L,1);

	if (t != MB_OK) {
		if (result == IDYES)
			lua_pushstring(L, "yes");
		else if (result == IDNO)
			lua_pushstring(L, "no");
		else if (result == IDCANCEL)
			lua_pushstring(L, "cancel");
		else
			luaL_error(L, "win32 unrecognized return value %d", result);
		return 1;
	}

	// else, we don't care.
	return 0;
#else

	char *t;
#ifdef __linux

	// The Linux backend has a "FromPause" variable.
	// If set to 1, assume some known external event has screwed with the flow of time.
	// Since this pauses the emulator waiting for a response, we set it to 1.
	extern int FromPause;
	FromPause = 1;


	int pid; // appease compiler

	// Before doing any work, verify the correctness of the parameters.
	if (strcmp(type, "ok") == 0)
		t = "OK:100";
	else if (strcmp(type, "yesno") == 0)
		t = "Yes:100,No:101";
	else if (strcmp(type, "yesnocancel") == 0)
		t = "Yes:100,No:101,Cancel:102";
	else
		return luaL_error(L, "invalid popup type \"%s\"", type);

	// Can we find a copy of xmessage? Search the path.
	
	char *path = strdup(getenv("PATH"));

	char *current = path;
	
	char *colon;

	int found = 0;

	while (current) {
		colon = strchr(current, ':');
		
		// Clip off the colon.
		*colon++ = 0;
		
		int len = strlen(current);
		char *filename = (char*)malloc(len + 12); // always give excess
		snprintf(filename, len+12, "%s/xmessage", current);
		
		if (access(filename, X_OK) == 0) {
			free(filename);
			found = 1;
			break;
		}
		
		// Failed, move on.
		current = colon;
		free(filename);
		
	}

	free(path);

	// We've found it?
	if (!found)
		goto use_console;

	pid = fork();
	if (pid == 0) {// I'm the virgin sacrifice
	
		// I'm gonna be dead in a matter of microseconds anyways, so wasted memory doesn't matter to me.
		// Go ahead and abuse strdup.
		char * parameters[] = {"xmessage", "-buttons", t, strdup(message), NULL};

		execvp("xmessage", parameters);
		
		// Aw shitty
		perror("exec xmessage");
		exit(1);
	}
	else if (pid < 0) // something went wrong!!! Oh hell... use the console
		goto use_console;
	else {
		// We're the parent. Watch for the child.
		int r;
		int res = waitpid(pid, &r, 0);
		if (res < 0) // wtf?
			goto use_console;
		
		// The return value gets copmlicated...
		if (!WIFEXITED(r)) {
			luaL_error(L, "don't screw with my xmessage process!");
		}
		r = WEXITSTATUS(r);
		
		// We assume it's worked.
		if (r == 0)
		{
			return 0; // no parameters for an OK
		}
		if (r == 100) {
			lua_pushstring(L, "yes");
			return 1;
		}
		if (r == 101) {
			lua_pushstring(L, "no");
			return 1;
		}
		if (r == 102) {
			lua_pushstring(L, "cancel");
			return 1;
		}
		
		// Wtf?
		return luaL_error(L, "popup failed due to unknown results involving xmessage (%d)", r);
	}

use_console:
#endif

	// All else has failed

	if (strcmp(type, "ok") == 0)
		t = "";
	else if (strcmp(type, "yesno") == 0)
		t = "yn";
	else if (strcmp(type, "yesnocancel") == 0)
		t = "ync";
	else
		return luaL_error(L, "invalid popup type \"%s\"", type);

	fprintf(stderr, "Lua Message: %s\n", message);

	while (true) {
		char buffer[64];

		// We don't want parameters
		if (!t[0]) {
			fprintf(stderr, "[Press Enter]");
			fgets(buffer, sizeof(buffer), stdin);
			// We're done
			return 0;

		}
		fprintf(stderr, "(%s): ", t);
		fgets(buffer, sizeof(buffer), stdin);
		
		// Check if the option is in the list
		if (strchr(t, tolower(buffer[0]))) {
			switch (tolower(buffer[0])) {
			case 'y':
				lua_pushstring(L, "yes");
				return 1;
			case 'n':
				lua_pushstring(L, "no");
				return 1;
			case 'c':
				lua_pushstring(L, "cancel");
				return 1;
			default:
				luaL_error(L, "internal logic error in console based prompts for gui.popup");
			
			}
		}
		
		// We fell through, so we assume the user answered wrong and prompt again.
	
	}

	// Nothing here, since the only way out is in the loop.
#endif

}

#ifdef _WIN32
const char* s_keyToName[256] =
{
	NULL,
	"leftclick",
	"rightclick",
	NULL,
	"middleclick",
	NULL,
	NULL,
	NULL,
	"backspace",
	"tab",
	NULL,
	NULL,
	NULL,
	"enter",
	NULL,
	NULL,
	"shift", // 0x10
	"control",
	"alt",
	"pause",
	"capslock",
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	"escape",
	NULL,
	NULL,
	NULL,
	NULL,
	"space", // 0x20
	"pageup",
	"pagedown",
	"end",
	"home",
	"left",
	"up",
	"right",
	"down",
	NULL,
	NULL,
	NULL,
	NULL,
	"insert",
	"delete",
	NULL,
	"0","1","2","3","4","5","6","7","8","9",
	NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	"A","B","C","D","E","F","G","H","I","J",
	"K","L","M","N","O","P","Q","R","S","T",
	"U","V","W","X","Y","Z",
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	"numpad0","numpad1","numpad2","numpad3","numpad4","numpad5","numpad6","numpad7","numpad8","numpad9",
	"numpad*","numpad+",
	NULL,
	"numpad-","numpad.","numpad/",
	"F1","F2","F3","F4","F5","F6","F7","F8","F9","F10","F11","F12",
	"F13","F14","F15","F16","F17","F18","F19","F20","F21","F22","F23","F24",
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	"numlock",
	"scrolllock",
	NULL, // 0x92
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, // 0xB9
	"semicolon",
	"plus",
	"comma",
	"minus",
	"period",
	"slash",
	"tilde",
	NULL, // 0xC1
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, // 0xDA
	"leftbracket",
	"backslash",
	"rightbracket",
	"quote",
};

#endif

// input.get()
// takes no input, returns a lua table of entries representing the current input state,
// independent of the joypad buttons the emulated game thinks are pressed
// for example:
//   if the user is holding the W key and the left mouse button
//   and has the mouse at the bottom-right corner of the game screen,
//   then this would return {W=true, leftclick=true, xmouse=255, ymouse=223}
static int input_getcurrentinputstatus(lua_State *L) {
	lua_newtable(L);

#ifdef _WIN32
	// keyboard and mouse button status
	{
		unsigned char keys [256];
		if(!GUI.BackgroundInput)
		{
			if(GetKeyboardState(keys))
			{
				for(int i = 1; i < 255; i++)
				{
					int mask = (i == VK_CAPITAL || i == VK_NUMLOCK || i == VK_SCROLL) ? 0x01 : 0x80;
					if(keys[i] & mask)
					{
						const char* name = s_keyToName[i];
						if(name)
						{
							lua_pushboolean(L, true);
							lua_setfield(L, -2, name);
						}
					}
				}
			}
		}
		else // use a slightly different method that will detect background input:
		{
			for(int i = 1; i < 255; i++)
			{
				const char* name = s_keyToName[i];
				if(name)
				{
					int active;
					if(i == VK_CAPITAL || i == VK_NUMLOCK || i == VK_SCROLL)
						active = GetKeyState(i) & 0x01;
					else
						active = GetAsyncKeyState(i) & 0x8000;
					if(active)
					{
						lua_pushboolean(L, true);
						lua_setfield(L, -2, name);
					}
				}
			}
		}
	}
	// mouse position in game screen pixel coordinates
	{
		extern BOOL GetCursorPosSNES(LPPOINT lpPoint, bool clip);

		POINT mouse;
		GetCursorPosSNES(&mouse, true);
		if (IPPU.RenderedScreenWidth > SNES_WIDTH)
			mouse.x /= 2;

		lua_pushinteger(L, mouse.x);
		lua_setfield(L, -2, "xmouse");
		lua_pushinteger(L, mouse.y);
		lua_setfield(L, -2, "ymouse");
	}
#else
	// NYI (well, return an empty table)
#endif

	return 1;
}

// int AND(int one, int two, ..., int n)
//
//  Since Lua doesn't provide binary, I provide this function.
//  Does a full binary AND on all parameters and returns the result.
//  A single element is okay, and it's returned straight, but it does you
//  little good.
static int base_AND(lua_State *L) {
	int count = lua_gettop(L);
	
	if (count == 0)
		luaL_error(L, "Binary AND called empty.");
	
	int result = luaL_checkinteger(L,1);
	int i;
	for (i=2; i <= count; i++)
		result &= luaL_checkinteger(L,i);
	lua_settop(L,0);
	lua_pushinteger(L, result);
	return 1;
}


// int OR(int one, int two, ..., int n)
//
//   ..and similarly for a binary OR
static int base_OR(lua_State *L) {
	int count = lua_gettop(L);
	
	if (count == 0)
		luaL_error(L, "Binary OR called empty.");
	
	int result = luaL_checkinteger(L,1);
	int i;
	for (i=2; i <= count; i++)
		result |= luaL_checkinteger(L,i);
	lua_settop(L,0);
	lua_pushinteger(L, result);
	return 1;
}

// int XOR(int one, int two, ..., int n)
//
//   ..and so on
static int base_XOR(lua_State *L) {
	int count = lua_gettop(L);
	
	if (count == 0)
		luaL_error(L, "Binary XOR called empty.");
	
	int result = luaL_checkinteger(L,1);
	int i;
	for (i=2; i <= count; i++)
		result ^= luaL_checkinteger(L,i);
	lua_settop(L,0);
	lua_pushinteger(L, result);
	return 1;
}

static int base_SHIFT(lua_State *L) {
	int num = luaL_checkinteger(L,1);
	int shift = luaL_checkinteger(L,2);
	if(shift < 0)
		num <<= -shift;
	else
		num >>= shift;
	lua_settop(L,0);
	lua_pushinteger(L,num);
	return 1;
}

static int base_BIT(lua_State *L) {
	int bit = luaL_checkinteger(L,1);
	if (bit < 0 || bit > 30)
		luaL_error(L, "range is 0 to 30 inclusive");
	lua_settop(L, 0);
	lua_pushinteger(L, 1 << bit);
	return 1;
}



// The function called periodically to ensure Lua doesn't run amok.
static void S9xLuaHookFunction(lua_State *L, lua_Debug *dbg) {
	
	if (numTries-- == 0) {

		int kill = 0;

#ifdef __WIN32__
		// Uh oh
		int ret = MessageBox(GUI.hWnd, "The Lua script running has been running a long time. It may have gone crazy. Kill it?\n\n(No = don't check anymore either)", "Lua Script Gone Nuts?", MB_YESNO);
		
		if (ret == IDYES) {
			kill = 1;
		}

#else
		fprintf(stderr, "The Lua script running has been running a long time.\nIt may have gone crazy. Kill it? (I won't ask again if you say No)\n");
		char buffer[64];
		while (true) {
			fprintf(stderr, "(y/n): ");
			fgets(buffer, sizeof(buffer), stdin);
			if (buffer[0] == 'y' || buffer[0] == 'Y') {
				kill = 1;
				break;
			}
			
			if (buffer[0] == 'n' || buffer[0] == 'N')
				break;
		}
#endif

		if (kill) {
			luaL_error(L, "Killed by user request.");
			S9xLuaOnStop();
		}

		// else, kill the debug hook.
		lua_sethook(L, NULL, 0, 0);
	}
	

}


static const struct luaL_reg snes9xlib [] = {

	{"speedmode", snes9x_speedmode},
	{"frameadvance", snes9x_frameadvance},
	{"pause", snes9x_pause},
	{"framecount", snes9x_framecount},
	{"lagcount", snes9x_lagcount},
	{"lagged", snes9x_lagged},
	{"registerbefore", snes9x_registerbefore},
	{"registerafter", snes9x_registerafter},
	{"registerexit", snes9x_registerexit},
	{"message", snes9x_message},
	{NULL,NULL}
};

static const struct luaL_reg memorylib [] = {

	{"readbyte", memory_readbyte},
	{"readbytesigned", memory_readbytesigned},
	{"readword", memory_readword},
	{"readwordsigned", memory_readwordsigned},
	{"readdword", memory_readdword},
	{"readdwordsigned", memory_readdwordsigned},
	{"readbyterange", memory_readbyterange},
	{"writebyte", memory_writebyte},
	{"writeword", memory_writeword},
	{"writedword", memory_writedword},
	// alternate naming scheme for word and double-word and unsigned
	{"readbyteunsigned", memory_readbyte},
	{"readwordunsigned", memory_readword},
	{"readdwordunsigned", memory_readdword},
	{"readshort", memory_readword},
	{"readshortunsigned", memory_readword},
	{"readshortsigned", memory_readwordsigned},
	{"readlong", memory_readdword},
	{"readlongunsigned", memory_readdword},
	{"readlongsigned", memory_readdwordsigned},
	{"writeshort", memory_writeword},
	{"writelong", memory_writedword},

	// memory hooks
	{"registerwrite", memory_registerwrite},
	// alternate names
	{"register", memory_registerwrite},

	{NULL,NULL}
};

static const struct luaL_reg joypadlib[] = {
	{"get", joypad_get},
	{"set", joypad_set},
	// alternative names
	{"read", joypad_get},
	{"write", joypad_set},
	{NULL,NULL}
};

static const struct luaL_reg savestatelib[] = {
	{"create", savestate_create},
	{"save", savestate_save},
	{"load", savestate_load},

	{"registersave", savestate_registersave},
	{"registerload", savestate_registerload},
	{"loadscriptdata", savestate_loadscriptdata},

	{NULL,NULL}
};

static const struct luaL_reg movielib[] = {

	{"framecount", snes9x_framecount}, // for those familiar with other emulators that have movie.framecount() instead of emulatorname.framecount()
	{"mode", movie_mode},
	{"rerecordcounting", movie_rerecordcounting},
	{"stop", movie_stop},

	// alternative names
	{"close", movie_stop},
	{NULL,NULL}
};


static const struct luaL_reg guilib[] = {
	{"register", gui_register},
	{"text", gui_text},
	{"box", gui_drawbox},
	{"line", gui_drawline},
	{"pixel", gui_drawpixel},
	{"circle", gui_drawcircle},
	{"opacity", gui_setopacity},
	{"fillbox", gui_fillbox},
	{"fillcircle", gui_fillcircle},
	{"transparency", gui_transparency},
	{"popup", gui_popup},
	{"gdscreenshot", gui_gdscreenshot},
	{"gdoverlay", gui_gdoverlay},
	// alternative names
	{"drawtext", gui_text},
	{"drawbox", gui_drawbox},
	{"drawline", gui_drawline},
	{"drawpixel", gui_drawpixel},
	{"setpixel", gui_drawpixel},
	{"writepixel", gui_drawpixel},
	{"drawcircle", gui_drawcircle},
	{"rect", gui_drawbox},
	{"drawrect", gui_drawbox},
	{"drawimage", gui_gdoverlay},
	{"image", gui_gdoverlay},
	{NULL,NULL}
};

static const struct luaL_reg inputlib[] = {
	{"get", input_getcurrentinputstatus},
	// alternative names
	{"read", input_getcurrentinputstatus},
	{NULL, NULL}
};

void HandleCallbackError(lua_State* L)
{
	if(L->errfunc || L->errorJmp)
		luaL_error(L, "%s", lua_tostring(L,-1));
	else {
		lua_pushnil(LUA);
		lua_setfield(LUA, LUA_REGISTRYINDEX, guiCallbackTable);

		// Error?
#ifdef __WIN32__
		MessageBox( GUI.hWnd, lua_tostring(LUA,-1), "Lua run error", MB_OK | MB_ICONSTOP);
#else
		fprintf(stderr, "Lua thread bombed out: %s\n", lua_tostring(LUA,-1));
#endif

		S9xLuaStop();
	}
}

void CallExitFunction() {
	if (!LUA)
		return;

	lua_settop(LUA, 0);
	lua_getfield(LUA, LUA_REGISTRYINDEX, luaCallIDStrings[LUACALL_BEFOREEXIT]);

	int errorcode = 0;
	if (lua_isfunction(LUA, -1))
	{
		errorcode = lua_pcall(LUA, 0, 0, 0);
	}

	if (errorcode)
		HandleCallbackError(LUA);
}

void CallRegisteredLuaFunctions(LuaCallID calltype)
{
	assert((unsigned int)calltype < (unsigned int)LUACALL_COUNT);
	const char* idstring = luaCallIDStrings[calltype];

	if (!LUA)
		return;

	lua_settop(LUA, 0);
	lua_getfield(LUA, LUA_REGISTRYINDEX, idstring);

	int errorcode = 0;
	if (lua_isfunction(LUA, -1))
	{
		errorcode = lua_pcall(LUA, 0, 0, 0);
		if (errorcode)
			HandleCallbackError(LUA);
	}
	else
	{
		lua_pop(LUA, 1);
	}
}

void S9xLuaFrameBoundary() {

//	printf("Lua Frame\n");

	// HA!
	if (!LUA || !luaRunning)
		return;

	// Our function needs calling
	lua_settop(LUA,0);
	lua_getfield(LUA, LUA_REGISTRYINDEX, frameAdvanceThread);
	lua_State *thread = lua_tothread(LUA,1);	

	// Lua calling C must know that we're busy inside a frame boundary
	frameBoundary = TRUE;
	frameAdvanceWaiting = FALSE;

	lua_joypads_used = 0;

	numTries = 1000;
	int result = lua_resume(thread, 0);
	
	if (result == LUA_YIELD) {
		// Okay, we're fine with that.
	} else if (result != 0) {
		// Done execution by bad causes
		S9xLuaOnStop();
		lua_pushnil(LUA);
		lua_setfield(LUA, LUA_REGISTRYINDEX, frameAdvanceThread);
		lua_pushnil(LUA);
		lua_setfield(LUA, LUA_REGISTRYINDEX, guiCallbackTable);

		// Error?
#ifdef __WIN32__
		MessageBox( GUI.hWnd, lua_tostring(thread,-1), "Lua run error", MB_OK | MB_ICONSTOP);
#else
		fprintf(stderr, "Lua thread bombed out: %s\n", lua_tostring(thread,-1));
#endif
	} else {
		S9xLuaOnStop();
		printf("Script died of natural causes.\n");
	}

	// Past here, the snes actually runs, so any Lua code is called mid-frame. We must
	// not do anything too stupid, so let ourselves know.
	frameBoundary = FALSE;

	if (!frameAdvanceWaiting) {
		S9xLuaOnStop();
	}

}

/**
 * Loads and runs the given Lua script.
 * The emulator MUST be paused for this function to be
 * called. Otherwise, all frame boundary assumptions go out the window.
 *
 * Returns true on success, false on failure.
 */
int S9xLoadLuaCode(const char *filename) {
	if (filename != luaScriptName)
	{
		if (luaScriptName) free(luaScriptName);
		luaScriptName = strdup(filename);
	}

	//stop any lua we might already have had running
	S9xLuaStop();

	// Set current directory from filename (for dofile)
	char dir[_MAX_PATH];
	char *slash, *backslash;
	strcpy(dir, filename);
	slash = strrchr(dir, '/');
	backslash = strrchr(dir, '\\');
	if (!slash || (backslash && backslash < slash))
		slash = backslash;
	if (slash) {
		slash[1] = '\0';    // keep slash itself for some reasons
		_chdir(dir);
	}

	if (!LUA) {
		LUA = lua_open();
		luaL_openlibs(LUA);

		luaL_register(LUA, "snes9x", snes9xlib);
		luaL_register(LUA, "memory", memorylib);
		luaL_register(LUA, "joypad", joypadlib);
		luaL_register(LUA, "savestate", savestatelib);
		luaL_register(LUA, "movie", movielib);
		luaL_register(LUA, "gui", guilib);
		luaL_register(LUA, "input", inputlib);
		lua_settop(LUA, 0); // clean the stack, because each call to luaL_register leaves a table on top

		lua_register(LUA, "AND", base_AND);
		lua_register(LUA, "OR", base_OR);
		lua_register(LUA, "XOR", base_XOR);
		lua_register(LUA, "SHIFT", base_SHIFT);
		lua_register(LUA, "BIT", base_BIT);

		lua_newtable(LUA);
		lua_setfield(LUA, LUA_REGISTRYINDEX, memoryWatchTable);
	}

	// We make our thread NOW because we want it at the bottom of the stack.
	// If all goes wrong, we let the garbage collector remove it.
	lua_State *thread = lua_newthread(LUA);
	
	// Load the data	
	int result = luaL_loadfile(LUA,filename);

	if (result) {
#ifdef __WIN32__
		MessageBox( GUI.hWnd, lua_tostring(LUA,-1), "Lua load error", MB_OK | MB_ICONSTOP);
#else
		fprintf(stderr, "Failed to compile file: %s\n", lua_tostring(LUA,-1));
#endif

		// Wipe the stack. Our thread
		lua_settop(LUA,0);
		return 0; // Oh shit.
	}

	
	// Get our function into it
	lua_xmove(LUA, thread, 1);
	
	// Save the thread to the registry. This is why I make the thread FIRST.
	lua_setfield(LUA, LUA_REGISTRYINDEX, frameAdvanceThread);
	

	// Initialize settings
	luaRunning = TRUE;
	skipRerecords = FALSE;
	transparencyModifier = 255; // opaque
	lua_joypads_used = 0; // not used

	wasPaused = Settings.Paused;
	Settings.Paused = FALSE;

	// And run it right now. :)
	//S9xLuaFrameBoundary();

	// Set up our protection hook to be executed once every 10,000 bytecode instructions.
	lua_sethook(thread, S9xLuaHookFunction, LUA_MASKCOUNT, 10000);

	// We're done.
	return 1;
}

/**
 * Equivalent to repeating the last S9xLoadLuaCode() call.
 */
int S9xReloadLuaCode()
{
	if (!luaScriptName) {
		S9xSetInfoString("There's no script to reload.");
		return 0;
	}
	else
		return S9xLoadLuaCode(luaScriptName);
}

/**
 * Terminates a running Lua script by killing the whole Lua engine.
 *
 * Always safe to call, except from within a lua call itself (duh).
 *
 */
void S9xLuaStop() {
	//already killed
	if (!LUA) return;

	//execute the user's shutdown callbacks
	CallExitFunction();

	//sometimes iup uninitializes com
	//MBG TODO - test whether this is really necessary. i dont think it is
	#ifdef WIN32
	CoInitialize(0);
	#endif

	//lua_gc(LUA,LUA_GCCOLLECT,0);


	lua_close(LUA); // this invokes our garbage collectors for us
	LUA = NULL;
	S9xLuaOnStop();
}

/**
 * Returns true if there is a Lua script running.
 *
 */
int S9xLuaRunning() {
	return LUA && luaRunning;
}


/**
 * Returns true if Lua would like to steal the given joypad control.
 *
 * Range is 0 through 4
 */
int S9xLuaUsingJoypad(int which) {
	return lua_joypads_used & (1 << which);
}

/**
 * Reads the buttons Lua is feeding for the given joypad, in the same
 * format as the OS-specific code.
 *
 * This function must not be called more than once per frame. Ideally exactly once
 * per frame (if S9xLuaUsingJoypad says it's safe to do so)
 */
int S9xLuaReadJoypad(int which) {
	if (lua_joypads_used & (1 << which)) {
		//lua_joypads_used &= ~(1 << which);
		return lua_joypads[which] | 0x80000000;
	}
	else
		return 0; // disconnected
}

/**
 * If this function returns true, the movie code should NOT increment
 * the rerecord count for a load-state.
 *
 * This function will not return true if a script is not running.
 */
bool8 S9xLuaRerecordCountSkip() {
	return LUA && luaRunning && skipRerecords;
}


/**
 * Given a 16 bit screen with the indicated resolution,
 * draw the current GUI onto it.
 *
 * Currently we only support 256x* resolutions.
 */
//static inline int dibPitchOf(int width, int bpp) { return (((width*bpp+31)/8)&~3); }
void S9xLuaGui(void *s, int width, int height, int bpp, int pitch) {

	if (!LUA || !luaRunning)
		return;

	// First, check if we're being called by anybody
	lua_getfield(LUA, LUA_REGISTRYINDEX, guiCallbackTable);
	
	if (lua_isfunction(LUA, -1)) {
		// We call it now
		numTries = 1000;
		int ret = lua_pcall(LUA, 0, 0, 0);
		if (ret != 0) {
			// This is grounds for trashing the function
			// Note: This must be done before the messagebox pops up,
			//       otherwise the messagebox will cause a paint event which causes a weird
			//       infinite call sequence that makes Snes9x silently exit with error code 3,
			//       if a Lua GUI function crashes. (nitsuja)
			lua_pushnil(LUA);
			lua_setfield(LUA, LUA_REGISTRYINDEX, guiCallbackTable);

#ifdef __WIN32__
			MessageBox(GUI.hWnd, lua_tostring(LUA, -1), "Lua Error in GUI function", MB_OK);
#else
			fprintf(stderr, "Lua error in gui.register function: %s\n", lua_tostring(LUA, -1));
#endif
		}
	}

	// And wreak the stack
	lua_settop(LUA, 0);

	if (!gui_used || !gui_enabled)
		return;

	gui_used = FALSE;

	if (width != 256 && width != 512) {
		assert(width == 256 || width == 512);
		return;
	}

	int x, y, x2;
	int xscale = (width == 512) ? 2 : 1;

	switch(bpp)
	{
	case 16:
	 {
		uint16 *screen = (uint16*) s;
		int ppl = pitch/2;
		for (y=0; y < height && y < 239; y++) {
			for (x=0; x < 256; x++) {
				const uint8 gui_alpha = gui_data[(y*256+x)*4+3];
				const uint8 gui_red   = gui_data[(y*256+x)*4+2];
				const uint8 gui_green = gui_data[(y*256+x)*4+1];
				const uint8 gui_blue  = gui_data[(y*256+x)*4];
				int red, green, blue;

				for (x2 = 0; x2 < xscale; x2++) {
					const uint8 scr_red   = ((screen[y*ppl + (x*xscale+x2)] >> 11) & 31) << 3;
					const uint8 scr_green = ((screen[y*ppl + (x*xscale+x2)] >> 5)  & 63) << 2;
					const uint8 scr_blue  = ( screen[y*ppl + (x*xscale+x2)]        & 31) << 3;

					if (gui_alpha == 0) {
						// do nothing
						red = scr_red;
						green = scr_green;
						blue = scr_blue;
					}
					else if (gui_alpha == 255) {
						// direct copy
						red = gui_red;
						green = gui_green;
						blue = gui_blue;
					}
					else {
						// alpha-blending
						red   = (((int) gui_red   - scr_red)   * gui_alpha / 255 + scr_red)   & 255;
						green = (((int) gui_green - scr_green) * gui_alpha / 255 + scr_green) & 255;
						blue  = (((int) gui_blue  - scr_blue)  * gui_alpha / 255 + scr_blue)  & 255;
					}
					screen[y*ppl + (x*xscale+x2)] =  ((red >> 3) << 11) | ((green >> 2) << 5) | (blue >> 3);
				}
			}
		}
		break;
	 }
	case 24:
	 {
		#define bytesPerPixel   3
		uint8 *screen = (uint8*) s;
		for (y=0; y < height && y < 239; y++) {
			for (x=0; x < 256; x++) {
				const uint8 gui_alpha = gui_data[(y*256+x)*4+3];
				const uint8 gui_red   = gui_data[(y*256+x)*4+2];
				const uint8 gui_green = gui_data[(y*256+x)*4+1];
				const uint8 gui_blue  = gui_data[(y*256+x)*4];
				int red, green, blue;

				for (x2 = 0; x2 < xscale; x2++) {
					const uint8 scr_red   = screen[y*pitch + (x*xscale+x2)*bytesPerPixel + 2];
					const uint8 scr_green = screen[y*pitch + (x*xscale+x2)*bytesPerPixel + 1];
					const uint8 scr_blue  = screen[y*pitch + (x*xscale+x2)*bytesPerPixel];

					if (gui_alpha == 0) {
						// do nothing
						red = scr_red;
						green = scr_green;
						blue = scr_blue;
					}
					else if (gui_alpha == 255) {
						// direct copy
						red = gui_red;
						green = gui_green;
						blue = gui_blue;
					}
					else {
						// alpha-blending
						red   = (((int) gui_red   - scr_red)   * gui_alpha / 255 + scr_red)   & 255;
						green = (((int) gui_green - scr_green) * gui_alpha / 255 + scr_green) & 255;
						blue  = (((int) gui_blue  - scr_blue)  * gui_alpha / 255 + scr_blue)  & 255;
					}
					screen[y*pitch + (x*xscale+x2)*bytesPerPixel] = blue;
					screen[y*pitch + (x*xscale+x2)*bytesPerPixel + 1] = green;
					screen[y*pitch + (x*xscale+x2)*bytesPerPixel + 2] = red;
				}
			}
		}
		#undef bytesPerPixel
		break;
	 }
	case 32:
	 {
		#define bytesPerPixel   4
		uint8 *screen = (uint8*) s;
		for (y=0; y < height && y < 239; y++) {
			for (x=0; x < 256; x++) {
				const uint8 gui_alpha = gui_data[(y*256+x)*4+3];
				const uint8 gui_red   = gui_data[(y*256+x)*4+2];
				const uint8 gui_green = gui_data[(y*256+x)*4+1];
				const uint8 gui_blue  = gui_data[(y*256+x)*4];
				int red, green, blue;

				for (x2 = 0; x2 < xscale; x2++) {
					const uint8 scr_red   = screen[y*pitch + (x*xscale+x2)*bytesPerPixel + 2];
					const uint8 scr_green = screen[y*pitch + (x*xscale+x2)*bytesPerPixel + 1];
					const uint8 scr_blue  = screen[y*pitch + (x*xscale+x2)*bytesPerPixel];

					if (gui_alpha == 0) {
						// do nothing
						red = scr_red;
						green = scr_green;
						blue = scr_blue;
					}
					else if (gui_alpha == 255) {
						// direct copy
						red = gui_red;
						green = gui_green;
						blue = gui_blue;
					}
					else {
						// alpha-blending
						red   = (((int) gui_red   - scr_red)   * gui_alpha / 255 + scr_red)   & 255;
						green = (((int) gui_green - scr_green) * gui_alpha / 255 + scr_green) & 255;
						blue  = (((int) gui_blue  - scr_blue)  * gui_alpha / 255 + scr_blue)  & 255;
					}
					screen[y*pitch + (x*xscale+x2)*bytesPerPixel] = blue;
					screen[y*pitch + (x*xscale+x2)*bytesPerPixel + 1] = green;
					screen[y*pitch + (x*xscale+x2)*bytesPerPixel + 2] = red;
				}
			}
		}
		#undef bytesPerPixel
		break;
	 }
	default:
		assert(false /* unsupported color-depth */);
	}
	return;
}

void S9xLuaClearGui() {
	gui_used = false;
}
void S9xLuaEnableGui(bool enabled) {
	gui_enabled = enabled;
}
 
lua_State* S9xGetLuaState() {
	return LUA;
}