#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <string.h>
//#include <unistd.h> // for unlink
#include <ctype.h>
#include <assert.h>

#ifdef __linux
#include <sys/types.h>
#include <sys/wait.h>
#endif


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
}

#include "s9xlua.h"

static lua_State *LUA;

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

// Transparency strength. 0=opaque, 4=so transparent it's invisible
static int transparency;

// Our joypads.
static uint32 lua_joypads[5];
static uint8 lua_joypads_used;


// NON-static. This is a listing of memory addresses we're watching as bitfields
// in order to speed up emulation, at the cost of memory usage. the SNES has
// 128 KB of main RAM, 128 * 1024 / 8 = 16384 entries with bit-level precision.
unsigned char lua_watch_bitfield[16384];


static bool8 gui_used = FALSE;
static uint16 *gui_data = NULL;


// Protects Lua calls from going nuts.
// We set this to a big number like 1000 and decrement it
// over time. The script gets knifed once this reaches zero.
static int numTries;


// Look in snes9x.h for macros named like SNES9X_UP_MASK to determine the order.
static const char *button_mappings[] = {
	"","","","", // These are unused
	
	"R", "L", "X", "A", "right", "left", "down", "up", "start", "select", "Y", "B"
};


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


// snes9x.message(string msg)
//
//  Displays the given message on the screen.
static int snes9x_message(lua_State *L) {

	const char *msg = luaL_checkstring(L,1);
	S9xMessage(S9X_INFO, S9X_MOVIE_INFO, msg);
	
	return 0;

}


/**
 * Rather than write out all the code for the memory library, 
 * I'm just going to abuse the preprocessor. :)
 */

#define memread(x,y) static int memory_##x(lua_State *L) {   lua_pushinteger(L, S9x##y(luaL_checkinteger(L,1), true)); return 1; }
memread(readbyte,GetByte)
memread(readword,GetWord)

#define memwrite(x,y) static int memory_##x(lua_State *L) {   S9x##y(luaL_checkinteger(L,2), luaL_checkinteger(L,1), true); return 0; }
memwrite(writebyte,SetByte)
memwrite(writeword,SetWord)

// Not for the signed versions though
static int memory_readbytesigned(lua_State *L) {
	signed char c = (signed char) S9xGetByte(luaL_checkinteger(L,1), true);
	lua_pushinteger(L, c);
	return 1;
}

static int memory_readwordsigned(lua_State *L) {
	signed short c = (signed short) S9xGetWord(luaL_checkinteger(L,1), true);
	lua_pushinteger(L, c);
	return 1;
}


// memory.register(int address, function func)
//
//  Calls the given function when the indicated memory address is
//  written to. No args are given to the function. The write has already
//  occurred, so the new address is readable.
static int memory_register(lua_State *L) {

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


// table joypad.read(int which = 1)
//
//  Reads the joypads as inputted by the user.
//  This is really the only way to get input to the system.
static int joypad_read(lua_State *L) {

	// Reads the joypads as inputted by the user
	int which = luaL_checkinteger(L,1);
	
	if (which < 1 || which > 5) {
		luaL_error(L,"Invalid input port (valid range 1-5, specified %d)", which);
	}
	
	// Use the OS-specific code to do the reading.
	uint32 buttons = S9xReadJoypad(which - 1);
	
	lua_newtable(L);
	
	int i;
	for (i = 4; i < 16; i++) {
		if (buttons & (1<<i)) {
			lua_pushinteger(L,1);
			lua_setfield(L, -2, button_mappings[i]);
		}
	}
	
	return 1;
}


// joypad.write(int which, table buttons)
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
	for (i=4; i < 16; i++) {
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
	
	// We exit, and the garbage collector takes care of the rest.
	// Edit: Visual Studio needs a return value anyway, so returns 0.
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


// int movie.framecount()
//
//   Gets the frame counter for the movie, or nil if no movie running.
int movie_framecount(lua_State *L) {
	if (!S9xMovieActive()) {
		lua_pushnil(L);
		return 1;
	}
	
	lua_pushinteger(L, S9xMovieGetFrameCounter());
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
		gui_data = (uint16*)malloc(256*239*sizeof(uint16));
	if (!gui_used) {
		for (int i = 0; i < 256*239; i++)
			gui_data[i] = 1;
	}
	gui_used = TRUE;
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

/**
 * Converts an integer or a string on the stack at the given
 * offset to a RGB16 (565) colour. Several encodings are supported.
 * The user may construct their own RGB16 value, given a simple colour name,
 * or an HTML-style "#09abcd" colour. Of course, 16 bit reduction occurs.
 */
static uint16 gui_getcolour(lua_State *L, int offset) {
	switch (lua_type(L,offset)) {
	case LUA_TSTRING:
		{
			const char *str = lua_tostring(L,offset);
			if (strcmp(str,"red")==0) {
				return 31 << 11;
			} else if (strcmp(str, "green")==0) {
				return 63 << 5;
			} else if (strcmp(str, "blue")==0) {
				return 31;
			} else if (strcmp(str, "black")==0) {
				return 0;
			} else if (strcmp(str, "white")==0) {
				return 65535;
			} else if (strcmp(str, "clear")==0) {
				return 1;
			} else if (str[0] == '#' && strlen(str) == 7) {
				int red, green, blue;
				red = (hex2int(L, str[1]) << 4) | hex2int(L, str[2]);
				green = (hex2int(L, str[3]) << 4) | hex2int(L, str[4]);
				blue = (hex2int(L, str[5]) << 4) | hex2int(L, str[6]);

				int assembled = (red >> 3) << 11;
				assembled |= (green >> 2) << 5;
				assembled |= blue >> 3;
				return assembled;
			} else
				return luaL_error(L, "unknown colour %s", str);

		}
	case LUA_TNUMBER:
		return (uint16) lua_tointeger(L,offset);
	default:
		return luaL_error(L, "invalid colour");
	}

}

// I'm going to use this a lot in here

template <class T> static void swap(T &one, T &two) {
	T temp = one;
	one = two;
	two = temp;
}

// gui.drawpixel(x,y,colour)
static int gui_drawpixel(lua_State *L) {

	int x = luaL_checkinteger(L, 1);
	int y = luaL_checkinteger(L,2);

	uint16 colour = gui_getcolour(L,3);

	if (x < 0 || x >= 256 || y < 0 || y >= 239)
		luaL_error(L,"bad coordinates");

	gui_prepare();

	gui_data[y*256 + x] = colour;

	return 0;
}

// gui.drawline(x1,y1,x2,y2,type colour)
static int gui_drawline(lua_State *L) {

	int x1,y1,x2,y2;
	uint16 colour;
	x1 = luaL_checkinteger(L,1);
	y1 = luaL_checkinteger(L,2);
	x2 = luaL_checkinteger(L,3);
	y2 = luaL_checkinteger(L,4);
	colour = gui_getcolour(L,5);

	if (x1 < 0 || x1 >= 256 || y1 < 0 || y1 >= 239)
		luaL_error(L,"bad coordinates");

	if (x2 < 0 || x2 >= 256 || y2 < 0 || y2 >= 239)
		luaL_error(L,"bad coordinates");

	gui_prepare();


	// Horizontal line?
	if (y1 == y2) {
		if (x1 > x2) 
			swap<int>(x1,x2);
		int i;
		for (i=x1; i <= x2; i++)
			gui_data[y1*256+i] = colour;
	} else if (x1 == x2) { // Vertical line?
		if (y1 > y2)
			swap<int>(y1,y2);
		int i;
		for (i=y1; i < y2; i++)
			gui_data[i*256+x1] = colour;
	} else {
		// Some very real slope. We want to increase along the x value, so we swap for that.
		if (x1 > x2) {
			swap<int>(x1,x2);
			swap<int>(y1,y2);
		}


		double slope = ((double)y2-(double)y1) / ((double)x2-(double)x1);
		int myX = x1, myY = y1;
		double accum = 0;

		while (myX <= x2) {
			// Draw the current pixel
			gui_data[myY*256 + myX] = colour;

			// If it's above 1, we knock 1 off it and go up 1 pixel
			if (accum >= 1.0) {
				myY += 1;
				accum -= 1.0;
			} else {
				myX += 1;
				accum += slope; // Step up

			}
		}


	}

	return 0;
}

// gui.drawbox(x1, y1, x2, y2, colour)
static int gui_drawbox(lua_State *L) {

	int x1,y1,x2,y2;
	uint16 colour;
	int i;

	x1 = luaL_checkinteger(L,1);
	y1 = luaL_checkinteger(L,2);
	x2 = luaL_checkinteger(L,3);
	y2 = luaL_checkinteger(L,4);
	colour = gui_getcolour(L,5);

	if (x1 < 0 || x1 >= 256 || y1 < 0 || y1 >= 239)
		luaL_error(L,"bad coordinates");

	if (x2 < 0 || x2 >= 256 || y2 < 0 || y2 >= 239)
		luaL_error(L,"bad coordinates");


	gui_prepare();

	// For simplicity, we mandate that x1,y1 be the upper-left corner
	if (x1 > x2)
		swap<int>(x1,x2);
	if (y1 > y2)
		swap<int>(y1,y2);

	// top surface
	for (i=x1; i <= x2; i++)
		gui_data[y1*256 + i] = colour;

	// bottom surface
	for (i=x1; i <= x2; i++)
		gui_data[y2*256 + i] = colour;

	// left surface
	for (i=y1; i <= y2; i++)
		gui_data[i*256+x1] = colour;

	// right surface
	for (i=y1; i <= y2; i++)
		gui_data[i*256+x2] = colour;


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


// gui.transparency(int strength)
//
//  0 = solid, 
static int gui_transparency(lua_State *L) {
	int trans = luaL_checkinteger(L,1);
	if (trans < 0 || trans > 4) {
		luaL_error(L, "transparency out of range");
	}
	
	transparency = trans;
	return 0;
}


/// This code is copy/pasted from gfx.cpp. It's modified to save to our framebuffer rather than
// the user's actual screen. As a result, a lot of modification is done. Support for sacling was dropped.

#include "font.h"

static inline void FontPixToScreen(char p, uint16 *s)
{
	if(p == '#')
	{
		*s = Settings.DisplayColor;
	}
	else if(p == '.')
	{
		static const uint16 black = BUILD_PIXEL(0,0,0);
		*s = black;
	}
}

static void DisplayChar(uint16 *s, uint8 c) {
	extern int font_height, font_width;
	int line = ((c - 32) >> 4) * font_height;
	int offset = ((c - 32) & 15) * font_width;
	int h, w;
	for(h=0; h<font_height; h++, line++, s+=256-font_width)
		for(w=0; w<font_width; w++, s++)
			FontPixToScreen(font [line] [(offset + w)], s);
}

static void LuaDisplayString (const char *string, int linesFromTop, int pixelsFromLeft)
{

	extern int font_width, font_height;

	uint16 *Screen = gui_data // text draw position, starting on the screen
                  + pixelsFromLeft // with this much horizontal offset
                  + linesFromTop*256;

    int len = strlen(string);
    int max_chars = (256-pixelsFromLeft) / (font_width);
    int char_count = 0;

	// loop through and draw the characters
	for(int i = 0 ; i < len && char_count < max_chars ; i++, char_count++)
	{
		if((unsigned char) string[i]<32) continue;

		DisplayChar(Screen, string[i]);
		Screen += /*Settings.SixteenBit ? (display_fontwidth-display_hfontaccessscale)*sizeof(uint16) :*/ (font_width);
	}
}


// gui.text(int x, int y, string msg)
//
//  Displays the given text on the screen, using the same font and techniques as the
//  main HUD.
static int gui_text(lua_State *L) {
	extern int font_height;
	const char *msg;
	int x, y;

	x = luaL_checkinteger(L,1);
	y = luaL_checkinteger(L,2);
	msg = luaL_checkstring(L,3);

	if (x < 0 || x >= 256 || y < 0 || y >= (239 - font_height))
		luaL_error(L,"bad coordinates");

	gui_prepare();

	LuaDisplayString(msg, y, x);

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
	if (size < (11+ width*height*4))
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

			int red = pixels[4 * (y*width+x)+1];
			int green = pixels[4 * (y*width+x)+2];
			int blue = pixels[4 * (y*width+x)+3];
			gui_data[256*(sy)+sx] = ((red >> 3) << 11) | ((green >> 2)<<5) | (blue >> 3);
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
		}

		// else, kill the debug hook.
		lua_sethook(L, NULL, 0, 0);
	}
	

}


static const struct luaL_reg snes9xlib [] = {

	{"speedmode", snes9x_speedmode},
	{"frameadvance", snes9x_frameadvance},
	{"pause", snes9x_pause},

	{"message", snes9x_message},
	{NULL,NULL}
};

static const struct luaL_reg memorylib [] = {

	{"readbyte", memory_readbyte},
	{"writebyte", memory_writebyte},
	{"readword", memory_readword},
	{"writeword", memory_writeword},

	{"readwordsigned", memory_readwordsigned},
	{"readbytesigned", memory_readbytesigned},

	{"register", memory_register},

	{NULL,NULL}
};

static const struct luaL_reg joypadlib[] = {
	{"read", joypad_read},
	{"set", joypad_set},

	{NULL,NULL}
};

static const struct luaL_reg savestatelib[] = {
	{"create", savestate_create},
	{"save", savestate_save},
	{"load", savestate_load},

	{NULL,NULL}
};

static const struct luaL_reg movielib[] = {

	{"framecount", movie_framecount},
	{"mode", movie_mode},
	{"rerecordcounting", movie_rerecordcounting},
	{"stop", movie_stop},
//	{"record", movie_record},
//	{"playback", movie_playback},

	{NULL,NULL}

};


static const struct luaL_reg guilib[] = {
	
	{"drawpixel", gui_drawpixel},
	{"drawline", gui_drawline},
	{"drawbox", gui_drawbox},
	{"text", gui_text},

	{"gdscreenshot", gui_gdscreenshot},
	{"gdoverlay", gui_gdoverlay},
	{"transparency", gui_transparency},

	{"register", gui_register},
	
	{"popup", gui_popup},
	{NULL,NULL}

};


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

	numTries = 1000;
	int result = lua_resume(thread, 0);
	
	if (result == LUA_YIELD) {
		// Okay, we're fine with that.
	} else if (result != 0) {
		// Done execution by bad causes
		luaRunning = FALSE;
		lua_pushnil(LUA);
		lua_setfield(LUA, LUA_REGISTRYINDEX, frameAdvanceThread);
		
		// Error?
#ifdef __WIN32__
		MessageBox( GUI.hWnd, lua_tostring(thread,-1), "Lua run error", MB_OK | MB_ICONSTOP);
#else
		fprintf(stderr, "Lua thread bombed out: %s\n", lua_tostring(thread,-1));
#endif
		if (wasPaused)
			Settings.Paused = TRUE;

	} else {
		printf("Script died of natural causes.\n");
		luaRunning = FALSE;
		if (wasPaused)
			Settings.Paused = TRUE;
	}

	// Past here, the snes actually runs, so any Lua code is called mid-frame. We must
	// not do anything too stupid, so let ourselves know.
	frameBoundary = FALSE;

	if (!frameAdvanceWaiting) {
		// Erase running state
		memset(lua_watch_bitfield, 0, sizeof(lua_watch_bitfield));
		luaRunning = FALSE;
#ifdef __WIN32__
		MENUITEMINFO mii;
		unsigned int i;

		ZeroMemory( &mii, sizeof( mii));
		mii.cbSize = sizeof( mii);
		mii.fMask = MIIM_STATE;
  
		mii.fState = MFS_UNCHECKED;
		SetMenuItemInfo (GUI.hMenu, IDD_FILE_LUA_LOAD, FALSE, &mii);
		mii.fState |= MFS_DISABLED;
		SetMenuItemInfo (GUI.hMenu, IDD_FILE_LUA_STOP, FALSE, &mii);
#endif
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
	if (!LUA) {
		LUA = lua_open();
	        luaL_openlibs(LUA);

		luaL_register(LUA, "snes9x", snes9xlib);
		luaL_register(LUA, "memory", memorylib);
		luaL_register(LUA, "joypad", joypadlib);
		luaL_register(LUA, "savestate", savestatelib);
		luaL_register(LUA, "movie", movielib);
		luaL_register(LUA, "gui", guilib);

		lua_pushcfunction(LUA, base_AND);
		lua_setfield(LUA, LUA_GLOBALSINDEX, "AND");
		lua_pushcfunction(LUA, base_OR);
		lua_setfield(LUA, LUA_GLOBALSINDEX, "OR");
		lua_pushcfunction(LUA, base_XOR);
		lua_setfield(LUA, LUA_GLOBALSINDEX, "XOR");
		lua_pushcfunction(LUA, base_BIT);
		lua_setfield(LUA, LUA_GLOBALSINDEX, "BIT");

		
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
 * Terminates a running Lua script by killing the whole Lua engine.
 *
 * Always safe to call, except from within a lua call itself (duh).
 *
 */
void S9xLuaStop() {

	// Kill it.
	if (LUA) {
		lua_close(LUA); // this invokes our garbage collectors for us
		LUA = NULL;
		luaRunning = 0;
		if (wasPaused)
			Settings.Paused = TRUE;
		memset(lua_watch_bitfield, 0, sizeof(lua_watch_bitfield));
	}

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
	lua_joypads_used &= ~(1 << which);
	return lua_joypads[which];
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
void S9xLuaGui(uint16 *screen, int ppl, int width, int height) {

	if (!LUA || !luaRunning)
		return;

	// First, check if we're being called by anybody
	lua_getfield(LUA, LUA_REGISTRYINDEX, guiCallbackTable);
	
	if (lua_isfunction(LUA, -1)) {
		// We call it now
		numTries = 1000;
		int ret = lua_pcall(LUA, 0, 0, 0);
		if (ret != 0) {
#ifdef __WIN32__
			MessageBox(GUI.hWnd, lua_tostring(LUA, -1), "Lua Error in GUI function", MB_OK);
#else
			fprintf(stderr, "Lua error in gui.register function: %s\n", lua_tostring(LUA, -1));
#endif
			// This is grounds for trashing the function
			lua_pushnil(LUA);
			lua_setfield(LUA, LUA_REGISTRYINDEX, guiCallbackTable);
		
		}
	}

	// And wreak the stack
	lua_settop(LUA, 0);

	if (!gui_used)
		return;

	gui_used = FALSE;

	int x,y;

	if (transparency == 4) // wtf?
		return;
	
	if (width == 256) {
		// direct copy
		if (transparency == 0) {
			for (y=0; y < height && y < 239; y++) {
				for (x=0; x < 256; x++) {
					if (gui_data[y*256+x] != 1)
						screen[y*ppl + x] =  gui_data[y*256+x];
				}
			}	
		} else {
			int strans = 4 - transparency;
			for (y=0; y < height && y < 239; y++) {
				for (x=0; x < 256; x++) {
					if (gui_data[y*256+x] != 1) {
						int red, green, blue;
						red = ((gui_data[y*256+x] >> 11) & 31) * transparency + ((screen[y*ppl + x] >> 11) & 31) * strans;
						green = ((gui_data[y*256+x] >> 5) & 63) * transparency + ((screen[y*ppl + x] >> 5) & 63) * strans;
						blue = ((gui_data[y*256+x] >> 11)) * transparency + ((screen[y*ppl + x]) & 31) * strans;


						screen[y*ppl + x] =  ((red/4) << 11) | ((green/4) << 5) | (blue);
					}
				}
			}	
		
		
		}

	} else {
		// Now it gets ugly
		
		// This WAS going to be an image scaling function. Maybe that's not
		// such a hot idea right now. Certainly not how I was going to do it.
/*		double xd, yd;
		double dwidth = (double)width, dheight = (double)height;

		for (y = 0; y < height; y++) {
			yd = (double) y / dheight;
			for (x = 0; x < width; x++) {
			}
		}*/
		// Maybe I just won't do this.
	}

	return;
}