/**********************************************************************************
  Snes9x - Portable Super Nintendo Entertainment System (TM) emulator.

  (c) Copyright 1996 - 2002  Gary Henderson (gary.henderson@ntlworld.com),
                             Jerremy Koot (jkoot@snes9x.com)

  (c) Copyright 2002 - 2004  Matthew Kendora

  (c) Copyright 2002 - 2005  Peter Bortas (peter@bortas.org)

  (c) Copyright 2004 - 2005  Joel Yliluoma (http://iki.fi/bisqwit/)

  (c) Copyright 2001 - 2006  John Weidman (jweidman@slip.net)

  (c) Copyright 2002 - 2006  funkyass (funkyass@spam.shaw.ca),
                             Kris Bleakley (codeviolation@hotmail.com)

  (c) Copyright 2002 - 2007  Brad Jorsch (anomie@users.sourceforge.net),
                             Nach (n-a-c-h@users.sourceforge.net),
                             zones (kasumitokoduck@yahoo.com)

  (c) Copyright 2006 - 2007  nitsuja


  BS-X C emulator code
  (c) Copyright 2005 - 2006  Dreamer Nom,
                             zones

  C4 x86 assembler and some C emulation code
  (c) Copyright 2000 - 2003  _Demo_ (_demo_@zsnes.com),
                             Nach,
                             zsKnight (zsknight@zsnes.com)

  C4 C++ code
  (c) Copyright 2003 - 2006  Brad Jorsch,
                             Nach

  DSP-1 emulator code
  (c) Copyright 1998 - 2006  _Demo_,
                             Andreas Naive (andreasnaive@gmail.com)
                             Gary Henderson,
                             Ivar (ivar@snes9x.com),
                             John Weidman,
                             Kris Bleakley,
                             Matthew Kendora,
                             Nach,
                             neviksti (neviksti@hotmail.com)

  DSP-2 emulator code
  (c) Copyright 2003         John Weidman,
                             Kris Bleakley,
                             Lord Nightmare (lord_nightmare@users.sourceforge.net),
                             Matthew Kendora,
                             neviksti


  DSP-3 emulator code
  (c) Copyright 2003 - 2006  John Weidman,
                             Kris Bleakley,
                             Lancer,
                             z80 gaiden

  DSP-4 emulator code
  (c) Copyright 2004 - 2006  Dreamer Nom,
                             John Weidman,
                             Kris Bleakley,
                             Nach,
                             z80 gaiden

  OBC1 emulator code
  (c) Copyright 2001 - 2004  zsKnight,
                             pagefault (pagefault@zsnes.com),
                             Kris Bleakley,
                             Ported from x86 assembler to C by sanmaiwashi

  SPC7110 and RTC C++ emulator code
  (c) Copyright 2002         Matthew Kendora with research by
                             zsKnight,
                             John Weidman,
                             Dark Force

  S-DD1 C emulator code
  (c) Copyright 2003         Brad Jorsch with research by
                             Andreas Naive,
                             John Weidman

  S-RTC C emulator code
  (c) Copyright 2001-2006    byuu,
                             John Weidman

  ST010 C++ emulator code
  (c) Copyright 2003         Feather,
                             John Weidman,
                             Kris Bleakley,
                             Matthew Kendora

  Super FX x86 assembler emulator code
  (c) Copyright 1998 - 2003  _Demo_,
                             pagefault,
                             zsKnight,

  Super FX C emulator code
  (c) Copyright 1997 - 1999  Ivar,
                             Gary Henderson,
                             John Weidman

  Sound DSP emulator code is derived from SNEeSe and OpenSPC:
  (c) Copyright 1998 - 2003  Brad Martin
  (c) Copyright 1998 - 2006  Charles Bilyue'

  SH assembler code partly based on x86 assembler code
  (c) Copyright 2002 - 2004  Marcus Comstedt (marcus@mc.pp.se)

  2xSaI filter
  (c) Copyright 1999 - 2001  Derek Liauw Kie Fa

  HQ2x, HQ3x, HQ4x filters
  (c) Copyright 2003         Maxim Stepin (maxim@hiend3d.com)

  Win32 GUI code
  (c) Copyright 2003 - 2006  blip,
                             funkyass,
                             Matthew Kendora,
                             Nach,
                             nitsuja

  Mac OS GUI code
  (c) Copyright 1998 - 2001  John Stiles
  (c) Copyright 2001 - 2007  zones


  Specific ports contains the works of other authors. See headers in
  individual files.


  Snes9x homepage: http://www.snes9x.com

  Permission to use, copy, modify and/or distribute Snes9x in both binary
  and source form, for non-commercial purposes, is hereby granted without
  fee, providing that this license information and copyright notice appear
  with all copies and any derived work.

  This software is provided 'as-is', without any express or implied
  warranty. In no event shall the authors be held liable for any damages
  arising from the use of this software or it's derivatives.

  Snes9x is freeware for PERSONAL USE only. Commercial users should
  seek permission of the copyright holders first. Commercial use includes,
  but is not limited to, charging money for Snes9x or software derived from
  Snes9x, including Snes9x or derivatives in commercial game bundles, and/or
  using Snes9x as a promotion for your commercial product.

  The copyright holders request that bug fixes and improvements to the code
  should be forwarded to them so everyone can benefit from the modifications
  in future versions.

  Super NES and Super Nintendo Entertainment System are trademarks of
  Nintendo Co., Limited and its subsidiary companies.
**********************************************************************************/



#include "../port.h"
#include "../snes9x.h"
#include "../memmap.h"
#include "../apu.h"
#include "../soundux.h"

#include "wsnes9x.h"
#include "directx.h"
#include "WAVOutput.h"
#include "AVIOutput.h"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

uint8 *FrameSound = NULL;               // samples of last frame (only used in avi recording)
static uint8 *SoundBuffer = NULL;       // sound buffer shadow
size_t FrameSoundWritten = 0;           // sample count for AVIAddSoundSamples

static bool pending_setup = false;      // true to setup sound in sound timer proc
static long pending_rate;               // 
static bool pending_16bit;              // 
static bool pending_stereo;             // 

unsigned long _interval;        // 
long _buffernos;                // 
long _blocksize;                // 
long _samplecount;              // 
long _buffersize;               // 
DWORD _lastblock;               // 
bool StartPlaying = false;

static bool block_signal = false;               // true to block any actions in ProcessSound
static volatile bool pending_signal = false;    // indicates if ProcessSound was called when block_signal is true

/*********************************************************************************/

// S9xMixSamples (output size can be greater than SOUND_BUFFER_SIZE)
void S9xMixSamplesEx(uint8 *buffer, int sample_count)
{
	const int _maxsamplecount = so.sixteen_bit ? (SOUND_BUFFER_SIZE / 2) : SOUND_BUFFER_SIZE;
	const int stereo_multiplier = so.stereo ? 2 : 1;
	int byte_offset = 0;

	if(sample_count < 1)
		return;

	while(sample_count > _maxsamplecount)
	{
		S9xMixSamples(&buffer[byte_offset], _maxsamplecount);
		sample_count -= _maxsamplecount;
		byte_offset += SOUND_BUFFER_SIZE;
	}
	S9xMixSamples(&buffer[byte_offset], sample_count);
}

/*********************************************************************************/

bool FlexibleSoundMixMode(void)
{
	if (GUI.AVIOut || GUI.WAVOut)
		return false;
	else
		return GUI.FlexibleSoundMixMaster;
}

/*********************************************************************************/

void S9xWinInitSound(void)
{
	S9xInitAPU();
	S9xInitSound (7, true, _blocksize);

	so.playback_rate = Settings.SoundPlaybackRate;
	so.stereo = Settings.Stereo;
	so.sixteen_bit = Settings.SixteenBitSound;
	so.buffer_size = _blocksize;

	// Sound options
	Settings.SoundBufferSize = Settings.SoundBufferSize;

	if (FrameSound)
	{
		delete [] FrameSound;
		FrameSound = NULL;
	}
	FrameSound = new uint8 [48000 * 4 / 50];
	ZeroMemory(FrameSound, 48000 * 4 / 50);
	FrameSoundWritten = 0;
}

void S9xWinDeinitSound(void)
{
	if (SoundBuffer)
	{
		SoundBuffer -= 32 * 1024;
		delete [] SoundBuffer;
		SoundBuffer = NULL;
	}

	if (FrameSound)
	{
		delete [] FrameSound;
		FrameSound = NULL;
	}
}

bool S9xWinIsSoundActive(void)
{
	return (SoundBuffer != NULL);
}

/*********************************************************************************/

// needs convert if main thread is in between WinPreSave and WinPostSave
// TODO: block signal when saving/loading config instead?
uint32 CorrectSoundPlaybackRate(uint32 rate)
{
	switch(rate){
		case 0  : return 0;
		case 1  : return 8000;
		case 2  : return 11025;
		case 3  : return 16000;
		case 4  : return 22050;
		case 5  : return 30000;
		case 6  : return 32000;
		case 7  : return 35000;
		case 8  : return 44100;
		case 9  : return 48000;
		default : return rate;
	}
}

/*********************************************************************************/

bool8 SetupSound (long rate, bool8 sixteen_bit, bool8 stereo)
{
	pending_setup = TRUE;
	pending_rate = rate;
	pending_16bit = sixteen_bit;
	pending_stereo = stereo;
	return (TRUE);
}

bool8 RealSetupSound (long rate, bool8 sixteen_bit, bool8 stereo)
{
	so.mute_sound = TRUE;
	if (SoundBuffer)
	{
		SoundBuffer -= 32 * 1024;
		delete SoundBuffer;
		SoundBuffer = NULL;
	}

	_interval = Settings.SoundMixInterval;

	if (Settings.SoundBufferSize < 1)
		Settings.SoundBufferSize = 1;
	if (Settings.SoundBufferSize > 64)
		Settings.SoundBufferSize = 64;

	_buffernos = 4 * Settings.SoundBufferSize;
	int s = (rate * _interval * (Settings.Stereo ? 2 : 1) * (Settings.SixteenBitSound ? 2 : 1)) / 1000;

	_blocksize = 64;
	while (_blocksize < s)
		_blocksize *= 2;

	_buffersize = _blocksize * _buffernos;
	_lastblock = 0;

	StartPlaying = false;

	so.playback_rate = CorrectSoundPlaybackRate(Settings.SoundPlaybackRate);
	so.stereo = Settings.Stereo;
	so.sixteen_bit = Settings.SixteenBitSound;
	so.buffer_size = _blocksize;
	so.encoded = FALSE;

	if (!DirectX.DSAvailable)
		return (false);

	if (DirectX.SetSoundMode ())
	{
		SoundBuffer = new uint8 [_blocksize * _buffernos + 1024 * 64];
		ZeroMemory (SoundBuffer, _blocksize * _buffernos + 1024 * 64);
		SoundBuffer += 32 * 1024;
		S9xSetPlaybackRate (so.playback_rate);
	}

	_samplecount = so.sixteen_bit ? _blocksize / 2 : _blocksize;

	so.samples_mixed_so_far = 0;
	so.mute_sound = FALSE;

	FrameSoundWritten = 0;

	return (SoundBuffer != NULL);
}

#define FIXED_POINT 0x10000
#define FIXED_POINT_SHIFT 16
#define FIXED_POINT_REMAINDER 0xffff

void ProcessSound ();

EXTERN_C void S9xGenerateSound(void)
{
	if (!S9xWinIsSoundActive())
		return;

	// FIXME: without this, audio samples may be noisy...
	if (so.samples_mixed_so_far >= _samplecount && FlexibleSoundMixMode())
		return;

	block_signal = TRUE;
	so.err_counter += so.err_rate;
	if (so.err_counter >= FIXED_POINT)
	{
		const int stereo_multiplier = so.stereo ? 2 : 1;
		const int byte_count = so.sixteen_bit ? 2 : 1;
		const int framerate = Memory.ROMFramesPerSecond;
//		const int frameskip = (Settings.SkipFrames == AUTO_FRAMERATE) ? 1 : (Settings.SkipFrames + 1);
//		const int sound_samples_per_update = (so.playback_rate * frameskip) / framerate;
		const int sound_samples_per_update = so.playback_rate / framerate;
		int sample_count = so.err_counter >> FIXED_POINT_SHIFT;
		int sample_count_for_SoundBuffer;
		int byte_offset_FrameSound;
		int byte_offset;

		so.err_counter &= FIXED_POINT_REMAINDER;

		// limit sample count per frame (just in case)
		if (FrameSoundWritten + sample_count > sound_samples_per_update)
		{
			sample_count = sound_samples_per_update - FrameSoundWritten;
		}
		sample_count *= stereo_multiplier;

		if (sample_count == 0)
			goto finishGenerateSound;

		if (so.samples_mixed_so_far >= _samplecount)
			sample_count_for_SoundBuffer = 0;
		else if (so.samples_mixed_so_far + sample_count > _samplecount)
			sample_count_for_SoundBuffer = _samplecount - so.samples_mixed_so_far;
		else
			sample_count_for_SoundBuffer = sample_count;

		byte_offset = so.samples_mixed_so_far * byte_count;
		byte_offset_FrameSound = FrameSoundWritten * stereo_multiplier * byte_count;

		S9xMixSamplesEx (&FrameSound[byte_offset_FrameSound], sample_count);
		FrameSoundWritten += sample_count / stereo_multiplier;
		if (sample_count_for_SoundBuffer)
		{
			memcpy(&SoundBuffer[byte_offset], &FrameSound[byte_offset_FrameSound], sample_count_for_SoundBuffer * byte_count);
			so.samples_mixed_so_far += sample_count_for_SoundBuffer;
		}
	}
finishGenerateSound:
	block_signal = FALSE;
	if (pending_signal)
	{
		ProcessSound ();
		pending_signal = FALSE;
	}
}

EXTERN_C void S9xGenerateFrameSound(void)
{
	so.err_counter = 0;

	if (!S9xWinIsSoundActive() || FlexibleSoundMixMode())
	{
		FrameSoundWritten = 0;
		return;
	}

	block_signal = TRUE;

	if (Settings.SoundSync == 0)
		FrameSoundWritten = 0;

	const int stereo_multiplier = so.stereo ? 2 : 1;
	const int byte_count = so.sixteen_bit ? 2 : 1;
	const int framerate = Memory.ROMFramesPerSecond;
//	const int frameskip = (Settings.SkipFrames == AUTO_FRAMERATE) ? 1 : (Settings.SkipFrames + 1);
//	const int sound_samples_per_update = (so.playback_rate * frameskip) / framerate;
	const int sound_samples_per_update = so.playback_rate / framerate;
	const int byte_offset = FrameSoundWritten * byte_count * stereo_multiplier;
	const int byte_offset_FrameSound = FrameSoundWritten * stereo_multiplier * byte_count;
	int sample_count = sound_samples_per_update;
	int sample_count_for_SoundBuffer;

	if (Settings.SoundSync == 1)
	{
		// make sound length compatible with SoundSync == 0
		sample_count = sound_samples_per_update - FrameSoundWritten;
		if (sample_count < 0)
			sample_count = 0; // just in case

		so.err_counter = 0;
	}
	sample_count *= stereo_multiplier;

	if (so.samples_mixed_so_far >= _samplecount)
		sample_count_for_SoundBuffer = 0;
	else if (so.samples_mixed_so_far + sample_count > _samplecount)
		sample_count_for_SoundBuffer = _samplecount - so.samples_mixed_so_far;
	else
		sample_count_for_SoundBuffer = sample_count;

	S9xMixSamplesEx (&FrameSound[byte_offset_FrameSound], sample_count);
	FrameSoundWritten += sample_count / stereo_multiplier;
	if (sample_count_for_SoundBuffer)
	{
		memcpy(&SoundBuffer[byte_offset], &FrameSound[byte_offset_FrameSound], sample_count_for_SoundBuffer * byte_count);
		so.samples_mixed_so_far += sample_count_for_SoundBuffer;
	}

//	if (Settings.Mute)
//		SecureZeroMemory(FrameSound, FrameSoundWritten * stereo_multiplier * byte_count);
	if (GUI.WAVOut)
		WAVAddSoundSamples(FrameSound, FrameSoundWritten, GUI.WAVOut);
	if (GUI.AVIOut)
		AVIAddSoundSamples(FrameSound, FrameSoundWritten, GUI.AVIOut);
//	if (!Settings.Mute)
		SecureZeroMemory(FrameSound, FrameSoundWritten * stereo_multiplier * byte_count);
	FrameSoundWritten = 0;

	block_signal = FALSE;
	if (pending_signal)
	{
		ProcessSound ();
		pending_signal = FALSE;
	}
}

// Interval ms has passed
void ProcessSound (void)
{
	extern bool freezing_to_stream;
	extern bool unfreezing_from_stream;

#ifdef DEBUG_MK_APU
	static FILE* fp;
#endif
	if (block_signal || freezing_to_stream || unfreezing_from_stream)
	{
		pending_signal = TRUE;
		return;
	}

	// if this function is called by main thread,
	// kick another call from Windows timer
	block_signal = TRUE;

	if (pending_setup)
	{
		pending_setup = false;
		RealSetupSound (pending_rate, pending_16bit, pending_stereo);
	}

	if (!DirectX.lpDSB || !SoundBuffer)
		goto finishProcessSound;

	bool8 mute = (DirectX.IdleCount >= GUI.PausedFramesBeforeMutingSound
		|| Settings.Mute || Settings.ForcedPause
		|| Settings.Paused && (GUI.FAMute || !(Settings.FrameAdvance) && !(DirectX.IdleCount < 8)) // gives frame advance sound
		|| Settings.StopEmulation);

	if (StartPlaying)
	{
		if (!SUCCEEDED(DirectX.lpDSB->Play (0, 0, DSBPLAY_LOOPING)))
			goto finishProcessSound;

		_lastblock = 0;
		StartPlaying = false;
	}
	DWORD play_pos = 0, write_pos = 0;
	DWORD write_size = _blocksize;
	HRESULT hResult = DS_OK;

	DirectX.lpDSB->GetCurrentPosition (&play_pos, &write_pos);

	DWORD curr_block = ((play_pos / _blocksize) + 1 * Settings.SoundBufferSize) % _buffernos;

//printf ("play_pos = %d, write_pos = %d, curr_block = %d, lastblock = %d\n", play_pos, write_pos, curr_block, _lastblock);
//fflush (stdout);
	if (curr_block != _lastblock)
	{
		BYTE  *B1, *B2;
		DWORD S1, S2;

		write_pos = curr_block * _blocksize;
		_lastblock = curr_block;

		hResult = DirectX.lpDSB->Lock (write_pos, write_size, (void **)&B1, &S1, (void **)&B2, &S2, 0);
		if (hResult == DSERR_BUFFERLOST)
		{
			DirectX.lpDSB->Restore ();
			hResult = DirectX.lpDSB->Lock (write_pos, write_size, (void **)&B1, &S1, (void **)&B2, &S2, 0);
		}
		if (!SUCCEEDED(hResult))
		{
			hResult = DirectX.lpDSB -> Unlock (B1, S1, B2, S2);
			goto finishProcessSound;
		}
		unsigned int sample_count = so.buffer_size;
		int byte_offset;

		if (so.sixteen_bit)
			sample_count >>= 1;

		if (so.samples_mixed_so_far < (int32) sample_count)
		{
			byte_offset = (so.sixteen_bit ? (so.samples_mixed_so_far << 1) : so.samples_mixed_so_far);

			if (FlexibleSoundMixMode())
			{
				// XXX: S9xMixSamples can change the state of the APU (or at least the SoundData object),
				//      but here we are in a function normally called by an asynchronous Windows timer! (SoundTimer())
				//      Also, we used GetCurrentPosition of an asynchronously-playing DirectSound Buffer,
				//      above, as part of deciding whether to call this code.
				//      These things cause recorded movies of games that interact with the APU
				//      (such as Terranigma and Out of This World) to often desync upon playback.
				//      The temporary fix is to have a FakeMute mode for movies of these games,
				//      but the real fix should be to only mix the sound in synchronization with the CPU or APU.

				sample_count -= so.samples_mixed_so_far;
				S9xMixSamplesEx (&SoundBuffer[byte_offset], sample_count);
			}
			so.samples_mixed_so_far = 0;
		}
		else so.samples_mixed_so_far -= sample_count;

		if (mute || hResult != DS_OK)
		{
			if (so.sixteen_bit)
			{
				if (B1)
					ZeroMemory (B1, S1);
				if (B2)
					ZeroMemory (B2, S2);
			}
			else
			{
				if (B1)
					memset (B1, 128, S1);
				if (B2)
					memset (B2, 128, S2);
			}
		}
		else
		{
			if (B1)
			{
				memmove (B1, SoundBuffer, S1);
#ifdef DEBUG_MK_APU
				if(fp==NULL)
				{
					fp=fopen("FF2DS6.raw", "ab");
				}
				fwrite(SoundBuffer,1, S1, fp);
#endif
			}
			if (B2)
			{
				memmove (B2, SoundBuffer + S1, S2);
#ifdef DEBUG_MK_APU
				if(fp==NULL)
				{
					fp=fopen("FF2DS6.raw", "ab");
				}
				fwrite(SoundBuffer+S1,1, S2, fp);
#endif
			}
		}
		hResult = DirectX.lpDSB -> Unlock (B1, S1, B2, S2);
		if (!SUCCEEDED(hResult))
			goto finishProcessSound;
	}
	DWORD Status;
	hResult = DirectX.lpDSB->GetStatus (&Status);
	StartPlaying = !Status;
#ifdef MK_APU
	if(SoundData.sample_cycles<0)
		SoundData.sample_cycles=0;
#endif
finishProcessSound:
	block_signal = FALSE;
}

bool8 S9xOpenSoundDevice (int mode, bool8 pStereo, int BufferSize)
{
	return (TRUE);
}

