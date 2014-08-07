
 Mod2PSG2 Music and Sound Effects Library
 
 This is part of the Mod2PSG2 archive, which
 you can get at http://mod2psg2.kontechs.com.
 
 This library is written for the WLA-Z80 assembler of WLA-DX.
 The website of WLA-DX is http://www.villehelin.com/wla.html.
 
 With this library you can play music and sound effects made
 with the Mod2PSG2 tracker. Music modules have to be exported
 into the EPSGMOD format.

===============================================================================
Step-by-step-guide to play music:
===============================================================================
( 1) In your own code, set the PSGMOD_START_ADDRESS constant to an
     address inside the RAM. This will set the start address of
     the variables of the library. It must be 256-bytes aligned.
     Example: .DEFINE PSGMOD_START_ADDRESS $C100

( 2) Define PSGMOD_SUPPORT_GG_STEREO as 0 or 1:
       1 for GG stereo support (will crash real Sega Master Systems)
       0 for no GG stereo support (GG stereo commands in the module will be ignored)
     Example: .DEFINE PSGMOD_SUPPORT_GG_STEREO 1

( 3) Define PSGMOD_PSG_PORT as the port to write data to the PSG.
     For SEGA Master System and Game Gear this is $7F.
     Example: .DEFINE PSGMOD_PSG_PORT $7F

( 4) Include the file psgmod.vib in a 16 KiB ROM bank, with a label called
     PSGMOD_VIBRATO_TABLES. The address of the label must be in the $8000-$BFFF
     address range. The file fills one 16 KiB bank completly.
     Example:
     .BANK 3 SLOT 2            ; Slot 2 is for the $8000-$BFFF address range
     .ORG $0000
     PSGMOD_VIBRATO_TABLES:
       .incbin "psgmod.vib"

( 5) Include the file psgmod.inc into your code. This file
     includes the variables definitions. You have to reserve
     384 bytes in the RAM for these variables.

( 6) Call the function PSGMOD_SetCallbackFunction() to set
     the function which is called for Callback-commands in the module.
     Example:
     ld   hl, MyCallbackFunction
     call PSGMOD_SetCallbackFunction

( 7) Include psgmod.asm in your code.
     This file includes the actual library code.

( 8) Load a module with PSGMOD_LoadModule().
     Example:
     ld   a, :MY_MODULE
     ld   hl, MY_MODULE
     call PSGMOD_LoadModule
     MY_MODULE has to be in the $8000-$BFFF address range
     (for which the memory mapping register at $FFFF).

( 9) Start the music by calling PSGMOD_Start().

(10) Write a loop which calls PSGMOD_Play() every video frame.

Notes:
(1) PSGMOD_LoadModule() and PSGMOD_Play() will set the memory mapping register
    for the $8000-$BFFF memory slot (0xFFFF) to the access the module and
    the vibrato tables (psgmod.vib). The other memory mapping registers
    are not touched. All other functions won't touch any of the memory mapping
    registers.
(2) None of this library's functions uses the IX/IY or the shadow registers.
    (But your callback-function could change these registers when PSGMOD_Play()
     is called.)

===============================================================================
Functions reference
===============================================================================
The following is a list of the functions included in the library with
descriptions. All function names begin with PSGMOD_.

-------------------------------------------------------------------------------
PSGMOD_LoadModule(A, HL)
-------------------------------------------------------------------------------
This will load a new module and prepare it to be played. The module must
be exported as .epsgmod file.

The module's address must be in the $8000-$BFFF address range. The memory
mapping register for this address range ($FFFF) will be changed to access
the module.

This function does not change any of the PSG registers.

Parameters:
	A  = memory bank number of the module
	HL = address of module in frame 2

-------------------------------------------------------------------------------
PSGMOD_SetCallbackFunction(HL)
-------------------------------------------------------------------------------
Sets the callback-function, which is called when a callback-command occurs in
the module. The only function in this library that calls the callback-function
is PSGMOD_Play().

The callback-function must be in the memory map when PSGMOD_Play() is called,
but not in the $8000-$BFFF address range, because the memory there is mapped
out by PSGMOD_Play().

Your callback-function is called with the callback-parameter in the A-register.
There are no Z80 registers that need to be preserved by your callback-function.

-------------------------------------------------------------------------------
PSGMOD_Play()
-------------------------------------------------------------------------------
This will play music and sound effects, but only if the player was started via
PSGMOD_Start(). If not, then it will mute the PSG.

-------------------------------------------------------------------------------
PSGMOD_Start()
-------------------------------------------------------------------------------
This sets the player into a started-state. It also resets the PSG. Call this
function before PSGMOD_Play().

-------------------------------------------------------------------------------
PSGMOD_Pause()
-------------------------------------------------------------------------------
This sets the player into a paused-state. It also resets the PSG. Subsequent
calls to PSGMOD_Play() will mute the PSG instead of playing music and sound
effects.
