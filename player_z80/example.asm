;
; Mod2PSG2 Music and Sound Effects Library Example
;
; This example shows how to play music. The code
; is written for WLA DX.
;
; Warning:
; This is not ready for being assembled. You have
; to fill in some gaps yourself.
;

;
; Definitions for memory mapping
;
.ROMBANKMAP
BANKSTOTAL 4
BANKSIZE $4000
BANKS 4
.ENDRO

.MEMORYMAP
SLOTSIZE $4000
DEFAULTSLOT 0
SLOT 0 $0000
SLOT 1 $4000
SLOT 2 $8000
.ENDME

;
; Set some constants and include the variables and the library code.
;
.DEFINE PSGMOD_START_ADDRESS     $C100
.DEFINE PSGMOD_SUPPORT_GG_STEREO 1
.DEFINE PSGMOD_PSG_PORT          $7F
.include "psgmod.inc"
.include "psgmod.asm"

;------------------------------------------------------------------------------
.BANK 0 SLOT 0
.ORG $0000
; Standard startup code.
  di
  im 1
  jp Start
  
; Call PSGMOD_Play() in the VBlank interrupt handler.
.ORG $0038
  ;
  ; ...
  ;
  call PSGMOD_Play
  ;
  ; ...
  ;
  reti
  
.ORG $0100
Start:
  ;
  ; ...
  ;
  
  ; Load a module and start the player.
  ; Music is only played by PSGMOD_Play().
  ld   hl, Module
  ld   a, :Module
  call PSGMOD_LoadModule
  call PSGMOD_Start
  
  ; Set the callback function for callback-commands.
  ld   hl, MyCallbackFunction
  call PSGMOD_SetCallbackFunction
  
  ; Enable VBlank interrupts.
  
  ; The music is now played via the VBlank interrupt.
Forever:
  jp Forever
  
MyCallbackFunction:
  ;
  ; ...
  ;
  ret
  
;------------------------------------------------------------------------------
;
; Include an .epsgmod file.
;
.BANK 2 SLOT 2
.ORG $0000
Module:
  .incbin "mymusic.epsgmod"

;------------------------------------------------------------------------------
;
; Include the vibrato tables.
;
.BANK 3 SLOT 2
.ORG $0000
PSGMOD_VIBRATO_TABLES:
  .incbin "psgmod.vib"
