.memorymap
slotsize $4000
slot 0 $0000
slot 1 $4000
slot 2 $8000
defaultslot 0
.endme

.rombankmap
bankstotal 21
banksize $4000
banks 21
.endro

.define TEXT_MODE

; Macros

; Patch a byte at the given offset
.macro patchByte args offset, value
.orga offset
.section "patchByte\@" overwrite
.db value
.ends
.endm

; Patch two bytes at the given offset
.macro patchWord args offset, value
.orga offset
.section "patchWord\@" overwrite
.dw value
.ends
.endm

; Nop out one or more bytes at the given offset
.macro nopOut args offset, numBytes
.orga offset
.section "nopOut\@" overwrite
.repeat numBytes
nop
.endr
.ends
.endm

; Set DE to the VRAM location of the gven x,y tilemap location
.macro LocationToDE args x, y
  ld de, $7800 | ((y * 32 + x) * 2)
.endm

; Memory map (for harness)
; Note that we use the memory right up to $dfff - we assume there's no writes going to Sega paging registers any more
.enum $e000 desc export
Slot2PageNumber          db ; Last page mapped to slot 2
FrameCounter             dw ; Enough for up to 21.8 minutes (at 50Hz)...
AKMWMoneyDividedBy10     db ; Score from game 1
HangOnScoreDividedBy10   dw ; Score from game 2
ColumnsScoreDividedBy10  dw ; Score from game 3
RAMJumpHelpers           dsb 9 ; RAM-located code for jumping between the game and the framework
GameNumber               db ; Current game
TopOfStack               db ; Rest of space is for games to use, they need to have their stacks patched
.ende

; Memory map (for when no game is running)
; Note that we should grab any game state before using any of this...
.enum $c000 asc export
Port3EValue               db
GameLoaderRAMAddress      dsb 255
PSGMOD_START_ADDRESS      dsb 256 ; Must be 256-byte aligned...
PSGDecoderBuffer          dsb 34
NumberTileIndicesOffset   dw ; So I can choose colours
.ende

; Defines
.define Port_VDPAddress      $bf ; w
.define Port_VDPRegisters    $bf ; w
.define Port_VDPData         $be ; w
.define Port_VDPStatus       $bf ; r
.define Port_Player1Controls $dc ; r

.define Player1_ButtonU %00000001
.define Player1_ButtonD %00000010
.define Player1_ButtonL %00000100
.define Player1_ButtonR %00001000
.define Player1_Button1 %00010000
.define Player1_Button2 %00100000

.define Slot0Control $0000
.define Slot1Control $4000
.define Slot2Control $8000

.define JumpOut RAMJumpHelpers + 0
.define JumpOutAddress RAMJumpHelpers + 2
.define JumpBack RAMJumpHelpers + 5

.define InitialTime 50 * 60 * 4 ; 4 minutes

.bank 0 slot 0
.org 0
.section "Boot" force
di
im 1
jp start
.ends

; Just in case there are any rogue INTs when my code is paged in...
.orga $0038
.section "INT handler" force
  push af
  push bc
  push hl
    in a,(Port_VDPStatus)
    call CheckForReset
    call PSGMOD_Play
  pop hl
  pop bc
  pop af
  ei
  reti
.ends

; ...and you must always expect NMIs...
.orga $0066
.section "NMI handler" force
  retn
.ends

.section "Start" free
start:
  ld sp,TopOfStack
  ; Initialise paging, also makes Meka detect a Codemasters mapper
  xor a
  ld (Slot0Control),a
  inc a
  ld (Slot1Control),a
  inc a
  ld (Slot2Control),a
  
  ; Blank our RAM
  ld hl,TopOfStack + 1
  ld de,TopOfStack + 2
  ld bc,$e000 - TopOfStack - 2
  ld (hl),0
  ldir

  ld hl,InitialTime
  ld (FrameCounter),hl

  ; Copy VBlank helper routines to RAM
  ld hl, RAMJumpHelpersCode
  ld de, RAMJumpHelpers
  ld bc, RAMJumpHelpersCodeEnd-RAMJumpHelpersCode
  ldir

  ; Show "title screen"
  call InitialiseSystem
  call TitleScreen

  jp AKMWStart
  
.ends

.section "Initialise screen for framework" free
InitialiseSystem:
  di

  ; Turn off sound by setting all volumes to 0
  ld a,%10011111
  out ($7f),a
  ld a,%10111111
  out ($7f),a
  ld a,%11011111
  out ($7f),a
  ld a,%11111111
  out ($7f),a

  ; Set up VDP registers
  ld hl,VdpData
  ld b,VdpDataEnd-VdpData
  ld c,Port_VDPAddress
  otir

  ; Clear VRAM
  ld a,$00
  out (Port_VDPAddress),a
  ld a,$40
  out (Port_VDPAddress),a
  ld bc,$4000
-:
  xor a
  out (Port_VDPData),a
  dec bc
  ld a,b
  or c
  jp nz,-
  
  ; Disable sprites
  ld a,$00
  out (Port_VDPAddress),a
  ld a,$7f
  out (Port_VDPAddress),a
  ld a,208
  out (Port_VDPData),a

  ; Load palette
  ld a,$00
  out (Port_VDPAddress),a
  ld a,$c0
  out (Port_VDPAddress),a
  ; 2. Output colour data
  ld hl,PaletteData
  ld b,(PaletteDataEnd-PaletteData)
  ld c,Port_VDPData
  otir

.ifdef TEXT_MODE
  ; Load font
  ld a,$00
  out (Port_VDPAddress),a
  ld a,$40
  out (Port_VDPAddress),a
  ld hl,FontData
  ld bc,FontDataEnd-FontData
-:ld a,(hl)        ; Get data byte
  out (Port_VDPData),a
  xor a
  out (Port_VDPData),a
  out (Port_VDPData),a
  out (Port_VDPData),a
  inc hl
  dec bc
  ld a,b
  or c
  jr nz,-
.else
  ; Load graphics
  ld ix,Tiles
  ld hl,$4000
  ld a,:Tiles
  ld ($8000),a
  call PSG_decompress
.endif
  
  ; Start music
  ld a,:MusicData
  ld hl,MusicData
  call PSGMOD_LoadModule
  call PSGMOD_Start
  ei
  ret

PaletteData:
.ifdef TEXT_MODE
.db $00,$3f,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ; Black, White
.else
.db $00 $10 $34 $25 $35 $39 $2A $3A $2B $0F $1F $3F 0 0 0 0
.dsb 16 $35 ; background
.endif
PaletteDataEnd:

; VDP initialisation data
VdpData:
.db $04,$80,$04,$81,$ff,$82,$ff,$85,$ff,$86,$ff,$87,$00,$88,$00,$89,$ff,$8a
VdpDataEnd:

.ends
.ifdef TEXT_MODE
.section "Font data" free
FontData:
.include "BBC Micro font.inc"
FontDataEnd:
.ends
.else
.slot 2
.section "Graphics data" superfree
Tiles:
.incbin "Tiles.psgcompr"
Tilemaps:
.incbin "Tilemaps.bin" skip 32*2*4 ; top 4 rows are digits
.ends
.slot 0

.include "Phantasy Star Gaiden decompressor.inc"
.endif

.section "Maths helpers" free
BCDToBin:
  ; Input: a = BCD value
  ; Output: a = value
  ; Trashes: de
  ld e,a
  and $f0
  srl a
  ld d,a
  srl a
  srl a
  add a,d
  ld d,a
  ld a,e
  and $0f
  add a,d
  ret
  
AddAToHL:
  ; Input: hl, a
  ; Output: hl = hl + a
  ; Trashes: nothing
  
  ; l += a
  add a,l
  ld l,a
  
  ; h += carry
  adc a,h
  sub l
  ld h,a
  
  ; done
  ret
    
DETimesAToBCHL:
  ; Input: de, a
  ; Output: bchl = de * a
  ; Trashes: b
  
  ; Clear hl
  ld hl,0
  ; Loop over 8 bits in a, from left to right
  ld bc,8<<8 + 0
  ; chl <<= 1
-:add hl,hl
  rl c
  ; a <<= 1 into carry
+:add a,a
  jr nc,+
  ; If carry out, hl += de
  add hl,de
  jr nc,+
  inc c
+:djnz -

  ; e.g. if a = %10000011
  ;           =  1 << 7 +  1 << 1 +  1 << 0
  ; then bchl = de << 7 + de << 1 + de << 0
  ret

DivideHLByB:
  ; Input: hl, b
  ; Output: hl = hl / b, a = hl % b
  xor a
.rept 16
  add hl,hl
  rla
  cp b
  jr c,+
  sub b
  inc l
+:
.endr
  ret
    
; bchl += de
; Uses de
AddDEToBCHLLow:
  ; Do the maths
  add hl,de
  ; Carry into bc
  ex de,hl
  ld hl,0
  adc hl,bc
  ld c,l
  ld b,h
  ex de,hl
  ret

; bchl += de << 16
; Uses a
AddDEToBCHLHigh:
  ; Do the maths
  ex de,hl  ; save hl, get de
  add hl,bc ; add bc
  ld c,l    ; put in bc
  ld b,h
  ex de,hl  ; restore hl
  ret
.ends

.section "Text writer" free
VRAMToDE:
  ld a,e
  out (Port_VDPAddress),a
  ld a,d
  out (Port_VDPAddress),a
  ret

.ifdef TEXT_MODE
WriteText:
  ; Args:
  ; hl = text to write
  ; de = VRAM address to write to (with high bit set)
  call VRAMToDE
-:ld a,(hl)
  or a
  ret z
  sub ' '
  out (Port_VDPData),a
  xor a
  out (Port_VDPData),a
  inc hl
  jr -
.endif

.macro SHOW_ZEROES
.ifdef TEXT_MODE
  ld d,0
.endif
.endm

.macro HIDE_ZEROES
.ifdef TEXT_MODE
  ld d,-$10 ; zero correction to space
.endif
.endm

WriteNumber2Digits:
  ; Args:
  ; a = number to display (no bigger than 99)
  ; Trashes: hl, bc, hl', bc', af, d
  exx
  ld h, 0
  ld l, a
  exx
  ld hl, 0
  SHOW_ZEROES
  jp _WriteNumber2DigitsEntry

WriteNumber16:
  ; Args:
  ; hl = number to display
  ; de = VRAM address of the most significant digit
  ; Trashes: bc, hl', bc', a
.ifdef TEXT_MODE
  call VRAMToDE ; Set VRAM address now
.else
  xor a
  ld (NumberTileIndicesOffset),a
.endif

  push hl
  exx
  pop hl
  exx
  ld hl, 0
  HIDE_ZEROES
  jp _WriteNumber16Entry

WriteNumberSevenDigits:
  ; Args:
  ; bchl = 32-bit number, should be no larger than 9999999 else you get your number modulo $ffffff
  ; de = VRAM address of the most significant digit
  ; Trashes: af, bc, de, hl, bc', hl'
.ifdef TEXT_MODE
  call VRAMToDE ; Set VRAM address now
.else
  xor a
  ld (NumberTileIndicesOffset),a
.endif

  ; Shuffle so hl = high word, hl' = low word
  push hl
  push bc
  pop hl
  exx
  pop hl
  exx
  
  HIDE_ZEROES
  jp _WriteNumber7DigitsEntry

WriteNumber32:
  ; Args:
  ; bchl = 32-bit number
  ; de = VRAM address of the most significant digit
  ; Trashes: af, bc, de, hl, bc', hl'
.ifdef TEXT_MODE
  call VRAMToDE ; Set VRAM address now
.else
  ld a,OFFSET_PINK_DIGITS
  ld (NumberTileIndicesOffset),a
.endif

  ; Shuffle so hl = high word, hl' = low word
  push hl
  push bc
  pop hl
  exx
  pop hl
  exx
  
  HIDE_ZEROES

.macro WriteNumber32_Digit args digit
  ld bc,(-digit >> 16) & $ffff
  exx
  ld bc,-digit & $ffff
  exx
  call WriteDigit
.endm

  WriteNumber32_Digit 1000000000
  WriteNumber32_Digit 100000000
  WriteNumber32_Digit 10000000
_WriteNumber7DigitsEntry:
  WriteNumber32_Digit 1000000
  WriteNumber32_Digit 100000
_WriteNumber16Entry:
  WriteNumber32_Digit 10000
  WriteNumber32_Digit 1000
  WriteNumber32_Digit 100
_WriteNumber2DigitsEntry:
  WriteNumber32_Digit 10
  ; Always write the last zero
  SHOW_ZEROES
  WriteNumber32_Digit 1
  ret
  
WriteDigit:
  xor a
  ; Add bcbc' to hlhl' until it the 32-bit result goes below 0, incrementing a as we go
-:inc a
  exx
  add hl,bc   ; low word
  exx
  adc hl,bc   ; high word
  jr c,-    ; carry = overflow = more to write
  ; Then subtract it to get back to a positive number
  exx
  sbc hl,bc   ; low word
  exx
  sbc hl,bc   ; high word
  dec a

.ifdef TEXT_MODE
  jr nz,_writeNonZero
  ; Write spaces for leading zeroes
  add a,d
  jr _write

_writeNonZero:
  ld d,0    ; Disable zero to space correction
  ; fall through
_write:
  add a,$10    ; Convert to tile index - $10 = index of '0'
  out (Port_VDPData),a
  xor a
  out (Port_VDPData),a
  ret
.else
  ; Graphical mode number display
  ; Always shows leading zeroes
  ; Look up in table
  push hl
    ld hl,_NumberTileIndices
    add a,a
    add a,l
    ld l,a
    adc a,h
    sub l
    ld h,a
    ld a,(hl) ; Top half of number
    ; Switch to pink background if wanted
    push hl
      ld hl,NumberTileIndicesOffset
      add a,(hl)
    pop hl
    push af
      call VRAMToDE
    pop af
    out (Port_VDPData),a
    xor a
    out (Port_VDPData),a
    ; Move down a row
    push de
      inc hl
      ld a,(hl)
      ; Switch to pink background if wanted
      ld hl,NumberTileIndicesOffset
      add a,(hl)
      ld hl,64
      add hl,de
      ex de,hl
      push af
        call VRAMToDE
      pop af
      out (Port_VDPData),a
      xor a
      out (Port_VDPData),a
    pop de
    ; Move on a tile
    inc de
    inc de
  pop hl
  
  ret
_NumberTileIndices:
; Blue background
.db 1,9,2,10,3,11,4,12,5,13,6,14,7,15,8,16,1,17,1,18
.define OFFSET_PINK_DIGITS 18
.endif

ShowTime:
  ; Args:
  ; hl = time in frames
  ; de = VRAM address of the most significant digit
  ; Trashes: bc, hl', bc', a
.ifdef TEXT_MODE
  call VRAMToDE
.endif
  ld hl,(FrameCounter)
  ; Frames to seconds
  ld b,50
  call DivideHLByB
  ; Minutes
  ld b,60
  call DivideHLByB
  ; Modulo = seconds in a
  push af
    ld a,l
    call WriteNumber2Digits
.ifdef TEXT_MODE
    ; Colon
    ld a,$1a ; ':'
    out (Port_VDPData),a
    xor a
    out (Port_VDPData),a
.else
    ; TODO
.endif
  pop af
  ; Seconds
  call WriteNumber2Digits
  ret

.ends

.section "Screen control" free
ScreenOn:
  ; Turn screen on
  ld a,%11100000
;        |||| |`- Zoomed sprites -> 16x16 pixels
;        |||| `-- Doubled sprites -> 2 tiles per sprite, 8x16
;        |||`---- 30 row/240 line mode
;        ||`----- 28 row/224 line mode
;        |`------ VBlank interrupts
;        `------- Enable display
-:out (Port_VDPRegisters),a
  ld a,$81
  out (Port_VDPRegisters),a
  ret

ScreenOff:
  ; Turn screen off
  ld a,%10000100
;        |||| |`- Zoomed sprites -> 16x16 pixels
;        |||| `-- Doubled sprites -> 2 tiles per sprite, 8x16
;        |||`---- 30 row/240 line mode
;        ||`----- 28 row/224 line mode
;        |`------ VBlank interrupts
;        `------- Enable display
  jr -
.ends

.section "Wait for button" free
WaitForButton:
-:halt
  in a,(Port_Player1Controls)
  and Player1_Button1
  jr nz,-
-:halt
  in a,(Port_Player1Controls)
  and Player1_Button1
  jr z,-
  ret
.ends

.ifndef TEXT_MODE
.section "Load a screen" free
.macro LoadScreen args index
  ld hl, Tilemaps + 32*24*2*index
  call LoadScreenImpl
.endm
  
LoadScreenImpl:
  ; Hacky :P
  ld de,$7800
  call VRAMToDE
  ld a,:Tilemaps
  ld ($8000),a

  ; fast
  ld bc,0 << 8 | Port_VDPData
  otir
  otir
  otir
  otir
  otir
  otir
/*
  ; slow
  ld bc, 32*24*2
-:ld a,(hl)
  inc hl
  out (Port_VDPData),a
  ; Slow down
  push bc
    ld b,0
 --:push ix
    pop ix
    djnz --
  pop bc
  dec bc
  ld a,b
  or c
  jr nz,-
*/
  ret
.ends
.endif

.section "Title screen" free
TitleScreen:
.ifdef TEXT_MODE
  ld hl,_text
  LocationToDE 0, 2
  call WriteText
.else
  LoadScreen 0

/*  
  ; Testing
  ld bc,$4996
  ld hl,$02d2
  LocationToDE 2, 2
  call WriteNumber32

  ld hl,12345
  LocationToDE 2, 5
  call WriteNumber16
  
  ld hl,(12*60+34)*50
  ld (FrameCounter),hl
  LocationToDE 2, 8
  call ShowTime
*/

  call ScreenOn
  call WaitForButton
  call ScreenOff

  LoadScreen 1
.endif

  call ScreenOn
  call WaitForButton
  call ScreenOff
  ret

.ifdef TEXT_MODE
_text:
; Hugely wasteful but easy to write...
;    01234567890123456789012345678901
.db "     Sega8bit.com/SMS Power!    "
.db "           meetup 2014          "
.db "                                "
.db "           at pub/zoo           "
.db "       on August 2nd 2013       "
.db "                                "
.db "        Competition cart!       "
.db "                                "
.db "                                "
.db " Game 1:                        "
.db "   Alex Kidd in Miracle World   "
.db "                                "
.db " Objective: Collect as much     "
.db "   money as you can in level 1, "
.db "   but be quick!                "
.db "                                "
.db " Controls: 1 = jump             "
.db "           2 = punch            "
.db "                                "
.db "        Press 1 to start", 0
.endif
.ends

.section "Game swapping helpers" free
LoadGame:
  ; Page number in a, we map three consecutive pages
  ; Copy game start routine to RAM
  ld hl, LoadGameCode
  ld de, GameLoaderRAMAddress
  ld bc, LoadGameCodeEnd - LoadGameCode
  ldir
  ; Execute it
  jp GameLoaderRAMAddress

; This routine is copied to RAM and executed in order to swap the ROM area for a chosen game.
; Register a holds the number of the first of up to three consecutive ROM pages that are selected.
; Note that this code is not size-sensitive.
LoadGameCode:
  ; Page in the game
  ld (Slot0Control),a
  inc a
  ld (Slot1Control),a
  inc a
  ld (Slot2Control),a

  ; Jump to it
  jp $0000
LoadGameCodeEnd:

; This routine is copied to RAM and patched in order to either call into the framework or to return to the game.
; We patch it to do what is wanted ($1234 is a placeholder).
; This wants to be as small as possible (as it steals RAM from the game). But it's hard to make it much smaller...
; - To jump out of the game to the framework, we patch $1234 below with the desired framework code address and 
;   jp to _JumpOut (at its RAM-located address, JumpOut .defined above). The per-frame handler is patched in
;   before the game starts and the game sets it to the game end handler at the appropriate time.
;   We must make sure it is safe to overwrite hl, a and f at the point we patch into.
; - To jump back from the framework to the game, we use no extra RAM-located code; we just waste some framework
;   bytes to push the return address and set a to the page number.
;   We must make sure it is safe to overwrite a and f at the point we patch into.
; How could it be made smaller?
; - Do more in the patched game code (i.e. the first 3 opcodes) - tricky, need to find space for that (which 
;   might just mean overwriting some code and then performing the operations after the jump)
; - Use a ld (rr),a opcode to do the paging - but then we would need to zero the register pair
RAMJumpHelpersCode:
_JumpOut:
  xor a         ; 1 bytes
  ld hl,$1234   ; 3 bytes
  push hl       ; 1 byte
_JumpBack:
  ld (Slot0Control),a  ; 3 bytes
  ret           ; 1 byte
RAMJumpHelpersCodeEnd:

CheckForReset:
  in a,($dd)
  and %00010000 ; Reset button
  ret nz
  ; Reboot
  jp 0
.ends
/*
.section "Sonic helpers" free
SonicStart:
  ; Record the game number
  xor a
  ld (GameNumber),a
  ; Patch in where to go...
;  ld hl,AKMWFrameHandler
;  ld (JumpOutAddress), hl

  ; Jump to the game
  ld a,:Sonic
  jp LoadGame
.ends
*/
.section "AKMW helpers" free

; $c01f is the mode variable...
; The high bit means it's been processed by the game...
; $8a means in-game
; $84 is the end of the level
.define AKMWModeControl $c01f
.define AKMWModeControl_InGame $8a
.define AKMWModeControl_EndOfLevel $84
; Money is stored in BCD at $c030..$c031
; Score abcd0 is stored as cd ab
.define AKMWScoreHigh $c031
.define AKMWScoreLow $c030

AKMWStart:
  ; Record the game number
  xor a
  ld (GameNumber),a
  ; Patch in where to go...
  ld hl,AKMWFrameHandler
  ld (JumpOutAddress), hl

  ; Jump to the game
  ld a,:AlexKiddInMiracleWorld
  jp LoadGame
  

AKMWFrameHandler:
  ; All registers are free to use, we're in the VBlank handler
  
  call CheckForReset
  
  ld a,(AKMWModeControl)
  cp AKMWModeControl_InGame
  jr nz,+

  ; In game
  ; Decrement timer
  ld hl,(FrameCounter)
  dec hl
  ld (FrameCounter),hl
  ld a,h
  or l
  jp z,TimeUp
  jr ++

+:cp AKMWModeControl_EndOfLevel
  jp z,AKMWEndOfGame
  
++:
  ; Return to game
  ld a,:AlexKiddInMiracleWorld
  ld hl,AKMWReturnAddress
  push hl
  jp JumpBack

AKMWEndOfGame:
  ; End of level
  ; Reset the system
  call InitialiseSystem
  ld sp,TopOfStack

.ifdef TEXT_MODE
 ; Display stuff
  ld hl,_text
  LocationToDE 0, 2
  call WriteText
.else
  LoadScreen 2
.endif

  ; Get the money score
  call AKMWGetScore

.ifdef TEXT_MODE
  LocationToDE 22, 4
  call WriteNumberSevenDigits
  
  LocationToDE 24, 6
  call ShowTime
.endif

  call ScreenOn
  call WaitForButton
  call ScreenOff

  ; Jump to the next game
  jp HangOnStart

.ifdef TEXT_MODE
_text:
.db "   Alex Kidd in Miracle World   "
.db "                                "
.db "   Money:               _____   "
.db "                                "
.db "   Time left:           __:__   "
.db "                                "
.db "                                "
.db "                                "
.db " Game 2:                        "
.db "   Hang On                      "
.db "                                "
.db " Objective: Get a high score    "
.db "   on stage 1... but time is    "
.db "   running out...               "
.db "                                "
.db " Controls: 2 = accelerator      "
.db "           U/D = gear           "
.db "           L/R = steer          "
.db "                                "
.db "   Press 1 to continue", 0
.endif

AKMWGetScore:
  ; Store to RAM (divided by 10) and return (unmolested) in bchl
  ld a,(AKMWScoreHigh) ; High byte x 10
  call BCDToBin
  ld de,10
  call DETimesAToBCHL
  ld a,(AKMWScoreLow) ; + low byte x 1
  call BCDToBin
  call AddAToHL
  ; There are 51 bags possible, so a maximum of 1020, so we can store it in a single byte.
  ld a,l
  ld (AKMWMoneyDividedBy10),a
  ; Then we scale it up again
  ld de, 10
  call DETimesAToBCHL
  ret

.ends

.section "Hang On helpers" free

.define HangOnReturnAddress $2433
; Score is stored as BCD from $c004, 3 bytes
; abcdef0 is stored as ab cd ef
; Stage 1 score doesn't exceed 30000 (00 30 00), so the last 2 bytes are plenty
.define HangOnScoreHigh $c005
.define HangOnScoreLow $c006

HangOnStart:
  ; Record the game number
  ld a,1
  ld (GameNumber),a

  ; Patch in where to go...
  ld hl,HangOnFrameHandler
  ld (JumpOutAddress), hl

  ; Jump to the game
  ld a,:HangOn
  jp LoadGame

HangOnFrameHandler:
  ; This only gets called once the race has started.
  ; We pushed hl on entry and need to pop it
  ; de and a are free
    call CheckForReset
    ld hl,(FrameCounter)
    dec hl
    ld (FrameCounter),hl
    ld a,h
    or l
  pop hl
  jp z,TimeUp

  ld a,:HangOn
  ld de,HangOnReturnAddress ; Hang On return to game point: post time decrement if time is positive (which we make it always be)
  push de
  jp JumpBack

HangOnEnd:
  ; Clear things up
  call InitialiseSystem
  ld sp,TopOfStack

.ifdef TEXT_MODE
 ; Display stuff
  ld hl,_text
  LocationToDE 0, 2
  call WriteText
.else
  LoadScreen 3
.endif

  ; Get the score
  call HangOnGetScore

.ifdef TEXT_MODE
  ld e,l
  ld d,h
  ld a,10
  call DETimesAToBCHL
  LocationToDE 22, 4
  call WriteNumberSevenDigits

  LocationToDE 24, 6
  call ShowTime
.endif

  call ScreenOn
  call WaitForButton
  call ScreenOff

  ; Jump to the next game
  jp ColumnsStart
  
.ifdef TEXT_MODE
_text:
.db "             Hang On            "
.db "                                "
.db "   Score:               _____   "
.db "                                "
.db "   Time left:           __:__   "
.db "                                "
.db "                                "
.db "                                "
.db " Game 3:                        "
.db "   Columns                      "
.db "                                "
.db " Objective: Score as many       "
.db "   points as you can in the     "
.db "   remaining time               "
.db "                                "
.db " Controls: L/R = move           "
.db "           D   = drop           "
.db "           1/2 = swap gems      "
.db "                                "
.db "   Press 1 to continue", 0
.endif

HangOnGetScore:
  ld a,(HangOnScoreHigh) ; High byte x 100
  call BCDToBin
  ld de,100
  call DETimesAToBCHL
  ld a,(HangOnScoreLow) ; + low byte
  call BCDToBin
  call AddAToHL
  ld (HangOnScoreDividedBy10),hl
  ret
.ends

.section "Columns helpers" free

.define ColumnsReturnAddress $020a
; Mode control variable
.define ColumnsModeControl $c015
.define ColumnsModeControl_InGame 2
; "Pause" control variable, used for pausing and also start/game over display
.define ColumnsIsPaused $c102
; Score is held at $c694 as a *little-endian number* (!), 3 bytes long.
.define ColumnsScoreLowWord $c694
.define ColumnsScoreHighByte $c696

ColumnsStart:
  ; Record the game number
  ld a,2
  ld (GameNumber),a

  ; Patch in where to go...
  ld hl,ColumnsFrameHandler
  ld (JumpOutAddress), hl

  ; Jump to the game
  ld a,:Columns
  jp LoadGame
  
ColumnsFrameHandler:
  ; af, bc, de, hl are safe to use
  ; What I replaced to get here
  push ix
  push iy
  
  call CheckForReset

  ; Are we in the game?
  ld a,(ColumnsModeControl)
  cp ColumnsModeControl_InGame
  jr nz,+ ; No: do nothing
  ld a,(ColumnsIsPaused)
  or a
  jr nz,+ ; This is 1 if the game is not actually active... (paused, "Ready", "Game Over")

  ; Decrement timer
  ld hl,(FrameCounter)
  dec hl
  ld (FrameCounter),hl
  ld a,h
  or l
  jp z,TimeUp

  ; That's it!
+:ld a,:Columns
  ld hl,ColumnsReturnAddress
  push hl
    jp JumpBack
  
ColumnsEnd_GameOver:
  ; Clear things up
  call InitialiseSystem
  ld sp,TopOfStack

.ifdef TEXT_MODE
  ; Display stuff
  ld hl,_textGameOver
  LocationToDE 0, 2
  call WriteText

_textGameOver:
.db "             Columns            "
.db "                                "
.db "   Wait a minute...             "
.db "                                "
.db "   You died?!?                  "
.db "                                "
.db "   But you had __:__ left!      "
.db "                                "
.db "   Think of all the points you  "
.db "   could have got!              "
.db "                                "
.db "                                "
.db "                                "
.db "                                "
.db "                                "
.db "                                "
.db "                                "
.db "                                "
.db "                                "
.db "   Press 1 to continue", 0
.else
  jp FinalResults
.endif

ColumnsGetScore:
  ; We truncate to 16 bits, so a maximum of 655350
  ld hl,(ColumnsScoreLowWord)
  ld a,(ColumnsScoreHighByte)
  or a
  jr z,+
  ld hl,$ffff
+:ld (ColumnsScoreDividedBy10),hl
  ret
.ends

.section "Time up handler" free
TimeUp:
  ; Clear things up
  call InitialiseSystem
  ld sp,TopOfStack
  
.ifndef TEXT_MODE
  LoadScreen 4 ; Time up

  call ScreenOn
  call WaitForButton
  call ScreenOff
.endif

FinalResults:
  
  ; Process the score for the game in progress
  ld a,(GameNumber)
  or a
  call z,AKMWGetScore
  dec a
  call z,HangOnGetScore
  dec a
  call z,ColumnsGetScore

.ifdef TEXT_MODE
 ; Display stuff
  ld hl,_text
  LocationToDE 0, 2
  call WriteText
.else
  LoadScreen 5 ; Results
.endif

  ; Draw stuff. We retrieve the scores from RAM and scale them up as necessary.
  ld a,(AKMWMoneyDividedBy10)
  ld de,10
  call DETimesAToBCHL
.ifdef TEXT_MODE
  LocationToDE 15, 7
.else
  LocationToDE 5, 4
.endif
  call WriteNumberSevenDigits

  ld de,(HangOnScoreDividedBy10)
  ld a,10
  call DETimesAToBCHL
.ifdef TEXT_MODE
  LocationToDE 15, 10
.else
  LocationToDE 5, 9
.endif
  call WriteNumberSevenDigits

  ld de,(ColumnsScoreDividedBy10)
  ld a,10
  call DETimesAToBCHL
.ifdef TEXT_MODE
  LocationToDE 15, 13
.else
  LocationToDE 5, 14
.endif
  call WriteNumberSevenDigits

  ; Calculate the total score
  ; 1. AKMW x 100. Can't overflow...
  ld a,(AKMWMoneyDividedBy10)
  ld de, 100*10
  call DETimesAToBCHL
  push bc
    push hl
.ifndef TEXT_MODE
      LocationToDE 20, 4
      call WriteNumberSevenDigits
.endif
      ; 2. Hang On
      ld de,(HangOnScoreDividedBy10) ; up to about 8000
      ld a,10
      call DETimesAToBCHL ; may reach into c?
      push bc
        push hl
.ifndef TEXT_MODE
          LocationToDE 20, 9
          call WriteNumberSevenDigits
.endif
          ; 3. Columns x30
          ld de,(ColumnsScoreDividedBy10) ; may be quite high?
          ld a,150 ; can't do 300 in a byte!
          call DETimesAToBCHL
          ; Then x2...
          add hl,hl
          ex de,hl
            ld h,b
            ld l,c
            adc hl,bc
            ld b,h
            ld c,l
          ex de,hl
.ifndef TEXT_MODE
          push bc
          push hl
            LocationToDE 20, 14
            call WriteNumberSevenDigits
          pop hl
          pop bc
.endif
          ; Unwind and add up...
        pop de
        call AddDEToBCHLLow
      pop de
      call AddDEToBCHLHigh
    pop de
    call AddDEToBCHLLow
  pop de
  call AddDEToBCHLHigh
    
  ; Done! Now to display it...
.ifdef TEXT_MODE
  LocationToDE 12, 17
.else
  LocationToDE 17, 19
.endif
  call WriteNumber32

  call ScreenOn
  call WaitForButton
  call ScreenOff
  jp 0

_text:
.db "            GAME OVER           "
.db "                                "
.db " Your score:                    "
.db "                                "
.db " Alex Kidd in Miracle World:    "
.db "         Money:  _____ x  128   "
.db "                                "
.db " Hang On:                       "
.db "         Score:  _____ x    1   "
.db "                                "
.db " Columns:                       "
.db "         Score:  _____ x   32   "
.db "                                "
.db " Total score:                   "
.db "                                "
.db "            __________          "
.db "                                "
.db "                                "
.db "                                "
.db "       Press 1 to restart", 0

.ends

.section "Mod2PSG2" free
.define PSGMOD_SUPPORT_GG_STEREO 0
.define PSGMOD_PSG_PORT $7f
.define PSGMOD_MAPPER_ADDRESS $8000
.include "player_z80/psgmod.inc"
.include "player_z80/psgmod.asm"
.ends

.bank 1 slot 1
.orga $7fe0
.section "Everdrive Codemasters fakery" force
; This is a copy of the header from Excellent Dizzy Collection. Most of it is needed to
; make the Everdrive switch into Codemasters mapper mode.
.db 32 ; bank count - best to say 32
.db $07, $06, $94, $15, $44 ; date/time - not used
.dw $ef36, $10000 - $ef36 ; fake checksum - must be non-zero in LSB
.dsb 6 $00 ; unused bytes - must be zero
.ends

.smstag

; The games

; We have to split them into chunks manually to get the bank numbers to assemble correctly.
; Macros don't work as the offsets exceed 16 bits and WLA DX doesn't like that.
.bank 2
.orga 0
HangOn:
.incbin "Hang On.sms" skip $0000 read $4000

.orga $0066
.section "Hang On pause killer" overwrite
  retn
.ends

 nopOut $1d5 7 ; Remove reset button check

.orga $2412
.section "VBlank in-race hack" overwrite
push hl ; Preserve this
jp JumpOut
; We replaced the bit of code that decrements the time...
.ends

.orga $3ce8
.section "Hang On stage end patch" overwrite
  ; Hit only when you get to the goal.
  ; We pause a bit...
  ld hl,0
-:dec hl
  push hl
  pop hl
  ld a,h
  or l
  djnz -
  ; We patch the VBlank helper and jump to it to escape the game.
  ld hl,HangOnEnd
  ld (JumpOutAddress), hl
  jp JumpOut
.ends

 patchWord $0003+1, TopOfStack ; move the stack down
 patchWord $00ad+1, TopOfStack - $c000 - 1 ; shorten range of memory cleared on startup
 patchByte $04a6+1, $30 ; Disable up/down on the title screen
 nopOut $04ad, 1 ; Disable attract mode timer

.bank 3
.org 0
.incbin "Hang On.sms" skip $4000 read $4000

.bank 4
.org 0
AlexKiddInMiracleWorld:
.incbin "akbios.sms" skip $0000 read $4000

; Disable the BIOS hook at the start
.orga $0000
.section "AKMW Boot" overwrite
di
im 1
ld sp,TopOfStack ; restore the original code here
jr $0085-CADDR-1
.ends

; Disable pause
.orga $0066
.section "AKMW pause killer" overwrite
  retn
.ends

.orga $00bc; was $00c4
.section "AKMW VBlank patch" overwrite
; Was reset button check in VBlank:
; in     a,($dd)         ; 0000BC DB DD 
; and    $10             ; 0000BE E6 10 
; ld     hl,$c096        ; 0000C0 21 96 C0 
; ld     c,(hl)          ; 0000C3 4E 
; ld     (hl),a          ; 0000C4 77 
; xor    c               ; 0000C5 A9 
; and    c               ; 0000C6 A1 
; jp     nz,$0094        ; 0000C7 C2 94 00 
  jp JumpOut
.define AKMWReturnAddress $00ca
.ends

; Move the stack down...
; Non-BIOS patchWord $0003+1, TopOfStack ; move the stack down
 patchWord $0099+1, TopOfStack ; it's set twice was $00a4
 patchWord $008b+1 /* was $0093+1*/, TopOfStack - $c000 - 1 ; shorten range of memory cleared on startup

; Disable title screen timeout
 nopOut $07cc, 1 ; was $07ff

.macro AKMWPagingA
;    ld     a,$82           ; 000085 3E 82
;    ld     ($ffff),a       ; 000087 32 FF FF
.orga \1
.section "AKMWPagingA\@" overwrite
  ld a,\2+:AlexKiddInMiracleWorld
  call AKMWPagingHelper
.ends
.endm

.macro AKMWPagingB
;    (something not just a constant and possibly with high bits set)
;    ld     ($ffff),a       ; 000087 32 FF FF
.orga \1
.section "AKMWPagingB\@" overwrite
  call AKMWPagingHelperWithMaskAndAdjustment
.ends
.endm

.macro AKMWPagingC
;    (something to get the *adjusted* page in a)
;    ld     ($ffff),a       ; 000087 32 FF FF
.orga \1
.section "AKMWPagingC\@" overwrite
  call AKMWPagingHelper
.ends
.endm

; Choose starting level
 patchByte $07f5 5 ; Lake Fathom Part 2 ; was $0828

; These were auto-generated...
 AKMWPagingA $0094, 2
 AKMWPagingA $00f3, 2
 AKMWPagingA $0489, 3
 AKMWPagingA $0754, 2
 AKMWPagingA $0778, 4
 AKMWPagingA $08f1, 2
 AKMWPagingA $09df, 5
 AKMWPagingA $0a02, 5
 AKMWPagingA $0a90, 2
 AKMWPagingA $0b07, 5
 AKMWPagingA $0b15, 3
 AKMWPagingA $0b45, 2
 AKMWPagingA $0c73, 7
 AKMWPagingA $0c8d, 5
 AKMWPagingA $0cb3, 2
 AKMWPagingA $10c5, 7
 AKMWPagingA $10fb, 7
 AKMWPagingA $14e5, 5
 AKMWPagingA $150b, 7
 AKMWPagingA $1511, 5
 AKMWPagingA $1522, 7
 AKMWPagingA $156d, 5
 AKMWPagingA $1653, 2
 AKMWPagingA $16ac, 2
 AKMWPagingA $1729, 3
 AKMWPagingA $173c, 7
 AKMWPagingA $176c, 5
 AKMWPagingA $17cf, 2
 AKMWPagingA $1879, 2
 AKMWPagingA $189e, 2
 AKMWPagingA $18ac, 5
 AKMWPagingA $1925, 2
 AKMWPagingA $193b, 5
 AKMWPagingA $1993, 2
 AKMWPagingA $19c9, 5
 AKMWPagingA $1a19, 2
 AKMWPagingA $1a2d, 5
 AKMWPagingA $1aa5, 7
 AKMWPagingA $1ad8, 3
 AKMWPagingA $1aee, 2
 AKMWPagingA $1af6, 5
 AKMWPagingA $1bcc, 6
 AKMWPagingA $1c01, 2
 AKMWPagingA $1c5b, 3
 AKMWPagingA $1c63, 5
 AKMWPagingA $1c71, 2
 AKMWPagingA $1c81, 7
 AKMWPagingA $1cb4, 2
 AKMWPagingA $1cfd, 5
 AKMWPagingA $1d1d, 2
 AKMWPagingA $1d53, 3
 AKMWPagingA $1d66, 5
 AKMWPagingA $1df9, 6
 AKMWPagingA $1e5f, 2
 AKMWPagingA $1fb4, 2
 AKMWPagingA $1ffd, 3
 AKMWPagingA $2015, 5
 AKMWPagingA $2023, 7
 AKMWPagingA $2044, 5
 AKMWPagingA $2067, 5
 AKMWPagingA $20a1, 7
 AKMWPagingA $20ad, 2
 AKMWPagingA $219f, 5
 AKMWPagingA $21fe, 7
 AKMWPagingA $2226, 2
 AKMWPagingA $2275, 3
 AKMWPagingA $2288, 5
 AKMWPagingA $22ae, 7
 AKMWPagingA $236c, 2
 AKMWPagingA $24c9, 6
 AKMWPagingA $25ba, 5
 AKMWPagingA $25d4, 2
/*
 AKMWPagingA $0085, 2
 AKMWPagingA $009f, 2
 AKMWPagingA $0107, 2
 AKMWPagingA $04bc, 2
 AKMWPagingA $0787, 2
 AKMWPagingA $07ab, 4
 AKMWPagingA $0924, 2
 AKMWPagingA $0a12, 5
 AKMWPagingA $0a35, 5
 AKMWPagingA $0ac3, 2
 AKMWPagingA $0b3a, 5
 AKMWPagingA $0b48, 3
 AKMWPagingA $0b78, 2
 AKMWPagingA $0cae, 7
 AKMWPagingA $0cc8, 5
 AKMWPagingA $0cee, 2
 AKMWPagingA $10ff, 7
 AKMWPagingA $1134, 7
 AKMWPagingA $151e, 5
 AKMWPagingA $1544, 7
 AKMWPagingA $154a, 5
 AKMWPagingA $155b, 7
 AKMWPagingA $15a6, 5
 AKMWPagingA $1687, 2
 AKMWPagingA $16e0, 3
 AKMWPagingA $175e, 3
 AKMWPagingA $1771, 7
 AKMWPagingA $17a1, 5
 AKMWPagingA $1804, 2
 AKMWPagingA $18ae, 3
 AKMWPagingA $18d4, 2
 AKMWPagingA $18e2, 5
 AKMWPagingA $195d, 2
 AKMWPagingA $1973, 5
 AKMWPagingA $19cb, 2
 AKMWPagingA $1a01, 5
 AKMWPagingA $1a51, 2
 AKMWPagingA $1a65, 5
 AKMWPagingA $1add, 7
 AKMWPagingA $1b10, 3
 AKMWPagingA $1b26, 2
 AKMWPagingA $1b2e, 5
 AKMWPagingA $1c04, 6
 AKMWPagingA $1c39, 2
 AKMWPagingA $1c93, 3
 AKMWPagingA $1c9b, 5
 AKMWPagingA $1ca9, 2
 AKMWPagingA $1cb9, 7
 AKMWPagingA $1cec, 2
 AKMWPagingA $1d35, 5
 AKMWPagingA $1d55, 2
 AKMWPagingA $1d8b, 2
 AKMWPagingA $1d9e, 5
 AKMWPagingA $1e31, 6
 AKMWPagingA $1e97, 2
 AKMWPagingA $1fec, 2
 AKMWPagingA $2035, 3
 AKMWPagingA $204d, 5
 AKMWPagingA $205b, 7
 AKMWPagingA $207c, 5
 AKMWPagingA $209f, 5
 AKMWPagingA $20d9, 7
 AKMWPagingA $20e5, 2
 AKMWPagingA $21d7, 5
 AKMWPagingA $2236, 7
 AKMWPagingA $225e, 2
 AKMWPagingA $22ad, 2
 AKMWPagingA $22c0, 5
 AKMWPagingA $22e6, 7
 AKMWPagingA $23a4, 2
 AKMWPagingA $2501, 6
 AKMWPagingA $25f2, 5
 AKMWPagingA $260c, 2
*/
; This leaves a few tricky spots...

;    ld     a,($ffff)       ; 0000DE 3A FF FF
.orga $00ca ; was $00de
.section "Paging read fix part 1" overwrite
  ld a,(Slot2PageNumber)
.ends
; ...
;    ld     ($ffff),a       ; 000114 32 FF FF
.orga $0100 ; was $0114
.section "Paging read fix part 2" overwrite
  call AKMWPagingHelper
.ends

;    ld     hl,$ffff        ; 000867 21 FF FF
;    ld     (hl),$84        ; 00086A 36 84
; Need to leave a alone... but hl is fair game (not used afterwards)
.orga $0834 ; was $0867
.section "Different type of paging" overwrite
  ld h,4+:AlexKiddInMiracleWorld
  call AKMWPagingHelper2
.ends

.bank 5 slot 1
.orga $4000
.incbin "akbios.sms" skip $4000 read $4000
 AKMWPagingA $4191, 4
 AKMWPagingA $4214, 5
 AKMWPagingA $42c1, 5
 AKMWPagingA $4346, 4
 AKMWPagingA $4393, 5
 AKMWPagingA $4b67, 7
 AKMWPagingA $6035, 5
 AKMWPagingA $6040, 2
 AKMWPagingA $6227, 5
 AKMWPagingA $6238, 2
 AKMWPagingA $6691, 2
 AKMWPagingA $6840, 5
 AKMWPagingA $6a55, 5
 AKMWPagingA $6b0e, 2
 AKMWPagingA $6bf5, 2
 AKMWPagingA $6d72, 2
 AKMWPagingA $71c5, 2
 AKMWPagingA $78fc, 4
 AKMWPagingA $7dc3, 4
 AKMWPagingA $7ddf, 3
 AKMWPagingA $7dec, 7
 AKMWPagingA $7df9, 6
 AKMWPagingA $7e0f, 4
 AKMWPagingA $7e2a, 5
 AKMWPagingA $7eb8, 2
 AKMWPagingA $7f47, 2
 AKMWPagingA $7f65, 7

/*
 AKMWPagingA $41c8, 4
 AKMWPagingA $42f8, 5
 AKMWPagingA $437d, 4
 AKMWPagingA $43ca, 5
 AKMWPagingA $4b9e, 7
 AKMWPagingA $606c, 5
 AKMWPagingA $6077, 2
 AKMWPagingA $625e, 5
 AKMWPagingA $626f, 2
 AKMWPagingA $66c8, 2
 AKMWPagingA $6877, 5
 AKMWPagingA $6a90, 5
 AKMWPagingA $6b49, 2
 AKMWPagingA $6c30, 2
 AKMWPagingA $6dad, 2
 AKMWPagingA $71ff, 2
 AKMWPagingA $7932, 4
 AKMWPagingA $7e44, 2
 AKMWPagingA $7ed3, 2
 AKMWPagingA $7ef1, 7
*/
; non-BIOS AKMWPagingB $424d ; e.g. a = 85
 AKMWPagingB $6818 ; was $684f ; e.g. a = 86
 AKMWPagingB $6AF8 ; was $6b33 ; e.g. a = 85

 AKMWPagingC $6582 ; was $65b9

 ; Infinite lives
 patchByte $6bee+1, 0 ; sub 0 on dying ; was $6c29
;    ld     hl,$c025        ; 006BEA 21 25 C0 
;    ld     a,(hl)          ; 006BED 7E 
;    sub    $01             ; 006BEE D6 01 
;    jp     z,$6d8e         ; 006BF0 CA 8E 6D 
;    daa                    ; 006BF3 27 
;    ld     (hl),a          ; 006BF4 77 

; We add paging functions to the game, in some unused space...
.orga $7d96 ; was $7fd0, for the BIOS we overwrite the BIOS stuff
.section "Paging helpers" overwrite
AKMWPagingHelperWithMaskAndAdjustment:
  and $0f
  add a,:AlexKiddInMiracleWorld
AKMWPagingHelper:
  ld (Slot2Control),a
  ld (Slot2PageNumber),a
  ret
AKMWPagingHelper2:
  push af
    ld a,h
    ld (Slot2Control),a
    ld (Slot2PageNumber),a
  pop af
  ret
.ends

.bank 6
.org 0
.incbin "akbios.sms" skip $8000 read $4000

.bank 7
.org 0
.incbin "akbios.sms" skip $c000 read $4000

.bank 8
.org 0
.incbin "akbios.sms" skip $10000 read $4000

.bank 9
.org 0
.incbin "akbios.sms" skip $14000 read $4000

.bank 10
.org 0
.incbin "akbios.sms" skip $18000 read $4000

.bank 11
.org 0
.incbin "akbios.sms" skip $1c000 read $4000

.bank 12 slot 0
.orga 0
Columns:
.incbin "Columns.sms" skip $00000 read $4000

.orga $0066
.section "Columns pause killer" overwrite
  retn
.ends

.orga $89
.section "Paging reset" overwrite
  ld a,:Columns+2
  ld (Slot2Control),a
  jp $0097
.ends

; Move the stack down
 patchWord $0084+1, TopOfStack ; move the stack down
 patchWord $00ec+1, TopOfStack ; it's set twice
 patchWord $00b7+1, TopOfStack - $dd00 - 1 ; Memory blanking

; Disable title screen timeout
 nopOut $01b4, 1
 
; Disable reset button check
.orga $41d
.section "Reset check" overwrite
  ret
.ends

 patchByte $2fe3 $c9 ; Disable menu selections - level 1 - immediate ret
 patchByte $301a $c9 ; Disable menu selections - level 2 - immediate ret
 patchByte $3078 $18 ; Disable menu selections - level 3 - change conditional jump to unconditional

.orga $0206
.section "VBlank patch" overwrite
  jp JumpOut
  ; Replaced:
  ; push   ix              ; 000206 DD E5
  ; push   iy              ; 000208 FD E5

.ends

.orga $3b15
.section "Game Over patch" overwrite
; Hit only after game over
; We patch the VBlank helper and jump to it to escape the game.
  ld hl,ColumnsEnd_GameOver
  ld (JumpOutAddress), hl
  jp JumpOut
.ends

; Paging patches...
.macro ColumnsPagingA
;    ld     a,$02           ; 001652 3E 02
;    ld     ($ffff),a       ; 001654 32 FF FF
.orga \1
.section "ColumnsPagingA\@" overwrite
  ld a,\2+:Columns
  call ColumnsPagingHelper
.ends
.endm

; Generated...
 ColumnsPagingA $1652, 2
 ColumnsPagingA $176a, 2
 ColumnsPagingA $17b2, 2
 ColumnsPagingA $1879, 2
 ColumnsPagingA $1bac, 2
 ColumnsPagingA $1bdb, 3
 ColumnsPagingA $1c13, 2
 ColumnsPagingA $1c24, 3
 ColumnsPagingA $1c6e, 2
 ColumnsPagingA $1cc3, 2
 ColumnsPagingA $1cf7, 3
 ColumnsPagingA $1d1a, 2
 ColumnsPagingA $1d2b, 3
 ColumnsPagingA $1d8e, 2
 ColumnsPagingA $1dc7, 2
 ColumnsPagingA $1e26, 2
 ColumnsPagingA $1e3a, 2
 ColumnsPagingA $1e7f, 2
 ColumnsPagingA $1eb5, 3
 ColumnsPagingA $1edb, 2
 ColumnsPagingA $1eec, 3
 ColumnsPagingA $1f47, 2
 ColumnsPagingA $1f7c, 2
 ColumnsPagingA $1fc5, 2
 ColumnsPagingA $1ffa, 2
 ColumnsPagingA $205d, 2
 ColumnsPagingA $20a2, 2
 ColumnsPagingA $20e0, 3
 ColumnsPagingA $2112, 2
 ColumnsPagingA $2123, 3
 ColumnsPagingA $21a8, 2
 ColumnsPagingA $21ef, 2
 ColumnsPagingA $222b, 2
 ColumnsPagingA $2281, 2
 ColumnsPagingA $22ed, 2
 ColumnsPagingA $234c, 2
 ColumnsPagingA $237b, 3
 ColumnsPagingA $23ba, 2
 ColumnsPagingA $23cb, 3
 ColumnsPagingA $241a, 2
 ColumnsPagingA $246f, 2
 ColumnsPagingA $249e, 3
 ColumnsPagingA $24c1, 2
 ColumnsPagingA $24d2, 3
 ColumnsPagingA $2540, 2
 ColumnsPagingA $2579, 2

; The more complicated ones, where it's reading the page number and restoring it
.orga $01fa
;    ld     a,($ffff)       ; 0001FA 3A FF FF
;    push   af              ; 0001FD F5
.section "Columns paging save" overwrite
  ld a,(Slot2PageNumber)
  push af
.ends
.orga $023a
;    pop    af              ; 00023A F1
;    ld     ($ffff),a       ; 00023B 32 FF FF
.section "Columns paging restore 1" overwrite
  pop af
  call ColumnsPagingHelper
.ends
.orga $027b
;    pop    af              ; 00027B F1
;    ld     ($ffff),a       ; 00027C 32 FF FF
.section "Columns paging restore 2" overwrite
  pop af
  call ColumnsPagingHelper
.ends


.bank 13 slot 1
.orga $4000
.incbin "Columns.sms" skip $04000 read $4000

.orga $7f00
.section "Paging helper" overwrite
ColumnsPagingHelper:
  ld (Slot2Control),a
  ld (Slot2PageNumber),a
  ret

.ends

.bank 14
.org 0
.incbin "Columns.sms" skip $08000 read $4000

.bank 15
.org 0
.incbin "Columns.sms" skip $0c000 read $4000

.bank 16
.orga 0
.incbin "Columns.sms" skip $10000 read $4000

.bank 17
.org 0
.incbin "Columns.sms" skip $14000 read $4000

.bank 18
.org 0
.incbin "Columns.sms" skip $18000 read $4000

.bank 19
.org 0
.incbin "Columns.sms" skip $1c000 read $4000

.slot 2
.section "Music data" superfree
MusicData:
;.incbin "butterfly.psgmod.epsgmod"
;.incbin "1ultra.psgmod.epsgmod"
;.incbin "Oxygene.epsgmod"
;.incbin "run_under_fire.epsgmod"
.incbin "Track_01.epsgmod"
.ends

.section "Vibrato tables" align $4000 superfree 
PSGMOD_VIBRATO_TABLES:
.incbin "player_z80/psgmod.vib"
.ends

/*
.bank 22 slot 0
.org 0
Sonic:
.incbin "Sonic The Hedgehog.sms" skip $00000 read $4000

.org $000c
.section "Sonic paging helpers 1" overwrite ; We are fitting around the interrupt vectors, we have to not trash what is there
SonicPagingfffea: ; 6 bytes
  add a,:Sonic ; 2
SonicPagingfffea_no_adjustment:
  ld ($4000),a ; 3
  ret          ; 1
SonicPagingffffa: ; 6 bytes
  add a,:Sonic ; 2
SonicPagingffffa_no_adjustment:
  ld ($8000),a ; 3
  ret          ; 1
  ; Interrupt vector
  jp $02d7     ; 000018 C3 D7 02 
  ; 5b space
SonicPagingfffede:
  push af         ; 1
    ld a,d        ; 1
    jr +          ; 2
    nop           ; 1
  ; Interrupt vector
  jp $02ed        ; 000020 C3 ED 02 
  ; 5b space
+:  ld ($8000),a  ; 3
    jr +          ; 2
  ; Interrupt vector
  jp $02fe        ; 000028 C3 FE 02 
  ; 13b space
+:  ld a,e        ; 1
    ld ($4000),a  ; 3
  pop af          ; 1
  ret             ; 1
SonicPagingfffehl:
  ; Pages are pre-adjusted
  ; We can trash a
  ld a,h              ; 1
  ld ($8000),a        ; 3
  ld a,l              ; 1
  jr SonicPagingfffea_no_adjustment ; 2
.ends

.org $00e2
.section "Post VBlank fixup" overwrite
;    pop    hl              ; 0000E2 E1 
;    ld     ($fffe),hl      ; 0000E3 22 FE FF 
;    ld     ($d235),hl      ; 0000E6 22 35 D2 
  pop hl
  call SonicPagingfffehl
  ld ($d235),hl
.ends

.orga $028b
.section "Mapper initialisation removal, memory blanking at startup" overwrite
;    ld     a,$80           ; 00028B 3E 80 
;    ld     ($fffc),a       ; 00028D 32 FC FF 
;    ld     a,$00           ; 000290 3E 00 
;    ld     ($fffd),a       ; 000292 32 FD FF 
;    ld     a,$01           ; 000295 3E 01 
;    ld     ($fffe),a       ; 000297 32 FE FF 
;    ld     a,$02           ; 00029A 3E 02 
;    ld     ($ffff),a       ; 00029C 32 FF FF 
;    ld     hl,$c000        ; 00029F 21 00 C0 
;    ld     de,$c001        ; 0002A2 11 01 C0 
;    ld     bc,$1fef        ; 0002A5 01 EF 1F 
;    ld     (hl),l          ; 0002A8 75 
;    ldir                   ; 0002A9 ED B0 
  ld hl,(:Sonic << 8) + :Sonic + 1 ; 3
  ld ($d235),hl                    ; 3
  .dsb 4 $00 ; fill the space

.ends
 
; Memory blanking at startup, stack location
;    ld     hl,$c000        ; 00029F 21 00 C0 
;    ld     de,$c001        ; 0002A2 11 01 C0 
;    ld     bc,$1fef        ; 0002A5 01 EF 1F 
;    ld     (hl),l          ; 0002A8 75 
;    ldir                   ; 0002A9 ED B0 
;    ld     sp,hl           ; 0002AB F9 
 patchWord $02a5+1, TopOfStack - $c000 - 1
 
.orga $4ea
.section "Paging via de fixup" overwrite
;    pop    de              ; 0004EA D1 
;    ld     ($d235),de      ; 0004EB ED 53 35 D2 
;    ld     ($fffe),de      ; 0004EF ED 53 FE FF 
  pop de ; not sure if I need to leave it there... I think it's pre-adjusted
  ld ($d235),de
  call SonicPagingfffede
  nop ; to fill out the space
.ends

.macro M_SonicPagingfffea
.orga \1
.section "M_SonicPagingfffea\@" overwrite
call SonicPagingfffea
.ends
.endm
.macro M_SonicPagingffffa
.orga \1
.section "M_SonicPagingfffffa\@" overwrite
call SonicPagingffffa
.ends
.endm
.macro M_SonicPagingffffa_no_adjustment
.orga \1
.section "M_SonicPagingfffffa_no_adjustment\@" overwrite
call SonicPagingffffa_no_adjustment
.ends
.endm



; Generated...
 M_SonicPagingfffea $000C5
 M_SonicPagingfffea $0012F
 M_SonicPagingffffa $00137
 M_SonicPagingfffea $00146
 M_SonicPagingffffa $0014E
 M_SonicPagingfffea $00176
 M_SonicPagingffffa $0017E
 M_SonicPagingfffea $001A7
 M_SonicPagingffffa $001AF
 M_SonicPagingfffea $00297
 M_SonicPagingffffa $0029C
 M_SonicPagingfffea $002DB
 M_SonicPagingfffea $002E8
 M_SonicPagingfffea $002F0
 M_SonicPagingfffea $002F9
 M_SonicPagingfffea $00302
 M_SonicPagingfffea $0030C
 M_SonicPagingfffea $003BC
 M_SonicPagingffffa_no_adjustment $003C3
 M_SonicPagingfffea $003FC
 M_SonicPagingffffa $00400
 M_SonicPagingfffea $00426
 M_SonicPagingffffa_no_adjustment $0042D
 M_SonicPagingfffea $006C5
 M_SonicPagingffffa $006CD
 M_SonicPagingfffea $00969
 M_SonicPagingffffa $00971
 M_SonicPagingfffea $00A42
 M_SonicPagingffffa $00A4A
 M_SonicPagingfffea $00ABE
 M_SonicPagingffffa $00AC6
 M_SonicPagingfffea $00B70
 M_SonicPagingffffa $00B78
 M_SonicPagingfffea $00C20
 M_SonicPagingfffea $00C4D
 M_SonicPagingfffea $00CAC
 M_SonicPagingfffea $00D0E
 M_SonicPagingfffea $012AE
 M_SonicPagingfffea $0141E
 M_SonicPagingfffea $0158D
 M_SonicPagingfffea $01CEF
 M_SonicPagingfffea $01D57
 M_SonicPagingfffea $01D81
 M_SonicPagingffffa $01D89
 M_SonicPagingfffea $01DB7
 M_SonicPagingfffea $01E52
 M_SonicPagingffffa $01E5A
 M_SonicPagingfffea $01EB3
 M_SonicPagingfffea $01ECE
 M_SonicPagingfffea $01F0E
 M_SonicPagingffffa $01F16
 M_SonicPagingfffea $020BA
 M_SonicPagingfffea $0221E
 M_SonicPagingffffa $02226
 M_SonicPagingfffea $02230
 M_SonicPagingffffa $02238
 M_SonicPagingfffea $0227F
 M_SonicPagingffffa $02287
 M_SonicPagingfffea $022BF
 M_SonicPagingffffa $022C7
 M_SonicPagingfffea $022E2
 M_SonicPagingfffea $025B6
 M_SonicPagingfffea $025DD
 M_SonicPagingfffea $02677
 M_SonicPagingfffea $026C3

.bank 23 slot 1
.org 0
.incbin "Sonic The Hedgehog.sms" skip $04000 read $4000

 M_SonicPagingffffa $049E9
 M_SonicPagingffffa $04A20

.bank 24 slot 2
.org 0
.incbin "Sonic The Hedgehog.sms" skip $08000 read $4000

.bank 25
.org 0
.incbin "Sonic The Hedgehog.sms" skip $0c000 read $4000

.bank 26
.orga 0
.incbin "Sonic The Hedgehog.sms" skip $10000 read $4000

.bank 27
.org 0
.incbin "Sonic The Hedgehog.sms" skip $14000 read $4000

.bank 28
.org 0
.incbin "Sonic The Hedgehog.sms" skip $18000 read $4000

.bank 29
.org 0
.incbin "Sonic The Hedgehog.sms" skip $1c000 read $4000

.bank 30
.org 0
.incbin "Sonic The Hedgehog.sms" skip $20000 read $4000

.bank 31
.org 0
.incbin "Sonic The Hedgehog.sms" skip $24000 read $4000

.bank 32
.org 0
.incbin "Sonic The Hedgehog.sms" skip $28000 read $4000

.bank 33
.org 0
.incbin "Sonic The Hedgehog.sms" skip $2c000 read $4000

.bank 34
.orga 0
.incbin "Sonic The Hedgehog.sms" skip $30000 read $4000

.bank 35
.org 0
.incbin "Sonic The Hedgehog.sms" skip $34000 read $4000

.bank 36
.org 0
.incbin "Sonic The Hedgehog.sms" skip $38000 read $4000

.bank 37
.org 0
.incbin "Sonic The Hedgehog.sms" skip $3c000 read $4000
; */