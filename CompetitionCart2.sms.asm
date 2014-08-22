.include "Common.inc"

;.define TEXT_MODE

.define TILE_COMPRESSION "Sonic" ; could be PSG, PS, raw, aPLib, PuCrunch, Sonic

; Benchmarking the compression of our large tiles blob...
;
; Compression  Bytes  Ratio  Load time (cycles)  Ratio
; None          9888   100%              494435   100%
; PScompr       8472    86%             1354926   274%
; Sonic 1       5616    57%             1026969   207%
; PSGcompr      5116    52%             1600820   324%
; PuCrunch      4109    42$             3475587   703%
; aPLib         4058    41%             3640882   736%
;
; Decompressor sizes are not included in the byte counts.

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

  jp SonicStart;AKMWStart
  
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
  ld a,:Tiles
  ld ($8000),a
.if TILE_COMPRESSION == "raw"
  ld hl,Tiles
  ld de,$4000
  ld bc,TilesSize
  call raw_decompress ; TODO
.endif
.if TILE_COMPRESSION == "PSG"
  ld ix,Tiles
  ld hl,$4000
  call PSG_decompress
.endif
.if TILE_COMPRESSION == "PS"
  ld hl,Tiles
  ld de,$4000
  call LoadTiles4BitRLENoDI
.endif
.if TILE_COMPRESSION == "aPLib"
  ld hl,Tiles
  ld de,$4000
  call aPLib_decompress
.endif
.if TILE_COMPRESSION == "PuCrunch"
  ld hl,Tiles
  ld de,$4000
  call Uncrunch
.endif
.if TILE_COMPRESSION == "Sonic"
  ld hl,Tiles
  ld de,$4000
  call Sonic1TileLoader_Decompress
.endif
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
.if TILE_COMPRESSION == "raw"
.incbin "Tiles.bin" fsize TilesSize
.endif
.if TILE_COMPRESSION == "PSG"
.incbin "Tiles.psgcompr"
.endif
.if TILE_COMPRESSION == "PS"
.incbin "Tiles.pscompr"
.endif
.if TILE_COMPRESSION == "aPLib"
.incbin "Tiles.aPLib"
.endif
.if TILE_COMPRESSION == "PuCrunch"
.incbin "Tiles.PuCrunch"
.endif
.if TILE_COMPRESSION == "Sonic"
.incbin "Tiles.soniccompr"
.endif
.ends
.section "Tilemaps" superfree
Tilemaps:
.incbin "Tilemaps.bin" skip 32*2*4 ; top 4 rows are digits
.ends
.slot 0

.if TILE_COMPRESSION == "raw"
.section "Raw data loader" free
raw_decompress:
  ; hl = src
  ; de = dest
  ; bc = count
  ; Set VRAM address
  ld a,e
  out ($bf),a
  ld a,d
  out ($bf),a
  ; Output
-:ld a,(hl)
  inc hl
  out ($be),a
  dec bc
  ld a,b
  or c
  jr nz,-
  ret
.ends
.endif
.if TILE_COMPRESSION == "PSG"
.include "Phantasy Star Gaiden decompressor.inc"
.endif
.if TILE_COMPRESSION == "PS"
.include "Phantasy Star decompressors.inc"
.endif
.if TILE_COMPRESSION == "aPLib"
.define aPLibToVRAM
.include "aplib-z80.asm"
.endif
.if TILE_COMPRESSION == "PuCrunch"
.define PuCrunchToVRAM
.include "uncrunch-z80.asm"
.endif
.if TILE_COMPRESSION == "Sonic"
.include "Sonic decompressor.inc"
.endif
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

.ifdef TEXT_MODE
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
.endif
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

; The games

; We have to split them into chunks manually to get the bank numbers to assemble correctly.
; Macros don't work as the offsets exceed 16 bits and WLA DX doesn't like that.

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

