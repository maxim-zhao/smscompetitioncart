.include "Common.inc"

.define TILE_COMPRESSION "aPLib" ; could be PSG, PS, raw, aPLib, PuCrunch, Sonic

; Benchmarking the compression of our large tiles blob...
;
; Compression  Data size  Ratio  Decompressor size  Load time (cycles)  Ratio
; None              9728  100%                  24              161365   100%
; PScompr           8338   86%                  54             1335193   827%
; Sonic 1           5507   57%                 162             1011588   627%
; PSGcompr          5029   52%                 223             1576965   977%
; PuCrunch          4005   41%                 414             3394510  2104%
; aPLib             3946   41%                 304             3552372  2201%
; aPLib-fast        3946   41%                 334             1789523  1109%
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
    call UpdatePalette
    call HandlePaletteFade
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

  jp OutRunStart;SonicStart
  
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
  
  ; Blacken palette
  ld a,$00
  out (Port_VDPAddress),a
  ld a,$c0
  out (Port_VDPAddress),a
  xor a
  ld b,32
-:out (Port_VDPData),a
  djnz -

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
  
  ; Load palette, blacken "current palette"
  ld hl,PaletteData
  ld de,TargetPalette
  ld bc,32
  ldir
  ld hl,CurrentPalette
  ld de,CurrentPalette+1
  xor a
  ld (hl),a
  ld bc,31
  ldir

  ; Load graphics
  ld a,:Tiles
  ld ($8000),a
x:
.if TILE_COMPRESSION == "raw"
  ld hl,Tiles
  ld de,$4000
  ld bc,TilesSize
  call raw_decompress
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
  
  ; Start music
  ld a,:MusicData
  ld hl,MusicData
  call PSGMOD_LoadModule
  ld hl,+ ; safe "do nothing" function
  call PSGMOD_SetCallbackFunction
  call PSGMOD_Start
  ei
+:ret

PaletteData:
.db $00 $10 $34 $25 $35 $39 $2A $3A $2B $0F $1F $3F 0 0 0 0
.dsb 16 $35 ; background
PaletteDataEnd:

; VDP initialisation data
VdpData:
.db $04,$80,$04,$81,$ff,$82,$ff,$85,$ff,$86,$ff,$87,$00,$88,$00,$89,$ff,$8a
VdpDataEnd:
.ends

.slot 2
.section "Graphics data" superfree
Tiles:
.block "Tile_data"
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
.endb
.ends
.section "Tilemaps" superfree
Tilemaps:
.incbin "Tilemaps.bin" skip 32*2*4 ; top 4 rows are digits
.enum 0 asc export
Tilemap_TitleScreen db
;Tilemap_AKMW        db
;Tilemap_HangOn      db
;Tilemap_Columns     db
Tilemap_OutRun      db
Tilemap_Sonic       db
Tilemap_DrRobotniks db
Tilemap_TimeUp      db
Tilemap_Results     db
.ende
.ends
.slot 0

.section "outi block" align 256
outiblock:
.rept 256
  outi
.endr
  ret
.ends

.section "Tile loader"
.block "Tile_loader"
.if TILE_COMPRESSION == "raw"
raw_decompress:
  ; hl = src
  ; de = dest
  ; bc = count
  ; save c
  ld ixl,c
  ; Set VRAM address
  ld c,Port_VDPAddress
  out (c),e
  out (c),d
  ld c,Port_VDPData
  ; Check for bc<256
  ld a,b
  or a
  jr z,+
  ; Output 256 byte chunks
-:call outiblock
  djnz -
  ; Output remaining bits
+:ld ixh,>outiblock
  jp (ix) ; and ret
.endif
.if TILE_COMPRESSION == "PSG"
.include "Phantasy Star Gaiden decompressor.inc"
.endif
.if TILE_COMPRESSION == "PS"
.include "Phantasy Star decompressors.inc"
.endif
.if TILE_COMPRESSION == "aPLib"
.define aPLibToVRAM
.include "aplib-z80-fast.asm"
.endif
.if TILE_COMPRESSION == "PuCrunch"
.define PuCrunchToVRAM
.include "uncrunch-z80.asm"
.endif
.if TILE_COMPRESSION == "Sonic"
.include "Sonic decompressor.inc"
.endif
.endb
.ends

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
  xor a
  ld (NumberTileIndicesOffset),a

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
  xor a
  ld (NumberTileIndicesOffset),a

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
  ld a,OFFSET_PINK_DIGITS
  ld (NumberTileIndicesOffset),a

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

ShowTime:
  ; Args:
  ; hl = time in frames
  ; de = VRAM address of the most significant digit
  ; Trashes: bc, hl', bc', a
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
    ; TODO: colon
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
  ret
.ends

.section "Title screen" free
TitleScreen:
  LoadScreen Tilemap_TitleScreen

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
  call FadeInFullPalette
  call WaitForButton
  call FadeOutFullPalette
  call ScreenOff

  ret
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
  
  LoadScreen Tilemap_TimeUp
  call ScreenOn
  call FadeInFullPalette
  call WaitForButton
  call FadeOutFullPalette
  call ScreenOff

FinalResults:
  
  ; Process the score for the game in progress
  ld a,(GameNumber)
;  or a
;  call z,OutRunGetScore
  dec a
;  call z,SonicGetScore
  dec a
  call z,DrRobotniksGetScore

  LoadScreen Tilemap_Results

  ; Draw the scores
  ld a,(OutRunScore)
  ld bc,0
  ld h,0
  ld l,a
  LocationToDE 20, 4
  call WriteNumberSevenDigits

  ld a,(SonicScore)
  ld bc,0
  ld h,0
  ld l,a
  LocationToDE 20, 9
  call WriteNumberSevenDigits

  ld hl,(DrRobotniksScore+0)
  ld bc,(DrRobotniksScore+2)
  LocationToDE 20, 14
  call WriteNumberSevenDigits
  
  ; Calculate the total
  ld hl,(DrRobotniksScore+0)
  ld bc,(DrRobotniksScore+2)
  ld a,(OutRunScore)
  ld e,a
  ld a,(SonicScore)
  add a,e
  ld e,a
  ld d,0
  call AddDEToBCHLLow


/*
  ; Draw stuff. We retrieve the scores from RAM and scale them up as necessary.
  ld ix,OutRunScore
  ld h,(ix+0)
  ld l,(ix+1)
  ld b,(ix+2)
  ld c,(ix+3)
  ld a,10
  ; call BCHLTimesAToBCHL ; TODO
  LocationToDE 5, 4
  call WriteNumberSevenDigits

  ld a,(SonicRings)
  ld de,10000
  call DETimesAToBCHL
  LocationToDE 5, 9
  call WriteNumberSevenDigits

  ld ix,DrRobotniksScore
  ld h,(ix+0)
  ld l,(ix+1)
  ld b,(ix+2)
  ld c,(ix+3)
  ld a,10
  ; call BCHLTimesAToBCHL ; TODO
  LocationToDE 5, 14
  call WriteNumberSevenDigits
*/


  ; Calculate the total score
  /*
  ; 1. AKMW x 100. Can't overflow...
  ld a,(AKMWMoneyDividedBy10)
  ld de, 100*10
  call DETimesAToBCHL
  push bc
    push hl
      LocationToDE 20, 4
      call WriteNumberSevenDigits
      ; 2. Hang On
      ld de,(HangOnScoreDividedBy10) ; up to about 8000
      ld a,10
      call DETimesAToBCHL ; may reach into c?
      push bc
        push hl
          LocationToDE 20, 9
          call WriteNumberSevenDigits
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
          push bc
          push hl
            LocationToDE 20, 14
            call WriteNumberSevenDigits
          pop hl
          pop bc
          ; Unwind and add up...
        pop de
        call AddDEToBCHLLow
      pop de
      call AddDEToBCHLHigh
    pop de
    call AddDEToBCHLLow
  pop de
  call AddDEToBCHLHigh
    */
  ; Done! Now to display it...
  LocationToDE 17, 19
  call WriteNumber32

  call ScreenOn
  call FadeInFullPalette
  call WaitForButton
  call FadeOutFullPalette
  call ScreenOff
  jp 0
.ends

.section "Mod2PSG2" free
.define PSGMOD_SUPPORT_GG_STEREO 0
.define PSGMOD_PSG_PORT $7f
.define PSGMOD_MAPPER_ADDRESS $8000
.include "player_z80/psgmod.inc"
.include "player_z80/psgmod.asm"
.ends

.section "Palette fading" free
; This is lifted from Phantasy Star.
; Call FadeInFullPalette and FadeOutFullPalette to initiate a palette fade. 
; These rely on HandlePaletteFade being called in the VBlank, and won't return
; until the fade is complete.
FadeInFullPalette:
  ld hl,$2089
  ld (PaletteFadeControl),hl ; PaletteFadeControl = fade in/counter=9; PaletteSize=32
  jr _DoFade

FadeOutFullPalette:
  ld hl,$2009
  ld (PaletteFadeControl),hl ; PaletteFadeControl = fade out/counter=9; PaletteSize=32
  ; fall through

_DoFade:
  halt
  ld a,(PaletteFadeControl)       ; wait for palette to fade out
  or a
  jp nz,_DoFade
  ret

UpdatePalette:
  ; Copy palette from RAM to VRAM
  ld c,Port_VDPAddress
  ld a,0
  out (c),a
  ld a,$c0
  out (c),a
  ld hl,CurrentPalette
  ld c,Port_VDPData
.rept 32
  outi
.endr
  ret

HandlePaletteFade:
; stolen from Phantasy Star
; must run every VBlank
; Main function body only runs every 4 calls (using PaletteFadeFrameCounter as a counter)
; Checks PaletteFadeControl - bit 7 = fade in, rest = counter
; PaletteSize tells it how many palette entries to fade
; TargetPalette and ActualPalette are referred to
    ld hl,PaletteFadeFrameCounter ; Decrement PaletteFadeFrameCounter
    dec (hl)
    ret p              ; return if >=0
    ld (hl),3          ; otherwise set to 3 and continue (so only do this part every 4 calls)
    ld hl,PaletteFadeControl ; Check PaletteFadeControl
    ld a,(hl)
    bit 7,a            ; if bit 7 is set
    jp nz,_FadeIn      ; then fade in
    or a               ; If PaletteFadeControl==0
    ret z              ; then return
    dec (hl)           ; Otherwise, decrement PaletteFadeControl
    inc hl
    ld b,(hl)          ; PaletteSize
    ld hl,CurrentPalette
  -:call _FadeOut      ; process PaletteSize bytes from ActualPalette
    inc hl
    djnz -
    ret

FadeOutPaletteEntryAtHL:
_FadeOut:
    ld a,(hl)
    or a
    ret z              ; zero = black = no fade to do

    and %11<<0         ; check red
    jr z,+
    dec (hl)           ; If non-zero, decrement
    ret

  +:ld a,(hl)
    and %11<<2         ; check green
    jr z,+
    ld a,(hl)
    sub 1<<2           ; If non-zero, decrement
    ld (hl),a
    ret

  +:ld a,(hl)
    and %11<<4         ; check blue
    ret z
    sub 1<<4            ; If non-zero, decrement
    ld (hl),a
    ret

_FadeIn:
    cp $80             ; Is only bit 7 set?
    jr nz,+            ; If not, handle that
    ld (hl),$00        ; Otherwise, zero it (PaletteFadeControl)
    ret
  +:dec (hl)           ; Decrement it (PaletteFadeControl)
    inc hl
    ld b,(hl)          ; PaletteSize
    ld hl,TargetPalette
    ld de,CurrentPalette
  -:call _FadePaletteEntry ; fade PaletteSize bytes from ActualPalette
    inc hl
    inc de
    djnz -
    ret

_FadePaletteEntry:
    ld a,(de)          ; If (de)==(hl) then leave it
    cp (hl)
    ret z
    add a,%00010000    ; increment blue
    cp (hl)
    jr z,+
    jr nc,++           ; if it's too far then try green
  +:ld (de),a          ; else save that
    ret
 ++:ld a,(de)
    add a,%00000100    ; increment green
    cp (hl)
    jr z,+
    jr nc,++           ; if it's too far then try red
  +:ld (de),a          ; else save that
    ret
 ++:ex de,hl
    inc (hl)           ; increment red
    ex de,hl
    ret
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
; We can't also make it a valid SDSC header...
.ends

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

