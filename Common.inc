.smstag ; has to be included in all files

.memorymap
slotsize $4000
slot 0 $0000
slot 1 $4000
slot 2 $8000
defaultslot 0
.endme

.emptyfill $aa

; Enumeration to define bank numbers
; Has to come before the rombankmap
.enum 0
Framework             db ; Framework code, boot, etc
Framework2            db ; Pretty much just the ROM headers...
Mod2PSG2Data          db
; numbers are the size (in 16KB banks) of the game in question...
;HangOnStartBank       dsb 2
;AKMWStartBank         dsb 8
SonicStartBank        dsb 16
;ColumnsStartBank      dsb 8
DrRobotniksStartBank  dsb 16
OutRunStartBank       dsb 16
; End marker
NumberOfBanks         .db
.ende

.rombankmap
bankstotal NumberOfBanks
banksize $4000
banks NumberOfBanks
.endro

; Memory map (for harness)
; Note that we use the memory right up to $dfff - we assume there's no writes going to Sega paging registers any more
.enum $e000 desc export
Slot2PageNumber          db ; Last page mapped to slot 2. This ends up in $dfff so any game code reading there or $ffff will still work.
FrameCounter             dw ; Enough for up to 21.8 minutes (at 50Hz)...
;AKMWMoneyDividedBy10     db ; Score from game 1
;HangOnScoreDividedBy10   dw ; Score from game 2
;ColumnsScoreDividedBy10  dw ; Score from game 3, truncated to 2 bytes
OutRunScore              db ; 1 point for finishing
SonicScore               db ; 1 point for finishing
DrRobotniksScore         dsb 4
RAMJumpHelpers           dsb 9 ; RAM-located code for jumping between the game and the framework
GameNumber               db ; Current game
TopOfStack               db ; Rest of space is for games to use, they need to have their stacks patched
.ende

; Memory map (for when no game is running)
; Note that we should grab any game state before using any of this...
.enum $c000 asc export
Port3EValue               db
GameLoaderRAMAddress      dsb 255 ; Doesn't need all of this - see next line
PSGMOD_START_ADDRESS      dsb 384 ; Must be 256-byte aligned...
aPLibMemory               .db ; needs 5
PucrunchVars              .db ; needs 44
PSGDecoderBuffer          .db ; needs 34
Sonic1TileLoaderMemory    .db ; needs 8
DecompressorBuffer        dsb 44
NumberTileIndicesOffset   dw ; So I can choose colours
PaletteFadeControl        db ; Taken from Ohantasy Star...
PaletteSize               db
CurrentPalette            dsb 32
TargetPalette             dsb 32
PaletteFadeFrameCounter   db
.ende


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

.macro LoadScreen
  ld a,:Tilemaps
  ld (Slot2Control),a
  ld hl, Tilemaps + 32*24*2*\1
  call LoadScreenImpl
.endm

; Games should set this as they bootstrap
.macro SetFrameHandler
  ld hl,\1
  ld (JumpOutAddress), hl
.endm

; Games may use this to jump out of the game from within game code
.macro ExitTo
  SetFrameHandler \1
  jp JumpOut
.endm

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

; See "RAMJumpHelpersCode" for these magic numbers
.define JumpOut RAMJumpHelpers + 0
.define JumpOutAddress RAMJumpHelpers + 2
.define JumpBack RAMJumpHelpers + 5
