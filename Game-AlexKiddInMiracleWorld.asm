.include "Common.inc"

.bank 0 slot 0

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
  LoadScreen Tilemap_AKMW
  call ScreenOn
  call FadeInFullPalette
  call WaitForButton
  call FadeOutFullPalette
  call ScreenOff

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

  ; Get the money score
  call AKMWGetScore

  ; Jump to the next game
  jp HangOnStart

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

.bank AKMWStartBank+0
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

.orga $00bc
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
 patchWord $0099+1, TopOfStack ; it's set twice
 patchWord $008b+1, TopOfStack - $c000 - 1 ; shorten range of memory cleared on startup

; Disable title screen timeout
 nopOut $07cc, 1

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
 patchByte $07f5 5 ; Lake Fathom Part 2

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

; This leaves a few tricky spots...

;    ld     a,($ffff)       ; 0000DE 3A FF FF
.orga $00ca
.section "Paging read fix part 1" overwrite
  ld a,(Slot2PageNumber)
.ends
; ...
;    ld     ($ffff),a       ; 000114 32 FF FF
.orga $0100
.section "Paging read fix part 2" overwrite
  call AKMWPagingHelper
.ends

;    ld     hl,$ffff        ; 000867 21 FF FF
;    ld     (hl),$84        ; 00086A 36 84
; Need to leave a alone... but hl is fair game (not used afterwards)
.orga $0834
.section "Different type of paging" overwrite
  ld h,4+:AlexKiddInMiracleWorld
  call AKMWPagingHelper2
.ends

.bank AKMWStartBank+1, slot 1
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

 AKMWPagingB $6818 ; e.g. a = 86
 AKMWPagingB $6AF8 ; e.g. a = 85

 AKMWPagingC $6582

 ; Infinite lives
 patchByte $6bee+1, 0 ; sub 0 on dying
;    ld     hl,$c025        ; 006BEA 21 25 C0 
;    ld     a,(hl)          ; 006BED 7E 
;    sub    $01             ; 006BEE D6 01 
;    jp     z,$6d8e         ; 006BF0 CA 8E 6D 
;    daa                    ; 006BF3 27 
;    ld     (hl),a          ; 006BF4 77 

; We add paging functions to the game, in some unused space...
.orga $7d96 ; we overwrite the BIOS stuff
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

.bank AKMWStartBank+2
.org 0
.incbin "akbios.sms" skip $8000 read $4000

.bank AKMWStartBank+3
.org 0
.incbin "akbios.sms" skip $c000 read $4000

.bank AKMWStartBank+4
.org 0
.incbin "akbios.sms" skip $10000 read $4000

.bank AKMWStartBank+5
.org 0
.incbin "akbios.sms" skip $14000 read $4000

.bank AKMWStartBank+6
.org 0
.incbin "akbios.sms" skip $18000 read $4000

.bank AKMWStartBank+7
.org 0
.incbin "akbios.sms" skip $1c000 read $4000
