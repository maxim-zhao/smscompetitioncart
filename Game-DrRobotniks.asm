.include "Common.inc"

.bank 0 slot 0

; Memory addresses
.define ControlInputs $ca18
.define MenuIndex     $d401

.section "DrRobotniks helpers" free

DrRobotniksStart:
/*  LoadScreen Tilemap_DrRobotniks
  call ScreenOn
  call FadeInFullPalette
  call WaitForButton
  call FadeOutFullPalette
  call ScreenOff
*/
  ; Record the game number
  ld a,2
  ld (GameNumber),a

  ; Patch in where to go...
  ld hl,DrRobotniksFrameHandler
  ld (JumpOutAddress), hl

  ; Jump to the game
  ld a,:DrRobotniks
  jp LoadGame
  
DrRobotniksFrameHandler:
/*
  ; af, bc, de, hl are safe to use
  ; What I replaced to get here
  push ix
  push iy
  
  call CheckForReset

  ; Are we in the game?
  ld a,(DrRobotniksModeControl)
  cp DrRobotniksModeControl_InGame
  jr nz,+ ; No: do nothing
  ld a,(DrRobotniksIsPaused)
  or a
  jr nz,+ ; This is 1 if the game is not actually active... (paused, "Ready", "Game Over")

  ; Decrement timer
  ld hl,(FrameCounter)
  dec hl
  ld (FrameCounter),hl
  ld a,h
  or l
  jp z,TimeUp
*/
  ; That's it!
+:ld a,:DrRobotniks
  ld hl,DrRobotniksReturnAddress
  push hl
    jp JumpBack
  
DrRobotniksGetScore:
/*
  ; We truncate to 16 bits, so a maximum of 655350
  ld hl,(DrRobotniksScoreLowWord)
  ld a,(DrRobotniksScoreHighByte)
  or a
  jr z,+
  ld hl,$ffff
+:ld (DrRobotniksScoreDividedBy10),hl
*/
  ret
.ends

.bank DrRobotniksStartBank slot 0
.orga 0
DrRobotniks:
.incbin "Dr. Robotnik's Mean Bean Machine.sms" skip $00000 read $4000

.define DrRobotniksReturnAddress 0 ; TODO

.orga $0003
.section "Intialisation patch" overwrite
  ld sp,TopOfStack
  jp $0075 ; We skip the pre-init block at $0046
  
  ; Spare space here for us to use

DrRobotniksSelectPageAWithAdjustment:
  add a,:DrRobotniks
  ; Fall through
DrRobotniksSelectPageA:
  ; We save to $dfff as well as paging
  ld (Slot2PageNumber),a
  ld (Slot2Control),a
  ret
.ends

.orga $0066
.section "Pause killer" overwrite
  retn
.ends

.orga $0088
.section "Initialisation patch" overwrite
;    xor    a               ; 000088 AF 
;    ld     ($fffc),a       ; 000089 32 FC FF 
;    ld     ($fffd),a       ; 00008C 32 FD FF 
;    ld     a,$01           ; 00008F 3E 01 
;    ld     ($fffe),a       ; 000091 32 FE FF 
;    ld     a,$02           ; 000094 3E 02 
;    ld     ($ffff),a       ; 000096 32 FF FF 
;    ld     hl,$c001        ; 000099 21 01 C0 
;    ld     de,$c002        ; 00009C 11 02 C0 
;    ld     bc,$1426        ; 00009F 01 26 14 
;    ld     (hl),$00        ; 0000A2 36 00 
;    ldir                   ; 0000A4 ED B0 
;    ld     hl,$d453        ; 0000A6 21 53 D4 
;    ld     de,$d454        ; 0000A9 11 54 D4 
;    ld     bc,$0bab        ; 0000AC 01 AB 0B 
;    ld     (hl),$00        ; 0000AF 36 00 
;    ldir                   ; 0000B1 ED B0 
.rept 12
  nop
.endr
  ld a,:DrRobotniks+2    ; patch
  ld ($8000),a           ; patch
  ld     hl,$c001        ; 000099 21 01 C0 
  ld     de,$c002        ; 00009C 11 02 C0 
  ld     bc,$1426        ; 00009F 01 26 14 
  ld     (hl),$00        ; 0000A2 36 00 
  ldir                   ; 0000A4 ED B0 
  ld     hl,$d453        ; 0000A6 21 53 D4 
  ld     de,$d454        ; 0000A9 11 54 D4 
  ld     bc,TopOfStack-$d454 ; patch
  ld     (hl),$00        ; 0000AF 36 00 
  ldir                   ; 0000B1 ED B0 
.ends

.orga $32e5
.section "VBlank paging restore patch" overwrite
;    ld     a,$02           ; 0032E5 3E 02 
;    ld     ($ffff),a       ; 0032E7 32 FF FF 
;    ld     a,($d484)       ; 0032EA 3A 84 D4 
;    ld     ($ffff),a       ; 0032ED 32 FF FF 
.rept 5
  nop ; no point, maybe we can use this for a hook?
.endr
  ld a,($d484)
  call DrRobotniksSelectPageA
.ends

; Paging patches...
.macro PagingA
;    ld     a,$02           ; 001652 3E 02 <-- Patch here
;    ld     ($ffff),a       ; 001654 32 FF FF
.orga \1
.section "PagingA\@" overwrite
  ld a,\2+:DrRobotniks
  call DrRobotniksSelectPageA
.ends
.endm

.macro PagingB
;    ld     ($ffff),a       ; 00277D 32 FF FF <-- patch here, value is unadjusted
.orga \1
.section "PagingB\@" overwrite
  call DrRobotniksSelectPageAWithAdjustment
.ends
.endm
 
; Generated...
; PagingA $0055, $2
 PagingA $0094, $2
 PagingA $00d2, $2
 PagingA $2792, $2
 PagingA $27c0, $2
 PagingA $284b, $2
 PagingA $2884, $2
 PagingA $28b9, $2
 PagingA $2afd, $2
 PagingA $2dab, $2
 PagingA $2de4, $2
 PagingA $2e19, $2
 PagingA $30f4, $2
 PagingA $3166, $2
 PagingA $3278, $2
 PagingA $32c5, $f
 PagingA $32e5, $2
 PagingA $3555, $8
 PagingA $358f, $2
 PagingA $365e, $e
 PagingA $36a0, $2
 
 PagingB $277d
 PagingB $27ab
 PagingB $2832
 PagingB $2857
 PagingB $2a98
 PagingB $2d87
 PagingB $2dbd
 PagingB $2df6


.bank DrRobotniksStartBank+1, slot 1
.orga $4000
.incbin "Dr. Robotnik's Mean Bean Machine.sms" skip $04000 read $4000

.orga $4ecc
.section "Intro skip" overwrite
;    ld     a,($ca18)       ; 004ECC 3A 18 CA ; Check control inputs
;    and    $30             ; 004ECF E6 30 
;    jr     nz,$4ede        ; 004ED1 20 0B 
  jp $4ede
.ends

; Generated...
 PagingA $4a04, $2
 PagingA $4cd5, $2
 PagingA $4ddf, $7
 PagingA $4df9, $2
 PagingA $5072, $b
 PagingA $50ad, $b
 PagingA $50b5, $2
 PagingA $50f0, $b
 PagingA $50f8, $2
 PagingA $5178, $b
 PagingA $5180, $2
 PagingA $52f2, $b
 PagingA $52fa, $2
 PagingA $5841, $2
 PagingA $5847, $a
 PagingA $5946, $a
 PagingA $594e, $2
 PagingA $5970, $a
 PagingA $5978, $2
 PagingA $5ab4, $a
 PagingA $5ac6, $2
 PagingA $5c4d, $5
 PagingA $5c82, $2
 PagingA $5c9f, $5
 PagingA $5cd2, $2
 PagingA $5ef6, $5
 PagingA $5f5e, $2
 PagingA $5fc5, $5
 PagingA $602c, $2

 PagingB $49e4
 PagingB $4c9f
 
/*
.orga $7f00
.section "Paging helper" overwrite
DrRobotniksPagingHelper:
  ld (Slot2Control),a
  ld (Slot2PageNumber),a
  ret

.ends
*/
.bank DrRobotniksStartBank+2
.org 0
.incbin "Dr. Robotnik's Mean Bean Machine.sms" skip $08000 read $4000

.bank DrRobotniksStartBank+3
.org 0
.incbin "Dr. Robotnik's Mean Bean Machine.sms" skip $0c000 read $4000

.bank DrRobotniksStartBank+4
.orga 0
.incbin "Dr. Robotnik's Mean Bean Machine.sms" skip $10000 read $4000

.bank DrRobotniksStartBank+5
.org 0
.incbin "Dr. Robotnik's Mean Bean Machine.sms" skip $14000 read $4000

.bank DrRobotniksStartBank+6
.org 0
.incbin "Dr. Robotnik's Mean Bean Machine.sms" skip $18000 read $4000

.bank DrRobotniksStartBank+7
.org 0
.incbin "Dr. Robotnik's Mean Bean Machine.sms" skip $1c000 read $4000

.bank DrRobotniksStartBank+8
.org 0
.incbin "Dr. Robotnik's Mean Bean Machine.sms" skip $20000 read $4000

.bank DrRobotniksStartBank+9
.org 0
.incbin "Dr. Robotnik's Mean Bean Machine.sms" skip $24000 read $4000

.bank DrRobotniksStartBank+10
.orga 0
.incbin "Dr. Robotnik's Mean Bean Machine.sms" skip $28000 read $4000

.bank DrRobotniksStartBank+11
.org 0
.incbin "Dr. Robotnik's Mean Bean Machine.sms" skip $2c000 read $4000

.bank DrRobotniksStartBank+12
.org 0
.incbin "Dr. Robotnik's Mean Bean Machine.sms" skip $30000 read $4000

.bank DrRobotniksStartBank+13
.org 0
.incbin "Dr. Robotnik's Mean Bean Machine.sms" skip $34000 read $4000

.bank DrRobotniksStartBank+14
.org 0
.incbin "Dr. Robotnik's Mean Bean Machine.sms" skip $38000 read $4000

.bank DrRobotniksStartBank+15
.org 0
.incbin "Dr. Robotnik's Mean Bean Machine.sms" skip $3c000 read $4000
