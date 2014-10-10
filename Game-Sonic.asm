.include "Common.inc"

.ifdef SonicStartBank

.bank 0 slot 0

.section "Sonic helpers" free
SonicStart:
  LoadScreen Tilemap_Sonic
  call ScreenOn
  call FadeInFullPalette
  call WaitForButton
  call FadeOutFullPalette
  call ScreenOff

  ; Record the game number
  ld a, 1
  ld (GameNumber),a
  ; Patch in where to go...
  SetFrameHandler SonicFrameHandler

  ; Jump to the game
  ld a,:Sonic
  jp LoadGame
  
; Useful RAM locations
.define SonicRingCounter $d2aa ; BCD
.define SonicLivesCounter $d246 ; Hex
.define SonicTimeMinutes $d2ce ; 0-9
.define SonicTimeSeconds $d2cf ; BCD
.define SonicLevelNumber $d23e ; Hex
.define SonicStateByte $d206 ; ?

SonicFrameHandler:
  ; All registers are free to use, we're in the VBlank handler 
  call CheckForReset
  
  ; Check the game state
  ld a,(SonicStateByte)
  and $08
  jr z,+
  
  ; We're in the game (?)
  ; Decrement timer
  ld hl,(FrameCounter)
  dec hl
  ld (FrameCounter),hl
  ld a,h
  or l
  jp z,TimeUp
  
  ; We can't detect 100 rings in here... we catch it in the game

+:; Return to game
  ld a,:Sonic
  ld hl,SonicReturnAddress
  push hl
  jp JumpBack
    
SonicEnd:
  ld a,1
  ld (SonicScore),a
  call InitialiseSystem
  ld sp,TopOfStack
  jp DrRobotniksStart
/*  
SonicGetScore:
  ld a,(SonicRingCounter)
  ld (SonicRings),a
  ret
*/
.ends


.bank SonicStartBank slot 0
.org 0
Sonic:
.incbin "Sonic The Hedgehog.sms" skip $00000 read $4000

; Game hacks
; Disable ring counter reset on level start
 nopOut $2156 3
; Disable losing rings when hurt
 nopOut $3645 3
; Disable ring countdown at level end, plus the rest of the score adding
 nopOut $1692 9
 nopOut $16bf 3

.orga $0066
.section "Sonic pause remover" overwrite
  retn
.ends

.orga $1328
.section "Sonic: disable title screen timeout" overwrite
; Was:
;    and    a               ; 001328 A7 
;    jr     z,$1350         ; 001329 28 25 ; Zero counter means end
  jp SonicAttractModeDisabler
.ends

.org $003b
.section "Sonic paging helpers 1" overwrite ; We are overwriting some secret text, we have 43 bytes of space
SonicPageHLFixup: ; 9 bytes
  ; can use a
  ld a,h          ; 1
  ld (Slot2Control),a ; 3
  ld a,l          ; 1
-:ld (Slot1Control),a ; 3
  ret             ; 1

Sonic_fn0405Fix:  ; 4 bytes
  add a,:Sonic    ; 2
  jr -            ; 2 - happens to do what I need

SonicPageDEFixup: ; 7 bytes
  ; seems safe to use a
  ld a,e          ; 1
  ld (Slot2Control),a ; 3
  ld a,d          ; 1
  jr -            ; 2 - happens to do what I need
  
SonicAttractModeDisabler: ; 13 bytes
  ; check a
  or a            ; 1
  jp nz,$132b     ; 3 - back to where we hooked from
  ; set behaviour back to start
  ld hl,$1372     ; 3
  ld ($d210), hl  ; 3
  jp $131f        ; 3 - back to the behaviour loader
  
SonicLevelChooser: ; 6 bytes
  ;  0- 2 = Green Hill
  ;  3- 5 = Bridge
  ;  6- 8 = Jungle
  ;  9-11 = Labyrinth
  ; 12-14 = Scrap Brain
  ; 15-17 = Sky Base
  ;    18 = Ending
  ; It's hard to get to the special stages this way, the game protects against it..?
  ld a,0          ; 2
  ld ($d23e),a    ; 3
  ret             ; 1
  
  ; 4 bytes left
.ends

.org $00db
.section "Sonic VBlank hook" overwrite
;    in     a,($dd)         ; 0000DB DB DD 
;    and    $10             ; 0000DD E6 10 
;    jp     z,$0000         ; 0000DF CA 00 00
  jp JumpOut
.define SonicReturnAddress $00e2
.ends

.org $00e2
.section "Sonic VBlank paging fixup" overwrite
;    pop    hl              ; 0000E2 E1 
;    ld     ($fffe),hl      ; 0000E3 22 FE FF 
;    ld     ($d235),hl      ; 0000E6 22 35 D2 
  pop hl
  call SonicPageHLFixup
  ld ($d235),hl
.ends

; disable initialisation of other paging regs
 nopOut $28b 10
 
; Fix memory blanking, stack top
 patchWord $2a5+1, TopOfStack-$c001
 
.org $0426
.section "fn0405 page number fix up" overwrite
; was:
;    ld     ($fffe),a       ; 000426 32 FE FF    ; Page into that slot
; ...
  call Sonic_fn0405Fix
  ld ($d235),a       ; as original
  inc a              ; as original
  ld ($8000),a       ; patched
.ends

.org $04ef
.section "Page from de fixup" overwrite
;    ld     ($fffe),de      ; 0004EF ED 53 FE FF 
  call SonicPageDEFixup
.ends

.org $1c5d
.section "Level select" overwrite
;    xor    a               ; 001C5D AF 
;    ld     ($d23e),a       ; 001C5E 32 3E D2 ; Level number
  call SonicLevelChooser
  xor a
.ends

.org $39C1
.section "100 rings" overwrite
;    cp     $a0             ; 0039BD FE A0     Over 99?
;    jr     c,$39d1         ; 0039BF 38 10 
;    sub    $a0             ; 0039C1 D6 A0     Wrap
;    ld     ($d2aa),a       ; 0039C3 32 AA D2  
;    ld     a,($d246)       ; 0039C6 3A 46 D2  Increment lives
; ...
  ExitTo SonicEnd
.ends

.macro m_Sonic_fffe_a
;    ld     a,$08           ; 00012D 3E 08 
;    ld     ($fffe),a       ; 00012F 32 FE FF 
.orga \1
.section "m_Sonic_fffe_a_\@_" overwrite
.if nargs == 2
  ld a,\2 + :Sonic
.endif
  ld (Slot1Control),a
.ends
.endm

.macro m_Sonic_ffff_a
;    ld     a,$09           ; 000135 3E 09 
;    ld     ($ffff),a       ; 000137 32 FF FF 
.orga \1
.section "m_Sonic_ffff_a_\@_" overwrite
.if nargs == 2
  ld a,\2 + :Sonic
.endif
  ld (Slot2Control),a
.ends
.endm

; Generated...
 m_Sonic_fffe_a $000C3, $03
 m_Sonic_fffe_a $0012D, $08
 m_Sonic_ffff_a $00135, $09
 m_Sonic_fffe_a $00144, $01
 m_Sonic_ffff_a $0014C, $02
 m_Sonic_fffe_a $00174, $01
 m_Sonic_ffff_a $0017C, $02
 m_Sonic_fffe_a $001A5, $01
 m_Sonic_ffff_a $001AD, $02
 m_Sonic_fffe_a $00295, $01
 m_Sonic_ffff_a $0029A, $02
 m_Sonic_fffe_a $002D9, $03
 m_Sonic_fffe_a $002E8
 m_Sonic_fffe_a $002EE, $03
 m_Sonic_fffe_a $002F9
 m_Sonic_fffe_a $00300, $03
 m_Sonic_fffe_a $0030C
 m_Sonic_fffe_a $003BC
 m_Sonic_ffff_a $003C3
 m_Sonic_fffe_a $003FC
 m_Sonic_ffff_a $00400
 m_Sonic_fffe_a $006C3, $04
 m_Sonic_ffff_a $006CB, $05
 m_Sonic_fffe_a $00967, $04
 m_Sonic_ffff_a $0096F, $05
 m_Sonic_fffe_a $00A40, $01
 m_Sonic_ffff_a $00A48, $02
 m_Sonic_fffe_a $00ABC, $01
 m_Sonic_ffff_a $00AC4, $02
 m_Sonic_fffe_a $00B6E, $01
 m_Sonic_ffff_a $00B76, $02
 m_Sonic_fffe_a $00C1E, $05
 m_Sonic_fffe_a $00C4D
 m_Sonic_fffe_a $00CAA, $05
 m_Sonic_fffe_a $00D0C, $05
 m_Sonic_fffe_a $012AC, $05
 m_Sonic_fffe_a $0141C, $05
 m_Sonic_fffe_a $0158B, $05
 m_Sonic_fffe_a $01CED, $05
 m_Sonic_fffe_a $01D55, $0B
 m_Sonic_fffe_a $01D7F, $01
 m_Sonic_ffff_a $01D87, $02
 m_Sonic_fffe_a $01DB5, $0B
 m_Sonic_fffe_a $01E50, $01
 m_Sonic_ffff_a $01E58, $02
 m_Sonic_fffe_a $01EB1, $0B
 m_Sonic_fffe_a $01ECC, $03
 m_Sonic_fffe_a $01F0C, $01
 m_Sonic_ffff_a $01F14, $02
 m_Sonic_fffe_a $020B8, $03
 m_Sonic_fffe_a $0221C, $06
 m_Sonic_ffff_a $02224, $07
 m_Sonic_fffe_a $0222E, $05
 m_Sonic_ffff_a $02236, $06
 m_Sonic_fffe_a $0227D, $01
 m_Sonic_ffff_a $02285, $02
 m_Sonic_fffe_a $022BD, $01
 m_Sonic_ffff_a $022C5, $02
 m_Sonic_fffe_a $022E0, $05
 m_Sonic_fffe_a $025B4, $05
 m_Sonic_fffe_a $025DB, $01
 m_Sonic_fffe_a $02675, $05
 m_Sonic_fffe_a $026C1, $05

.bank SonicStartBank+1, slot 1
.org 0
.incbin "Sonic The Hedgehog.sms" skip $04000 read $4000

 m_Sonic_ffff_a $049E7, $0F
 m_Sonic_ffff_a $04A1E, $02
 
 ; Infinite lives
 nopOut $5464 1
;    ld     hl,$d246        ; 005461 21 46 D2 
;    dec    (hl)            ; 005464 35 
 

.bank SonicStartBank+2
.org 0
.incbin "Sonic The Hedgehog.sms" skip $08000 read $4000

.bank SonicStartBank+3
.org 0
.incbin "Sonic The Hedgehog.sms" skip $0c000 read $4000

.bank SonicStartBank+4
.orga 0
.incbin "Sonic The Hedgehog.sms" skip $10000 read $4000

.bank SonicStartBank+5
.org 0
.incbin "Sonic The Hedgehog.sms" skip $14000 read $4000

.bank SonicStartBank+6
.org 0
.incbin "Sonic The Hedgehog.sms" skip $18000 read $4000

.bank SonicStartBank+7
.org 0
.incbin "Sonic The Hedgehog.sms" skip $1c000 read $4000

.bank SonicStartBank+8
.org 0
.incbin "Sonic The Hedgehog.sms" skip $20000 read $4000

.bank SonicStartBank+9
.org 0
.incbin "Sonic The Hedgehog.sms" skip $24000 read $4000

.bank SonicStartBank+10
.org 0
.incbin "Sonic The Hedgehog.sms" skip $28000 read $4000

.bank SonicStartBank+11
.org 0
.incbin "Sonic The Hedgehog.sms" skip $2c000 read $4000

.bank SonicStartBank+12
.orga 0
.incbin "Sonic The Hedgehog.sms" skip $30000 read $4000

.bank SonicStartBank+13
.org 0
.incbin "Sonic The Hedgehog.sms" skip $34000 read $4000

.bank SonicStartBank+14
.org 0
.incbin "Sonic The Hedgehog.sms" skip $38000 read $4000

.bank SonicStartBank+15
.org 0
.incbin "Sonic The Hedgehog.sms" skip $3c000 read $4000

.endif