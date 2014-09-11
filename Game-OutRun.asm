.include "Common.inc"

.bank 0 slot 0

; Memory addresses
.define GameState $c102
.define Score $de21 ; Stored as BCD, divided by 10
; e.g. 12 34 56 78 = 123456780, but high digit is not shown in-game
; e.g.    fast run = 1597620
; e.g.    slow run =   26052 (time up)
; e.g.      my run = 1437980

.section "OutRun helpers" free

OutRunStart:
/*  LoadScreen Tilemap_OutRun
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
  ld hl,OutRunFrameHandler
  ld (JumpOutAddress), hl

  ; Jump to the game
  ld a,:OutRun
  jp LoadGame
  
OutRunFrameHandler:
  ; All registers are safe to use
  
  call CheckForReset

  ; Check the game state pointer
  ; $0b = in-game
  ld a,(GameState)
  cp $0b
  jr nz,_ReturnToGame

  ; Decrement timer
  ld hl,(FrameCounter)
  dec hl
  ld (FrameCounter),hl
  ld a,h
  or l
  jp z,TimeUp
  
  ; That's it!
_ReturnToGame:
  ld a,:OutRun
  ld hl,OutRunReturnAddress
  push hl
  jp JumpBack
  
OutRunEnd:
  call InitialiseSystem
  ld sp,TopOfStack
  jp DrRobotniksStart

OutRunGetScore:
/*
  TODO
  ld hl,Score
  ld de,OutRunScore
  ld bc,4
  ldir
*/
  ret
.ends

.bank OutRunStartBank slot 0
.orga 0
OutRun:
.incbin "Out Run.sms" skip $00000 read $4000
; The top part of RAM is used for the high score table, so we can leave the stack alone, so long as we protect it...

.orga $0084
.section "Game start" overwrite
; We have 14 bytes of pager initialisation here which we don't want any more.
; We can't just make the entry go a bit further on because it's very close to
; the maximum range for a relative jump. So we insert a jump to skip it.
  jr GameStart
; We could use the remaining 12 bytes for something, but our paging helpers don't fit.
.ends

.orga $0092
.section "Game start (real)" overwrite
GameStart:
; ld     hl,$de00        ; 000092 21 00 DE 
; ld     de,$de01        ; 000095 11 01 DE 
; ld     bc,$01fb        ; 000098 01 FB 01 
; ld     (hl),$00        ; 00009B 36 00 
; ldir                   ; 00009D ED B0 
  ld hl,$de00
  ld de,$de01
  ld bc,TopOfStack - $de01
  ld (hl),0
  ldir
.ends

 ; Remove high score table initialisation
 nopOut $00b0 11

.orga $0176
.section "VBlank paging backup" overwrite
; ld     a,($ffff)       ; 000176 3A FF FF 
  ld a,(Slot2PageNumber)
.ends

.orga $17a
.section "Reset button check removal, plus VBlank hook" overwrite
; in     a,($dd)         ; 00017A DB DD 
; and    $10             ; 00017C E6 10 
; ld     hl,$c10b        ; 00017E 21 0B C1 
; ld     c,(hl)          ; 000181 4E 
; ld     (hl),a          ; 000182 77 
; xor    c               ; 000183 A9 
; and    c               ; 000184 A1 
; jp     nz,$081a        ; 000185 C2 1A 08 
  jp JumpOut
.define OutRunReturnAddress $0188
.ends

 nopOut $023c 1 ; Title screen timeout - the same counter is used for other phases of the attract mode, but the decrement code is specific to each phase
; dec    hl              ; 00023C 2B 

 nopOut $3ef9 1 ; Timer decrement (frame part)
; dec    (hl)            ; 003EF9 35 


.orga $01ba
.section "VBlank paging restore" overwrite
; ld     ($ffff),a       ; 0001BA 32 FF FF 
  call OutRunSelectPageA
.ends

; Paging patches...
.macro PagingA
; ld     hl,$ffff        ; 00099B 21 FF FF
; ld     (hl),$0a        ; 00099E 36 0A 
.orga \1
.section "PagingA\@" overwrite
  ; Can't use a!
  ld h,\2+:OutRun
  call OutRunSelectPageH
.ends
.endm

.macro PagingB
; ld     ($ffff),a       ; 000A5A 32 FF FF <-- patch here, value is unadjusted
.orga \1
.section "PagingB\@" overwrite
  call OutRunSelectPageAWithAdjustment
.ends
.endm

; Generated...
 PagingA $099b,$0a
 PagingA $1195,$0a
 PagingA $1282,$0a
 PagingA $12d7,$07
 PagingA $1431,$0a
 PagingA $1493,$0a
 PagingA $157d,$02
 PagingA $158e,$0b
 PagingA $159c,$08
 PagingA $15bc,$09
 PagingA $188d,$05
 PagingA $1940,$04
 PagingA $1951,$0f
 PagingA $1b92,$0d
 PagingA $1e54,$09
 PagingA $1e62,$0f
 PagingA $1e8a,$0d
 PagingA $1f52,$0f
 PagingA $1fa0,$0f
 PagingA $1ff8,$0f
 PagingA $2270,$0f
 PagingA $23a1,$0f
 PagingA $2aa2,$03
 PagingA $2f6b,$09
 PagingA $3909,$02
 PagingA $3a2c,$03
 PagingA $3aca,$03
 PagingA $3cca,$02
 PagingA $3ce3,$02
 PagingA $3d34,$02
 PagingA $3d6a,$02
 PagingA $3ebe,$02
 
 PagingB $0a5a
 PagingB $12c1

.bank OutRunStartBank+1, slot 1
.orga $4000
.incbin "Out Run.sms" skip $04000 read $4000

; Generated...
 PagingA $4039,$09
 PagingA $416d,$02
 PagingA $419d,$0e
 PagingA $4240,$09
 PagingA $4287,$08
 PagingA $42dd,$0c
 PagingA $4323,$0c
 PagingA $4393,$0c
 PagingA $4574,$06
 PagingA $47c7,$03
 PagingA $4bd4,$04 ; missed by disassembly due to alignment
 PagingA $4e0e,$0b
 PagingA $4e53,$0b
 PagingA $4e98,$0b
 PagingA $4efd,$0b
 PagingA $4f3f,$03
 PagingA $4fb6,$05
 PagingA $4fe6,$05
 PagingA $5104,$05
 
.orga $5153
.section "Stage end hook" overwrite
; call   $519b           ; 005153 CD 9B 51 
; call   $5203           ; 005156 CD 03 52
; ...
  ld hl,OutRunEnd
  ld (JumpOutAddress), hl
  jp JumpOut
.ends

.orga $7ff0-10-7-2
; Some FFs at the end of the bank
.section "Paging helpers" overwrite
; Here's our helpers
OutRunSelectPageAWithAdjustment: ; 2 bytes
  add a,:OutRun
  ; Fall through
OutRunSelectPageA: ; 7 bytes
  ld (Slot2Control),a
  ld (Slot2PageNumber),a
  ret
OutRunSelectPageH: ; 10 bytes
  push af
    ld a,h
    ld (Slot2Control),a
    ld (Slot2PageNumber),a
  pop af
  ret
.ends

.bank OutRunStartBank+2
.org 0
.incbin "Out Run.sms" skip $08000 read $4000

.bank OutRunStartBank+3
.org 0
.incbin "Out Run.sms" skip $0c000 read $4000

.bank OutRunStartBank+4
.orga 0
.incbin "Out Run.sms" skip $10000 read $4000

.bank OutRunStartBank+5
.org 0
.incbin "Out Run.sms" skip $14000 read $4000

.bank OutRunStartBank+6
.org 0
.incbin "Out Run.sms" skip $18000 read $4000

.bank OutRunStartBank+7
.org 0
.incbin "Out Run.sms" skip $1c000 read $4000

.bank OutRunStartBank+8
.org 0
.incbin "Out Run.sms" skip $20000 read $4000

.bank OutRunStartBank+9
.org 0
.incbin "Out Run.sms" skip $24000 read $4000

.bank OutRunStartBank+10
.orga 0
.incbin "Out Run.sms" skip $28000 read $4000

.bank OutRunStartBank+11
.org 0
.incbin "Out Run.sms" skip $2c000 read $4000

.bank OutRunStartBank+12
.org 0
.incbin "Out Run.sms" skip $30000 read $4000

.bank OutRunStartBank+13
.org 0
.incbin "Out Run.sms" skip $34000 read $4000

.bank OutRunStartBank+14
.org 0
.incbin "Out Run.sms" skip $38000 read $4000

.bank OutRunStartBank+15
.org 0
.incbin "Out Run.sms" skip $3c000 read $4000
