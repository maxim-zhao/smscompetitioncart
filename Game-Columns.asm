.include "Common.inc"

.bank 0 slot 0

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

.bank ColumnsStartBank slot 0
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


.bank ColumnsStartBank+1, slot 1
.orga $4000
.incbin "Columns.sms" skip $04000 read $4000

.orga $7f00
.section "Paging helper" overwrite
ColumnsPagingHelper:
  ld (Slot2Control),a
  ld (Slot2PageNumber),a
  ret

.ends

.bank ColumnsStartBank+2
.org 0
.incbin "Columns.sms" skip $08000 read $4000

.bank ColumnsStartBank+3
.org 0
.incbin "Columns.sms" skip $0c000 read $4000

.bank ColumnsStartBank+4
.orga 0
.incbin "Columns.sms" skip $10000 read $4000

.bank ColumnsStartBank+5
.org 0
.incbin "Columns.sms" skip $14000 read $4000

.bank ColumnsStartBank+6
.org 0
.incbin "Columns.sms" skip $18000 read $4000

.bank ColumnsStartBank+7
.org 0
.incbin "Columns.sms" skip $1c000 read $4000
