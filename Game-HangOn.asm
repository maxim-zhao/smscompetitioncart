.include "Common.inc"

.bank 0 slot 0

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

.bank HangOnStartBank+0
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

.bank HangOnStartBank+1
.org 0
.incbin "Hang On.sms" skip $4000 read $4000
