\ ******************************************************************
\ *	256b demo framework
\ ******************************************************************

_DEBUG = TRUE			; don't limit to 256b
_STABLE_RASTER = FALSE
_DEMO_SIZE = 256

\ ******************************************************************
\ *	OS defines
\ ******************************************************************

osfile = &FFDD
oswrch = &FFEE
osasci = &FFE3
osbyte = &FFF4
osword = &FFF1
osfind = &FFCE
osgbpb = &FFD1
osargs = &FFDA

\\ Palette values for ULA
PAL_black	= (0 EOR 7)
PAL_blue	= (4 EOR 7)
PAL_red		= (1 EOR 7)
PAL_magenta = (5 EOR 7)
PAL_green	= (2 EOR 7)
PAL_cyan	= (6 EOR 7)
PAL_yellow	= (3 EOR 7)
PAL_white	= (7 EOR 7)

ULA_Mode2	= &F4

PIXEL_LEFT_0 = &00
PIXEL_LEFT_1 = &02
PIXEL_LEFT_2 = &08
PIXEL_LEFT_3 = &0A
PIXEL_LEFT_4 = &20
PIXEL_LEFT_5 = &22
PIXEL_LEFT_6 = &28
PIXEL_LEFT_7 = &2A
PIXEL_LEFT_8 = &80
PIXEL_LEFT_9 = &82
PIXEL_LEFT_A = &88
PIXEL_LEFT_B = &8A
PIXEL_LEFT_C = &A0
PIXEL_LEFT_D = &A2
PIXEL_LEFT_E = &A8
PIXEL_LEFT_F = &AA

PIXEL_RIGHT_0 = &00
PIXEL_RIGHT_1 = &01
PIXEL_RIGHT_2 = &04
PIXEL_RIGHT_3 = &05
PIXEL_RIGHT_4 = &10
PIXEL_RIGHT_5 = &11
PIXEL_RIGHT_6 = &14
PIXEL_RIGHT_7 = &15
PIXEL_RIGHT_8 = &40
PIXEL_RIGHT_9 = &41
PIXEL_RIGHT_A = &44
PIXEL_RIGHT_B = &45
PIXEL_RIGHT_C = &50
PIXEL_RIGHT_D = &51
PIXEL_RIGHT_E = &54
PIXEL_RIGHT_F = &55

\ ******************************************************************
\ *	SYSTEM defines
\ ******************************************************************

MODE1_COL0=&00
MODE1_COL1=&20
MODE1_COL2=&80
MODE1_COL3=&A0

\ ******************************************************************
\ *	MACROS
\ ******************************************************************

MACRO PAGE_ALIGN
    PRINT "ALIGN LOST ", ~LO(((P% AND &FF) EOR &FF)+1), " BYTES"
    ALIGN &100
ENDMACRO

MACRO WAIT_NOPS n
PRINT "WAIT",n," CYCLES AS NOPS"

IF n < 0
	ERROR "Can't wait negative cycles!"
ELIF n=0
	; do nothing
ELIF n=1
	EQUB $33
	PRINT "1 cycle NOP is Master only and not emulated by b-em."
ELIF (n AND 1) = 0
	FOR i,1,n/2,1
	NOP
	NEXT
ELSE
	BIT 0
	IF n>3
		FOR i,1,(n-3)/2,1
		NOP
		NEXT
	ENDIF
ENDIF
ENDMACRO

MACRO WAIT_CYCLES n

PRINT "WAIT",n," CYCLES"

IF n >= 12
	FOR i,1,n/12,1
	JSR return
	NEXT
	WAIT_NOPS n MOD 12
ELSE
	WAIT_NOPS n
ENDIF

ENDMACRO

\ ******************************************************************
\ *	GLOBAL constants
\ ******************************************************************

; Default screen address
screen_addr = &3000
SCREEN_SIZE_BYTES = &8000 - screen_addr
row_bytes = 640

; Exact time for a 50Hz frame less latch load time
FramePeriod = 312*64-2

; Calculate here the timer value to interrupt at the desired line
TimerValue = 32*64 - 2*64 - 2 - 25 + 10

\\ 40 lines for vblank
\\ 32 lines for vsync (vertical position = 35 / 39)
\\ interupt arrives 2 lines after vsync pulse
\\ 2 us for latch
\\ XX us to fire the timer before the start of the scanline so first colour set on column -1
\\ YY us for code that executes after timer interupt fires

\ ******************************************************************
\ *	ZERO PAGE
\ ******************************************************************

ORG &70
GUARD &9F
.writeptr	skip 2
.scanlines	skip 1
.index 		skip 1
.index2 	skip 1

\ ******************************************************************
\ *	CODE START
\ ******************************************************************

ORG &1900
IF _DEBUG
GUARD screen_addr			; ensure code size doesn't hit start of screen memory
ELSE
GUARD &1900+_DEMO_SIZE
ENDIF

.start

\ ******************************************************************
\ *	Code entry
\ ******************************************************************

.main_start

.main
{
	\\ Set MODE
	lda #22:jsr oswrch
	lda #2:jsr oswrch
	\\ Turn off interlace & cursor
	lda #8:sta &fe00
	lda #&c0:sta &fe01

	\\ Generate 256b sine table.
	\\ Taken from: http://www.sizecoding.org/wiki/6502_based_CPUs#Atari_8bit_family
	{
		ldx #0
		ldy #$3f
	.make_sine
	.value_lo
		lda #0
		clc
	.delta_lo
		adc #0
		sta value_lo+1
	.value_hi
		lda #0
	.delta_hi
		adc #0
		sta value_hi+1

		sta sinewave+$c0,x
		sta sinewave+$80,y
		eor #$7f
		sta sinewave+$40,x
		sta sinewave+$00,y

		lda delta_lo+1
		adc #8
		sta delta_lo+1
		bcc nothing
		inc delta_hi+1
	.nothing
		inx
		dey
		bpl make_sine
	}
	\\ C=1

	\\ Init vars.
	\\ Set up screen.

	\\ Disable interrupts
	sei 	; not needed from !BOOT?

	\\ Wait for vsync
	{
		lda #2
		.vsync1
		bit &FE4D
		beq vsync1
	}

	; Roughly synced to VSync
    IF _STABLE_RASTER
    ; Now fine tune by waiting just less than one frame
    ; and check if VSync has fired. Repeat until it hasn't.
    ; One frame = 312*128 = 39936 cycles
	{
		.syncloop
		STA &FE4D       ; 6
		LDX #209        ; 2
		.outerloop
		LDY #37         ; 2
		.innerloop
		DEY             ; 2
		BNE innerloop   ; 3/2 (innerloop = 5*37+2-1 = 186)
		DEX             ; 2
		BNE outerloop   ; 3/2 (outerloop = (186+2+3)*209+2-1 = 39920)
		BIT &FE4D       ; 6
		BNE syncloop    ; 3 (total = 39920+6+6+3 = 39935, one cycle less than a frame!)
		IF HI(syncloop) <> HI(P%)
		ERROR "This loop must execute within the same page"
		ENDIF
	}
    ; We are synced precisely with VSync!
	ENDIF

	\\ Set up Timer1 to start at the first scanline
    LDA #LO(TimerValue):STA &FE44		; 8c
    LDA #HI(TimerValue):STA &FE45		; 8c

  	; Latch T1 to interupt exactly every 50Hz frame
	LDA #LO(FramePeriod):STA &FE46		; 8c
	LDA #HI(FramePeriod):STA &FE47		; 8c

	\ ******************************************************************
	\ *	MAIN LOOP
	\ ******************************************************************

	.frame_loop
	\\ Do any processing in vblank here!
	{
		ldx #0
	;	clc				; normally C=0 except first time.
		.clear_loop
		lda #0
		sta screen_addr, X
		sta screen_addr + &100, X
		sta screen_addr + &200, X
		txa:adc #8:tax
		bne clear_loop
	}
	\\ X=0

	\\ Synchronise with start of screen display.
	{
		LDA #&40
		.waitTimer1
		BIT &FE4D
		BEQ waitTimer1

		\\ Reading the T1 low order counter also resets the T1 interrupt flag in IFR
		LDA &FE44

		IF _STABLE_RASTER
		\\ New stable raster NOP slide thanks to VectorEyes 8)
		\\ Observed values $FA (early) - $F7 (late) so map these from 7 - 0
		\\ then branch into NOPs to even this out.
		AND #15
		SEC
		SBC #7
		EOR #7
		STA branch+1
		.branch
		BNE branch
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		.stable
		ENDIF
		\\ Now synced to scanline 0, character 0.
	}
	.here

	\\ R9=0, R4=0, R7=&ff, R6=1
	lda #9:sta &fe00
	stx &fe01			; X=0

	lda #4:sta &fe00
	stx &fe01 			; X=0

	ldy #6:sty &fe00	; Y=6
	lda #1:sta &fe01

	iny:sty &fe00		; Y=7
	dex:stx &fe01		; X=255
	dex:stx scanlines 	; X=254

	\\ Update index per frame.
	dec index:ldx index

	\\ Effect here!
	.scanline_loop
	{
		\\ Calculate X position.
		lda #0:sta writeptr+1				; 5c
		lda sinewave, X						; 4c
		lsr a:lsr a							; 4c

		ldy index2							; 3c
		adc sinewave, Y						; 4c
		lsr a								; 2c

		\\ Multiply by 8 and add screen address.
		asl a:rol writeptr+1				; 7c
		asl a:rol writeptr+1				; 7c
		asl a:rol writeptr+1				; 7c
		sta writeptr						; 3c
		lda writeptr+1						; 3c
		adc #HI(&3000)						; 2c
		sta writeptr+1						; 3c
		; 54c

		\\ Plot the bar.
		LDA # PIXEL_LEFT_7 OR PIXEL_RIGHT_3
		LDY #0:STA (writeptr), Y			; 10c
		LDA # PIXEL_LEFT_6 OR PIXEL_RIGHT_2
		LDY #8:STA (writeptr), Y			; 10c
		LDA # PIXEL_LEFT_5 OR PIXEL_RIGHT_1
		LDY #16:STA (writeptr), Y			; 10c
		LDA # PIXEL_LEFT_4
		LDY #24:STA (writeptr), Y			; 10c
		; 40c

		\\ Update indices per line.
		inx									; 2c
		inc index2							; 5c

		WAIT_CYCLES 17
		LDA #26								; 2c

		dec scanlines						; 5c
		bne scanline_loop					; 3c
		; 5c
	}

	\\ R7=3 - vsync is at row 35 = 280 scanlines
	\\ R7 was last one to be programmed.
	STA &FE01

	\\ R4=6 - CRTC cycle is 32 + 7 more rows = 312 scanlines
	LDA #4: STA &FE00
	LDA #57: STA &FE01

	\\ Use branch to save a byte.
	jmp frame_loop
}
.return
rts

.main_end

\ ******************************************************************
\ *	SYSTEM DATA
\ ******************************************************************

.data_start

.data_end

\ ******************************************************************
\ *	End address to be saved
\ ******************************************************************

.end

\ ******************************************************************
\ *	Save the code
\ ******************************************************************

SAVE "256beeb", start, end, main

\ ******************************************************************
\ *	Space reserved for runtime buffers not preinitialised
\ ******************************************************************

ALIGN &100
.bss_start

.sinewave
skip 256

.bss_end

\ ******************************************************************
\ *	Memory Info
\ ******************************************************************

PRINT "------"
PRINT "256b demo"
PRINT "------"
PRINT "CODE size =", ~main_end-main_start
PRINT "DATA size =",~data_end-data_start
PRINT "BSS size =",~bss_end-bss_start
PRINT "------"
PRINT "HIGH WATERMARK =", ~P%
IF (end-start) > _DEMO_SIZE
PRINT "OVER =", ~(end-start)-_DEMO_SIZE
ELSE
PRINT "FREE =", ~_DEMO_SIZE-(end-start)
ENDIF
PRINT "------"

\ ******************************************************************
\ *	Any other files for the disc
\ ******************************************************************
