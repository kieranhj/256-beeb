\ ******************************************************************
\ *	256b demo framework
\ ******************************************************************

_DEBUG = TRUE			; don't limit to 256b
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
TimerValue = 32*64 - 2*64 - 2 - 25 + 40

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
.u 	skip 1
.v	skip 1

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

	\\ Do any initialisation here!

	\\ Set up screen.
	{
		.outer
		ldx #7
		.loop1
		ldy #7
		.loop2
		lda pixel_pairs, X
		.write_to
		sta screen_addr
		inc write_to+1
		bne ok
		inc write_to+2
		bmi done
		.ok
		dey
		bpl loop2
		dex
		bpl loop1
		bmi outer
		.done
	}

	\\ Init vars.
	lda #0
	sta u
	sta v

	\\ Disable interrupts
	sei

	\\ Wait for vsync
	{
		lda #2
		.vsync1
		bit &FE4D
		beq vsync1
	}

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
	inc u
	inc v

	\\ Set vertical colours
	{
		lda #0
		sta ok+1
		ldx u
		.loop
		txa
		and #8
		bne ok
		lda #15
		.ok
		ora #0
		sta &fe21
		inx
		clc
		lda ok+1
		adc #&10
		sta ok+1
		bcc loop
	}

	\\ Synchronise with start of screen display.
	{
		LDA #&40
		.waitTimer1
		BIT &FE4D
		BEQ waitTimer1

		\\ Reading the T1 low order counter also resets the T1 interrupt flag in IFR
		LDA &FE44

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
	}
	\\ Now synced to scanline 0, character 0.

	\\ Effect here!
	ldy #0								; 2c
	ldx v

	.scanline_loop
	{
		txa								; 2c
		and #16							; 2c
		lsr a:lsr a:lsr a:lsr a			; 8c
		ora #&f4						; 2c
		sta &fe20						; 4c
		inx								; 2c
		WAIT_CYCLES 123-20

		iny								; 2c
		bne scanline_loop				; 3c
		; loop total = 128c
	}

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

\\ pixel pairs
.pixel_pairs
equb %11111101		; 14, 15
equb %11110001		; 12, 13
equb %11001101		; 10, 11
equb %11000001		; 8, 9
equb %00111101		; 6, 7
equb %00110001		; 4, 5
equb %00001101		; 2, 3
equb %00000001		; 0, 1

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

.bss_start

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
PRINT "FREE =", ~start+_DEMO_SIZE-P%
PRINT "------"

\ ******************************************************************
\ *	Any other files for the disc
\ ******************************************************************
