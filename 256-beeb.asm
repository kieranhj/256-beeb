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

MACRO WAIT_CYCLES n

PRINT "WAIT",n," CYCLES"

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
TimerValue = 32*64 - 2*64 - 2 - 25

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

\\ Add ZP vars here

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

	\\ Disable interrupts
	sei

	\\ Sync to vsync
	{
		lda #2
		.vsync1
		bit &FE4D
		beq vsync1 \ wait for vsync
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
	ldx #0								; 2c

	.scanline_loop
	{
		lda #PAL_red:sta &fe21			; 6c
		lda #PAL_green:sta &fe21
		lda #PAL_yellow:sta &fe21
		lda #PAL_blue:sta &fe21
		lda #PAL_magenta:sta &fe21
		lda #PAL_cyan:sta &fe21
		lda #PAL_white:sta &fe21
		; 7*6c = 42c

		lda #PAL_red:sta &fe21			; 6c
		lda #PAL_green:sta &fe21
		lda #PAL_yellow:sta &fe21
		lda #PAL_blue:sta &fe21
		lda #PAL_magenta:sta &fe21
		lda #PAL_cyan:sta &fe21
		lda #PAL_white:sta &fe21
		; 7*6c = 42c

		lda #PAL_red:sta &fe21			; 6c
		lda #PAL_green:sta &fe21
		lda #PAL_yellow:sta &fe21
		lda #PAL_blue:sta &fe21
		lda #PAL_magenta:sta &fe21
		lda #PAL_cyan:sta &fe21
		; 6*6c = 36c

		BIT 0							; 3c
		dex								; 2c
		bne scanline_loop				; 3c
		; loop total = 128c
	}

	\\ Do any processing in vblank here!
	lda #PAL_black:sta &fe21

	JMP frame_loop
}

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
