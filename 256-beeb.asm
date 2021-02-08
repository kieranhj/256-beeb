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
.writeptr	skip 2

.scanline	skip 1

.kefrens_top_angle skip 1
.kefrens_row_angle skip 1
.kefrens_top_add skip 1
.kefrens_add_index skip 1
.kefrens_add_anim skip 1

.crtc_reg	skip 2
.crtc_val	skip 2

\ ******************************************************************
\ *	CODE START
\ ******************************************************************

ORG &1C05
IF _DEBUG
GUARD screen_addr			; ensure code size doesn't hit start of screen memory
ELSE
GUARD &1C05+_DEMO_SIZE
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

	\\ Init vars.

	\\ Set up screen.

	\\ Disable interrupts
	sei

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
		clc
		.clear_loop
		lda #0
		sta screen_addr, X
		sta screen_addr + &100, X
		sta screen_addr + &200, X
		txa
		adc #8
		tax
		bne clear_loop
	}

	DEC kefrens_top_angle
	LDA kefrens_top_angle
	STA kefrens_row_angle

	CLC
	LDA kefrens_top_add
	ADC kefrens_add_anim
	STA kefrens_top_add

	LDA kefrens_top_add
	STA kefrens_add_index

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

	\\ R9=0, R4=0, R7=&ff, R6=1
	lda #9:sta &fe00
	ldy #0:sty &fe01

	lda #4:sta &fe00
	sty &fe01 ;0

	lda #7:sta &fe00
	ldx #&ff:stx &fe01

	lda #6:sta &fe00
	iny:sta &fe01 ;1

	dex:stx scanline ;254

	\\ Effect here!
	.scanline_loop
	{
		ldy kefrens_row_angle				; 3c
		lda kefrens_speed_table_1, y		; 4c
		tay									; 2c
		lda kefrens_width_table_1, Y		; 4c
		clc									; 2c
		ldy kefrens_add_index				; 3c
		adc kefrens_add_table_1, Y			; 4c
		tay									; 2c

		.left_side_nop_later
		LDA kefrens_addr_table_LO, Y		; 4c
		STA writeptr						; 3c
		LDA kefrens_addr_table_HI, Y		; 4c
		STA writeptr+1						; 3c

		TYA:LSR A							; 4c
		\\ 42c

		BCS right
		;2c
		\\ Left
		LDA # PIXEL_LEFT_7 OR PIXEL_RIGHT_3
		LDY #0:STA (writeptr), Y			; 10c
		LDA # PIXEL_LEFT_6 OR PIXEL_RIGHT_2
		LDY #8:STA (writeptr), Y			; 10c
		LDA # PIXEL_LEFT_5 OR PIXEL_RIGHT_1
		LDY #16:STA (writeptr), Y			; 10c
		LDY #24								; 2c
		LDA (writeptr), Y					; 5c
		AND #&55							; 2c
		ORA #PIXEL_LEFT_4					; 2c
		STA (writeptr), Y					; 6c
		bne continue 						; 3c
		\\ 52c

		.right	;3c
		\\ Left
		LDY #0								; 2c
		LDA (writeptr), Y					; 5c
		AND #&AA							; 2c
		ORA #PIXEL_RIGHT_7					; 2c
		STA (writeptr), Y					; 6c

		LDA # PIXEL_LEFT_3 OR PIXEL_RIGHT_6			; yellow/cyan
		LDY #8:STA (writeptr), Y			; 10c
		LDA # PIXEL_LEFT_2 OR PIXEL_RIGHT_5			; green/magenta
		LDY #16:STA (writeptr), Y			; 10c
		LDA # PIXEL_LEFT_1 OR PIXEL_RIGHT_4			; red/blue
		LDY #24:STA (writeptr), Y			; 10c
		NOP									; 2c
		\\ 52c

		.continue
		INC kefrens_row_angle				; 5c
		INC kefrens_add_index				; 5c
		\\ 10c

		WAIT_CYCLES 16
		\\ 16c

		DEC scanline						; 5c
		BNE scanline_loop					; 3c
		\\ 8c
	}

	\\ R4=6 - CRTC cycle is 32 + 7 more rows = 312 scanlines
	LDA #4: STA &FE00
	LDA #56: STA &FE01			; 312 - 256 = 56 scanlines

	\\ R7=3 - vsync is at row 35 = 280 scanlines
	LDA #7:	STA &FE00
	LDA #25: STA &FE01			; 280 - 256 = 24 scanlines

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

PAGE_ALIGN
.kefrens_addr_table_LO
FOR x,0,255,1
EQUB LO(screen_addr + ((x-48-2) DIV 2)*8)
NEXT

PAGE_ALIGN
.kefrens_addr_table_HI
FOR x,0,255,1
EQUB HI(screen_addr + ((x-48-2) DIV 2)*8)
NEXT

PAGE_ALIGN
.kefrens_width_table_1
FOR y,0,255,1
x=INT(128 + 76 * SIN(y * 2 * PI / 256))			\\ config 1
EQUB x
NEXT

PAGE_ALIGN
.kefrens_add_table_1
FOR y,0,255,1
EQUB 30 * SIN(y * 2 * PI / 256)				\\ config 1
NEXT

PAGE_ALIGN
.kefrens_speed_table_1
FOR n,0,&FF,1			; &13F for COS
EQUB 127 * SIN(2 * PI * n / 256)				\\ config 1
NEXT

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
PRINT "FREE =", ~start+_DEMO_SIZE-end
PRINT "------"

\ ******************************************************************
\ *	Any other files for the disc
\ ******************************************************************
