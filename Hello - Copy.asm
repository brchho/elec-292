$MODLP52
org 0000H
   ljmp MainProgram
; External interrupt 0 vector (not used in this code)
org 0x0003
	reti
; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR
; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$include(math32.inc) ; A library of math related functions and utility macros
$LIST

CLK  EQU 22118400
BAUD equ 115200
T1LOAD equ (0x100-(CLK/(16*BAUD)))
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))


DSEG at 30H
Result: ds 2
x: ds 4
y: ds 4
bcd: ds 2
Count1ms:     ds 2 ; Used to determine when half second has passed

bseg
mf: dbit 1
half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
ramp_to_soak_done: dbit 1
soak_done_flag: dbit 1
reflow_start: dbit 1
reflow_peak: dbit 1
reflow_done: dbit 1
open_door: dbit 1
take_PCB: dbit 1

cseg
; These ’EQU’ must match the wiring between the microcontroller and ADC
CE_ADC equ P2.0
MY_MOSI equ P2.1 ;Master out, Slave in
MY_MISO equ P2.2 ;Master in, Slave out
MY_SCLK equ P2.3 ;Serial Clock
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P1.2
LCD_RW equ P1.3
LCD_E  equ P1.4
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5
SOUND_OUT     equ P3.7

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:	
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	; In mode 1 we need to reload the timer.
	clr TR0
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	setb TR0
	cpl SOUND_OUT ; Connect speaker to P3.7!
	reti

INIT_SPI:
 	setb MY_MISO ; Make MISO an input pin
 	clr MY_SCLK ; For mode (0,0) SCLK is zero
 	;setb CE_ADC ;putting enables into inactive state
 	ret

DO_SPI_G:
 	push acc
 	mov R1, #0 ; Received byte stored in R1
 	mov R2, #8 ; Loop counter (8-bits)
DO_SPI_G_LOOP:
 	mov a, R0 ; Byte to write is in R0
 	rlc a ; Carry flag has bit to write
 	mov R0, a
 	mov MY_MOSI, c
 	setb REN
 	setb MY_SCLK ; Transmit
 	mov c, MY_MISO ; Read received bit
 	mov a, R1 ; Save received bit in R1
	rlc a
 	mov R1, a
 	clr MY_SCLK
 	djnz R2, DO_SPI_G_LOOP
 	pop acc
 	ret
    
; Configure the serial port and baud rate using timer 1
InitSerialPort:
    ; Since the reset button bounces, we need to wait a bit before
    ; sending messages, or risk displaying gibberish!
    mov R1, #222
    mov R0, #166
    djnz R0, $   ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, $-4 ; 22.51519us*222=4.998ms
    ; Now we can safely proceed with the configuration
	clr	TR1
	anl	TMOD, #0x0f
	orl	TMOD, #0x20
	orl	PCON,#0x80
	mov	TH1,#T1LOAD
	mov	TL1,#T1LOAD
	setb TR1
	mov	SCON,#0x52
    ret

; Send a character using the serial port
putchar:
    jnb TI, putchar
    clr TI
    mov SBUF, a
    ret

; Send a constant-zero-terminated string using the serial port
SendString:
    clr A
    movc A, @A+DPTR
    jz SendStringDone
    lcall putchar
    inc DPTR
    sjmp SendString
SendStringDone:
    ret
 
Hello_World:
    DB '\r','\n', 0

Do_Something_With_Result:
	mov a, Result
	mov x+0, a
	mov a, Result+1
	mov x+1, a
	mov x+2, #0x0
	mov x+3, #0x0
	lcall Calculate_Temp
	lcall hex2bcd
	lcall display_temp
	mov a, bcd+0											
	lcall begin
	
display_temp:
	Send_BCD(bcd)
	mov DPTR, #Hello_World
	lcall SendString
	ret

begin:
	jb ramp_to_soak_done, soak_stage 
	cjne a, #0x20, booster
	lcall beep
  	setb ramp_to_soak_done
	sjmp booster

soak_stage:
	jb soak_done_flag, reflow_begin
	cjne a, #0x22, booster
	lcall beep
	setb soak_done_flag
	sjmp booster
	
reflow_begin:
	jb reflow_start, reflow_max
	cjne a, #0x25, booster
	lcall beep
	setb reflow_start
	sjmp booster
		
reflow_max:
	jb reflow_peak, reflow_finish
	cjne a, #0x30, booster
	lcall beep
	setb reflow_peak
	sjmp booster
	
reflow_finish:
	jb reflow_done, door_open
	cjne a, #0x27, booster
	lcall beep 
	setb reflow_done
	sjmp booster

door_open:
	jb open_door, remove_PCB
	cjne a, #0x24, booster
	lcall long_beep
	setb open_door
	sjmp booster

remove_PCB:
	jb take_PCB, booster
	cjne a, #0x20, booster
	lcall beep
	lcall beep
	lcall beep
	lcall beep
	lcall beep
	lcall beep
	setb take_PCB
	sjmp booster
	
booster:
	ljmp Forever
	
Calculate_Temp:
	Load_y(500)
	lcall mul32
	Load_y(1023)
	lcall div32
	Load_y(273)
	lcall sub32
	ret	
				
MainProgram:
    mov SP, #7FH ; Set the stack pointer to the begining of idata
    mov PMOD, #0 ; Configure all ports in bidirectional mode
   
    lcall InitSerialPort
    lcall SendString
    lcall INIT_SPI
    lcall Timer0_Init
    setb EA
   	lcall clear_flags
    clr TR0
    lcall LCD_4BIT
    
Forever:
	clr CE_ADC
	mov R0, #00000001B ; Start bit:1
	lcall DO_SPI_G
	mov R0, #10000000B ; Single ended, read channel 0
	lcall DO_SPI_G
	mov a, R1 ; R1 contains bits 8 and 9
	anl a, #00000011B ; We need only the two least significant bits
	mov Result+1, a ; Save result high.
	mov R0, #55H ; It doesn't matter what we transmit...
	lcall DO_SPI_G
	mov Result, R1 ; R1 contains bits 0 to 7. Save result low.
	setb CE_ADC
	Set_Cursor(2,16)
	Display_char(#'C')
	lcall delay
	ljmp Do_Something_With_Result
	sjmp Forever
	sjmp $ ; This is equivalent to 'forever: sjmp forever'  
		
END