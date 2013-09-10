.nolist						;Turn listfile generation Off
.include "m8def.inc"		;All defines(Ports, Interrupt Table, etc...) needed for the Atmega8
.list						;Turn listfile generation On

.equ rowMultiplier = 4
.equ FactAdivisor = 10
.equ tickRate = 192.5   ;only using this value for now
.equ gForceCheck = 5


.dseg 						; Start data segment
	.org 0x67 					; Set SRAM address to hex 67
	scheduleFlag: .BYTE 1		; Reserve a byte at SRAM 



.cseg
.org $00000					;Setting Origin Address
		rjmp Main 			;Reset vector
.org INT0addr				;Setting Origin Address
		rjmp IntV0 			;INT vector
.org OVF0addr				;Setting Origin Address
		rjmp ClockTick 		;ClockTick vector


.org   0x0150               ;table address engine speed (RPM) and load
;RPM(000)/Load
RPMLoad_Lookup:        ;1/1	   2/1	    3/1	    4/1	   1/2     2/2 		3/2		4/2		1/3		2/3		3/3		4/3		1/4		2/4		3/4		4/4		  
	        .db       0x0001, 0x0002, 0x0003, 0x0004, 0x0002, 0x0004, 0x0006, 0x0008, 0x0003, 0x0006, 0x0009, 0x000C, 0x0004, 0x0008, 0x000C, 0x0010
					 
.org 0x0180
FactorALookUp:
			.db    12, 11, 10, 9    ;will need to divide this by 10 later


.org 0x0200
FactorBLookUp:
			.db		4, 4, 4, 3   ;will need to divide this by 4 later

.org $00250					;Setting Origin Address

.include "MECHENG313.inc"	;Functions needed for MECHENG313



Main: 
						;Initialise your stack pointer here
		ldi r16,high(RAMEND)	;Loading Higher Ram end address in to r16
		out SPH,r16			;Init Stack Pointer Higher Bytes
		ldi r16,low(RAMEND) 	;Loading Lower Ram end address in to r16
		out SPL,r16			;Init Stack Pointer Lower Bytes

		;ldi r20, 0x01
		;sts ScheduleFlag, r20		;Hint - variable to use for task scheduling. you might also need a schedule counter?

		sbi PORTD,PD2			;I/O Setup
		sbi PORTB,0;
		sbi DDRB,0;	

		;********* INT0 ********
		ldi r16,(1<<INT0) ; int masks INT0 set
		out GIMSK,r16
		ldi r16,(1<<ISC01) ;External interrupts are triggered by the INT0
		out MCUCR,r16

		;********* ADC ********
		; set MUX to channel 1, AREF taken from AVCC PORTC
		ldi r16, (1<<MUX0) 
		out ADMUX,r16
		; switch AD conversion on, start conversion (free running), divider rate = 16
			
		ldi r16, (1<<ADEN||1<<ADFR||1<<ADPS2)
		out ADCSRA, r16

		;********* ClockTick 8-bit Timer/Counter 0 *******      
		ldi r16, (1<<CS01)            
      		out TCCR0, r16			; Timer Clock = Sys Clock (1MHz) / 8 (prescaler)
		ldi r16, (1<<TOIE0)            
		out TIMSK, r16			; Enable Timer Overflow interrupt

		; MaxValue = TOVck (1.5ms or your Cal time) * Pck (1MHz) / 8 (prescaler)
		; TCNT0Value = 255 - MaxValue
		; for 1.5ms  TCNT0 value is 67.5, use this value for now as our tickRate

		ldi    r16, -high(tickRate/2)
		out     TCNT1H, r16
		ldi     r16, -low(tickRate/2) ;time from here to 0
		out     TCNT1L, r16


		sei ; enable interrupts and off we go!

Collision_Detection:
	
	ldi r16, 1<<ADSC ;start conversion
	out ADSC, r16
	
	ldi r16, ADCH		;Load ADC high value into r16 
	andi r16,0b00000011 ;only take two lsb's of ADCH
	lsl r16				; shift output to left by two to give 2 MSBs of g force measurement 
	lsl r16
	
	ldi r17, ADCL		; Load ADC low value into r17 and get two MSBs 
	andi r17,0b11000000
	lsr r17				; Shift output to the right by 6 to get 2 LSBs of g-force measurement 
	lsr r17
	lsr r17
	lsr r17
	lsr r17
	lsr r17

	add r16,r17 ; add two MSBs to LSBs of the g-force measurement
	
	ldi r17, gForceCheck ; Load and compare g-Force threshold to measurement
	cp r16,r17
	BRGE collisionIndicatorOn ;If g-force is >= 5 then turn LED0 on
	
	

	cp r16,r17
	BRLT collisionIndicatorOff ; If g-force is <= 5 then turn LED0 off

	rjmp Collision_Detection
	
	


collisionIndicatorOn: cbi PORTB,0	;Turn collision detection LED0 on
collisionIndicatorOff: sbi PORTB,0	;Turn collision detection LED0 off

forever:
		Start_Task UpTime
		rcall Task_1

		End_Task UpTime
		rjmp forever 


Task_1:	Start_Task 	1	;Turn output indicator pin On

		ldi ZH, HIGH(2*RPMLoad_Lookup)
		ldi ZL, LOW(2*RPMLoad_Lookup)

		ldi r16, 1<<ADSC ;start conversion
		out ADSC, r16

		in r17, ADCL         ;read in the ADC value
		andi r17, 0b11000000	; Get two MSBs for RPM value
		mov r20, r17			; shift the bits to bit 0,1
		lsr r20
		lsr r20
		lsr r20
		lsr r20
		lsr r20
		lsr r20


		in r17, ADCL
		andi r17, 0b00110000	; Get next two bits for for load value 
		mov r21, r17			; shift the bits to bit 0,1
		lsr r21
		lsr r21
		lsr r21
		lsr r21

		ldi r16, rowMultiplier			;RPM lookup table row value
		mul r20, r16				;multiply ADC value by row multiplier
		add r21, r0						;RPM column + row value	

		add ZL, r21						;load column/row value into z register
		lpm r16, Z				;store the value of base pulse width 


		ldi ZH, HIGH(2*FactorALookUp)
		ldi ZL, LOW(2*FactorALookUp)
		in r17, ADCL					;read in ADC value
		andi r17, 0b00001100 ;Get the next bits for factor A
		lsr r17					;shift to bits 0,1
		lsr r17

		add ZL, r17   ;offset Z by this level
		lpm r17, Z     ;store factor A value



		ldi ZH, HIGH(2*FactorBLookUp)
		ldi ZL, LOW(2*FactorBLookUp)
		in r18, ADCL					;read in ADC value
		andi r18, 0b00000011	;Get last bits for factor B

		add ZL, r18   ;offset Z by this level
		lpm r18, Z     ;store factor B value


		mul r16, r17    ;mul base pulse and factor A
		mul r0, r18    ;mul it by factor B
		ldi r24, 0
		movw r23:r22, r1:r0
		ldi r19, 40
		ldi r20, 0
		ldi r21, 0
		rcall div24x24_24   ;result in r22  reminder in r16
		                    ;result in r22 is accurate to whole number, it is the pulse width in milliseconds
							;frequency of this task execution must be adjusted everytime to the result calculated


		End_Task	1	;Turn output indicator pin Off
		ret


IntV0:
		reti			;Return from Interurpt



ClockTick:
		Start_Task 	ClockTick_Task	;Turn output indicator pin On
		push    r16
		in      r16, SREG
		push    r16
		ldi     r16, -high(tickRate/2)
		out     TCNT1H, r16
		ldi     r16, -low(tickRate/2) ;time from here to 0
		out     TCNT1L, r16            ;AVR rule: always write H,L, read L,H
		lds     r16, scheduleFlag
		inc     r16
		sts     scheduleFlag, r16
		pop     r16
		out     SREG, r16
		pop     r16
		reti		

		End_Task	ClockTick_Task	;Turn output indicator pin Off
		reti						;Return from Interrupt

