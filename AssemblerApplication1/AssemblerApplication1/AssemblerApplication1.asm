.nolist						;Turn listfile generation Off
.include "m8def.inc"		;All defines(Ports, Interrupt Table, etc...) needed for the Atmega8
.list						;Turn listfile generation On

.equ rowMultiplier = 4
.equ FactAdivisor = 10 
.
.dseg 						; Start data segment
.org 0x67 					; Set SRAM address to hex 67



ScheduleFlag:
			.BYTE 1		; Reserve a byte at SRAM 

.cseg
.org $00000					;Setting Origin Address
		rjmp Main 			;Reset vector
.org INT0addr				;Setting Origin Address
		rjmp IntV0 			;INT vector
.org OVF0addr				;Setting Origin Address
		rjmp ClockTick 		;ClockTick vector
.org   0x0100               ;table address engine speed (RPM) and load

;naming format = RPM(000)/Load
RPMLoad_Lookup:        ;1/1	   2/1	    3/1	    4/1	   1/2     2/2 		3/2		4/2		1/3		2/3		3/3		4/3		1/4		2/4		3/4		4/4		  
	        .db       0x0001, 0x0002, 0x0003, 0x0004, 0x0002, 0x0004, 0x0006, 0x0008, 0x0003, 0x0006, 0x0009, 0x000C, 0x0004, 0x0008, 0x000C, 0x0010
					 
.org 0x0125

FactorALookUp:
			.db    12, 11, 10, 9, 10


.org 0x0150
FactorBLookUp:
			.db		4, 4, 4, 3

			
			   

.org $00200					;Setting Origin Address

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

		sei ; enable interrupts and off we go!



forever:
		Start_Task UpTime
		rcall Task_1

		End_Task UpTime
		rjmp forever 






Task_1:	Start_Task 	1	;Turn output indicator pin On

		ldi ZH, HIGH(2*RPMLoad_Lookup)
		ldi ZL, LOW(2*RPMLoad_Lookup)

		ldi r16, 1<<ADSC

		out ADSC, r16

		ldi r20, 2;(ADCL & 0b11000000)	; Get two MSBs for RPM value

		ldi r21, 3;(ADCL & 0b00110000)	; Get bits 4-5 for for load value 

		ldi r17, rowMultiplier			; RPM lookup table row value

		mul r20, r17					;

		add r21, r0						; RPM column + row value	

		add ZL, r21						; load column/row value into z register
		lpm r16,z

		ldi r22, (ADCL & 0b00001100) ;Get the next bits for factor A


		ldi ZH, HIGH(2*FactorALookUp)
		ldi ZL, LOW(2*FactorALookUp)



		ldi r22, (ADCL & 0b00000011) ;Get the last bits for factor B

		ldi ZH, HIGH(2*FactorBLookUp)
		ldi ZL, LOW(2*FactorBLookUp)

		ldi r17, 3

		cp r22, r17

		breq equal

		equal: r21, r0




		End_Task	1	;Turn output indicator pin Off
		ret


IntV0:
		reti			;Return from Interurpt



ClockTick:
		Start_Task 	ClockTick_Task	;Turn output indicator pin On
		
		;********* Write ClockTick Code here ********

		;************************************

		End_Task	ClockTick_Task	;Turn output indicator pin Off
		reti						;Return from Interurpt

