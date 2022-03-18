;-------------------------------------------------------------------------------
;Encabezado
;-------------------------------------------------------------------------------
    
; Archivo: postlab.s
; Dispositivo: PIC16F887
; Autor: José Fernando de León González
; Compilador:  pic-as (v2.30), MPLABX v5.40
;
; Programa: reloj multifuncional
; Hardware: displays en el puerto C, leds en el puerto A, botones en el puerto B, transistores y resistencias.
;
; Creado: 14/03/22
; Última modificación: 14/03/22
    
PROCESSOR 16F887

;-------------------------------------------------------------------------------
;Palabras de configuración 
;-------------------------------------------------------------------------------
    
; CONFIG1
  CONFIG  FOSC = INTRC_NOCLKOUT ; Oscillator Selection bits (INTOSCIO oscillator: I/O function on RA6/OSC2/CLKOUT pin, I/O function on RA7/OSC1/CLKIN)
  CONFIG  WDTE = OFF            ; Watchdog Timer Enable bit (WDT disabled and can be enabled by SWDTEN bit of the WDTCON register)
  CONFIG  PWRTE = ON            ; Power-up Timer Enable bit (PWRT enabled)
  CONFIG  MCLRE = OFF           ; RE3/MCLR pin function select bit (RE3/MCLR pin function is digital input, MCLR internally tied to VDD)
  CONFIG  CP = OFF              ; Code Protection bit (Program memory code protection is disabled)
  CONFIG  CPD = OFF             ; Data Code Protection bit (Data memory code protection is disabled)
  CONFIG  BOREN = OFF            ; Brown Out Reset Selection bits (BOR enabled)
  CONFIG  IESO = OFF             ; Internal External Switchover bit (Internal/External Switchover mode is enabled)
  CONFIG  FCMEN = OFF            ; Fail-Safe Clock Monitor Enabled bit (Fail-Safe Clock Monitor is enabled)
  CONFIG  LVP = ON              ; Low Voltage Programming Enable bit (RB3/PGM pin has PGM function, low voltage programming enabled)

; CONFIG2
  CONFIG  BOR4V = BOR40V        ; Brown-out Reset Selection bit (Brown-out Reset set to 4.0V)
  CONFIG  WRT = OFF             ; Flash Program Memory Self Write Enable bits (Write protection off)

;-------------------------------------------------------------------------------
;Librerías incluidas
;-------------------------------------------------------------------------------
  
#include <xc.inc>


;-------------------------------------------------------------------------------
;Macros
;-------------------------------------------------------------------------------
  
;******************** Macros de los timers *************************************  
RESTART_TMR0 macro value
    BANKSEL TMR0        ; banco 00
    MOVLW value         ; cargar valor inicial a W
    MOVWF TMR0          ; cargar el valor inicial al TIMER0
    BCF T0IF            ; limpiar la bandera  de overflow del TIMER0
    endm
    
RESTART_TMR1 macro valueH, valueL
 
    MOVLW valueH	  ;Cargando valor inicial de conteo
    MOVWF TMR1H
    
    MOVLW valueL
    MOVWF TMR1L
    
    BCF TMR1IF
    endm
    
;******************** Macros de overflow y underflow ***************************

COUNT_RESET_INC macro limit, register_rev, register_inc
    MOVLW limit			; Enviar la literal limit a W
    SUBWF register_rev, W	; Restar limit al registro register_rev
    BTFSS STATUS, 2		; Verificar si ya se ha contado hasta limit
    return			; NO: salir de la subrutina
    CLRF register_rev		; SÍ: reiniciar el registro register_rev e incrementar en uno el registro register_inc
    INCF register_inc
    endm
    
COUNT_RESET macro limit, register_rev
    MOVLW limit		    ; Enviar la literal limit a W
    SUBWF register_rev, W   ; Restar limit al registro register_rev
    BTFSS STATUS, 2	    ; Verificar si ya se ha contado hasta limit
    return		    ; NO: salir de la subrutina
    CLRF register_rev	    ; SÍ: reiniciar el registro register_rev e incrementar en uno el registro register_inc
    endm
    
OVRFLW_COUNT_RESET macro limit_decs, limit_units, register_rev_decs, register_rev_units
    MOVLW limit_units		; SÍ: enviar la literal limit_units a W
    SUBWF register_rev_units, W	; Restar limit al registro register_rev_units
    BTFSS STATUS, 2		; verificar si ya se ha contado hasta limit_units
    return			; NO: salir de la subrutina
    MOVLW limit_decs		; Enviar la literal limit_decs a W
    SUBWF register_rev_decs, W	; Restar limit al registro register_rev_decs
    BTFSS STATUS, 2		; Verificar si ya se ha contado hasta limit_decs
    return			; NO: salir de la subrutina
    CLRF register_rev_decs	; SÍ: limpiar register_rev_decs y register_rev_units
    CLRF register_rev_units
    endm

UNDRFLW_RESET_DEC macro under_limit, adopt_value_units, register_rev,  register_dec
    MOVLW under_limit
    SUBWF register_rev, W
    BTFSS STATUS, 2
    return
    MOVLW adopt_value_units
    MOVWF register_rev
    DECF register_dec
    endm
    
UNDRFLW_RESET macro under_limit, adopt_value_units, register_rev
    MOVLW under_limit
    SUBWF register_rev, W
    BTFSS STATUS, 2
    return
    MOVLW adopt_value_units
    MOVWF register_rev
    endm
 
    
;******************** Macros de display ****************************************
DISPLAY_SHOW_STATE macro state_register1, state_register2, state_register3, state_register4 
    MOVF    state_register1, W	; Movemos nibble bajo a W
    call    table		; Buscamos valor a cargar en PORTC
    MOVWF   display_val		; Guardamos en el primer registro de la variable display_val
    
    MOVF    state_register2, W	; Movemos nibble alto a W
    call    table		; Buscamos valor a cargar en PORTC
    MOVWF   display_val+1	; Guardamos en en el segundo registro de la variable display_val
    
    MOVF    state_register3, W	; Movemos nibble alto a W
    call    table		; Buscamos valor a cargar en PORTC
    MOVWF   display_val+2	; Guardamos en en el segundo registro de la variable display_val
    
    MOVF    state_register4, W	; Movemos nibble alto a W
    call    table		; Buscamos valor a cargar en PORTC
    MOVWF   display_val+3	; Guardamos en en el segundo registro de la variable display_val
    endm

;******************** Macros del modo edit *************************************
    
MOV_REG_SETS macro register1, dest_register1, register2, dest_register2, register3, dest_register3, register4, dest_register4
    MOVF register1, W
    MOVWF dest_register1
    
    MOVF register2, W
    MOVWF dest_register2
    
    MOVF register3, W
    MOVWF dest_register3
    
    MOVF register4, W
    MOVWF dest_register4
    endm
    
;-------------------------------------------------------------------------------
;Variables
;-------------------------------------------------------------------------------
  PSECT udata_shr ; Variables en la memoria RAM compartida entre bancos
    
;******************** variables de interrupción ******************************** 
    
    W_TEMP:	    DS 1	; 1 byte reservado (W Temporal)
    STATUS_TEMP:    DS 1	; 1 byte reservado (STATUS Temporal)

;********************** variables multiplexado *********************************
    
  PSECT udata_bank0 ; Variables en la memoria RAM del banco 0  
    flags:		DS 1	; 1 byte reservado (Variable que indica que display hay que encender en cada instante)
    nibbles:		DS 2	; 2 bytes reservado (Variable que divide los nibbles alto y bajo de valor)
    display_val:	DS 4	; 4 bytes reservado Representación de cada nibble en el display de 7-seg

;******************** variables de selección de modo ***************************
    STATE:	    DS 1	; 1 byte reservado (Variable que indica el modo actual)
    EDIT:	    DS 1	; 1 byte reservado (Variable que indica el si se ha habilitado el modo editar)

;******************** variables de modo edición ********************************
    
    EDIT_REG_1: DS 1	; 4 bytes reservados (Variable que almacena el primer valor de los displays del modo edición)
    EDIT_REG_2: DS 1	; 4 bytes reservados (Variable que almacena el segundo valor de los displays del modo edición)
    EDIT_REG_3: DS 1	; 4 bytes reservados (Variable que almacena el tercer valor de los displays del modo edición)
    EDIT_REG_4: DS 1	; 4 bytes reservados (Variable que almacena el cuarto valor de los displays del modo edición)
    
;******************** variables de modo hora  H ********************************
    
    S0_SECS:	    DS 1	; 1 byte reservado (Variable para definir los segundos transcurridos)
    S0_MINS_UNITS:  DS 1	; 1 byte reservado (Variable para definir las unidades de minutos transcurridos)
    S0_MINS_DECS:   DS 1	; 1 byte reservado (Variable para definir las decenas  de minutos transcurridos)
    S0_HRS_UNITS:   DS 1	; 1 byte reservado (Variable para definir las unidades de horas transcurridas)
    S0_HRS_DECS:    DS 1	; 1 byte reservado (Variable para definir las decenas de horas transcurridas)

    
;-------------------------------------------------------------------------------
;Vector Reset
;-------------------------------------------------------------------------------

PSECT VectorReset, class = CODE, abs, delta = 2 ; delta = 2: Las instrucciones necesitan 2 localidades para ejecutarse & abs = absolute: indicamos que contamos a partir de 0x0000
ORG 00h  ; la localidad del vector reset es 0x0000
 
VectorReset:
    PAGESEL main
    GOTO main

;-------------------------------------------------------------------------------
;Vector de interrupción
;-------------------------------------------------------------------------------

ORG 04h	    ; posición 0004h para las interrupciones
push:
    MOVWF W_TEMP		;guardamos los valores previos del STATUS y el W en variables temporales
    SWAPF STATUS, W
    MOVWF STATUS_TEMP
isr:
    
    BTFSC   RBIF
    call    int_IOCB
    BTFSC   T0IF
    call    int_TMR0
    BTFSC   TMR1IF  
    call    int_TMR1
   // BTFSC   TMR2IF
   // call    int_TMR2
    
pop:				;regresamos los valores de W y STATUS
    SWAPF STATUS_TEMP, W
    MOVWF STATUS
    SWAPF W_TEMP, F
    SWAPF W_TEMP, W
    retfie

;-------------------------------------------------------------------------------
;Subrutinas de interrupción
;-------------------------------------------------------------------------------
int_IOCB:
    banksel PORTB
    BTFSS PORTB, 0
    call change_state
    BTFSS PORTB, 2
    call change_edit
    BTFSS PORTB, 3
    DECF EDIT_REG_1
    BTFSS PORTB, 4
    INCF EDIT_REG_1 
    
    BCF RBIF		    ; SÍ: limpiamos la bandera de interrupción
    return

change_state:
    INCF STATE
    COUNT_RESET 6, STATE
    return
    
change_edit:
    MOVLW 1
    XORWF EDIT, F
    
    BTFSC EDIT, 0
    goto $+2
    goto $+3
    call capture_reg_values
    goto $+2
    call ret_reg_values
    return

capture_reg_values:
    MOVLW 0		    ; Revisión de si el estado actual es 0 - mover 0 a W
    SUBWF STATE, W	    ; Restar la literal a la variable STATE
    BTFSC STATUS, 2	    ; Revisar si el resultado de la resta es 0
			    ; SÍ: enviar los valores de las variables de S0 a las variables del modo edit
    MOV_REG_SETS S0_MINS_UNITS, EDIT_REG_1, S0_MINS_DECS, EDIT_REG_2, S0_HRS_UNITS, EDIT_REG_3, S0_HRS_DECS, EDIT_REG_4     
    return

ret_reg_values:
    MOVLW 0		    ; Revisión de si el estado actual es 0 - mover 0 a W
    SUBWF STATE, W	    ; Restar la literal a la variable STATE
    BTFSC STATUS, 2	    ; Revisar si el resultado de la resta es 0
			    ; SÍ: enviar los valores de las variables del modo edit a las variables de S0 
    MOV_REG_SETS EDIT_REG_1, S0_MINS_UNITS, EDIT_REG_2, S0_MINS_DECS, EDIT_REG_3, S0_HRS_UNITS, EDIT_REG_4, S0_HRS_DECS      
    return    

dec_edit_reg:
    return
int_TMR0:
    call display_selection
    RESTART_TMR0 6
    return

display_selection:
    BCF	    PORTD, 0		; Apagamos todos los displays
    BCF	    PORTD, 1		 
    BCF	    PORTD, 2
    BCF	    PORTD, 3
    
    BTFSC   flags, 1		; Verificamos banderas
    goto    display_1		  
    
    BTFSC   flags, 2
    goto    display_2		  
    
    BTFSC   flags, 3
    goto    display_3		  
    goto    display_0
    return
    
display_0:			
    MOVF    display_val, W	; Movemos display a W
    MOVWF   PORTC		; Movemos Valor de tabla a PORTC
    BSF	PORTD, 0	        ; Encendemos display de nibble bajo
    CLRF flags			; Cambiamos bandera para cambiar el otro display en la siguiente interrupción
    BSF  flags, 1
    return

display_1:
    MOVF    display_val+1, W	; Movemos display+1 a W
    MOVWF   PORTC		; Movemos Valor de tabla a PORTC
    BSF	PORTD, 1		; Encendemos display de nibble alto
    CLRF flags			; Cambiamos bandera para cambiar el otro display en la siguiente interrupción
    BSF  flags, 2
    return
    
display_2:
    MOVF    display_val+2, W	; Movemos display+1 a W
    MOVWF   PORTC		; Movemos Valor de tabla a PORTC
    BSF	PORTD, 2		; Encendemos display de nibble alto
    CLRF flags			; Cambiamos bandera para cambiar el otro display en la siguiente interrupción
    BSF  flags, 3
    return
    

display_3:
    MOVF    display_val+3, W	; Movemos display+1 a W
    MOVWF   PORTC		; Movemos Valor de tabla a PORTC
    BSF	PORTD, 3		; Encendemos display de nibble alto
    CLRF flags			; Cambiamos bandera para cambiar el otro display en la siguiente interrupción
    BSF  flags, 0
    return
    
int_TMR1:    
    INCF S0_SECS		; incrementamos la variable de segundos
    RESTART_TMR1 0x0B, 0xDC	; reiniciamos el TIMER1
        
    return
       
;-------------------------------------------------------------------------------
;Tabla para display de siete segmentos
;-------------------------------------------------------------------------------

PSECT table, class = CODE, abs, delta = 2
ORG 150h 

table:
    CLRF PCLATH
    BSF PCLATH, 0           ; PCLATH en 01
    ANDLW 0X0F
    ADDWF PCL               ; PC = PCLATH + PCL | Sumamos W al PCL para seleccionar un dato en la tabla
    retlw 00111111B         ; 0
    retlw 00000110B         ; 1
    retlw 01011011B         ; 2
    retlw 01001111B         ; 3
    retlw 01100110B         ; 4
    retlw 01101101B         ; 5
    retlw 01111101B         ; 6
    retlw 00000111B         ; 7
    retlw 01111111B         ; 8 
    retlw 01101111B         ; 9
    retlw 01110111B         ; A
    retlw 01111100B         ; b
    retlw 00111001B         ; C
    retlw 01011110B         ; D
    retlw 01111001B         ; C
    retlw 01110001B         ; F
    
    
;-------------------------------------------------------------------------------
;main (configuración)
;-------------------------------------------------------------------------------

main:
    call config_clock	    ; configuramos el reloj 
    call config_TMR0	    ; configuramos el TIMER0
    call config_TMR1	    ; configuramos el TIMER1
    call config_TMR2	    ; configuramos el TIMER2
    call config_IOCB	    ; configuramos las interruptions on change de PORTB
    call config_int_enable  ; configuramos las interrupciones
    call config_ports	    ; configuramos puertos
    
    banksel PORTA
    			    ;---------------------------------------------------
			    ;TEMPORIZACIÓN TIMER0:  2 ms
			    ;TEMPORIZACIÓN TIMER1:  1000 ms
			    ;TEMPORIZACIÓN TIMER2:  500 ms
			    ;---------------------------------------------------
 
;-------------------------------------------------------------------------------
;Loop
;-------------------------------------------------------------------------------
loop: 
    call S0_time_check	    ; Control del tiempo en las variables del modo hora
    call edit_reg_check	    ; Control del overflow y underflow de las variables de edición
    call display_state_show ; Envío de literales a los displays dependiendo del modo
    
    goto loop
    
    
;-------------------------------------------------------------------------------
;subrutinas 
;-------------------------------------------------------------------------------
  
S0_time_check:
    
    COUNT_RESET_INC 60, S0_SECS, S0_MINS_UNITS
    COUNT_RESET_INC 10, S0_MINS_UNITS, S0_MINS_DECS
    COUNT_RESET_INC 6, S0_MINS_DECS, S0_HRS_UNITS
    OVRFLW_COUNT_RESET 2, 4, S0_HRS_DECS, S0_HRS_UNITS
    COUNT_RESET_INC 10, S0_HRS_UNITS, S0_HRS_DECS
    
    return
    
edit_reg_check:
    MOVLW 0		    ; Revisión de si el estado actual es 0 - mover 0 a W
    SUBWF STATE, W	    ; Restar la literal a la variable STATE
    BTFSC STATUS, 2	    ; Revisar si el resultado de la resta es 0
    call S0_edit_limits	    ; SÍ: verificar los límites de las variables de edición durante el estado S0
    return
    
S0_edit_limits:
    call S0_edit_limits_up
    call S0_edit_limits_down
    
S0_edit_limits_up:
    UNDRFLW_RESET_DEC 0xFF, 9, EDIT_REG_1,  EDIT_REG_2
    UNDRFLW_RESET_DEC 0xFF, 5, EDIT_REG_2,  EDIT_REG_3
    UNDRFLW_RESET_DEC 0xFF, 3, EDIT_REG_3,  EDIT_REG_4
    UNDRFLW_RESET  0xFF, 2, EDIT_REG_4
    return
    
S0_edit_limits_down:
    COUNT_RESET_INC 10, EDIT_REG_1, EDIT_REG_2
    COUNT_RESET_INC 6, EDIT_REG_2, EDIT_REG_3
    OVRFLW_COUNT_RESET 2, 4, EDIT_REG_4, EDIT_REG_3
    COUNT_RESET_INC 10, EDIT_REG_3, EDIT_REG_4
    
    return
    
display_state_show:
    MOVLW 0		    ; Revisión de si el estado actual es 0 - mover 0 a W
    SUBWF STATE, W	    ; Restar la literal a la variable STATE
    BTFSC STATUS, 2	    ; Revisar si el resultado de la resta es 0
    call S0_edition_mode    ; SÍ: enviar los valores de los displays de S0 en base al estado de EDIT	
    ///			    ; NO: verificar si el estado actual es 1 - mover 1 a W
    return

S0_edition_mode:
    BTFSC EDIT, 0
    goto edit
    goto hrs
    edit:
    DISPLAY_SHOW_STATE EDIT_REG_1, EDIT_REG_2, EDIT_REG_3, EDIT_REG_4 
    return
    hrs:
    DISPLAY_SHOW_STATE S0_MINS_UNITS, S0_MINS_DECS, S0_HRS_UNITS, S0_HRS_DECS 
    return


;-------------------------------------------------------------------------------
;subrutinas de configuración
;-------------------------------------------------------------------------------

config_clock:
    banksel OSCCON      ;banco 01
    BCF IRCF2
    BSF IRCF1
    BSF IRCF0           ; IRCF <2:0> -> 011 500 kHz
    
    BSF SCS             ;reloj interno
    return
    
config_TMR0:
    banksel TRISA  ; banco 01
    BCF T0CS            ; TIMER0 como temporizador
    BCF PSA             ; Prescaler a TIMER0
    BCF PS2
    BCF PS1
    BCF PS0             ; PS<2:0> -> preescaler 000 1:1
    
    RESTART_TMR0 6    
    return
    
config_TMR1:
    banksel T1CON
    BCF TMR1GE		; Siempre contando
    
    BCF T1CKPS1 
    BSF T1CKPS0		; T1CKPS <1:0> -> 01 Prescaler 1:2 
    
    BCF T1OSCEN		;reloj interno
    BCF TMR1CS
    BSF TMR1ON		;prender TMR1
    
    RESTART_TMR1 0x0B, 0xDC
    return

config_TMR2:
    banksel PR2
    MOVLW   244		    ; Valor para interrupciones cada 50ms
    MOVWF   PR2		    ; Cargamos litaral a PR2
    
    BANKSEL T2CON	    ; Cambiamos a banco 00
    BSF	    T2CKPS1	    ; Prescaler 1:16
    BSF	    T2CKPS0
    
    BSF	    TOUTPS3	    ;Postscaler 1:16
    BSF	    TOUTPS2
    BSF	    TOUTPS1
    BSF	    TOUTPS0
    
    BSF	    TMR2ON	    ; Encendemos TMR2
    return
 
config_IOCB:
    banksel TRISA
    BSF IOCB, 0
    BSF IOCB, 1	    
    BSF IOCB, 2
    BSF IOCB, 3
    BSF IOCB, 4	    ; seteamos los primeros 5 bits del puerto B como interrupt on change
    
    banksel PORTA
    MOVF PORTB, W   ; Al leer termina condición de mismatch
    BCF RBIF
    
    return
    
config_int_enable:
    banksel TRISA
    BSF	TMR1IE		    ; Habilitar interrupción del TIMER1 
    //BSF TMR2IE		    ; Habilitar interrupción del TIMER2
    
    banksel PORTA
    BSF GIE		    ; INTCON: Habilitar interrupciones globales	    
    BSF PEIE		    ; Habilitar interrupciones de periféricos
    
    BSF RBIE
    BCF RBIF
    
    BCF TMR1IF		    ; Limpiar bandera de interrupción de TIMER1
    BCF TMR2IF		    ; Limpiar bandera de interrupción de TIMER2
    
    BSF T0IE		    ; Habilitar interrupción del TIMER0
    BCF T0IF		    ; Limpiar bandera de interrupciones
    return
    
config_ports:
    banksel ANSEL       ; banco 11
    CLRF ANSEL		; pines digitales
    CLRF ANSELH
    
    banksel TRISA       ; banco 01
    CLRF TRISA		; PORTA como salida
    CLRF TRISC		; PORTC como salida
    CLRF TRISD		; PORTD como salida
    
    BSF  TRISB0
    BSF  TRISB1         
    BSF  TRISB2
    BSF  TRISB3
    BSF  TRISB4		; pines 1, 2, 3, 4 & 5 del puerto B como entradas
    
    
    BCF OPTION_REG, 7	;Habilitar Pull-ups
    
    banksel PORTA       ; banco 00
    CLRF PORTA		; limpiamos PORTA
    CLRF PORTC		; limpiamos PORTC
    CLRF PORTD		; limpiamos PORTD
    CLRF flags
    CLRF STATE
    
    return
END