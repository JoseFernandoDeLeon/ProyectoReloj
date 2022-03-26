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
; Última modificación: 19/03/22
    
PROCESSOR 16F887

;-------------------------------------------------------------------------------
;Palabras de configuración 
;-------------------------------------------------------------------------------
    
; CONFIG1
  CONFIG  FOSC = INTRC_NOCLKOUT ; Oscillator Selection bits (INTOSCIO oscillator: I/O function on RA6/OSC2/CLKOUT pin, I/O function on RA7/OSC1/CLKIN)
  CONFIG  WDTE = OFF            ; Watchdog Timer Enable bit (WDT disabled and can be enabled by SWDTEN bit of the WDTCON register)
  CONFIG  PWRTE = OFF            ; Power-up Timer Enable bit (PWRT enabled)
  CONFIG  MCLRE = OFF           ; RE3/MCLR pin function select bit (RE3/MCLR pin function is digital input, MCLR internally tied to VDD)
  CONFIG  CP = OFF              ; Code Protection bit (Program memory code protection is disabled)
  CONFIG  CPD = OFF             ; Data Code Protection bit (Data memory code protection is disabled)
  CONFIG  BOREN = OFF            ; Brown Out Reset Selection bits (BOR enabled)
  CONFIG  IESO = OFF             ; Internal External Switchover bit (Internal/External Switchover mode is enabled)
  CONFIG  FCMEN = OFF            ; Fail-Safe Clock Monitor Enabled bit (Fail-Safe Clock Monitor is enabled)
  CONFIG  LVP = OFF              ; Low Voltage Programming Enable bit (RB3/PGM pin has PGM function, low voltage programming enabled)

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

; Macro para incrementar un registro B cuando un registro A llega a un valor límite, donde se reinicia dicho registro
COUNT_RESET_INC macro limit, register_rev, register_inc
    MOVLW limit			; Enviar la literal limit a W
    SUBWF register_rev, W	; Restar limit al registro register_rev
    BTFSS STATUS, 2		; Verificar si ya se ha contado hasta limit
    return			; NO: salir de la subrutina
    CLRF register_rev		; SÍ: reiniciar el registro register_rev e incrementar en uno el registro register_inc
    INCF register_inc
    endm

; Macro para reiniciar un registro A cuando este llega a un valor límite
COUNT_RESET macro limit, register_rev
    MOVLW limit			; Enviar la literal limit a W
    SUBWF register_rev, W	; Restar limit al registro register_rev
    BTFSS STATUS, 2		; Verificar si ya se ha contado hasta limit
    return			; NO: salir de la subrutina
    CLRF register_rev		; SÍ: reiniciar el registro register_rev e incrementar en uno el registro register_inc
    endm

; Macro para reiniciar dos registros A & B cuando estos llegan a sus respectivos valores límite 
OVRFLW_COUNT_RESET macro limit_decs, limit_units, register_rev_decs, register_rev_units
    MOVLW limit_units		; Enviar la literal limit_units a W
    SUBWF register_rev_units, W	; Restar limit_units al registro register_rev_units
    BTFSS STATUS, 2		; verificar si ya se ha contado hasta limit_units
    return			; NO: salir de la subrutina
    MOVLW limit_decs		; Enviar la literal limit_decs a W
    SUBWF register_rev_decs, W	; Restar limit al registro register_rev_decs
    BTFSS STATUS, 2		; Verificar si ya se ha contado hasta limit_decs
    return			; NO: salir de la subrutina
    CLRF register_rev_decs	; SÍ: limpiar register_rev_decs y register_rev_units
    CLRF register_rev_units
    endm

; Macro para reiniciar dos registros A & B cuando estos llegan a sus respectivos valores límite, e incrementar un registro C
OVRFLW_COUNT_RESET_INC macro limit_decs, limit_units, register_rev_decs, register_rev_units, inc_register
    MOVLW limit_units		; Enviar la literal limit_units a W
    SUBWF register_rev_units, W	; Restar limit_units al registro register_rev_units
    BTFSS STATUS, 2		; verificar si ya se ha contado hasta limit_units
    return			; NO: salir de la subrutina
    MOVLW limit_decs		; Enviar la literal limit_decs a W
    SUBWF register_rev_decs, W	; Restar limit al registro register_rev_decs
    BTFSS STATUS, 2		; Verificar si ya se ha contado hasta limit_decs
    return			; NO: salir de la subrutina
    CLRF register_rev_decs	; SÍ: limpiar register_rev_decs y register_rev_units, incrementar inc_register
    CLRF register_rev_units
    INCF inc_register
    endm

; Macro para incrementar un registro C cuando dos macros llegan a sus respectivos valores límite SIN REINICIARLOS
  INC_REG_TWO_LIMITS macro limit_decs, limit_units, register_rev_decs, register_rev_units, set_register
    MOVLW limit_units		; Enviar la literal limit_units a W
    SUBWF register_rev_units, W	; Restar limit_units al registro register_rev_units
    BTFSS STATUS, 2		; verificar si ya se ha contado hasta limit_units
    return			; NO: salir de la subrutina
    MOVLW limit_decs		; Enviar la literal limit_decs a W
    SUBWF register_rev_decs, W	; Restar limit al registro register_rev_decs
    BTFSS STATUS, 2		; Verificar si ya se ha contado hasta limit_decs
    return			; NO: salir de la subrutina				
    INCF set_register		; SÍ: incrementar set_register
    endm

; Macro para enviar dos valores a dos respectivos registros
MOV_VALS_REGS macro val_decs, val_units, reg_decs, reg_units

    MOVLW val_decs
    MOVWF reg_decs
    MOVLW val_units
    MOVWF reg_units
    endm

; Macro para decrementar un registro B cuando un registro A llega a un valor límite, y asignar un nuevo valor a A
UNDRFLW_RESET_DEC macro under_limit, adopt_value_units, register_rev,  register_dec
    MOVLW under_limit
    SUBWF register_rev, W
    BTFSS STATUS, 2
    return
    MOVLW adopt_value_units
    MOVWF register_rev
    DECF register_dec
    endm

; Macro para asignar un nuevo valor a un registro A cuando este alcanza un valor límite
UNDRFLW_RESET macro under_limit, adopt_value_units, register_rev
    MOVLW under_limit
    SUBWF register_rev, W
    BTFSS STATUS, 2
    return
    MOVLW adopt_value_units
    MOVWF register_rev
    endm

; Macro para asignar dos nuevos valores a dos respectivos registros A & B cuando ambos llegan a 0, y decrementar un tercer registro C
 ZERO_DOUBLE_UNDRFLW_DEC macro adopt_value_decs, adopt_value_units, register_rev_decs, register_rev_units, dec_reg
    MOVLW 0		; SÍ: enviar la literal limit_units a W
    SUBWF register_rev_units, W	; Restar limit al registro register_rev_units
    BTFSS STATUS, 2		; verificar si ya se ha contado hasta limit_units
    return			; NO: salir de la subrutina
    MOVLW 0		; Enviar la literal limit_decs a W
    SUBWF register_rev_decs, W	; Restar limit al registro register_rev_decs
    BTFSS STATUS, 2		; Verificar si ya se ha contado hasta limit_decs
    return			; NO: salir de la subrutina
    MOVLW adopt_value_decs
    MOVWF register_rev_decs
    MOVLW adopt_value_units
    MOVWF register_rev_units
    DECF dec_reg
    endm
;******************** Macros de display ****************************************
; Macro para enviar valores de registros A, B, C & D a los displays
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
; Macro para enviar los valores de cuatro registros A, B, C & D a otros cuatro registros    
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
    TOOGLE:	    DS 1	; 1 byte reservado (Variable para encender o apagar los dos puntos del display)
;******************** variables de modo edición ********************************
    
    EDIT_REG_1: DS 1		; 4 bytes reservados (Variable que almacena el primer valor de los displays del modo edición)
    EDIT_REG_2: DS 1		; 4 bytes reservados (Variable que almacena el segundo valor de los displays del modo edición)
    EDIT_REG_3: DS 1		; 4 bytes reservados (Variable que almacena el tercer valor de los displays del modo edición)
    EDIT_REG_4: DS 1		; 4 bytes reservados (Variable que almacena el cuarto valor de los displays del modo edición)
    
;******************** variables de modo hora  S0 ********************************
    
    S0_SECS:	    DS 1	; 1 byte reservado (Variable para definir los segundos transcurridos)
    S0_MINS_UNITS:  DS 1	; 1 byte reservado (Variable para definir las unidades de minutos transcurridos)
    S0_MINS_DECS:   DS 1	; 1 byte reservado (Variable para definir las decenas  de minutos transcurridos)
    S0_HRS_UNITS:   DS 1	; 1 byte reservado (Variable para definir las unidades de horas transcurridas)
    S0_HRS_DECS:    DS 1	; 1 byte reservado (Variable para definir las decenas de horas transcurridas)

;******************** variables de modo fecha S1 *******************************
    
    S1_DAYS_UNITS:	DS 1	; 1 byte reservado (Variable para definir las unidades de días transcurridos)
    S1_DAYS_DECS:	DS 1	; 1 byte reservado (Variable para definir las decenas de días transcurridos)
    S1_MONTHS_UNITS:	DS 1	; 1 byte reservado (Variable para definir las unidades de meses transcurridos)
    S1_MONTHS_DECS:	DS 1	; 1 byte reservado (Variable para definir las decenas de meses transcurridos)
    
    TRANSC_MONTHS:	DS 1	; 1 byte reservado (Variable para contar la cantidad de meses transcurridos durante el año para definir la cantidad de días por mes)

;******************** variables de modo timer S2 *******************************
    S2_SECS_UNITS:	DS 1	; 1 byte reservado (Variable para definir las unidades de segundos transcurridos)
    S2_SECS_DECS:	DS 1	; 1 byte reservado (Variable para definir las decenas de segundos transcurridos)
    S2_MINS_UNITS:	DS 1	; 1 byte reservado (Variable para definir las unidades de minutos transcurridos)
    S2_MINS_DECS:	DS 1	; 1 byte reservado (Variable para definir las decenas de minutos transcurridos)
    TIME_OUT:		DS 1	; 1 byte reservado (Variable para indicar que la cuenta regresiva a finalizado)
    TIME_OUT_SECS:	DS 1	; 1 byte reservado (Variable para contar el tiempo transcurrido desde que la cuenta regresiva a finalizado)
    TIMER_START:	DS 1	; 1 byte reservado (Variable para indicar al timer que debe iniciar la cuenta)

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
isr:				; revisamos donde ocurrió la interrupción
    
    BTFSC   RBIF
    call    int_IOCB
    BTFSC   T0IF
    call    int_TMR0
    BTFSC   TMR1IF  
    call    int_TMR1
    BTFSC   TMR2IF
    call    int_TMR2
    
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
    BTFSS PORTB, 1
    call change_start
    BTFSS PORTB, 2
    call change_edit
    BTFSS PORTB, 3
    DECF EDIT_REG_1
    BTFSS PORTB, 4
    INCF EDIT_REG_1
    
    BCF RBIF		    ; limpiamos la bandera de interrupción
    return

change_state:		    ; se incrementa el valor del estado si el modo edición está deshabilitado
    BTFSC EDIT, 0
    return
    INCF STATE
    COUNT_RESET 3, STATE
    return
    
change_start:
    MOVLW 2			; Comprobamos que estamos en el modo timer para habilitar la bandera 
    SUBWF STATE, W		; iniciar/ parar.
    BTFSS STATUS, 2
    return			; si la bandera de timeout está activada, desactivarla y aumentar en uno los segundos del timer
    BTFSC TIME_OUT, 0
    goto restart_timer
    goto start_stop_timer
    return
    
    restart_timer:
    BCF TIME_OUT, 0
    INCF S2_SECS_UNITS
    BCF PORTA, 4
    return
    
    start_stop_timer:
    BTFSC EDIT, 0
    return
    MOVLW 1			; Cambiamos el estado de la bandera de iniciar/parar
    XORWF TIMER_START, F
    return

change_edit:			; Se habilita o deshabilita el modo edición
    MOVLW 1
    XORWF EDIT, F
    
    BTFSC EDIT, 0
    goto $+2
    goto $+3
    call capture_reg_values	; Se capturan los valores de las variables de modo en las variables de datos temporales
    goto $+2
    call ret_reg_values		; Se envían los valores de las variables de datos temporales a las variables de modo
    return

capture_reg_values:
    MOVLW 0		    ; Revisión de si el estado actual es 0 - mover 0 a W
    SUBWF STATE, W	    ; Restar la literal a la variable STATE
    BTFSS STATUS, 2	    ; Revisar si el resultado de la resta es 0
    goto S1_reg_values	    
			    ; SÍ: enviar los valores de las variables de S0 a las variables del modo edit
    MOV_REG_SETS S0_MINS_UNITS, EDIT_REG_1, S0_MINS_DECS, EDIT_REG_2, S0_HRS_UNITS, EDIT_REG_3, S0_HRS_DECS, EDIT_REG_4
    return
    
    S1_reg_values:
    MOVLW 1		    ; NO: Revisión de si el estado actual es 1 - mover 1 a W
    SUBWF STATE, W	    ; Restar la literal a la variable STATE
    BTFSS STATUS, 2	    ; Revisar si el resultado de la resta es 0
    goto S2_reg_values
			    ; SÍ: enviar los valores de las variables de S1 a las variables del modo edit
    MOV_REG_SETS S1_DAYS_UNITS, EDIT_REG_1, S1_DAYS_DECS, EDIT_REG_2, S1_MONTHS_UNITS, EDIT_REG_3, S1_MONTHS_DECS, EDIT_REG_4
    return
    
    S2_reg_values:
    MOVLW 2		    ; NO: Revisión de si el estado actual es 2 - mover 2 a W
    SUBWF STATE, W	    ; Restar la literal a la variable STATE
    BTFSS STATUS, 2	    ; Revisar si el resultado de la resta es 0
    return
			    ; SÍ: enviar los valores de las variables de S2 a las variables del modo edit
    MOV_REG_SETS S2_SECS_UNITS, EDIT_REG_1, S2_SECS_DECS, EDIT_REG_2, S2_MINS_UNITS, EDIT_REG_3, S2_MINS_DECS, EDIT_REG_4
    return

ret_reg_values:
    MOVLW 0		    ; Revisión de si el estado actual es 0 - mover 0 a W
    SUBWF STATE, W	    ; Restar la literal a la variable STATE
    BTFSS STATUS, 2	    ; Revisar si el resultado de la resta es 0
    goto S1_ret_values
			    ; SÍ: enviar los valores de las variables del modo edit a las variables de S0 
    MOV_REG_SETS EDIT_REG_1, S0_MINS_UNITS, EDIT_REG_2, S0_MINS_DECS, EDIT_REG_3, S0_HRS_UNITS, EDIT_REG_4, S0_HRS_DECS      
    return
    
    S1_ret_values:
    MOVLW 1		    ; NO: Revisión de si el estado actual es 1 - mover 1 a W
    SUBWF STATE, W	    ; Restar la literal a la variable STATE
    BTFSS STATUS, 2	    ; Revisar si el resultado de la resta es 0
    goto S2_ret_values
			    ; SÍ: enviar los valores de las variables del modo edit a las variables de S0 
    MOV_REG_SETS EDIT_REG_1, S1_DAYS_UNITS, EDIT_REG_2, S1_DAYS_DECS, EDIT_REG_3, S1_MONTHS_UNITS, EDIT_REG_4, S1_MONTHS_DECS  
    return
    
    S2_ret_values:
    MOVLW 2		    ; NO: Revisión de si el estado actual es 2 - mover 2 a W
    SUBWF STATE, W	    ; Restar la literal a la variable STATE
    BTFSS STATUS, 2	    ; Revisar si el resultado de la resta es 0
    return
			    ; SÍ: enviar los valores de las variables del modo edit a las variables de S0 
    MOV_REG_SETS EDIT_REG_1, S2_SECS_UNITS, EDIT_REG_2, S2_SECS_DECS, EDIT_REG_3, S2_MINS_UNITS, EDIT_REG_4, S2_MINS_DECS  
    return    

int_TMR0:
    call display_selection	; llamamos a multiplexar
    RESTART_TMR0 6		; reiniciamos el TIMER0
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
    MOVF    display_val+1, W	; Movemos display+2 a W
    MOVWF   PORTC		; Movemos Valor de tabla a PORTC
    BSF	PORTD, 1		; Encendemos display de nibble alto
    CLRF flags			; Cambiamos bandera para cambiar el otro display en la siguiente interrupción
    BSF  flags, 2
    return
    
display_2:
    MOVF    display_val+2, W	; Movemos display+3 a W
    MOVWF   PORTC		; Movemos Valor de tabla a PORTC
    BSF	PORTD, 2		; Encendemos display de nibble alto
    CLRF flags			; Cambiamos bandera para cambiar el otro display en la siguiente interrupción
    BSF  flags, 3
    return
    

display_3:
    MOVF    display_val+3, W	; Movemos display+4 a W
    MOVWF   PORTC		; Movemos Valor de tabla a PORTC
    BSF	PORTD, 3		; Encendemos display de nibble alto
    CLRF flags			; Cambiamos bandera para cambiar el otro display en la siguiente interrupción
    BSF  flags, 0
    return
    
int_TMR1:    
    INCF S0_SECS		; incrementamos la variable de segundos del modo hora
    MOVLW 1			; Si la bandera de iniciar está activada, decrementar 
    SUBWF TIMER_START, W	; la variable segundos del modo timer
    BTFSC STATUS, 2
    call dec_timer
    MOVLW 1			; verificamos si la bandera de time out está activada
    SUBWF TIME_OUT, W
    BTFSC STATUS, 2
    call time_out_count
    RESTART_TMR1 0x0B, 0xDC	; reiniciamos el TIMER1
    return
    
dec_timer:
    DECF S2_SECS_UNITS
    
    MOVLW 0			; verificamos si la cuenta regresiva en las variables del timer ha llegado a cero
    SUBWF S2_SECS_UNITS, W
    BTFSS STATUS, 2
    return
    MOVLW 0			
    SUBWF S2_SECS_DECS, W
    BTFSS STATUS, 2
    return
    MOVLW 0
    SUBWF S2_MINS_UNITS, W
    BTFSS STATUS, 2
    return
    MOVLW 0
    SUBWF S2_MINS_DECS, W
    BTFSS STATUS, 2
    return
    BCF TIMER_START, 0		; si todas las variables del timer han llegado a 0, habilitar la bandera de
    BSF TIME_OUT, 0		; timeout, deshabilitar la bandera de start y
    BSF PORTA, 4		; habilitar la led de alarma
    return	

time_out_count:
    INCF TIME_OUT_SECS		; Si la bandera de time out está activada, incrementar segundos de timeout
    MOVLW 60			; Si ya han transcurrido 60 segundos de timeout, apagar la led de alarma
    SUBWF TIME_OUT_SECS, W	; e incrementar en uno la cantidad de segundos transcurridos
    BTFSS STATUS, 2
    return
    BCF PORTA, 4
    BCF TIME_OUT, 0
    CLRF TIME_OUT_SECS
    INCF S2_SECS_UNITS
    return

int_TMR2:
    call change_led_1
    call change_led_2
   
    BCF TMR2IF
    return
    
change_led_1:
    BTFSC PORTA, 5
    goto $+3
    BSF PORTA, 5
    goto $+4
    BTFSS PORTA, 5
    goto $+2
    BCF PORTA, 5
    return
    
change_led_2:
    BTFSC PORTA, 6
    goto $+3
    BSF PORTA, 6
    goto $+4
    BTFSS PORTA, 6
    goto $+2
    BCF PORTA, 6
    return
;-------------------------------------------------------------------------------
;Tabla para display de siete segmentos
;-------------------------------------------------------------------------------

PSECT table, class = CODE, abs, delta = 2
ORG 100h 

table:
    CLRF PCLATH
    BSF PCLATH, 0           ; PCLATH en 01
    ANDLW 0x0F
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
    
    INCF TRANSC_MONTHS	    ; empezamos en el mes 1 (enero)
    INCF S2_SECS_UNITS   ; empezamos la cuenta del timer en 1 segundo

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
    call S1_date_check	    ; Control de la fecha en las variables del modo fecha
    call S2_timer_check	    ; control del tiempo en las variables del modo timer
    call display_state_show ; Envío de literales a los displays dependiendo del modo
    call edit_reg_check	    ; Control del overflow y underflow de las variables de edición
    call state_edit_leds    ; Control sobre la activación de leds acorde al modo y estado
    
    
    //MOVF TRANSC_MONTHS, W
    //MOVWF PORTA
    goto loop
    
    
;-------------------------------------------------------------------------------
;subrutinas 
;-------------------------------------------------------------------------------
  
S0_time_check:
    
    COUNT_RESET_INC 60, S0_SECS, S0_MINS_UNITS
    COUNT_RESET_INC 10, S0_MINS_UNITS, S0_MINS_DECS
    COUNT_RESET_INC 6, S0_MINS_DECS, S0_HRS_UNITS
    OVRFLW_COUNT_RESET_INC 2, 4, S0_HRS_DECS, S0_HRS_UNITS, S1_DAYS_UNITS
    COUNT_RESET_INC 10, S0_HRS_UNITS, S0_HRS_DECS
    
    return

S1_date_check:
    call months_transc_limits
    call date_limits
    
    ; Se efectúa una resta para verificar el mes actual y así efectuar el overflow 
    ;adecuado respecto a la cantidad de días del mes
    
    MOVLW 1				; Enero	
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Febrero
    MOV_VALS_REGS  0, 1, S1_MONTHS_DECS, S1_MONTHS_UNITS
    OVRFLW_COUNT_RESET_INC 3, 2, S1_DAYS_DECS, S1_DAYS_UNITS, TRANSC_MONTHS
    
    Febrero:
    MOVLW 2
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Marzo
    MOV_VALS_REGS 0, 2, S1_MONTHS_DECS, S1_MONTHS_UNITS
    OVRFLW_COUNT_RESET_INC 2, 9, S1_DAYS_DECS, S1_DAYS_UNITS, TRANSC_MONTHS
    
    
    Marzo:
    MOVLW 3
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Abril
    MOV_VALS_REGS 0, 3, S1_MONTHS_DECS, S1_MONTHS_UNITS
    OVRFLW_COUNT_RESET_INC 3, 2, S1_DAYS_DECS, S1_DAYS_UNITS, TRANSC_MONTHS
    
    Abril:
    MOVLW 4
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Mayo
    MOV_VALS_REGS 0, 4, S1_MONTHS_DECS, S1_MONTHS_UNITS
    OVRFLW_COUNT_RESET_INC 3, 1, S1_DAYS_DECS, S1_DAYS_UNITS, TRANSC_MONTHS
    
    Mayo:
    MOVLW 5
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Junio
    MOV_VALS_REGS 0, 5, S1_MONTHS_DECS, S1_MONTHS_UNITS
    OVRFLW_COUNT_RESET_INC 3, 2, S1_DAYS_DECS, S1_DAYS_UNITS, TRANSC_MONTHS
    
    Junio:
    MOVLW 6
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Julio
    MOV_VALS_REGS 0, 6, S1_MONTHS_DECS, S1_MONTHS_UNITS
    OVRFLW_COUNT_RESET_INC 3, 1, S1_DAYS_DECS, S1_DAYS_UNITS, TRANSC_MONTHS
    
    Julio:
    MOVLW 7
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Agosto
    MOV_VALS_REGS 0, 7, S1_MONTHS_DECS, S1_MONTHS_UNITS
    OVRFLW_COUNT_RESET_INC 3, 2, S1_DAYS_DECS, S1_DAYS_UNITS, TRANSC_MONTHS
    
    Agosto:
    MOVLW 8
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Septiembre
    MOV_VALS_REGS 0, 8, S1_MONTHS_DECS, S1_MONTHS_UNITS
    OVRFLW_COUNT_RESET_INC 3, 2, S1_DAYS_DECS, S1_DAYS_UNITS, TRANSC_MONTHS
    
    Septiembre:
    MOVLW 9
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Octubre
    MOV_VALS_REGS 0, 9, S1_MONTHS_DECS, S1_MONTHS_UNITS
    OVRFLW_COUNT_RESET_INC 3, 1, S1_DAYS_DECS, S1_DAYS_UNITS, TRANSC_MONTHS
    
    Octubre:
    MOVLW 10
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Noviembre
    MOV_VALS_REGS 1, 0, S1_MONTHS_DECS, S1_MONTHS_UNITS
    OVRFLW_COUNT_RESET_INC 3, 2, S1_DAYS_DECS, S1_DAYS_UNITS, TRANSC_MONTHS
    
    Noviembre:
    MOVLW 11
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Diciembre
    MOV_VALS_REGS 1, 1, S1_MONTHS_DECS, S1_MONTHS_UNITS
    OVRFLW_COUNT_RESET_INC 3, 1, S1_DAYS_DECS, S1_DAYS_UNITS, TRANSC_MONTHS
    
    Diciembre:
    MOVLW 12
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto $+3
    MOV_VALS_REGS 1, 2, S1_MONTHS_DECS, S1_MONTHS_UNITS
    OVRFLW_COUNT_RESET_INC 3, 2, S1_DAYS_DECS, S1_DAYS_UNITS, TRANSC_MONTHS
    return

months_transc_limits:		; Regresar al mes 1 después de alcanzar el mes 13
    MOVLW 13
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    return
    MOVLW 1
    MOVWF TRANSC_MONTHS
    return
    
date_limits:			; Empezar los días de cada mes en 1 y aumentar una decena cada 10 unidades
    INC_REG_TWO_LIMITS 0, 0, S1_DAYS_DECS, S1_DAYS_UNITS, S1_DAYS_UNITS
 
    COUNT_RESET_INC 10, S1_DAYS_UNITS, S1_DAYS_DECS
    COUNT_RESET 6, S1_DAYS_DECS
    return

S2_timer_check:
    UNDRFLW_RESET_DEC 0xFF, 9, S2_SECS_UNITS,  S2_SECS_DECS
    UNDRFLW_RESET_DEC 0xFF, 5, S2_SECS_DECS,  S2_MINS_UNITS
    UNDRFLW_RESET_DEC 0xFF, 9, S2_MINS_UNITS,  S2_MINS_DECS
    UNDRFLW_RESET  0xFF, 9, S2_MINS_DECS
    return
    
edit_reg_check:
    MOVLW 0		    ; Revisión de si el estado actual es 0 - mover 0 a W
    SUBWF STATE, W	    ; Restar la literal a la variable STATE
    BTFSC STATUS, 2	    ; Revisar si el resultado de la resta es 0
    call S0_edit_limits	    ; SÍ: verificar los límites de las variables de edición durante el estado S0
    
    MOVLW 1		    ; Revisión de si el estado actual es 1 - mover 1 a W
    SUBWF STATE, W	    ; Restar la literal a la variable STATE
    BTFSC STATUS, 2	    ; Revisar si el resultado de la resta es 0
    call S1_edit_limits	    ; SÍ: verificar los límites de las variables de edición durante el estado S1
    
    MOVLW 2		    ; Revisión de si el estado actual es 2 - mover 2 a W
    SUBWF STATE, W	    ; Restar la literal a la variable STATE
    BTFSC STATUS, 2	    ; Revisar si el resultado de la resta es 0
    call S2_edit_limits	    ; SÍ: verificar los límites de las variables de edición durante el estado S1
    return
    
S0_edit_limits:		    ; Revisión de límites superiores e inferiores de S0
    call S0_edit_limits_up
    call S0_edit_limits_down
    return
    
S0_edit_limits_up:	    ; Valores máximos del modo hora
    COUNT_RESET_INC 10, EDIT_REG_1, EDIT_REG_2
    COUNT_RESET_INC 6, EDIT_REG_2, EDIT_REG_3
    OVRFLW_COUNT_RESET 2, 4, EDIT_REG_4, EDIT_REG_3
    COUNT_RESET_INC 10, EDIT_REG_3, EDIT_REG_4
    return
    
S0_edit_limits_down:	    ; Valores mínimos del modo hora
    UNDRFLW_RESET_DEC 0xFF, 9, EDIT_REG_1,  EDIT_REG_2
    UNDRFLW_RESET_DEC 0xFF, 5, EDIT_REG_2,  EDIT_REG_3
    UNDRFLW_RESET_DEC 0xFF, 3, EDIT_REG_3,  EDIT_REG_4
    UNDRFLW_RESET  0xFF, 2, EDIT_REG_4
    return
    
S1_edit_limits:		    ; Revisión de límites superiores e inferiores de S1
    call S1_edit_limits_up
    call S1_edit_limits_down
    return
    
S1_edit_limits_up:
    call months_transc_limits	; Empezar en el mes 1 al llegar al mes 13
    call date_limits_edit	; Indicar límites máximos en el modo fecha
    
    ; Se efectúa una resta para verificar el mes actual y así efectuar el overflow 
    ;adecuado respecto a la cantidad de días del mes
    MOVLW 1				; Enero	
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Febrero_up
    MOV_VALS_REGS  0, 1, EDIT_REG_4, EDIT_REG_3
    OVRFLW_COUNT_RESET_INC 3, 2, EDIT_REG_2, EDIT_REG_1, TRANSC_MONTHS
    INC_REG_TWO_LIMITS 0, 0, EDIT_REG_2, EDIT_REG_1, EDIT_REG_1
    
    Febrero_up:
    MOVLW 2
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Marzo_up
    MOV_VALS_REGS 0, 2, EDIT_REG_4, EDIT_REG_3
    OVRFLW_COUNT_RESET_INC 2, 9, EDIT_REG_2, EDIT_REG_1, TRANSC_MONTHS
    INC_REG_TWO_LIMITS 0, 0, EDIT_REG_2, EDIT_REG_1, EDIT_REG_1
    
    Marzo_up:
    MOVLW 3
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Abril_up
    MOV_VALS_REGS 0, 3, EDIT_REG_4, EDIT_REG_3
    OVRFLW_COUNT_RESET_INC 3, 2, EDIT_REG_2, EDIT_REG_1, TRANSC_MONTHS
    INC_REG_TWO_LIMITS 0, 0, EDIT_REG_2, EDIT_REG_1, EDIT_REG_1
    
    Abril_up:
    MOVLW 4
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Mayo_up
    MOV_VALS_REGS 0, 4, EDIT_REG_4, EDIT_REG_3
    OVRFLW_COUNT_RESET_INC 3, 1, EDIT_REG_2, EDIT_REG_1, TRANSC_MONTHS
    INC_REG_TWO_LIMITS 0, 0, EDIT_REG_2, EDIT_REG_1, EDIT_REG_1
    
    Mayo_up:
    MOVLW 5
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Junio_up
    MOV_VALS_REGS 0, 5, EDIT_REG_4, EDIT_REG_3
    OVRFLW_COUNT_RESET_INC 3, 2, EDIT_REG_2, EDIT_REG_1, TRANSC_MONTHS
    INC_REG_TWO_LIMITS 0, 0, EDIT_REG_2, EDIT_REG_1, EDIT_REG_1
    
    Junio_up:
    MOVLW 6
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Julio_up
    MOV_VALS_REGS 0, 6, EDIT_REG_4, EDIT_REG_3
    OVRFLW_COUNT_RESET_INC 3, 1, EDIT_REG_2,EDIT_REG_1, TRANSC_MONTHS
    INC_REG_TWO_LIMITS 0, 0, EDIT_REG_2, EDIT_REG_1, EDIT_REG_1
    
    Julio_up:
    MOVLW 7
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Agosto_up
    MOV_VALS_REGS 0, 7, EDIT_REG_4, EDIT_REG_3
    OVRFLW_COUNT_RESET_INC 3, 2, EDIT_REG_2, EDIT_REG_1, TRANSC_MONTHS
    INC_REG_TWO_LIMITS 0, 0, EDIT_REG_2, EDIT_REG_1, EDIT_REG_1
    
    Agosto_up:
    MOVLW 8
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Septiembre_up
    MOV_VALS_REGS 0, 8, EDIT_REG_4, EDIT_REG_3
    OVRFLW_COUNT_RESET_INC 3, 2, EDIT_REG_2, EDIT_REG_1, TRANSC_MONTHS
    INC_REG_TWO_LIMITS 0, 0, EDIT_REG_2, EDIT_REG_1, EDIT_REG_1
    
    Septiembre_up:
    MOVLW 9
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Octubre_up
    MOV_VALS_REGS 0, 9, EDIT_REG_4, EDIT_REG_3
    OVRFLW_COUNT_RESET_INC 3, 1, EDIT_REG_2, EDIT_REG_1, TRANSC_MONTHS
    INC_REG_TWO_LIMITS 0, 0, EDIT_REG_2, EDIT_REG_1, EDIT_REG_1
    
    Octubre_up:
    MOVLW 10
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Noviembre_up
    MOV_VALS_REGS 1, 0, EDIT_REG_4, EDIT_REG_3
    OVRFLW_COUNT_RESET_INC 3, 2, EDIT_REG_2, EDIT_REG_1, TRANSC_MONTHS
    INC_REG_TWO_LIMITS 0, 0, EDIT_REG_2, EDIT_REG_1, EDIT_REG_1
    
    Noviembre_up:
    MOVLW 11
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Diciembre_up
    MOV_VALS_REGS 1, 1, EDIT_REG_4, EDIT_REG_3
    OVRFLW_COUNT_RESET_INC 3, 1, EDIT_REG_2, EDIT_REG_1, TRANSC_MONTHS
    INC_REG_TWO_LIMITS 0, 0, EDIT_REG_2, EDIT_REG_1, EDIT_REG_1
    
    Diciembre_up:
    MOVLW 12
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto $+4
    MOV_VALS_REGS 1, 2, EDIT_REG_4, EDIT_REG_3
    OVRFLW_COUNT_RESET_INC 3, 2, EDIT_REG_2, EDIT_REG_1, TRANSC_MONTHS
    INC_REG_TWO_LIMITS 0, 0, EDIT_REG_2, EDIT_REG_1, EDIT_REG_1
    return
    
    
date_limits_edit:    
    call initial_day_value  ; hacemos que los registros de datos temporales cuenten desde el día 1
    COUNT_RESET_INC 10, EDIT_REG_1, EDIT_REG_2
    COUNT_RESET 6, EDIT_REG_2
     
    return
initial_day_value:
    INC_REG_TWO_LIMITS 0, 0, EDIT_REG_2, EDIT_REG_1, EDIT_REG_1
    return
    
S1_edit_limits_down:
    call months_transc_undrflw	; si se llega al valor de mes 0xFF, utilizar el mes 12
    call date_undrflw_limit
    
    ; Se efectúa una resta para verificar el mes actual y así efectuar el underflow
    ;adecuado respecto a la cantidad de días del mes
    MOVLW 1				; Enero	
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Febrero_down
    MOV_VALS_REGS  0, 1, EDIT_REG_4, EDIT_REG_3
    ZERO_DOUBLE_UNDRFLW_DEC 3, 1, EDIT_REG_2, EDIT_REG_1, TRANSC_MONTHS
    
    Febrero_down:
    MOVLW 2				 	
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Marzo_down
    MOV_VALS_REGS  0, 2, EDIT_REG_4, EDIT_REG_3
    ZERO_DOUBLE_UNDRFLW_DEC 3, 1, EDIT_REG_2, EDIT_REG_1, TRANSC_MONTHS
    return
    
    Marzo_down:
    MOVLW 3				 	
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Abril_down
    MOV_VALS_REGS  0, 3, EDIT_REG_4, EDIT_REG_3
    ZERO_DOUBLE_UNDRFLW_DEC 2, 8, EDIT_REG_2, EDIT_REG_1, TRANSC_MONTHS
    return
    
    Abril_down:
    MOVLW 4				 	
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Mayo_down
    MOV_VALS_REGS  0, 4, EDIT_REG_4, EDIT_REG_3
    ZERO_DOUBLE_UNDRFLW_DEC 3, 1, EDIT_REG_2, EDIT_REG_1, TRANSC_MONTHS
    return
    
    Mayo_down:
    MOVLW 5				 	
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Junio_down
    MOV_VALS_REGS  0, 5, EDIT_REG_4, EDIT_REG_3
    ZERO_DOUBLE_UNDRFLW_DEC 3, 0, EDIT_REG_2, EDIT_REG_1, TRANSC_MONTHS
    return
    
    Junio_down:
    MOVLW 6				 	
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Julio_down
    MOV_VALS_REGS  0, 6, EDIT_REG_4, EDIT_REG_3
    ZERO_DOUBLE_UNDRFLW_DEC 3, 1, EDIT_REG_2, EDIT_REG_1, TRANSC_MONTHS
    return
    
    Julio_down:
    MOVLW 7				 	
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Agosto_down
    MOV_VALS_REGS  0, 7, EDIT_REG_4, EDIT_REG_3
    ZERO_DOUBLE_UNDRFLW_DEC 3, 0, EDIT_REG_2, EDIT_REG_1, TRANSC_MONTHS
    return
    
    Agosto_down:
    MOVLW 8				 	
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Septiembre_down
    MOV_VALS_REGS  0, 8, EDIT_REG_4, EDIT_REG_3
    ZERO_DOUBLE_UNDRFLW_DEC 3, 1, EDIT_REG_2, EDIT_REG_1, TRANSC_MONTHS
    return
    
    Septiembre_down:
    MOVLW 9				 	
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Octubre_down
    MOV_VALS_REGS  0, 9, EDIT_REG_4, EDIT_REG_3
    ZERO_DOUBLE_UNDRFLW_DEC 3, 1, EDIT_REG_2, EDIT_REG_1, TRANSC_MONTHS
    return
    
    Octubre_down:
    MOVLW 10				 	
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Noviembre_down
    MOV_VALS_REGS  1, 0, EDIT_REG_4, EDIT_REG_3
    ZERO_DOUBLE_UNDRFLW_DEC 3, 0, EDIT_REG_2, EDIT_REG_1, TRANSC_MONTHS
    return
    
    Noviembre_down:
    MOVLW 11				 	
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto Diciembre_down
    MOV_VALS_REGS  1, 1, EDIT_REG_4, EDIT_REG_3
    ZERO_DOUBLE_UNDRFLW_DEC 3, 1, EDIT_REG_2, EDIT_REG_1, TRANSC_MONTHS
    return
    
    Diciembre_down:
    MOVLW 12				 	
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    goto $+3
    MOV_VALS_REGS  1, 2, EDIT_REG_4, EDIT_REG_3
    ZERO_DOUBLE_UNDRFLW_DEC 3, 0, EDIT_REG_2, EDIT_REG_1, TRANSC_MONTHS
    return
    
months_transc_undrflw:
    MOVLW 0
    SUBWF TRANSC_MONTHS, W
    BTFSS STATUS, 2
    return
    MOVLW 12
    MOVWF TRANSC_MONTHS
    return
    
date_undrflw_limit:	; Contar desde 9 cuando el valor del día baja de 0
    UNDRFLW_RESET_DEC 0xFF, 9, EDIT_REG_1, EDIT_REG_2
    return
    
S2_edit_limits:
    call S2_edit_limits_up
    call S2_edit_limits_down
    return
    
S2_edit_limits_up:	    ; Valores máximos del modo timer
    COUNT_RESET_INC 10, EDIT_REG_1, EDIT_REG_2
    COUNT_RESET_INC 6, EDIT_REG_2, EDIT_REG_3
    OVRFLW_COUNT_RESET_INC 9, 10, EDIT_REG_4, EDIT_REG_3, EDIT_REG_1
    COUNT_RESET_INC 10, EDIT_REG_3, EDIT_REG_4
    return
    
S2_edit_limits_down:	    ; Valores mínimos del modo timer
    call S2_undrflw_limit
    UNDRFLW_RESET_DEC 0xFF, 9, EDIT_REG_1,  EDIT_REG_2
    UNDRFLW_RESET_DEC 0xFF, 5, EDIT_REG_2,  EDIT_REG_3
    UNDRFLW_RESET_DEC 0xFF, 9, EDIT_REG_3,  EDIT_REG_4
    UNDRFLW_RESET  0xFF, 9, EDIT_REG_4
    return
    
S2_undrflw_limit:
    ZERO_DOUBLE_UNDRFLW_DEC  5,9, EDIT_REG_2, EDIT_REG_1, EDIT_REG_3
    UNDRFLW_RESET_DEC 0xFF, 9, EDIT_REG_3,  EDIT_REG_4
    UNDRFLW_RESET  0xFF, 9, EDIT_REG_4
    return
    
display_state_show:
    MOVLW 0		    ; Revisión de si el estado actual es 0 - mover 0 a W
    SUBWF STATE, W	    ; Restar la literal a la variable STATE
    BTFSC STATUS, 2	    ; Revisar si el resultado de la resta es 0
    call S0_edition_mode    ; SÍ: enviar los valores de los displays de S0 en base al estado de EDIT	
    MOVLW 1		    ; NO: verificar si el estado actual es 1 - mover 1 a W
    SUBWF STATE, W	    ; Restar la literal a la variable STATE
    BTFSC STATUS, 2	    ; Revisar si el resultado de la resta es 0
    call S1_edition_mode    ; SÍ: enviar los valores de los displays de S1 en base al estado de EDIT
    MOVLW 2		    ; NO: verificar si el estado actual es 2 - mover 2 a W
    SUBWF STATE, W	    ; Restar la literal a la variable STATE
    BTFSC STATUS, 2	    ; Revisar si el resultado de la resta es 2
    call S2_edition_mode    ; SÍ: enviar los valores de los displays de S2 en base al estado de EDIT
    return

S0_edition_mode:
    BTFSC EDIT, 0   ; verificar si estamos en el modo edición o modo hora
    goto S0_edit
    goto S0_hrs
    S0_edit:	    
    DISPLAY_SHOW_STATE EDIT_REG_1, EDIT_REG_2, EDIT_REG_3, EDIT_REG_4 
    return
    S0_hrs:
    DISPLAY_SHOW_STATE S0_MINS_UNITS, S0_MINS_DECS, S0_HRS_UNITS, S0_HRS_DECS 
    return

S1_edition_mode:
    BTFSC EDIT, 0   ; verificar si estamos en el modo edición o modo fecha
    goto S1_edit
    goto S1_date
    S1_edit:
    DISPLAY_SHOW_STATE EDIT_REG_3, EDIT_REG_4, EDIT_REG_1, EDIT_REG_2 
    return
    S1_date:
    DISPLAY_SHOW_STATE S1_MONTHS_UNITS, S1_MONTHS_DECS, S1_DAYS_UNITS, S1_DAYS_DECS    
    return

S2_edition_mode:
    BTFSC EDIT, 0   ; verificar si estamos en el modo edición o modo timer
    goto S2_edit
    goto S2_timer
    S2_edit:
    DISPLAY_SHOW_STATE EDIT_REG_1, EDIT_REG_2, EDIT_REG_3, EDIT_REG_4 
    return
    S2_timer:
    DISPLAY_SHOW_STATE S2_SECS_UNITS, S2_SECS_DECS, S2_MINS_UNITS, S2_MINS_DECS        
    return
    
state_edit_leds:
    call state_leds
    call edit_led
    return

state_leds:

    MOVLW 0
    SUBWF STATE, W
    BTFSS STATUS, 2
    goto S1_led
    BCF PORTA, 0
    BSF PORTA, 2
    return
    
    S1_led:
    MOVLW 1
    SUBWF STATE, W
    BTFSS STATUS, 2
    goto S2_led
    BCF PORTA, 2
    BSF PORTA, 1
    return
    
    S2_led:
    MOVLW 2
    SUBWF STATE, W
    BTFSS STATUS, 2
    return
    BCF PORTA, 1
    BSF PORTA, 0   
    return
edit_led:
    btfss EDIT, 0	    ; Verificar si el modo edición está activado, y encender una led en ese caso
    goto $+2
    goto $+3
    BCF PORTA, 3
    goto $+2
    BSF PORTA, 3
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
    MOVLW   244		    ; Valor para interrupciones cada 500ms
    MOVWF   PR2		    ; Cargamos literal a PR2
    
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
    BSF TMR2IE		    ; Habilitar interrupción del TIMER2
    
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
    CLRF nibbles
    CLRF display_val    
    CLRF STATE
    CLRF EDIT
    CLRF TOOGLE
    CLRF EDIT_REG_1
    CLRF EDIT_REG_2
    CLRF EDIT_REG_3
    CLRF EDIT_REG_4
    
    CLRF S0_SECS
    CLRF S0_MINS_UNITS
    CLRF S0_MINS_DECS
    CLRF S0_HRS_UNITS
    CLRF S0_HRS_DECS
    
    CLRF S1_DAYS_UNITS	
    CLRF S1_DAYS_DECS	
    CLRF S1_MONTHS_UNITS	
    CLRF S1_MONTHS_DECS	
    
    CLRF TRANSC_MONTHS
    
    CLRF S2_SECS_UNITS
    CLRF S2_SECS_DECS
    CLRF S2_MINS_UNITS
    CLRF S2_MINS_DECS
    CLRF TIME_OUT
    CLRF TIME_OUT_SECS	
    CLRF TIMER_START


    return
END