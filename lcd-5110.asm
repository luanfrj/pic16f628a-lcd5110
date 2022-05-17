;-------------------------------------------------------------------
;                           BLINK
;-------------------------------------------------------------------
;AUTHOR: LUAN FERREIRA REIS DE JESUS       LAST REVISION: 14/05/2022
;V 1.0.0
;-------------------------------------------------------------------
;                        DESCRIPTION
;-------------------------------------------------------------------
; Código para interfacear o display LCD 5110
;-------------------------------------------------------------------
;                     DEFINITION FILES
;-------------------------------------------------------------------
#INCLUDE <p16f628a.inc>
    ERRORLEVEL -302

#DEFINE RST	0x01
#DEFINE CE	0x02
#DEFINE DC	0x04
#DEFINE DIN	0x08
#DEFINE CLK	0x10
#DEFINE BL	0x20

;-------------------------------------------------------------------
; DEVICE CONFIGURATION
;-------------------------------------------------------------------
    __CONFIG _CONFIG, _LVP_OFF & _BODEN_OFF & _WDTE_OFF & _PWRTE_ON & _INTRC_OSC_NOCLKOUT

;-------------------------------------------------------------------
; VARIABLES
;-------------------------------------------------------------------
    CBLOCK 0x20
        CONT1
        CONT2
        CONT3
        CONT4
        DATA_BYTE
        X_ADDR
        Y_ADDR
        CHAR_VALUE
        PTRH
        PTRL
        mulcnd
        mulplr
        H_byte
        L_byte
        count
    ENDC

;-------------------------------------------------------------------
; RESET VECTOR
;-------------------------------------------------------------------
    ORG     0x0000          ; Initial Address
    NOP
    GOTO    INICIO

;-------------------------------------------------------------------
; SUBSOUTINES
;-------------------------------------------------------------------
DELAY_MS
    ;49993 CYCLES
    BANKSEL CONT1
    MOVLW   0x0E
    MOVWF   CONT1
    MOVLW   0x28
    MOVWF   CONT2
DELAY_MS_0
    DECFSZ  CONT1, F
    GOTO    $+2
    DECFSZ  CONT2, F
    GOTO    DELAY_MS_0

    ;3 CYCLES
    GOTO    $+1
    NOP

    ;4 CYCLES (INCLUDING CALL)
    RETURN

SEND_BYTE
    MOVLW   D'09'
    MOVWF   CONT3
S0
    DECFSZ  CONT3, 1
    GOTO    S1
    GOTO    END_SEND_BYTE
S1
    MOVLW   0xFF & ~CLK
    ANDWF   PORTB, 1

    BTFSC   DATA_BYTE, 7
    GOTO    SEND_1
    MOVLW   0xFF & ~DIN
    ANDWF   PORTB, 1
    GOTO    ROTATE_BYTE
SEND_1
    MOVLW   DIN
    IORWF   PORTB, 1
ROTATE_BYTE
    NOP
    MOVLW   CLK
    IORWF   PORTB, 1
    RLF     DATA_BYTE, 1
    GOTO    S0
END_SEND_BYTE
    MOVLW   0xFF & ~CLK
    ANDWF   PORTB, 1
    RETURN

SEND_DATA
    MOVLW   DC
    IORWF   PORTB, 1
    CALL    SEND_BYTE
    RETURN

SEND_CMD
    MOVLW   0xFF & ~DC
    ANDWF   PORTB, 1
    CALL    SEND_BYTE
    RETURN

INIT_LCD
    BANKSEL PORTB
    MOVLW   B'00111111'
    MOVWF   PORTB

    MOVLW   B'00000111'
    MOVWF   PORTB

    CALL    DELAY_MS

    ; RESTART LCD
    MOVLW   0xFF & ~RST
    ANDWF   PORTB, 1
    NOP
    MOVLW   RST
    IORWF   PORTB, 1

    ; START
    MOVLW   0xFF & ~CE
    ANDWF   PORTB, 1

    MOVLW   0x21
    MOVWF   DATA_BYTE
    CALL    SEND_CMD
    MOVLW   0x99
    MOVWF   DATA_BYTE
    CALL    SEND_CMD
    MOVLW   0x20
    MOVWF   DATA_BYTE
    CALL    SEND_CMD
    MOVLW   0x0C
    MOVWF   DATA_BYTE
    CALL    SEND_CMD

    ;TURN ON BACKLIGHT
    MOVLW   BL
    IORWF   PORTB, 1

    RETURN

SET_CURSOR
    MOVLW   0x80
    IORWF   X_ADDR, 1
    MOVLW   0x40
    IORWF   Y_ADDR, 1

    MOVLW   0x20
    MOVWF   DATA_BYTE
    CALL    SEND_CMD

    MOVF    X_ADDR, 0
    MOVWF   DATA_BYTE
    CALL    SEND_CMD

    MOVF    Y_ADDR, 0
    MOVWF   DATA_BYTE
    CALL    SEND_CMD

    RETURN

; Variables:
; mulcnd - 8 bit multiplicand
; mulplr - 8 bit multiplier
; H_byte - High byte of the 16 bit result
; L_byte - Low byte of the 16 bit result
; count - loop counter
;
; ***************************** Begin Multiplier Routine
mpy_S 
    clrf H_byte     ; start with result = 0
    clrf L_byte
    movlw 8         ; count = 8
    movwf count
    movf mulcnd,w   ; multiplicand in W
    bcf STATUS,C    ; and carry clear
loop 
    rrf mulplr,f    ; right shift multiplier
    btfsc STATUS,C  ; if low-order bit of multiplier was set
    addwf H_byte,f  ; add multiplicand to MSB of result
    rrf H_byte,f    ; right shift result
    rrf L_byte,f
    decfsz count,f  ; repeat for all 8 bits
    goto loop
    return

PRINT_CHAR
    MOVLW   0x20
    SUBWF   CHAR_VALUE, 1

    MOVLW   0x05
    MOVWF   mulplr
    MOVF    CHAR_VALUE, 0
    MOVWF   mulcnd

    CALL    mpy_S

    MOVLW   HIGH CHAR_TABLE
    MOVWF   PTRH
    MOVLW   LOW CHAR_TABLE
    MOVWF   PTRL

    MOVF    H_byte, 0
    ADDWF   PTRH, 1
    MOVF    L_byte, 0
    ADDWF   PTRL, 1
    BTFSC   STATUS, C
    INCF    PTRH, 1

    MOVLW   0x06
    MOVWF   CONT4
RP_PC1
    DECFSZ  CONT4, 1
    GOTO    RP_PC2
    GOTO    END_PC
RP_PC2
    CALL    GET_COLUMN

    MOVWF   DATA_BYTE
    CALL    SEND_DATA
    INCF    PTRL, 1
    BTFSC   STATUS,Z
    INCF    PTRH, 1
    GOTO    RP_PC1

END_PC
    MOVLW   0x00
    MOVWF   DATA_BYTE
    CALL    SEND_DATA
    RETURN

GET_COLUMN
    MOVF    PTRH, 0
    MOVWF   PCLATH
    MOVF    PTRL, 0
    MOVWF   PCL
    RETURN
;-------------------------------------------------------------------
; MAIN ROUTINE
;-------------------------------------------------------------------
INICIO
    BANKSEL TRISB
    MOVLW   B'11000000'
    MOVWF   TRISB
    BANKSEL PORTB

    CALL    INIT_LCD

    MOVLW   0x00
    MOVWF   X_ADDR
    MOVLW   0x00
    MOVWF   Y_ADDR
    CALL    SET_CURSOR

    MOVLW   'L'
    MOVWF   CHAR_VALUE
    CALL    PRINT_CHAR

    MOVLW   'u'
    MOVWF   CHAR_VALUE
    CALL    PRINT_CHAR

    MOVLW   'a'
    MOVWF   CHAR_VALUE
    CALL    PRINT_CHAR

    MOVLW   'n'
    MOVWF   CHAR_VALUE
    CALL    PRINT_CHAR

    MOVLW   0x02
    MOVWF   X_ADDR
    MOVLW   0x01
    MOVWF   Y_ADDR
    CALL    SET_CURSOR

    MOVLW   'L'
    MOVWF   CHAR_VALUE
    CALL    PRINT_CHAR

    MOVLW   'U'
    MOVWF   CHAR_VALUE
    CALL    PRINT_CHAR

    MOVLW   'A'
    MOVWF   CHAR_VALUE
    CALL    PRINT_CHAR

    MOVLW   'N'
    MOVWF   CHAR_VALUE
    CALL    PRINT_CHAR

    MOVLW   CE
    IORWF   PORTB, 1

REPETE
    GOTO    REPETE

CHAR_TABLE
    DT      0x00, 0x00, 0x00, 0x00, 0x00 ;20
    DT      0x00, 0x00, 0x5f, 0x00, 0x00 ;21 !
    DT      0x00, 0x07, 0x00, 0x07, 0x00 ;22 "
    DT      0x14, 0x7f, 0x14, 0x7f, 0x14 ;23 #
    DT      0x24, 0x2a, 0x7f, 0x2a, 0x12 ;24 $
    DT      0x23, 0x13, 0x08, 0x64, 0x62 ;25 %
    DT      0x36, 0x49, 0x55, 0x22, 0x50 ;26 &
    DT      0x00, 0x05, 0x03, 0x00, 0x00 ;27 '
    DT      0x00, 0x1c, 0x22, 0x41, 0x00 ;28 (
    DT      0x00, 0x41, 0x22, 0x1c, 0x00 ;29 )
    DT      0x14, 0x08, 0x3e, 0x08, 0x14 ;2a *
    DT      0x08, 0x08, 0x3e, 0x08, 0x08 ;2b +
    DT      0x00, 0x50, 0x30, 0x00, 0x00 ;2c ,
    DT      0x08, 0x08, 0x08, 0x08, 0x08 ;2d -
    DT      0x00, 0x60, 0x60, 0x00, 0x00 ;2e .
    DT      0x20, 0x10, 0x08, 0x04, 0x02 ;2f /
    DT      0x3e, 0x51, 0x49, 0x45, 0x3e ;30 0
    DT      0x00, 0x42, 0x7f, 0x40, 0x00 ;31 1
    DT      0x42, 0x61, 0x51, 0x49, 0x46 ;32 2
    DT      0x21, 0x41, 0x45, 0x4b, 0x31 ;33 3
    DT      0x18, 0x14, 0x12, 0x7f, 0x10 ;34 4
    DT      0x27, 0x45, 0x45, 0x45, 0x39 ;35 5
    DT      0x3c, 0x4a, 0x49, 0x49, 0x30 ;36 6
    DT      0x01, 0x71, 0x09, 0x05, 0x03 ;37 7
    DT      0x36, 0x49, 0x49, 0x49, 0x36 ;38 8
    DT      0x06, 0x49, 0x49, 0x29, 0x1e ;39 9
    DT      0x00, 0x36, 0x36, 0x00, 0x00 ;3a :
    DT      0x00, 0x56, 0x36, 0x00, 0x00 ;3b ;
    DT      0x08, 0x14, 0x22, 0x41, 0x00 ;3c <
    DT      0x14, 0x14, 0x14, 0x14, 0x14 ;3d =
    DT      0x00, 0x41, 0x22, 0x14, 0x08 ;3e >
    DT      0x02, 0x01, 0x51, 0x09, 0x06 ;3f ?
    DT      0x32, 0x49, 0x79, 0x41, 0x3e ;40 @
    DT      0x7e, 0x11, 0x11, 0x11, 0x7e ;41 A
    DT      0x7f, 0x49, 0x49, 0x49, 0x36 ;42 B
    DT      0x3e, 0x41, 0x41, 0x41, 0x22 ;43 C
    DT      0x7f, 0x41, 0x41, 0x22, 0x1c ;44 D
    DT      0x7f, 0x49, 0x49, 0x49, 0x41 ;45 E
    DT      0x7f, 0x09, 0x09, 0x09, 0x01 ;46 F
    DT      0x3e, 0x41, 0x49, 0x49, 0x7a ;47 G
    DT      0x7f, 0x08, 0x08, 0x08, 0x7f ;48 H
    DT      0x00, 0x41, 0x7f, 0x41, 0x00 ;49 I
    DT      0x20, 0x40, 0x41, 0x3f, 0x01 ;4a J
    DT      0x7f, 0x08, 0x14, 0x22, 0x41 ;4b K
    DT      0x7f, 0x40, 0x40, 0x40, 0x40 ;4c L
    DT      0x7f, 0x02, 0x0c, 0x02, 0x7f ;4d M
    DT      0x7f, 0x04, 0x08, 0x10, 0x7f ;4e N
    DT      0x3e, 0x41, 0x41, 0x41, 0x3e ;4f O
    DT      0x7f, 0x09, 0x09, 0x09, 0x06 ;50 P
    DT      0x3e, 0x41, 0x51, 0x21, 0x5e ;51 Q
    DT      0x7f, 0x09, 0x19, 0x29, 0x46 ;52 R
    DT      0x46, 0x49, 0x49, 0x49, 0x31 ;53 S
    DT      0x01, 0x01, 0x7f, 0x01, 0x01 ;54 T
    DT      0x3f, 0x40, 0x40, 0x40, 0x3f ;55 U
    DT      0x1f, 0x20, 0x40, 0x20, 0x1f ;56 V
    DT      0x3f, 0x40, 0x38, 0x40, 0x3f ;57 W
    DT      0x63, 0x14, 0x08, 0x14, 0x63 ;58 X
    DT      0x07, 0x08, 0x70, 0x08, 0x07 ;59 Y
    DT      0x61, 0x51, 0x49, 0x45, 0x43 ;5a Z
    DT      0x00, 0x7f, 0x41, 0x41, 0x00 ;5b [
    DT      0x02, 0x04, 0x08, 0x10, 0x20 ;5c ¥
    DT      0x00, 0x41, 0x41, 0x7f, 0x00 ;5d ]
    DT      0x04, 0x02, 0x01, 0x02, 0x04 ;5e ^
    DT      0x40, 0x40, 0x40, 0x40, 0x40 ;5f _
    DT      0x00, 0x01, 0x02, 0x04, 0x00 ;60 `
    DT      0x20, 0x54, 0x54, 0x54, 0x78 ;61 a
    DT      0x7f, 0x48, 0x44, 0x44, 0x38 ;62 b
    DT      0x38, 0x44, 0x44, 0x44, 0x20 ;63 c
    DT      0x38, 0x44, 0x44, 0x48, 0x7f ;64 d
    DT      0x38, 0x54, 0x54, 0x54, 0x18 ;65 e
    DT      0x08, 0x7e, 0x09, 0x01, 0x02 ;66 f
    DT      0x0c, 0x52, 0x52, 0x52, 0x3e ;67 g
    DT      0x7f, 0x08, 0x04, 0x04, 0x78 ;68 h
    DT      0x00, 0x44, 0x7d, 0x40, 0x00 ;69 i
    DT      0x20, 0x40, 0x44, 0x3d, 0x00 ;6a j
    DT      0x7f, 0x10, 0x28, 0x44, 0x00 ;6b k
    DT      0x00, 0x41, 0x7f, 0x40, 0x00 ;6c l
    DT      0x7c, 0x04, 0x18, 0x04, 0x78 ;6d m
    DT      0x7c, 0x08, 0x04, 0x04, 0x78 ;6e n
    DT      0x38, 0x44, 0x44, 0x44, 0x38 ;6f o
    DT      0x7c, 0x14, 0x14, 0x14, 0x08 ;70 p
    DT      0x08, 0x14, 0x14, 0x18, 0x7c ;71 q
    DT      0x7c, 0x08, 0x04, 0x04, 0x08 ;72 r
    DT      0x48, 0x54, 0x54, 0x54, 0x20 ;73 s
    DT      0x04, 0x3f, 0x44, 0x40, 0x20 ;74 t
    DT      0x3c, 0x40, 0x40, 0x20, 0x7c ;75 u
    DT      0x1c, 0x20, 0x40, 0x20, 0x1c ;76 v
    DT      0x3c, 0x40, 0x30, 0x40, 0x3c ;77 w
    DT      0x44, 0x28, 0x10, 0x28, 0x44 ;78 x
    DT      0x0c, 0x50, 0x50, 0x50, 0x3c ;79 y
    DT      0x44, 0x64, 0x54, 0x4c, 0x44 ;7a z
    DT      0x00, 0x08, 0x36, 0x41, 0x00 ;7b {
    DT      0x00, 0x00, 0x7f, 0x00, 0x00 ;7c |
    DT      0x00, 0x41, 0x36, 0x08, 0x00 ;7d }
    DT      0x10, 0x08, 0x08, 0x10, 0x08 ;7e ~
    DT      0x00, 0x06, 0x09, 0x09, 0x06 ;7f Deg Symbol

;-------------------------------------------------------------------
;                           END OF PROGRAM
;-------------------------------------------------------------------
    END