.global _start
.section .text

_start:
    # uart 
    mov r0, #1
    ldr r1, =welcome
    ldr r2, =len
    mov r7, #4
    swi 0

    mov r7, #1
    swi 0

	; b _write_uart
    ; ldr r1, =welcome
	; b _write_uart

_write_uart:
    ldr r0, =0x09000000


.section .data
    welcome: .ascii  "moloch moloch\n\0"
    len = .-welcome
