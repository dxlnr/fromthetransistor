.global _start
.section .text

_start:
    b _reset_handler
	b .

    ; mov r0, #1
    ; ldr r1, =welcome
    ; ldr r2, =len
    ; mov r7, #4
    ; swi 0

    mov r7, #1
    swi 0

_reset_handler:
	ldr		r1, =0x09000000
	mov		r2, #0xd
	str		r2, [r1, #0x24]
	mov		r2, #0x0300
	str		r2, [r1, #0x30]

	ldr		r0, =kernel_addr
	mov		sp, r0

.section .data
    welcome: .ascii  "moloch moloch\n\0"
    len = .-welcome

