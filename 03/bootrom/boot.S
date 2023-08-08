.section .text
.globl _start

_start:
    ldr r0, =0x101f1000
    mov r1, #0x30
_welcome:
    str r1, [r0]
    add r1, r1, #1
    and r1, r1, #0x37
    b _welcome
_init:
    b _reset_handler
    b . /* 0x4  Undefined Instruction */
    b . /* Software Interrupt */
    b .  /* 0xC  Prefetch Abort */
    b . /* 0x10 Data Abort */
    b . /* 0x14 Reserved */
    b . /* 0x18 IRQ */
    b . /* 0x1C FIQ */

_reset_handler:
	b .

.section .data
    welcome: .ascii  "moloch moloch\n\0"
    len = .-welcome
