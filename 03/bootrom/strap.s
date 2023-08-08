.globl _start

_start:
ldr r0, =0x101f1000
mov r1, #0x30
top:
    str r1, [r0]
    add r1, r1, #1
    and r1, r1, #0x37
    b top
