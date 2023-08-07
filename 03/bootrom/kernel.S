_main:
    mov r0, #1
    ldr r1, =welcome
    ldr r2, =len
    mov r7, #4
    swi 0

    mov r7, #1
    swi 0

.section .data
    welcome: .ascii  "moloch moloch\n\0"
    len = .-welcome
