	.arch armv5t
	.fpu softvfp
	.eabi_attribute 20, 1
	.eabi_attribute 21, 1
	.eabi_attribute 23, 3
	.eabi_attribute 24, 1
	.eabi_attribute 25, 1
	.eabi_attribute 26, 2
	.eabi_attribute 30, 6
	.eabi_attribute 34, 0
	.eabi_attribute 18, 4
	.file	"fib.c"
	.text
	.align	2
	.global	fib
	.syntax unified
	.arm
	.type	fib, %function
fib:
	@ args = 0, pretend = 0, frame = 24
	@ frame_needed = 1, uses_anonymous_args = 0
	@ link register save eliminated.
	str	fp, [sp, #-4]!
	add	fp, sp, #0
	sub	sp, sp, #28
	str	r0, [fp, #-24]
	mov	r3, #0
	str	r3, [fp, #-20]
	mov	r3, #1
	str	r3, [fp, #-16]
	ldr	r3, [fp, #-24]
	cmp	r3, #0
	bne	.L2
	ldr	r3, [fp, #-20]
	b	.L3
.L2:
	mov	r3, #2
	str	r3, [fp, #-12]
	b	.L4
.L5:
	ldr	r2, [fp, #-20]
	ldr	r3, [fp, #-16]
	add	r3, r2, r3
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-16]
	str	r3, [fp, #-20]
	ldr	r3, [fp, #-8]
	str	r3, [fp, #-16]
	ldr	r3, [fp, #-12]
	add	r3, r3, #1
	str	r3, [fp, #-12]
.L4:
	ldr	r2, [fp, #-12]
	ldr	r3, [fp, #-24]
	cmp	r2, r3
	ble	.L5
	ldr	r3, [fp, #-16]
.L3:
	mov	r0, r3
	add	sp, fp, #0
	@ sp needed
	ldr	fp, [sp], #4
	bx	lr
	.size	fib, .-fib
	.section	.rodata
	.align	2
.LC0:
	.ascii	"%d\000"
	.text
	.align	2
	.global	main
	.syntax unified
	.arm
	.type	main, %function
main:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #8
	mov	r3, #9
	str	r3, [fp, #-8]
	ldr	r0, [fp, #-8]
	bl	fib
	mov	r3, r0
	mov	r1, r3
	ldr	r0, .L8
	bl	printf
	mov	r3, #0
	mov	r0, r3
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
.L9:
	.align	2
.L8:
	.word	.LC0
	.size	main, .-main
	.ident	"GCC: (Ubuntu 11.3.0-1ubuntu1~22.04.1) 11.3.0"
	.section	.note.GNU-stack,"",%progbits
