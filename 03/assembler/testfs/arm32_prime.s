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
	.file	"prime.c"
	.text
	.global	__aeabi_idivmod
	.align	2
	.global	main
	.syntax unified
	.arm
	.type	main, %function
main:
	@ args = 0, pretend = 0, frame = 16
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #16
	mov	r3, #30
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-8]
	cmp	r3, #1
	bne	.L2
	mov	r3, #0
	b	.L3
.L2:
	mov	r3, #0
	str	r3, [fp, #-16]
	mov	r3, #2
	str	r3, [fp, #-12]
	b	.L4
.L6:
	ldr	r3, [fp, #-8]
	ldr	r1, [fp, #-12]
	mov	r0, r3
	bl	__aeabi_idivmod
	mov	r3, r1
	cmp	r3, #0
	bne	.L5
	ldr	r3, [fp, #-16]
	add	r3, r3, #1
	str	r3, [fp, #-16]
.L5:
	ldr	r3, [fp, #-12]
	add	r3, r3, #1
	str	r3, [fp, #-12]
.L4:
	ldr	r2, [fp, #-12]
	ldr	r3, [fp, #-8]
	cmp	r2, r3
	blt	.L6
	ldr	r3, [fp, #-16]
	cmp	r3, #0
	bne	.L7
	mov	r3, #1
	b	.L3
.L7:
	mov	r3, #0
.L3:
	mov	r0, r3
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
	.size	main, .-main
	.ident	"GCC: (Ubuntu 11.3.0-1ubuntu1~22.04.1) 11.3.0"
	.section	.note.GNU-stack,"",%progbits
