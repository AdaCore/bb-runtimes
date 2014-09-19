	.cpu cortex-r4
	.eabi_attribute 27, 3
	.eabi_attribute 28, 1
	.fpu vfpv3-d16
	.eabi_attribute 20, 1
	.eabi_attribute 21, 1
	.eabi_attribute 23, 3
	.eabi_attribute 24, 1
	.eabi_attribute 25, 1
	.eabi_attribute 26, 1
	.eabi_attribute 30, 4
	.eabi_attribute 34, 1
	.file	"sys_startup_simple.adb"
	.text
	.align	2
	.global	__ada_sys_startup
	.type	__ada_sys_startup, %function
__ada_sys_startup:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	mvn	r3, #0
	ldr	r2, .L9
	movw	r0, #785
	str	r2, [r3, #-143]
	ldr	r2, .L9+4
	str	r2, [r3, #-139]
	mvn	r2, #4096
	ldr	r1, .L9+8
	str	r1, [r2, #-3839]
	mov	r1, #66
	str	r1, [r3, #-199]
	str	r3, [r2, #-3935]
	str	r3, [r2, #-3931]
	str	r3, [r2, #-3927]
	str	r3, [r2, #-3923]
	ldr	r1, [r3, #-47]
	orr	r1, r1, #256
	str	r1, [r3, #-47]
	mvn	r1, #491520
	str	r0, [r1, #-4095]
	mov	r0, #5
	str	r0, [r1, #-3447]
	ldr	r0, .L9+12
	str	r0, [r1, #-3399]
	mov	r0, #10
	str	r0, [r1, #-3447]
	ldr	r1, .L9+16
	str	r1, [r3, #-119]
	ldr	r1, [r3, #-47]
	uxth	r1, r1
	orr	r1, r1, #16777216
	orr	r1, r1, #65536
	str	r1, [r3, #-47]
	mov	r1, #1
	str	r1, [r2, #-3779]
.L2:
	ldr	r1, [r3, #-171]
	mvn	r2, #0
	and	r1, r1, #66
	cmp	r1, #66
	bne	.L2
	ldr	r3, .L9+20
	mov	r1, #1
	str	r3, [r2, #-183]
	ldr	r3, [r2, #-47]
	ldr	r0, .L9+24
	uxth	r3, r3
	orr	r3, r3, #16777216
	orr	r3, r3, #65536
	str	r3, [r2, #-47]
	mvn	r3, #4096
	str	r1, [r3, #-3779]
	str	r0, [r2, #-175]
	mov	r0, #1536
	str	r0, [r2, #-179]
	ldr	r0, .L9+28
	str	r0, [r3, #-3775]
	ldr	r0, [r2, #-143]
	bic	r0, r0, #520093696
	str	r0, [r2, #-143]
	ldr	r0, [r3, #-3839]
	bic	r0, r0, #520093696
	str	r0, [r3, #-3839]
	mov	r3, #10
	str	r3, [r2, #-163]
	str	r1, [r2, #-159]
.L3:
	ldr	r3, [r2, #-151]
	tst	r3, #256
	beq	.L3
	mov	r2, #5
	mvn	r3, #0
	str	r2, [r3, #-163]
	b	__ada_sys_startup_cont
.L10:
	.align	2
.L9:
	.word	788748544
	.word	80069193
	.word	788730624
	.word	196610
	.word	16781320
	.word	16842753
	.word	17367305
	.word	18415641
	.size	__ada_sys_startup, .-__ada_sys_startup
