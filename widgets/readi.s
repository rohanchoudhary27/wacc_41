// int a = 4
// read a

// length of .L._readi_str0
	.word 2
.L._readi_str0:
	.asciz "%d"
.align 4
_readi:
	// X0 contains the "original" value of the destination of the read
	// allocate space on the stack to store the read: preserve alignment!
	// the passed default argument should be stored in case of EOF
	// aarch64 mandates 16-byte SP alignment at all times, might as well merge the stores
	// push {x0, lr}
	stp x0, lr, [sp, #-16]!
	mov x1, sp
	adr x0, .L._readi_str0
	bl scanf
	// pop {x0, lr}
	ldp x0, lr, [sp], #16
	ret