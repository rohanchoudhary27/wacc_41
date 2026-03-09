.text
.global main
main:
	stp fp, lr, [sp, #-16]!
	mov fp, sp
	mov w0, #7
	bl exit
	ldp fp, lr, [sp], #16

// Runtime Library


