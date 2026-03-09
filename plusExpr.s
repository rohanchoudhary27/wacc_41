.text
.global main
main:
	stp fp, lr, [sp, #-16]!
	mov fp, sp
	stp x19, x20, [sp, #-16]!
	str x21, [sp, #-16]!
	movz w16, #15
	mov w19, w16
	movz w16, #20
	mov w20, w16
	adds w21, w19, w20
	bvs _errOverflow
	sub sp, sp, #0
	mov w0, w21
	bl _printi
	sub sp, sp, #0
	bl _println
	movz w0, #0
	ldr x21, [sp], #16
	ldp x19, x20, [sp], #16
	ldp fp, lr, [sp], #16
	ret


// RUNTIME LIBRARY:

// prints a string value to stdout
// length of .L._prints_str0
    .word 4
.L._prints_str0:
    .asciz "%.*s"
    .align 4
_prints:
    // push {lr}
    stp lr, xzr, [sp, #-16]!
    mov x2, x0
    ldur w1, [x0, #-4]
    adr x0, .L._prints_str0
    bl printf
    mov x0, #0
    bl fflush
    // pop {lr}
    ldp lr, xzr, [sp], #16
    ret

// prints a new line to stdout
// length of .L._println_str0
     .word 0
.L._println_str0:
    .asciz ""
    .align 4
_println:
     // push {lr}
     stp lr, xzr, [sp, #-16]!
     adr x0, .L._println_str0
     bl puts
     mov x0, #0
     bl fflush
     // pop {lr}
     ldp lr, xzr, [sp], #16
     ret

// integer overflow or underflow error handler
// length of .L._errOverflow_str0
    .word 52
.L._errOverflow_str0:
    .asciz "fatal error: integer overflow or underflow occurred\n"
    .align 4
_errOverflow:
    adr x0, .L._errOverflow_str0
    bl _prints
    mov w0, #255
    bl exit

// prints an integer value to stdout
// length of .L._printi_str0
    .word 2
.L._printi_str0:
    .asciz "%d"
    .align 4
_printi:
    // push {lr}
    stp lr, xzr, [sp, #-16]!
    mov x1, x0
    adr x0, .L._printi_str0
    bl printf
    mov x0, #0
    bl fflush
    // pop {lr}
    ldp lr, xzr, [sp], #16
    ret
