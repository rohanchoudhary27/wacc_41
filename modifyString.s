.text
.global main
main:
	stp fp, lr, [sp, #-16]!
	mov fp, sp
	stp x19, x20, [sp, #-16]!
	stp x21, x22, [sp, #-16]!
	stp x23, x24, [sp, #-16]!
	str x25, [sp, #-16]!
	sub sp, sp, #0
	movz w0, #16
	bl _malloc
	mov x19, x0
	movz w16, #12
	mov w20, w16
	stur w20, [x19, #0]
	adds x21, x19, #4
	bvs _errOverflow
	mov x7, x21
	movz w17, #0
	movz w8, #104
	bl _arrStore4
	mov x7, x21
	movz w17, #1
	movz w8, #101
	bl _arrStore4
	mov x7, x21
	movz w17, #2
	movz w8, #108
	bl _arrStore4
	mov x7, x21
	movz w17, #3
	movz w8, #108
	bl _arrStore4
	mov x7, x21
	movz w17, #4
	movz w8, #111
	bl _arrStore4
	mov x7, x21
	movz w17, #5
	movz w8, #32
	bl _arrStore4
	mov x7, x21
	movz w17, #6
	movz w8, #119
	bl _arrStore4
	mov x7, x21
	movz w17, #7
	movz w8, #111
	bl _arrStore4
	mov x7, x21
	movz w17, #8
	movz w8, #114
	bl _arrStore4
	mov x7, x21
	movz w17, #9
	movz w8, #108
	bl _arrStore4
	mov x7, x21
	movz w17, #10
	movz w8, #100
	bl _arrStore4
	mov x7, x21
	movz w17, #11
	movz w8, #33
	bl _arrStore4
	mov x22, x21
	sub sp, sp, #0
	mov x0, x22
	bl _prints
	sub sp, sp, #0
	bl _println
	mov x7, x22
	movz w17, #0
	movz w8, #72
	bl _arrStore4
	sub sp, sp, #0
	mov x0, x22
	bl _prints
	sub sp, sp, #0
	bl _println
	sub sp, sp, #0
	movz w0, #7
	bl _malloc
	mov x23, x0
	movz w16, #3
	mov w24, w16
	stur w24, [x23, #0]
	adds x25, x23, #4
	bvs _errOverflow
	mov x7, x25
	movz w17, #0
	movz w8, #72
	bl _arrStore4
	mov x7, x25
	movz w17, #1
	movz w8, #105
	bl _arrStore4
	mov x7, x25
	movz w17, #2
	movz w8, #33
	bl _arrStore4
	mov x22, x25
	sub sp, sp, #0
	mov x0, x22
	bl _prints
	sub sp, sp, #0
	bl _println
	movz w0, #0
	ldr x25, [sp], #16
	ldp x23, x24, [sp], #16
	ldp x21, x22, [sp], #16
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
    mov w0, #-1
    bl exit

// array index out of bounds error handler
// length of .L._errOutOfBounds_str0
    .word 42
.L._errOutOfBounds_str0:
    .asciz "fatal error: array index %d out of bounds\n"
    .align 4
_errOutOfBounds:
    adr x0, .L._errOutOfBounds_str0
    bl printf
    mov x0, #0
    bl fflush
    mov w0, #-1
    bl exit

// bounds-checked array store for 4-byte elements (int)
// Special calling convention: array ptr passed in X7, index in X17, value to store in X8, LR (W30) is used as general register
_arrStore4:
    // push {lr}
    stp lr, xzr, [sp, #-16]!
    cmp w17, #0
    csel x1, x17, x1, lt // this must be a 64-bit move so that it doesn't truncate if the move fails
    b.lt _errOutOfBounds
    ldur w30, [x7, #-4]
    cmp w17, w30
    csel x1, x17, x1, ge // this must be a 64-bit move so that it doesn't truncate if the move fails
    b.ge _errOutOfBounds
    str w8, [x7, x17, lsl #2]
    // pop {lr}
    ldp lr, xzr, [sp], #16
    ret

// allocate heap memory with out-of-memory checking
_malloc:
    // push {lr}
    stp lr, xzr, [sp, #-16]!
    bl malloc
    cbz x0, _errOutOfMemory
    // pop {lr}
    ldp lr, xzr, [sp], #16
    ret

// heap allocation failure error handler
// length of .L._errOutOfMemory_str0
    .word 27
.L._errOutOfMemory_str0:
    .asciz "fatal error: out of memory\n"
    .align 4
_errOutOfMemory:
    adr x0, .L._errOutOfMemory_str0
    bl _prints
    mov w0, #-1
    bl exit
