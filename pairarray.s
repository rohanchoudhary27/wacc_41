.text
.global main
main:
	stp fp, lr, [sp, #-16]!
	mov fp, sp
	stp x19, x20, [sp, #-16]!
	stp x21, x22, [sp, #-16]!
	stp x23, x24, [sp, #-16]!
	stp x25, x26, [sp, #-16]!
	stp x27, x28, [sp, #-16]!
	sub sp, sp, #0
	movz w0, #16
	bl _malloc
	mov x19, x0
	movz x17, #5
	str x17, [x19, #0]
	movz x17, #6
	str x17, [x19, #8]
	mov x20, x19
	sub sp, sp, #0
	movz w0, #20
	bl _malloc
	mov x21, x0
	movz w16, #2
	mov w22, w16
	stur w16, [x21, #0]
	adds x23, x21, #4
	bvs _errOverflow
	mov x7, x23
	movz w17, #0
	mov x8, x20
	bl _arrStore8
	mov x7, x23
	movz w17, #1
	mov x8, x20
	bl _arrStore8
	mov x24, x23
	mov x7, x24
	movz w17, #0
	bl _arrLoad8
	mov x25, x7
	cmp x25, #0
	beq _errNull
	movz x17, #3
	str x17, [x25, #0]
	mov x7, x24
	movz w17, #1
	bl _arrLoad8
	mov x26, x7
	cmp x26, #0
	beq _errNull
	ldr x17, [x26, #0]
	mov w27, w17
	mov w28, w27
	sub sp, sp, #0
	mov w0, w28
	bl _printi
	sub sp, sp, #0
	bl _println
	movz w0, #0
	ldp x27, x28, [sp], #16
	ldp x25, x26, [sp], #16
	ldp x23, x24, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x19, x20, [sp], #16
	ldp fp, lr, [sp], #16
	ret


// RUNTIME LIBRARY:

// null pair dereference or free error handler
// length of .L._errNull_str0
    .word 45
.L._errNull_str0:
    .asciz "fatal error: null pair dereferenced or freed\n"
    .align 4
_errNull:
    adr x0, .L._errNull_str0
    bl _prints
    mov w0, #-1
    bl exit

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

// allocate heap memory with out-of-memory checking
_malloc:
    // push {lr}
    stp lr, xzr, [sp, #-16]!
    bl malloc
    cbz x0, _errOutOfMemory
    // pop {lr}
    ldp lr, xzr, [sp], #16
    ret

// bounds-checked array load for 8-byte elements (pointers)
// Special calling convention: array ptr passed in X7, index in X17, LR (W30) is used as general register, and return into X7
_arrLoad8:
    // push {lr}
    stp lr, xzr, [sp, #-16]!
    cmp w17, #0
    csel x1, x17, x1, lt // this must be a 64-bit move so that it doesn't truncate if the move fails
    b.lt _errOutOfBounds
    ldur w30, [x7, #-4]
    cmp w17, w30
    csel x1, x17, x1, ge // this must be a 64-bit move so that it doesn't truncate if the move fails
    b.ge _errOutOfBounds
    ldr x7, [x7, x17, lsl #3]
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

// bounds-checked array store for 8-byte elements (pointers)
// Special calling convention: array ptr passed in X7, index in X17, value to store in X8, LR (W30) is used as general register
_arrStore8:
    // push {lr}
    stp lr, xzr, [sp, #-16]!
    cmp w17, #0
    csel x1, x17, x1, lt // this must be a 64-bit move so that it doesn't truncate if the move fails
    b.lt _errOutOfBounds
    ldur w30, [x7, #-4]
    cmp w17, w30
    csel x1, x17, x1, ge // this must be a 64-bit move so that it doesn't truncate if the move fails
    b.ge _errOutOfBounds
    str x8, [x7, x17, lsl #3]
    // pop {lr}
    ldp lr, xzr, [sp], #16
    ret
