.data
.format: .string "%d\n"
.false: .string "false\n"
.true: .string "true\n"
x: .int 2
y: .int 0
.text
.global main
main:
    movl x(%rip), %r8d
    movl %r8d, y(%rip)
    imul x(%rip), %r8d
    movl %r8d, y(%rip)
    movl y(%rip), %r10d
    addl x(%rip), %r10d

    movl %r10d, %esi
    movl $0, %eax
    movl $.format, %edi
    call printf

    movl $0, %eax
    ret
