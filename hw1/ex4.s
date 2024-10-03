.data
.format: .string "%d\n"
.false: .string "false\n"
.true: .string "true\n"
.text
.global main
main:
    movl $2, %r8d
    movl %r8d, %r9d
    imul %r8d, %r9d
    movl %r9d, %r10d
    addl %r8d, %r10d

    movl %r10d, %esi
    movl $0, %eax
    movl $.format, %edi
    call printf

    movl $0, %eax
    ret
