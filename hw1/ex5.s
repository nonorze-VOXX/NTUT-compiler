.data
.format: .string "%d\n"
.false: .string "false\n"
.true: .string "true\n"
.text
.global main
main:
    call p1
    call p2

    movl $0, %eax
    ret


p1:
    pushq % rbp
    movq %rsp, %rbp
    subq $16, %rsp

    movl $3, -4(%rbp)
    movl -4(%rbp), %r9d
    imul %r9d, %r9d

    movl %r9d, %esi
    movl $0, %eax
    movl $.format, %edi
    call printf


    movl $0, %eax
    leave
    ret

p2:
    pushq % rbp
    movq %rsp, %rbp
    subq $16, %rsp

    # store x
    movl $3, -4(%rbp)
    movl -4(%rbp), %r8d
    addl %r8d, %r8d

    # store y
    movl %r8d, -8(%rbp)

    # store z
    movl $3, -4(%rbp)
    movl -4(%rbp), %r8d
    addl $3, %r8d
    movl %r8d, -12(%rbp)



    # x*y
    movl -4(%rbp), %r8d
    movl -8(%rbp), %r9d
    imul %r8d, %r9d
    # %r9d = x*y

    # z/z
    movl -12(%rbp), %eax
    movl -12(%rbp), %r8d
    movl $0, %edx
    idivl %r8d
    # %eax = z/z
    
    # x*y + z/z
    movl %r9d, %esi
    addl %eax, %esi
    # %esi = x*y + z/z

    movl $0, %eax
    movl $.format, %edi
    call printf


    movl $0, %eax
    leave
    ret


