.data
format: .string "sqrt(%2d) = %2d \n"
.false: .string "false\n"
.true: .string "true\n"
.text
.global main

main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp

    # n = 0
    movl $0, -4(%rbp)

start_for:

    mov -4(%rbp), %r8d
    cmp $20, %r8d
    je end_for

    mov -4(%rbp), %r8d
    add $1, %r8d
    mov %r8d, -4(%rbp)

    mov -4(%rbp), %ebx
    call is_sqrt
    mov %eax, %r8d


    mov -4(%rbp), %edi
    mov %r8d, %esi
    call print

    jmp start_for
end_for:

    movl $0, %eax
    leave
    ret

is_sqrt:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp


    movl $0, %r8d # c
    movl $1, %r9d # s

start_sqrt_for:
    cmpl %ebx, %r9d # %ebx= n, %r9d =s
    jg end_sqrt_for 

    addl $1, %r8d

    # s += 2*c +1
    leal 1(%r9d,%r8d,2), %r9d

    jmp start_sqrt_for
end_sqrt_for:


    movl %r8d, %eax
    leave
    ret

print:
    pushq %rbp
    movq %rsp, %rbp

    movl %esi, %edx
    movl %edi, %esi
    movl $format, %edi
    call printf

    movl $0, %eax
    leave
    ret
