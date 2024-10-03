.data
.format: .string "%d\n"
.false: .string "false\n"
.true: .string "true\n"
.text
.global main
main:
    call p1
    call p2
    call p3

    ret
p1:
	movl	$0, %r9d
    movl $1, %r8d
    testl %r8d, %r9d
    je printFalse
    jne printTrue

	movl	$0, %eax
    ret

p2:
	movl	$3, %r9d
    movl $4, %r8d
    cmpl %r8d, %r9d
    je print14
    jne print20

print20:
    movl $10, %esi
    imul $2, %esi

    movl $0, %eax
    movl $.format, %edi
    call printf

	movl	$0, %eax
    ret
print14:
    movl $14, %esi
    movl $0, %eax
    movl $.format, %edi
    call printf

	movl	$0, %eax
    ret


printFalse:
    movq $.false, %rdi
    call printf
	movl	$0, %eax
    ret

printTrue:
    movq $.true, %rdi
    call printf
	movl	$0, %eax
    ret


p3:
    movl $2, %r8d
    movl $3, %r9d
    cmpl %r8d, %r9d
    je printTrue
    jne next
	movl $0, %eax
    ret
next:
    movl $2, %r8d
    imul $3, %r8d

    movl $4, %r9d
    cmpl %r8d, %r9d
    jg unwant

    call printTrue
    jmp end_if
unwant:
    call printFalse
    jmp end_if

end_if:
	movl $0, %eax
    ret






