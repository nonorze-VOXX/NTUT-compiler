.data
.format: .string "%d\n"
.text
.global main
main:
    call .p1
    call .p2
    call .p3
    call .p4
    ret

.p1:
    movl $4, %eax
    addl $6, %eax
    mov %eax, %esi
    movl $0, %eax
    movl $.format, %edi
    call printf
	movl	$0, %eax
    ret

.p2:
    movl $21, %eax
    imul $2, %eax
    mov %eax, %esi
    movl $0, %eax
    movl $.format, %edi
    call printf
	movl	$0, %eax
    ret

.p3:
    movl $7, %eax
    movl $0, %edx
    movl $2, %r8d
    idivl %r8d
    addl $4, %eax
    mov %eax, %esi
    movl $0, %eax
    movl $.format, %edi
    call printf
	movl	$0, %eax
    ret

.p4:
    movl $10, %eax
    movl $0, %edx
    movl $5, %r8d
    idivl %r8d
    imul $6, %eax

    mov %eax, %r8d
    mov $3, %eax
    subl %r8d, %eax

    mov %eax, %esi
    movl $0, %eax
    movl $.format, %edi
    call printf
	movl	$0, %eax
    ret
