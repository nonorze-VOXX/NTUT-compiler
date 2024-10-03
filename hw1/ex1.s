.data
.format: .string "n = %d\n"
.text
.global main
main:
	movl	$.format, %edi
    movl $42, %edx
	movl	$42, %esi
	movl	$0, %eax
    call printf
	movl	$0, %eax
    ret
