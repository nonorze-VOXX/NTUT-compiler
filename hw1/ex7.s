        .text
	.globl	main

main:

		mov $1, %r8d
		mov N(%rip), %cl
		sal %cl, %r8d
		sub $1, %r8d

		mov $0, %edi
		mov %r8d, %esi

		call f

		movl %eax, %esi
		movl $solution, %edi

		movl $0,%eax
		call printf

		call print_memo


        xorq    %rax, %rax
        ret

print_memo:
		pushq %rbp
		movq %rsp, %rbp
		subq $64, %rsp

		mov $0, -4(%rbp)
start_D:
		mov N(%rip), %r8d
		cmp %r8d, -4(%rbp)
		je end_D

		mov $0, -8(%rbp)
start_DD:

		mov N(%rip), %r8d
		cmp %r8d, -8(%rbp)
		je end_DD
		

		mov -4(%rbp), %r8d
		imul N(%rip), %r8d
		addl -8(%rbp), %r8d
		movl memo(,%r8d,4), %esi
		movl $format, %edi

		movl $0,%eax
		call printf

		add $1,-8(%rbp)
		jmp start_DD

end_DD:
		movl $endl, %edi

		movl $0,%eax
		call printf


		add $1, -4(%rbp)
		jmp start_D
end_D:
		movl $0, %eax
		leave
		ret


f:
		pushq %rbp
		movq %rsp, %rbp
		subq $64, %rsp

		movl %edi, -4(%rbp) #i
		movl %esi, -8(%rbp) #c

		movl N(%rip), %r8d
		cmpl %r8d, -4(%rbp)
		je return_0
		
		movl -8(%rbp), %r8d #c
		mov L(%rip), %cl
		sal %cl, %r8d # ;c << L
		or -4(%rbp), %r8d# ; c<<L |i
		movl %r8d, -12(%rbp) # key

		movl -12(%rbp), %r8d
		movl memo(,%r8d,4),  %r8d
		movl %r8d,-16(%rbp) # r

		cmpl $0, -16(%rbp)
		jne return_r
		jmp no_return_r
return_r:
		movl -16(%rbp),%eax
		jmp return_eax
no_return_r:
		movl $0, -20(%rbp) # s
		movl $0, -24(%rbp) # j

start_for:
		mov N(%rip), %r8d	
		mov -24(%rbp), %r9d	
		cmp %r8d, %r9d # N cmp j
		jge end_for


		movl $1, -28(%rbp)
		mov -24(%rbp), %cl 
		sal %cl, -28(%rbp) # ; col = 1<< j

		mov -8(%rbp), %r8d #c
		and -28(%rbp),%r8d # c&col


		cmpl  $0, %r8d
		je continue_for

		mov N(%rip), %r8d
		imul -4(%rbp), %r8d # N*i
		add -24(%rbp), %r8d  # +j
		mov m(,%r8d,4), %r8d
		mov %r8d,	-32(%rbp) # m[i][j]


		movl -4(%rbp), %edi
		add $1, %edi # i+1
		movl -8(%rbp), %esi #c
		sub -28(%rbp), %esi #c-col

		call f

		add %eax, -32(%rbp) # x= m[i][j] + f(i+1,c-col)

		mov -20(%rbp), %r8d
		mov -32(%rbp), %r9d
		cmp %r8d, %r9d
		jg s_eq_x
		jle continue_for
s_eq_x:
		mov -32(%rbp), %r8d
		mov %r8d, -20(%rbp)

continue_for:
		addl $1, -24(%rbp) # j++
		jmp start_for
end_for:
		mov -20(%rbp), %r8d
		mov -12(%rbp), %r9d
		mov %r8d, memo(, %r9d,4)
		mov %r8d, %eax
		movl -12(%rbp), %r10d # ; debug key value
		jmp return_eax

return_0:
		movl $0, %eax
return_eax:
		leave
		ret

        .data
N: .int 15
L: .int 4
solution: .string "solution = %d\n"
format: .string "%d "
endl: .string "\n"
m:
	.long	7
	.long	53
	.long	183
	.long	439
	.long	863
	.long	497
	.long	383
	.long	563
	.long	79
	.long	973
	.long	287
	.long	63
	.long	343
	.long	169
	.long	583
	.long	627
	.long	343
	.long	773
	.long	959
	.long	943
	.long	767
	.long	473
	.long	103
	.long	699
	.long	303
	.long	957
	.long	703
	.long	583
	.long	639
	.long	913
	.long	447
	.long	283
	.long	463
	.long	29
	.long	23
	.long	487
	.long	463
	.long	993
	.long	119
	.long	883
	.long	327
	.long	493
	.long	423
	.long	159
	.long	743
	.long	217
	.long	623
	.long	3
	.long	399
	.long	853
	.long	407
	.long	103
	.long	983
	.long	89
	.long	463
	.long	290
	.long	516
	.long	212
	.long	462
	.long	350
	.long	960
	.long	376
	.long	682
	.long	962
	.long	300
	.long	780
	.long	486
	.long	502
	.long	912
	.long	800
	.long	250
	.long	346
	.long	172
	.long	812
	.long	350
	.long	870
	.long	456
	.long	192
	.long	162
	.long	593
	.long	473
	.long	915
	.long	45
	.long	989
	.long	873
	.long	823
	.long	965
	.long	425
	.long	329
	.long	803
	.long	973
	.long	965
	.long	905
	.long	919
	.long	133
	.long	673
	.long	665
	.long	235
	.long	509
	.long	613
	.long	673
	.long	815
	.long	165
	.long	992
	.long	326
	.long	322
	.long	148
	.long	972
	.long	962
	.long	286
	.long	255
	.long	941
	.long	541
	.long	265
	.long	323
	.long	925
	.long	281
	.long	601
	.long	95
	.long	973
	.long	445
	.long	721
	.long	11
	.long	525
	.long	473
	.long	65
	.long	511
	.long	164
	.long	138
	.long	672
	.long	18
	.long	428
	.long	154
	.long	448
	.long	848
	.long	414
	.long	456
	.long	310
	.long	312
	.long	798
	.long	104
	.long	566
	.long	520
	.long	302
	.long	248
	.long	694
	.long	976
	.long	430
	.long	392
	.long	198
	.long	184
	.long	829
	.long	373
	.long	181
	.long	631
	.long	101
	.long	969
	.long	613
	.long	840
	.long	740
	.long	778
	.long	458
	.long	284
	.long	760
	.long	390
	.long	821
	.long	461
	.long	843
	.long	513
	.long	17
	.long	901
	.long	711
	.long	993
	.long	293
	.long	157
	.long	274
	.long	94
	.long	192
	.long	156
	.long	574
	.long	34
	.long	124
	.long	4
	.long	878
	.long	450
	.long	476
	.long	712
	.long	914
	.long	838
	.long	669
	.long	875
	.long	299
	.long	823
	.long	329
	.long	699
	.long	815
	.long	559
	.long	813
	.long	459
	.long	522
	.long	788
	.long	168
	.long	586
	.long	966
	.long	232
	.long	308
	.long	833
	.long	251
	.long	631
	.long	107
	.long	813
	.long	883
	.long	451
	.long	509
	.long	615
	.long	77
	.long	281
	.long	613
	.long	459
	.long	205
	.long	380
	.long	274
	.long	302
	.long	35
	.long	805
        .bss
memo:
        .space	2097152

## Local Variables:

## compile-command: "gcc ex7.s && ./a.out"
## End:
