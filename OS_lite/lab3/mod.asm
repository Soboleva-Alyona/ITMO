section .text
	global _start

_start:
	mov eax, 2
	int 0x80
	
	cmp eax, 0
	je _child         ; we are a child process
	                  ; else we are parent
_parent:
	mov edx, msg1_len
	mov ecx, msg1
	jmp _print

_child:
	mov edx, msg2_len
	mov ecx, msg2

_print:
	mov eax, 4
	mov ebx, 1
	int 0x80
_exit:
	mov ebx, 0
	mov eax, 1
	int 0x80
	
section .rodata 
	msg1: db 'I am parent process',0x0a
	msg1_len: equ $ - msg1

	msg2: db "I'm child process",0x0a
	msg2_len: equ $ - msg2
