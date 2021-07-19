sys_exit:       equ             60

                section         .text
                global          _start

buf_size:       equ             8192
_start:
                xor             ebx, ebx                    ;зануляем счётчик 
                sub             rsp, buf_size               
                mov             rsi, rsp
                mov             byte[rsi - 1], 0x0d

read_again:
                xor             eax, eax
                xor             edi, edi                    
                mov             rdx, buf_size
                syscall
                ; result of syscall for read goes to rax
                ; if < 0 - error, == 0 - eof, >0 - this number of chars we've read

                test            rax, rax
                jz              quit
                js              read_error

                xor             ecx, ecx

check_char:
                cmp             rcx, rax
                je              read_again
                ; 9 10 11 12 13 and 32
                
                cmp             byte [rsi + rcx], 0x20
                je              skip
                cmp             byte [rsi + rcx], 0x09
                je              skip
                cmp             byte [rsi + rcx], 0x0a
                je              skip
                cmp             byte [rsi + rcx], 0x0b
                je              skip
                cmp             byte [rsi + rcx], 0x0c
                je              skip
                cmp             byte [rsi + rcx], 0x0d
                je              skip
                cmp             byte [rsi + rcx], 0x20
                ja              check_prev
                
                cmp             byte [rsi + rcx], 0x09
                jb              check_prev
                cmp             byte [rsi + rcx], 0x0d
                jng             skip

check_prev:   
                cmp             byte [rsi + rcx - 1], 0x09
                jb              skip
                cmp             byte [rsi + rcx - 1], 0x20
                je              plus_1
                cmp             byte [rsi + rcx - 1], 0x09
                je              plus_1
                cmp             byte [rsi + rcx - 1], 0x0a
                je              plus_1
                cmp             byte [rsi + rcx - 1], 0x0b
                je              plus_1
                cmp             byte [rsi + rcx - 1], 0x0c
                je              plus_1  
                cmp             byte [rsi + rcx - 1], 0x0d
                je              plus_1
                cmp             byte [rsi + rcx - 1], 0x20
                ja              skip
                cmp             byte [rsi + rcx - 1], 0x20
                jb              skip

                
                

plus_1:         inc             rbx             ; +1 word 

skip:           
                inc             rcx
                jmp             check_char

quit:
                mov             rax, rbx
                call            print_int

                mov             rax, sys_exit
                xor             rdi, rdi
                syscall

; rax -- number to print
print_int:
                mov             rsi, rsp
                mov             ebx, 10

                dec             rsi
                mov             byte [rsi], 0x0a

next_char:
                xor             edx, edx
                div             ebx
                add             dl, '0'
                dec             rsi
                mov             [rsi], dl
                test            rax, rax
                jnz             next_char

                mov             eax, 1
                mov             edi, 1
                mov             rdx, rsp
                sub             rdx, rsi
                syscall

                ret

read_error:
                mov             eax, 1
                mov             edi, 2
                mov             rsi, read_error_msg
                mov             rdx, read_error_len
                syscall

                mov             rax, sys_exit
                mov             edi, 1
                syscall

                section         .rodata

read_error_msg: db              "read failure", 0x0a
read_error_len: equ             $ - read_error_msg
