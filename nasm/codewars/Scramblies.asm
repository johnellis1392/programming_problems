; global scramble:function
global main
extern printf
; extern strlen

section .data
  running_msg: db 'Running...', 10, 0
  done_msg: db 'Done.', 10, 0
  
  test_success_msg: db 'Success.', 10, 0
  test_failure_msg: db 'Failure.', 10, 0
  
  test1_str1: db 'rkqodlw', 0
  test1_str2: db 'world', 0
  test1_exp: dq 1

  test2_str1: db 'cedewaraaossoqqyt', 0
  test2_str2: db 'codewars', 0
  test2_exp dq 1

  test3_str1: db 'katas', 0
  test3_str2: db 'steak', 0
  test3_exp: dq 0
  
  debug1: db 'Char: %d', 10, 0
  

section .bss
  charbuf: resw 26

section .text

main:
  push rbp
  mov rbp, rsp

  mov rdi, running_msg
  call printf


  ;; ;;;;;;;;; ;;
  ;; Run Tests ;;
  ;; ;;;;;;;;; ;;

  ;; test1
  mov rdi, test1_str1
  mov rsi, test1_str2
  mov rdx, [test1_exp]
  call run_test

  ;; test2
  mov rdi, test2_str1
  mov rsi, test2_str2
  mov rdx, [test2_exp]
  call run_test

  ;; test3
  mov rdi, test3_str1
  mov rsi, test3_str2
  mov rdx, [test3_exp]
  call run_test


  mov rdi, done_msg
  call printf

  mov rsp, rbp
  pop rbp
  ret


run_test: ;; (rdi:str1, rsi:str2, rdx:exp)
  push rbp
  mov rbp, rsp

  ;; Save exp
  push rdx
  ; sub rsp, 8 ;; Align stack
  call scramble
  pop rdx
  cmp rax, rdx ;; actual == exp?
  je .success
  jmp .failure

.success:
  mov rdi, test_success_msg
  call printf
  jmp .return

.failure:
  mov rdi, test_failure_msg
  call printf
  
.return:
  mov rsp, rbp
  pop rbp
  ret


; scramble(&str, &str) -> bool
scramble: ;; (rdi:str1, rsi:str2) -> rax:bool
  push rbp
  mov rbp, rsp

  push rbx
  push rcx
  push rdx

;; Initialize char counter buffer
.init_charbuf:
  mov rcx, 0
  ; mov rax, charbuf
.init_charbuf_loop:
  ; mov qword [rax+rcx], 0
  mov dword [charbuf+rcx], 0
  inc rcx
  cmp rcx, 26
  jne .init_charbuf_loop

;; Load str1 buffer counter
  mov rcx, 0
  mov rax, rdi
.load_str1_loop:
  cmp byte [rax+rcx], 0
  je .exit_str1_loop

  mov rbx, [rax+rcx]
  sub rbx, 'a'
  and rbx, 0xFF
  inc dword [charbuf+rbx]
  inc rcx
  jmp .load_str1_loop
.exit_str1_loop:

;; Analyze str2
  mov rcx, 0
  mov rax, rsi
.str2_loop:
  cmp byte [rax+rcx], 0
  je .exit_str2_loop

  mov rbx, [rax+rcx]
  sub rbx, 'a'
  and rbx, 0xFF
  dec dword [charbuf+rbx]
  inc rcx

  cmp dword [charbuf+rbx], -1
  je .false

  jmp .str2_loop
.exit_str2_loop

.true:
  mov rax, 1
  jmp .return

.false:
  mov rax, 0

.return:
  pop rdx
  pop rcx
  pop rbx
  mov rsp, rbp
  pop rbp
  ret