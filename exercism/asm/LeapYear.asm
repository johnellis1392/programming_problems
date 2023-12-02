global main
extern printf

section .data
  msg: db 'Year %d is a leap year: %d', 10, 0
  test1: dd 1997
  test2: dd 1996

section .text
main:
  push rbp
  mov rbp, rsp
  push qword [test1]
  push qword 1
  call printf
  add rsp, 8

  mov rsp, rbp
  pop rbp

  mov rax, 0
  ret

; A leap year is any year that is:
; - Divisible by 4,
; - Unless divisible by 100,
; - Unless also divisible by 400

leap_year:
  mov rax, 1
  ret
