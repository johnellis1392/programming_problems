  section .data
msg: db "Hello, World!", 10

  section .text
  global _main

_main: 
  mov rax, 1
  mov rdi, 1
  mov rsi, msg
  mov rdx, 14
  syscall

  mov rax, 60
  xor rdi, rdi
  syscall
