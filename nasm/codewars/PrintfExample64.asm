global main
extern printf
section .data
  a: dd 5
  ; fmt: db "a=%d, eax=%d", 10, 0
  fmt: db "eax=%d", 10, 0

section .text
main:
  push rbx
  mov ecx, 90
  xor rax, rax
  xor rbx, rbx
  inc rbx

print:
  push rax
  push rcx
  mov rdi, fmt
  mov rsi, rax
  xor rax, rax

  call printf

  pop rcx
  pop rax

  mov rdx, rax
  mov rax, rbx
  add rbx, rdx
  dec ecx
  jnz print

  pop rbx
  ret
