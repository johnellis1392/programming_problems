  global main
  extern printf
  section .data
a: dd 5
fmt: db "a=%d, eax=%d", 10, 0

  section .text
main:
  push ebp
  mov ebp, esp

  mov eax, [a]
  add eax, 2
  push eax
  push dword [a]
  push dword fmt
  call printf
  add esp, 12

  mov esp, ebp
  pop ebp

  mov eax, 0
  ret
