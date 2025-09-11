global main

extern printf

section .data
msg:    db  'Hello World: %c %s of length %d %d %X %e %E', 10, 0
char1:  db  'a'
str1:   db  'string', 0
len:    equ $-str1
inta1:  dd  1234567
hex1:   dd  0x6789ABCD
flt1:   dd  5.327e-30
flt2:   dq  -123.456789e300

section .bss
flttmp: resq 1

section .text
main:
  fld dword [flt1]
  fstp qword [flttmp]

  ; push dword [flt2+4]
  ; push dword [flt2]
  ; push dword [flttmp+4]
  ; push dword [flttmp]
  ; push dword [hex1]
  ; push dword [inta1]

  mov rax, [flt2+4]
  push rax
  mov rax, [flt2]
  push rax
  mov rax, [flttmp+4]
  push rax
  mov rax, [flttmp]
  push rax
  mov rax, [hex1]
  push rax
  mov rax, [inta1]
  push rax

  ; push dword len
  ; push dword str1
  mov rax, len
  push rax
  mov rax, str1
  push rax

  ; push dword [char1]
  mov rax, [char1]
  push rax

  ; push dword msg
  mov rax, msg
  push rax

  call printf
  add esp, 40

  mov eax, 0
  ret
