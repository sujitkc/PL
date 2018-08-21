    .data
theString:
    .space 64
out_string: .asciiz "\nHello, Sujit!\n"
    .text
main:
#    li      $v0, 8
#    la      $a0, theString
#    li      $a1, 64
#    syscall
    li      $v0, 4
    la      $a0, out_string
    syscall
    li      $v0, 10
    syscall
