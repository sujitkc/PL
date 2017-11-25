    .data
    .text
main:
    li      $a0, 4
    sw      $a0, 0($sp)
    addiu   $sp, -4
    li      $a0, 5
    lw      $t1, 4($sp)
    addiu   $sp, 4
    addu    $a0, $a0, $t1   
    li      $v0, 1
    syscall
    li      $v0, 10
    syscall
