
    .data
    .text

f_print:

	move	$fp $sp

        sw      $ra 0($sp) # push $ra(step 1)
    	addiu	$sp $sp -4 # push $ra(step 2)

        lw      $a0 4($fp)
        li      $v0 1
        syscall

        lw      $ra 4($sp) # pop $ra(step 1)
    	addiu	$sp $sp 4  # pop $ra(step 2)

        move    $sp $fp     # restore stack pointer to the beginning of the current stack frame
        lw      $fp 0($sp)  # restore frame pointer to its previous value
        jr      $ra         # jump back to the caller
f_add:

	move	$fp $sp

        sw      $ra 0($sp) # push $ra(step 1)
    	addiu	$sp $sp -4 # push $ra(step 2)

        lw      $a0, 4($fp)

        sw      $a0 0($sp) # push $a0(step 1)
    	addiu	$sp $sp -4 # push $a0(step 2)

        lw      $a0, 8($fp)

        lw      $t1 4($sp) # pop $t1(step 1)
    	addiu	$sp $sp 4  # pop $t1(step 2)
      
        addu    $a0 $a0 $t1

        lw      $ra 4($sp) # pop $ra(step 1)
    	addiu	$sp $sp 4  # pop $ra(step 2)

        move    $sp $fp     # restore stack pointer to the beginning of the current stack frame
        lw      $fp 0($sp)  # restore frame pointer to its previous value
        jr      $ra         # jump back to the caller

main:
       move $fp $sp

        # function call sequence - begin

        sw      $fp 0($sp) # push $fp(step 1)
    	addiu	$sp $sp -4 # push $fp(step 2)

        li      $a0 2

        sw      $a0 0($sp) # push $a0(step 1)
    	addiu	$sp $sp -4 # push $a0(step 2)

        li      $a0 2

        sw      $a0 0($sp) # push $a0(step 1)
    	addiu	$sp $sp -4 # push $a0(step 2)

        jal f_add
        # function call sequence - end

        # function call sequence - begin

        sw      $fp 0($sp) # push $fp(step 1)
    	addiu	$sp $sp -4 # push $fp(step 2)

        li      $a0 10

        sw      $a0 0($sp) # push $a0(step 1)
    	addiu	$sp $sp -4 # push $a0(step 2)

        jal f_print
        # function call sequence - end

        li      $v0 10
        syscall
