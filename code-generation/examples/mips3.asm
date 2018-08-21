
    .data
    .text
add_start:
	move	$fp $sp     # update frame pointer to point to the beginning of the new stack frame
	sw	$ra 0($sp)  # push return address (step 1)
    	addiu	$sp $sp -4  # push return address (step 2)
        lw      $a0 4($fp)  # load arg 1
        sw      $a0 0($sp)  # push arg 1 (step 1)
    	addiu	$sp $sp -4  # push arg 1 (step 2)
        lw      $a0 8($fp)  # load arg 2
        lw      $t1 4($sp)  # load arg 1 (step 1)
    	addiu	$sp $sp  4  # load arg 1 (step 2)
        addu    $a0 $a0 $t1 # add
        lw      $ra 4($sp)  # restore return address from stack (step 1)
        move    $sp $fp     # restore stack pointer to the beginning of the current stack frame
        lw      $fp 0($sp)  # restore frame pointer to its previous value
        jr      $ra         # jump back to the caller
        
main:
                            # call sequence for call to add - start
	sw	$fp 0($sp)  # push frame pointer (step 1)
    	addiu	$sp $sp -4  # push frame pointer (step 2)
        li      $a0 2       # evaluate arg 2
	sw	$a0 ($sp)   # push arg 2 (step 1)
    	addiu	$sp $sp -4  # push arg 2 (step 2)
        li      $a0 3       # evaluate arg 2
	sw	$a0 ($sp)   # push arg 1 (step 1)
    	addiu	$sp $sp -4  # push arg 1 (step 2)
        jal     add_start
                            # call sequence for call to add - end
        li      $v0 1
        syscall
        li      $v0 10
        syscall
