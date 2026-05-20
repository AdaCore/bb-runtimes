/* This is an -*- asm -*- file.  */
	.section ".text"
        .global _start_rom

_start_rom:
  	# Load the stack.
        lis   %r1,__stack_end@h
        ori   %r1,%r1,__stack_end@l

        # Allocate a cleared frame
        li    %r0,0
	stwu  %r0,-16(1)
        stw   %r0,12(1)

        # Copy sdata2
	lis   %r11,(__sdata2_start - 1)@ha
	la    %r11,(__sdata2_start - 1)@l(%r11)
	lis   %r10,(__sdata2_end)@ha
        la    %r10,(__sdata2_end)@l(%r10)
        subf  %r10,%r11,%r10
	lis   %r8,(__sdata2_load - 1)@ha
	la    %r8,(__sdata2_load - 1)@l(%r8)
	mtctr %r10
	b     1f
0:      lbzu  %r4,1(%r8)
	stbu  %r4,1(%r11)
1:
	bdnz  0b

        # Copy data
	lis   %r11,(__data_start - 1)@ha
	la    %r11,(__data_start - 1)@l(%r11)
	lis   %r10,(__data_end)@ha
        la    %r10,(__data_end)@l(%r10)
        subf  %r10,%r11,%r10
	lis   %r8,(__data_load - 1)@ha
	la    %r8,(__data_load - 1)@l(%r8)
	mtctr %r10
	b     1f
0:      lbzu  %r4,1(%r8)
	stbu  %r4,1(%r11)
1:
	bdnz  0b

        bl    _setup

#ifdef CALL__init
	# Call constructors
	bl    __init
#endif
	
        # Run the program
	bl    main

        # Try to stop the machine
        bl    _exit

	.size _start_rom, . - _start_rom

	.section ".reset","ax"
_reset:
	b _start_rom
	.size _reset, . - _reset
