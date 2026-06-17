#include "traps_common.h"
	
	.section .vects_el3_cur,"ax"
	.globl __vectors_el3_cur
	.type  __vectors_el3_cur, @function
__vectors_el3_cur:
	//  EL0 stack
	ehandler __trap_el3_syn_el0, 1
	ehandler __trap_el3_irq_el0, 2
	ehandler __trap_el3_fiq_el0, 3
	ehandler __trap_el3_ser_el0, 4

	//  Current exception level
	ehandler __trap_el3_syn_elx, 5
	ehandler __trap_el3_irq_elx, 6
	ehandler __trap_el3_fiq_elx, 7
	ehandler __trap_el3_ser_elx, 8

	.section .vects_el3_low,"ax"
	.globl	 __vectors_el3_low
	.type	 __vectors_el3_low, @function
__vectors_el3_low:
	//  Lower level, aarch64
	ehandler __trap_el3_syn_a64, 9
	ehandler __trap_el3_irq_a64, 10
	ehandler __trap_el3_fiq_a64, 11
	ehandler __trap_el3_ser_a64, 12

	ehandler __trap_el3_syn_a32, 13
	ehandler __trap_el3_irq_a32, 14
	ehandler __trap_el3_fiq_a32, 15
	ehandler __trap_el3_ser_a32, 16
