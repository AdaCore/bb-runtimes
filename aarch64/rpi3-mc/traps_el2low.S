#include "traps_common.h"
	
	.section .vects_el2_low,"ax"
	.globl  __vectors_el2_low
__vectors_el2_low:
	//  Lower level, aarch64
	ehandler __trap_el2_syn_a64, 9
	ehandler __trap_el2_irq_a64, 10
	ehandler __trap_el2_fiq_a64, 11
	ehandler __trap_el2_ser_a64, 12

	ehandler __trap_el2_syn_a32, 13
	ehandler __trap_el2_irq_a32, 14
	ehandler __trap_el2_fiq_a32, 15
	ehandler __trap_el2_ser_a32, 16
