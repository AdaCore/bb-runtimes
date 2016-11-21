/*****************************************************************************
**                                                                          **
**                               GNAT EXAMPLE                               **
**                                                                          **
**                        Copyright (C) 2016, AdaCore                       **
**                                                                          **
** GNAT is free software;  you can  redistribute it  and/or modify it under **
** terms of the  GNU General Public License as published  by the Free Soft- **
** ware  Foundation;  either version 2,  or (at your option) any later ver- **
** sion.  GNAT is distributed in the hope that it will be useful, but WITH- **
** OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY **
** or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License **
** for  more details.  You should have  received  a copy of the GNU General **
** Public License  distributed with GNAT;  see file COPYING.  If not, write **
** to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, **
** Boston, MA 02110-1301, USA.                                              **
**                                                                          **
** As a special exception,  if other files  instantiate  generics from this **
** unit, or you link  this unit with other files  to produce an executable, **
** this  unit  does not  by itself cause  the resulting  executable  to  be **
** covered  by the  GNU  General  Public  License.  This exception does not **
** however invalidate  any other reasons why  the executable file  might be **
** covered by the  GNU Public License.                                      **
**                                                                          **
** GNAT was originally developed  by the GNAT team at  New York University. **
** Extensive contributions were provided by Ada Core Technologies Inc.      **
**                                                                          **
******************************************************************************/

	.text

	.macro handler, name, num
	.globl \name
	.type \name, %function
\name:
	sub	sp, sp, #0x40
	str	lr, [sp, #0x3c]
	stm	sp,{r0-r14}^
	mov	r0, sp
	mov	r1, #\num
	bl	__gnat_trap_dump
	.size \name, . - \name
	.endm

	handler __gnat_undef_trap, 1
	handler __gnat_svc_trap, 2
	handler __gnat_pabt_trap, 3
	handler __gnat_dabt_trap, 4
	handler __gnat_irq_trap, 6
	handler __gnat_fiq_trap, 7

        .bss
	.p2align 4
__cpu0_mon_stack_start:
	.skip 	512
__cpu0_mon_stack_end:

	.text

	.macro mon_handler, name, num
	.globl \name
	.type \name, %function
\name:
	ldr	sp,=__cpu0_mon_stack_end
	sub	sp, sp, #0x40
	str	lr, [sp, #0x3c]
	stm	sp,{r0-r14}^
	mov	r0, sp
	mov	r1, #\num
	bl	__gnat_trap_dump
	.size \name, . - \name
	.endm

	mon_handler __gnat_undef_trap_mon, 9
	mon_handler __gnat_smc_trap_mon, 10
	mon_handler __gnat_pabt_trap_mon, 11
	mon_handler __gnat_dabt_trap_mon, 12
	mon_handler __gnat_irq_trap_mon, 14
	mon_handler __gnat_fiq_trap_mon, 15

	# For the monitor
	.arch armv7ve

	.globl __gnat_smc_switch_mon
	.type __gnat_smc_switch_mon, %function
__gnat_smc_switch_mon:
	mrs	sp, sp_usr
	mov	r2, lr
	mrs	lr, lr_usr
	mov	r1, sp
	bx	r2
	.size __gnat_smc_switch_mon, . - __gnat_smc_switch_mon

