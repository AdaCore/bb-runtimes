ENTRY(_start)
SECTIONS
{
	/*
	 * On the MPC5634:
	 *   0x40000000 - 0x40007fff SRAM, standby
	 *   0x40008000 - 0x400177ff SRAM
	 */
	. = 0x40000000;
	.text          : { *(.text .text.*) }
	.data ALIGN(8) : { *(.data .sdata .sdata2) }
	__bss_start = .;
	.bss ALIGN(8)  : { *(.bss .sbss) }
	__bss_end = .;
PROVIDE (__stack = 0x400177f0);
}
