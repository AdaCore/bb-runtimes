// First level: 2 (w/ 64 entries), max VA: 2**27
	.p2align 12
__mmu_part1_l2_000000000:
	.dword 0x00000000020006fd  // for 0x00000000, RAM
	.dword 0x00000000022006fd  // for 0x00200000, RAM
	.dword 0x00000000024006fd  // for 0x00400000, RAM
	.dword 0x00000000026006fd  // for 0x00600000, RAM
	.dword 0x00000000028006fd  // for 0x00800000, RAM
	.dword 0x0000000002a006fd  // for 0x00a00000, RAM
	.dword 0x0000000002c006fd  // for 0x00c00000, RAM
	.dword 0x0000000002e006fd  // for 0x00e00000, RAM
	.dword 0x00000000030006fd  // for 0x01000000, RAM
	.dword 0x00000000032006fd  // for 0x01200000, RAM
	.dword 0x00000000034006fd  // for 0x01400000, RAM
	.dword 0x00000000036006fd  // for 0x01600000, RAM
	.dword 0x00000000038006fd  // for 0x01800000, RAM
	.dword 0x0000000003a006fd  // for 0x01a00000, RAM
	.dword 0x0000000003c006fd  // for 0x01c00000, RAM
	.dword 0x0000000003e006fd  // for 0x01e00000, RAM
	.dword 0x00000000040006fd  // for 0x02000000, RAM
	.dword 0x00000000042006fd  // for 0x02200000, RAM
	.dword 0x00000000044006fd  // for 0x02400000, RAM
	.dword 0x00000000046006fd  // for 0x02600000, RAM
	.dword 0x00000000048006fd  // for 0x02800000, RAM
	.dword 0x0000000004a006fd  // for 0x02a00000, RAM
	.dword 0x0000000004c006fd  // for 0x02c00000, RAM
	.dword 0x0000000004e006fd  // for 0x02e00000, RAM
	.dword 0x00000000050006fd  // for 0x03000000, RAM
	.dword 0x00000000052006fd  // for 0x03200000, RAM
	.dword 0x00000000054006fd  // for 0x03400000, RAM
	.dword 0x00000000056006fd  // for 0x03600000, RAM
	.dword 0x00000000058006fd  // for 0x03800000, RAM
	.dword 0x0000000005a006fd  // for 0x03a00000, RAM
	.dword 0x0000000005c006fd  // for 0x03c00000, RAM
	.dword 0x0000000005e006fd  // for 0x03e00000, RAM
	.dword 0x00000000060006fd  // for 0x04000000, RAM
	.dword 0x00000000062006fd  // for 0x04200000, RAM
	.dword 0x00000000064006fd  // for 0x04400000, RAM
	.dword 0x00000000066006fd  // for 0x04600000, RAM
	.dword 0x00000000068006fd  // for 0x04800000, RAM
	.dword 0x0000000006a006fd  // for 0x04a00000, RAM
	.dword 0x0000000006c006fd  // for 0x04c00000, RAM
	.dword 0x0000000006e006fd  // for 0x04e00000, RAM
	.dword 0x00000000070006fd  // for 0x05000000, RAM
	.dword 0x00000000072006fd  // for 0x05200000, RAM
	.dword 0x00000000074006fd  // for 0x05400000, RAM
	.dword 0x00000000076006fd  // for 0x05600000, RAM
	.dword 0x00000000078006fd  // for 0x05800000, RAM
	.dword 0x0000000007a006fd  // for 0x05a00000, RAM
	.dword 0x0000000007c006fd  // for 0x05c00000, RAM
	.dword 0x0000000007e006fd  // for 0x05e00000, RAM
	.dword 0x00000000080006fd  // for 0x06000000, RAM
	.dword 0x00000000082006fd  // for 0x06200000, RAM
	.dword 0x00000000084006fd  // for 0x06400000, RAM
	.dword 0x00000000086006fd  // for 0x06600000, RAM
	.dword 0x00000000088006fd  // for 0x06800000, RAM
	.dword 0x0000000008a006fd  // for 0x06a00000, RAM
	.dword 0x0000000008c006fd  // for 0x06c00000, RAM
	.dword 0x0000000008e006fd  // for 0x06e00000, RAM
	.dword 0x00000000090006fd  // for 0x07000000, RAM
	.dword 0x00000000092006fd  // for 0x07200000, RAM
	.dword 0x00000000094006fd  // for 0x07400000, RAM
	.dword 0x00000000096006fd  // for 0x07600000, RAM
	.dword 0x00000000098006fd  // for 0x07800000, RAM
	.dword 0x0000000009a006fd  // for 0x07a00000, RAM
	.dword 0x0000000009c006fd  // for 0x07c00000, RAM
	.dword 0x0000000009e006fd  // for 0x07e00000, RAM
	.type __memmap_part1, @object
__memmap_part1:
	.dword 0x0
	.dword 0x2000000
	.dword 0x8000000
	.size __memmap_part1, . - __memmap_part1
	.type __files_part1_name_0, @object
__files_part1_name_0:
	.asciz "monitor.bin"
	.size __files_part1_name_0, . - __files_part1_name_0
	.type __files_part1_content_0, @object
__files_part1_content_0:
	.incbin "monitor.bin"
__files_part1_content_0_end:
	.size __files_part1_content_0, . - __files_part1_content_0

	.type __files_part1, @object
__files_part1:
	.dword __files_part1_name_0
	.dword __files_part1_content_0
	.dword __files_part1_content_0_end - __files_part1_content_0
	.size __files_part1, . - __files_part1

	.globl __dir_part1
	.type __dir_part1, @object
__dir_part1:
	.dword __mmu_part1_l2_000000000
	.dword __memmap_part1
	.word 1
	.word 0
	.dword __files_part1
	.word 1
	.word 0
	.size __dir_part1, . - __dir_part1

