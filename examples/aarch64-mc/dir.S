// First level: 2 (w/ 32 entries), max VA: 2**26
	.p2align 12
__mmu_cpu_default_l2_000000000:
	.dword 0x00000000000006fd  // for 0x00000000, ram
	.dword 0x00000000002006fd  // for 0x00200000, ram
	.dword 0x00000000004006fd  // for 0x00400000, ram
	.dword 0x00000000006006fd  // for 0x00600000, ram
	.dword 0x00000000008006fd  // for 0x00800000, ram
	.dword 0x0000000000a006fd  // for 0x00a00000, ram
	.dword 0x0000000000c006fd  // for 0x00c00000, ram
	.dword 0x0000000000e006fd  // for 0x00e00000, ram
	.dword 0x00000000010006fd  // for 0x01000000, ram
	.dword 0x00000000012006fd  // for 0x01200000, ram
	.dword 0x00000000014006fd  // for 0x01400000, ram
	.dword 0x00000000016006fd  // for 0x01600000, ram
	.dword 0x00000000018006fd  // for 0x01800000, ram
	.dword 0x0000000001a006fd  // for 0x01a00000, ram
	.dword 0x0000000001c006fd  // for 0x01c00000, ram
	.dword 0x0000000001e006fd  // for 0x01e00000, ram
	.dword 0x000000000200067d  // for 0x02000000, rom
	.dword 0x000000000220067d  // for 0x02200000, rom
	.dword 0x000000000240067d  // for 0x02400000, rom
	.dword 0x000000000260067d  // for 0x02600000, rom
	.dword 0x000000000280067d  // for 0x02800000, rom
	.dword 0x0000000002a0067d  // for 0x02a00000, rom
	.dword 0x0000000002c0067d  // for 0x02c00000, rom
	.dword 0x0000000002e0067d  // for 0x02e00000, rom
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
// First level: 2 (w/ 128 entries), max VA: 2**28
	.p2align 12
__mmu_cpu_1_l2_000000000:
	.dword 0x00000000030006fd  // for 0x00000000, ram
	.dword 0x00000000032006fd  // for 0x00200000, ram
	.dword 0x00000000034006fd  // for 0x00400000, ram
	.dword 0x00000000036006fd  // for 0x00600000, ram
	.dword 0x00000000038006fd  // for 0x00800000, ram
	.dword 0x0000000003a006fd  // for 0x00a00000, ram
	.dword 0x0000000003c006fd  // for 0x00c00000, ram
	.dword 0x0000000003e006fd  // for 0x00e00000, ram
	.dword 0x00000000040006fd  // for 0x01000000, ram
	.dword 0x00000000042006fd  // for 0x01200000, ram
	.dword 0x00000000044006fd  // for 0x01400000, ram
	.dword 0x00000000046006fd  // for 0x01600000, ram
	.dword 0x00000000048006fd  // for 0x01800000, ram
	.dword 0x0000000004a006fd  // for 0x01a00000, ram
	.dword 0x0000000004c006fd  // for 0x01c00000, ram
	.dword 0x0000000004e006fd  // for 0x01e00000, ram
	.dword 0x00000000050006fd  // for 0x02000000, ram
	.dword 0x00000000052006fd  // for 0x02200000, ram
	.dword 0x00000000054006fd  // for 0x02400000, ram
	.dword 0x00000000056006fd  // for 0x02600000, ram
	.dword 0x00000000058006fd  // for 0x02800000, ram
	.dword 0x0000000005a006fd  // for 0x02a00000, ram
	.dword 0x0000000005c006fd  // for 0x02c00000, ram
	.dword 0x0000000005e006fd  // for 0x02e00000, ram
	.dword 0x00000000060006fd  // for 0x03000000, ram
	.dword 0x00000000062006fd  // for 0x03200000, ram
	.dword 0x00000000064006fd  // for 0x03400000, ram
	.dword 0x00000000066006fd  // for 0x03600000, ram
	.dword 0x00000000068006fd  // for 0x03800000, ram
	.dword 0x0000000006a006fd  // for 0x03a00000, ram
	.dword 0x0000000006c006fd  // for 0x03c00000, ram
	.dword 0x0000000006e006fd  // for 0x03e00000, ram
	.dword 0x00000000070006fd  // for 0x04000000, ram
	.dword 0x00000000072006fd  // for 0x04200000, ram
	.dword 0x00000000074006fd  // for 0x04400000, ram
	.dword 0x00000000076006fd  // for 0x04600000, ram
	.dword 0x00000000078006fd  // for 0x04800000, ram
	.dword 0x0000000007a006fd  // for 0x04a00000, ram
	.dword 0x0000000007c006fd  // for 0x04c00000, ram
	.dword 0x0000000007e006fd  // for 0x04e00000, ram
	.dword 0x00000000080006fd  // for 0x05000000, ram
	.dword 0x00000000082006fd  // for 0x05200000, ram
	.dword 0x00000000084006fd  // for 0x05400000, ram
	.dword 0x00000000086006fd  // for 0x05600000, ram
	.dword 0x00000000088006fd  // for 0x05800000, ram
	.dword 0x0000000008a006fd  // for 0x05a00000, ram
	.dword 0x0000000008c006fd  // for 0x05c00000, ram
	.dword 0x0000000008e006fd  // for 0x05e00000, ram
	.dword 0x00000000090006fd  // for 0x06000000, ram
	.dword 0x00000000092006fd  // for 0x06200000, ram
	.dword 0x00000000094006fd  // for 0x06400000, ram
	.dword 0x00000000096006fd  // for 0x06600000, ram
	.dword 0x00000000098006fd  // for 0x06800000, ram
	.dword 0x0000000009a006fd  // for 0x06a00000, ram
	.dword 0x0000000009c006fd  // for 0x06c00000, ram
	.dword 0x0000000009e006fd  // for 0x06e00000, ram
	.dword 0x000000000a0006fd  // for 0x07000000, ram
	.dword 0x000000000a2006fd  // for 0x07200000, ram
	.dword 0x000000000a4006fd  // for 0x07400000, ram
	.dword 0x000000000a6006fd  // for 0x07600000, ram
	.dword 0x000000000a8006fd  // for 0x07800000, ram
	.dword 0x000000000aa006fd  // for 0x07a00000, ram
	.dword 0x000000000ac006fd  // for 0x07c00000, ram
	.dword 0x000000000ae006fd  // for 0x07e00000, ram
	.dword 0x000000000b00067d  // for 0x08000000, rom
	.dword 0x000000000b20067d  // for 0x08200000, rom
	.dword 0x000000000b40067d  // for 0x08400000, rom
	.dword 0x000000000b60067d  // for 0x08600000, rom
	.dword 0x000000000b80067d  // for 0x08800000, rom
	.dword 0x000000000ba0067d  // for 0x08a00000, rom
	.dword 0x000000000bc0067d  // for 0x08c00000, rom
	.dword 0x000000000be0067d  // for 0x08e00000, rom
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.globl __dir_cpu_default
	.type __dir_cpu_default, @object
__dir_cpu_default:
	.dword __mmu_cpu_default_l2_000000000
	.dword 0x0
	.dword 0x0
	.dword 0x2000000
	.dword 0x2000000
	.dword 0x2000000
	.dword 0x1000000
	.dword 0
	.dword 0
	.size __dir_cpu_default, . - __dir_cpu_default

	.globl __dir_cpu_1
	.type __dir_cpu_1, @object
__dir_cpu_1:
	.dword __mmu_cpu_1_l2_000000000
	.dword 0x0
	.dword 0x3000000
	.dword 0x8000000
	.dword 0x8000000
	.dword 0xb000000
	.dword 0x1000000
	.dword __file_cpu_1
	.dword __file_cpu_1_end - __file_cpu_1
	.size __dir_cpu_1, . - __dir_cpu_1

	.type __file_cpu_1, @object
__file_cpu_1:
	.incbin "monitor.bin"
__file_cpu_1_end:
	.size __file_cpu_1, . - __file_cpu_1

