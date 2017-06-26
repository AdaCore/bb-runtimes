	.section .rodata,"a"
	.type __files_1_name, @object
__files_1_name:
	.asciz "dtb"
	.size __files_1_name, . - __files_1_name
	.type __files_1_content, @object
__files_1_content:
	.incbin "../linux/mc.dtb"
__files_1_content_end:
	.size __files_1_content, . - __files_1_content
	.type __files_2_name, @object
__files_2_name:
	.asciz "initrd"
	.size __files_2_name, . - __files_2_name
	.type __files_2_content, @object
__files_2_content:
	.incbin "../linux/initramfs.cpio.gz"
__files_2_content_end:
	.size __files_2_content, . - __files_2_content
	.type __files_3_name, @object
__files_3_name:
	.asciz "linux"
	.size __files_3_name, . - __files_3_name
	.type __files_3_content, @object
__files_3_content:
	.incbin "../linux/Image"
__files_3_content_end:
	.size __files_3_content, . - __files_3_content

// First level: 1 (w/ 2 entries), max VA: 2**31
	.p2align 12
__mmu_part1_l2_000040000:
	.dword 0x00000000020006fd  // for 0x40000000, RAM
	.dword 0x00000000022006fd  // for 0x40200000, RAM
	.dword 0x00000000024006fd  // for 0x40400000, RAM
	.dword 0x00000000026006fd  // for 0x40600000, RAM
	.dword 0x00000000028006fd  // for 0x40800000, RAM
	.dword 0x0000000002a006fd  // for 0x40a00000, RAM
	.dword 0x0000000002c006fd  // for 0x40c00000, RAM
	.dword 0x0000000002e006fd  // for 0x40e00000, RAM
	.dword 0x00000000030006fd  // for 0x41000000, RAM
	.dword 0x00000000032006fd  // for 0x41200000, RAM
	.dword 0x00000000034006fd  // for 0x41400000, RAM
	.dword 0x00000000036006fd  // for 0x41600000, RAM
	.dword 0x00000000038006fd  // for 0x41800000, RAM
	.dword 0x0000000003a006fd  // for 0x41a00000, RAM
	.dword 0x0000000003c006fd  // for 0x41c00000, RAM
	.dword 0x0000000003e006fd  // for 0x41e00000, RAM
	.dword 0x00000000040006fd  // for 0x42000000, RAM
	.dword 0x00000000042006fd  // for 0x42200000, RAM
	.dword 0x00000000044006fd  // for 0x42400000, RAM
	.dword 0x00000000046006fd  // for 0x42600000, RAM
	.dword 0x00000000048006fd  // for 0x42800000, RAM
	.dword 0x0000000004a006fd  // for 0x42a00000, RAM
	.dword 0x0000000004c006fd  // for 0x42c00000, RAM
	.dword 0x0000000004e006fd  // for 0x42e00000, RAM
	.dword 0x00000000050006fd  // for 0x43000000, RAM
	.dword 0x00000000052006fd  // for 0x43200000, RAM
	.dword 0x00000000054006fd  // for 0x43400000, RAM
	.dword 0x00000000056006fd  // for 0x43600000, RAM
	.dword 0x00000000058006fd  // for 0x43800000, RAM
	.dword 0x0000000005a006fd  // for 0x43a00000, RAM
	.dword 0x0000000005c006fd  // for 0x43c00000, RAM
	.dword 0x0000000005e006fd  // for 0x43e00000, RAM
	.dword 0x00000000060006fd  // for 0x44000000, RAM
	.dword 0x00000000062006fd  // for 0x44200000, RAM
	.dword 0x00000000064006fd  // for 0x44400000, RAM
	.dword 0x00000000066006fd  // for 0x44600000, RAM
	.dword 0x00000000068006fd  // for 0x44800000, RAM
	.dword 0x0000000006a006fd  // for 0x44a00000, RAM
	.dword 0x0000000006c006fd  // for 0x44c00000, RAM
	.dword 0x0000000006e006fd  // for 0x44e00000, RAM
	.dword 0x00000000070006fd  // for 0x45000000, RAM
	.dword 0x00000000072006fd  // for 0x45200000, RAM
	.dword 0x00000000074006fd  // for 0x45400000, RAM
	.dword 0x00000000076006fd  // for 0x45600000, RAM
	.dword 0x00000000078006fd  // for 0x45800000, RAM
	.dword 0x0000000007a006fd  // for 0x45a00000, RAM
	.dword 0x0000000007c006fd  // for 0x45c00000, RAM
	.dword 0x0000000007e006fd  // for 0x45e00000, RAM
	.dword 0x00000000080006fd  // for 0x46000000, RAM
	.dword 0x00000000082006fd  // for 0x46200000, RAM
	.dword 0x00000000084006fd  // for 0x46400000, RAM
	.dword 0x00000000086006fd  // for 0x46600000, RAM
	.dword 0x00000000088006fd  // for 0x46800000, RAM
	.dword 0x0000000008a006fd  // for 0x46a00000, RAM
	.dword 0x0000000008c006fd  // for 0x46c00000, RAM
	.dword 0x0000000008e006fd  // for 0x46e00000, RAM
	.dword 0x00000000090006fd  // for 0x47000000, RAM
	.dword 0x00000000092006fd  // for 0x47200000, RAM
	.dword 0x00000000094006fd  // for 0x47400000, RAM
	.dword 0x00000000096006fd  // for 0x47600000, RAM
	.dword 0x00000000098006fd  // for 0x47800000, RAM
	.dword 0x0000000009a006fd  // for 0x47a00000, RAM
	.dword 0x0000000009c006fd  // for 0x47c00000, RAM
	.dword 0x0000000009e006fd  // for 0x47e00000, RAM
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
	.p2align 12
__mmu_part1_l1_000040000:
	.dword 0
	.dword __mmu_part1_l2_000040000 + 0x3
// First level: 1 (w/ 2 entries), max VA: 2**31
	.p2align 12
__mmu_part2_l2_000040000:
	.dword 0x000000000a0006fd  // for 0x40000000, RAM
	.dword 0x000000000a2006fd  // for 0x40200000, RAM
	.dword 0x000000000a4006fd  // for 0x40400000, RAM
	.dword 0x000000000a6006fd  // for 0x40600000, RAM
	.dword 0x000000000a8006fd  // for 0x40800000, RAM
	.dword 0x000000000aa006fd  // for 0x40a00000, RAM
	.dword 0x000000000ac006fd  // for 0x40c00000, RAM
	.dword 0x000000000ae006fd  // for 0x40e00000, RAM
	.dword 0x000000000b0006fd  // for 0x41000000, RAM
	.dword 0x000000000b2006fd  // for 0x41200000, RAM
	.dword 0x000000000b4006fd  // for 0x41400000, RAM
	.dword 0x000000000b6006fd  // for 0x41600000, RAM
	.dword 0x000000000b8006fd  // for 0x41800000, RAM
	.dword 0x000000000ba006fd  // for 0x41a00000, RAM
	.dword 0x000000000bc006fd  // for 0x41c00000, RAM
	.dword 0x000000000be006fd  // for 0x41e00000, RAM
	.dword 0x000000000c0006fd  // for 0x42000000, RAM
	.dword 0x000000000c2006fd  // for 0x42200000, RAM
	.dword 0x000000000c4006fd  // for 0x42400000, RAM
	.dword 0x000000000c6006fd  // for 0x42600000, RAM
	.dword 0x000000000c8006fd  // for 0x42800000, RAM
	.dword 0x000000000ca006fd  // for 0x42a00000, RAM
	.dword 0x000000000cc006fd  // for 0x42c00000, RAM
	.dword 0x000000000ce006fd  // for 0x42e00000, RAM
	.dword 0x000000000d0006fd  // for 0x43000000, RAM
	.dword 0x000000000d2006fd  // for 0x43200000, RAM
	.dword 0x000000000d4006fd  // for 0x43400000, RAM
	.dword 0x000000000d6006fd  // for 0x43600000, RAM
	.dword 0x000000000d8006fd  // for 0x43800000, RAM
	.dword 0x000000000da006fd  // for 0x43a00000, RAM
	.dword 0x000000000dc006fd  // for 0x43c00000, RAM
	.dword 0x000000000de006fd  // for 0x43e00000, RAM
	.dword 0x000000000e0006fd  // for 0x44000000, RAM
	.dword 0x000000000e2006fd  // for 0x44200000, RAM
	.dword 0x000000000e4006fd  // for 0x44400000, RAM
	.dword 0x000000000e6006fd  // for 0x44600000, RAM
	.dword 0x000000000e8006fd  // for 0x44800000, RAM
	.dword 0x000000000ea006fd  // for 0x44a00000, RAM
	.dword 0x000000000ec006fd  // for 0x44c00000, RAM
	.dword 0x000000000ee006fd  // for 0x44e00000, RAM
	.dword 0x000000000f0006fd  // for 0x45000000, RAM
	.dword 0x000000000f2006fd  // for 0x45200000, RAM
	.dword 0x000000000f4006fd  // for 0x45400000, RAM
	.dword 0x000000000f6006fd  // for 0x45600000, RAM
	.dword 0x000000000f8006fd  // for 0x45800000, RAM
	.dword 0x000000000fa006fd  // for 0x45a00000, RAM
	.dword 0x000000000fc006fd  // for 0x45c00000, RAM
	.dword 0x000000000fe006fd  // for 0x45e00000, RAM
	.dword 0x00000000100006fd  // for 0x46000000, RAM
	.dword 0x00000000102006fd  // for 0x46200000, RAM
	.dword 0x00000000104006fd  // for 0x46400000, RAM
	.dword 0x00000000106006fd  // for 0x46600000, RAM
	.dword 0x00000000108006fd  // for 0x46800000, RAM
	.dword 0x0000000010a006fd  // for 0x46a00000, RAM
	.dword 0x0000000010c006fd  // for 0x46c00000, RAM
	.dword 0x0000000010e006fd  // for 0x46e00000, RAM
	.dword 0x00000000110006fd  // for 0x47000000, RAM
	.dword 0x00000000112006fd  // for 0x47200000, RAM
	.dword 0x00000000114006fd  // for 0x47400000, RAM
	.dword 0x00000000116006fd  // for 0x47600000, RAM
	.dword 0x00000000118006fd  // for 0x47800000, RAM
	.dword 0x0000000011a006fd  // for 0x47a00000, RAM
	.dword 0x0000000011c006fd  // for 0x47c00000, RAM
	.dword 0x0000000011e006fd  // for 0x47e00000, RAM
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
	.p2align 12
__mmu_part2_l1_000040000:
	.dword 0
	.dword __mmu_part2_l2_000040000 + 0x3
	.type __memmap_part1, @object
__memmap_part1:
	.dword 0x40000000
	.dword 0x2000000
	.dword 0x8000000
	.size __memmap_part1, . - __memmap_part1
	.type __files_part1, @object
__files_part1:
	.dword __files_1_name
	.dword __files_1_content
	.dword __files_1_content_end - __files_1_content
	.dword __files_3_name
	.dword __files_3_content
	.dword __files_3_content_end - __files_3_content
	.dword __files_2_name
	.dword __files_2_content
	.dword __files_2_content_end - __files_2_content
	.size __files_part1, . - __files_part1

	.globl __dir_part1
	.type __dir_part1, @object
__dir_part1:
	.dword __mmu_part1_l1_000040000
	.dword __memmap_part1
	.word 1
	.word 0
	.dword __files_part1
	.word 3
	.word 0
	.size __dir_part1, . - __dir_part1

	.type __memmap_part2, @object
__memmap_part2:
	.dword 0x40000000
	.dword 0xa000000
	.dword 0x8000000
	.size __memmap_part2, . - __memmap_part2
	.type __files_part2, @object
__files_part2:
	.dword __files_1_name
	.dword __files_1_content
	.dword __files_1_content_end - __files_1_content
	.dword __files_3_name
	.dword __files_3_content
	.dword __files_3_content_end - __files_3_content
	.dword __files_2_name
	.dword __files_2_content
	.dword __files_2_content_end - __files_2_content
	.size __files_part2, . - __files_part2

	.globl __dir_part2
	.type __dir_part2, @object
__dir_part2:
	.dword __mmu_part2_l1_000040000
	.dword __memmap_part2
	.word 1
	.word 0
	.dword __files_part2
	.word 3
	.word 0
	.size __dir_part2, . - __dir_part2

