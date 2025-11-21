// Automatically generated from memmap.xml
//  cmd line: memmap.py --el1 --arch aarch64 memmap.xml

// First level: 1 (w/ 4 entries), max VA: 2**32
	.p2align 12
__mmu_l1_000000000:
	.dword 0x0060000000000665  // for 0x00000000, pl
	.dword 0x0060000040000665  // for 0x40000000, pl
	.dword 0x0000000080000621  // for 0x80000000, ram
	.dword 0x00000000c0000621  // for 0xc0000000, ram
__mmu_tcr = 0x00000020
