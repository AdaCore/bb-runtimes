// First level: 1 (w/ 2 entries), max VA: 2**31
	.p2align 12
__mmu_l2_000000000:
	.quad 0x0000000000000661  // for 0x00000000, ram
	.quad 0x0000000000200661  // for 0x00200000, ram
	.quad 0x0000000000400661  // for 0x00400000, ram
	.quad 0x0000000000600661  // for 0x00600000, ram
	.quad 0x0000000000800661  // for 0x00800000, ram
	.quad 0x0000000000a00661  // for 0x00a00000, ram
	.quad 0x0000000000c00661  // for 0x00c00000, ram
	.quad 0x0000000000e00661  // for 0x00e00000, ram
	.quad 0x0000000001000661  // for 0x01000000, ram
	.quad 0x0000000001200661  // for 0x01200000, ram
	.quad 0x0000000001400661  // for 0x01400000, ram
	.quad 0x0000000001600661  // for 0x01600000, ram
	.quad 0x0000000001800661  // for 0x01800000, ram
	.quad 0x0000000001a00661  // for 0x01a00000, ram
	.quad 0x0000000001c00661  // for 0x01c00000, ram
	.quad 0x0000000001e00661  // for 0x01e00000, ram
	.quad 0x0000000002000661  // for 0x02000000, ram
	.quad 0x0000000002200661  // for 0x02200000, ram
	.quad 0x0000000002400661  // for 0x02400000, ram
	.quad 0x0000000002600661  // for 0x02600000, ram
	.quad 0x0000000002800661  // for 0x02800000, ram
	.quad 0x0000000002a00661  // for 0x02a00000, ram
	.quad 0x0000000002c00661  // for 0x02c00000, ram
	.quad 0x0000000002e00661  // for 0x02e00000, ram
	.quad 0x0000000003000661  // for 0x03000000, ram
	.quad 0x0000000003200661  // for 0x03200000, ram
	.quad 0x0000000003400661  // for 0x03400000, ram
	.quad 0x0000000003600661  // for 0x03600000, ram
	.quad 0x0000000003800661  // for 0x03800000, ram
	.quad 0x0000000003a00661  // for 0x03a00000, ram
	.quad 0x0000000003c00661  // for 0x03c00000, ram
	.quad 0x0000000003e00661  // for 0x03e00000, ram
	.quad 0x0000000004000661  // for 0x04000000, ram
	.quad 0x0000000004200661  // for 0x04200000, ram
	.quad 0x0000000004400661  // for 0x04400000, ram
	.quad 0x0000000004600661  // for 0x04600000, ram
	.quad 0x0000000004800661  // for 0x04800000, ram
	.quad 0x0000000004a00661  // for 0x04a00000, ram
	.quad 0x0000000004c00661  // for 0x04c00000, ram
	.quad 0x0000000004e00661  // for 0x04e00000, ram
	.quad 0x0000000005000661  // for 0x05000000, ram
	.quad 0x0000000005200661  // for 0x05200000, ram
	.quad 0x0000000005400661  // for 0x05400000, ram
	.quad 0x0000000005600661  // for 0x05600000, ram
	.quad 0x0000000005800661  // for 0x05800000, ram
	.quad 0x0000000005a00661  // for 0x05a00000, ram
	.quad 0x0000000005c00661  // for 0x05c00000, ram
	.quad 0x0000000005e00661  // for 0x05e00000, ram
	.quad 0x0000000006000661  // for 0x06000000, ram
	.quad 0x0000000006200661  // for 0x06200000, ram
	.quad 0x0000000006400661  // for 0x06400000, ram
	.quad 0x0000000006600661  // for 0x06600000, ram
	.quad 0x0000000006800661  // for 0x06800000, ram
	.quad 0x0000000006a00661  // for 0x06a00000, ram
	.quad 0x0000000006c00661  // for 0x06c00000, ram
	.quad 0x0000000006e00661  // for 0x06e00000, ram
	.quad 0x0000000007000661  // for 0x07000000, ram
	.quad 0x0000000007200661  // for 0x07200000, ram
	.quad 0x0000000007400661  // for 0x07400000, ram
	.quad 0x0000000007600661  // for 0x07600000, ram
	.quad 0x0000000007800661  // for 0x07800000, ram
	.quad 0x0000000007a00661  // for 0x07a00000, ram
	.quad 0x0000000007c00661  // for 0x07c00000, ram
	.quad 0x0000000007e00661  // for 0x07e00000, ram
	.quad 0x0000000008000661  // for 0x08000000, ram
	.quad 0x0000000008200661  // for 0x08200000, ram
	.quad 0x0000000008400661  // for 0x08400000, ram
	.quad 0x0000000008600661  // for 0x08600000, ram
	.quad 0x0000000008800661  // for 0x08800000, ram
	.quad 0x0000000008a00661  // for 0x08a00000, ram
	.quad 0x0000000008c00661  // for 0x08c00000, ram
	.quad 0x0000000008e00661  // for 0x08e00000, ram
	.quad 0x0000000009000661  // for 0x09000000, ram
	.quad 0x0000000009200661  // for 0x09200000, ram
	.quad 0x0000000009400661  // for 0x09400000, ram
	.quad 0x0000000009600661  // for 0x09600000, ram
	.quad 0x0000000009800661  // for 0x09800000, ram
	.quad 0x0000000009a00661  // for 0x09a00000, ram
	.quad 0x0000000009c00661  // for 0x09c00000, ram
	.quad 0x0000000009e00661  // for 0x09e00000, ram
	.quad 0x000000000a000661  // for 0x0a000000, ram
	.quad 0x000000000a200661  // for 0x0a200000, ram
	.quad 0x000000000a400661  // for 0x0a400000, ram
	.quad 0x000000000a600661  // for 0x0a600000, ram
	.quad 0x000000000a800661  // for 0x0a800000, ram
	.quad 0x000000000aa00661  // for 0x0aa00000, ram
	.quad 0x000000000ac00661  // for 0x0ac00000, ram
	.quad 0x000000000ae00661  // for 0x0ae00000, ram
	.quad 0x000000000b000661  // for 0x0b000000, ram
	.quad 0x000000000b200661  // for 0x0b200000, ram
	.quad 0x000000000b400661  // for 0x0b400000, ram
	.quad 0x000000000b600661  // for 0x0b600000, ram
	.quad 0x000000000b800661  // for 0x0b800000, ram
	.quad 0x000000000ba00661  // for 0x0ba00000, ram
	.quad 0x000000000bc00661  // for 0x0bc00000, ram
	.quad 0x000000000be00661  // for 0x0be00000, ram
	.quad 0x000000000c000661  // for 0x0c000000, ram
	.quad 0x000000000c200661  // for 0x0c200000, ram
	.quad 0x000000000c400661  // for 0x0c400000, ram
	.quad 0x000000000c600661  // for 0x0c600000, ram
	.quad 0x000000000c800661  // for 0x0c800000, ram
	.quad 0x000000000ca00661  // for 0x0ca00000, ram
	.quad 0x000000000cc00661  // for 0x0cc00000, ram
	.quad 0x000000000ce00661  // for 0x0ce00000, ram
	.quad 0x000000000d000661  // for 0x0d000000, ram
	.quad 0x000000000d200661  // for 0x0d200000, ram
	.quad 0x000000000d400661  // for 0x0d400000, ram
	.quad 0x000000000d600661  // for 0x0d600000, ram
	.quad 0x000000000d800661  // for 0x0d800000, ram
	.quad 0x000000000da00661  // for 0x0da00000, ram
	.quad 0x000000000dc00661  // for 0x0dc00000, ram
	.quad 0x000000000de00661  // for 0x0de00000, ram
	.quad 0x000000000e000661  // for 0x0e000000, ram
	.quad 0x000000000e200661  // for 0x0e200000, ram
	.quad 0x000000000e400661  // for 0x0e400000, ram
	.quad 0x000000000e600661  // for 0x0e600000, ram
	.quad 0x000000000e800661  // for 0x0e800000, ram
	.quad 0x000000000ea00661  // for 0x0ea00000, ram
	.quad 0x000000000ec00661  // for 0x0ec00000, ram
	.quad 0x000000000ee00661  // for 0x0ee00000, ram
	.quad 0x000000000f000661  // for 0x0f000000, ram
	.quad 0x000000000f200661  // for 0x0f200000, ram
	.quad 0x000000000f400661  // for 0x0f400000, ram
	.quad 0x000000000f600661  // for 0x0f600000, ram
	.quad 0x000000000f800661  // for 0x0f800000, ram
	.quad 0x000000000fa00661  // for 0x0fa00000, ram
	.quad 0x000000000fc00661  // for 0x0fc00000, ram
	.quad 0x000000000fe00661  // for 0x0fe00000, ram
	.quad 0x0000000010000661  // for 0x10000000, ram
	.quad 0x0000000010200661  // for 0x10200000, ram
	.quad 0x0000000010400661  // for 0x10400000, ram
	.quad 0x0000000010600661  // for 0x10600000, ram
	.quad 0x0000000010800661  // for 0x10800000, ram
	.quad 0x0000000010a00661  // for 0x10a00000, ram
	.quad 0x0000000010c00661  // for 0x10c00000, ram
	.quad 0x0000000010e00661  // for 0x10e00000, ram
	.quad 0x0000000011000661  // for 0x11000000, ram
	.quad 0x0000000011200661  // for 0x11200000, ram
	.quad 0x0000000011400661  // for 0x11400000, ram
	.quad 0x0000000011600661  // for 0x11600000, ram
	.quad 0x0000000011800661  // for 0x11800000, ram
	.quad 0x0000000011a00661  // for 0x11a00000, ram
	.quad 0x0000000011c00661  // for 0x11c00000, ram
	.quad 0x0000000011e00661  // for 0x11e00000, ram
	.quad 0x0000000012000661  // for 0x12000000, ram
	.quad 0x0000000012200661  // for 0x12200000, ram
	.quad 0x0000000012400661  // for 0x12400000, ram
	.quad 0x0000000012600661  // for 0x12600000, ram
	.quad 0x0000000012800661  // for 0x12800000, ram
	.quad 0x0000000012a00661  // for 0x12a00000, ram
	.quad 0x0000000012c00661  // for 0x12c00000, ram
	.quad 0x0000000012e00661  // for 0x12e00000, ram
	.quad 0x0000000013000661  // for 0x13000000, ram
	.quad 0x0000000013200661  // for 0x13200000, ram
	.quad 0x0000000013400661  // for 0x13400000, ram
	.quad 0x0000000013600661  // for 0x13600000, ram
	.quad 0x0000000013800661  // for 0x13800000, ram
	.quad 0x0000000013a00661  // for 0x13a00000, ram
	.quad 0x0000000013c00661  // for 0x13c00000, ram
	.quad 0x0000000013e00661  // for 0x13e00000, ram
	.quad 0x0000000014000661  // for 0x14000000, ram
	.quad 0x0000000014200661  // for 0x14200000, ram
	.quad 0x0000000014400661  // for 0x14400000, ram
	.quad 0x0000000014600661  // for 0x14600000, ram
	.quad 0x0000000014800661  // for 0x14800000, ram
	.quad 0x0000000014a00661  // for 0x14a00000, ram
	.quad 0x0000000014c00661  // for 0x14c00000, ram
	.quad 0x0000000014e00661  // for 0x14e00000, ram
	.quad 0x0000000015000661  // for 0x15000000, ram
	.quad 0x0000000015200661  // for 0x15200000, ram
	.quad 0x0000000015400661  // for 0x15400000, ram
	.quad 0x0000000015600661  // for 0x15600000, ram
	.quad 0x0000000015800661  // for 0x15800000, ram
	.quad 0x0000000015a00661  // for 0x15a00000, ram
	.quad 0x0000000015c00661  // for 0x15c00000, ram
	.quad 0x0000000015e00661  // for 0x15e00000, ram
	.quad 0x0000000016000661  // for 0x16000000, ram
	.quad 0x0000000016200661  // for 0x16200000, ram
	.quad 0x0000000016400661  // for 0x16400000, ram
	.quad 0x0000000016600661  // for 0x16600000, ram
	.quad 0x0000000016800661  // for 0x16800000, ram
	.quad 0x0000000016a00661  // for 0x16a00000, ram
	.quad 0x0000000016c00661  // for 0x16c00000, ram
	.quad 0x0000000016e00661  // for 0x16e00000, ram
	.quad 0x0000000017000661  // for 0x17000000, ram
	.quad 0x0000000017200661  // for 0x17200000, ram
	.quad 0x0000000017400661  // for 0x17400000, ram
	.quad 0x0000000017600661  // for 0x17600000, ram
	.quad 0x0000000017800661  // for 0x17800000, ram
	.quad 0x0000000017a00661  // for 0x17a00000, ram
	.quad 0x0000000017c00661  // for 0x17c00000, ram
	.quad 0x0000000017e00661  // for 0x17e00000, ram
	.quad 0x0000000018000661  // for 0x18000000, ram
	.quad 0x0000000018200661  // for 0x18200000, ram
	.quad 0x0000000018400661  // for 0x18400000, ram
	.quad 0x0000000018600661  // for 0x18600000, ram
	.quad 0x0000000018800661  // for 0x18800000, ram
	.quad 0x0000000018a00661  // for 0x18a00000, ram
	.quad 0x0000000018c00661  // for 0x18c00000, ram
	.quad 0x0000000018e00661  // for 0x18e00000, ram
	.quad 0x0000000019000661  // for 0x19000000, ram
	.quad 0x0000000019200661  // for 0x19200000, ram
	.quad 0x0000000019400661  // for 0x19400000, ram
	.quad 0x0000000019600661  // for 0x19600000, ram
	.quad 0x0000000019800661  // for 0x19800000, ram
	.quad 0x0000000019a00661  // for 0x19a00000, ram
	.quad 0x0000000019c00661  // for 0x19c00000, ram
	.quad 0x0000000019e00661  // for 0x19e00000, ram
	.quad 0x000000001a000661  // for 0x1a000000, ram
	.quad 0x000000001a200661  // for 0x1a200000, ram
	.quad 0x000000001a400661  // for 0x1a400000, ram
	.quad 0x000000001a600661  // for 0x1a600000, ram
	.quad 0x000000001a800661  // for 0x1a800000, ram
	.quad 0x000000001aa00661  // for 0x1aa00000, ram
	.quad 0x000000001ac00661  // for 0x1ac00000, ram
	.quad 0x000000001ae00661  // for 0x1ae00000, ram
	.quad 0x000000001b000661  // for 0x1b000000, ram
	.quad 0x000000001b200661  // for 0x1b200000, ram
	.quad 0x000000001b400661  // for 0x1b400000, ram
	.quad 0x000000001b600661  // for 0x1b600000, ram
	.quad 0x000000001b800661  // for 0x1b800000, ram
	.quad 0x000000001ba00661  // for 0x1ba00000, ram
	.quad 0x000000001bc00661  // for 0x1bc00000, ram
	.quad 0x000000001be00661  // for 0x1be00000, ram
	.quad 0x000000001c000661  // for 0x1c000000, ram
	.quad 0x000000001c200661  // for 0x1c200000, ram
	.quad 0x000000001c400661  // for 0x1c400000, ram
	.quad 0x000000001c600661  // for 0x1c600000, ram
	.quad 0x000000001c800661  // for 0x1c800000, ram
	.quad 0x000000001ca00661  // for 0x1ca00000, ram
	.quad 0x000000001cc00661  // for 0x1cc00000, ram
	.quad 0x000000001ce00661  // for 0x1ce00000, ram
	.quad 0x000000001d000661  // for 0x1d000000, ram
	.quad 0x000000001d200661  // for 0x1d200000, ram
	.quad 0x000000001d400661  // for 0x1d400000, ram
	.quad 0x000000001d600661  // for 0x1d600000, ram
	.quad 0x000000001d800661  // for 0x1d800000, ram
	.quad 0x000000001da00661  // for 0x1da00000, ram
	.quad 0x000000001dc00661  // for 0x1dc00000, ram
	.quad 0x000000001de00661  // for 0x1de00000, ram
	.quad 0x000000001e000661  // for 0x1e000000, ram
	.quad 0x000000001e200661  // for 0x1e200000, ram
	.quad 0x000000001e400661  // for 0x1e400000, ram
	.quad 0x000000001e600661  // for 0x1e600000, ram
	.quad 0x000000001e800661  // for 0x1e800000, ram
	.quad 0x000000001ea00661  // for 0x1ea00000, ram
	.quad 0x000000001ec00661  // for 0x1ec00000, ram
	.quad 0x000000001ee00661  // for 0x1ee00000, ram
	.quad 0x000000001f000661  // for 0x1f000000, ram
	.quad 0x000000001f200661  // for 0x1f200000, ram
	.quad 0x000000001f400661  // for 0x1f400000, ram
	.quad 0x000000001f600661  // for 0x1f600000, ram
	.quad 0x000000001f800661  // for 0x1f800000, ram
	.quad 0x000000001fa00661  // for 0x1fa00000, ram
	.quad 0x000000001fc00661  // for 0x1fc00000, ram
	.quad 0x000000001fe00661  // for 0x1fe00000, ram
	.quad 0x0000000020000661  // for 0x20000000, ram
	.quad 0x0000000020200661  // for 0x20200000, ram
	.quad 0x0000000020400661  // for 0x20400000, ram
	.quad 0x0000000020600661  // for 0x20600000, ram
	.quad 0x0000000020800661  // for 0x20800000, ram
	.quad 0x0000000020a00661  // for 0x20a00000, ram
	.quad 0x0000000020c00661  // for 0x20c00000, ram
	.quad 0x0000000020e00661  // for 0x20e00000, ram
	.quad 0x0000000021000661  // for 0x21000000, ram
	.quad 0x0000000021200661  // for 0x21200000, ram
	.quad 0x0000000021400661  // for 0x21400000, ram
	.quad 0x0000000021600661  // for 0x21600000, ram
	.quad 0x0000000021800661  // for 0x21800000, ram
	.quad 0x0000000021a00661  // for 0x21a00000, ram
	.quad 0x0000000021c00661  // for 0x21c00000, ram
	.quad 0x0000000021e00661  // for 0x21e00000, ram
	.quad 0x0000000022000661  // for 0x22000000, ram
	.quad 0x0000000022200661  // for 0x22200000, ram
	.quad 0x0000000022400661  // for 0x22400000, ram
	.quad 0x0000000022600661  // for 0x22600000, ram
	.quad 0x0000000022800661  // for 0x22800000, ram
	.quad 0x0000000022a00661  // for 0x22a00000, ram
	.quad 0x0000000022c00661  // for 0x22c00000, ram
	.quad 0x0000000022e00661  // for 0x22e00000, ram
	.quad 0x0000000023000661  // for 0x23000000, ram
	.quad 0x0000000023200661  // for 0x23200000, ram
	.quad 0x0000000023400661  // for 0x23400000, ram
	.quad 0x0000000023600661  // for 0x23600000, ram
	.quad 0x0000000023800661  // for 0x23800000, ram
	.quad 0x0000000023a00661  // for 0x23a00000, ram
	.quad 0x0000000023c00661  // for 0x23c00000, ram
	.quad 0x0000000023e00661  // for 0x23e00000, ram
	.quad 0x0000000024000661  // for 0x24000000, ram
	.quad 0x0000000024200661  // for 0x24200000, ram
	.quad 0x0000000024400661  // for 0x24400000, ram
	.quad 0x0000000024600661  // for 0x24600000, ram
	.quad 0x0000000024800661  // for 0x24800000, ram
	.quad 0x0000000024a00661  // for 0x24a00000, ram
	.quad 0x0000000024c00661  // for 0x24c00000, ram
	.quad 0x0000000024e00661  // for 0x24e00000, ram
	.quad 0x0000000025000661  // for 0x25000000, ram
	.quad 0x0000000025200661  // for 0x25200000, ram
	.quad 0x0000000025400661  // for 0x25400000, ram
	.quad 0x0000000025600661  // for 0x25600000, ram
	.quad 0x0000000025800661  // for 0x25800000, ram
	.quad 0x0000000025a00661  // for 0x25a00000, ram
	.quad 0x0000000025c00661  // for 0x25c00000, ram
	.quad 0x0000000025e00661  // for 0x25e00000, ram
	.quad 0x0000000026000661  // for 0x26000000, ram
	.quad 0x0000000026200661  // for 0x26200000, ram
	.quad 0x0000000026400661  // for 0x26400000, ram
	.quad 0x0000000026600661  // for 0x26600000, ram
	.quad 0x0000000026800661  // for 0x26800000, ram
	.quad 0x0000000026a00661  // for 0x26a00000, ram
	.quad 0x0000000026c00661  // for 0x26c00000, ram
	.quad 0x0000000026e00661  // for 0x26e00000, ram
	.quad 0x0000000027000661  // for 0x27000000, ram
	.quad 0x0000000027200661  // for 0x27200000, ram
	.quad 0x0000000027400661  // for 0x27400000, ram
	.quad 0x0000000027600661  // for 0x27600000, ram
	.quad 0x0000000027800661  // for 0x27800000, ram
	.quad 0x0000000027a00661  // for 0x27a00000, ram
	.quad 0x0000000027c00661  // for 0x27c00000, ram
	.quad 0x0000000027e00661  // for 0x27e00000, ram
	.quad 0x0000000028000661  // for 0x28000000, ram
	.quad 0x0000000028200661  // for 0x28200000, ram
	.quad 0x0000000028400661  // for 0x28400000, ram
	.quad 0x0000000028600661  // for 0x28600000, ram
	.quad 0x0000000028800661  // for 0x28800000, ram
	.quad 0x0000000028a00661  // for 0x28a00000, ram
	.quad 0x0000000028c00661  // for 0x28c00000, ram
	.quad 0x0000000028e00661  // for 0x28e00000, ram
	.quad 0x0000000029000661  // for 0x29000000, ram
	.quad 0x0000000029200661  // for 0x29200000, ram
	.quad 0x0000000029400661  // for 0x29400000, ram
	.quad 0x0000000029600661  // for 0x29600000, ram
	.quad 0x0000000029800661  // for 0x29800000, ram
	.quad 0x0000000029a00661  // for 0x29a00000, ram
	.quad 0x0000000029c00661  // for 0x29c00000, ram
	.quad 0x0000000029e00661  // for 0x29e00000, ram
	.quad 0x000000002a000661  // for 0x2a000000, ram
	.quad 0x000000002a200661  // for 0x2a200000, ram
	.quad 0x000000002a400661  // for 0x2a400000, ram
	.quad 0x000000002a600661  // for 0x2a600000, ram
	.quad 0x000000002a800661  // for 0x2a800000, ram
	.quad 0x000000002aa00661  // for 0x2aa00000, ram
	.quad 0x000000002ac00661  // for 0x2ac00000, ram
	.quad 0x000000002ae00661  // for 0x2ae00000, ram
	.quad 0x000000002b000661  // for 0x2b000000, ram
	.quad 0x000000002b200661  // for 0x2b200000, ram
	.quad 0x000000002b400661  // for 0x2b400000, ram
	.quad 0x000000002b600661  // for 0x2b600000, ram
	.quad 0x000000002b800661  // for 0x2b800000, ram
	.quad 0x000000002ba00661  // for 0x2ba00000, ram
	.quad 0x000000002bc00661  // for 0x2bc00000, ram
	.quad 0x000000002be00661  // for 0x2be00000, ram
	.quad 0x000000002c000661  // for 0x2c000000, ram
	.quad 0x000000002c200661  // for 0x2c200000, ram
	.quad 0x000000002c400661  // for 0x2c400000, ram
	.quad 0x000000002c600661  // for 0x2c600000, ram
	.quad 0x000000002c800661  // for 0x2c800000, ram
	.quad 0x000000002ca00661  // for 0x2ca00000, ram
	.quad 0x000000002cc00661  // for 0x2cc00000, ram
	.quad 0x000000002ce00661  // for 0x2ce00000, ram
	.quad 0x000000002d000661  // for 0x2d000000, ram
	.quad 0x000000002d200661  // for 0x2d200000, ram
	.quad 0x000000002d400661  // for 0x2d400000, ram
	.quad 0x000000002d600661  // for 0x2d600000, ram
	.quad 0x000000002d800661  // for 0x2d800000, ram
	.quad 0x000000002da00661  // for 0x2da00000, ram
	.quad 0x000000002dc00661  // for 0x2dc00000, ram
	.quad 0x000000002de00661  // for 0x2de00000, ram
	.quad 0x000000002e000661  // for 0x2e000000, ram
	.quad 0x000000002e200661  // for 0x2e200000, ram
	.quad 0x000000002e400661  // for 0x2e400000, ram
	.quad 0x000000002e600661  // for 0x2e600000, ram
	.quad 0x000000002e800661  // for 0x2e800000, ram
	.quad 0x000000002ea00661  // for 0x2ea00000, ram
	.quad 0x000000002ec00661  // for 0x2ec00000, ram
	.quad 0x000000002ee00661  // for 0x2ee00000, ram
	.quad 0x000000002f000661  // for 0x2f000000, ram
	.quad 0x000000002f200661  // for 0x2f200000, ram
	.quad 0x000000002f400661  // for 0x2f400000, ram
	.quad 0x000000002f600661  // for 0x2f600000, ram
	.quad 0x000000002f800661  // for 0x2f800000, ram
	.quad 0x000000002fa00661  // for 0x2fa00000, ram
	.quad 0x000000002fc00661  // for 0x2fc00000, ram
	.quad 0x000000002fe00661  // for 0x2fe00000, ram
	.quad 0x0000000030000661  // for 0x30000000, ram
	.quad 0x0000000030200661  // for 0x30200000, ram
	.quad 0x0000000030400661  // for 0x30400000, ram
	.quad 0x0000000030600661  // for 0x30600000, ram
	.quad 0x0000000030800661  // for 0x30800000, ram
	.quad 0x0000000030a00661  // for 0x30a00000, ram
	.quad 0x0000000030c00661  // for 0x30c00000, ram
	.quad 0x0000000030e00661  // for 0x30e00000, ram
	.quad 0x0000000031000661  // for 0x31000000, ram
	.quad 0x0000000031200661  // for 0x31200000, ram
	.quad 0x0000000031400661  // for 0x31400000, ram
	.quad 0x0000000031600661  // for 0x31600000, ram
	.quad 0x0000000031800661  // for 0x31800000, ram
	.quad 0x0000000031a00661  // for 0x31a00000, ram
	.quad 0x0000000031c00661  // for 0x31c00000, ram
	.quad 0x0000000031e00661  // for 0x31e00000, ram
	.quad 0x0000000032000661  // for 0x32000000, ram
	.quad 0x0000000032200661  // for 0x32200000, ram
	.quad 0x0000000032400661  // for 0x32400000, ram
	.quad 0x0000000032600661  // for 0x32600000, ram
	.quad 0x0000000032800661  // for 0x32800000, ram
	.quad 0x0000000032a00661  // for 0x32a00000, ram
	.quad 0x0000000032c00661  // for 0x32c00000, ram
	.quad 0x0000000032e00661  // for 0x32e00000, ram
	.quad 0x0000000033000661  // for 0x33000000, ram
	.quad 0x0000000033200661  // for 0x33200000, ram
	.quad 0x0000000033400661  // for 0x33400000, ram
	.quad 0x0000000033600661  // for 0x33600000, ram
	.quad 0x0000000033800661  // for 0x33800000, ram
	.quad 0x0000000033a00661  // for 0x33a00000, ram
	.quad 0x0000000033c00661  // for 0x33c00000, ram
	.quad 0x0000000033e00661  // for 0x33e00000, ram
	.quad 0x0000000034000661  // for 0x34000000, ram
	.quad 0x0000000034200661  // for 0x34200000, ram
	.quad 0x0000000034400661  // for 0x34400000, ram
	.quad 0x0000000034600661  // for 0x34600000, ram
	.quad 0x0000000034800661  // for 0x34800000, ram
	.quad 0x0000000034a00661  // for 0x34a00000, ram
	.quad 0x0000000034c00661  // for 0x34c00000, ram
	.quad 0x0000000034e00661  // for 0x34e00000, ram
	.quad 0x0000000035000661  // for 0x35000000, ram
	.quad 0x0000000035200661  // for 0x35200000, ram
	.quad 0x0000000035400661  // for 0x35400000, ram
	.quad 0x0000000035600661  // for 0x35600000, ram
	.quad 0x0000000035800661  // for 0x35800000, ram
	.quad 0x0000000035a00661  // for 0x35a00000, ram
	.quad 0x0000000035c00661  // for 0x35c00000, ram
	.quad 0x0000000035e00661  // for 0x35e00000, ram
	.quad 0x0000000036000661  // for 0x36000000, ram
	.quad 0x0000000036200661  // for 0x36200000, ram
	.quad 0x0000000036400661  // for 0x36400000, ram
	.quad 0x0000000036600661  // for 0x36600000, ram
	.quad 0x0000000036800661  // for 0x36800000, ram
	.quad 0x0000000036a00661  // for 0x36a00000, ram
	.quad 0x0000000036c00661  // for 0x36c00000, ram
	.quad 0x0000000036e00661  // for 0x36e00000, ram
	.quad 0x0000000037000661  // for 0x37000000, ram
	.quad 0x0000000037200661  // for 0x37200000, ram
	.quad 0x0000000037400661  // for 0x37400000, ram
	.quad 0x0000000037600661  // for 0x37600000, ram
	.quad 0x0000000037800661  // for 0x37800000, ram
	.quad 0x0000000037a00661  // for 0x37a00000, ram
	.quad 0x0000000037c00661  // for 0x37c00000, ram
	.quad 0x0000000037e00661  // for 0x37e00000, ram
	.quad 0x0000000038000661  // for 0x38000000, ram
	.quad 0x0000000038200661  // for 0x38200000, ram
	.quad 0x0000000038400661  // for 0x38400000, ram
	.quad 0x0000000038600661  // for 0x38600000, ram
	.quad 0x0000000038800661  // for 0x38800000, ram
	.quad 0x0000000038a00661  // for 0x38a00000, ram
	.quad 0x0000000038c00661  // for 0x38c00000, ram
	.quad 0x0000000038e00661  // for 0x38e00000, ram
	.quad 0x0000000039000661  // for 0x39000000, ram
	.quad 0x0000000039200661  // for 0x39200000, ram
	.quad 0x0000000039400661  // for 0x39400000, ram
	.quad 0x0000000039600661  // for 0x39600000, ram
	.quad 0x0000000039800661  // for 0x39800000, ram
	.quad 0x0000000039a00661  // for 0x39a00000, ram
	.quad 0x0000000039c00661  // for 0x39c00000, ram
	.quad 0x0000000039e00661  // for 0x39e00000, ram
	.quad 0x000000003a000661  // for 0x3a000000, ram
	.quad 0x000000003a200661  // for 0x3a200000, ram
	.quad 0x000000003a400661  // for 0x3a400000, ram
	.quad 0x000000003a600661  // for 0x3a600000, ram
	.quad 0x000000003a800661  // for 0x3a800000, ram
	.quad 0x000000003aa00661  // for 0x3aa00000, ram
	.quad 0x000000003ac00661  // for 0x3ac00000, ram
	.quad 0x004000003ae00665  // for 0x3ae00000, uc
	.quad 0x004000003b000665  // for 0x3b000000, vc
	.quad 0x004000003b200665  // for 0x3b200000, vc
	.quad 0x004000003b400665  // for 0x3b400000, vc
	.quad 0x004000003b600665  // for 0x3b600000, vc
	.quad 0x004000003b800665  // for 0x3b800000, vc
	.quad 0x004000003ba00665  // for 0x3ba00000, vc
	.quad 0x004000003bc00665  // for 0x3bc00000, vc
	.quad 0x004000003be00665  // for 0x3be00000, vc
	.quad 0x004000003c000665  // for 0x3c000000, vc
	.quad 0x004000003c200665  // for 0x3c200000, vc
	.quad 0x004000003c400665  // for 0x3c400000, vc
	.quad 0x004000003c600665  // for 0x3c600000, vc
	.quad 0x004000003c800665  // for 0x3c800000, vc
	.quad 0x004000003ca00665  // for 0x3ca00000, vc
	.quad 0x004000003cc00665  // for 0x3cc00000, vc
	.quad 0x004000003ce00665  // for 0x3ce00000, vc
	.quad 0x004000003d000665  // for 0x3d000000, vc
	.quad 0x004000003d200665  // for 0x3d200000, vc
	.quad 0x004000003d400665  // for 0x3d400000, vc
	.quad 0x004000003d600665  // for 0x3d600000, vc
	.quad 0x004000003d800665  // for 0x3d800000, vc
	.quad 0x004000003da00665  // for 0x3da00000, vc
	.quad 0x004000003dc00665  // for 0x3dc00000, vc
	.quad 0x004000003de00665  // for 0x3de00000, vc
	.quad 0x004000003e000665  // for 0x3e000000, vc
	.quad 0x004000003e200665  // for 0x3e200000, vc
	.quad 0x004000003e400665  // for 0x3e400000, vc
	.quad 0x004000003e600665  // for 0x3e600000, vc
	.quad 0x004000003e800665  // for 0x3e800000, vc
	.quad 0x004000003ea00665  // for 0x3ea00000, vc
	.quad 0x004000003ec00665  // for 0x3ec00000, vc
	.quad 0x004000003ee00665  // for 0x3ee00000, vc
	.quad 0x004000003f000665  // for 0x3f000000, io
	.quad 0x004000003f200665  // for 0x3f200000, io
	.quad 0x004000003f400665  // for 0x3f400000, io
	.quad 0x004000003f600665  // for 0x3f600000, io
	.quad 0x004000003f800665  // for 0x3f800000, io
	.quad 0x004000003fa00665  // for 0x3fa00000, io
	.quad 0x004000003fc00665  // for 0x3fc00000, io
	.quad 0x004000003fe00665  // for 0x3fe00000, io
	.p2align 12
__mmu_l2_000040000:
	.quad 0x0040000040000665  // for 0x40000000, io
	.quad 0x0040000040200665  // for 0x40200000, io
	.quad 0x0040000040400665  // for 0x40400000, io
	.quad 0x0040000040600665  // for 0x40600000, io
	.quad 0x0040000040800665  // for 0x40800000, io
	.quad 0x0040000040a00665  // for 0x40a00000, io
	.quad 0x0040000040c00665  // for 0x40c00000, io
	.quad 0x0040000040e00665  // for 0x40e00000, io
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.p2align 12
__mmu_l1_000000000:
	.word __mmu_l2_000000000 + 0x3
	.word 0x0
	.word __mmu_l2_000040000 + 0x3
	.word 0x0
__mmu_tcr = 0x00000021
