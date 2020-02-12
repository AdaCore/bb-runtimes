.section .mmu_tbl2,"a"

// First level: 1 (w/ 4 entries), max VA: 2**32
	.p2align 12
__mmu_l2_0000c0000:
	.dword 0x00600000c0000665  // for 0xc0000000, qspi
	.dword 0x00600000c0200665  // for 0xc0200000, qspi
	.dword 0x00600000c0400665  // for 0xc0400000, qspi
	.dword 0x00600000c0600665  // for 0xc0600000, qspi
	.dword 0x00600000c0800665  // for 0xc0800000, qspi
	.dword 0x00600000c0a00665  // for 0xc0a00000, qspi
	.dword 0x00600000c0c00665  // for 0xc0c00000, qspi
	.dword 0x00600000c0e00665  // for 0xc0e00000, qspi
	.dword 0x00600000c1000665  // for 0xc1000000, qspi
	.dword 0x00600000c1200665  // for 0xc1200000, qspi
	.dword 0x00600000c1400665  // for 0xc1400000, qspi
	.dword 0x00600000c1600665  // for 0xc1600000, qspi
	.dword 0x00600000c1800665  // for 0xc1800000, qspi
	.dword 0x00600000c1a00665  // for 0xc1a00000, qspi
	.dword 0x00600000c1c00665  // for 0xc1c00000, qspi
	.dword 0x00600000c1e00665  // for 0xc1e00000, qspi
	.dword 0x00600000c2000665  // for 0xc2000000, qspi
	.dword 0x00600000c2200665  // for 0xc2200000, qspi
	.dword 0x00600000c2400665  // for 0xc2400000, qspi
	.dword 0x00600000c2600665  // for 0xc2600000, qspi
	.dword 0x00600000c2800665  // for 0xc2800000, qspi
	.dword 0x00600000c2a00665  // for 0xc2a00000, qspi
	.dword 0x00600000c2c00665  // for 0xc2c00000, qspi
	.dword 0x00600000c2e00665  // for 0xc2e00000, qspi
	.dword 0x00600000c3000665  // for 0xc3000000, qspi
	.dword 0x00600000c3200665  // for 0xc3200000, qspi
	.dword 0x00600000c3400665  // for 0xc3400000, qspi
	.dword 0x00600000c3600665  // for 0xc3600000, qspi
	.dword 0x00600000c3800665  // for 0xc3800000, qspi
	.dword 0x00600000c3a00665  // for 0xc3a00000, qspi
	.dword 0x00600000c3c00665  // for 0xc3c00000, qspi
	.dword 0x00600000c3e00665  // for 0xc3e00000, qspi
	.dword 0x00600000c4000665  // for 0xc4000000, qspi
	.dword 0x00600000c4200665  // for 0xc4200000, qspi
	.dword 0x00600000c4400665  // for 0xc4400000, qspi
	.dword 0x00600000c4600665  // for 0xc4600000, qspi
	.dword 0x00600000c4800665  // for 0xc4800000, qspi
	.dword 0x00600000c4a00665  // for 0xc4a00000, qspi
	.dword 0x00600000c4c00665  // for 0xc4c00000, qspi
	.dword 0x00600000c4e00665  // for 0xc4e00000, qspi
	.dword 0x00600000c5000665  // for 0xc5000000, qspi
	.dword 0x00600000c5200665  // for 0xc5200000, qspi
	.dword 0x00600000c5400665  // for 0xc5400000, qspi
	.dword 0x00600000c5600665  // for 0xc5600000, qspi
	.dword 0x00600000c5800665  // for 0xc5800000, qspi
	.dword 0x00600000c5a00665  // for 0xc5a00000, qspi
	.dword 0x00600000c5c00665  // for 0xc5c00000, qspi
	.dword 0x00600000c5e00665  // for 0xc5e00000, qspi
	.dword 0x00600000c6000665  // for 0xc6000000, qspi
	.dword 0x00600000c6200665  // for 0xc6200000, qspi
	.dword 0x00600000c6400665  // for 0xc6400000, qspi
	.dword 0x00600000c6600665  // for 0xc6600000, qspi
	.dword 0x00600000c6800665  // for 0xc6800000, qspi
	.dword 0x00600000c6a00665  // for 0xc6a00000, qspi
	.dword 0x00600000c6c00665  // for 0xc6c00000, qspi
	.dword 0x00600000c6e00665  // for 0xc6e00000, qspi
	.dword 0x00600000c7000665  // for 0xc7000000, qspi
	.dword 0x00600000c7200665  // for 0xc7200000, qspi
	.dword 0x00600000c7400665  // for 0xc7400000, qspi
	.dword 0x00600000c7600665  // for 0xc7600000, qspi
	.dword 0x00600000c7800665  // for 0xc7800000, qspi
	.dword 0x00600000c7a00665  // for 0xc7a00000, qspi
	.dword 0x00600000c7c00665  // for 0xc7c00000, qspi
	.dword 0x00600000c7e00665  // for 0xc7e00000, qspi
	.dword 0x00600000c8000665  // for 0xc8000000, qspi
	.dword 0x00600000c8200665  // for 0xc8200000, qspi
	.dword 0x00600000c8400665  // for 0xc8400000, qspi
	.dword 0x00600000c8600665  // for 0xc8600000, qspi
	.dword 0x00600000c8800665  // for 0xc8800000, qspi
	.dword 0x00600000c8a00665  // for 0xc8a00000, qspi
	.dword 0x00600000c8c00665  // for 0xc8c00000, qspi
	.dword 0x00600000c8e00665  // for 0xc8e00000, qspi
	.dword 0x00600000c9000665  // for 0xc9000000, qspi
	.dword 0x00600000c9200665  // for 0xc9200000, qspi
	.dword 0x00600000c9400665  // for 0xc9400000, qspi
	.dword 0x00600000c9600665  // for 0xc9600000, qspi
	.dword 0x00600000c9800665  // for 0xc9800000, qspi
	.dword 0x00600000c9a00665  // for 0xc9a00000, qspi
	.dword 0x00600000c9c00665  // for 0xc9c00000, qspi
	.dword 0x00600000c9e00665  // for 0xc9e00000, qspi
	.dword 0x00600000ca000665  // for 0xca000000, qspi
	.dword 0x00600000ca200665  // for 0xca200000, qspi
	.dword 0x00600000ca400665  // for 0xca400000, qspi
	.dword 0x00600000ca600665  // for 0xca600000, qspi
	.dword 0x00600000ca800665  // for 0xca800000, qspi
	.dword 0x00600000caa00665  // for 0xcaa00000, qspi
	.dword 0x00600000cac00665  // for 0xcac00000, qspi
	.dword 0x00600000cae00665  // for 0xcae00000, qspi
	.dword 0x00600000cb000665  // for 0xcb000000, qspi
	.dword 0x00600000cb200665  // for 0xcb200000, qspi
	.dword 0x00600000cb400665  // for 0xcb400000, qspi
	.dword 0x00600000cb600665  // for 0xcb600000, qspi
	.dword 0x00600000cb800665  // for 0xcb800000, qspi
	.dword 0x00600000cba00665  // for 0xcba00000, qspi
	.dword 0x00600000cbc00665  // for 0xcbc00000, qspi
	.dword 0x00600000cbe00665  // for 0xcbe00000, qspi
	.dword 0x00600000cc000665  // for 0xcc000000, qspi
	.dword 0x00600000cc200665  // for 0xcc200000, qspi
	.dword 0x00600000cc400665  // for 0xcc400000, qspi
	.dword 0x00600000cc600665  // for 0xcc600000, qspi
	.dword 0x00600000cc800665  // for 0xcc800000, qspi
	.dword 0x00600000cca00665  // for 0xcca00000, qspi
	.dword 0x00600000ccc00665  // for 0xccc00000, qspi
	.dword 0x00600000cce00665  // for 0xcce00000, qspi
	.dword 0x00600000cd000665  // for 0xcd000000, qspi
	.dword 0x00600000cd200665  // for 0xcd200000, qspi
	.dword 0x00600000cd400665  // for 0xcd400000, qspi
	.dword 0x00600000cd600665  // for 0xcd600000, qspi
	.dword 0x00600000cd800665  // for 0xcd800000, qspi
	.dword 0x00600000cda00665  // for 0xcda00000, qspi
	.dword 0x00600000cdc00665  // for 0xcdc00000, qspi
	.dword 0x00600000cde00665  // for 0xcde00000, qspi
	.dword 0x00600000ce000665  // for 0xce000000, qspi
	.dword 0x00600000ce200665  // for 0xce200000, qspi
	.dword 0x00600000ce400665  // for 0xce400000, qspi
	.dword 0x00600000ce600665  // for 0xce600000, qspi
	.dword 0x00600000ce800665  // for 0xce800000, qspi
	.dword 0x00600000cea00665  // for 0xcea00000, qspi
	.dword 0x00600000cec00665  // for 0xcec00000, qspi
	.dword 0x00600000cee00665  // for 0xcee00000, qspi
	.dword 0x00600000cf000665  // for 0xcf000000, qspi
	.dword 0x00600000cf200665  // for 0xcf200000, qspi
	.dword 0x00600000cf400665  // for 0xcf400000, qspi
	.dword 0x00600000cf600665  // for 0xcf600000, qspi
	.dword 0x00600000cf800665  // for 0xcf800000, qspi
	.dword 0x00600000cfa00665  // for 0xcfa00000, qspi
	.dword 0x00600000cfc00665  // for 0xcfc00000, qspi
	.dword 0x00600000cfe00665  // for 0xcfe00000, qspi
	.dword 0x00600000d0000665  // for 0xd0000000, qspi
	.dword 0x00600000d0200665  // for 0xd0200000, qspi
	.dword 0x00600000d0400665  // for 0xd0400000, qspi
	.dword 0x00600000d0600665  // for 0xd0600000, qspi
	.dword 0x00600000d0800665  // for 0xd0800000, qspi
	.dword 0x00600000d0a00665  // for 0xd0a00000, qspi
	.dword 0x00600000d0c00665  // for 0xd0c00000, qspi
	.dword 0x00600000d0e00665  // for 0xd0e00000, qspi
	.dword 0x00600000d1000665  // for 0xd1000000, qspi
	.dword 0x00600000d1200665  // for 0xd1200000, qspi
	.dword 0x00600000d1400665  // for 0xd1400000, qspi
	.dword 0x00600000d1600665  // for 0xd1600000, qspi
	.dword 0x00600000d1800665  // for 0xd1800000, qspi
	.dword 0x00600000d1a00665  // for 0xd1a00000, qspi
	.dword 0x00600000d1c00665  // for 0xd1c00000, qspi
	.dword 0x00600000d1e00665  // for 0xd1e00000, qspi
	.dword 0x00600000d2000665  // for 0xd2000000, qspi
	.dword 0x00600000d2200665  // for 0xd2200000, qspi
	.dword 0x00600000d2400665  // for 0xd2400000, qspi
	.dword 0x00600000d2600665  // for 0xd2600000, qspi
	.dword 0x00600000d2800665  // for 0xd2800000, qspi
	.dword 0x00600000d2a00665  // for 0xd2a00000, qspi
	.dword 0x00600000d2c00665  // for 0xd2c00000, qspi
	.dword 0x00600000d2e00665  // for 0xd2e00000, qspi
	.dword 0x00600000d3000665  // for 0xd3000000, qspi
	.dword 0x00600000d3200665  // for 0xd3200000, qspi
	.dword 0x00600000d3400665  // for 0xd3400000, qspi
	.dword 0x00600000d3600665  // for 0xd3600000, qspi
	.dword 0x00600000d3800665  // for 0xd3800000, qspi
	.dword 0x00600000d3a00665  // for 0xd3a00000, qspi
	.dword 0x00600000d3c00665  // for 0xd3c00000, qspi
	.dword 0x00600000d3e00665  // for 0xd3e00000, qspi
	.dword 0x00600000d4000665  // for 0xd4000000, qspi
	.dword 0x00600000d4200665  // for 0xd4200000, qspi
	.dword 0x00600000d4400665  // for 0xd4400000, qspi
	.dword 0x00600000d4600665  // for 0xd4600000, qspi
	.dword 0x00600000d4800665  // for 0xd4800000, qspi
	.dword 0x00600000d4a00665  // for 0xd4a00000, qspi
	.dword 0x00600000d4c00665  // for 0xd4c00000, qspi
	.dword 0x00600000d4e00665  // for 0xd4e00000, qspi
	.dword 0x00600000d5000665  // for 0xd5000000, qspi
	.dword 0x00600000d5200665  // for 0xd5200000, qspi
	.dword 0x00600000d5400665  // for 0xd5400000, qspi
	.dword 0x00600000d5600665  // for 0xd5600000, qspi
	.dword 0x00600000d5800665  // for 0xd5800000, qspi
	.dword 0x00600000d5a00665  // for 0xd5a00000, qspi
	.dword 0x00600000d5c00665  // for 0xd5c00000, qspi
	.dword 0x00600000d5e00665  // for 0xd5e00000, qspi
	.dword 0x00600000d6000665  // for 0xd6000000, qspi
	.dword 0x00600000d6200665  // for 0xd6200000, qspi
	.dword 0x00600000d6400665  // for 0xd6400000, qspi
	.dword 0x00600000d6600665  // for 0xd6600000, qspi
	.dword 0x00600000d6800665  // for 0xd6800000, qspi
	.dword 0x00600000d6a00665  // for 0xd6a00000, qspi
	.dword 0x00600000d6c00665  // for 0xd6c00000, qspi
	.dword 0x00600000d6e00665  // for 0xd6e00000, qspi
	.dword 0x00600000d7000665  // for 0xd7000000, qspi
	.dword 0x00600000d7200665  // for 0xd7200000, qspi
	.dword 0x00600000d7400665  // for 0xd7400000, qspi
	.dword 0x00600000d7600665  // for 0xd7600000, qspi
	.dword 0x00600000d7800665  // for 0xd7800000, qspi
	.dword 0x00600000d7a00665  // for 0xd7a00000, qspi
	.dword 0x00600000d7c00665  // for 0xd7c00000, qspi
	.dword 0x00600000d7e00665  // for 0xd7e00000, qspi
	.dword 0x00600000d8000665  // for 0xd8000000, qspi
	.dword 0x00600000d8200665  // for 0xd8200000, qspi
	.dword 0x00600000d8400665  // for 0xd8400000, qspi
	.dword 0x00600000d8600665  // for 0xd8600000, qspi
	.dword 0x00600000d8800665  // for 0xd8800000, qspi
	.dword 0x00600000d8a00665  // for 0xd8a00000, qspi
	.dword 0x00600000d8c00665  // for 0xd8c00000, qspi
	.dword 0x00600000d8e00665  // for 0xd8e00000, qspi
	.dword 0x00600000d9000665  // for 0xd9000000, qspi
	.dword 0x00600000d9200665  // for 0xd9200000, qspi
	.dword 0x00600000d9400665  // for 0xd9400000, qspi
	.dword 0x00600000d9600665  // for 0xd9600000, qspi
	.dword 0x00600000d9800665  // for 0xd9800000, qspi
	.dword 0x00600000d9a00665  // for 0xd9a00000, qspi
	.dword 0x00600000d9c00665  // for 0xd9c00000, qspi
	.dword 0x00600000d9e00665  // for 0xd9e00000, qspi
	.dword 0x00600000da000665  // for 0xda000000, qspi
	.dword 0x00600000da200665  // for 0xda200000, qspi
	.dword 0x00600000da400665  // for 0xda400000, qspi
	.dword 0x00600000da600665  // for 0xda600000, qspi
	.dword 0x00600000da800665  // for 0xda800000, qspi
	.dword 0x00600000daa00665  // for 0xdaa00000, qspi
	.dword 0x00600000dac00665  // for 0xdac00000, qspi
	.dword 0x00600000dae00665  // for 0xdae00000, qspi
	.dword 0x00600000db000665  // for 0xdb000000, qspi
	.dword 0x00600000db200665  // for 0xdb200000, qspi
	.dword 0x00600000db400665  // for 0xdb400000, qspi
	.dword 0x00600000db600665  // for 0xdb600000, qspi
	.dword 0x00600000db800665  // for 0xdb800000, qspi
	.dword 0x00600000dba00665  // for 0xdba00000, qspi
	.dword 0x00600000dbc00665  // for 0xdbc00000, qspi
	.dword 0x00600000dbe00665  // for 0xdbe00000, qspi
	.dword 0x00600000dc000665  // for 0xdc000000, qspi
	.dword 0x00600000dc200665  // for 0xdc200000, qspi
	.dword 0x00600000dc400665  // for 0xdc400000, qspi
	.dword 0x00600000dc600665  // for 0xdc600000, qspi
	.dword 0x00600000dc800665  // for 0xdc800000, qspi
	.dword 0x00600000dca00665  // for 0xdca00000, qspi
	.dword 0x00600000dcc00665  // for 0xdcc00000, qspi
	.dword 0x00600000dce00665  // for 0xdce00000, qspi
	.dword 0x00600000dd000665  // for 0xdd000000, qspi
	.dword 0x00600000dd200665  // for 0xdd200000, qspi
	.dword 0x00600000dd400665  // for 0xdd400000, qspi
	.dword 0x00600000dd600665  // for 0xdd600000, qspi
	.dword 0x00600000dd800665  // for 0xdd800000, qspi
	.dword 0x00600000dda00665  // for 0xdda00000, qspi
	.dword 0x00600000ddc00665  // for 0xddc00000, qspi
	.dword 0x00600000dde00665  // for 0xdde00000, qspi
	.dword 0x00600000de000665  // for 0xde000000, qspi
	.dword 0x00600000de200665  // for 0xde200000, qspi
	.dword 0x00600000de400665  // for 0xde400000, qspi
	.dword 0x00600000de600665  // for 0xde600000, qspi
	.dword 0x00600000de800665  // for 0xde800000, qspi
	.dword 0x00600000dea00665  // for 0xdea00000, qspi
	.dword 0x00600000dec00665  // for 0xdec00000, qspi
	.dword 0x00600000dee00665  // for 0xdee00000, qspi
	.dword 0x00600000df000665  // for 0xdf000000, qspi
	.dword 0x00600000df200665  // for 0xdf200000, qspi
	.dword 0x00600000df400665  // for 0xdf400000, qspi
	.dword 0x00600000df600665  // for 0xdf600000, qspi
	.dword 0x00600000df800665  // for 0xdf800000, qspi
	.dword 0x00600000dfa00665  // for 0xdfa00000, qspi
	.dword 0x00600000dfc00665  // for 0xdfc00000, qspi
	.dword 0x00600000dfe00665  // for 0xdfe00000, qspi
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0
	.dword 0x00600000f8000665  // for 0xf8000000, io
	.dword 0x00600000f8200665  // for 0xf8200000, io
	.dword 0x00600000f8400665  // for 0xf8400000, io
	.dword 0x00600000f8600665  // for 0xf8600000, io
	.dword 0x00600000f8800665  // for 0xf8800000, io
	.dword 0x00600000f8a00665  // for 0xf8a00000, io
	.dword 0x00600000f8c00665  // for 0xf8c00000, io
	.dword 0x00600000f8e00665  // for 0xf8e00000, io
	.dword 0x00600000f9000665  // for 0xf9000000, io
	.dword 0x00600000f9200665  // for 0xf9200000, io
	.dword 0x00600000f9400665  // for 0xf9400000, io
	.dword 0x00600000f9600665  // for 0xf9600000, io
	.dword 0x00600000f9800665  // for 0xf9800000, io
	.dword 0x00600000f9a00665  // for 0xf9a00000, io
	.dword 0x00600000f9c00665  // for 0xf9c00000, io
	.dword 0x00600000f9e00665  // for 0xf9e00000, io
	.dword 0x00600000fa000665  // for 0xfa000000, io
	.dword 0x00600000fa200665  // for 0xfa200000, io
	.dword 0x00600000fa400665  // for 0xfa400000, io
	.dword 0x00600000fa600665  // for 0xfa600000, io
	.dword 0x00600000fa800665  // for 0xfa800000, io
	.dword 0x00600000faa00665  // for 0xfaa00000, io
	.dword 0x00600000fac00665  // for 0xfac00000, io
	.dword 0x00600000fae00665  // for 0xfae00000, io
	.dword 0x00600000fb000665  // for 0xfb000000, io
	.dword 0x00600000fb200665  // for 0xfb200000, io
	.dword 0x00600000fb400665  // for 0xfb400000, io
	.dword 0x00600000fb600665  // for 0xfb600000, io
	.dword 0x00600000fb800665  // for 0xfb800000, io
	.dword 0x00600000fba00665  // for 0xfba00000, io
	.dword 0x00600000fbc00665  // for 0xfbc00000, io
	.dword 0x00600000fbe00665  // for 0xfbe00000, io
	.dword 0x00600000fc000665  // for 0xfc000000, io
	.dword 0x00600000fc200665  // for 0xfc200000, io
	.dword 0x00600000fc400665  // for 0xfc400000, io
	.dword 0x00600000fc600665  // for 0xfc600000, io
	.dword 0x00600000fc800665  // for 0xfc800000, io
	.dword 0x00600000fca00665  // for 0xfca00000, io
	.dword 0x00600000fcc00665  // for 0xfcc00000, io
	.dword 0x00600000fce00665  // for 0xfce00000, io
	.dword 0x00600000fd000665  // for 0xfd000000, io
	.dword 0x00600000fd200665  // for 0xfd200000, io
	.dword 0x00600000fd400665  // for 0xfd400000, io
	.dword 0x00600000fd600665  // for 0xfd600000, io
	.dword 0x00600000fd800665  // for 0xfd800000, io
	.dword 0x00600000fda00665  // for 0xfda00000, io
	.dword 0x00600000fdc00665  // for 0xfdc00000, io
	.dword 0x00600000fde00665  // for 0xfde00000, io
	.dword 0x00600000fe000665  // for 0xfe000000, io
	.dword 0x00600000fe200665  // for 0xfe200000, io
	.dword 0x00600000fe400665  // for 0xfe400000, io
	.dword 0x00600000fe600665  // for 0xfe600000, io
	.dword 0x00600000fe800665  // for 0xfe800000, io
	.dword 0x00600000fea00665  // for 0xfea00000, io
	.dword 0x00600000fec00665  // for 0xfec00000, io
	.dword 0x00600000fee00665  // for 0xfee00000, io
	.dword 0x00600000ff000665  // for 0xff000000, io
	.dword 0x00600000ff200665  // for 0xff200000, io
	.dword 0x00600000ff400665  // for 0xff400000, io
	.dword 0x00600000ff600665  // for 0xff600000, io
	.dword 0x00600000ff800665  // for 0xff800000, io
	.dword 0x00600000ffa00665  // for 0xffa00000, io
	.dword 0x00600000ffc00665  // for 0xffc00000, io
	.dword 0x00600000ffe00665  // for 0xffe00000, io

.section .mmu_tbl1,"a"

	.p2align 12
__mmu_l1_000000000:
	.dword 0x0000000000000621  // for 0x00000000, ram
	.dword 0x0000000040000621  // for 0x40000000, ram
	.dword 0x0060000080000665  // for 0x80000000, pl
	.dword __mmu_l2_0000c0000 + 0x3
