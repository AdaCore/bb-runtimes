	.cpu cortex-r4f
	.eabi_attribute 27, 3
	.eabi_attribute 28, 1
	.fpu vfpv3-d16
	.eabi_attribute 20, 1
	.eabi_attribute 21, 1
	.eabi_attribute 23, 3
	.eabi_attribute 24, 1
	.eabi_attribute 25, 1
	.eabi_attribute 26, 1
	.eabi_attribute 30, 2
	.eabi_attribute 34, 1
	.eabi_attribute 18, 4

	.text
	.align	2
	.global	PartitionMain
	.type	PartitionMain, %function
PartitionMain:
	b	main
	.size	PartitionMain, .-PartitionMain

