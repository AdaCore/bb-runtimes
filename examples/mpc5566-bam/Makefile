CROSS=powerpc-eabispe-
all: esci.bin

esci.bin: esci.elf
	$(CROSS)objcopy -O binary $< $@

esci.elf: esci.o bam.ld
	$(CROSS)ld -o $@ $< -Tbam.ld -Map esci.map

esci.o: esci.S
	$(CROSS)gcc -c -o $@ $<

clean:
	$(RM) esci.o esci.elf esci.bin *~ *.map
