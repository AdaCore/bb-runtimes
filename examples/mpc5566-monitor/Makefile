TARGET=powerpc-eabispe

RTS=powerpc-eabispe/zfp-p5566
#RTS=../../install # For building in place

GPRBUILD_CMD=gprbuild --target=$(TARGET) --RTS=$(RTS) monitor_p5566.gpr

all: monitor.bin monitor-ram.srec monitor-flash.srec

monitor.bam: force
	$(GPRBUILD_CMD) -XLOADER=BAM -o $@

monitor.bin: monitor.bam
	$(TARGET)-objcopy -O binary $< $@

monitor.rom: force
	$(GPRBUILD_CMD) -XLOADER=FLASH -o $@

monitor.ram: force
	$(GPRBUILD_CMD) -XLOADER=EXTRAM -o $@

monitor-flash.srec: monitor.rom
	$(TARGET)-objcopy --srec-forceS3 -O srec $< $@

monitor-ram.srec: monitor.ram
	$(TARGET)-objcopy --srec-forceS3 -O srec $< $@

force:

clean:
	$(RM) obj/*.* *.srec *.bin *.bam *.rom *.ram
