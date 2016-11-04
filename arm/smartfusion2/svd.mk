SVD2ADA_DIR=$(shell dirname $(shell which svd2ada))

all: svd

.PHONY: svd

svd:
	rm -rf svd svdtmp
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/Microsemi/M2Sxxx.svd \
	  -o svdtmp -p Interfaces.SF2 --boolean
	mkdir svd
	mv svdtmp/i-sf2.ads svd
	mv svdtmp/i-sf2-system_registers.ads svd
	mv svdtmp/i-sf2-gpio.ads svd
	mv svdtmp/i-sf2-mmuart.ads svd
	mv svdtmp/handler.S svd
	mv svdtmp/a-intnam.ads svd
	rm -rf svdtmp
