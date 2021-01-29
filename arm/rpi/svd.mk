SVD2ADA_DIR=$(shell dirname $(shell which svd2ada))

.PHONY: svd

all: svd

svd: svd-rp2040

svd-rp2040:
	rm -rf */svd */svdtmp
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/RaspberryPi/rp2040.svd -p Interfaces.RP2040 -o rp2040/svdtmp
	for d in */svdtmp; do \
	  cd $$d; \
	  mkdir ../svd; \
	  mv i-rp2040.ads ../svd; \
	  mv i-rp2040-clocks.ads ../svd; \
	  mv i-rp2040-pll_sys.ads ../svd; \
	  mv i-rp2040-psm.ads ../svd; \
	  mv i-rp2040-resets.ads ../svd; \
	  mv i-rp2040-rosc.ads ../svd; \
	  mv i-rp2040-sio.ads ../svd; \
	  mv i-rp2040-timer.ads ../svd; \
	  mv i-rp2040-watchdog.ads ../svd; \
	  mv i-rp2040-xosc.ads ../svd; \
	  cd ../..; \
	done
	rm -rf */svdtmp
