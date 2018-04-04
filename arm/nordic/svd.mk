SVD2ADA_DIR=$(shell dirname $(shell which svd2ada))

.PHONY: svd

all: svd

svd:
	rm -rf */svd */svdtmp
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/Nordic/nrf51.svd -p Interfaces.NRF51 -o nrf51/svdtmp --boolean
	for d in */svdtmp; do \
	  cd $$d; \
	  mkdir ../svd; \
	  mv i-nrf51.ads ../svd; \
	  mv i-nrf51-clock.ads ../svd; \
	  mv i-nrf51-rtc.ads ../svd; \
	  mv i-nrf51-uart.ads ../svd; \
	  mv a-intnam.ads ../svd; \
	  mv handler.S ../svd; \
	  cd ../..; \
	done
	rm -rf */svdtmp
