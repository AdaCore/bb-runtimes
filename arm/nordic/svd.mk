SVD2ADA_DIR=$(shell dirname $(shell which svd2ada))

.PHONY: svd

all: svd

svd: svd-nrf51 svd-nrf52

svd-nrf51:
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

svd-nrf52:
	rm -rf nrf52/nrf52832/svd nrf52/nrf52832/svdtmp
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/Nordic/nrf52.svd -p Interfaces.NRF52 -o nrf52/nrf52832/svdtmp
	cd nrf52/nrf52832/svdtmp && \
	echo $(CURDIR) && \
	mkdir ../svd && \
	mv i-nrf52.ads ../svd && \
	mv i-nrf52-clock.ads ../svd && \
	mv i-nrf52-ficr.ads ../svd && \
	mv i-nrf52-gpio.ads ../svd && \
	mv i-nrf52-nvmc.ads ../svd && \
	mv i-nrf52-rtc.ads ../svd && \
	mv i-nrf52-temp.ads ../svd && \
	mv i-nrf52-uart.ads ../svd && \
	mv i-nrf52-uicr.ads ../svd && \
	mv a-intnam.ads ../svd && \
	mv handler.S ../svd && \
	cd ../../..
	rm -rf nrf52/nrf52832/svdtmp

svd-nrf52840:
	rm -rf nrf52/nrf52840/svd nrf52/nrf52840/svdtmp
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/Nordic/nrf52840.svd -p Interfaces.NRF52 -o nrf52/nrf52840/svdtmp
	cd nrf52/nrf52840/svdtmp && \
	echo $(CURDIR) && \
	mkdir ../svd && \
	mv i-nrf52.ads ../svd && \
	mv i-nrf52-ccm.ads ../svd && \
	mv i-nrf52-clock.ads ../svd && \
	mv i-nrf52-ficr.ads ../svd && \
	mv i-nrf52-gpio.ads ../svd && \
	mv i-nrf52-nvmc.ads ../svd && \
	mv i-nrf52-rtc.ads ../svd && \
	mv i-nrf52-temp.ads ../svd && \
	mv i-nrf52-uart.ads ../svd && \
	mv i-nrf52-uicr.ads ../svd && \
	mv a-intnam.ads ../svd && \
	mv handler.S ../svd && \
	cd ../../..
	rm -rf nrf52/nrf52840/svdtmp
