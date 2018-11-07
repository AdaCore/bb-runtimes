SVD2ADA_DIR=$(shell dirname $(shell which svd2ada))

.PHONY: svd

all: svd

svd:
	rm -rf */svd */svdtmp
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/SiFive/FE310.svd -p Interfaces.FE310 -o fe310/svdtmp --boolean
	for d in */svdtmp; do \
	  cd $$d; \
	  mkdir ../svd; \
	  mv i-fe310.ads ../svd; \
	  mv i-fe310-uart.ads ../svd; \
	  mv i-fe310-gpio.ads ../svd; \
	  mv i-fe310-plic.ads ../svd; \
	  mv a-intnam.ads     ../svd; \
	  cd ../..; \
	done
	rm -rf */svdtmp
