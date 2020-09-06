SVD2ADA_DIR=$(shell dirname $(shell which svd2ada))

.PHONY: svd

all: svd

svd:
	rm -rf */svd */svdtmp
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/Microsemi/PolarFire.svd -p Interfaces.Microsemi -o microsemi/svdtmp --boolean
	for d in */svdtmp; do \
	  cd $$d; \
	  mkdir ../svd; \
	  mv i-microsemi.ads ../svd; \
	  mv i-microsemi-coreuartapb.ads ../svd; \
	  cd ../..; \
	done
	rm -rf */svdtmp
