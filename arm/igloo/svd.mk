SVD2ADA_DIR=$(shell dirname $(shell which svd2ada))

.PHONY: svd

all: svd

svd:
	rm -rf */svd */svdtmp
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/Microsemi/M1AGL.svd -p Interfaces.M1AGL -o m1agl/svdtmp --boolean
	for d in */svdtmp; do \
	  cd $$d; \
	  mkdir ../svd; \
	  mv i-m1agl.ads ../svd; \
	  mv i-m1agl-coretimer.ads ../svd; \
	  mv i-m1agl-coreinterrupt.ads ../svd; \
	  mv i-m1agl-coreuartapb.ads ../svd; \
	  mv a-intnam.ads ../svd; \
	  cd ../..; \
	done
	rm -rf */svdtmp
