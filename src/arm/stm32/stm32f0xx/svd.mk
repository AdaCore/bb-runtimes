SVD2ADA_DIR=$(shell dirname $(shell which svd2ada))

all: svd

svd:
	rm -rf */svd */svdtmp
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/ST/STM32F0x0.svd \
	  -o stm32f0x0/svdtmp -p Interfaces.STM32
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/ST/STM32F0x1.svd \
	  -o stm32f0x1/svdtmp -p Interfaces.STM32
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/ST/STM32F0x2.svd \
	  -o stm32f0x2/svdtmp -p Interfaces.STM32
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/ST/STM32F0x8.svd \
	  -o stm32f0x8/svdtmp -p Interfaces.STM32
	for d in */svdtmp; do \
	   cd $$d; \
	   mkdir ../svd; \
	   mv i-stm32.ads ../svd; \
	   mv i-stm32-flash.ads ../svd; \
	   mv i-stm32-rcc.ads ../svd; \
	   mv a-intnam.ads ../svd; \
	   cd ../..; \
	done
	rm -rf */svdtmp
