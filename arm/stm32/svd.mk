SVD2ADA_DIR=$(shell dirname $(shell which svd2ada))

all: svd

svd:
	rm -rf */svd */svdtmp
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/ST/STM32F40x.svd \
	  -o stm32f40x/svdtmp -p Interfaces.STM32
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/ST/STM32F429x.svd \
	  -o stm32f429x/svdtmp -p Interfaces.STM32
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/ST/STM32F46_79x.svd \
	  -o stm32f469x/svdtmp -p Interfaces.STM32
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/ST/STM32F7x.svd \
	  -o stm32f7x/svdtmp -p Interfaces.STM32
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/ST/STM32F7x9.svd \
	  -o stm32f7x9/svdtmp -p Interfaces.STM32
	for d in */svdtmp; do \
	   cd $$d; \
	   mkdir ../svd; \
	   mv i-stm32.ads ../svd; \
	   mv i-stm32-flash.ads ../svd; \
	   mv i-stm32-gpio.ads ../svd; \
	   mv i-stm32-pwr.ads ../svd; \
	   mv i-stm32-rcc.ads ../svd; \
	   mv i-stm32-syscfg.ads ../svd; \
	   mv i-stm32-usart.ads ../svd; \
	   mv handler.S ../svd; \
	   mv a-intnam.ads ../svd; \
	   cd ../..; \
	done
	rm -rf */svdtmp
