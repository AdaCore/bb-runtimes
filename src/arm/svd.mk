SVD2ADA_DIR=$(shell dirname $(shell which svd2ada))

all: svd

svd:
	make -C stm32 -f svd.mk
	make -C sam -f svd.mk
	make -C smartfusion2 -f svd.mk
