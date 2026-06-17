#!/bin/sh

mkdir -p generated
./genboot2asm.sh generic_03 generated/boot2__generic_03.S
./genboot2asm.sh generic_qspi generated/boot2__generic_qspi.S
./genboot2asm.sh w25qxx generated/boot2__w25qxx.S
