#!/bin/bash

ALL="stm32f4 stm32f429disco stm32f469disco stm32f746disco stm32756geval \
     stm32f769disco samg55 sam4s openmv2 rpi3 rpi2"

if [ ! -d ../embedded-runtimes ]; then
    echo "Cannot find embedded-runtimes"
    exit 1
fi

rm -rf ../embedded-runtimes/BSPs

./build-rts.py --bsps-only --output=../embedded-runtimes $ALL
