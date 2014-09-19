#! /bin/sh

# Build
rm -f main.elf; gprbuild --target=arm-eabi --RTS=../../../install

# clean
# gprclean --target=arm-eabi

# Flash
#../src/openocd -f board/atmel_sam4s_xplained_pro.cfg -c "program xx/tetris.elf verify reset"

# Debug
#../src/openocd -f board/atmel_sam4s_xplained_pro.cfg

