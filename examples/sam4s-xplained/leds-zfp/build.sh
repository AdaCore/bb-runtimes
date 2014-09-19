gprbuild --target=arm-eabi --RTS=../../../install leds.gpr

echo "To program (on windows):"
echo "atprogram -t edbg -i swd -d atsam4sd32c program --format elf -f leds"

