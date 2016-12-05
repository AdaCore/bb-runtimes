SECTIONS
{
 .bss :
 {
  . = ALIGN(8);
  __heap_start = .;
  . = . + (DEFINED (__heap_size) ? __heap_size : 0x5000);
  __heap_end = .;
 }
}
