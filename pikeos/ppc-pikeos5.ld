/*
This was based on /opt/pikeos-5.0/target/ppc/e500mc-4g/ldscript/app-debug.ld
with the following changes:

1. give exec rights on stack to allow trampolines

    In PikeOS 5, memory zones inherits read/write/exec rights from
    ELF flags of the corresponding section. To allow trampolines,
    we need to allow code to be executed from stack; so add exec right
    to stack section.

*/
PHDRS
{
    text PT_LOAD FLAGS(7);
    rodata PT_LOAD FLAGS(4);
    data PT_LOAD FLAGS(6);
    stack PT_LOAD FLAGS(7);

}
ENTRY(__p4_start)

INPUT(libp4start.a)
SECTIONS
{
    . = 0x08010000 + SIZEOF_HEADERS;
    .text :
    {
        _ftext = . ;
        *(.text)
        *(.text.*)
        *(.gnu.linkonce.t.*)

    } : text
    .eh_frame_hdr :
    {
        *(.eh_frame_hdr)
    } : text
    .eh_frame :
    {
        __EH_FRAME__ = .;
        KEEP(*(.eh_frame))
        LONG(0);
    } : text
    .gcc_except_table :
    {
        *(.gcc_except_table .gcc_except_table.*)
    } : text
    .ctors :
    {
        __CTOR_LIST__ = .;
        KEEP(*(.ctors))
        KEEP(*(SORT_BY_INIT_PRIORITY(.ctors.*)))
        __CTOR_END__ = .;
    } : text
    .dtors :
    {
        __DTOR_LIST__ = .;
        KEEP(*(.dtors))
        KEEP(*(SORT_BY_INIT_PRIORITY(.dtors.*)))
        __DTOR_END__ = .;
    } : text
    .init_array :
    {
        __init_array_start = .;
        KEEP(*(.init_array))
        KEEP(*(SORT_BY_INIT_PRIORITY(.init_array.*)))
        __init_array_end = .;
    } : text
    .fini_array :
    {
        __fini_array_start = .;
        KEEP(*(.fini_array))
        KEEP(*(SORT_BY_INIT_PRIORITY(.fini_array.*)))
        __fini_array_end = .;
    } : text
    _etext = .;
    . = ALIGN(0x1000);
    .rodata :
    {
        __rodata_start = .;
        *(.rodata)
        *(.rodata.*)
        *(.gnu.linkonce.r.*)
        *(.rodata1)
        . = ALIGN(16);
        __scov_desc_start = .;
        KEEP(*(.scov-*))
        __scov_desc_end = .;

        *(.reginfo)
    } : rodata
    .sdata2 :
    {
        _SDA2_BASE_ = . + 0x8000;
        *(.sdata2)
        *(.sdata2.*)
        *(.gnu.linkonce.s2.*)
    } : rodata
    .sbss2 :
    {
        *(.sbss2)
        *(.gnu.linkonce.sb2.*)
    } : rodata
    __rodata_end = .;
    . = ALIGN(0x1000);
    .data :
    {
        _fdata = . ;
        *(.data)
        *(.data.*)
        *(.gnu.linkonce.d.*)
        *(.data1)

    } : data
    .sdata :
    {
        _SDA_BASE_ = . + 0x8000;
        *(.sdata)
        *(.sdata.*)
        *(.gnu.linkonce.s.*)
    } : data
    . = ALIGN(64);
    _edata = .;
    .sbss :
    {
        __bss_start = .;
        *(.sbss)
        *(.sbss.*)
        *(.gnu.linkonce.sb.*)
        *(.scommon)
    } : data
    .bss :
    {
        *(.bss)
        *(.bss.*)
        *(.gnu.linkonce.b.*)

        *(COMMON)
        __bss_end = .;
    } : data
    . = ALIGN(0x1000) + 0x1000;
    __p4_stack_start = .;
    .stack (NOLOAD) : {
        *(.p4stack)
        . = ALIGN(0x1000);
    } : stack
    __p4_stack_end = .;
    . = . + 0x1000;

    _end = . ;
}
