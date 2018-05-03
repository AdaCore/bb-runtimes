PHDRS
{
    text PT_LOAD FLAGS(5);
    data PT_LOAD FLAGS(6);
    stack PT_LOAD FLAGS(6);

}
ENTRY(__p4_start)

INPUT(libp4start.a)
SECTIONS
{
    . = 0x08010000 + SIZEOF_HEADERS;
    _ftext = . ;
    .text :
    {
        *(.text)
        *(.text.*)
        *(.gnu.linkonce.t.*)

    } : text
    .rodata ALIGN(64) :
    {
        *(.rodata)
        *(.rodata.*)
        *(.gnu.linkonce.r.*)
        *(.rodata1)
        *(.sdata2)
        *(.sbss2)

        *(.reginfo)
    } : text
    .ARM.extab :
    {
            *(.ARM.extab* .gnu.linkonce.armextab.*)
    }
    .ARM.exidx :
    {
            __exidx_start = .;
            *(.ARM.exidx* .gnu.linkonce.armexidx.*)
            __exidx_end = .;
    }
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
        __CTOR_END__ = .;
    } : text
    .dtors :
    {
        __DTOR_LIST__ = .;
        KEEP(*(.dtors))
        __DTOR_END__ = .;
    } : text
    .init_array :
    {
        __init_array_start = .;
        KEEP(*(.init_array))
        __init_array_end = .;
    } : text
    .fini_array :
    {
        __fini_array_start = .;
        KEEP(*(.fini_array))
        __fini_array_end = .;
    } : text
    _etext = .;
    . = ALIGN(0x1000);
    _fdata = . ;
    ._vm_stack :
    {
        LONG(DEFINED(_vm_stack_size) ? ABSOLUTE(_vm_stack_size) : 0);
        LONG(DEFINED(_vm_stack) ? ABSOLUTE(_vm_stack) : 0);
    } : data
    . = ALIGN(64);
    .data :
    {
        *(.data)
        *(.data.*)
        *(.gnu.linkonce.d.*)
        *(.data1)
        *(.sdata)

    } : data
    _edata = .;
    . = ALIGN(64);
    __bss_start = .;
    .bss :
    {
        *(.sbss)
        *(.scommon)
        *(.bss)
        *(.bss.*)
        *(.gnu.linkonce.b.*)

        *(COMMON)
    } : data
    . = ALIGN(0x1000) + 0x1000;
    __p4_stack_start = .;
    .stack : {
        *(.p4stack)
        . = ALIGN(0x1000);
    } : stack
    __p4_stack_end = .;
    . = . + 0x1000;

    _end = . ;
}
