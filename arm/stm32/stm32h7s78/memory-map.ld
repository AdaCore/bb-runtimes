/* memory-map_stm32h7s78.ld
 *
 * STM32H7S7L8 (H7R/S family) — linker script skeleton for bb-runtimes.
 * Board: STM32H7S78-DK
 *
 * Key facts (adjust only if your part differs):
 *  - User Flash: 64 KiB @ 0x0800_0000 (8×8 KiB sectors)
 *  - SRAM total ≈ 620 KiB across: ITCM 64K, DTCM 64K, AXI ≈456K, AHB 32K, BKPSRAM 4K
 *  - External XiP window (XSPI): 0x9000_0000 (not used here)
 *
 * This script:
 *  - Places vectors/.text/.rodata in FLASH
 *  - Places .data/.bss in AXI SRAM by default (fast, large)
 *  - Provides optional ITCM section (.itcm_text) for time‑critical routines
 *  - Uses DTCM for main stack by default (deterministic access)
 *
 * You can redirect sections by changing the SECTIONS map below.
 */

ENTRY(Reset_Handler)

/* ---------- Memory Regions ---------- */
MEMORY
{
  FLASH     (rx)  : ORIGIN = 0x08000000, LENGTH = 64K

  /* TCMs (deterministic, zero‑wait): */
  ITCM      (rx)  : ORIGIN = 0x00000000, LENGTH = 64K
  DTCM      (rwx) : ORIGIN = 0x20000000, LENGTH = 64K

  /* AXI SRAM (largest, high bandwidth) */
  RAM       (rwx) : ORIGIN = 0x24000000, LENGTH = 456K

  /* Small AHB SRAM banks (optional use) */
  AHB1      (rwx) : ORIGIN = 0x30000000, LENGTH = 16K
  AHB2      (rwx) : ORIGIN = 0x30004000, LENGTH = 16K

  /* VBAT‑retained backup SRAM */
  BKPSRAM   (rwx) : ORIGIN = 0x38800000, LENGTH = 4K
}

/* Symbols exported for runtime/system code */
__flash_start__   = ORIGIN(FLASH);
__flash_end__     = ORIGIN(FLASH) + LENGTH(FLASH);

__itcm_start__    = ORIGIN(ITCM);
__itcm_end__      = ORIGIN(ITCM) + LENGTH(ITCM);

__dtcm_start__    = ORIGIN(DTCM);
__dtcm_end__      = ORIGIN(DTCM) + LENGTH(DTCM);

__ram_start__     = ORIGIN(RAM);
__ram_end__       = ORIGIN(RAM) + LENGTH(RAM);

/* Default stack in DTCM for determinism; change to RAM if preferred */
__stack_top__     = __dtcm_end__;
__stack_limit__   = __dtcm_start__ + 4K; /* adjust as needed; keep some guard */

/* Data init copy ranges */
__sidata = LOADADDR(.data);  /* flash source for .data init */
__sdata  = ADDR(.data);
__edata  = ADDR(.data) + SIZEOF(.data);

__sbss   = ADDR(.bss);
__ebss   = ADDR(.bss) + SIZEOF(.bss);

/* Optional: ITCM text range for copy from flash if you want runtime copy */
__sitcm  = LOADADDR(.itcm_text);
__eitcm  = ADDR(.itcm_text) + SIZEOF(.itcm_text);

/* ---------- Sections ---------- */
SECTIONS
{
  /* Vector table (KEEP to avoid GC) */
  .isr_vector :
  {
    __isr_vector_start__ = .;
    KEEP(*(.isr_vector))         /* your startup provides this symbol/section */
    KEEP(*(.vectors))            /* or alternate name */
    __isr_vector_end__ = .;
  } > FLASH

  /* Optional: time‑critical code linked to ITCM */
  .itcm_text :
  {
    __itcm_text_start__ = .;
    *(.itcm_text .itcm_text.*)
    __itcm_text_end__ = .;
  } > ITCM AT > FLASH
  __itcm_load_addr__ = LOADADDR(.itcm_text);

  /* Code & read‑only */
  .text :
  {
    __text_start__ = .;
    *(.text .text.*)
    *(.gnu.linkonce.t.*)
    *(.glue_7t) *(.glue_7)
    __text_end__ = .;
  } > FLASH

  .rodata :
  {
    __rodata_start__ = .;
    *(.rodata .rodata.*)
    *(.gnu.linkonce.r.*)
    __rodata_end__ = .;
  } > FLASH

  /* Initialized data → AXI RAM, load image in FLASH */
  .data : AT (LOADADDR(.rodata) + SIZEOF(.rodata))
  {
    __data_start__ = .;
    *(.data .data.*)
    *(.gnu.linkonce.d.*)
    __data_end__ = .;
  } > RAM

  /* Zero‑init */
  .bss (NOLOAD) :
  {
    __bss_start__ = .;
    *(.bss .bss.*)
    *(COMMON)
    __bss_end__ = .;
  } > RAM

  /* Heap/stack carve‑out in RAM if you put stack in RAM */
  /* ._user_heap_stack (NOLOAD) : { . = . + 0; } > RAM */

  /* Discard build artifacts */
  /DISCARD/ :
  {
    *(.comment .note .note.*)
    *(.eh_frame .eh_frame.*)
  }
}
