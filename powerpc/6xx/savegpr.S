##############################################################################
#                                                                            #
#                         Copyright (C) 2013, AdaCore                        #
#                                                                            #
#                               Assembly File                                #
#                                                                            #
##############################################################################

        # ABI defined functions
        # Save the indicated registers
        .text

        .globl _savegpr_14
_savegpr_14:
        stw	%r14, -72(%r11)
        .globl _savegpr_15
_savegpr_15:
        stw	%r15, -68(%r11)
        .globl _savegpr_16
_savegpr_16:
        stw	%r16, -64(%r11)
        .globl _savegpr_17
_savegpr_17:
        stw	%r17, -60(%r11)
        .globl _savegpr_18
_savegpr_18:
        stw	%r18, -56(%r11)
        .globl _savegpr_19
_savegpr_19:
        stw	%r19, -52(%r11)
        .globl _savegpr_20
_savegpr_20:
        stw	%r20, -48(%r11)
        .globl _savegpr_21
_savegpr_21:
        stw	%r21, -44(%r11)
        .globl _savegpr_22
_savegpr_22:
        stw	%r22, -40(%r11)
        .globl _savegpr_23
_savegpr_23:
        stw	%r23, -36(%r11)
        .globl _savegpr_24
_savegpr_24:
        stw	%r24, -32(%r11)
        .globl _savegpr_25
_savegpr_25:
        stw	%r25, -28(%r11)
        .globl _savegpr_26
_savegpr_26:
        stw	%r26, -24(%r11)
        .globl _savegpr_27
_savegpr_27:
        stw	%r27, -20(%r11)
        .globl _savegpr_28
_savegpr_28:
        stw	%r28, -16(%r11)
        .globl _savegpr_29
_savegpr_29:
        stw	%r29, -12(%r11)
        .globl _savegpr_30
_savegpr_30:
        stw	%r30, -8(%r11)
        .globl _savegpr_31
_savegpr_31:
	stw	%r31, -4(%r11)
        blr
        .size _savegpr_14, . - _savegpr_14
        .size _savegpr_15, . - _savegpr_15
        .size _savegpr_16, . - _savegpr_16
        .size _savegpr_17, . - _savegpr_17
        .size _savegpr_18, . - _savegpr_18
        .size _savegpr_19, . - _savegpr_19
        .size _savegpr_20, . - _savegpr_20
        .size _savegpr_21, . - _savegpr_21
        .size _savegpr_22, . - _savegpr_22
        .size _savegpr_23, . - _savegpr_23
        .size _savegpr_24, . - _savegpr_24
        .size _savegpr_25, . - _savegpr_25
        .size _savegpr_26, . - _savegpr_26
        .size _savegpr_27, . - _savegpr_27
        .size _savegpr_28, . - _savegpr_28
        .size _savegpr_29, . - _savegpr_29
        .size _savegpr_30, . - _savegpr_30
        .size _savegpr_31, . - _savegpr_31

