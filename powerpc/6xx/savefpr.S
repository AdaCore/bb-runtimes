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

        .globl _savefpr_14
_savefpr_14:
        stfd	%r14, -144(%r11)
        .globl _savefpr_15
_savefpr_15:
        stfd	%r15, -136(%r11)
        .globl _savefpr_16
_savefpr_16:
        stfd	%r16, -128(%r11)
        .globl _savefpr_17
_savefpr_17:
        stfd	%r17, -120(%r11)
        .globl _savefpr_18
_savefpr_18:
        stfd	%r18, -112(%r11)
        .globl _savefpr_19
_savefpr_19:
        stfd	%r19, -104(%r11)
        .globl _savefpr_20
_savefpr_20:
        stfd	%r20, -96(%r11)
        .globl _savefpr_21
_savefpr_21:
        stfd	%r21, -88(%r11)
        .globl _savefpr_22
_savefpr_22:
        stfd	%r22, -80(%r11)
        .globl _savefpr_23
_savefpr_23:
        stfd	%r23, -72(%r11)
        .globl _savefpr_24
_savefpr_24:
        stfd	%r24, -64(%r11)
        .globl _savefpr_25
_savefpr_25:
        stfd	%r25, -56(%r11)
        .globl _savefpr_26
_savefpr_26:
        stfd	%r26, -48(%r11)
        .globl _savefpr_27
_savefpr_27:
        stfd	%r27, -40(%r11)
        .globl _savefpr_28
_savefpr_28:
        stfd	%r28, -32(%r11)
        .globl _savefpr_29
_savefpr_29:
        stfd	%r29, -24(%r11)
        .globl _savefpr_30
_savefpr_30:
        stfd	%r30, -16(%r11)
        .globl _savefpr_31
_savefpr_31:
	stfd	%r31, -8(%r11)
        blr
        .size _savefpr_14, . - _savefpr_14
        .size _savefpr_15, . - _savefpr_15
        .size _savefpr_16, . - _savefpr_16
        .size _savefpr_17, . - _savefpr_17
        .size _savefpr_18, . - _savefpr_18
        .size _savefpr_19, . - _savefpr_19
        .size _savefpr_20, . - _savefpr_20
        .size _savefpr_21, . - _savefpr_21
        .size _savefpr_22, . - _savefpr_22
        .size _savefpr_23, . - _savefpr_23
        .size _savefpr_24, . - _savefpr_24
        .size _savefpr_25, . - _savefpr_25
        .size _savefpr_26, . - _savefpr_26
        .size _savefpr_27, . - _savefpr_27
        .size _savefpr_28, . - _savefpr_28
        .size _savefpr_29, . - _savefpr_29
        .size _savefpr_30, . - _savefpr_30
        .size _savefpr_31, . - _savefpr_31

