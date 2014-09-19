##############################################################################
#                                                                            #
#                         Copyright (C) 2013, AdaCore                        #
#                                                                            #
#                               Assembly File                                #
#                                                                            #
##############################################################################

        # ABI defined functions
        # Restore the indicated registers
        .text

        .globl _restfpr_14
_restfpr_14:
        lfd	%r14, -144(%r11)
        .globl _restfpr_15
_restfpr_15:
        lfd	%r15, -136(%r11)
        .globl _restfpr_16
_restfpr_16:
        lfd	%r16, -128(%r11)
        .globl _restfpr_17
_restfpr_17:
        lfd	%r17, -120(%r11)
        .globl _restfpr_18
_restfpr_18:
        lfd	%r18, -112(%r11)
        .globl _restfpr_19
_restfpr_19:
        lfd	%r19, -104(%r11)
        .globl _restfpr_20
_restfpr_20:
        lfd	%r20, -96(%r11)
        .globl _restfpr_21
_restfpr_21:
        lfd	%r21, -88(%r11)
        .globl _restfpr_22
_restfpr_22:
        lfd	%r22, -80(%r11)
        .globl _restfpr_23
_restfpr_23:
        lfd	%r23, -72(%r11)
        .globl _restfpr_24
_restfpr_24:
        lfd	%r24, -64(%r11)
        .globl _restfpr_25
_restfpr_25:
        lfd	%r25, -56(%r11)
        .globl _restfpr_26
_restfpr_26:
        lfd	%r26, -48(%r11)
        .globl _restfpr_27
_restfpr_27:
        lfd	%r27, -40(%r11)
        .globl _restfpr_28
_restfpr_28:
        lfd	%r28, -32(%r11)
        .globl _restfpr_29
_restfpr_29:
        lfd	%r29, -24(%r11)
        .globl _restfpr_30
_restfpr_30:
        lfd	%r30, -16(%r11)
        .globl _restfpr_31
_restfpr_31:
	lfd	%r31, -8(%r11)
        blr
        .size _restfpr_14, . - _restfpr_14
        .size _restfpr_15, . - _restfpr_15
        .size _restfpr_16, . - _restfpr_16
        .size _restfpr_17, . - _restfpr_17
        .size _restfpr_18, . - _restfpr_18
        .size _restfpr_19, . - _restfpr_19
        .size _restfpr_20, . - _restfpr_20
        .size _restfpr_21, . - _restfpr_21
        .size _restfpr_22, . - _restfpr_22
        .size _restfpr_23, . - _restfpr_23
        .size _restfpr_24, . - _restfpr_24
        .size _restfpr_25, . - _restfpr_25
        .size _restfpr_26, . - _restfpr_26
        .size _restfpr_27, . - _restfpr_27
        .size _restfpr_28, . - _restfpr_28
        .size _restfpr_29, . - _restfpr_29
        .size _restfpr_30, . - _restfpr_30
        .size _restfpr_31, . - _restfpr_31

