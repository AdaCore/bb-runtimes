##############################################################################
#                                                                            #
#                      Copyright (C) 2013-2020, AdaCore                      #
#                                                                            #
#                               Assembly File                                #
#                                                                            #
##############################################################################

        # ABI defined functions
        # Restore the indicated registers, restore the link register,
        # remove the stack frame and return
        .text

        .globl _restfpr_14_x
_restfpr_14_x:
        lfd     %f14, -144(%r11)
        .globl _restfpr_15_x
_restfpr_15_x:
        lfd     %f15, -136(%r11)
        .globl _restfpr_16_x
_restfpr_16_x:
        lfd     %f16, -128(%r11)
        .globl _restfpr_17_x
_restfpr_17_x:
        lfd     %f17, -120(%r11)
        .globl _restfpr_18_x
_restfpr_18_x:
        lfd     %f18, -112(%r11)
        .globl _restfpr_19_x
_restfpr_19_x:
        lfd     %f19, -104(%r11)
        .globl _restfpr_20_x
_restfpr_20_x:
        lfd     %f20, -96(%r11)
        .globl _restfpr_21_x
_restfpr_21_x:
        lfd     %f21, -88(%r11)
        .globl _restfpr_22_x
_restfpr_22_x:
        lfd     %f22, -80(%r11)
        .globl _restfpr_23_x
_restfpr_23_x:
        lfd     %f23, -72(%r11)
        .globl _restfpr_24_x
_restfpr_24_x:
        lfd     %f24, -64(%r11)
        .globl _restfpr_25_x
_restfpr_25_x:
        lfd     %f25, -56(%r11)
        .globl _restfpr_26_x
_restfpr_26_x:
        lfd     %f26, -48(%r11)
        .globl _restfpr_27_x
_restfpr_27_x:
        lfd     %f27, -40(%r11)
        .globl _restfpr_28_x
_restfpr_28_x:
        lfd     %f28, -32(%r11)
        .globl _restfpr_29_x
_restfpr_29_x:
        lfd     %f29, -24(%r11)
        .globl _restfpr_30_x
_restfpr_30_x:
        lfd     %f30, -16(%r11)
        .globl _restfpr_31_x
_restfpr_31_x:
        lwz     %r0, 4(%r11)
        lfd     %f31, -8(%r11)
        mtlr    %r0
        mr      %r1, %r11
        blr
        .size _restfpr_14_x, . - _restfpr_14_x
        .size _restfpr_15_x, . - _restfpr_15_x
        .size _restfpr_16_x, . - _restfpr_16_x
        .size _restfpr_17_x, . - _restfpr_17_x
        .size _restfpr_18_x, . - _restfpr_18_x
        .size _restfpr_19_x, . - _restfpr_19_x
        .size _restfpr_20_x, . - _restfpr_20_x
        .size _restfpr_21_x, . - _restfpr_21_x
        .size _restfpr_22_x, . - _restfpr_22_x
        .size _restfpr_23_x, . - _restfpr_23_x
        .size _restfpr_24_x, . - _restfpr_24_x
        .size _restfpr_25_x, . - _restfpr_25_x
        .size _restfpr_26_x, . - _restfpr_26_x
        .size _restfpr_27_x, . - _restfpr_27_x
        .size _restfpr_28_x, . - _restfpr_28_x
        .size _restfpr_29_x, . - _restfpr_29_x
        .size _restfpr_30_x, . - _restfpr_30_x
        .size _restfpr_31_x, . - _restfpr_31_x
