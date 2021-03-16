##############################################################################
#                                                                            #
#                         Copyright (C) 2013, AdaCore                        #
#                                                                            #
#                               Assembly File                                #
#                                                                            #
##############################################################################

        # ABI defined functions
        # Restore the indicated registers, restore the link register,
        # remove the stack frame and return
        .text

        .globl _restgpr_14_x
_restgpr_14_x:
        lwz	%r14, -72(%r11)
        .globl _restgpr_15_x
_restgpr_15_x:
        lwz	%r15, -68(%r11)
        .globl _restgpr_16_x
_restgpr_16_x:
        lwz	%r16, -64(%r11)
        .globl _restgpr_17_x
_restgpr_17_x:
        lwz	%r17, -60(%r11)
        .globl _restgpr_18_x
_restgpr_18_x:
        lwz	%r18, -56(%r11)
        .globl _restgpr_19_x
_restgpr_19_x:
        lwz	%r19, -52(%r11)
        .globl _restgpr_20_x
_restgpr_20_x:
        lwz	%r20, -48(%r11)
        .globl _restgpr_21_x
_restgpr_21_x:
        lwz	%r21, -44(%r11)
        .globl _restgpr_22_x
_restgpr_22_x:
        lwz	%r22, -40(%r11)
        .globl _restgpr_23_x
_restgpr_23_x:
        lwz	%r23, -36(%r11)
        .globl _restgpr_24_x
_restgpr_24_x:
        lwz	%r24, -32(%r11)
        .globl _restgpr_25_x
_restgpr_25_x:
        lwz	%r25, -28(%r11)
        .globl _restgpr_26_x
_restgpr_26_x:
        lwz	%r26, -24(%r11)
        .globl _restgpr_27_x
_restgpr_27_x:
        lwz	%r27, -20(%r11)
        .globl _restgpr_28_x
_restgpr_28_x:
        lwz	%r28, -16(%r11)
        .globl _restgpr_29_x
_restgpr_29_x:
        lwz	%r29, -12(%r11)
        .globl _restgpr_30_x
_restgpr_30_x:
        lwz	%r30, -8(%r11)
        .globl _restgpr_31_x
_restgpr_31_x:
        lwz	%r0, 4(%r11)
	lwz	%r31, -4(%r11)
        mtlr	%r0
	mr      %r1, %r11
        blr
        .size _restgpr_14_x, . - _restgpr_14_x
        .size _restgpr_15_x, . - _restgpr_15_x
        .size _restgpr_16_x, . - _restgpr_16_x
        .size _restgpr_17_x, . - _restgpr_17_x
        .size _restgpr_18_x, . - _restgpr_18_x
        .size _restgpr_19_x, . - _restgpr_19_x
        .size _restgpr_20_x, . - _restgpr_20_x
        .size _restgpr_21_x, . - _restgpr_21_x
        .size _restgpr_22_x, . - _restgpr_22_x
        .size _restgpr_23_x, . - _restgpr_23_x
        .size _restgpr_24_x, . - _restgpr_24_x
        .size _restgpr_25_x, . - _restgpr_25_x
        .size _restgpr_26_x, . - _restgpr_26_x
        .size _restgpr_27_x, . - _restgpr_27_x
        .size _restgpr_28_x, . - _restgpr_28_x
        .size _restgpr_29_x, . - _restgpr_29_x
        .size _restgpr_30_x, . - _restgpr_30_x
        .size _restgpr_31_x, . - _restgpr_31_x

