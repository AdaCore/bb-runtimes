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

        .globl _restgpr_14
_restgpr_14:
        lwz	%r14, -72(%r11)
        .globl _restgpr_15
_restgpr_15:
        lwz	%r15, -68(%r11)
        .globl _restgpr_16
_restgpr_16:
        lwz	%r16, -64(%r11)
        .globl _restgpr_17
_restgpr_17:
        lwz	%r17, -60(%r11)
        .globl _restgpr_18
_restgpr_18:
        lwz	%r18, -56(%r11)
        .globl _restgpr_19
_restgpr_19:
        lwz	%r19, -52(%r11)
        .globl _restgpr_20
_restgpr_20:
        lwz	%r20, -48(%r11)
        .globl _restgpr_21
_restgpr_21:
        lwz	%r21, -44(%r11)
        .globl _restgpr_22
_restgpr_22:
        lwz	%r22, -40(%r11)
        .globl _restgpr_23
_restgpr_23:
        lwz	%r23, -36(%r11)
        .globl _restgpr_24
_restgpr_24:
        lwz	%r24, -32(%r11)
        .globl _restgpr_25
_restgpr_25:
        lwz	%r25, -28(%r11)
        .globl _restgpr_26
_restgpr_26:
        lwz	%r26, -24(%r11)
        .globl _restgpr_27
_restgpr_27:
        lwz	%r27, -20(%r11)
        .globl _restgpr_28
_restgpr_28:
        lwz	%r28, -16(%r11)
        .globl _restgpr_29
_restgpr_29:
        lwz	%r29, -12(%r11)
        .globl _restgpr_30
_restgpr_30:
        lwz	%r30, -8(%r11)
        .globl _restgpr_31
_restgpr_31:
	lwz	%r31, -4(%r11)
        blr
        .size _restgpr_14, . - _restgpr_14
        .size _restgpr_15, . - _restgpr_15
        .size _restgpr_16, . - _restgpr_16
        .size _restgpr_17, . - _restgpr_17
        .size _restgpr_18, . - _restgpr_18
        .size _restgpr_19, . - _restgpr_19
        .size _restgpr_20, . - _restgpr_20
        .size _restgpr_21, . - _restgpr_21
        .size _restgpr_22, . - _restgpr_22
        .size _restgpr_23, . - _restgpr_23
        .size _restgpr_24, . - _restgpr_24
        .size _restgpr_25, . - _restgpr_25
        .size _restgpr_26, . - _restgpr_26
        .size _restgpr_27, . - _restgpr_27
        .size _restgpr_28, . - _restgpr_28
        .size _restgpr_29, . - _restgpr_29
        .size _restgpr_30, . - _restgpr_30
        .size _restgpr_31, . - _restgpr_31

