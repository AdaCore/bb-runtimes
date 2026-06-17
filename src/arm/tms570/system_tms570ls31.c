/*
    Copyright (C) 2017, AdaCore
 */

/** @file system.c
*   @brief System Driver Source File
*   @date 08-Feb-2017
*   @version 04.06.01
*
*   This file contains:
*   - API Functions
*   .
*   which are relevant for the System driver.
*/

/*
* Copyright (C) 2009-2016 Texas Instruments Incorporated - www.ti.com
*
*
*  Redistribution and use in source and binary forms, with or without
*  modification, are permitted provided that the following conditions
*  are met:
*
*    Redistributions of source code must retain the above copyright
*    notice, this list of conditions and the following disclaimer.
*
*    Redistributions in binary form must reproduce the above copyright
*    notice, this list of conditions and the following disclaimer in the
*    documentation and/or other materials provided with the
*    distribution.
*
*    Neither the name of Texas Instruments Incorporated nor the names of
*    its contributors may be used to endorse or promote products derived
*    from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
*  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
*  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
*  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
*  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
*  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*
*/

/* Include Files */

typedef unsigned int uint32;

typedef volatile struct systemBase1
{
    uint32 SYSPC1;                 /* 0x0000 */
    uint32 SYSPC2;                 /* 0x0004 */
    uint32 SYSPC3;                 /* 0x0008 */
    uint32 SYSPC4;                 /* 0x000C */
    uint32 SYSPC5;                 /* 0x0010 */
    uint32 SYSPC6;                 /* 0x0014 */
    uint32 SYSPC7;                 /* 0x0018 */
    uint32 SYSPC8;                 /* 0x001C */
    uint32 SYSPC9;                 /* 0x0020 */
    uint32 SSWPLL1;                /* 0x0024 */
    uint32 SSWPLL2;                /* 0x0028 */
    uint32 SSWPLL3;                /* 0x002C */
    uint32 CSDIS;                  /* 0x0030 */
    uint32 CSDISSET;               /* 0x0034 */
    uint32 CSDISCLR;               /* 0x0038 */
    uint32 CDDIS;                  /* 0x003C */
    uint32 CDDISSET;               /* 0x0040 */
    uint32 CDDISCLR;               /* 0x0044 */
    uint32 GHVSRC;                 /* 0x0048 */
    uint32 VCLKASRC;               /* 0x004C */
    uint32 RCLKSRC;                /* 0x0050 */
    uint32 CSVSTAT;                /* 0x0054 */
    uint32 MSTGCR;                 /* 0x0058 */
    uint32 MINITGCR;               /* 0x005C */
    uint32 MSINENA;                /* 0x0060 */
    uint32 MSTFAIL;                /* 0x0064 */
    uint32 MSTCGSTAT;              /* 0x0068 */
    uint32 MINISTAT;               /* 0x006C */
    uint32 PLLCTL1;                /* 0x0070 */
    uint32 PLLCTL2;                /* 0x0074 */
    uint32 SYSPC10;                /* 0x0078 */
    uint32 DIEIDL;                 /* 0x007C */
    uint32 DIEIDH;                 /* 0x0080 */
    uint32 VRCTL;                  /* 0x0084 */
    uint32 LPOMONCTL;              /* 0x0088 */
    uint32 CLKTEST;                /* 0x008C */
    uint32 DFTCTRLREG1;            /* 0x0090 */
    uint32 DFTCTRLREG2;            /* 0x0094 */
    uint32   rsvd1;                /* 0x0098 */
    uint32   rsvd2;                /* 0x009C */
    uint32 GPREG1;                 /* 0x00A0 */
    uint32 BTRMSEL;                /* 0x00A4 */
    uint32 IMPFASTS;               /* 0x00A8 */
    uint32 IMPFTADD;               /* 0x00AC */
    uint32 SSISR1;                 /* 0x00B0 */
    uint32 SSISR2;                 /* 0x00B4 */
    uint32 SSISR3;                 /* 0x00B8 */
    uint32 SSISR4;                 /* 0x00BC */
    uint32 RAMGCR;                 /* 0x00C0 */
    uint32 BMMCR1;                 /* 0x00C4 */
    uint32 BMMCR2;                 /* 0x00C8 */
    uint32 CPURSTCR;               /* 0x00CC */
    uint32 CLKCNTL;                /* 0x00D0 */
    uint32 ECPCNTL;                /* 0x00D4 */
    uint32 DSPGCR;                 /* 0x00D8 */
    uint32 DEVCR1;                 /* 0x00DC */
    uint32 SYSECR;                 /* 0x00E0 */
    uint32 SYSESR;                 /* 0x00E4 */
    uint32 SYSTASR;                /* 0x00E8 */
    uint32 GBLSTAT;                /* 0x00EC */
    uint32 DEV;                    /* 0x00F0 */
    uint32 SSIVEC;                 /* 0x00F4 */
    uint32 SSIF;                   /* 0x00F8 */
} systemBASE1_t;

#define systemREG1 ((systemBASE1_t *)0xFFFFFF00U)

typedef volatile struct systemBase2
{
    uint32 PLLCTL3;        /* 0x0000 */
    uint32   rsvd1;        /* 0x0004 */
    uint32 STCCLKDIV;      /* 0x0008 */
    uint32   rsvd2[6U];     /* 0x000C */
    uint32 ECPCNTRL0;      /* 0x0024 */
    uint32   rsvd3[5U];     /* 0x0028 */
    uint32 CLK2CNTL;       /* 0x003C */
    uint32 VCLKACON1;      /* 0x0040 */
    uint32  rsvd4[11U];     /* 0x0044 */
    uint32  CLKSLIP;       /* 0x0070 */
    uint32  rsvd5[30U];   	 /* 0x0074 */
    uint32  EFC_CTLEN;     /* 0x00EC */
    uint32  DIEIDL_REG0;   /* 0x00F0 */
    uint32  DIEIDH_REG1;   /* 0x00F4 */
    uint32  DIEIDL_REG2;   /* 0x00F8 */
    uint32  DIEIDH_REG3;   /* 0x00FC */
} systemBASE2_t;

#define systemREG2 ((systemBASE2_t *)0xFFFFE100U)

typedef volatile struct flashWBase
{
    uint32 FRDCNTL;       /* 0x0000 */
    uint32   rsvd1;       /* 0x0004 */
    uint32 FEDACCTRL1;    /* 0x0008 */
    uint32 FEDACCTRL2;    /* 0x000C */
    uint32 FCORERRCNT;    /* 0x0010 */
    uint32 FCORERRADD;    /* 0x0014 */
    uint32 FCORERRPOS;    /* 0x0018 */
    uint32 FEDACSTATUS;   /* 0x001C */
    uint32 FUNCERRADD;    /* 0x0020 */
    uint32 FEDACSDIS;     /* 0x0024 */
    uint32 FPRIMADDTAG;   /* 0x0028 */
    uint32 FREDUADDTAG;   /* 0x002C */
    uint32 FBPROT;        /* 0x0030 */
    uint32 FBSE;          /* 0x0034 */
    uint32 FBBUSY;        /* 0x0038 */
    uint32 FBAC;          /* 0x003C */
    uint32 FBFALLBACK;    /* 0x0040 */
    uint32 FBPRDY;        /* 0x0044 */
    uint32 FPAC1;         /* 0x0048 */
    uint32 FPAC2;         /* 0x004C */
    uint32 FMAC;          /* 0x0050 */
    uint32 FMSTAT;        /* 0x0054 */
    uint32 FEMUDMSW;      /* 0x0058 */
    uint32 FEMUDLSW;      /* 0x005C */
    uint32 FEMUECC;       /* 0x0060 */
    uint32 FLOCK;         /* 0x0064 */
    uint32 FEMUADDR;      /* 0x0068 */
    uint32 FDIAGCTRL;     /* 0x006C */
    uint32 FRAWDATAH;     /* 0x0070 */
    uint32 FRAWDATAL;     /* 0x0074 */
    uint32 FRAWECC;       /* 0x0078 */
    uint32 FPAROVR;       /* 0x007C */
    uint32   rsvd2[16U];  /* 0x009C */
    uint32 FEDACSDIS2;    /* 0x00C0 */
    uint32   rsvd3[15U];  /* 0x00C4 */
    uint32   rsvd4[13U];  /* 0x0100 */
    uint32   rsvd5[85U];  /* 0x0134 */
    uint32 FSMWRENA;      /* 0x0288 */
    uint32   rsvd6[6U];   /* 0x028C */
    uint32 FSMSECTOR;     /* 0x02A4 */
    uint32   rsvd7[4U];   /* 0x02A8 */
    uint32 EEPROMCONFIG;  /* 0x02B8 */
    uint32   rsvd8[19U];  /* 0x02BC */
    uint32 EECTRL1;       /* 0x0308 */
    uint32 EECTRL2;       /* 0x030C */
    uint32 EECORRERRCNT;  /* 0x0310 */
    uint32 EECORRERRADD;  /* 0x0314 */
    uint32 EECORRERRPOS;  /* 0x0318 */
    uint32 EESTATUS;      /* 0x031C */
    uint32 EEUNCERRADD;   /* 0x0320 */
} flashWBASE_t;

#define flashWREG ((flashWBASE_t *)(0xFFF87000U))

typedef volatile struct pcrBase
{
    uint32 PMPROTSET0;    /* 0x0000 */
    uint32 PMPROTSET1;    /* 0x0004 */
    uint32   rsvd1[2U];    /* 0x0008 */
    uint32 PMPROTCLR0;    /* 0x0010 */
    uint32 PMPROTCLR1;    /* 0x0014 */
    uint32   rsvd2[2U];    /* 0x0018 */
    uint32 PPROTSET0;     /* 0x0020 */
    uint32 PPROTSET1;     /* 0x0024 */
    uint32 PPROTSET2;     /* 0x0028 */
    uint32 PPROTSET3;     /* 0x002C */
    uint32   rsvd3[4U];    /* 0x0030 */
    uint32 PPROTCLR0;     /* 0x0040 */
    uint32 PPROTCLR1;     /* 0x0044 */
    uint32 PPROTCLR2;     /* 0x0048 */
    uint32 PPROTCLR3;     /* 0x004C */
    uint32   rsvd4[4U];    /* 0x0050 */
    uint32 PCSPWRDWNSET0; /* 0x0060 */
    uint32 PCSPWRDWNSET1; /* 0x0064 */
    uint32   rsvd5[2U];    /* 0x0068 */
    uint32 PCSPWRDWNCLR0; /* 0x0070 */
    uint32 PCSPWRDWNCLR1; /* 0x0074 */
    uint32   rsvd6[2U];    /* 0x0078 */
    uint32 PSPWRDWNSET0;  /* 0x0080 */
    uint32 PSPWRDWNSET1;  /* 0x0084 */
    uint32 PSPWRDWNSET2;  /* 0x0088 */
    uint32 PSPWRDWNSET3;  /* 0x008C */
    uint32   rsvd7[4U];    /* 0x0090 */
    uint32 PSPWRDWNCLR0;  /* 0x00A0 */
    uint32 PSPWRDWNCLR1;  /* 0x00A4 */
    uint32 PSPWRDWNCLR2;  /* 0x00A8 */
    uint32 PSPWRDWNCLR3;  /* 0x00AC */
} pcrBASE_t;

#define pcrREG ((pcrBASE_t *)0xFFFFE000U)

enum systemClockSource
{
    SYS_OSC       = 0U,  /* oscillator clock Source                */
    SYS_PLL1      = 1U,  /* Pll1 clock Source                      */
    SYS_EXTERNAL1 = 3U,  /* external clock Source                  */
    SYS_LPO_LOW   = 4U,  /* low power oscillator low clock Source  */
    SYS_LPO_HIGH  = 5U,  /* low power oscillator high clock Source */
    SYS_PLL2      = 6U,  /* Pll2 clock Source                      */
    SYS_EXTERNAL2 = 7U,  /* external 2 clock Source                */
    SYS_VCLK      = 9U   /* synchronous VCLK1 clock Source         */
};

enum flashWPowerModes
{
    SYS_SLEEP   = 0U, /* flash bank power mode sleep   */
    SYS_STANDBY = 1U, /* flash bank power mode standby */
    SYS_ACTIVE  = 3U  /* flash bank power mode active  */
};

#define FSM_WR_ENA_HL    (*(volatile uint32 *)0xFFF87288U)
#define EEPROM_CONFIG_HL (*(volatile uint32 *)0xFFF872B8U)

#define LPO_TRIM_VALUE  (((*(volatile uint32 *)0xF00801B4U) & 0xFFFF0000U)>>16U)

void setupPLL(void)
{
    /* Disable PLL1 and PLL2 */
    systemREG1->CSDISSET = 0x00000002U | 0x00000040U;

    /*SAFETYMCUSW 28 D MR:NA <APPROVED> "Hardware status bit read check" */
    while((systemREG1->CSDIS & 0x42U) != 0x42U)
    {
    /* Wait */
    }

    /* Clear Global Status Register */
    systemREG1->GBLSTAT = 0x301U;

    /** - Configure PLL control registers */
    /** @b Initialize @b Pll1: */

    /**   - Setup pll control register 1:
     *    - Setup reset on oscillator slip
     *    - Setup bypass on pll slip
     *    - setup Pll output clock divider to max before Lock
     *    - Setup reset on oscillator fail
     *    - Setup reference clock divider
     *    - Setup Pll multiplier
     */
    systemREG1->PLLCTL1 = (uint32)0x00000000U
                        | (uint32)0x20000000U
                        | (uint32)((uint32)255U << 24U)
                        | (uint32)0x00000000U
                        | (uint32)((uint32)(6U - 1U) << 16U)
                        | (uint32)((uint32)(135U - 1U) << 8U);

    /**   - Setup pll control register 2
    *     - Setup spreading rate
    *     - Setup bandwidth adjustment
    *     - Setup internal Pll output divider
    *     - Setup spreading amount
    */
    systemREG1->PLLCTL2 =  (uint32)((uint32)255U << 22U)
                        |  (uint32)((uint32)7U << 12U)
                        |  (uint32)((uint32)(2U - 1U) << 9U)
                        |  (uint32)61U;

    /** @b Initialize @b Pll2: */

    /**   - Setup pll2 control register :
    *     - setup Pll output clock divider to max before Lock
    *     - Setup reference clock divider
    *     - Setup internal Pll output divider
    *     - Setup Pll multiplier
    */
    systemREG2->PLLCTL3 = (uint32)((uint32)(2U - 1U) << 29U)
                        | (uint32)((uint32)0x0U << 24U)
                        | (uint32)((uint32)(6U - 1U)<< 16U)
                        | (uint32)((uint32)(135U - 1U) << 8U);

    /** - Enable PLL(s) to start up or Lock */
    systemREG1->CSDIS = 0x00000000U
                      | 0x00000000U
                      | 0x00000008U
                      | 0x00000080U
                      | 0x00000000U
                      | 0x00000000U
                      | 0x00000000U;
}

void trimLPO(void)
{
    /** @b Initialize Lpo: */
    /** Load TRIM values from OTP if present else load user defined values */
    /*SAFETYMCUSW 139 S MR:13.7 <APPROVED> "Hardware status bit read check" */
    if(LPO_TRIM_VALUE != 0xFFFFU)
    {

        systemREG1->LPOMONCTL  = (uint32)((uint32)1U << 24U)
                                | LPO_TRIM_VALUE;
    }
    else
    {

        systemREG1->LPOMONCTL   =  (uint32)((uint32)1U << 24U)
                                 | (uint32)((uint32)16U << 8U)
                                 | 16U;
    }
}

void setupFlash(void)
{
    /** - Setup flash read mode, address wait states and data wait states */
    flashWREG->FRDCNTL =  0x00000000U
                       | (uint32)((uint32)3U << 8U)
                       | (uint32)((uint32)1U << 4U)
                       |  1U;

    /** - Setup flash access wait states for bank 7 */
    FSM_WR_ENA_HL    = 0x5U;
    EEPROM_CONFIG_HL = 0x00000002U
                     | (uint32)((uint32)3U << 16U) ;

    /** - Disable write access to flash state machine registers */
    FSM_WR_ENA_HL    = 0xAU;

    /** - Setup flash bank power modes */
    flashWREG->FBFALLBACK = 0x00000000U
                          | (uint32)((uint32)SYS_ACTIVE << 14U) /* BANK 7 */
                          | (uint32)((uint32)SYS_ACTIVE << 2U)  /* BANK 1 */
                          | (uint32)((uint32)SYS_ACTIVE << 0U); /* BANK 0 */
}

void periphInit(void)
{
    /** - Disable Peripherals before peripheral powerup*/
    systemREG1->CLKCNTL &= 0xFFFFFEFFU;

    /** - Release peripherals from reset and enable clocks to all peripherals */
    /** - Power-up all peripherals */
    pcrREG->PSPWRDWNCLR0 = 0xFFFFFFFFU;
    pcrREG->PSPWRDWNCLR1 = 0xFFFFFFFFU;
    pcrREG->PSPWRDWNCLR2 = 0xFFFFFFFFU;
    pcrREG->PSPWRDWNCLR3 = 0xFFFFFFFFU;

    /** - Enable Peripherals */
    /* VCLK = VCLK2 = HCLK / 1 */
    systemREG1->CLKCNTL |= 0x00000100U;
}

void mapClocks(void)
{
    uint32 SYS_CSVSTAT, SYS_CSDIS;

    /** @b Initialize @b Clock @b Tree: */
    /** - Disable / Enable clock domain */
    systemREG1->CDDIS =
      (uint32)((uint32)0U << 4U ) /* AVCLK1 , 1 - OFF, 0 - ON */
      | (uint32)((uint32)0U << 5U ) /* AVCLK2 , 1 - OFF, 0 - ON */
      | (uint32)((uint32)0U << 8U ) /* VCLK3 , 1 - OFF, 0 - ON */
      | (uint32)((uint32)0U << 9U ) /* VCLK4 , 1 - OFF, 0 - ON */
      | (uint32)((uint32)1U << 10U) /* AVCLK3 , 1 - OFF, 0 - ON */
      | (uint32)((uint32)0U << 11U); /* AVCLK4 , 1 - OFF, 0 - ON */


    /* Work Around for Errata SYS#46:
     *
     * Errata Description:
     *            Clock Source Switching Not Qualified with Clock Source Enable And Clock Source Valid
     * Workaround:
     *            Always check the CSDIS register to make sure the clock source is turned on and check
     * the CSVSTAT register to make sure the clock source is valid. Then write to GHVSRC to switch the clock.
     */
    /** - Wait for until clocks are locked */
    SYS_CSVSTAT = systemREG1->CSVSTAT;
    SYS_CSDIS = systemREG1->CSDIS;
    while ((SYS_CSVSTAT & ((SYS_CSDIS ^ 0xFFU) & 0xFFU)) !=
           ((SYS_CSDIS ^ 0xFFU) & 0xFFU))
    {
        SYS_CSVSTAT = systemREG1->CSVSTAT;
        SYS_CSDIS = systemREG1->CSDIS;
    } /* Wait */

    /** - Map device clock domains to desired sources and configure top-level
          dividers */
    /** - All clock domains are working off the default clock sources until
          now */
    /** - The below assignments can be easily modified using the HALCoGen GUI */
    /** - Setup GCLK, HCLK and VCLK clock source for normal operation, power
          down mode and after wakeup */
    systemREG1->GHVSRC = (uint32)((uint32)SYS_OSC << 24U)
                       | (uint32)((uint32)SYS_OSC << 16U)
                       | (uint32)((uint32)SYS_PLL1 << 0U);

    /** - Setup asynchronous peripheral clock sources for AVCLK1 and AVCLK2 */
    systemREG1->VCLKASRC = (uint32)((uint32)SYS_VCLK << 8U)
                         | (uint32)((uint32)SYS_VCLK << 0U);

    /** - Setup RTICLK clock */
    /* RTICLK = VCLK */
    systemREG1->RCLKSRC = (uint32)((uint32)2U << 8U)
                        | (uint32)((uint32)SYS_PLL1 << 0U);

    /** - Setup synchronous peripheral clock dividers for VCLK1, VCLK2, VCLK3 */
    /* CLKCNTL.VCLKR [16-19] = 1: VCLK = HCLK / 2
       CLKCNTL.VCLK2R [24-27] = 1: VCLK2 = HCLK / 2
    */
    systemREG1->CLKCNTL  = (systemREG1->CLKCNTL & 0xF0F0FFFFU)
                         | (uint32)((uint32)1U << 24U)
                         | (uint32)((uint32)1U << 16U);

    systemREG2->CLK2CNTL = (systemREG2->CLK2CNTL & 0xFFFFF0F0U)
                         | (uint32)((uint32)1U << 8U)
                         | (uint32)((uint32)1U << 0U);

    systemREG2->VCLKACON1 =  (uint32)((uint32)(1U - 1U) << 24U)
                           | (uint32)((uint32)0U << 20U)
                           | (uint32)((uint32)SYS_VCLK << 16U)
                           | (uint32)((uint32)(1U - 1U) << 8U)
                           | (uint32)((uint32)0U << 4U)
                           | (uint32)((uint32)SYS_VCLK << 0U);

    /* Now the PLLs are locked and the PLL outputs can be sped up */
    /* The R-divider was programmed to be 0xF. Now this divider is changed to programmed value */
    systemREG1->PLLCTL1 = (systemREG1->PLLCTL1 & 0xE0FFFFFFU)
                        | (uint32)((uint32)(1U - 1U) << 24U);
    /*SAFETYMCUSW 134 S MR:12.2 <APPROVED> "LDRA Tool issue" */
    systemREG2->PLLCTL3 = (systemREG2->PLLCTL3 & 0xE0FFFFFFU)
                        | (uint32)((uint32)(1U - 1U) << 24U);

    /* Enable/Disable Frequency modulation */
    systemREG1->PLLCTL2 |= 0x00000000U;
}

void __gnat_system_init(void)
{
    /* Configure PLL control registers and enable PLLs.
     * The PLL takes (127 + 1024 * NR) oscillator cycles to acquire lock.
     * This initialization sequence performs all the tasks that are not
     * required to be done at full application speed while the PLL locks.
     */
    setupPLL();

    /* Enable clocks to peripherals and release peripheral reset */
    periphInit();

    /* Configure device-level multiplexing and I/O multiplexing */
    // muxInit();

    /** - Set up flash address and data wait states based on the target CPU clock frequency
     * The number of address and data wait states for the target CPU clock frequency are specified
     * in the specific part's datasheet.
     */
    setupFlash();

    /** - Configure the LPO such that HF LPO is as close to 10MHz as possible */
    trimLPO();

    /** - Wait for PLLs to start up and map clock domains to desired clock sources */
    mapClocks();
}
