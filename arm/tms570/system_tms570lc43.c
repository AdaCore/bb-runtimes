/*
    Copyright (C) 2017, AdaCore
 */

/** @file HL_system.c
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
    uint32 rsvd1;                  /* 0x0024 */
    uint32 rsvd2;                  /* 0x0028 */
    uint32 rsvd3;                  /* 0x002C */
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
    uint32 rsvd4;                  /* 0x0084 */
    uint32 LPOMONCTL;              /* 0x0088 */
    uint32 CLKTEST;                /* 0x008C */
    uint32 DFTCTRLREG1;            /* 0x0090 */
    uint32 DFTCTRLREG2;            /* 0x0094 */
    uint32 rsvd5;                  /* 0x0098 */
    uint32 rsvd6;                  /* 0x009C */
    uint32 GPREG1;                 /* 0x00A0 */
    uint32 rsvd7;                  /* 0x00A4 */
    uint32 rsvd8;                  /* 0x00A8 */
    uint32 rsvd9;                  /* 0x00AC */
    uint32 SSIR1;                  /* 0x00B0 */
    uint32 SSIR2;                  /* 0x00B4 */
    uint32 SSIR3;                  /* 0x00B8 */
    uint32 SSIR4;                  /* 0x00BC */
    uint32 RAMGCR;                 /* 0x00C0 */
    uint32 BMMCR1;                 /* 0x00C4 */
    uint32 rsvd10;                 /* 0x00C8 */
    uint32 CPURSTCR;               /* 0x00CC */
    uint32 CLKCNTL;                /* 0x00D0 */
    uint32 ECPCNTL;                /* 0x00D4 */
    uint32 rsvd11;                 /* 0x00D8 */
    uint32 DEVCR1;                 /* 0x00DC */
    uint32 SYSECR;                 /* 0x00E0 */
    uint32 SYSESR;                 /* 0x00E4 */
    uint32 SYSTASR;                /* 0x00E8 */
    uint32 GBLSTAT;                /* 0x00EC */
    uint32 DEVID;                  /* 0x00F0 */
    uint32 SSIVEC;                 /* 0x00F4 */
    uint32 SSIF;                   /* 0x00F8 */
} systemBASE1_t;

#define systemREG1 ((systemBASE1_t *)0xFFFFFF00U)

typedef volatile struct systemBase2
{
    uint32 PLLCTL3;        /* 0x0000 */
    uint32 rsvd1;          /* 0x0004 */
    uint32 STCCLKDIV;      /* 0x0008 */
    uint32 rsvd2[6U];      /* 0x000C */
    uint32 ECPCNTL;        /* 0x0024 */
    uint32 ECPCNTL1;       /* 0x0028 */
    uint32 rsvd3[4U];      /* 0x002C */
    uint32 CLK2CNTRL;      /* 0x003C */
    uint32 VCLKACON1;      /* 0x0040 */
    uint32 rsvd4[4U];      /* 0x0044 */
    uint32 HCLKCNTL;       /* 0x0054 */
    uint32 rsvd5[6U];      /* 0x0058 */
    uint32 CLKSLIP;        /* 0x0070 */
    uint32 rsvd6;          /* 0x0074 */
    uint32 IP1ECCERREN;	   /* 0x0078 */
    uint32 rsvd7[28U];     /* 0x007C */
    uint32 EFC_CTLEN;      /* 0x00EC */
    uint32 DIEIDL_REG0;    /* 0x00F0 */
    uint32 DIEIDH_REG1;    /* 0x00F4 */
    uint32 DIEIDL_REG2;    /* 0x00F8 */
    uint32 DIEIDH_REG3;    /* 0x00FC */
} systemBASE2_t;

#define systemREG2 ((systemBASE2_t *)0xFFFFE100U)

typedef volatile struct flashWBase
{
    uint32 FRDCNTL;         /* 0x0000 */
    uint32 rsvd1;           /* 0x0004 */
    uint32 EE_FEDACCTRL1;   /* 0x0008 */
    uint32 rsvd2;           /* 0x000C */
    uint32 rsvd3;           /* 0x0010 */
    uint32 FEDAC_PASTATUS;  /* 0x0014 */
    uint32 FEDAC_PBSTATUS;  /* 0x0018 */
    uint32 FEDAC_GBLSTATUS; /* 0x001C */
    uint32 rsvd4;           /* 0x0020 */
    uint32 FEDACSDIS;       /* 0x0024 */
    uint32 FPRIM_ADD_TAG;   /* 0x0028 */
    uint32 FDUP_ADD_TAG;    /* 0x002C */
    uint32 FBPROT;          /* 0x0030 */
    uint32 FBSE;            /* 0x0034 */
    uint32 FBBUSY;          /* 0x0038 */
    uint32 FBAC;            /* 0x003C */
    uint32 FBPWRMODE;       /* 0x0040 */
    uint32 FBPRDY;          /* 0x0044 */
    uint32 FPAC1;           /* 0x0048 */
    uint32 rsvd5;           /* 0x004C */
    uint32 FMAC;            /* 0x0050 */
    uint32 FMSTAT;          /* 0x0054 */
    uint32 FEMU_DMSW;       /* 0x0058 */
    uint32 FEMU_DLSW;       /* 0x005C */
    uint32 FEMU_ECC;        /* 0x0060 */
    uint32 FLOCK;           /* 0x0064 */
    uint32 rsvd6;           /* 0x0068 */
    uint32 FDIAGCTRL;       /* 0x006C */
    uint32 rsvd7;           /* 0x0070 */
    uint32 FRAW_ADDR;       /* 0x0074 */
    uint32 rsvd8;           /* 0x0078 */
    uint32 FPAR_OVR;        /* 0x007C */
    uint32 rsvd9[13U];      /* 0x0080 - 0x00B0 */
    uint32 RCR_VALID;       /* 0x00B4 */
    uint32 ACC_THRESHOLD;   /* 0x00B8 */
    uint32 rsvd10;          /* 0x00BC */
    uint32 FEDACSDIS2;      /* 0x00C0 */
    uint32 rsvd11;          /* 0x00C4 */
    uint32 rsvd12;          /* 0x00C8 */
    uint32 rsvd13;          /* 0x00CC */
    uint32 RCR_VALUE0;      /* 0x00D0 */
    uint32 RCR_VALUE1;      /* 0x00D4 */
    uint32 rsvd14[108U];    /* 0x00D8 - 0x00284 */
    uint32 FSM_WR_ENA;      /* 0x0288 */
    uint32 rsvd15[11U];     /* 0x028C - 0x002B4 */
    uint32 EEPROM_CONFIG;   /* 0x02B8 */
    uint32 rsvd16;          /* 0x02BC */
    uint32 FSM_SECTOR1;     /* 0x02C0 */
    uint32 FSM_SECTOR2;     /* 0x02C4 */
    uint32 rsvd17[78U];     /* 0x02A8 */
    uint32 FCFG_BANK;       /* 0x02B8 */

} flashWBASE_t;

#define flashWREG ((flashWBASE_t *)(0xFFF87000U))

typedef volatile struct pcrBase
{
    uint32 PMPROTSET0;    /* 0x0000 */
    uint32 PMPROTSET1;    /* 0x0004 */
    uint32 rsvd1[2U];     /* 0x0008 */
    uint32 PMPROTCLR0;    /* 0x0010 */
    uint32 PMPROTCLR1;    /* 0x0014 */
    uint32 rsvd2[2U];     /* 0x0018 */
    uint32 PPROTSET0;     /* 0x0020 */
    uint32 PPROTSET1;     /* 0x0024 */
    uint32 PPROTSET2;     /* 0x0028 */
    uint32 PPROTSET3;     /* 0x002C */
    uint32 rsvd3[4U];     /* 0x0030 */
    uint32 PPROTCLR0;     /* 0x0040 */
    uint32 PPROTCLR1;     /* 0x0044 */
    uint32 PPROTCLR2;     /* 0x0048 */
    uint32 PPROTCLR3;     /* 0x004C */
    uint32 rsvd4[4U];     /* 0x0050 */
    uint32 PCSPWRDWNSET0; /* 0x0060 */
    uint32 PCSPWRDWNSET1; /* 0x0064 */
    uint32 rsvd5[2U];     /* 0x0068 */
    uint32 PCSPWRDWNCLR0; /* 0x0070 */
    uint32 PCSPWRDWNCLR1; /* 0x0074 */
    uint32 rsvd6[2U];     /* 0x0078 */
    uint32 PSPWRDWNSET0;  /* 0x0080 */
    uint32 PSPWRDWNSET1;  /* 0x0084 */
    uint32 PSPWRDWNSET2;  /* 0x0088 */
    uint32 PSPWRDWNSET3;  /* 0x008C */
    uint32 rsvd7[4U];     /* 0x0090 */
    uint32 PSPWRDWNCLR0;  /* 0x00A0 */
    uint32 PSPWRDWNCLR1;  /* 0x00A4 */
    uint32 PSPWRDWNCLR2;  /* 0x00A8 */
    uint32 PSPWRDWNCLR3;  /* 0x00AC */
    uint32 rsvd8[4U];     /* 0x00B0 */
    uint32 PDPWRDWNSET;   /* 0x00C0 */
    uint32 PDPWRDWNCLR;   /* 0x00C4 */
    uint32 rsvd9[78U];    /* 0x00C8 */
    uint32 MSTIDWRENA;    /* 0x0200 */
    uint32 MSTIDENA;      /* 0x0204 */
    uint32 MSTIDDIAGCTRL; /* 0x0208 */
    uint32 rsvd10[61U];   /* 0x020C */
    struct
    {
        uint32 PSxMSTID_L;
        uint32 PSxMSTID_H;
    }PSxMSTID[32];            /* 0x0300 */
    struct
    {
        uint32 PPSxMSTID_L;
        uint32 PPSxMSTID_H;
    }PPSxMSTID[8];            /* 0x0400 */
    struct
    {
        uint32 PPSExMSTID_L;
        uint32 PPSExMSTID_H;
    }PPSExMSTID[32];            /* 0x0440 */
    uint32 PCSxMSTID[32];    /* 0x0540 */
    uint32 PPCSxMSTID[8];    /* 0x05C0 */
} pcrBASE_t;

#define pcrREG1 ((pcrBASE_t *)0xFFFF1000U)

#define pcrREG2 ((pcrBASE_t *)0xFCFF1000U)

#define pcrREG3 ((pcrBASE_t *)0xFFF78000U)

#define FSM_WR_ENA_HL       (*(volatile uint32 *)0xFFF87288U)
#define EEPROM_CONFIG_HL    (*(volatile uint32 *)0xFFF872B8U)
#define FSM_SECTOR1         (*(volatile uint32 *)0xFFF872C0U)
#define FSM_SECTOR2         (*(volatile uint32 *)0xFFF872C4U)
#define FCFG_BANK           (*(volatile uint32 *)0xFFF87400U)
#define LPO_TRIM_VALUE      ((*(volatile uint32 *)0xF00801B4U)>>16U)

enum systemClockSource
{
    SYS_OSC             = 0x0U,  /* oscillator clock Source                */
    SYS_PLL1            = 0x1U,  /* Pll1 clock Source                      */
    SYS_EXTERNAL1       = 0x3U,  /* external clock Source                  */
    SYS_LPO_LOW         = 0x4U,  /* low power oscillator low clock Source  */
    SYS_LPO_HIGH        = 0x5U,  /* low power oscillator high clock Source */
    SYS_PLL2            = 0x6U,  /* Pll2 clock Source                      */
    SYS_EXTERNAL2       = 0x7U,  /* external 2 clock Source                */

    SYS_VCLK            = 0x9U,  /* synchronous VCLK1 clock Source         */
    SYS_PLL2_ODCLK_8    = 0xEU,  /* PLL2_post_ODCLK/8                      */
    SYS_PLL2_ODCLK_16   = 0xFU   /* PLL2_post_ODCLK/8                      */
};

/** @fn void systemInit(void)
*   @brief Initializes System Driver
*
*   This function initializes the System driver.
*
*/

static void setupPLL(void)
{
    /* Disable PLL1 and PLL2 */
    systemREG1->CSDISSET = 0x00000002U | 0x00000040U;

    while((systemREG1->CSDIS & 0x42U) != 0x42U)
      {
	/* Wait */
      }

    /* Clear Global Status Register */
    systemREG1->GBLSTAT = 0x301U;

    /** - Configure PLL control registers */
    /** @b Initialize @b Pll1: */

    /**   - Setup pll control register 1:
    *     - Setup reset on oscillator slip
    *     - Setup bypass on pll slip
    *     - setup Pll output clock divider to max before Lock (R=32)
    *     - Setup reset on oscillator fail
    *     - Setup reference clock divider (NR=8)
    *     - Setup Pll multiplier (NF=150)
    *  -> VCO=300Mhz
    */
    systemREG1->PLLCTL1 =  (uint32)0x00000000U
                        |  (uint32)0x20000000U
                        |  (uint32)((uint32)0x1FU << 24U)
                        |  (uint32)0x00000000U
                        |  (uint32)((uint32)(8U - 1U) << 16U)
                        |  (uint32)((uint32)(150U - 1U) << 8U);

    /**   - Setup pll control register 2
    *     - Setup spreading rate
    *     - Setup bandwidth adjustment
    *     - Setup internal Pll output divider (OD=1)
    *     - Setup spreading amount
    *  -> PLLCLK=300Mhz/32
    */
    systemREG1->PLLCTL2 =  (uint32)((uint32)255U << 22U)
                        |  (uint32)((uint32)7U << 12U)
                        |  (uint32)((uint32)(1U - 1U) << 9U)
                        |  (uint32)61U;

    /** @b Initialize @b Pll2: */

    /**   - Setup pll2 control register :
    *     - setup Pll output clock divider to max before Lock
    *     - Setup reference clock divider
    *     - Setup internal Pll output divider
    *     - Setup Pll multiplier
    */
    systemREG2->PLLCTL3 = (uint32)((uint32)(1U - 1U) << 29U)
                        | (uint32)((uint32)0x1FU << 24U)
                        | (uint32)((uint32)(8U - 1U) << 16U)
                        | (uint32)((uint32)(150U - 1U) << 8U);

    /** - Enable PLL(s) to start up or Lock */
    systemREG1->CSDIS = 0x00000000U
                      | 0x00000000U
                      | 0x00000008U	/* Disable EXTCLKIN */
                      | 0x00000080U	/* Disable EXTCLKIN2 */
                      | 0x00000000U
                      | 0x00000000U
                      | 0x00000000U
                      | 0x00000004U;	/* Always 1 */
}

static void trimLPO(void)
{
    /** @b Initialize Lpo: */
    /** Load TRIM values from OTP if present else load user defined values */
    if (LPO_TRIM_VALUE != 0xFFFFU)
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

static void setupFlash(void)
{
    /** - Setup flash read mode, address wait states and data wait states */
    flashWREG->FRDCNTL =  0x00000000U
                       | (uint32)((uint32)3U << 8U)
                       |  3U;

    /** - Setup flash access wait states for bank 7 */
    FSM_WR_ENA_HL    = 0x5U; /* Enable write access to EEPROM_CONFIG */
    EEPROM_CONFIG_HL = 0x00000002U
                     | (uint32)((uint32)9U << 16U) ;

    /** - Disable write access to flash state machine registers */
    FSM_WR_ENA_HL    = 0x2U;

    /** - Setup flash bank power modes */
    flashWREG->FBPWRMODE = 0x00000000U
                          | (uint32)((uint32)3 << 14U) /* BANK 7 */
                          | (uint32)((uint32)3 << 2U)  /* BANK 1 */
                          | (uint32)((uint32)3 << 0U); /* BANK 0 */
}

static void periphInit(void)
{
    /** - Disable Peripherals before peripheral powerup*/
    systemREG1->CLKCNTL &= 0xFFFFFEFFU;

    /** - Release peripherals from reset and enable clocks to all peripherals */
    /** - Power-up all peripherals */
    pcrREG1->PSPWRDWNCLR0 = 0xFFFFFFFFU;
    pcrREG1->PSPWRDWNCLR1 = 0xFFFFFFFFU;
    pcrREG1->PSPWRDWNCLR2 = 0xFFFFFFFFU;
    pcrREG1->PSPWRDWNCLR3 = 0xFFFFFFFFU;

    pcrREG2->PSPWRDWNCLR0 = 0xFFFFFFFFU;
    pcrREG2->PSPWRDWNCLR1 = 0xFFFFFFFFU;
    pcrREG2->PSPWRDWNCLR2 = 0xFFFFFFFFU;
    pcrREG2->PSPWRDWNCLR3 = 0xFFFFFFFFU;

    pcrREG3->PSPWRDWNCLR0 = 0xFFFFFFFFU;
    pcrREG3->PSPWRDWNCLR1 = 0xFFFFFFFFU;
    pcrREG3->PSPWRDWNCLR2 = 0xFFFFFFFFU;
    pcrREG3->PSPWRDWNCLR3 = 0xFFFFFFFFU;

    /** - Enable Peripherals */
    systemREG1->CLKCNTL |= 0x00000100U;
}

static void mapClocks(void)
{
    uint32 SYS_CSVSTAT, SYS_CSDIS;

    /** @b Initialize @b Clock @b Tree: */
    /** - Setup system clock divider for HCLK: HCLK = GCLK / 2 (150 Mhz) */
    systemREG2->HCLKCNTL = 1U;

    /** - Disable / Enable clock domain */
    systemREG1->CDDIS =
      (uint32)((uint32)0U << 4U ) /* AVCLK1 , 1 - OFF, 0 - ON */
      | (uint32)((uint32)1U << 5U ) /* AVCLK2 , 1 - OFF, 0 - ON */
      | (uint32)((uint32)0U << 8U ) /* VCLK3 , 1 - OFF, 0 - ON */
      | (uint32)((uint32)0U << 9U ) /* VCLK4 , 1 - OFF, 0 - ON */
      | (uint32)((uint32)0U << 10U) /* AVCLK3 , 1 - OFF, 0 - ON */
      | (uint32)((uint32)0U << 11U); /* AVCLK4 , 1 - OFF, 0 - ON */


    /* Always check the CSDIS register to make sure the clock source
     * is turned on and check the CSVSTAT register to make sure the
     * clock source is valid. Then write to GHVSRC to switch the
     * clock.
     */
    /** - Wait for until clocks are locked */
    SYS_CSVSTAT = systemREG1->CSVSTAT;
    SYS_CSDIS = systemREG1->CSDIS;
    while ((SYS_CSVSTAT & ((SYS_CSDIS ^ 0xFFU) & 0xFFU))
	   != ((SYS_CSDIS ^ 0xFFU) & 0xFFU))
      {
        SYS_CSVSTAT = systemREG1->CSVSTAT;
        SYS_CSDIS = systemREG1->CSDIS;
      } /* Wait */

    /* Now the PLLs are locked and the PLL outputs can be sped up */
    /* The R-divider was programmed to be 0xF. Now this divider is
       changed to programmed value */
    systemREG1->PLLCTL1 = (systemREG1->PLLCTL1 & 0xE0FFFFFFU)
                         | (uint32)((uint32)(1U - 1U) << 24U);
    /* SAFETYMCUSW 134 S MR:12.2 <APPROVED> " Clear and write to the
       volatile register " */
    systemREG2->PLLCTL3 = (systemREG2->PLLCTL3 & 0xE0FFFFFFU)
                         | (uint32)((uint32)(1U - 1U) << 24U);

    /* Enable/Disable Frequency modulation */
    systemREG1->PLLCTL2 |= 0x00000000U;

    /** - Map device clock domains to desired sources and configure
          top-level dividers */
    /** - All clock domains are working off the default clock sources
          until now */
    /** - The below assignments can be easily modified using the
          HALCoGen GUI */

    /** - Setup GCLK, HCLK and VCLK clock source for normal operation,
          power down mode and after wakeup */
    systemREG1->GHVSRC = (uint32)((uint32)SYS_PLL1 << 24U)
                       | (uint32)((uint32)SYS_PLL1 << 16U)
                       | (uint32)((uint32)SYS_PLL1 << 0U);

    /** - Setup synchronous peripheral clock dividers for VCLK1, VCLK2, VCLK3
          VCLK = HCLK / 2.  */
    systemREG1->CLKCNTL   = (systemREG1->CLKCNTL & 0xF0FFFFFFU)
                           | (uint32)((uint32)1U << 24U);
    systemREG1->CLKCNTL   = (systemREG1->CLKCNTL & 0xFFF0FFFFU)
                           | (uint32)((uint32)1U << 16U);

    systemREG2->CLK2CNTRL = (systemREG2->CLK2CNTRL & 0xFFFFFFF0U)
                           | (uint32)((uint32)1U << 0U);

    /** - Setup RTICLK1 and RTICLK2 clocks */
    /* RTICLKn = PLL / 8 */
    systemREG1->RCLKSRC = (uint32)((uint32)3U << 8U)
                        | (uint32)((uint32)SYS_PLL1 << 0U);

    /** - Setup asynchronous peripheral clock sources for AVCLK1 and AVCLK2 */
    systemREG1->VCLKASRC = (uint32)((uint32)SYS_VCLK << 8U)
                         | (uint32)((uint32)SYS_VCLK << 0U);

    systemREG2->VCLKACON1 =  (uint32)((uint32)(1U - 1U) << 24U)
                           | (uint32)((uint32)0U << 20U)
                           | (uint32)((uint32)SYS_VCLK << 16U)
                           | (uint32)((uint32)(1U - 1U) << 8U)
                           | (uint32)((uint32)0U << 4U)
                           | (uint32)((uint32)SYS_VCLK << 0U);

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
    //  muxInit();

    /** - Set up flash address and data wait states based on the
     * target CPU clock frequency.  The number of address and data
     * wait states for the target CPU clock frequency are specified in
     * the specific part's datasheet.
     */
    setupFlash();

    /** - Configure the LPO such that HF LPO is as close to 10MHz as possible */
    trimLPO();

    /** - Wait for PLLs to start up and map clock domains to desired
          clock sources */
    mapClocks();
}
