/****************************************************************************
 *                                                                          *
 *                         GNAT RUN-TIME COMPONENTS                         *
 *                                                                          *
 *                            P I K E O S - A P P                           *
 *                                                                          *
 *          Copyright (C) 2009-2017, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* This file provides stack declaration and startup code.  */
/* The user is free to recompile this file to fit his needs.  Only 2 macros
   may be modified:

   DEBUG (not defined by default)
     If defined, the application will include a gdb stub and stop before
     initialization.

   STACK_SIZE (0x4000 by default)
     Define the stack size of the environment task.
*/

#include <vm.h>
#ifdef DEBUG
#include <vm_debug.h>
#endif

#if defined (USE_ZCX) && !defined(__ARM_EABI__)
#include <stddef.h>
#include "unwind-dw2-fde.h"
#endif

#ifndef STACK_SIZE
#define STACK_SIZE 0x4000
#endif

/* Stack declaration.  */
#ifdef P4_DECLARE_STACK
P4_DECLARE_STACK(STACK_SIZE);
#else
VM_DECLARE_STACK(STACK_SIZE)
#endif

/* Maximum number of interrupts.  Interrupts are only supported by Ravenscar
   runtimes on Pikeos 4.  For Ravenscar runtimes, this must be less than or
   equal to the value of System.OS_Interfaces.Max_Interrupts.  */

#if P4_API_MAJOR == 4
#define NBR_INTERRUPTS 8
#else
#define NBR_INTERRUPTS 0
#endif

#if NBR_INTERRUPTS > 0

/* Number of interrupts granted.  Read by the runtime.  */
int __gnat_nbr_interrupts = 0;

/* Map between user interrupts (0 based) to system interrupts.  This map
   provides an additional indirection so that the user doesn't need to
   know the values of the properties.  */
P4_intid_t __gnat_interrupts_map[NBR_INTERRUPTS];
#endif

extern void main (void);

void exit (int status) __attribute__((noreturn));
void abort (void) __attribute__((noreturn));

void exit (int status)
{
  int error_code;
  static const char exit_msg[] =
    "Unexpected termination in Ravenscar application";

#if P4_API_MAJOR == 3
  error_code= vm_hm_raise_error (VM_HM_EI_APP_ERROR,
				 VM_HM_ERR_MSG_T_CUSTOM,
				 exit_msg, sizeof (exit_msg) - 1);

#else
  error_code = p4_hm_raise (P4_HM_TYPE_P4_E,
                            P4_E_STATE,
                            exit_msg, sizeof (exit_msg) - 1);
#endif

  /* vm_hm_raise_error and p4_hm_raise should only return in case of a
     failure.  */
  vm_cprintf (exit_msg);
  vm_cprintf ("\n vm_hm_raise_error returned with code: %d\n", error_code);

  while (1)
    vm_shutdown (VM_RESPART_MYSELF);
}

void abort (void)
{
  exit (1);
}

/* Traditionally the entry point of a native application is
   _p4_entry. For an APEX application, it is _begin.
   In order to use the same link options for both personalities,
   _begin is used for native as well. Note that this substitution
   does not seem to be possible since 4.1.  */

#ifdef P4_DECLARE_STACK
void _p4_entry(void)
#else
void _begin(void)
#endif
{
  /* Initialize the system software.  */
  vm_init ();

#ifdef DEBUG
  /* Initialize the gdb stub.  */
  init_gdbstub ("muxa:/%s/%s/dbg");
  gdb_breakpoint ();
#endif

#if defined (USE_ZCX) && !defined(__ARM_EABI__)
  /* Register exception frames.  */
  {
    static struct object object;
    extern void __register_frame_info (const void *, struct object *);
    extern char __EH_FRAME__[];

    __register_frame_info (__EH_FRAME__, &object);
  }
#endif /* USE_ZCX */

#pragma GCC diagnostic ignored "-Wcomment"
  /* This part could be modified by the user.

     Request interrup grants from the system software. In order to allow
     grants, properties must be added in the filesystem and the properties
     are read here. Furthermore, the properties must be readable by this
     partition.

     The interrupt properties could be added to project.xml by adding XML
     elements like:

       <Romimage>
         <properties>
           <prop_dir name="app/main_part/interrupts">
             <prop_interrupt data="0" name="int0"/>
           </prop_dir>
         </properties>
       </Romimage>

     The name of the directory is arbitrary but it must be consistently used.
     Likewise, the name of the interrupt is arbitrary but referenced just
     below. The 'data' value of the interrupt property is the PikeOS intid
     interrupt number. The first interrupt property corresponds to Ada
     Interrupt_ID 0, the second to Interrupt_ID 1, etc.

     The project.xml file must give read and map access to this directory.
     An XML element in the FileAccessTable element for the partition must be
     added for that effect:

       <FileAccess AccessMode="VM_O_RD VM_O_MAP"
         FileName="prop:app/main_part/*"/>
  */
#pragma GCC diagnostic pop
#if NBR_INTERRUPTS > 0
  {
    vm_file_desc_t propdir;
    P4_intid_t irq;
    P4_e_t res;
    int num = 0;

    /* Directory containing the interrupt properties.  */
    res = vm_open("prop:app/main_part/interrupts",
		  VM_O_RD | VM_O_MAP, &propdir);
    if (res != P4_E_OK)
      {
	vm_cprintf ("vm_open failure: %d\n", res);
	exit (1);
      }

    /* Get a grant for the first interrupt (number 0).  */
    res = vm_prop_int_grant(&propdir, "int0", &irq);
    if (res != P4_E_OK)
      {
	vm_cprintf ("vm_prop_int_grant failure: %d\n", res);
	exit (1);
      }
    /* Tell the runtime which PikeOS intid correspond to Ada Interrupt_ID
       num (here 0).  */
    __gnat_interrupts_map[num++] = irq;

    /* Add calls for other interrupts.  */
    /* ... */

    /* Tell the runtime the number of interrupts.  */
    __gnat_nbr_interrupts = num;
  }
#endif

  /* Run the Ada application.  */
  main ();

  exit (0);
}
