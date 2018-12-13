/****************************************************************************
 *                                                                          *
 *                         GNAT RUN-TIME COMPONENTS                         *
 *                                                                          *
 *                            P I K E O S 4 . 2 - A P P                     *
 *                                                                          *
 *          Copyright (C) 2009-2019, Free Software Foundation, Inc.         *
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

#include <vm.h>

#ifdef DEBUG
#include <vm_debug.h>
#endif

void __gnat_init_exception_frames (void);

void __gnat_init_debug(void)
{
#ifdef DEBUG
  init_gdbstub ("muxa:pikeos-dbg");
  gdb_breakpoint ();
#endif
}

/* `exit` is called when the Ravenscar application terminates. The end of a
   Ravenscar partition is an error; this is the responsability of the
   application to provide an implementation of `exit` that suits the system
   policy for error recovery. This files provides a default implementation that
   shuts down the system. */

void exit (int status)
{
  int error_code;
  static const char exit_msg[] =
    "Unexpected termination in Ravenscar application";

  error_code = p4_hm_raise (P4_HM_TYPE_P4_E,
                            P4_E_STATE,
                            exit_msg, sizeof (exit_msg) - 1);

  /* p4_hm_raise should only return in case of a failure.  */
  vm_cprintf (exit_msg);
  vm_cprintf ("\n vm_hm_raise_error returned with code: %d\n", error_code);

  while (1)
    vm_shutdown (VM_RESPART_MYSELF);
}

void abort (void)
{
  exit (1);
}

/* Maximum number of interrupts.  Interrupts are only supported by Ravenscar
   runtimes on Pikeos 4.  For Ravenscar runtimes, this must be less than or
   equal to the value of System.OS_Interfaces.Max_Interrupts.  */

#define NBR_INTERRUPTS 8

/* Number of interrupts granted.  Read by the runtime.  */
int __gnat_nbr_interrupts = 0;

/* Map between user interrupts (0 based) to system interrupts.  This map
   provides an additional indirection so that the user doesn't need to
   know the values of the properties.  */
P4_intid_t __gnat_interrupts_map[NBR_INTERRUPTS];

/* Set interrupt properties so that it can be used from the Ada runtime. This
   function must be called before any interrupt handler is attached. */

void __gnat_init_interrupts (void)
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

/* This function registers exception frames if we use zero cost exceptions.
   If we are on an ARM target, we do not use those as ARM provides its own
   mechanism for handling zero cost exceptions. */

void __gnat_init_exception_frames (void)
{
#if defined (USE_ZCX) && !defined(__ARM_EABI__)
  /* Register exception frames.  */
  {
    static struct object object;
    extern void __register_frame_info (const void *, struct object *);
    extern char __EH_FRAME__[];

    __register_frame_info (__EH_FRAME__, &object);
  }
#endif
}

/* This works because we do not support C++ on PikeOS.
   If this was the case we would need to explicitly register a
   constructor/destructor that does the work that we need. */

/* We override the C++ constructors to initialize
   the exceptions and the interrupts.*/

void __cxx_global_ctors(void)
{
  __gnat_init_exception_frames ();
  __gnat_init_debug ();
}

void __cxx_local_dtors (void)
{
  exit (0);
}

