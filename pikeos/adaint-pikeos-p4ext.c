/****************************************************************************
 *                                                                          *
 *                         GNAT RUN-TIME COMPONENTS                         *
 *                                                                          *
 *                  A D A I N T -  P I K E O S - P 4 E X T                  *
 *                                                                          *
 *          Copyright (C) 2009-2020, Free Software Foundation, Inc.         *
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
/* Simple helper to create a new thread.  */
#include <p4ext/p4ext_threads.h>
#include <vm.h>
#include <vm_debug.h>


/* Given the attributes of an Ada task,
   this function creates its underlying thread. */
P4_e_t
__gnat_p4ext_thread_create (P4_prio_t prio, void *code,
			    void *arg, void *stack_base,
			    unsigned long stack_size)
{
  p4ext_thr_attr_t attributes;
  P4_e_t res;

  p4ext_thr_attr_init(&attributes);

  attributes.context_flags = P4_THREAD_ARG_FPU
			   | P4_THREAD_ARG_VEC
			   | P4_THREAD_ARG_DEBUG;

  attributes.prio = prio;

  attributes.stacksize = stack_size;
  attributes.stack = stack_base;

  /* We let p4ext automatically attribute the thread number,
   * by passing NULL as the first argument.
   */
  res = p4ext_thr_create (NULL, &attributes, NULL, code, 1, arg);

  if (res != P4_E_OK) {
    vm_cprintf ("p4ext_thread_create failed: %u\n", res);
  }
  return res;
}

/* On PikeOS 5, p4_fast_set_prio is always inlined; have a wrapper to
   be able to import this function in Ada.  */

P4_prio_t
__gnat_p4_fast_set_prio(P4_prio_t new_prio)
{
  return p4_fast_set_prio (new_prio);
}
