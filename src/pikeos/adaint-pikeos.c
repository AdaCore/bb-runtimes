/****************************************************************************
 *                                                                          *
 *                         GNAT RUN-TIME COMPONENTS                         *
 *                                                                          *
 *                         A D A I N T -  P I K E O S                       *
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
/* Simple helper to create a new thread.  */
#include <p4.h>

extern void vm_cprintf(const char*, ...);

P4_e_t
__gnat_p4_thread_create (P4_thr_t num, P4_prio_t prio, void *code,
			 void *arg, void *stack_base,
			 unsigned long stack_size
#if P4_API_MAJOR == 4
			 , P4_tls_area_t *tls
#endif
)
{
  struct P4_thread_create_str tc;
  P4_regs_t context;
  P4_e_t res;

#if 0
  vm_cprintf ("thread_create: num=%u, prio=%u, code=%p, stack=%p, size=%lu\n",
	      num, prio, code, stack_base, stack_size);
#endif

  /* Initialize context.  */
  res = p4_thread_arg
    (&context, code, P4_STACK (stack_base, stack_size),
     P4_THREAD_ARG_FPU | P4_THREAD_ARG_VEC | P4_THREAD_ARG_DEBUG,
     1, &arg);
  if (res != P4_E_OK)
    {
      vm_cprintf ("p4_thread_arg: failure (%u)\n", res);
      return res;
    }

  tc.name = NULL;
  tc.context = &context;
  tc.prio = prio;
  tc.tp_id = P4_TIMEPART_INHERIT;
  tc.shortexh = P4_UID_INHERIT;
  tc.fullexh = P4_UID_INHERIT;
  tc.ipc_mask = P4_UID_ALL;
  tc.ev_mask = P4_UID_ALL;

  /* ??? P4_THREAD_CREATE_READY has been deprecated, so the macro is
     not visible anymore.  For now its value (0) is used instead  .*/
#if P4_API_MAJOR == 4
  p4_tls_init (tls);

  res = p4_thread_create_syscall (num, &tc, 0, tls);
#else
  res = p4_thread_create (num, &tc, 0);
#endif
  if (res != P4_E_OK)
    vm_cprintf ("p4_thread_create failed: %u\n", res);
  return res;
}

#if P4_API_MAJOR == 4
void
__gnat_p4_tls_init (P4_tls_area_t *tls)
{
  p4_tls_init (tls);
}
#endif

/* ??? This function is empty because the interrupt intialization already
   takes place in the main(). We did not take the time to use the new scheme
   for PikeOS 4.1 as we do not expect to support it in the future. */
void
__gnat_init_interrupts (void)
{
}
