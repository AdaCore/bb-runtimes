#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <sys/syscall.h>
#include <unistd.h>
#include <sched.h>

void *
__gnat_lwp_self (void)
{
   return (void *) syscall (__NR_gettid);
}

cpu_set_t *
__gnat_cpu_alloc (size_t count)
{
  return CPU_ALLOC (count);
}

size_t
__gnat_cpu_alloc_size (size_t count)
{
  return CPU_ALLOC_SIZE (count);
}

void
__gnat_cpu_free (cpu_set_t *set)
{
  CPU_FREE (set);
}

void
__gnat_cpu_zero (size_t count, cpu_set_t *set)
{
  CPU_ZERO_S (count, set);
}

void
__gnat_cpu_set (int cpu, size_t count, cpu_set_t *set)
{
  /* Ada handles CPU numbers starting from 1, while C identifies the first
     CPU by a 0, so we need to adjust. */
  CPU_SET_S (cpu - 1, count, set);
}
