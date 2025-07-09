#include <signal.h>

static __thread sigset_t set;
static __thread sigset_t oset;
static __thread int signals_disabled = 0;

int __gnat_disable_signals(void)
{
    sigemptyset(&set);
    sigaddset(&set, SIGHUP);
    sigaddset(&set, SIGINT);
    sigaddset(&set, SIGQUIT);
    sigaddset(&set, SIGILL);
    sigaddset(&set, SIGTRAP);
    sigaddset(&set, SIGIOT);
    sigaddset(&set, SIGABRT);
    sigaddset(&set, SIGEMT);
    sigaddset(&set, SIGDEADLK);
    sigaddset(&set, SIGFPE);
    sigaddset(&set, SIGKILL);
    sigaddset(&set, SIGBUS);
    sigaddset(&set, SIGSEGV);
    sigaddset(&set, SIGSYS);
    sigaddset(&set, SIGPIPE);
    sigaddset(&set, SIGALRM);
    sigaddset(&set, SIGTERM);
    sigaddset(&set, SIGUSR1);
    sigaddset(&set, SIGUSR2);
    sigaddset(&set, SIGCHLD);
    sigaddset(&set, SIGCLD);
    sigaddset(&set, SIGPWR);
    sigaddset(&set, SIGWINCH);
    sigaddset(&set, SIGURG);
    sigaddset(&set, SIGPOLL);
    sigaddset(&set, SIGIO);
    sigaddset(&set, SIGSTOP);
    sigaddset(&set, SIGTSTP);
    sigaddset(&set, SIGCONT);
    sigaddset(&set, SIGTTIN);
    sigaddset(&set, SIGTTOU);
    sigaddset(&set, SIGVTALRM);
    sigaddset(&set, SIGPROF);
    sigaddset(&set, SIGXCPU);
    sigaddset(&set, SIGXFSZ);
    sigaddset(&set, SIGDOOM);

    int ret = sigprocmask(SIG_BLOCK, &set, &oset);
    signals_disabled = !ret;
    return ret;
}

int __gnat_enable_signals(void)
{
    if (!signals_disabled) {
        return 0;
    }
    signals_disabled = 0;
    return sigprocmask(SIG_SETMASK, &oset, 0);
}
