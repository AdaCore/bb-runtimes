*startfile:
crti%O%s crtbegin%O%s -u _Unwind_Find_FDE --eh-frame-hdr

*endfile:
crtend%O%s crtn%O%s
