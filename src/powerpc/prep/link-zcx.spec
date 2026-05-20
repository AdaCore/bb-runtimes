*startfile:
ncrti.o%s crtbegin.o%s -u _Unwind_Find_FDE --eh-frame-hdr

*endfile:
crtend.o%s ncrtn.o%s
