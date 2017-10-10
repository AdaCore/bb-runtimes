Reconfigurable Run-Time Libraries
=================================

All GNAT runtimes for bare metal targets can be customized, built and installed
for use on targets that slightly differ from the base runtime we provide.

**Note:**
  When making permanent changes to a run-time library, for any reason, ensure
  that you can repeat the process because you may need to do it more than once.

  You may need to do it agin, in particular, when you upgrade to a newer version
  of the compiler, for example to get new functionalities or to get a bug fixed.

  The compiler and the run-time library are tightly coupled so the run-time may
  have changed as well. If so, you will need to reapply your changes to this new
  run-time library instance. Therefore we suggest you manage the differences, in
  addition to the resulting files, under configuration control.

Please refer to the GNAT User's Guide Supplement for Cross Platforms for
instructions on how to customize and install a run-time, in particular the
`Customized Run-Time Libraries` section.
