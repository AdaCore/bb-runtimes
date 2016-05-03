#!/bin/bash

set -e

function error {
  echo "$*" 1>&2 ; exit 2
}

function runcmd {
  echo "$*"
  test -n "$flag_dryrun" || $*
}

# The caller should really export this variable if necessary, so we don't
# need to hardcode absolute paths in this script.
if [ -d /netopt/xtratum/xtratum-4.2.0-tsim/xm ] ; then
    export XTRATUM_PATH=/netopt/xtratum/xtratum-4.2.0-tsim/xm
fi

parentdir=$(dirname $PWD)
export XTRATUM_PATH=${XTRATUM_PATH:-$parentdir/XtratuM/xm}
GNAT_SRC_DIR=${GNAT_SRC_DIR:-$parentdir/gnat}
GCC_SRC_DIR=${GCC_SRC_DIR:-$parentdir/gcc}
CROSS_DIR=${CROSS_DIR:-$parentdir/bareboard/src}
TESTSUITE_DIR=${TESTSUITE_DIR:-$parentdir/ravenscar}
ACATS_DIR=${ACATS_DIR:-$parentdir/acats}

build_opts=""
src_opts="--link"
flag_tests=n
flag_clean=y
flag_verbose=
flag_dryrun=
flag_keepall=
flag_force=

flag_build_rts_py=

build_rts()
{
    prefix=$1
    config=$2
    shift 2

    opts=$*

    rtsdir=$(echo $config|sed s,/,-,g)

    if [ "$prefix" = "x86-linux" ] || $prefix-gcc -v 2> /dev/null ||
       [ "$flag_force" = "y" ];
    then
      objdir=install
      if [ "$flag_keepall" = "y" ] ; then
        # Need per-runtime directory if we want to keep all
         objdir="install/$rtsdir"
      fi
      echo "objdir=$objdir"

      if [ "$flag_clean" = "y" ]; then
         # Don't directly do rm -rf "$objdir" to avoid mistakes
         if [ "$flag_keepall" = "y" ] ; then
            runcmd rm -rf "install/$rtsdir"
         else
            runcmd rm -rf "install"
         fi
	 if [ "$flag_build_rts_py" != "" ]; then
             runcmd ./build-rts.py --output=$objdir --gcc-dir=$GCC_SRC_DIR \
		    --gnat-dir=$GNAT_SRC_DIR $src_opts ${flag_verbose:+-v} \
		    $opts $config  || \
	     return 1
	 else
             runcmd ./build-rts.sh --objdir=$objdir --gcc-dir=$GCC_SRC_DIR \
		 $src_opts ${flag_verbose:+-v} $opts $config $GNAT_SRC_DIR || \
	     return 1
	 fi
      fi

      prj=$objdir/runtime_build.gpr
      if [ -f $objdir/ravenscar_build.gpr ]; then
	  prj=$objdir/ravenscar_build.gpr
      fi
      runcmd gprbuild -j0 ${flag_verbose:+-v} --target=$prefix $prj $build_opts
    else
      echo "$prefix-gcc not found, skipping configuration $config"
      return 1
    fi
}

build_zfp_example()
{
    prefix=$1

    echo "Building examples/hello-zfp"
    cd examples/hello-zfp
    rm -f *.ali *.o
    cmd="gprbuild --target=${prefix} --RTS=../../install -Phello $build_opts"
    echo $cmd
    eval $cmd
    cd ../..
}

build_raven_example()
{
    prefix=$1

    echo "Building examples/tasks-sfp"
    cd examples/tasks-sfp
    rm -f *.ali *.o
    cmd="gprbuild --target=${prefix} --RTS=../../install -Phello $build_opts"
    #cmd="${prefix}-gnatmake -Phello_sfp"
    echo $cmd
    eval $cmd
    cd ../..
}

raven_testsuite()
{
    config=$1

    cmd="./run-tests.sh $config $TESTSUITE_DIR"

    echo $cmd
    eval $cmd
}

do_zfp()
{
    prefix=$1
    config=$2
    shift 2

    opts=$*

    build_rts $prefix $config $opts && build_zfp_example $prefix
}

do_ravenscar()
{
    prefix=$1
    config=$2
    shift 2

    opts=$*

    build_rts "$prefix" $config $opts

    build_zfp_example $prefix
    build_raven_example $prefix

    if [ "$flag_tests" = "y" ]; then
        raven_testsuite $config
    fi
}

build_sfp_prep_nofpu()
{
    do_ravenscar powerpc-elf ravenscar-sfp/prep-nofpu
}

build_sfp_prep()
{
    do_ravenscar powerpc-elf ravenscar-sfp/prep
}

build_full_prep()
{
    do_ravenscar powerpc-elf ravenscar-full/prep
}

build_zfp_prep()
{
    do_zfp powerpc-elf zfp/prep
}

build_zfp_psim()
{
    do_zfp powerpc-elf zfp/psim
}

build_zfp_8349e()
{
    do_zfp powerpc-elf zfp/8349e
}

build_sfp_8349e()
{
    do_ravenscar powerpc-elf ravenscar-sfp/8349e
}

build_zfp_8641d()
{
    do_zfp powerpc-elf zfp/8641d
}

build_sfp_8641d()
{
    do_ravenscar powerpc-elf ravenscar-sfp/8641d
}

build_full_8641d()
{
    do_ravenscar powerpc-elf ravenscar-full/8641d
}

build_minimal_8349e()
{
    do_ravenscar powerpc-elf ravenscar-minimal/8349e
}

build_minimal_8321e()
{
    do_ravenscar powerpc-elf ravenscar-minimal/8321e
}

build_zfp_p2020()
{
    do_zfp powerpc-eabispe zfp/p2020
}

build_sfp_p2020()
{
    do_ravenscar powerpc-eabispe ravenscar-sfp/p2020
}

build_full_p2020()
{
    do_ravenscar powerpc-eabispe ravenscar-full/p2020
}

build_zfp_mpc5554()
{
    do_zfp powerpc-eabispe zfp/mpc5554
}

build_zfp_mpc5634()
{
    do_zfp powerpc-eabispe zfp/mpc5634
}

build_zfp_p5566()
{
    do_zfp powerpc-eabispe zfp/p5566
}

build_sfp_p5566()
{
    do_ravenscar powerpc-eabispe ravenscar-sfp/p5566
}

build_full_p5566()
{
    do_ravenscar powerpc-eabispe ravenscar-full/p5566
}

build_zfp_tms570()
{
    do_zfp arm-eabi zfp/tms570
}

build_zfp_tms570_sci()
{
    do_zfp arm-eabi zfp/tms570-sci
}

build_sfp_tms570()
{
    do_ravenscar arm-eabi ravenscar-sfp/tms570
}

build_full_tms570()
{
    do_ravenscar arm-eabi ravenscar-full/tms570
}

build_full_tms570_sci()
{
    do_ravenscar arm-eabi ravenscar-full/tms570-sci
}

build_sfp_xtratum_tms570()
{
    do_ravenscar arm-eabi ravenscar-xtratum/tms570
}

build_full_xtratum_tms570()
{
    do_ravenscar arm-eabi ravenscar-full-xtratum/tms570
}

build_sfp_rm48()
{
    do_ravenscar arm-eabi ravenscar-sfp/rm48
}

build_zfp_zynq()
{
    do_zfp arm-eabi zfp/zynq
}

build_sfp_zynq()
{
    do_ravenscar arm-eabi ravenscar-sfp/zynq
}

build_zfp_stm32f4()
{
    do_zfp arm-eabi zfp/stm32f4
}

build_sfp_stm32f4()
{
    do_ravenscar arm-eabi ravenscar-sfp/stm32f4
}

build_full_stm32f4()
{
    do_ravenscar arm-eabi ravenscar-full/stm32f4
}

build_zfp_stm32f429()
{
    do_zfp arm-eabi zfp/stm32f429disco
}

build_sfp_stm32f429()
{
    do_ravenscar arm-eabi ravenscar-sfp/stm32f429disco
}

build_full_stm32f429()
{
    do_ravenscar arm-eabi ravenscar-full/stm32f429disco
}

build_zfp_stm32f469()
{
    do_zfp arm-eabi zfp/stm32f469disco
}

build_sfp_stm32f469()
{
    do_ravenscar arm-eabi ravenscar-sfp/stm32f469disco
}

build_full_stm32f469()
{
    do_ravenscar arm-eabi ravenscar-full/stm32f469disco
}

build_zfp_stm32f7()
{
    do_zfp arm-eabi zfp/stm32f7disco
}

build_sfp_stm32f7()
{
    do_ravenscar arm-eabi ravenscar-sfp/stm32f7disco
}

build_full_stm32f7()
{
    do_ravenscar arm-eabi ravenscar-full/stm32f7disco
}

build_zfp_lm3s()
{
    do_zfp arm-eabi zfp/lm3s
}

build_zfp_sam4s()
{
    do_zfp arm-eabi zfp/sam4s
}

build_sfp_sam4s()
{
    do_ravenscar arm-eabi ravenscar-sfp/sam4s
}

build_zfp_samg55()
{
    do_zfp arm-eabi zfp/samg55
}

build_sfp_samg55()
{
    do_ravenscar arm-eabi ravenscar-sfp/samg55
}

build_zfp_visium()
{
    do_zfp visium-elf zfp/mcm
}

build_zfp_support_visium()
{
    do_zfp visium-elf zfp-support/mcm
}

build_sfp_erc32()
{
    do_ravenscar erc32-elf ravenscar-sfp/erc32 --cross-dir=$CROSS_DIR
}

build_full_erc32()
{
    do_ravenscar erc32-elf ravenscar-full/erc32 --cross-dir=$CROSS_DIR
}

build_zfp_leon()
{
    do_zfp leon-elf zfp/leon --cross-dir=$CROSS_DIR
}

build_sfp_leon()
{
    do_ravenscar leon-elf ravenscar-sfp/leon --cross-dir=$CROSS_DIR
}

build_full_leon()
{
    do_ravenscar leon-elf ravenscar-full/leon --cross-dir=$CROSS_DIR
}

build_zfp_leon3()
{
    do_zfp leon3-elf zfp/leon3 --cross-dir=$CROSS_DIR
}

build_sfp_leon3()
{
    do_ravenscar leon3-elf ravenscar-sfp/leon3 --cross-dir=$CROSS_DIR
}

build_full_leon3()
{
    do_ravenscar leon3-elf ravenscar-full/leon3 --cross-dir=$CROSS_DIR
}

build_zfp_leon4()
{
    do_zfp leon3-elf zfp/leon4 --cross-dir=$CROSS_DIR
}

build_sfp_leon4()
{
    do_ravenscar leon3-elf ravenscar-sfp/leon4 --cross-dir=$CROSS_DIR
}

build_full_leon4()
{
    do_ravenscar leon3-elf ravenscar-full/leon4 --cross-dir=$CROSS_DIR
}

build_zfp_x86_linux()
{
    do_zfp x86-linux zfp/x86-linux
}

build_zfp_arm_pikeos()
{
    do_zfp arm-sysgo-pikeos zfp/arm-pikeos
}

build_sfp_arm_pikeos()
{
    do_ravenscar arm-sysgo-pikeos ravenscar-sfp/arm-pikeos
}

build_full_arm_pikeos()
{
    do_ravenscar arm-sysgo-pikeos ravenscar-full/arm-pikeos
}

build_full_ppc_pikeos()
{
    do_ravenscar powerpc-sysgo-pikeos ravenscar-full/ppc-pikeos
}

build_zfp_x86_pikeos()
{
    do_zfp i586-sysgo-pikeos zfp/x86-pikeos
}

build_sfp_x86_pikeos()
{
    do_ravenscar i586-sysgo-pikeos ravenscar-sfp/x86-pikeos
}

build_full_x86_pikeos()
{
    do_ravenscar i586-sysgo-pikeos ravenscar-full/x86-pikeos
}

build_stub()
{
  prefix=$1

  rm -rf stub
  mkdir stub
  cd stub
  ln -s ../powerpc/gdbstub/*.ad? .
  ln -s ../powerpc/gdbstub/*.S .
  ln -s ../powerpc/gdbstub/*.gpr .
  gprbuild --target=$prefix -Pgdbstub --RTS=../install
}

run_acats_full()
{
    rm -rf acats
    mkdir acats
    cd acats
    $ACATS_DIR/run_acats_test.py -d stack,ravenscar --rts=ravenscar --target=ppc-elf,,qemu --project=$PWD/../install/runtime.gpr -a $ACATS_DIR
}

usage()
{
   echo "$0 [options]          - compiles all run times"
   echo "$0 [options] rts ...  - compiles specified runtimes on given target"
   echo
   echo "  -g           build with debug info"
   echo "  -t           run ravenscar test suite"
   echo "  -v           verbose"
   echo "  -n           dry run, echo commands but don't execute"
   echo "  --no-clean   don't remove install directory, do incremental build"
   echo "  --keep-all   use per run-time install directory, so all can be kept"
   echo "  --target=... use specific target, only relevant when rts is given"
   echo "  --no-link    copy files instead of using symbolic links"
   echo
   echo "GNAT_SRC_DIR=$GNAT_SRC_DIR, gnat checkout"
   echo "CROSS_DIR=${CROSS_DIR}, extra sources for LEON targets"
   echo "TESTSUITE_DIR=${TESTSUITE_DIR}, ravenscar check out"
   echo "ACATS_DIR=${ACATS_DIR}, acats check out"
   exit 2
}

# Parse options.
while [ $# -gt 0 ]; do
    case $1 in
        -g) build_opts="$build_opts -g -XBUILD=Debug";;
        -O*) build_opts="$build_opts $1";;
        -j*) build_opts="$build_opts $1";;
        -t) flag_tests=y;;
        -v) flag_verbose="-v";;
        -n) flag_dryrun=y;;
	-f) flag_force=y;;
        --help|-h) usage;;
        --no-clean) flag_clean=n;;
        --keep-all) flag_keepall=y;;
        --target=*) target=`echo $1 | sed -e s/--target=//` ;;
        --no-link) src_opts="";;
        -*) echo "unhandled option $1"; usage;;
        *) break;;
    esac
    shift
done

if [ $# -eq 0 ]; then
  build_opts="-q -j0"

  build_rts arm-eabi zfp/stm32f4
  build_rts arm-eabi zfp/lm3s
  build_rts arm-eabi zfp/tms570
  build_rts arm-eabi zfp/tms570-sci
  build_rts arm-eabi ravenscar-sfp/stm32f4
  build_rts arm-eabi ravenscar-sfp/tms570
  build_rts arm-eabi ravenscar-full/stm32f4
  build_rts arm-eabi ravenscar-full/tms570
  build_rts arm-eabi ravenscar-full/tms570-sci

  build_rts leon-elf zfp/leon --cross-dir=$CROSS_DIR
  build_rts leon-elf ravenscar-sfp/leon --cross-dir=$CROSS_DIR
  build_rts leon-elf ravenscar-full/leon --cross-dir=$CROSS_DIR

  build_rts leon3-elf zfp/leon3 --cross-dir=$CROSS_DIR
  build_rts leon3-elf ravenscar-sfp/leon3 --cross-dir=$CROSS_DIR
  build_rts leon3-elf ravenscar-full/leon3 --cross-dir=$CROSS_DIR

  build_rts powerpc-eabispe ravenscar-full/p2020
  build_rts powerpc-eabispe ravenscar-sfp/p2020
  build_rts powerpc-eabispe zfp/p2020
  build_rts powerpc-eabispe zfp/mpc5554
  build_rts powerpc-eabispe zfp/mpc5634
  build_rts powerpc-eabispe zfp/p5566
  build_rts powerpc-eabispe ravenscar-sfp/p5566
# FIXME:  build_rts powerpc-eabispe ravenscar-full/p5566

  build_rts powerpc-elf ravenscar-sfp/8349e
  build_rts powerpc-elf zfp/8349e
  build_rts powerpc-elf ravenscar-full/prep
  build_rts powerpc-elf ravenscar-sfp/prep
# FIXME:  build_rts powerpc-elf ravenscar-sfp/prep-nofpu
  build_rts powerpc-elf zfp/prep
# FIXME:  build_rts powerpc-elf zfp-support/prep

else
  target=${target:-prep}
  # Parse commands
  for opt; do
      case $target in
	  # PowerPc based
          prep)
              case $opt in
                  full) build_full_prep ;;
                  sfp)  build_sfp_prep ;;
                  zfp)  build_zfp_prep ;;
                  stub) build_stub powerpc-elf ;;
                  raven-sfp) raven_testsuite ravenscar-sfp/prep ;;
                  raven-full) raven_testsuite ravenscar-full/prep ;;
                  acats) run_acats_full ;;
                  gdbstub) build_stub powerpc-elf ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;
          8349e)
              case $opt in
                  zfp)  build_zfp_8349e ;;
                  sfp)  build_sfp_8349e ;;
                  minimal)  build_minimal_8349e ;;
		  raven-minimal) raven_testsuite ravenscar-minimal/8349e ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;
          8321e)
              case $opt in
                  minimal)  build_minimal_8321e ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;
          8641d)
              case $opt in
                  zfp)  build_zfp_8641d ;;
                  sfp)  build_sfp_8641d ;;
                  full) build_full_8641d ;;
		  raven-sfp) raven_testsuite ravenscar-sfp/8641d ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;
          psim)
              case $opt in
                  zfp)  build_zfp_psim ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;

	  # e500 based
          mpc5554)
              case $opt in
                  zfp)  build_zfp_mpc5554 ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;
          mpc5634)
              case $opt in
                  zfp)  build_zfp_mpc5634 ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;
          p2020)
              case $opt in
                  sfp)  build_sfp_p2020 ;;
                  full)  build_full_p2020 ;;
                  zfp)  build_zfp_p2020 ;;
                  raven-sfp) raven_testsuite ravenscar-sfp/p2020 ;;
                  raven-full) raven_testsuite ravenscar-full/p2020 ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;
          p5566)
              case $opt in
                  zfp)  build_zfp_p5566 ;;
                  sfp)  build_sfp_p5566 ;;
                  full)  build_full_p5566 ;;
                  raven-sfp) raven_testsuite ravenscar-sfp/p5566 ;;
                  raven-full) raven_testsuite ravenscar-full/p5566 ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;

	  # GR
          mcm)
              case $opt in
                  zfp)  build_zfp_visium ;;
                  zfp-support)  build_zfp_support_visium ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;

	  # ARM based
          lm3s)
              case $opt in
                  zfp)  build_zfp_lm3s ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;
          stm32f4)
	      flag_build_rts_py=y
              case $opt in
                  zfp)  build_zfp_stm32f4 ;;
                  sfp)  build_sfp_stm32f4 ;;
                  full) build_full_stm32f4 ;;
                  raven-sfp) raven_testsuite ravenscar-sfp/stm32f4 ;;
                  raven-full) raven_testsuite ravenscar-full/stm32f4 ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;
          stm32f40)
	      flag_build_rts_py=y
              case $opt in
                  zfp)  build_zfp_stm32f40 ;;
                  sfp)  build_sfp_stm32f40 ;;
                  full) build_full_stm32f40 ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;
          stm32f429)
	      flag_build_rts_py=y
              case $opt in
                  zfp)  build_zfp_stm32f429 ;;
                  sfp)  build_sfp_stm32f429 ;;
                  full) build_full_stm32f429 ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;
          stm32f469)
	      flag_build_rts_py=y
              case $opt in
                  zfp)  build_zfp_stm32f469 ;;
                  sfp)  build_sfp_stm32f469 ;;
                  full) build_full_stm32f469 ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;
          stm32f7)
	      flag_build_rts_py=y
              case $opt in
                  zfp)  build_zfp_stm32f7 ;;
                  sfp)  build_sfp_stm32f7 ;;
                  full) build_full_stm32f7 ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;
          sam4s)
	      flag_build_rts_py=y
              case $opt in
                  zfp)  build_zfp_sam4s ;;
                  sfp)  build_sfp_sam4s ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;
          samg55)
	      flag_build_rts_py=y
              case $opt in
                  zfp)  build_zfp_samg55 ;;
                  sfp)  build_sfp_samg55 ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;
          tms570)
               case $opt in
                  zfp)  build_zfp_tms570 ;;
                  zfp-sci)  build_zfp_tms570-sci ;;
                  sfp)  build_sfp_tms570 ;;
                  full) build_full_tms570 ;;
                  full-sci) build_full_tms570_sci ;;
                  xtratum-sfp) build_sfp_xtratum_tms570 ;;
                  xtratum-full) build_full_xtratum_tms570 ;;
                  raven-sfp) raven_testsuite ravenscar-sfp/tms570 ;;
                  raven-full) raven_testsuite ravenscar-full/tms570 ;;
                  raven-xtratum-sfp) raven_testsuite ravenscar-sfp/xtratum-tms570 ;;
                  raven-xtratum-full) raven_testsuite ravenscar-full/xtratum-tms570 ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;
          rm48)
               case $opt in
                  sfp)  build_sfp_rm48 ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;
          zynq)
	      flag_build_rts_py=y
               case $opt in
                   zfp)  build_zfp_zynq ;;
		   sfp)  build_sfp_zynq ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;

	  # Sparc based
          erc32)
              case $opt in
                  sfp) build_sfp_erc32 ;;
                  full) build_full_erc32 ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;
          leon)
              case $opt in
                  zfp) build_zfp_leon ;;
                  sfp) build_sfp_leon ;;
                  full) build_full_leon ;;
                  raven-sfp) raven_testsuite ravenscar-sfp/leon ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;
          leon3)
              case $opt in
                  zfp) build_zfp_leon3 ;;
                  sfp) build_sfp_leon3 ;;
                  full) build_full_leon3 ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;
          leon4)
              case $opt in
                  zfp) build_zfp_leon4 ;;
                  sfp) build_sfp_leon4 ;;
                  full) build_full_leon4 ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;

	  # pikeos
          arm-pikeos)
              case $opt in
                  zfp) build_zfp_arm_pikeos ;;
                  sfp) build_sfp_arm_pikeos ;;
                  full) build_full_arm_pikeos ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;
          ppc-pikeos)
              case $opt in
                  full) build_full_ppc_pikeos ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;
          x86-pikeos)
              case $opt in
                  full) build_full_x86_pikeos ;;
                  sfp) build_sfp_x86_pikeos ;;
                  zfp) build_zfp_x86_pikeos ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;

	  # Misc
	  x86-linux)
              case $opt in
                  zfp) build_zfp_x86_linux ;;
                  *) echo "Unknown command $opt for target $target"; exit 2;;
              esac
              ;;
          *)
              echo "Unknown target $target (command $opt)"; exit 2
              ;;
      esac
  done
fi
