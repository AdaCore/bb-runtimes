#!/bin/sh

# Ad-hoc script to run the ravenscar testsuite.

flag_continue=
flag_ignore_err=n
flag_remove=y
flag_verbose=
rts=../install

while [ $# -ne 0 ]; do
  case $1 in
      -c) flag_continue=y ;;
      -k) flag_ignore_err=y ;;
      -v) flag_verbose=y ;;
      --RTS=*) rts=`echo $1 | sed 's/--RTS=//'`;;
      -*) echo "unknown option $1"; exit 1;;
      *) break
  esac
  shift
done

if [ $# -lt 2 ]; then
  echo "usage: $0 [-c] [-k] [config] ravenscar-testsuite-path [tests]"
  echo " -c : continue from previous run"
  echo " -k : do not stop in case of error"
  exit 1
fi

config=$1
testsuite_dir=$2
shift 2

build_file=`pwd`/build_output
BUILDOPTS=

case $config in
    ravenscar-sfp/prep-nofpu | ravenscar-sfp/prep | ravenscar-full/prep)
        support_dirs="powerpc-elf ravenscar-sfp"
        discr="no_libc,no_serial_output,no_cache_control,no_accurate_clock,no_long_delay,powerpc,ravenscar-full-prep,ravenscar-sfp-prep,powerpc-elf"
        cross=powerpc-elf
        run=run_prep
        ;;
    ravenscar-sfp/p2020 | ravenscar-full/p2020)
        support_dirs="powerpc-eabispe-p2010 ravenscar-sfp"
        discr="no_libc,no_serial_output,no_cache_control,no_accurate_clock,no_long_delay,powerpc,p2020,powerpc-eabispe,high_interrupt"
        cross=powerpc-eabispe
        run=run_p2020
        ;;
    ravenscar-sfp/p5566)
        support_dirs="powerpc-eabispe-p5566 ravenscar-sfp"
        discr="no_libc,no_serial_output,no_cache_control,no_accurate_clock,no_long_delay,powerpc,p5566,powerpc-eabispe,high_interrupt"
        cross=powerpc-eabispe
        run=run_p5566
        ;;
    ravenscar-minimal/8349e )
        support_dirs="powerpc-elf-8349e ravenscar-sfp"
        discr="no_libc,no_serial_output,no_cache_control,no_accurate_clock,no_long_delay,powerpc,powerpc-elf,ravenscar-minimal-8349e,no_float,high_interrupt"
        cross=powerpc-elf
        run=run_8349e
        ;;
    ravenscar-sfp/8641d)
        support_dirs="powerpc-eabispe-p2010 ravenscar-sfp"
        discr="no_libc,no_serial_output,no_cache_control,no_accurate_clock,no_long_delay,powerpc,powerpc-elf,high_interrupt,smp"
        cross=powerpc-elf
        run=run_8641d
        ;;
    ravenscar-sfp/lm3s)
        support_dirs="powerpc-elf ravenscar-sfp"
        discr="no_libc,no_serial_output,no_cache_control,no_accurate_clock,no_long_delay,arm,lm3s,high_interrupt"
        cross=arm-eabi
        run=run_lm3s
        ;;
    ravenscar-sfp/tms570)
        support_dirs="arm-eabi ravenscar-sfp"
        discr="no_libc,no_serial_output,no_cache_control,no_accurate_clock,no_long_delay,arm"
        cross=arm-eabi
        run=run_tms570
	BUILDOPTS=-XLOADER=HIRAM
        ;;
    ravenscar-sfp/leon)
        support_dirs=leon-elf
        discr="no_libc,no_accurate_clock,no_cache_control,no_long_delay,sparc,leon"
        cross=sparc-elf
        run=run_leon
        ;;
    ravenscar-sfp/linux-x86)
        support_dirs=native
        discr="no_cache_control,no_long_delay,no_interrupt,no_serial_output,x86"
        cross=
        run=run_native
        ;;
    *) echo "$0: unsupported config \"$config\""
        exit 2
        ;;
esac

case $config in
    ravenscar-sfp/* | ravenscar-minimal/* )
        discr="$discr,rts-ravenscar-sfp"
        ;;
    ravenscar-full/*)
        discr="$discr,rts-ravenscar"
        ;;
    *) echo "$0: unsupported config \"$config\" (2)"
        exit 2
        ;;
esac
set -e

run_prep()
{
 ~/work/qemu-couverture/ppc-softmmu/qemu-system-ppc -M prep -nographic -no-reboot -bios - -kernel $1
}

run_p2020()
{
 ~/work/qemu-e500v2/ppc-softmmu/qemu-system-ppc -M p2010rdb -nographic -no-reboot -kernel $1
}

run_8641d()
{
 /gournay.a/users/gingold/sandbox-49/x86-linux/gnatpython/install/lib/python2.7/site-packages/gnatpython/internal/data/libexec/x86-linux/qemu-2.0.0/qemu-system-ppc-20140720 -nographic -M wrsbc8641d_vxworks -no-reboot -bios - -kernel $1
}

run_p5566()
{
  powerpc-eabispe-objcopy --srec-forceS3 -O srec $1 ${1}.srec
  ../examples/mpc5566-bam/sendsrec.py -r /dev/cu.PL* ${1}.srec
}

run_leon()
{
 ~/work/qemu-couverture/sparc-softmmu/qemu-system-sparc -M at697 -nographic -kernel $1 -no-reboot
}

run_lm3s()
{
  cat > lm3s.gdb <<EOF
target remote :3333
monitor reset halt
break exit
break fault
load
c
monitor reset halt
quit
EOF
  arm-eabi-gdb -q -x lm3s.gdb $1
}

run_8349e ()
{
 qemu-system-ppc -nographic -M wrsbc834x_vxworks -no-reboot -L . -bios - -kernel $1
}

run_tms570()
{
  arm-eabi-objcopy --srec-forceS3 -O srec $1 ${1}.srec
  ../examples/mpc5566-monitor/p5566/sendsrec.py -s 115200 -r /dev/cu.usbserial-TI* ${1}.srec
}

if [ "$flag_continue" != "y" ]; then
  rm -f $build_file
  rm -rf ravenscar
  mkdir ravenscar
fi

cd ravenscar
#echo 'project sfp extends "../install/runtime" is' > sfp.gpr
echo 'project sfp is' > sfp.gpr
echo '  for Source_Dirs use (".",' >> sfp.gpr
for f in $support_dirs; do
  echo "	\"$testsuite_dir/support/target-support/$f\"," >> sfp.gpr
done
echo "	\"$testsuite_dir/support\");" >> sfp.gpr
echo 'end sfp;' >> sfp.gpr

if [ $# -eq 0 ]; then
  files=`cd $testsuite_dir/tests; echo c/*.ada; echo d/*.ada; echo e/*.ada`
else
  if [ $# -eq 1 ]; then
     flag_remove=no
  fi
  testnames=$*
  files=`cd $testsuite_dir/tests; for i in $testnames; do echo ?/$i.ada; done`
fi

for file in $files;
do
    dead="false"
    xfail="false"
    #options="-O0 -fno-inline"
    options=-O
    filename=`basename $file`
    name=${filename%\.*}

    if [ -f $name.res ]; then
        echo "Already run $name"
        continue;
    fi

    if [ -f $testsuite_dir/etc/$name.opt ]; then
      opt_out=`opt-parser $discr,32bits,ALL $testsuite_dir/etc/$name.opt`
      #echo $opt_out
      eval "$opt_out"
    fi

    echo "******************** $name **********************************";

    if [ "$dead" != "false" ]; then
       echo "$name:DEAD:$dead"
       echo "$name:DEAD:$dead" >> $build_file

    elif [ "$xfail" != "false" ]; then
       echo "$name:XFAIL:$xfail"
       echo "$name:XFAIL:$xfail" >> $build_file

    else
       # Extract the test
       ${cross}-gnatchop -r -q -gnat05 -w -c $testsuite_dir/tests/$file

       # Build the test
       cmd="gprbuild --target=${cross} --RTS=$rts -q -Psfp.gpr -gnat05 -o $name test $BUILDOPTS -cargs $options"
       [ "$flag_verbose" = "y" ] && echo $cmd

       if eval $cmd; then
          echo "$name:Compilation OK" >> $build_file
	  err=n
       else
          echo "$name:PROBLEM:error during compilation" >> $build_file
	  err=y
       fi

       # Run the test (if build is ok)
       if [ $err = n ]; then
	   $run $name | tee $name.res
	   if ! grep -q PASSED $name.res; then
               echo "FAILURE : $name"
	       err=y
	   fi
       fi
       # Remove the binary (if run is ok), or mark the failure
       if [ $err = n ]; then
           if [ "$flag_remove" = "y" ]; then
              rm $name
           fi
       else
           echo "$name" >> failed
           if [ ! "$flag_ignore_err" = "y" ]; then
               break
           fi
       fi

       # Remove objects
       objs=`echo *.ad? | sed -e s/ad./o/g`
       if [ "$flag_remove" = "y" ]; then
          rm -f $objs *.ad?
       fi

#       leon3-elf-gnatclean test
    fi
done

if [ -f failed ]; then
  echo "Failures:"
  cat failed
  exit 1
fi

#rm *.o
#rm *.ali
#rm *.ads
#rm *.adb

