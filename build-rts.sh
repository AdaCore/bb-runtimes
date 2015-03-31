#!/bin/bash

# This script installs all sources for a given bareboard target

# You can use ln for debugging (option --link).
CP="cp -p"

verbose=
objdir=install
debug=0
crossdir=cross-dir
gccdir=no-gcc-dir

set -e

copy()
{
    if [ "$verbose" = "y" ]; then
      echo "Copying $1 to $2"
    fi
    $CP $1 $2
}

while [ $# -ne 0 ]; do
  case $1 in
      -v) verbose=y ;;
      -d) debug=`expr $debug + 1`;;
      --output=*) objdir=`echo $1 | sed -e "s/^--output=//"`;;
      --cross-dir=*) crossdir=`echo $1 | sed -e "s/^--cross-dir=//"`;;
      --objdir=*) objdir=`echo $1 | sed -e "s/^--objdir=//"`;;
      --gcc-dir=*) gccdir=`echo $1 | sed -e "s/^--gcc-dir=//"`;;
      --link) CP="ln -s";;
      -*) echo "unknown option $1"; exit 1;;
      *) break
  esac
  shift
done

if [ $# -ne 2 ]; then
  echo "Usage: $0 [options] config gnat-src"
  echo "
 -v              verbose output
 -d              debug output
 --cross-dir=dir use dir for locating architecture specific files
 --objdir=dir    use dir for installing run time (default: install)
 --link          use symbolic links instead of copying
 "
fi

config=$1
gnatsrc=$2

if [ -d $objdir ]; then
  echo "Object dir \"$objdir\" already exists"
  exit 1
fi

discarded_sources="none"

# Last chance handler with tracebacks
lch_tb_src="a-exctra.ads
            s-traent.ads s-traent.adb
            a-elchha.ads a-elchha.adb
            g-debuti.ads g-debuti.adb"
lch_tb_pairs="a-elchha.ads:a-elchha-zfp.ads
              a-elchha.adb:a-elchha-minimal.adb
              a-exctra.ads:a-exctra-zfp.ads
              s-traceb.ads:s-traceb-cert.ads"
lch_tb_ppc_pairs="$lch_tb_pairs
                  s-traceb.adb:s-traceb-xi-ppc.adb"

img_src="s-imgint.ads s-imgint.adb
	 s-imglli.ads s-imglli.adb
	 s-imgboo.ads s-imgboo.adb
         s-imguns.ads s-imguns.adb
         s-imgllu.ads s-imgllu.adb"

textio_src="a-textio.ads a-textio.adb text_io.ads"
textio_pairs="a-textio.ads:a-textio-zfp.ads a-textio.adb:a-textio-zfp.adb
              g-io-put.adb:g-io-put-stextio.adb"

extra_gnat_raven="$textio_src
                  s-macres.ads s-macres.adb"
extra_gnarl_raven="s-bbcpsp.ads s-bbcpsp.adb"

# Files that implement error and malloc.  For ravenscar full.
libc_files="s-c.ads
            s-cerrno.ads s-cerrno.adb
	    s-cmallo.ads s-cmallo.adb
            s-cstrle.ads s-cstrle.adb"
libc_pairs="s-c.ads:s-c-zfp.ads
            s-cerrno.ads:s-cerrno-zfp.ads
            s-cerrno.adb:s-cerrno-zfp.adb
	    s-cmallo.ads:s-cmallo-zfp.ads
            s-cmallo.adb:s-cmallo-zfp.adb
            s-cstrle.ads:s-cstrle-zfp.ads
            s-cstrle.adb:s-cstrle-zfp.adb"

# Files and pairs for cert libm (ravenscar-full only ?)
# Most of the libm files are already part of ravenscar-full sources
# We need the a-numaux version without -lm
libm_files="s-gcmain.ads s-gcmain.adb
            s-libm.ads s-libm.adb
            s-libsin.adb s-libsin.ads
            s-libdou.adb s-libdou.ads
            s-lisisq.ads s-lisisq.adb
            s-lidosq.ads s-lidosq.adb"

libm_common_pairs="a-ngelfu.adb:a-ngelfu-ada.adb
            	   a-ngelfu.ads:a-ngelfu-ada.ads
            	   a-nlelfu.ads:a-nlelfu-ada.ads
            	   a-nuelfu.ads:a-nuelfu-ada.ads
            	   a-numaux.ads:a-numaux-vxworks.ads
            	   s-gcmain.ads:s-gcmain-ada.ads
            	   s-gcmain.adb:s-gcmain-ada.adb
                   s-libm.ads:s-libm-ada.ads
                   s-libm.adb:s-libm-ada.adb
            	   s-libsin.adb:s-libsin-ada.adb
            	   s-libsin.ads:s-libsin-ada.ads
            	   s-libdou.adb:s-libdou-ada.adb
            	   s-libdou.ads:s-libdou-ada.ads
                   s-lisisq.ads:s-lisisq-ada.ads
                   s-lidosq.ads:s-lidosq-ada.ads"

libm_ada_pairs="$libm_common_pairs
                s-lisisq.adb:s-lisisq-ada.adb
                s-lidosq.adb:s-lidosq-ada.adb"

libm_fpu_pairs="$libm_common_pairs
                s-lisisq.adb:s-lisisq-fpu.adb
                s-lidosq.adb:s-lidosq-fpu.adb"

# Files and pairs for ZCX (ravenscar-full only)
zcx_files="s-except.ads s-except.adb
           s-exctab.ads s-exctab.adb
           s-excmac.ads
           a-exexda.adb
           a-exexpr.adb
           a-exextr.adb
           a-exstat.adb
           a-excpol.adb
           raise-gcc.c raise.h
           adaint-xi.c"

# Not to be used directly, common between arm and gcc.
zcx_common_pairs="s-stalib.ads:s-stalib.ads
                  s-stalib.adb:s-stalib.adb
                  a-except.adb:a-except-2005.adb
                  a-except.ads:a-except-2005.ads
                  a-exexpr.adb:a-exexpr-gcc.adb"

# For gcc zcx
zcx_gcc_pairs="$zcx_common_pairs
               s-excmac.ads:s-excmac-gcc.ads"

# For ARM zcx
zcx_arm_pairs="$zcx_common_pairs
               s-excmac.ads:s-excmac-arm.ads"

zcx_copy()
{
 copy $PWD/src/tconfig.h $objdir/common
 copy $PWD/src/tsystem.h $objdir/common
 copy $gccdir/libgcc/unwind-pe.h $objdir/common
}

zcx_dw2_copy()
{
 zcx_copy
 copy $PWD/src/unwind-dw2-fde-bb.c $objdir/common
 copy $gccdir/libgcc/unwind-dw2-fde.h $objdir/common
}

# libgcc in Ada (div, shifts)
libgcc_files="s-gcc.ads s-gcc.adb
              s-gccshi.ads s-gccshi.adb
              s-gccdiv.ads s-gccdiv.adb"

ppc_fpr_savres_files="powerpc/6xx/restfpr.S
                      powerpc/6xx/restxfpr.S
                      powerpc/6xx/savefpr.S"

ppc_gpr_savres_files="powerpc/6xx/restgpr.S
                      powerpc/6xx/restxgpr.S
                      powerpc/6xx/savegpr.S"

ppc6xx_raven_files="powerpc/6xx/handler.S powerpc/6xx/context_switch.S"

# Arch files for Prep
prep_arch_files="powerpc/prep/qemu-rom.ld
		 powerpc/prep/qemu-ram.ld
		 powerpc/prep/start-rom.S
		 powerpc/prep/start-ram.S
		 powerpc/prep/setup.S"

# Arch files for wrs8449
w8349e_arch_files="powerpc/8349e/ram.ld
 		   powerpc/8349e/start-ram.S
		   powerpc/8349e/setup.S"

w8349e_raven_files="$w8349e_arch_files $ppc6xx_raven_files"

# Arch files for 8641d
mpc8641d_arch_files="powerpc/8641d/qemu-rom.ld
                     powerpc/8641d/ram.ld
 		     powerpc/8641d/start-rom.S
		     powerpc/8641d/setup.S"

p2020_arch_files="powerpc/p2020/start-ram.S
		  powerpc/p2020/setup.S
		  powerpc/p2020/p2020.ld"

ppcspe_raven_files="powerpc/spe/handler.S powerpc/spe/context_switch.S"

p2020_raven_files="$p2020_arch_files $ppcspe_raven_files"

p55_arch_files="powerpc/p5566/start-bam.S
                powerpc/p5566/start-ram.S
                powerpc/p5566/start-flash.S
                powerpc/p5566/setup.S
                powerpc/p5566/setup-pll.S
		powerpc/p5566/bam.ld
		powerpc/p5566/flash.ld
		powerpc/p5566/ram.ld"

visiummcm_arch_files="visium/mcm/start.S"

tms570_arch_files="arm/tms570/tms570.ld
                   arm/tms570/sys_startup.S
                   arm/tms570/flash.ld
                   arm/tms570/monitor.ld
                   arm/tms570/hiram.ld
                   arm/tms570/loram.ld
                   arm/tms570/common.ld
                   arm/tms570/crt0.S
                   arm/tms570/start-ram.S
                   arm/tms570/start-rom.S"

raven_ppc_pairs="$textio_pairs
                 s-bbcppr.adb:s-bbcppr-ppc.adb
                 s-bbcppr.ads:s-bbcppr-ppc.ads
                 s-bbbosu.ads:s-bbbosu-ppc.ads
                 s-bbinte.adb:s-bbinte-ppc.adb
                 s-bbtime.adb:s-bbtime-ppc.adb"

lm3s_arch_files="arm/lm3s/lm3s-rom.ld
                 arm/lm3s/lm3s-ram.ld
                 arm/lm3s/start-rom.S
                 arm/lm3s/start-ram.S
                 arm/lm3s/setup_pll.adb"

stm32f4_arch_files="arm/stm32f4/stm32f4-rom.ld
                arm/stm32f4/stm32f4-ram.ld
                arm/stm32f4/start-rom.S
                arm/stm32f4/start-ram.S
                arm/stm32f4/setup_pll.adb"
#               arm/stm32f4/setup_pll.ads

sam4s_arch_files="arm/sam4s/sam4s-rom.ld
                  arm/sam4s/sam4s-samba.ld
                  arm/sam4s/start-rom.S
                  arm/sam4s/start-ram.S
                  arm/sam4s/setup_pll.ads
                  arm/sam4s/setup_pll.adb"

cortexm4_raven_files="arm/stm32f4/handler.S"

# Create directories.
mkdir $objdir
mkdir $objdir/obj
mkdir $objdir/adalib

ada_src_dirs="arch common"

case $config in
  ravenscar-sfp/* | ravenscar-minimal/* | ravenscar-xtratum/*)
	ada_src_dirs="$ada_src_dirs gnarl-common gnarl-arch"
	;;
  *)
	;;
esac

for d in $ada_src_dirs; do
    mkdir $objdir/$d
done

# The RTS which is based on. Used to copy file using Makefile.hie
case $config in
  zfp*/*) gnat_rts=zfp;;
  ravenscar-sfp/*) gnat_rts=ravenscar-sfp;;
  ravenscar-xtratum/*) gnat_rts=ravenscar-sfp;;
  ravenscar-minimal/*) gnat_rts=ravenscar-minimal;;
  ravenscar-full/*) gnat_rts=ravenscar;;
  ravenscar-full-xtratum/*) gnat_rts=ravenscar;;
  */*) echo "build-rts.sh: unknown runtime config $config"; exit 1;;
esac

if [ "$gnat_rts" = "ravenscar" ]; then
    img_src="" # No need to build img_src as they are already in the RTS.
fi

# Project file for zfp-support configs
zfp_support_prj_files="$PWD/support-prj/runtime.gpr
        $PWD/support-prj/runtime_build.gpr
        $PWD/support-prj/support.gpr"

SED_REMOVE_BIND='/package Binder/,/end Binder/d'
SED_ADD_BIND="/end Linker;/a \\
\\
   package Binder is\\
      for Required_Switches (\\\"Ada\\\") use Binder'Required_Switches (\\\"Ada\\\")\\
        &amp; (\\\"-nostdlib\\\");\\
   end Binder;\\
"

# Arch part of the config (ie the string after the /)
config_arch=$(echo $config | sed -e "s,.*/,,")

# Extract the compiler is use according to the arch.
case $config in
    */x86-linux)
        gnat_target=i686-linux
	;;
    */x86-windows)
        gnat_target=i686-mingw32
	;;
    */sparc-solaris)
        gnat_target=sparc-solaris
	;;
    */psim | */prep* | */8349e | */8321e | */8641d)
        gnat_target=powerpc-elf
	;;
    */p2020 | */mpc5554 | */mpc5634 | */p5566)
        gnat_target=powerpc-eabispe
	;;
    */tms570* | */stm32f4 | */lm3s | */sam4s)
        gnat_target=arm-eabi
	;;
    */erc32)
	gnat_target=erc32-elf
	;;
    */leon)
	gnat_target=leon-elf
	;;
    *xtratum/leon3)
	gnat_target=erc32-elf
	;;
    */leon3)
	gnat_target=leon3-elf
	;;
    */mcm)
	gnat_target=visium-elf
	;;
    *)
	echo "unhandled target in config $config"; exit 1
	;;
esac

# Needed if the script is invoked from a Makefile
unset MAKEFLAGS MAKEOVERRIDES MAKELEVEL
def_cmd="make -f $gnatsrc/Makefile.hie RTS=$gnat_rts \
      TARGET=$gnat_target GNAT_SRC_DIR=$gnatsrc show-sources"

# Get list of sources
defs=`$def_cmd`
# create the variables
eval $defs

if [ $debug -ge 1 ]; then
  echo $def_cmd
  echo "Defs:"
  echo $defs
  echo "LIBGNAT_SOURCES=$LIBGNAT_SOURCES"
  echo "LIBGNARL_SOURCES=$LIBGNARL_SOURCES"
  echo "LIBGNAT_NON_COMPILABLE_SOURCES=$LIBGNAT_NON_COMPILABLE_SOURCES"
fi

# Changes in sources list common to all runtimes
case $config in
  zfp-support/mcm)
	# MCM has its own version of memcpy/memset/xxx
	;;
  *)
	extra_gnat_files="s-memcop.ads s-memcop.adb
                          s-memmov.ads s-memmov.adb
                          s-memset.ads s-memset.adb
                          s-memcom.ads s-memcom.adb"
	extra_target_pairs="s-memcop.ads:s-memcop-zfp.ads
                            s-memcop.adb:s-memcop-zfp.adb"
      ;;
esac

case $gnat_rts in
  zfp)
	extra_gnat_files="$extra_gnat_files
                          s-memory.ads s-memory.adb
                          s-sssita.ads s-sssita.adb
                          a-elchha.ads a-elchha.adb"
        extra_target_pairs="$extra_target_pairs
                            s-memory.ads:s-memory-zfp.ads
                            s-memory.adb:s-memory-zfp.adb
                            s-sssita.ads:s-sssita-xi.ads
                            s-sssita.adb:s-sssita-xi.adb
                            a-elchha.ads:a-elchha-zfp.ads
                            a-elchha.adb:a-elchha-zfp.adb"
	;;
  ravenscar-sfp)
	extra_gnat_files="$extra_gnat_files
                          s-memory.ads s-memory.adb
                          a-elchha.ads a-elchha.adb"
        extra_target_pairs="$extra_target_pairs
                            s-memory.ads:s-memory-zfp.ads
                            s-memory.adb:s-memory-zfp.adb
                            a-elchha.ads:a-elchha-zfp.ads
                            a-elchha.adb:a-elchha-zfp.adb"
	;;
  ravenscar-minimal)
	# Use lcb_tb, s-memory-raven-min
	;;
  ravenscar)
	;;
  */*) echo "build-rts.sh: unhandled runtime $gnat_rts"; exit 1;;
esac

# Complete/modify the list of sources
case $config in
    "zfp/x86-linux" | "zfp/x86-windows" | "zfp/sparc-solaris")
        extra_gnat_files="$extra_gnat_files
                          $textio_src s-textio.ads s-textio.adb
                         s-macres.ads s-macres.adb"
        extra_target_pairs="$extra_target_pairs
                            s-textio.adb:s-textio-stdio.adb
                            s-textio.ads:s-textio-zfp.ads
                            s-macres.adb:s-macres-native.adb
                            $textio_pairs"
        copy $PWD/native/runtime.xml $objdir/runtime.xml
        copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
        ;;
    "zfp/p2020")
        arch_files=$p2020_arch_files
        extra_gnat_files="$extra_gnat_files
                          $textio_src s-textio.ads s-textio.adb
                          s-bb.ads
                          s-bbbopa.ads
                          s-macres.ads s-macres.adb"
        extra_target_pairs="$extra_target_pairs
                            s-textio.adb:s-textio-p2020.adb
                            s-textio.ads:s-textio-zfp.ads
                            s-bbbopa.ads:s-bbbopa-p2020.ads
                            s-macres.adb:s-macres-p2020.adb
                            $textio_pairs"
        copy $PWD/powerpc/p2020/runtime.xml $objdir/runtime.xml
        copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
        ;;
    "ravenscar-sfp/p2020" )
        arch_files=$p2020_raven_files
        extra_gnat_files="$extra_gnat_files
                          $extra_gnat_raven s-bbbopa.ads"
        extra_gnarl_files="$extra_gnarl_raven"
        extra_target_pairs="$extra_target_pairs
                            s-textio.adb:s-textio-p2020.adb
                            $raven_ppc_pairs
                            s-multip.adb:s-multip-raven-default.adb
                            s-bbcpsp.ads:s-bbcpsp-spe.ads
                            s-bbcpsp.adb:s-bbcpsp-spe.adb
                            s-bbbopa.ads:s-bbbopa-p2020.ads
                            s-bbpara.ads:s-bbpara-ppc.ads
                            s-macres.adb:s-macres-p2020.adb
                            s-bbbosu.adb:s-bbbosu-p2020.adb
                            a-intnam.ads:a-intnam-xi-p2020.ads
                            system.ads:system-xi-e500v2-sfp.ads"
	copy $PWD/powerpc/p2020/runtime.xml $objdir/runtime.xml
        copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
        copy $PWD/src/ravenscar_build.gpr $objdir/ravenscar_build.gpr
        ;;
    "ravenscar-full/p2020" )
	# For ZCX -lgcc is required (must be before libravenscar for strlen
	#  and other libc routines).
	# Need additional specs (to re-add crtbegin/crtend)
        sed -e "$SED_ADD_BIND" \
         -e '/-L/a \
        ("-lgnat", "-lgcc", "-lgnat", \
         "--specs=${RUNTIME_DIR(ada)}/link.spec") &amp;' \
            < powerpc/p2020/runtime.xml > $objdir/runtime.xml
	# Need to define CALL_init so that constructors (in particular
	#  ZCX tables registering) are called
        sed -e 's/ASMFLAGS := (/ASMFLAGS := ("-DCALL__init") \& (/' \
            < src/runtime_build.gpr > $objdir/runtime_build.gpr
	zcx_dw2_copy
        copy $PWD/powerpc/prep/link-zcx.spec $objdir/link.spec
        arch_files="$p2020_raven_files"
        discarded_sources="s-sssita.ads s-sssita.adb"
        extra_gnat_files="$extra_gnat_files
                          $extra_gnat_raven $libc_files $libm_files $zcx_files
                          s-bbbopa.ads"
	extra_gnarl_files="$extra_gnarl_raven"
        extra_target_pairs="$extra_target_pairs
                            s-textio.adb:s-textio-p2020.adb
                            $raven_ppc_pairs
                            $libc_pairs
                            $libm_ada_pairs
                            $zcx_gcc_pairs
                            s-multip.adb:s-multip-raven-default.adb
                            s-bbcpsp.ads:s-bbcpsp-spe.ads
                            s-bbcpsp.adb:s-bbcpsp-spe.adb
                            s-bbbopa.ads:s-bbbopa-p2020.ads
                            s-bbpara.ads:s-bbpara-ppc.ads
                            s-macres.adb:s-macres-p2020.adb
                            s-bbbosu.adb:s-bbbosu-p2020.adb
                            a-intnam.ads:a-intnam-xi-p2020.ads
                            system.ads:system-xi-e500v2-full.ads"
        ;;
    "zfp/mpc5554")
        arch_files="powerpc/mpc5554/5554.ld powerpc/mpc5554/start.S"
        extra_gnat_files="$extra_gnat_files
                          $textio_src s-textio.ads s-textio.adb
                          s-macres.ads s-macres.adb"
        extra_target_pairs="$extra_target_pairs
                            s-textio.adb:s-textio-p55.adb
                            s-textio.ads:s-textio-zfp.ads
                            $textio_pairs
                            s-macres.adb:s-macres-p55.adb"
        copy $PWD/powerpc/mpc5554/runtime.xml $objdir/runtime.xml
        copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
        ;;
    "zfp/mpc5634")
        arch_files="powerpc/mpc5634/5634.ld powerpc/mpc5634/start.S"
        extra_gnat_files="$extra_gnat_files
                          $textio_src s-textio.ads s-textio.adb
                          s-macres.ads s-macres.adb"
        extra_target_pairs="$extra_target_pairs
                            s-textio.adb:s-textio-p55.adb
                            s-textio.ads:s-textio-zfp.ads
                            $textio_pairs
                            s-macres.adb:s-macres-p55.adb"
        copy $PWD/powerpc/mpc5634/runtime.xml $objdir/runtime.xml
        copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
        ;;
    "zfp/p5566")
        arch_files=$p55_arch_files
        extra_gnat_files="$extra_gnat_files
                          $textio_src s-textio.ads s-textio.adb
                          s-macres.ads s-macres.adb"
        extra_target_pairs="$extra_target_pairs
                            s-textio.adb:s-textio-p55.adb
                            s-textio.ads:s-textio-zfp.ads
                            $textio_pairs
                            s-macres.adb:s-macres-p55.adb"
        copy $PWD/powerpc/p5566/runtime.xml $objdir/runtime.xml
        copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
        ;;

    "ravenscar-sfp/p5566" )
        arch_files="$p55_arch_files $ppcspe_raven_files"
        extra_gnat_files="$extra_gnat_files
                          $extra_gnat_raven s-bbbopa.ads"
	extra_gnarl_files="$extra_gnarl_raven"
        extra_target_pairs="$extra_target_pairs
                            s-textio.adb:s-textio-p55.adb
                            s-textio.ads:s-textio-zfp.ads
                            $raven_ppc_pairs
                            s-multip.adb:s-multip-raven-default.adb
                            s-bbcpsp.ads:s-bbcpsp-spe.ads
                            s-bbcpsp.adb:s-bbcpsp-spe.adb
                            s-macres.adb:s-macres-p55.adb
                            s-bbpara.ads:s-bbpara-p55.ads
                            s-bbbopa.ads:s-bbbopa-p55.ads
                            s-bbbosu.adb:s-bbbosu-p55.adb
                            a-intnam.ads:a-intnam-xi-p55.ads
                            system.ads:system-xi-e500v2-sfp.ads"
	copy $PWD/powerpc/p5566/runtime.xml $objdir/runtime.xml
        copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
        copy $PWD/src/ravenscar_build.gpr $objdir/ravenscar_build.gpr
        ;;
    "ravenscar-full/p5566" )
	# For ZCX -lgcc is required (must be before libravenscar for strlen
	#  and other libc routines).
	# Need additional specs (to re-add crtbegin/crtend)
        sed -e "$SED_ADD_BIND" \
         -e '/-L/a \
        ("-lgnat", "-lgcc", "-lgnat", \
         "--specs=${RUNTIME_DIR(ada)}/link.spec") &amp;' \
            < powerpc/p5566/runtime.xml > $objdir/runtime.xml
	# Need to define CALL_init so that constructors (in particular
	#  ZCX tables registering) are called
        sed -e 's/ASMFLAGS := (/ASMFLAGS := ("-DCALL__init") \& (/' \
            < src/runtime_build.gpr > $objdir/runtime_build.gpr
	zcx_dw2_copy
        copy $PWD/powerpc/prep/link-zcx.spec $objdir/link.spec
        arch_files="$p55_arch_files $ppcspe_raven_files"
        discarded_sources="s-sssita.ads s-sssita.adb"
        extra_gnat_files="$extra_gnat_files
                          $extra_gnat_raven $libc_files $zcx_files $libm_files"
	extra_gnarl_files="$extra_gnarl_raven s-bbbopa.ads"
        extra_target_pairs="$extra_target_pairs
                            s-textio.adb:s-textio-p55.adb
                            $raven_ppc_pairs
                            $libc_pairs
                            $zcx_gcc_pairs
                            $libm_ada_pairs
                            s-multip.adb:s-multip-raven-default.adb
                            s-bbcpsp.ads:s-bbcpsp-spe.ads
                            s-bbcpsp.adb:s-bbcpsp-spe.adb
                            s-macres.adb:s-macres-p55.adb
                            s-bbpara.ads:s-bbpara-p55.ads
                            s-bbbopa.ads:s-bbbopa-p55.ads
                            s-bbbosu.adb:s-bbbosu-p55.adb
                            a-intnam.ads:a-intnam-xi-p55.ads
                            system.ads:system-xi-e500v2-full.ads"
        ;;
    "zfp/mcm")
        extra_gnat_files="$extra_gnat_files
                          $textio_src s-textio.ads s-textio.adb
                          s-macres.ads s-macres.adb"
        extra_target_pairs="$extra_target_pairs
                            s-textio.adb:s-textio-stdio.adb
                            s-textio.ads:s-textio-zfp.ads
                            s-macres.adb:s-macres-native.adb
                            $textio_pairs"
        copy $PWD/visium/mcm/runtime.xml $objdir/runtime.xml
        copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
        ;;
    "zfp-support/mcm")
	LIBGNAT_SOURCES=
	LIBGNAT_NON_COMPILABLE_SOURCES=
        extra_gnat_files="$extra_gnat_files
                          $textio_src s-textio.ads s-textio.adb
                          s-macres.ads s-macres.adb"
        extra_target_pairs="$extra_target_pairs
                            s-textio.adb:s-textio-stdio.adb
                            s-textio.ads:s-textio-zfp.ads
                            s-macres.adb:s-macres-native.adb
                            $textio_pairs"
        copy $PWD/visium/support/runtime.xml $objdir/runtime.xml
	# Don't use "gnat" as library name, to linke with both the runtime
	# and this zfp-support.
	sed -e 's/"gnat"/"zfp"/' < $PWD/src/runtime_build.gpr \
	    > $objdir/runtime_build.gpr
        ;;
    "zfp/psim")
        arch_files="powerpc/psim/start.S"
        extra_gnat_files="$extra_gnat_files
                          $textio_src s-textio.ads s-textio.adb
                          s-macres.ads s-macres.adb"
        extra_target_pairs="$extra_target_pairs
                            s-textio.adb:s-textio-stdio.adb
                            s-textio.ads:s-textio-zfp.ads
                            s-macres.adb:s-macres-native.adb
                            $textio_pairs"
        copy $PWD/powerpc/psim/runtime.xml $objdir/runtime.xml
        copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
        ;;
    "zfp/prep")
        arch_files=$prep_arch_files
        extra_gnat_files="$extra_gnat_files
                          $textio_src s-textio.ads s-textio.adb
                          s-macres.ads s-macres.adb
                          s-ioport.ads s-ioport.adb"
        extra_target_pairs="$extra_target_pairs
                            s-textio.adb:s-textio-prep.adb
                            s-textio.ads:s-textio-zfp.ads
                            s-macres.adb:s-macres-prep.adb
                            s-ioport.ads:s-ioport-prep.ads
                            s-ioport.adb:s-ioport-prep.adb
                            $textio_pairs"
        copy $PWD/powerpc/prep/runtime.xml $objdir/runtime.xml
        copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
        ;;
    "ravenscar-sfp/prep" \
  | "ravenscar-sfp/prep-nofpu" )
        if [ "$config" = "ravenscar-sfp/prep" ]; then
          sed -e "$SED_REMOVE_BIND" \
              < powerpc/prep/runtime.xml > $objdir/runtime.xml
        else
          sed -e "$SED_REMOVE_BIND" -e "s/-mhard-float/-msoft-float/g" \
              < powerpc/prep/runtime.xml > $objdir/runtime.xml
        fi
        copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
        copy $PWD/src/ravenscar_build.gpr $objdir/ravenscar_build.gpr
        arch_files=$prep_arch_files
        gnarl_arch_files=$ppc6xx_raven_files
        extra_gnat_files="$extra_gnat_files
                          $extra_gnat_raven
                          s-ioport.ads s-ioport.adb"
        extra_gnarl_files="$extra_gnarl_raven"
        extra_target_pairs="$extra_target_pairs
                            s-textio.adb:s-textio-prep.adb
                            $raven_ppc_pairs
	                    s-multip.adb:s-multip-raven-default.adb
                            s-bbcpsp.ads:s-bbcpsp-6xx.ads
                            s-bbcpsp.adb:s-bbcpsp-6xx.adb
                            s-bbpara.ads:s-bbpara-ppc.ads
                            s-macres.adb:s-macres-prep.adb
                            a-intnam.ads:a-intnam-xi-ppc.ads
                            s-bbbosu.adb:s-bbbosu-prep.adb
                            s-ioport.ads:s-ioport-prep.ads
                            s-ioport.adb:s-ioport-prep.adb
                            system.ads:system-xi-ppc-sfp.ads"
        ;;
    "ravenscar-full/prep" )
	# For ZCX -lgcc is required (must be before libravenscar for strlen
	#  and other libc routines).
	# Need additional specs (to re-add crtbegin/crtend)
        sed -e "$SED_ADD_BIND" \
         -e '/-L/a \
        ("-lgnat", "-lgcc", "-lgnat", \
         "--specs=${RUNTIME_DIR(ada)}/link.spec") &amp;' \
            < powerpc/prep/runtime.xml > $objdir/runtime.xml
	# Need to define CALL_init so that constructors (in particular
	#  ZCX tables registering) are called
        sed -e 's/ASMFLAGS := (/ASMFLAGS := ("-DCALL__init") \& (/' \
            < src/runtime_build.gpr > $objdir/runtime_build.gpr
        arch_files=$prep_arch_files
        discarded_sources="s-sssita.ads s-sssita.adb"
        gnarl_arch_files="$ppc6xx_raven_files"
        extra_gnat_files="$extra_gnat_files
                          $extra_gnat_raven $libc_files $libm_files
                          s-ioport.ads s-ioport.adb
                          $zcx_files"
        extra_gnarl_files="$extra_gnarl_raven"
        extra_target_pairs="$extra_target_pairs
                            s-textio.adb:s-textio-prep.adb
                            $raven_ppc_pairs
                            $libc_pairs
                            $libm_ada_pairs
                            $zcx_gcc_pairs
                            s-multip.adb:s-multip-raven-default.adb
                            s-bbcpsp.ads:s-bbcpsp-6xx.ads
                            s-bbcpsp.adb:s-bbcpsp-6xx.adb
                            s-bbpara.ads:s-bbpara-ppc.ads
                            s-macres.adb:s-macres-prep.adb
                            a-intnam.ads:a-intnam-xi-ppc.ads
                            s-bbbosu.adb:s-bbbosu-prep.adb
                            s-ioport.ads:s-ioport-prep.ads
                            s-ioport.adb:s-ioport-prep.adb
                            system.ads:system-xi-ppc-full.ads"
        copy $PWD/powerpc/prep/link-zcx.spec $objdir/link.spec
	zcx_dw2_copy
        ;;
    "zfp/8349e" | "zfp/8641d")
        extra_gnat_files="$extra_gnat_files
                          $textio_src s-textio.ads s-textio.adb
                          s-bb.ads
                          s-bbbopa.ads
                          s-macres.ads s-macres.adb"
        extra_target_pairs="$extra_target_pairs
                            s-textio.adb:s-textio-p2020.adb
                            s-textio.ads:s-textio-zfp.ads
                            $textio_pairs"
	case $config in
	    */8349e)
		arch_files=$w8349e_arch_files
		extra_target_pairs="$extra_target_pairs
                                    s-bbbopa.ads:s-bbbopa-8349e.ads
                                    s-macres.adb:s-macres-8349e.adb"
		copy $PWD/powerpc/8349e/runtime.xml $objdir/runtime.xml
		;;
	    */8641d)
		arch_files=$mpc8641d_arch_files
		extra_target_pairs="$extra_target_pairs
                                    s-bbbopa.ads:s-bbbopa-8641d.ads
                                    s-macres.adb:s-macres-p2020.adb"
		copy $PWD/powerpc/8641d/runtime.xml $objdir/runtime.xml
		;;
	    *) exit 2;;
	esac
        copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
        ;;
    "ravenscar-sfp/8349e")
        arch_files=$w8349e_raven_files
        extra_gnat_files="$extra_gnat_files
                          $extra_gnat_raven
                          s-bbbopa.ads
                          $libgcc_files"
	extra_gnarl_files="g-intpri.ads $extra_gnarl_raven"
        extra_target_pairs="$extra_target_pairs
                            s-textio.adb:s-textio-p2020.adb
                            $raven_ppc_pairs
                            s-multip.adb:s-multip-raven-default.adb
                            s-bbcpsp.ads:s-bbcpsp-6xx.ads
                            s-bbcpsp.adb:s-bbcpsp-6xx.adb
                            s-bbbopa.ads:s-bbbopa-${config_arch}.ads
                            s-bbbosu.adb:s-bbbosu-8349e.adb
                            s-bbpara.ads:s-bbpara-ppc.ads
                            a-intnam.ads:a-intnam-xi-8349e.ads
                            system.ads:system-xi-ppc-sfp.ads
                            s-macres.adb:s-macres-8349e.adb"
	sed -e "$SED_REMOVE_BIND" \
	    < $PWD/powerpc/8349e/runtime.xml > $objdir/runtime.xml
        copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
        copy $PWD/src/ravenscar_build.gpr $objdir/ravenscar_build.gpr
        ;;
    "ravenscar-sfp/8641d")
        arch_files="$mpc8641d_arch_files $ppc6xx_raven_files"
        extra_gnat_files="$extra_gnat_files
                          $extra_gnat_raven
                          s-bbbopa.ads
                          $libgcc_files"
        extra_gnarl_files="$extra_gnarl_raven"
        extra_target_pairs="$extra_target_pairs
                            s-textio.adb:s-textio-p2020.adb
                            $raven_ppc_pairs
                            s-multip.adb:s-multip-8641d.adb
                            s-bbcpsp.ads:s-bbcpsp-6xx.ads
                            s-bbcpsp.adb:s-bbcpsp-6xx.adb
                            s-bbbopa.ads:s-bbbopa-8641d.ads
                            s-bbbosu.adb:s-bbbosu-8641d.adb
                            s-bbpara.ads:s-bbpara-8641d.ads
                            s-bcprmu.adb:s-bcprmu-8641d.adb
                            a-intnam.ads:a-intnam-xi-p2020.ads
                            system.ads:system-xi-e500v2-sfp.ads
                            s-macres.adb:s-macres-p2020.adb"
	copy $PWD/powerpc/8641d/runtime.xml $objdir/runtime.xml
        copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
        copy $PWD/src/ravenscar_build.gpr $objdir/ravenscar_build.gpr
        ;;
    "ravenscar-full/8641d" )
	# For ZCX -lgcc is required (must be before libravenscar for strlen
	#  and other libc routines).
	# Need additional specs (to re-add crtbegin/crtend)
        sed -e "$SED_ADD_BIND" \
         -e '/-L/a \
        ("-lgnat", "-lgcc", "-lgnat", \
         "--specs=${RUNTIME_DIR(ada)}/link.spec") &amp;' \
            < powerpc/8641d/runtime.xml > $objdir/runtime.xml
	# Need to define CALL_init so that constructors (in particular
	#  ZCX tables registering) are called
        sed -e 's/ASMFLAGS := (/ASMFLAGS := ("-DCALL__init") \& (/' \
            < src/runtime_build.gpr > $objdir/runtime_build.gpr
        arch_files="$mpc8641d_arch_files"
        discarded_sources="s-sssita.ads s-sssita.adb"
        gnarl_arch_files="$ppc6xx_raven_files"
        extra_gnat_files="$extra_gnat_files
                          $extra_gnat_raven $libc_files $libm_files
                          s-bbbopa.ads
                          $zcx_files"
        extra_gnarl_files="$extra_gnarl_raven"
        extra_target_pairs="$extra_target_pairs
                            s-textio.adb:s-textio-p2020.adb
                            $raven_ppc_pairs
                            $libc_pairs
                            $libm_ada_pairs
                            $zcx_gcc_pairs
                            s-multip.adb:s-multip-8641d.adb
                            s-bbcpsp.ads:s-bbcpsp-6xx.ads
                            s-bbcpsp.adb:s-bbcpsp-6xx.adb
                            s-bbbopa.ads:s-bbbopa-8641d.ads
                            s-bbbosu.adb:s-bbbosu-8641d.adb
                            s-bbpara.ads:s-bbpara-8641d.ads
                            s-bcprmu.adb:s-bcprmu-8641d.adb
                            a-intnam.ads:a-intnam-xi-p2020.ads
                            system.ads:system-xi-e500v2-full.ads
                            s-macres.adb:s-macres-p2020.adb"
        copy $PWD/powerpc/prep/link-zcx.spec $objdir/link.spec
	zcx_dw2_copy
        ;;
    "ravenscar-minimal/8349e" \
     | "ravenscar-minimal/8321e")
        arch_files="$w8349e_raven_files $ppc_gpr_savres_files"
        extra_gnat_files="$extra_gnat_files
                          $textio_src
                          s-bbbopa.ads
                          s-tracon.ads s-tracon.adb
                          g-secsta.ads
                          $libgcc_files"
        extra_gnarl_files="s-bbcpsp.ads s-bbcpsp.adb
                           g-intpri.ads"
        extra_target_pairs="$extra_target_pairs
                            s-textio.adb:s-textio-p2020.adb
                            $raven_ppc_pairs
                            s-multip.adb:s-multip-raven-default.adb
                            s-bbcpsp.ads:s-bbcpsp-6xx.ads
                            s-bbcpsp.adb:s-bbcpsp-6xx.adb
                            s-bbbopa.ads:s-bbbopa-${config_arch}.ads
                            s-bbbosu.adb:s-bbbosu-8349e.adb
                            s-bbpara.ads:s-bbpara-ppc.ads
                            a-intnam.ads:a-intnam-xi-${config_arch}.ads
                            system.ads:system-xi-ppc-minimal.ads
                            s-tracon.adb:s-tracon-ppc-eabi.adb
                            s-secsta.ads:s-secsta-zfp.ads
                            s-secsta.adb:s-secsta-zfp.adb
                            s-macres.adb:s-macres-8349e.adb
                            $lch_tb_ppc_pairs
                            s-memory.ads:s-memory-zfp.ads
                            s-memory.adb:s-memory-raven-min.adb
                            g-secsta.ads:g-secsta-minimal.ads"
	img_src="" # Already part of minimal profile
        copy $PWD/powerpc/8349e/runtime.xml $objdir/runtime.xml
        copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
        copy $PWD/src/ravenscar_build.gpr $objdir/ravenscar_build.gpr
        copy $PWD/powerpc/8349e/notes.txt $objdir/notes.txt
        ;;
    "zfp/tms570" | "zfp/tms570-sci")
        extra_gnat_files="$extra_gnat_files
                          $textio_src s-textio.ads s-textio.adb
                          s-macres.ads s-macres.adb"
        extra_target_pairs="$extra_target_pairs
                            system.ads:system-xi-arm.ads
                            s-macres.adb:s-macres-tms570.adb
                            s-textio.ads:s-textio-zfp.ads
                            $textio_pairs"
        arch_files="$tms570_arch_files arm/tms570/nozcx.S"
        if [ "$config" = "zfp/tms570-sci" ]; then
            extra_gnat_files="$extra_gnat_files"
            extra_target_pairs="$extra_target_pairs
                                s-textio.adb:s-textio-tms570-sci.adb"
	    sed -e 's/, "-lgnat"/, "-Wl,-u,__gnat_nozcx", "-lgnat"/' \
                -e '/LOADER/s/PROBE/LORAM/' \
		<  $PWD/arm/tms570/runtime.xml > $objdir/runtime.xml
	else
            extra_target_pairs="$extra_target_pairs
                                s-textio.adb:s-textio-tms570.adb"
	    sed -e 's/, "-lgnat"/, "-Wl,-u,__gnat_nozcx", "-lgnat"/' \
                 < arm/tms570/runtime.xml > $objdir/runtime.xml
	fi
        copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
        ;;
    "ravenscar-sfp/tms570")
        arch_files="$tms570_arch_files arm/tms570/nozcx.S"
        extra_gnat_files="$extra_gnat_files
                          $textio_src
                          s-macres.ads s-macres.adb"
        extra_target_pairs="$extra_target_pairs
                            a-intnam.ads:a-intnam-xi-tms570.ads
                            system.ads:system-xi-arm-sfp.ads
                            s-macres.adb:s-macres-tms570.adb
                            s-textio.adb:s-textio-tms570.adb
                            s-textio.ads:s-textio-zfp.ads
                            s-bbcppr.adb:s-bbcppr-arm.adb
                            s-bbpara.ads:s-bbpara-tms570.ads
                            s-bbbosu.adb:s-bbbosu-tms570.adb
                            s-parame.ads:s-parame-xi-small.ads
                            $textio_pairs"
	sed -e 's/, "-lgnat"/, "-Wl,-u,__gnat_nozcx", "-lgnat"/' \
	    -e "$SED_REMOVE_BIND" \
	    < arm/tms570/runtime.xml > $objdir/runtime.xml
        copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
        copy $PWD/src/ravenscar_build.gpr $objdir/ravenscar_build.gpr
        ;;
    "ravenscar-full/tms570" | "ravenscar-full/tms570-sci")
        discarded_sources="s-sssita.ads s-sssita.adb"
        arch_files="$tms570_arch_files arm/tms570/nozcx.S"
	extra_gnat_files="$extra_gnat_files
                          $textio_src $libc_files $libm_files $zcx_files
                          s-macres.ads s-macres.adb"
        extra_target_pairs="$extra_target_pairs
                            a-intnam.ads:a-intnam-xi-tms570.ads
                            system.ads:system-xi-arm-full.ads
                            s-textio.ads:s-textio-zfp.ads
                            s-macres.adb:s-macres-tms570.adb
                            s-bbcppr.adb:s-bbcppr-arm.adb
                            s-bbpara.ads:s-bbpara-tms570.ads
                            s-bbbosu.adb:s-bbbosu-tms570.adb
                            s-traceb.adb:s-traceb-xi-armeabi.adb
                            s-parame.ads:s-parame-xi-small.ads
                            $zcx_arm_pairs
                            $textio_pairs
                            $libc_pairs
                            $libm_fpu_pairs"
        if [ "$config" = "ravenscar-full/tms570-sci" ]; then
            extra_target_pairs="$extra_target_pairs
                                s-textio.adb:s-textio-tms570-sci.adb"
            sed -e '/LOADER/s/PROBE/LORAM/' \
	       -e '/Asm_Required_Switches := /s/()/("-D__ZCX__")/' \
		<  $PWD/arm/tms570/runtime.xml > $objdir/runtime.xml
	else
            extra_target_pairs="$extra_target_pairs
                                s-textio.adb:s-textio-tms570.adb"
            sed -e '/Asm_Required_Switches := /s/()/("-D__ZCX__")/' \
                 < arm/tms570/runtime.xml > $objdir/runtime.xml
	fi
	copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
	zcx_copy
	;;
    "ravenscar-xtratum/tms570" | "ravenscar-full-xtratum/tms570")
        arch_files="arm/xtratum/cswitch.S
                    arm/xtratum/init.S
                    arm/xtratum/xtratum.ld"
	extra_gnat_files="$extra_gnat_files
                          $textio_src
                          s-macres.ads s-macres.adb"
        extra_target_pairs="$extra_target_pairs
                            a-intnam.ads:a-intnam-xi-tms570.ads
                            s-textio.adb:s-textio-xtratum.adb
                            s-textio.ads:s-textio-zfp.ads
                            s-bbcppr.adb:s-bbcppr-arm-xtratum.adb
                            s-bbpara.ads:s-bbpara-xtratum-tms570.ads
                            s-bbbosu.adb:s-bbbosu-xtratum-arm.adb
                            s-parame.ads:s-parame-xi-small.ads
                            s-macres.adb:s-macres-xtratum.adb
                            s-bbexti.adb:s-bbexti-xtratum.adb
                            $textio_pairs"
        if [ "$config" = "ravenscar-xtratum/tms570" ]; then
	    # ravencar-sfp
            extra_target_pairs="$extra_target_pairs
                                system.ads:system-xi-arm-sfp.ads"
            copy $PWD/arm/xtratum/runtime.xml $objdir/runtime.xml
            copy $PWD/src/ravenscar_build.gpr $objdir/ravenscar_build.gpr
	else
	    # ravenscar-full
            discarded_sources="s-sssita.ads s-sssita.adb"
	    extra_gnat_files="$extra_gnat_files $libc_files"
            extra_target_pairs="$extra_target_pairs
                                $libc_pairs
                                s-traceb.adb:s-traceb-xi-arm.adb
                                system.ads:system-xi-arm-full.ads"
            copy $PWD/arm/xtratum/runtime.xml > $objdir/runtime.xml
	fi
        copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
        ;;
    "zfp/stm32f4")
        arch_files="$stm32f4_arch_files"
        extra_gnat_files="$extra_gnat_files
                          $textio_src s-textio.ads s-textio.adb
                          s-macres.ads s-macres.adb s-stm32f.ads
                          s-bb.ads s-bbpara.ads"
        extra_target_pairs="$extra_target_pairs
                            s-textio.adb:s-textio-stm32f4.adb
                            s-textio.ads:s-textio-zfp.ads
                            s-macres.adb:s-macres-cortexm3.adb
                            $textio_pairs
                            system.ads:system-xi-arm.ads
                            s-bbpara.ads:s-bbpara-stm32f4.ads"
        copy $PWD/arm/$config_arch/runtime.xml $objdir/runtime.xml
        copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
        ;;
    "ravenscar-sfp/stm32f4")
        arch_files="$stm32f4_arch_files $cortexm4_raven_files"
        extra_gnat_files="$extra_gnat_files
                          $textio_src
                          s-macres.adb s-macres.ads s-stm32f.ads"
        extra_target_pairs="$extra_target_pairs
                            a-intnam.ads:a-intnam-xi-stm32f4.ads
                            system.ads:system-xi-cortexm4-sfp.ads
                            s-macres.adb:s-macres-cortexm3.adb
                            s-textio.adb:s-textio-stm32f4.adb
                            s-textio.ads:s-textio-zfp.ads
                            s-bbcppr.adb:s-bbcppr-armv7m.adb
                            s-bbpara.ads:s-bbpara-stm32f4.ads
                            s-bbbosu.adb:s-bbbosu-armv7m.adb
                            s-parame.ads:s-parame-xi-small.ads
                            $textio_pairs"
	sed -e "$SED_REMOVE_BIND" \
	    < arm/stm32f4/runtime.xml > $objdir/runtime.xml
        copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
        copy $PWD/src/ravenscar_build.gpr $objdir/ravenscar_build.gpr
        ;;
    "ravenscar-full/stm32f4")
        discarded_sources="s-sssita.ads s-sssita.adb"
        arch_files="$stm32f4_arch_files $cortexm4_raven_files"
        extra_gnat_files="$extra_gnat_files
                          $textio_src $libc_files
                          s-stm32f.ads s-macres.ads s-macres.adb"
        extra_target_pairs="$extra_target_pairs
                            a-intnam.ads:a-intnam-xi-stm32f4.ads
                            system.ads:system-xi-arm-full.ads
                            s-macres.adb:s-macres-cortexm3.adb
                            s-textio.adb:s-textio-stm32f4.adb
                            s-textio.ads:s-textio-zfp.ads
                            s-bbcppr.adb:s-bbcppr-armv7m.adb
                            s-bbpara.ads:s-bbpara-stm32f4.ads
                            s-bbbosu.adb:s-bbbosu-armv7m.adb
                            s-parame.ads:s-parame-xi-small.ads
                            s-traceb.adb:s-traceb-xi-arm.adb
                            $libc_pairs
                            $textio_pairs"
        copy $PWD/arm/$config_arch/runtime.xml $objdir/runtime.xml
        copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
        ;;
    "zfp/lm3s")
        arch_files="$lm3s_arch_files"
        extra_gnat_files="$extra_gnat_files
                          $textio_src s-textio.ads s-textio.adb
                          s-macres.ads s-macres.adb"
        extra_target_pairs="$extra_target_pairs
                            s-textio.adb:s-textio-lm3s.adb
                            s-textio.ads:s-textio-zfp.ads
                            s-macres.adb:s-macres-cortexm3.adb
                            $textio_pairs
                            system.ads:system-xi-arm.ads"
        copy $PWD/arm/$config_arch/runtime.xml $objdir/runtime.xml
        copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
        ;;
    "zfp/sam4s")
        arch_files="$sam4s_arch_files"
        extra_gnat_files="$extra_gnat_files
                          $textio_src s-textio.ads s-textio.adb
                          s-macres.ads s-macres.adb s-sam4s.ads"
        extra_target_pairs="$extra_target_pairs
                            s-textio.adb:s-textio-sam4s.adb
                            s-textio.ads:s-textio-zfp.ads
                            s-macres.adb:s-macres-cortexm3.adb
                            $textio_pairs
                            system.ads:system-xi-arm.ads"
        copy $PWD/arm/$config_arch/runtime.xml $objdir/runtime.xml
        copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
        ;;
    "ravenscar-sfp/sam4s")
        arch_files="$sam4s_arch_files $cortexm4_raven_files"
        extra_gnat_files="$extra_gnat_files
                          $textio_src
                          s-macres.adb s-macres.ads s-sam4s.ads"
        extra_target_pairs="$extra_target_pairs
                            a-intnam.ads:a-intnam-xi-sam4s.ads
                            system.ads:system-xi-cortexm4-sfp.ads
                            s-macres.adb:s-macres-cortexm3.adb
                            s-textio.adb:s-textio-sam4s.adb
                            s-textio.ads:s-textio-zfp.ads
                            s-bbcppr.adb:s-bbcppr-armv7m.adb
                            s-bbpara.ads:s-bbpara-sam4s.ads
                            s-bbbosu.adb:s-bbbosu-armv7m.adb
                            s-parame.ads:s-parame-xi-small.ads
                            $textio_pairs"
	sed -e "$SED_REMOVE_BIND" \
           < $PWD/arm/$config_arch/runtime.xml > $objdir/runtime.xml
        copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
        copy $PWD/src/ravenscar_build.gpr $objdir/ravenscar_build.gpr
        ;;
    "zfp/erc32" \
  | "zfp/leon"  \
  | "zfp/leon3" \
  | "ravenscar-sfp/erc32"  \
  | "ravenscar-sfp/leon"   \
  | "ravenscar-sfp/leon3"  \
  | "ravenscar-full/erc32" \
  | "ravenscar-full/leon"  \
  | "ravenscar-full/leon3")
	extra_gnat_files="$extra_gnat_files
                          $textio_src s-macres.ads s-macres.adb";
        extra_target_pairs="$extra_target_pairs
                            $textio_pairs
                            s-textio.ads:s-textio-zfp.ads
                            s-macres.adb:s-macres-leon.adb"

	case $config in
	   zfp/*)
		extra_gnat_files="$extra_gnat_files
                                  s-textio.ads s-textio.adb"
		;;
	   ravenscar-sfp/*)
		extra_gnat_files="$extra_gnat_files"
                copy $PWD/src/ravenscar_build.gpr $objdir/ravenscar_build.gpr
		;;
	   ravenscar-full/*)
		extra_gnat_files="$extra_gnat_files
                                  $zcx_files $libm_files"
		extra_target_pairs="$extra_target_pairs
                                    $zcx_gcc_pairs $libm_fpu_pairs"
		zcx_dw2_copy
		;;
	esac

	case $config in
	   */leon)
		extra_target_pairs="$extra_target_pairs
                                    s-textio.adb:s-textio-leon.adb"
		arch_files="$crossdir/leon-elf/leon.ld
                            $crossdir/leon-elf/crt0.S"
		;;
	   */leon3)
		extra_target_pairs="$extra_target_pairs
                                    s-textio.adb:s-textio-leon3.adb"
		arch_files="$crossdir/leon3-elf/leon.ld
                            $crossdir/leon-elf/crt0.S"
		;;
	   */erc32)
		extra_target_pairs="$extra_target_pairs
                                    s-textio.adb:s-textio-erc32.adb"
		arch_files="$crossdir/leon-elf/leon.ld
                            $crossdir/leon-elf/crt0.S"
		;;
	   *) exit 2;;
	esac

	case $config in
	   ravenscar-full/*)
  	      # We also need -lgnat after -lc for abort(3).
	      sed  -e 's@"-nostartfiles",@@' -e "$SED_ADD_BIND" \
                -e '/-L/a \
                "-lgnat", "-lgcc", "-lgnat", \
                "--specs=${RUNTIME_DIR(ada)}/link-zcx.spec",' \
		< $PWD/sparc/$config_arch/runtime.xml > $objdir/runtime.xml
	      discarded_sources="s-sssita.ads s-sssita.adb"
              # Need to define CALL_init so that constructors (in particular
	      #  ZCX tables registering) are called
              sed -e 's/ASMFLAGS := (/ASMFLAGS := ("-DCALL__init") \& (/' \
                < src/runtime_build.gpr > $objdir/runtime_build.gpr
	      # Need crtbegin/crtend
	      copy $crossdir/leon-elf/leon-zcx.specs $objdir/link-zcx.spec
	      ;;
	   ravenscar-sfp/*)
	      sed -e "$SED_REMOVE_BIND" \
		  < $PWD/sparc/$config_arch/runtime.xml > $objdir/runtime.xml
              copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
	      copy $crossdir/leon-elf/leon.specs $objdir/arch/leon.specs
	      ;;
	   zfp/*)
	      copy $PWD/sparc/$config_arch/runtime.xml $objdir/runtime.xml
              copy $PWD/src/runtime_build.gpr $objdir/runtime_build.gpr
	      copy $crossdir/leon-elf/leon.specs $objdir/arch/leon.specs
	      ;;
	    *) echo "leon/leon3: unhandled runtime $config"; exit 1
	      ;;
	esac
        ;;
    "ravenscar-xtratum/leon3" | "ravenscar-full-xtratum/leon3")
        # Using target ERC32 because the dependencies on the concrete target
        # should be handled by XtratuM. Two important reasons to choose ERC32
        # are 1) I would get multiprocessor support when using LEON3, and
        # 2) I would get undesired support for cache control and atomic
        # operations though alternate address spaces (see s-musplo-leon.adb).
        # We need to avoid this because 1) on XtratuM multiprocessors would be
        # handled by the hypervisor, and 2) cache control and alternate address
        # spaces are privileged operations not available in user mode.
        discarded_sources="s-bbsuer.ads
                           s-sssita.ads
                           s-sssita.adb"
        extra_target_pairs="$extra_target_pairs
                            s-textio.adb:s-textio-xtratum.adb
                            s-bbpara.ads:s-bbpara-xtratum-sparc.ads
                            s-bcpith.adb:s-bcpith-xtratum-sparc.adb
	                    s-multip.adb:s-multip-raven-default.adb
                            a-intnam.ads:a-intnam-xi-leon3.ads
                            s-bbbosu.adb:s-bbbosu-xtratum.adb
                            g-io-put.adb:g-io-put-xtratum.adb
                            s-bbexti.adb:s-bbexti-xtratum.adb"
        copy $PWD/sparc/xtratum/runtime.xml $objdir/runtime.xml
        copy $PWD/sparc/runtime_build.gpr $objdir/runtime_build.gpr
        # libxal from XtratuM provides already memcpy, memcmp and memset
        if [ "$config" = "ravenscar-xtratum/leon3" ]; then
            # We want this run time to have the same functionality as
            # the Ravenscar SFP one.
            img_src=""
        fi
        ;;
    *)
        echo "error: unknown config \"$config\""
        exit 1
        ;;
esac

# Build list of sources.

set -e

# Remove pairs from extra_target_pairs of TARGET_PAIRS.
# This allow to remove an override in TARGET_PAIRS.
rmsedcmd="-e s:XYZ:XYZ:"
for i in `echo $extra_target_pairs | sed -e 's/:[a-z0-9-]*\.ad.//g'`; do
    rmsedcmd="$rmsedcmd -e s/$i:[a-z0-9-]*\.ad.//"
done
# Avoid to have an empty command
sedcmd="-e s:XYZ:XYZ:"
for i in $extra_target_pairs `echo $TARGET_PAIRS | sed $rmsedcmd`; do
    sedcmd="$sedcmd -e s:$i:"
done

# Create the list of source files.
libgnat_sources="$LIBGNAT_SOURCES $LIBGNAT_NON_COMPILABLE_SOURCES"
libgnarl_sources="$LIBGNARL_SOURCES"

for f in $discarded_sources; do
    libgnat_sources=`echo $libgnat_sources | sed -e "s/$f//"`
    libgnarl_sources=`echo $libgnarl_sources | sed -e "s/$f//"`
done

# Only ravenscar-sfp and ravenscar-minimal has separate libgnat and libgnarl.
case $config in
    ravenscar-sfp/* | ravenscar-minimal/* | ravenscar-xtratum/* )
	;;
    */*)
	libgnat_sources="$libgnat_sources $libgnarl_sources"
	libgnarl_sources=""
	extra_gnat_files="$extra_gnat_files $extra_gnarl_files"
	extra_gnarl_files=""
	arch_files="$arch_files $gnarl_arch_files"
	gnarl_arch_files=""
	;;
esac

if [ "$verbose" = "y" ]; then
    echo "libgnat sources:"
    echo $libgnat_sources
    echo "libgnarl sources:"
    echo $libgnarl_sources
fi

# Copy libgnat sources.
for f in $libgnat_sources $img_src $extra_gnat_files; do
    case $f in
	system.ads | s-textio.adb | s-macres.adb | s-bbbopa.ads | *.ld \
	 | s-bbpara.ads | s-stm32f.ads)
            dest=$objdir/arch/$f
	    ;;
	*)
	    dest=$objdir/common/$f
	    ;;
    esac

    pairf=`echo $f | sed $sedcmd`
    copy $gnatsrc/$pairf $dest
done

# Copy arch files
for f in $arch_files; do
   case $f in
       /*) copy $f $objdir/arch/`basename $f`;; # For cross files (leon)
        *) copy $PWD/$f $objdir/arch/`basename $f`;;
   esac
done

# Copy libgnarl sources.
for f in $libgnarl_sources $extra_gnarl_files; do
    case $f in
	s-bbbosu.adb | a-intnam.ads | s-bbbopa.ads | s-bbpara.ads)
            dest=$objdir/gnarl-arch/$f
	    ;;
	*)
	    dest=$objdir/gnarl-common/$f
	    ;;
    esac

    pairf=`echo $f | sed $sedcmd`
    copy $gnatsrc/$pairf $dest
done

for f in $gnarl_arch_files; do
   copy $PWD/$f $objdir/gnarl-arch/`basename $f`
done

# Create rts path files
echo adalib > $objdir/ada_object_path
echo $ada_src_dirs | tr ' ' '\n' > $objdir/ada_source_path

exit 0
