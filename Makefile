# User variables. type make to get some help
JOBS=0
TARGET=
GNAT_SOURCES=$(SRC_DIR)/gnat
GCC_SOURCES=$(SRC_DIR)/gcc
GCC_VERSION=unknown
CROSS_SOURCES=$(SRC_DIR)/libbareboard
NEWLIB_INCLUDE=unknown

###################
# Other variables #
###################

# If yes gdbstub example will be included
NEED_GDBSTUB=no

SRC_DIR:=$(shell pwd)

#########################
# TARGET Configurations #
#########################

# This section defined the list of configuration available for each target
# It's mainly used to compute the prerequisites of the all and install target
#
# When you add a new configuration you need to add also a <config>.src target
# that will perform the right call to build-rts.sh script.

ifeq ($(TARGET), powerpc-elf)
    RTS_LIST=ravenscar-sfp-mpc8641 ravenscar-full-mpc8641 zfp-mpc8641
    NEED_GDBSTUB=yes
endif

ifeq ($(TARGET), powerpc-eabispe)
    RTS_LIST=zfp-p2020 ravenscar-sfp-p2020 ravenscar-full-p2020 zfp-p5566 ravenscar-full-p5566 ravenscar-sfp-p5566
endif

ifeq ($(TARGET), arm-eabi)
    RTS_LIST=zfp-tms570 ravenscar-sfp-tms570 ravenscar-full-tms570 \
	     zfp-lm3s \
             zfp-stm32f4 ravenscar-sfp-stm32f4 ravenscar-full-stm32f4
ifneq ($(GPL),)
    RTS_LIST+=ravenscar-sfp-stm32f429disco ravenscar-full-stm32f429disco \
              ravenscar-sfp-stm32f469disco ravenscar-full-stm32f469disco \
              ravenscar-sfp-stm32f7disco ravenscar-full-stm32f7disco
endif
endif

ifeq ($(TARGET), leon-elf)
    RTS_LIST=zfp-leon ravenscar-sfp-leon ravenscar-full-leon
endif

ifeq ($(TARGET), leon3-elf)
    RTS_LIST=zfp-leon3 ravenscar-sfp-leon3 ravenscar-full-leon3
endif

ifeq ($(TARGET), visium-elf)
    RTS_LIST=zfp-mcm
endif

ifeq ($(TARGET), i686-pc-linux-gnu)
    RTS_LIST=zfp-x86-linux
endif

ifeq ($(TARGET), i686-pc-mingw32)
    RTS_LIST=zfp-x86-windows
endif

ifeq ($(TARGET), sparc-sun-solaris2.8)
    RTS_LIST=zfp-sparc-solaris
endif

ifeq ($(TARGET), arm-sysgo-pikeos)
    RTS_LIST=ravenscar-full-arm-pikeos ravenscar-sfp-arm-pikeos
endif

ifeq ($(TARGET), powerpc-sysgo-pikeos)
    RTS_LIST=ravenscar-full-ppc-pikeos
endif

ifeq ($(TARGET), i586-sysgo-pikeos)
    RTS_LIST=ravenscar-full-x86-pikeos
endif

# Helper for creating <config>.src targets.
# you can use it the following way $(BUILD_RTS) config [build-rts opts...]
# you don't have to specify --objdir and gnat sources location.
BUILD_RTS_OLD=fun () (if [ \"$(TARGET)\" = \"\" ]; then echo "TARGET not defined"; return 1; fi; \
	          mkdir -p obj; rm -rf obj/$@; config=$$1; shift; \
	          set -x; ./build-rts.sh $$@ --objdir=obj/$@ --cross-dir=$(CROSS_SOURCES) $${config} $(GNAT_SOURCES)); \
          fun

BUILD_RTS=fun () (if [ \"$(TARGET)\" = \"\" ]; then echo "TARGET not defined"; return 1; fi; \
	          mkdir -p obj; rm -rf obj/$@; config=$$1; shift; \
	          set -x; ./build-rts.py $$@ --output=obj/$@ --cross-dir=$(CROSS_SOURCES) --gnat-dir=$(GNAT_SOURCES) --gcc-dir=$(GCC_SOURCES) $${config}); \
          fun

# Compute prerequisites for target all
PREREQUISITES:=$(patsubst %, %.build, $(RTS_LIST))

# Compute prerequisites for target install
INSTALL_PREREQUISITES:=$(patsubst %, %.install, $(RTS_LIST))
ifeq ($(NEED_GDBSTUB),yes)
    INSTALL_PREREQUISITES+=install-gdbstub
endif

default:
	@echo "This makefile builds&install recompilable runtimes"
	@echo "Here is the list of variables:"
	@echo
	@echo "  TARGET           specifify the target (mandatory)"
	@echo "  JOBS             set number of jobs"
	@echo "  GNAT_SOURCES     location of GNAT sources"
	@echo "                   default is $(GNAT_SOURCES)"
	@echo "  GCC_SOURCES      location of GCC sources"
	@echo "                   default is $(GCC_SOURCES)"
	@echo "  CROSS_SOURCES    location of cross sources"
	@echo "                   default is $(CROSS_SOURCES)"
	@echo "  PREFIX           required for install targets"
	@echo "  GCC_VERSION      required for install targets on pikeos"
	@echo
	@echo "The makefile accepts the following targets:"
	@echo
	@echo "  make all          Build all runtimes (and maybe libm) for a given target"
	@echo "  make install      Install all runtimes for a given target"
	@echo "  make <config>.src Build runtime sources for 'config' in"
	@echo "                    ./obj/<config>.src". If needed it also build libm.
	@echo "  make <config>.build"
	@echo "                    Build the runtime in ./obj/<config>.build"
	@echo "  make <config>.install"
	@echo "                    Install a given runtime in PREFIX"
	@echo
	@echo "Available configs by target:"
	@echo
	@echo "  powerpc-elf: ravenscar-sfp-prep, ravenscar-full-prep, zfp-prep"


all: $(PREREQUISITES)

install: $(INSTALL_PREREQUISITES)

%.build: %.src
	@if [ "$(TARGET)" = "" ]; then \
	   echo "TARGET variable should be specified"; \
	   exit 1; \
	fi
	mkdir -p obj
	rm -rf obj/$@
	cp -pr obj/$*.src obj/$@
	cd obj/$@ && gprbuild --target=$(TARGET) -j$(JOBS) runtime_build.gpr $(EXTRA_GPRBUILD_OPTIONS) -cargs:C -I$(NEWLIB_INCLUDE)
	if [ -f obj/$@/ravenscar_build.gpr ]; then \
	 cd obj/$@ && \
	 gprbuild --target=$(TARGET) -j$(JOBS) ravenscar_build.gpr $(EXTRA_GPRBUILD_OPTIONS); \
	fi
	cd obj/$@ && chmod a-w adalib/*.ali

# Runtimes to be installed in the standard location (lib/gcc/target/version)
ravenscar-full-arm-pikeos.install ravenscar-full-ppc-pikeos.install ravenscar-full-x86-pikeos.install ravenscar-sfp-arm-pikeos.install :
	@if [ "$(PREFIX)" = "" ]; then \
	   echo "PREFIX variable should be specified"; \
	   exit 1; \
	fi
	mkdir -p $(PREFIX)/lib/gcc/$(TARGET)/$(GCC_VERSION)
	set -x; \
	rts_name=rts-`echo $@ | sed -e 's/-[a-z0-9]*-pikeos.install//'`; \
	rts_dir=$(PREFIX)/lib/gcc/$(TARGET)/$(GCC_VERSION)/$$rts_name; \
	rm -rf $$rts_dir; \
	mkdir $$rts_dir; \
	cp -rp obj/`echo $@ | sed -e 's/\.install/.build/'`/* $$rts_dir

%.install:
	@if [ "$(PREFIX)" = "" ]; then \
	   echo "PREFIX variable should be specified"; \
	   exit 1; \
	fi
	mkdir -p $(PREFIX)/$(TARGET)/lib/gnat
	rm -rf $(PREFIX)/$(TARGET)/lib/gnat/$*
	cp -rp obj/$*.build $(PREFIX)/$(TARGET)/lib/gnat/$*

# GDB stub.
test-gdbstub:
	rm -rf obj/gdbstub
	mkdir -p obj/gdbstub
	for ext in adb ads S gpr; do \
	   cp -rp powerpc/gdbstub/*.$$ext obj/gdbstub; \
	done
	# Compile gdbstub just for testing purposes
	cd obj/gdbstub && gprbuild --target=$(TARGET) -Pstub.gpr

install-gdbstub:
	@if [ "$(PREFIX)" = "" ]; then \
	   echo "PREFIX variable should be specified"; \
	   exit 1; \
	fi
	rm -rf $(PREFIX)/share/examples/gnat-cross/gdbstub/powerpc-elf
	mkdir -p $(PREFIX)/share/examples/gnat-cross/gdbstub/powerpc-elf
	for ext in adb ads S gpr; do \
	   cp -rp powerpc/gdbstub/*.$$ext \
	       $(PREFIX)/share/examples/gnat-cross/gdbstub/powerpc-elf; \
	done

# powerpc-elf runtimes
zfp-prep.src:
	@$(BUILD_RTS_OLD) zfp/prep

ravenscar-sfp-prep.src:
	@$(BUILD_RTS_OLD) ravenscar-sfp/prep

ravenscar-full-prep.src:
	@$(BUILD_RTS_OLD) ravenscar-full/prep --gcc-dir=$(GCC_SOURCES)

zfp-mpc8641.src:
	@$(BUILD_RTS_OLD) zfp/8641d

ravenscar-sfp-mpc8641.src:
	@$(BUILD_RTS_OLD) ravenscar-sfp/8641d

ravenscar-full-mpc8641.src:
	@$(BUILD_RTS_OLD) ravenscar-full/8641d --gcc-dir=$(GCC_SOURCES)

zfp-psim.src:
	@$(BUILD_RTS_OLD) zfp/psim

# powerpc-eabispe runtimes
zfp-p2020.src:
	@$(BUILD_RTS_OLD) zfp/p2020

ravenscar-sfp-p2020.src:
	@$(BUILD_RTS_OLD) ravenscar-sfp/p2020

ravenscar-full-p2020.src:
	@$(BUILD_RTS_OLD) ravenscar-full/p2020 --gcc-dir=$(GCC_SOURCES)

ravenscar-sfp-p5566.src:
	@$(BUILD_RTS_OLD) ravenscar-sfp/p5566

ravenscar-full-p5566.src:
	@$(BUILD_RTS_OLD) ravenscar-full/p5566 --gcc-dir=$(GCC_SOURCES)

zfp-p5566.src:
	@$(BUILD_RTS_OLD) zfp/p5566

zfp-mpc5634.src:
	@$(BUILD_RTS_OLD) zfp/mpc5634

# leon-elf runtimes
zfp-leon.src:
	$(BUILD_RTS_OLD) zfp/leon

ravenscar-sfp-leon.src:
	$(BUILD_RTS_OLD) ravenscar-sfp/leon

ravenscar-full-leon.src:
	$(BUILD_RTS_OLD) ravenscar-full/leon --gcc-dir=$(GCC_SOURCES)

# leon3-elf runtimes
zfp-leon3.src:
	$(BUILD_RTS_OLD) zfp/leon3

ravenscar-sfp-leon3.src:
	$(BUILD_RTS_OLD) ravenscar-sfp/leon3

ravenscar-full-leon3.src:
	$(BUILD_RTS_OLD) ravenscar-full/leon3 --gcc-dir=$(GCC_SOURCES)

# arm-eabi runtimes
zfp-tms570.src:
	@$(BUILD_RTS_OLD) zfp/tms570

ravenscar-sfp-tms570.src:
	@$(BUILD_RTS_OLD) ravenscar-sfp/tms570

ravenscar-full-tms570.src:
	@$(BUILD_RTS_OLD) ravenscar-full/tms570 --gcc-dir=$(GCC_SOURCES)

ravenscar-full-tms570-sci.src:
	@$(BUILD_RTS_OLD) ravenscar-full/tms570-sci --gcc-dir=$(GCC_SOURCES)

zfp-lm3s.src:
	@$(BUILD_RTS_OLD) zfp/lm3s

zfp-stm32f4.src:
	@$(BUILD_RTS) zfp/stm32f4

ravenscar-sfp-stm32f4.src:
	@$(BUILD_RTS) ravenscar-sfp/stm32f4

ravenscar-full-stm32f4.src:
	@$(BUILD_RTS) ravenscar-full/stm32f4

zfp-stm32f429disco.src:
	@$(BUILD_RTS) zfp/stm32f429disco

ravenscar-sfp-stm32f429disco.src:
	@$(BUILD_RTS) ravenscar-sfp/stm32f429disco

ravenscar-full-stm32f429disco.src:
	@$(BUILD_RTS) ravenscar-full/stm32f429disco

zfp-stm32f469disco.src:
	@$(BUILD_RTS) zfp/stm32f469disco

ravenscar-sfp-stm32f469disco.src:
	@$(BUILD_RTS) ravenscar-sfp/stm32f469disco

ravenscar-full-stm32f469disco.src:
	@$(BUILD_RTS) ravenscar-full/stm32f469disco

zfp-stm32f7disco.src:
	@$(BUILD_RTS) zfp/stm32f7disco

ravenscar-sfp-stm32f7disco.src:
	@$(BUILD_RTS) ravenscar-sfp/stm32f7disco

ravenscar-full-stm32f7disco.src:
	@$(BUILD_RTS) ravenscar-full/stm32f7disco

ravenscar-sfp-sam4s.src:
	@$(BUILD_RTS) ravenscar-sfp/sam4s

ravenscar-sfp-samg55.src:
	@$(BUILD_RTS) ravenscar-sfp/samg55

# visium-elf
zfp-mcm.src:
	@$(BUILD_RTS_OLD) zfp/mcm

# Native
zfp-x86-linux.src:
	@$(BUILD_RTS_OLD) zfp/x86-linux

zfp-x86-windows.src:
	@$(BUILD_RTS_OLD) zfp/x86-windows

zfp-sparc-solaris.src:
	@$(BUILD_RTS_OLD) zfp/sparc-solaris

# pikeos
ravenscar-sfp-arm-pikeos.src:
	@$(BUILD_RTS) ravenscar-sfp/arm-pikeos

ravenscar-full-arm-pikeos.src:
	@$(BUILD_RTS) ravenscar-full/arm-pikeos

ravenscar-full-ppc-pikeos.src:
	@$(BUILD_RTS) ravenscar-full/ppc-pikeos

ravenscar-full-x86-pikeos.src:
	@$(BUILD_RTS) ravenscar-full/x86-pikeos
