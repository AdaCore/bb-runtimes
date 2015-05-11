# User variables. type make to get some help
JOBS=1
TARGET=
GNAT_SOURCES=$(SRC_DIR)/gnat
GCC_SOURCES=$(SRC_DIR)/gcc
CROSS_SOURCES=$(SRC_DIR)/libbareboard

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
    RTS_LIST=ravenscar-sfp-prep ravenscar-full-prep zfp-prep zfp-psim ravenscar-sfp-mpc8641 ravenscar-full-mpc8641 zfp-mpc8641
    NEED_GDBSTUB=yes
endif

ifeq ($(TARGET), powerpc-eabispe)
    RTS_LIST=zfp-p2020 ravenscar-sfp-p2020 ravenscar-full-p2020 zfp-p5566 ravenscar-full-p5566 ravenscar-sfp-p5566
endif

ifeq ($(TARGET), arm-eabi)
    RTS_LIST=zfp-tms570 ravenscar-sfp-tms570 ravenscar-full-tms570 \
	     zfp-lm3s \
             zfp-stm32f4 ravenscar-sfp-stm32f4 ravenscar-full-stm32f4
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

# Helper for creating <config>.src targets.
# you can use it the following way $(BUILD_RTS) config [build-rts opts...]
# you don't have to specify --objdir and gnat sources location.
BUILD_RTS=fun () (if [ \"$(TARGET)\" = \"\" ]; then echo "TARGET not defined"; return 1; fi; \
	          mkdir -p obj; rm -rf obj/$@; config=$$1; shift; \
	          set -x; ./build-rts.sh $$@ --objdir=obj/$@ --cross-dir=$(CROSS_SOURCES) $${config} $(GNAT_SOURCES)); \
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
	@echo "  CROSS_SOURCES    location of cross sources"
	@echo "                   default is $(CROSS_SOURCES)"
	@echo "  PREFIX           required for install targets"
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
	cd obj/$@ && gprbuild --target=$(TARGET) -j$(JOBS) runtime_build.gpr
	if [ -f obj/$@/ravenscar_build.gpr ]; then \
	 cd obj/$@ && \
	 gprbuild --target=$(TARGET) -j$(JOBS) ravenscar_build.gpr; \
	fi
	cd obj/$@ && chmod a-w adalib/*.ali

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
	@$(BUILD_RTS) zfp/prep

ravenscar-sfp-prep.src:
	@$(BUILD_RTS) ravenscar-sfp/prep

ravenscar-full-prep.src:
	@$(BUILD_RTS) ravenscar-full/prep --gcc-dir=$(GCC_SOURCES)

zfp-mpc8641.src:
	@$(BUILD_RTS) zfp/8641d

ravenscar-sfp-mpc8641.src:
	@$(BUILD_RTS) ravenscar-sfp/8641d

ravenscar-full-mpc8641.src:
	@$(BUILD_RTS) ravenscar-full/8641d --gcc-dir=$(GCC_SOURCES)

zfp-psim.src:
	@$(BUILD_RTS) zfp/psim

# powerpc-eabispe runtimes
zfp-p2020.src:
	@$(BUILD_RTS) zfp/p2020

ravenscar-sfp-p2020.src:
	@$(BUILD_RTS) ravenscar-sfp/p2020

ravenscar-full-p2020.src:
	@$(BUILD_RTS) ravenscar-full/p2020 --gcc-dir=$(GCC_SOURCES)

ravenscar-sfp-p5566.src:
	@$(BUILD_RTS) ravenscar-sfp/p5566

ravenscar-full-p5566.src:
	@$(BUILD_RTS) ravenscar-full/p5566 --gcc-dir=$(GCC_SOURCES)

zfp-p5566.src:
	@$(BUILD_RTS) zfp/p5566

zfp-mpc5634.src:
	@$(BUILD_RTS) zfp/mpc5634

# leon-elf runtimes
zfp-leon.src:
	$(BUILD_RTS) zfp/leon

ravenscar-sfp-leon.src:
	$(BUILD_RTS) ravenscar-sfp/leon

ravenscar-full-leon.src:
	$(BUILD_RTS) ravenscar-full/leon --gcc-dir=$(GCC_SOURCES)

# leon3-elf runtimes
zfp-leon3.src:
	$(BUILD_RTS) zfp/leon3

ravenscar-sfp-leon3.src:
	$(BUILD_RTS) ravenscar-sfp/leon3

ravenscar-full-leon3.src:
	$(BUILD_RTS) ravenscar-full/leon3 --gcc-dir=$(GCC_SOURCES)

# arm-eabi runtimes
zfp-tms570.src:
	@$(BUILD_RTS) zfp/tms570

ravenscar-sfp-tms570.src:
	@$(BUILD_RTS) ravenscar-sfp/tms570

ravenscar-full-tms570.src:
	@$(BUILD_RTS) ravenscar-full/tms570 --gcc-dir=$(GCC_SOURCES)

ravenscar-full-tms570-sci.src:
	@$(BUILD_RTS) ravenscar-full/tms570-sci --gcc-dir=$(GCC_SOURCES)

zfp-lm3s.src:
	@$(BUILD_RTS) zfp/lm3s

zfp-stm32f4.src:
	@$(BUILD_RTS) zfp/stm32f4

ravenscar-sfp-stm32f4.src:
	@$(BUILD_RTS) ravenscar-sfp/stm32f4

ravenscar-full-stm32f4.src:
	@$(BUILD_RTS) ravenscar-full/stm32f4 --gcc-dir=$(GCC_SOURCES)

# visium-elf
zfp-mcm.src:
	@$(BUILD_RTS) zfp/mcm

# Native
zfp-x86-linux.src:
	@$(BUILD_RTS) zfp/x86-linux

zfp-x86-windows.src:
	@$(BUILD_RTS) zfp/x86-windows

zfp-sparc-solaris.src:
	@$(BUILD_RTS) zfp/sparc-solaris
