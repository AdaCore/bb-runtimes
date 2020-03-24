# User variables. type make to get some help

# Parallelism used when building the runtimes
JOBS=0

# Whether to create symlinks or just copy files when generating the rts
LINK=

# Build with Debug ?
DEBUG=

# The target to build
TARGET=

# Whether to just build BSPs
BSPS=

# Sources of GNAT and GCC
GNAT=$(SRC_DIR)/../gnat
GCC=$(SRC_DIR)/../gcc
GNAT_SOURCES=$(GNAT)
GCC_SOURCES=$(GCC)

RTS_SRCS=

###################
# Other variables #
###################

SRC_DIR:=$(shell pwd)

#########################
# TARGET Configurations #
#########################

TARGETS=none

ifeq ($(TARGET),$(filter $(TARGET),powerpc-elf ppc-elf ppc))
    TGT=powerpc-elf
    TARGETS=mpc8641
endif

ifeq ($(TARGET),$(filter $(TARGET),powerpc-eabispe p55-elf p55))
    TGT=powerpc-eabispe
    TARGETS=p2020 p5566
endif

ifeq ($(TARGET),$(filter $(TARGET),aarch64-elf aarch64))
    TGT=aarch64-elf
    TARGETS=rpi3 rpi3mc zynqmp
endif

ifeq ($(TARGET),$(filter $(TARGET),arm-eabi arm-elf arm))
    TGT=arm-eabi
    TARGETS=zynq7000 rpi2 rpi2mc sam4s samg55 smartfusion2 openmv2 stm32f4 \
       stm32f429disco stm32f469disco stm32f746disco stm32756geval \
       stm32f769disco tms570 tms570_sci tms570lc lm3s cortex-m0 cortex-m0p \
       cortex-m1 cortex-m3 cortex-m4 cortex-m4f cortex-m7f cortex-m7df
endif

ifeq ($(TARGET),$(filter $(TARGET),leon-elf leon2-elf leon leon2))
    TGT=leon-elf
    TARGETS=leon3 leon3-smp leon4 leon4-smp
endif

ifeq ($(TARGET),$(filter $(TARGET),leon3-elf leon3))
   TGT=leon3-elf
   TARGETS=leon3 leon3-smp
endif

ifeq ($(TARGET),$(filter $(TARGET),visium-elf visium))
    TGT=visium-elf
    TARGETS=mcm
endif

ifeq ($(TARGET),$(filter $(TARGET),riscv-elf riscv))
    TGT=riscv-elf
    TARGETS=spike
endif

ifeq ($(TARGET),$(filter $(TARGET),i686-pc-linux-gnu x86-linux))
    TGT=i686-pc-linux-gnu
    TARGETS=x86-linux
endif

ifeq ($(TARGET),$(filter $(TARGET),i686-pc-mingw32 x86-windows))
    TGT=i686-pc-mingw32
    TARGETS=x86-windows
endif

ifeq ($(TARGET),$(filter $(TARGET),x86_64-pc-linux-gnu x86_64-linux))
    TGT=x86_64-pc-linux-gnu
    TARGETS=x86_64-linux
endif

ifeq ($(TARGET),$(filter $(TARGET),x86_64-pc-mingw32 x86_64-windows))
    TGT=x86_64-pc-mingw32
    TARGETS=x86_64-windows
endif

ifeq ($(TARGET),$(filter $(TARGET),arm-sysgo-pikeos arm-pikeos))
    TGT=arm-sysgo-pikeos
    TARGETS=arm-pikeos
endif

ifeq ($(TARGET),$(filter $(TARGET),arm-sysgo-pikeos4 arm-pikeos4.2))
    TGT=arm-sysgo-pikeos4
    TARGETS=arm-pikeos4.2
endif

ifeq ($(TARGET),$(filter $(TARGET),arm-sysgo-pikeos4 arm-pikeos5))
    TGT=arm-sysgo-pikeos5
    TARGETS=arm-pikeos5
endif

ifeq ($(TARGETS), none)
  ifeq ($(TARGET),)
    $(error Error: TARGET is not defined)
  else
    $(error Error: unknown TARGET: '$(TARGET)')
  endif
endif

#########
# Tools #
#########

GCC_PREFIX:=$(abspath $(dir $(shell which $(TGT)-gcc))/..)
ifneq ($(PREFIX),)
  GCC_PREFIX:=$(PREFIX)
endif

BUILD_RTS_FLAGS=
GEN_RTS_FLAGS=
GPRBUILD_FLAGS:=-j$(JOBS) -v -s

ifneq ($(DEBUG),)
  GPRBUILD_FLAGS:=$(GPRBUILD_FLAGS) -XBUILD=Debug
endif

ifneq ($(LINK),)
  BUILD_RTS_FLAGS:=$(BUILD_RTS_FLAGS) --link
  GEN_RTS_FLAGS:=$(GEN_RTS_FLAGS) --link
endif

ifeq ($(BSPS),)
  RTS_SRCS=obj/rts-sources
  BUILD_RTS_FLAGS:=$(BUILD_RTS_FLAGS) --rts-src-descriptor=obj/rts.json
endif

GPRBUILD:=GPR_PROJECT_PATH=obj/$(TGT)/lib/gnat gprbuild $(GPRBUILD_FLAGS)
GPRINSTALL:=GPR_PROJECT_PATH=obj/$(TGT)/lib/gnat gprinstall \
              --prefix=$(GCC_PREFIX) -f -p
BUILD_RTS:=./build_rts.py $(BUILD_RTS_FLAGS)
GEN_RTS:=./gen_rts_sources.py $(GEN_RTS_FLAGS)


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
	@echo "  PREFIX           overrides the default prefix (gcc dir)"
	@echo
	@echo "The makefile accepts the following targets:"
	@echo
	@echo "  make all          Build all runtimes for a given target"
	@echo "  make install      Install all runtimes for a given target"
	@echo "  make srcs         Build runtime sources in ./obj"
	@echo "  make <board>.build"
	@echo "                    Build the runtimes for the board"
	@echo "  make <board>.fullbuild"
	@echo "                    Build the full runtime for the board"
	@echo "  make <board>.sfpbuild"
	@echo "                    Build the sfp runtime for the board"
	@echo "  make <board>.zfpbuild"
	@echo "                    Build the zfp runtime for the board"
	@echo "  make <board>.install"
	@echo "                    Install the board's rts in gcc"
	@echo "  make <board>.fullinstall"
	@echo "                    Install the board's full rts in gcc"
	@echo "  make <board>.sfpinstall"
	@echo "                    Install the board's sfp rts in gcc"
	@echo "  make <board>.zfpinstall"
	@echo "                    Install the board's zfp rts in gcc"

obj/rts-sources:
	$(GEN_RTS) \
	  --output-descriptor=obj/rts.json \
	  --output-sources=$@ \
	  --gnat-dir=$(GNAT_SOURCES) --gcc-dir=$(GCC_SOURCES) \
	  --rts-profile=ravenscar-full

srcs: $(RTS_SRCS)
	$(BUILD_RTS) --force \
	  --output=obj $(TARGETS)

all: $(RTS_SRCS)
	$(BUILD_RTS) --force --build \
	  --output=obj $(TARGETS)

install: $(RTS_SRCS)
	$(BUILD_RTS) --force --build $(TARGETS)

%.build: obj/$(TGT)
	for f in obj/$(TGT)/BSPs/*_$*.gpr; do \
	  $(GPRBUILD) -P $$f; \
	done

%.fullbuild: obj/$(TGT)
	if [ ! -f obj/$(TGT)/BSPs/ravenscar_full_$*.gpr ]; then \
	  echo "no ravenscar-full runtime for $*"; \
	  exit 1; \
	fi
	$(GPRBUILD) -P obj/$(TGT)/BSPs/ravenscar_full_$*.gpr

%.sfpbuild: obj/$(TGT)
	if [ ! -f obj/$(TGT)/BSPs/ravenscar_sfp_$*.gpr ]; then \
	  echo "no ravenscar-sfp runtime for $*"; \
	  exit 1; \
	fi
	$(GPRBUILD) -P obj/$(TGT)/BSPs/ravenscar_sfp_$*.gpr

%.zfpbuild: obj/$(TGT)
	if [ ! -f obj/$(TGT)/BSPs/zfp_$*.gpr ]; then \
	  echo "no ravenscar-sfp runtime for $*"; \
	  exit 1; \
	fi
	$(GPRBUILD) -P obj/$(TGT)/BSPs/zfp_$*.gpr

%.install: %.build
	for f in obj/$(TGT)/BSPs/*_$*.gpr; do \
	  $(GPRINSTALL) --uninstall -P $$f; \
	  $(GPRINSTALL) -p -f -P $$f; \
	done

%.fullinstall: %.fullbuild
	$(GPRINSTALL) -P obj/$(TGT)/BSPs/ravenscar_full_$*.gpr

%.sfpinstall: %.sfpbuild
	$(GPRINSTALL) -P obj/$(TGT)/BSPs/ravenscar_sfp_$*.gpr

%.zfpinstall: %.zfpbuild
	$(GPRINSTALL) -P obj/$(TGT)/BSPs/zfp_$*.gpr
