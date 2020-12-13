# bb-runtimes

This repository is used to generate the runtime source trees for AdaCore's
bare metal targets.

## dependencies

This repository depends on both gcc and gnat repositories to generate a full
runtime. However it can also use the sources from an installed compiler and
rely on those to just generate the BSP part of the runtime.

You need GNAT Pro 21.0w 20200327 or GNAT Community 2020 minimum to use
this repository to generate runtimes.

## generation of runtimes

```
./build_rts.py --output=temp --build <board1> <board2> ...
```

The list of supported boards is listed in build_rts.py within build_configs.

The above call with generate the runtimes for <board1> <board2> in 'temp'
and will build them, assuming the proper compiler is in the PATH.

To install the runtime in the compiler's default location, you will thus
need to specify

```
./build_rts.py --output <gnat_prefix>/<target>/lib/gnat` ...
```

So for example --output /opt/gnat/arm-eabi/lib/gnat

## rebuild of a runtime

To build a runtime with non default options, use the project file present in
the runtime folder: runtime_build.gpr for zfp runtimes and ravenscar_build.gpr
for ravenscar runtimes.

To build with debug options, use -XBUILD=Debug ; to build with assertions use
-XBUILD=Assert.

So for example to rebuild the ravenscar-sfp-stm32f4 runtime with debug
information, assuming GNAT is installed in ~/install/gnat, run the following:

```
gprbuild -P ~/install/gnat/arm-eabi/lib/gnat/ravenscar-sfp-stm32f4/ravenscar-build.gpr -XBUILD=Debug
```

## customizing runtimes

Some targets are configurable and allow certain parts of the runtime to be customized during generation.
For example, with a custom system clock configuration.

The configuration for such runtimes are defined in a JSON file in the following format:

```json
{
    "base_target": "<base target name>",
    "board_name": "<my board name>",
    "board_params": {
        "param1": 123,
        "param2": "example",
        "param3": True
    }
}
```

* `base_target` is the name of the target that will be customized. This can be any runtime
supported by bb-runtimes, but note that not all targets can be customized. Targets that are
not customizable will ignore the contents of `board_params`.

* `board_name` sets the name of the generated runtime.

* `board_params` sets the board parameters to use when generating the runtime.
The set of available board parameters depends on the chosen `base_target`. See the following
sub-sections of this README for a list of available parameters for each target.

All board parameters are optional. Board parameters that are not defined will use their default value.
Unrecognized board parameters are ignored.

To generate a runtime with the custom configuration, pass the name of the JSON file to build_rts.py:

```
./build_rts.py --output=temp --build my-board.json ...
```

**Note:** The file name *must* end in `.json`

The available board parameters for each customizable `base_target` are described in the following sub-sections:

### stm32f0xx board parameters

These are the board parameters for stm32f0xx `base_target`s:

**Note:** The `base_target` is the full STM32 device name, e.g. "stm32f072rb", or "stm32f030r8".

| Parameter          | Type  | Default Value | Description                                      |
|:------------------:|:-----:|:-------------:|:-------------------------------------------------|
| `sysclk_frequency` | `int` | 48000000      | Configures the target SYSCLK (and CPU) frequency (in Hz). |
| `hse_frequency`    | `int` | 8000000       | Specifies the frequency (in Hz) of the board's HSE oscillator. Valid range is 4 MHz - 32 MHz. |
| `clock_source`     | `str` | "HSE"         | Configures the source oscillator used to generate SYSCLK. Valid values are "HSE", "HSI", and "HSI48". |
| `hse_bypass`       | `bool` | False        | Enables or disables the HSE bypass (see the STM32F0xx Reference Manual). |
| `lsi_enabled`      | `bool` | True         | Enables or disables the LSI oscillator at startup. |
| `apb_prescaler`    | `int`  | 1            | Configures the APB prescaler. Valid values are: 1, 2, 4, 8, 16. |

Here is an example configuration to generate a runtime for the Nucleo-F072RB board from ST (no external HSE oscillator fitted):

```json
{
    "base_target": "stm32f072rb",
    "board_name": "nucleo-f072rb",
    "board_params": {
        "sysclk_frequency": 48000000,
        "clock_source": "HSI"
    }
}
```

### nRF52 board parameters

These are the board parameters for the nrf52832 and nrf52840 `base_target`s:

| Parameter   | Type   | Default Value | Description                                      |
|:-----------:|:------:|:-------------:|:-------------------------------------------------|
| `use_hfxo`  | `bool` | False         | Selects whether the high-frequency external oscillator (HFXO) is enabled at startup. When False the internal oscillator is used by default. |
| `lfclk_src` | `str`  | "Xtal"        | Selects the clock source used for the low-frequency (32 kHz) clock that drives the RTC peripherals. Valid values are: "Xtal", "Rc", and "Synth" |
| `use_swo_trace` | `bool` | True      | Set to True to configure the SWO trace pins to enable debugging. When False,the SWO pins are not configured. |
| `use_reset_pin` | `bool` | True      | Set to True to configure pin P0.18 as the reset pin. When False, the reset pin is not configured. |
| `alarm_rtc_periph` | `str` | "RTC0"  | Selects which RTC peripheral is used to implement the clock for Ravenscar delays (Ada.Real_Time). Valid values are: "RTC0", "RTC1", "RTC2" |

Here is an example configuration to generate a runtime that enables the HFXO at startup, and uses RTC1 for Ravenscar delays:

```json
{
    "base_target": "nrf52840",
    "board_name": "my-board",
    "board_params": {
        "use_hfxo": True,
        "alarm_rtc_periph": "RTC1"
    }
}
```