# Conclusion and refinements

![GNAT on the three boards](images/stm32-all.jpg)

As a conclusion, don't hesitate to navigate in bb-runtimes and compare
files to understand how specificities of boards are handled by the
BSPs.

To go further in customized run-time, you can refer to the following
documentation: [Customized
run-time](https://docs.adacore.com/gnat_ugx-docs/html/gnat_ugx/gnat_ugx/customized_run-time_topics.html).

Also, having a runtime is not enough to achieve a complete application
on board: you'll also need to control the various peripherals that's
in there. You can checkout our [Ada Drivers
Library](https://github.com/AdaCore/Ada_Drivers_Library) to see if the
peripheral support is already there, or at least get inspiration on
how to write your own driver.

[Home](README.md)
