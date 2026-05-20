#
# Copyright (C) 2025-2026, AdaCore
#


class TemplateConfigListerMixin:
    """Mixin class that provides functionality to manage template configuration
    for source files that require templating.
    """

    _template_config: dict[str, str]
    """
    Holds all key/value mapping to be applied on the template for the given
    class.
    """

    def __init__(self) -> None:
        self._template_config = {}

    def add_template_config_value(self, key: str, value: str) -> None:
        """
        Adds key/value that will be used to instantiate templated source

        :param key: Key that will be replaced in the template. Example:
                    "STM32_HSE_Clock_Frequency"
        :param value: Value that will override the key in the template file.
                    Example: "8_000_000"
        :raises TypeError: If key or value are not strings
        :raises ValueError: If key is already present in _template_config
        """
        """
        Adds template configuration to all sources registered in this composer

        :param template_config: The template configuration to add
        """
        if not (isinstance(key, str) and isinstance(value, str)):
            raise TypeError("template key and value must be strings")

        if key in self._template_config:
            raise ValueError(f"config key already defined {key}")

        self._template_config.update({key: value})

    @property
    def template_config(self) -> dict[str, str]:
        """
        Returns the template configuration dictionary

        :return: The template configuration dictionary
        """
        return self._template_config.copy()
