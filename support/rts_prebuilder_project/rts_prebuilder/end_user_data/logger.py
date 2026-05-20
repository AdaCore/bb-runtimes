#
# Copyright (C) 2025-2026, AdaCore
#

"""Logging infrastructure"""

import logging
from typing import List

_log_level: int = logging.INFO
_registered_loggers: List[logging.Logger] = []

COLORS = {
    "DEBUG": "\033[90m",  # dark gray
    "INFO": "\033[36m",  # cyan
    "WARNING": "\033[33m",  # yellow
    "ERROR": "\033[31m",  # red
    "CRITICAL": "\033[1;41m",  # white on red bg
}
RESET = "\033[0m"

# Pad level names first, then apply colors
for lvl, color in COLORS.items():
    logging.addLevelName(getattr(logging, lvl), f"{color}{lvl.ljust(8)}{RESET}")

# Create a handler with custom formatter that truncates module name to last 30 chars
handler = logging.StreamHandler()
handler.setFormatter(logging.Formatter("%(levelname)s | %(name)s | %(message)s"))


def _trim_record_name(record: logging.LogRecord, length: int = 30) -> bool:
    """Ensure record.name fits in 30 chars, right-aligned.
    Used as a filter for the logging handler. That's why it returns True.
    """
    record.name = record.name[-length:].rjust(length)
    return True


handler.addFilter(_trim_record_name)

logging.basicConfig(level=logging.DEBUG, handlers=[handler])


def set_log_level(log_level: int) -> None:
    """Set the log level and apply it to all registered loggers"""
    global _log_level
    _log_level = log_level

    # Apply the new log level to all registered loggers
    for logger in _registered_loggers:
        logger.setLevel(log_level)


def get_logger(module_name: str) -> logging.Logger:
    """Get or create a logger for the given module name and register it"""
    logger = logging.getLogger(module_name)
    logger.setLevel(_log_level)

    # Register the logger if it's not already in the list
    if logger not in _registered_loggers:
        _registered_loggers.append(logger)

    return logger


__all__ = ["set_log_level", "get_logger"]
