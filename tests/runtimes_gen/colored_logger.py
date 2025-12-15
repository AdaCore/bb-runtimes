#
# Copyright (C) 2025-2026, AdaCore
#

"""
Colored logging formatter with background colors for better readability.
"""

import logging
from typing import override


class ColoredFormatter(logging.Formatter):
    """Custom formatter with background colors for different log levels."""

    # ANSI color codes
    RESET = "\033[0m"

    # Background colors
    DEBUG_BG = "\033[44m"  # Blue background
    INFO_BG = "\033[42m"  # Green background
    WARNING_BG = "\033[43m"  # Yellow background
    ERROR_BG = "\033[41m"  # Red background
    CRITICAL_BG = "\033[45m"  # Magenta background

    # Text colors (white text for readability on colored backgrounds)
    WHITE_TEXT = "\033[97m"

    FORMATS = {
        logging.DEBUG: DEBUG_BG
        + WHITE_TEXT
        + "%(levelname)-8s"
        + RESET
        + " - %(message)s",
        logging.INFO: INFO_BG
        + WHITE_TEXT
        + "%(levelname)-8s"
        + RESET
        + " - %(message)s",
        logging.WARNING: WARNING_BG
        + WHITE_TEXT
        + "%(levelname)-8s"
        + RESET
        + " - %(message)s",
        logging.ERROR: ERROR_BG
        + WHITE_TEXT
        + "%(levelname)-8s"
        + RESET
        + " - %(message)s",
        logging.CRITICAL: CRITICAL_BG
        + WHITE_TEXT
        + "%(levelname)-8s"
        + RESET
        + " - %(message)s",
    }

    @override
    def format(self, record: logging.LogRecord) -> str:  # noqa: A003
        """Format the log record with appropriate background color."""
        log_fmt = self.FORMATS.get(record.levelno)
        formatter = logging.Formatter(log_fmt)
        return formatter.format(record)


def setup_colored_logging(
    logger_name: str | None = None, level: int = logging.DEBUG
) -> logging.Logger:
    """
    Setup logging with colored formatter.

    Args:
        logger_name: Name of the logger. If None, configures root logger.
        level: Logging level (default: DEBUG)

    Returns:
        Configured logger instance
    """
    handler = logging.StreamHandler()
    handler.setFormatter(ColoredFormatter())

    if logger_name:
        logger = logging.getLogger(logger_name)
        logger.setLevel(level)
        logger.addHandler(handler)
        logger.propagate = False
    else:
        logging.basicConfig(level=level, handlers=[handler])
        logger = logging.getLogger()

    return logger
