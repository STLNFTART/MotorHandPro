"""
NASA and Starlink/SpaceX API Integration Suite

This module provides comprehensive API clients for NASA and SpaceX/Starlink services,
integrated with the MotorHandPro Primal Logic framework.

Available APIs:
    NASA:
        - APOD (Astronomy Picture of the Day)
        - NeoWs (Near Earth Object Web Service)
        - EPIC (Earth Polychromatic Imaging Camera)
        - POWER (Weather and Climate Data)
        - Image Library (NASA Media Library)
        - SSD (Solar System Dynamics)

    SpaceX/Starlink:
        - SpaceX API (Launch, Rocket, Starlink satellite data)
        - Starlink Metrics (Public network performance data)

Author: MotorHandPro Team
License: See NOTICE file
"""

__version__ = "1.0.0"
__author__ = "MotorHandPro Team"

from .manager import APIManager

__all__ = ["APIManager"]
