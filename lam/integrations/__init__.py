"""LAM Integrations with existing MotorHandPro systems"""

# NOTE: Temporarily commented out due to merge conflicts in gotrax_hoverboard_integration.py
# from .gotrax_hoverboard_integration import (
#     GoTraxHoverboardController,
#     HederaSmartContractInterface,
#     LAMHoverboardInterface,
#     HoverboardMotorSpec,
#     TokenBurnConfig,
#     ActuationRequest,
#     ActuationResult,
#     ActuationMode,
#     HoverboardMotorType
# )

from .hedera_config import (
    load_hedera_config,
    validate_hedera_config,
    print_hedera_config_status
)

# Prosthetics integration
from .prosthetics_integration import (
    ProstheticsController,
    EMGSignal,
    GestureAction
)

# Radiation testing
from .radiation_testing import (
    RadiationSimulator,
    RadiationProfile,
    RadiationEvent,
    EffectivenessMetrics,
    RadiationSource,
    SpaceEnvironment
)
