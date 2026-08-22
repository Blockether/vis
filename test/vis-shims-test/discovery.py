# Synthetic shim used by shim-attach-test's discovery contract.
import sys
import types

actual_module = types.ModuleType("actual_module")
actual_module.__doc__ = "Synthetic discovery contract."
actual_module.answer = 42
sys.modules["actual_module"] = actual_module


def actual_global():
    """Synthetic discovery contract, as a global."""
    return 42
