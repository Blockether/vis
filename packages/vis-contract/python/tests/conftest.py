"""Make the package importable from the checkout."""

import os
import sys
from pathlib import Path

if not os.environ.get("VIS_TEST_INSTALLED"):
    sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "src"))
