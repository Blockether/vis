"""Wait for an exact line on the input terminal."""

import select
import sys

print("ready", flush=True)
ready, _, _ = select.select([sys.stdin], [], [], 180)
if not ready or sys.stdin.readline() != "release\n":
    sys.exit("No release line received.")
print("fold-result-7351")
