"""Run one of two local commands for the ShellTask trial."""

import sys
import time

if sys.argv[1:] == ["hold"]:
    print("hold-started", flush=True)
    time.sleep(45)
    print("hold-finished")
elif sys.argv[1:] == ["finish"]:
    print("finish-ok")
else:
    sys.exit("Use hold or finish.")
