#!/usr/bin/env bash
set -euo pipefail
./verify.sh --quick 2>&1 | tail -40
