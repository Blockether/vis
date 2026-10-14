"""Deterministic oversized stdout cases for the large-output E2E battery."""

import json
import sys

mode = globals().get("case") or sys.argv[1]

if mode == "lookup":
    rows = [
        f"ticket:{i:05d}|team=team{i % 7}|state={'hold' if i % 83 == 0 else 'open'}|checksum={(i * i * 37 + 17) % 10007:04d}"
        for i in range(6200)
    ]
    print("LOOKUP-HEAD\n" + "\n".join(rows) + "\nLOOKUP-TAIL")
elif mode == "ledger":
    accounts = ("alpha", "beta", "gamma", "delta")
    rows = [
        f"row:{i:05d}|acct={accounts[i % 4]}|amount={(i * 17) % 113 - 47}|status={'reversed' if i % 13 == 0 else 'settled'}"
        for i in range(6200)
    ]
    print("LEDGER-HEAD\n" + "\n".join(rows) + "\nLEDGER-TAIL")
elif mode == "jsonl":
    rows = [
        {
            "id": f"rid:{i:05d}",
            "region": ("west", "east", "north")[i % 3],
            "priority": ("low", "high")[i % 2],
            "latency": (i * i * 41 + 19 * i + 7) % 997,
            "tag": f"batch-{i // 13:04d}",
        }
        for i in range(4400)
    ]
    print(
        "JSONL-HEAD\n"
        + "\n".join(json.dumps(row, separators=(",", ":")) for row in rows)
        + "\nJSONL-TAIL"
    )
elif mode == "unicode":
    labels = ("漢字", "étoile", "🙂", "دليل")
    rows = [
        f"記録{i:05d}|label={labels[i % 4]}|metric={(i * 97 + 5) % 1231:04d}"
        for i in range(5400)
    ]
    print("UNICODE-HEAD\n" + "\n".join(rows) + "\nUNICODE-TAIL")
elif mode == "alpha":
    rows = [
        f"A{i:05d}|key=k{i % 331:03d}|value={(i * i + 37 * i + 11) % 1009:04d}"
        for i in range(5400)
    ]
    print("ALPHA-HEAD\n" + "\n".join(rows) + "\nALPHA-TAIL")
elif mode == "beta":
    rows = [
        f"B{i:05d}|key=k{i % 331:03d}|weight={(i * 71 + 3) % 809:04d}"
        for i in range(5300)
    ]
    print("BETA-HEAD\n" + "\n".join(rows) + "\nBETA-TAIL")
elif mode == "incident":
    components = ("api", "queue", "store")
    rows = [
        f"evt:{i:05d}|req=r{i % 173:03d}|component={components[i % 3]}|status={'retry' if i % 17 == 0 else 'ok'}|attempt={i % 5 + 1}"
        for i in range(6500)
    ]
    rows[1150] = "evt:01150|req=critical-72|component=api|status=start|attempt=1"
    rows[3150] = "evt:03150|req=critical-72|component=store|status=error|attempt=1"
    rows[4001] = "evt:04001|req=critical-72|component=store|status=error|attempt=2"
    rows[5602] = "evt:05602|req=critical-72|component=queue|status=delivered|attempt=3"
    print("INCIDENT-HEAD\n" + "\n".join(rows) + "\nINCIDENT-TAIL")
else:
    raise ValueError(f"unknown output case: {mode}")
