"""Generate deterministic large inputs for compact-processing E2E scenarios."""

import csv
import json
import sys
from pathlib import Path


def write_csv(path, header, rows):
    with path.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.writer(stream, lineterminator="\n")
        writer.writerow(header)
        writer.writerows(rows)


def generate(root):
    data = root / "data"
    data.mkdir(parents=True, exist_ok=True)
    regions = ("north", "south", "east", "west")
    write_csv(
        data / "sales.csv",
        ("tx_id", "account", "region", "amount", "status"),
        (
            (
                f"T{i:05d}",
                f"acct-{i % 317:03d}",
                regions[i % 4],
                (i * 43) % 199 - 87,
                "refunded" if i % 11 == 0 else "paid",
            )
            for i in range(12000)
        ),
    )
    write_csv(
        data / "accounts.csv",
        ("account_id", "tier", "region"),
        (
            (f"acct-{i:04d}", "gold" if i % 3 == 0 else "standard", regions[i % 4])
            for i in range(1800)
        ),
    )
    write_csv(
        data / "events.csv",
        ("event_id", "account_id", "kind", "latency"),
        (
            (
                f"E{i:05d}",
                f"acct-{(i * 37 + 11) % 1800:04d}",
                "failed" if i % 7 == 0 or i % 31 == 0 else "ok",
                (i * 83 + 5) % 1709,
            )
            for i in range(12000)
        ),
    )
    with (data / "audit.jsonl").open("w", encoding="utf-8") as stream:
        for i in range(5400):
            if i >= 2600 and i % 233 == 0:
                stream.write('{"record":\n')
            else:
                row = {
                    "record": f"rec-{i % 413:03d}",
                    "status": "error" if i % 13 == 0 else "ok",
                    "source": ("api", "queue", "store")[i % 3],
                    "message": f"batch-{i // 19:04d}: request {i:05d} 🙂",
                }
                stream.write(json.dumps(row, ensure_ascii=False) + "\n")
    write_csv(
        data / "reconcile-left.csv",
        ("invoice", "part", "amount"),
        (
            (f"INV{invoice:05d}", part, (invoice * 17 + part * 23) % 200 - 17)
            for invoice in range(2400)
            for part in range(3)
        ),
    )
    write_csv(
        data / "reconcile-right.csv",
        ("invoice", "part", "amount"),
        (
            (
                f"INV{invoice:05d}",
                part,
                (invoice * 17 + part * 23) % 200
                - 17
                + (7 if invoice % 17 == 0 and part == 1 else 0),
            )
            for invoice in range(150, 2550)
            for part in range(3)
        ),
    )


if __name__ == "__main__":
    generate(Path(sys.argv[1]))
