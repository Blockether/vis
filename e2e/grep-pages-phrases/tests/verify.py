import json


def verify(evidence):
    actual = json.loads((evidence.workspace / "answer.json").read_text())
    expected = {
        "ids": list(range(137)),
        "files": [f"part_{i:02}.txt" for i in range(7)],
        "phrases": ["alpha beta", "gamma delta"],
    }
    failures = []
    if actual != expected:
        failures.append("The IDs, files or phrase lines differ from the fixture.")
    for i in range(7):
        expected_lines = "".join(
            f"left {n}\nPAYMENT_READY id={n:03}\nright {n}\n"
            for n in range(i * 20, min((i + 1) * 20, 137))
        )
        if (evidence.workspace / f"part_{i:02}.txt").read_text() != expected_lines:
            failures.append(f"part_{i:02}.txt changed")
    if (evidence.workspace / "phrases.txt").read_text() != (
        "alpha beta\ngamma delta\nalpha gamma\nbeta delta\n"
    ):
        failures.append("phrases.txt changed")
    return failures
