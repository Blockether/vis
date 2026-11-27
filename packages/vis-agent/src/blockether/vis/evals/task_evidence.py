"""Read the host evidence of one Harbor trial, not guest output."""

import json
import sqlite3
import uuid
from contextlib import closing
from pathlib import Path

FILE = "task-evidence.json"


def capture(session_id, home, database):
    """Collect Task transitions and Council questions and replies for this session."""
    try:
        if str(uuid.UUID(session_id)) != session_id:
            return {}
    except (ValueError, TypeError, AttributeError):
        return {}

    events = []
    path = home / "gateway" / "events" / f"{session_id}.ndjson"
    if path.is_file():
        with path.open(encoding="utf-8") as stream:
            for line in stream:
                try:
                    event = json.loads(line)
                except ValueError:
                    continue
                if (
                    isinstance(event, dict)
                    and event.get("session_id") == session_id
                    and event.get("type")
                    in {"task.started", "task.updated", "task.ended"}
                    and isinstance(event.get("task"), dict)
                    and event["task"].get("session_id") == session_id
                ):
                    events.append({"type": event["type"], "task": event["task"]})
    questions = []
    replies = []
    children = {}
    shells = []
    if database.is_file():
        with closing(sqlite3.connect(database.as_uri() + "?mode=ro", uri=True)) as db:
            db.row_factory = sqlite3.Row
            for row in db.execute(
                """SELECT e.author_sid AS session_id, e.id AS entry_id, e.content, p.state,
                          r.id AS reply_entry_id, r.content AS reply_content,
                          r.created_at AS reply_created_at
                   FROM council_entry e JOIN council_ping p ON p.entry_id = e.id
                   LEFT JOIN council_entry r ON r.id = p.reply_entry_id
                     AND r.author_sid = p.recipient_sid AND r.reply_to = e.id
                   WHERE p.recipient_sid = ? AND e.reply_required = 1
                   ORDER BY e.id""",
                (session_id,),
            ):
                question = dict(row)
                reply_id = question.pop("reply_entry_id")
                reply_content = question.pop("reply_content")
                reply_created_at = question.pop("reply_created_at")
                questions.append(question)
                if reply_id is not None:
                    replies.append(
                        {
                            "entry_id": reply_id,
                            "reply_to": row["entry_id"],
                            "content": reply_content,
                            "created_at_ms": reply_created_at,
                        }
                    )
            children = {
                f"a{row['ordinal']}": row["session_id"]
                for row in db.execute(
                    "SELECT ordinal, session_id FROM session_agent WHERE parent_id = ?",
                    (session_id,),
                )
            }
            for row in db.execute(
                """SELECT index_data FROM extension_aggregate
                   WHERE extension_id = 'foundation-core' AND kind = ':shell-log'
                     AND session_soul_id = ?""",
                (session_id,),
            ):
                data = json.loads(row["index_data"])
                path = Path(data["log_path"]).resolve()
                base = (home / "logs").resolve()
                if (
                    not path.is_relative_to(base)
                    or path.parent.name != session_id
                    or path.parent.parent.name != "shell"
                ):
                    continue
                try:
                    with path.open(encoding="utf-8", errors="replace") as stream:
                        output = stream.read(32_768)
                except OSError:
                    continue
                shells.append(
                    {
                        "command": data["command"],
                        "exit": data.get("exit"),
                        "output": output,
                    }
                )
    return {
        "events": events,
        "questions": questions,
        "replies": replies,
        "children": children,
        "shells": shells,
    }


def tasks(evidence, kind):
    """Pair the start and terminal rows of each Task in its start order."""
    starts = {}
    ends = {}
    for event in evidence.get("events", ()):
        row = event["task"]
        if row.get("kind") != kind:
            continue
        if event["type"] == "task.started" and row.get("state") == "running":
            starts[row["id"]] = row
        if event["type"] == "task.ended":
            ends[row["id"]] = row
    return [(start, ends.get(task_id, {})) for task_id, start in starts.items()]
