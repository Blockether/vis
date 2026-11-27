"""Find the schedules and events that start workflow runs."""

from __future__ import annotations

import re
import threading
import time
from dataclasses import dataclass
from datetime import UTC, datetime
from typing import Any

from vis_optmem.shared import Absent, Equals, Table

from factory import settings
from factory.records import Document, Records, Refused, pack, unpack
from factory.runs import Runs
from factory.workflows import source

EVENT = re.compile(r"[a-z][a-z0-9_]*(?:\.[a-z][a-z0-9_]*)+")
SCHEDULE = "cursor/cron"
EVENTS = "cursor/events"


def field(text: str, low: int, high: int) -> frozenset[int]:
    """Read one cron field as a set of numbers."""
    values: set[int] = set()
    for part in text.split(","):
        matched = re.fullmatch(r"(\*|[0-9]{1,2}(?:-[0-9]{1,2})?)(?:/([0-9]{1,2}))?", part)
        if matched is None:
            raise Refused(400, "a cron field is not valid")
        span, step = matched.groups()
        stride = int(step or 1)
        if span == "*":
            start, end = low, high
        else:
            ends = [int(value) for value in span.split("-")]
            start, end = ends[0], ends[-1]
        if not low <= start <= end <= high or not 1 <= stride <= high - low + 1:
            raise Refused(400, "a cron field is outside its range")
        values.update(range(start, end + 1, stride))
    return frozenset(values)


@dataclass(frozen=True)
class Cron:
    """The match rule of a five-field UTC cron expression."""

    minute: frozenset[int]
    hour: frozenset[int]
    day: frozenset[int]
    month: frozenset[int]
    weekday: frozenset[int]
    day_any: bool
    weekday_any: bool

    @classmethod
    def parse(cls, expression: Any) -> Cron:
        """Read a cron expression, or refuse it."""
        parts = expression.split() if isinstance(expression, str) else []
        if len(parts) != 5:
            raise Refused(400, "cron needs five fields")
        minute, hour, day, month, weekday = parts
        return cls(field(minute, 0, 59), field(hour, 0, 23), field(day, 1, 31), field(month, 1, 12), frozenset(n % 7 for n in field(weekday, 0, 7)), day.startswith("*"), weekday.startswith("*"))

    def matches(self, moment: datetime) -> bool:
        """Tell if the expression matches a minute in UTC."""
        moment = moment.astimezone(UTC) if moment.tzinfo else moment
        return moment.month in self.month and moment.hour in self.hour and moment.minute in self.minute and self.day_matches(moment)

    def day_matches(self, moment: datetime) -> bool:
        """Match the date. If both day fields are restricted, either day can match."""
        day, weekday = moment.day in self.day, (moment.weekday() + 1) % 7 in self.weekday
        return (day and weekday) if self.day_any or self.weekday_any else (day or weekday)

    def next(self, after: int) -> int | None:
        """Return the next minute after the given minute, or None if no date matches."""
        hours = sorted(hour * 60 + minute for hour in self.hour for minute in self.minute)
        day, minute = divmod(after, 1440)
        # This includes a leap day even across a non-leap century year.
        for offset in range(366 * 8 + 1):
            date = datetime.fromtimestamp((day + offset) * 86400, UTC)
            if date.month not in self.month or not self.day_matches(date):
                continue
            for candidate in hours:
                if offset or candidate > minute:
                    return (day + offset) * 1440 + candidate
        return None


class Triggers:
    """Process the triggers in workflow files and the events in the table."""

    def __init__(self, table: Table, runs: Runs) -> None:
        """Use the table of the run manager, so a mark and a run can share a write."""
        self.records, self.table, self.runs = Records(table), table, runs
        self.lock = threading.Lock()

    def emit(self, kind: str, data: Document) -> str:
        """Keep an event and return its id."""
        while True:
            event_id = self.records.emit(kind, data)
            if event_id is not None:
                return event_id

    def workflows(self) -> list[tuple[Document, str]]:
        """Read each valid source and its trigger list from the same file version."""
        found = []
        for row in source.listing():
            if not row["triggers"]:
                continue
            try:
                text = source.read(row["name"])
                found.append((source.describe(row["name"], text), text))
            except Refused:
                continue  # the file changed while this tick read it
        return found

    def listing(self, now: float | None = None) -> list[Document]:
        """List triggers with the newest run and their next cron minute."""
        minute = int((time.time() if now is None else now) // 60)
        found = []
        for about, _ in self.workflows():
            for trigger in about["triggers"]:
                history = self.records.listing(f"fired/{trigger['id']}/")
                last = max(history, key=lambda row: row["at"], default={})
                due = Cron.parse(trigger["cron"]).next(minute) if trigger["kind"] == "cron" else None
                found.append(
                    {**trigger, "workflow": about["name"], "title": about["title"], "last_fired": last.get("at"), "last_run": last.get("run"), "next_due": due * 60 if due is not None else None}
                )
        return found

    def fire(self, about: Document, text: str, trigger: Document, slot: str, data: Document) -> None:
        """Start one run for this slot, unless another tick did so first."""
        mark = f"fired/{trigger['id']}/{slot}"
        if self.table.get(mark) is not None:
            return
        names = {item["name"] for item in about["inputs"]}
        given = {key: value for key, value in data.items() if key in names}
        given.update(trigger["input"])
        self.runs.create(about["name"], text, about, given, trigger={"id": trigger["id"], "kind": trigger["kind"], "slot": slot}, mark=mark)

    def advance(self, key: str, position: str | int) -> None:
        """Advance a cursor without moving it back after concurrent ticks."""
        while True:
            item = self.table.get(key)
            if item and unpack(item)["position"] >= position:
                return
            version = int(item["version"]) if item else 0
            condition = Equals(key, "version", version) if item else Absent(key)
            if self.records.write({key: pack({"position": position}, version + 1)}, [condition]):
                return

    def idle(self, sessions: list[Document], now: float) -> None:
        """Make one event for each idle session with new turns."""
        for session in sessions:
            count = session["turns"]
            if not count or session.get("busy") or session.get("run") or session.get("group") in (settings.TALKS_GROUP, settings.RUNS_GROUP) or now - session["updated"] < settings.IDLE_S:
                continue
            key = f"seen/{session['id']}"
            while True:
                item = self.table.get(key)
                if item and unpack(item)["turns"] >= count:
                    break
                version = int(item["version"]) if item else 0
                condition = Equals(key, "version", version) if item else Absent(key)
                data = {"session": session["id"], "title": session["title"], "turns": count}
                if self.records.write({key: pack({"turns": count, "at": now}, version + 1)}, [condition], ("session.idle", data), at=now):
                    break

    def tick(self, sessions: list[Document], now: float | None = None) -> None:
        """Fire due cron and event triggers, then update their cursors."""
        now = time.time() if now is None else now
        minute = int(now // 60)
        with self.lock:
            workflows = self.workflows()
            self.idle(sessions, now)
            last = self.table.get(SCHEDULE)
            start = min(unpack(last)["position"] + 1, minute) if last else minute
            for about, text in workflows:
                for trigger in about["triggers"]:
                    if trigger["kind"] != "cron":
                        continue
                    cron = Cron.parse(trigger["cron"])
                    slot = cron.next(start - 1)
                    while slot is not None and slot <= minute:
                        self.fire(about, text, trigger, str(slot), {})
                        slot = cron.next(slot)
            self.advance(SCHEDULE, minute)
            last = self.table.get(EVENTS)
            cursor = unpack(last)["position"] if last else ""
            for key, item in self.table.query("event/", start=cursor):
                if key == cursor:
                    continue
                event = unpack(item)
                for about, text in workflows:
                    for trigger in about["triggers"]:
                        if trigger["kind"] == "on" and trigger["event"] == event["kind"] and not own_run(about["name"], event):
                            self.fire(about, text, trigger, event["id"], event["data"])
                self.advance(EVENTS, key)


def own_run(workflow: str, event: Document) -> bool:
    """Prevent a workflow from starting another run on its own completion or failure."""
    return event["kind"] in ("run.completed", "run.failed") and event["data"].get("workflow") == workflow
