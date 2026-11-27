"""Vis sessions and the memory page read the same database."""

from __future__ import annotations

import json
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from threading import Thread

import pytest
from vis_optmem.remote import RemoteStore

from factory.memory import Memories


@pytest.fixture
def memories(tmp_path):
    """Keep both memories in one SQLite database."""
    return Memories(f"sqlite:///{tmp_path / 'factory.db'}")


def test_remote_memory_round_trip(memories):
    """Write, read and refuse a repeated note over the RemoteStore protocol."""

    class Handler(BaseHTTPRequestHandler):
        """Serve the memory protocol over HTTP for this test."""

        def do_POST(self):
            """Send the request to Memories and return its status and body."""
            assert self.path == "/factory/api/memory"
            status, data = memories.answer(self.rfile.read(int(self.headers["Content-Length"])))
            body = json.dumps(data).encode()
            self.send_response(status)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)

    with ThreadingHTTPServer(("127.0.0.1", 0), Handler) as server:
        thread = Thread(target=server.serve_forever)
        thread.start()
        try:
            url = f"http://127.0.0.1:{server.server_port}/factory/api/memory"
            team = RemoteStore(url)
            assert team.exists()
            note = "Use SQLite for the factory on one host."
            entry, is_new = team.note("2026-11-27", note)
            assert is_new
            assert team.entries(0, 1) == [entry]
            assert team.note("2026-11-27", note) == (entry, False)
            assert team.count() == 1
            assert RemoteStore(url, memory="personal").count() == 0
            with pytest.raises(LookupError):
                RemoteStore(url, memory="other").count()
            assert memories.entries("team")["entries"] == [{"id": 0, "date": "2026-11-27", "text": note}]
        finally:
            server.shutdown()
            thread.join()


def test_memory_page_search_and_pagination(memories):
    """Find the newest matches and continue just before the last returned ID."""
    notes = ["Check SQLite WAL mode.", "Keep a PostgreSQL backup.", "Use SQLite on the first host.", "Check SQLite after a restart."]
    store = memories.store("team")
    store.append([("2026-11-27", note) for note in notes])
    memories.store("personal").note("2026-11-27", "Keep separate notes for each memory.")
    assert {row["name"]: row["count"] for row in memories.listing()["memories"]} == {"personal": 1, "team": 4}
    page = memories.entries("team", query="sqlite", pattern="^#[0-9] .* CHECK", limit=1)
    assert page == {"name": "team", "count": 4, "entries": [{"id": 3, "date": "2026-11-27", "text": notes[3]}]}
    older = memories.entries("team", query="sqlite", pattern="check", before="3")
    assert [entry["id"] for entry in older["entries"]] == [0]
    assert [entry["id"] for entry in memories.entries("team", limit=2)["entries"]] == [3, 2]
    assert memories.entries("team", before=0)["entries"] == []
    with pytest.raises(ValueError, match="regular expression"):
        memories.entries("team", pattern="[")
