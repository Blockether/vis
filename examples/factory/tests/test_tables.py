"""The same table operations on each database."""

from __future__ import annotations

import io
import json
import multiprocessing
import os
import sqlite3
from concurrent.futures import ThreadPoolExecutor
from contextlib import closing, suppress
from threading import Barrier
from uuid import uuid4

import pytest
from vis_optmem.shared import Absent, Equals, MemoryTable

from factory import settings, tables
from factory.tables import open_table


class FakeD1:
    """Run each HTTP batch as one transaction in SQLite."""

    def __init__(self, path):
        """Keep a database file and a one-time hook before a batch."""
        self.path = path
        self.before_batch = None

    def __call__(self, request, *, timeout):
        """Read the REST request and return D1 results for each statement."""
        assert request.full_url == "https://api.cloudflare.com/client/v4/accounts/test-account/d1/database/test-db/query"
        assert request.get_method() == "POST"
        batch = json.loads(request.data)["batch"]
        if self.before_batch is not None and len(batch) > 1:
            hook, self.before_batch = self.before_batch, None
            hook()
        with closing(sqlite3.connect(self.path)) as db, db:
            db.row_factory = sqlite3.Row
            db.execute("begin immediate")
            results = []
            for step in batch:
                rows = db.execute(step["sql"], step["params"]).fetchall()
                results.append({"success": True, "results": [{key: row[key] for key in reversed(row.keys())} for row in rows]})
        return io.BytesIO(json.dumps({"success": True, "result": results}).encode())


@pytest.fixture
def d1(tmp_path, monkeypatch):
    """Set up the D1 adapter without a Cloudflare account."""
    # Keep the public endpoint fixed while each test uses a new backend cache key.
    monkeypatch.setattr(tables, "_backends", {})
    monkeypatch.setattr(tables, "_tables", {})
    fake = FakeD1(tmp_path / "d1.db")
    monkeypatch.setattr(tables, "urlopen", fake)
    monkeypatch.setattr(settings, "D1_TOKEN", "not-a-real-token")
    return "d1://test-account/test-db", fake


@pytest.fixture(params=["memory", "sqlite", "postgres", "d1"])
def backend(request, tmp_path):
    """Open independent key spaces and clean up test records."""
    url = None
    if request.param == "sqlite":
        url = f"sqlite:///{tmp_path / 'factory.db'}"
    elif request.param == "postgres":
        url = os.environ.get("FACTORY_TEST_POSTGRES")
        if not url:
            pytest.skip("Set FACTORY_TEST_POSTGRES to run the PostgreSQL table tests.")
    elif request.param == "d1":
        url, _ = request.getfixturevalue("d1")
    name = f"test_{uuid4().hex}"
    opened = {}

    def space(key):
        """Return one table for each test key space."""
        if key not in opened:
            opened[key] = MemoryTable() if url is None else open_table(name + key, url)
        return opened[key]

    yield space, url, name, request.param
    if request.param == "postgres":
        for table in opened.values():
            table.transact(deletes=[key for key, _ in table.query("")])


def test_query_in_key_order(backend):
    """Keep literal prefixes, sort Unicode keys and apply the inclusive start and limit."""
    space, _, _, _ = backend
    table = space("records")
    ordered = ["a%_", "a%_A", "a%_a", "a%_é", "a%_￿", "a%_😀", "a%_\U0010ffff", "a%_\U0010ffffz"]
    items = {key: {"value": index} for index, key in enumerate(ordered)}
    table.transact(dict(reversed(list(items.items()))) | {"aXY": {}, "a%`:not": {}, "later": {}})
    assert table.query("a%_") == list(items.items())
    assert table.query("a%_", start="a%_a", limit=2) == [(key, items[key]) for key in ordered[2:4]]
    assert table.query("a%_", start="a%_Z", limit=1) == [("a%_a", items["a%_a"])]
    assert table.query("a%_", start="b") == []
    assert table.query("a%_", limit=0) == []
    assert table.query("", start="later") == [("later", {})]


def test_conditions_see_old_items(backend):
    """A failed condition changes nothing; a successful one uses the state before the writes."""
    space, _, _, _ = backend
    table = space("records")
    previous = {"a": {"version": 1, "text": "1"}, "b": {"version": 4}, "x": {"text": "old"}}
    table.transact(previous)
    puts = {"a": {"version": 2}, "x": {"text": "new"}, "c": {"version": 1}}
    conditions = [Equals("a", "version", 1), Absent("c"), Equals("x", "text", "old")]
    assert not table.transact(puts, ["b", "x"], [*conditions, Absent("b")])
    assert not table.transact(puts, ["b", "x"], [Equals("a", "text", 1)])
    assert dict(table.query("")) == previous
    assert table.transact(puts, iter(["b", "x"]), iter(conditions))
    assert dict(table.query("")) == puts
    assert not table.transact(conditions=[Absent("a")])
    assert table.transact(conditions=[Absent("b")])


def test_two_spaces_do_not_mix(backend):
    """One key space cannot read, delete or block a key of another one."""
    space, url, name, _ = backend
    left, right = space("left"), space("right")
    if url:
        assert open_table(name + "left", url) is left
    assert left.transact({"same": {"value": "left"}}, conditions=[Absent("same")])
    assert right.get("same") is None
    assert right.transact({"same": {"value": "right"}}, conditions=[Absent("same")])
    assert right.transact(deletes=["same"], conditions=[Equals("same", "value", "right")])
    assert left.query("") == [("same", {"value": "left"})]
    assert right.query("") == []


def _insert(table, barrier, value):
    """Wait for the other writer before trying the same absent key."""
    barrier.wait(timeout=30)
    return table.transact({"key": {"value": value}}, conditions=[Absent("key")])


def _process_insert(url, name, barrier, results, value):
    """Open a separate process's connection and try to write the key."""
    results.put((value, _insert(open_table(name, url), barrier, value)))


def test_only_one_writer_wins(backend):
    """Reject the second conditional write from another writer."""
    space, url, name, kind = backend
    if kind in {"sqlite", "postgres"}:
        context = multiprocessing.get_context("spawn")
        barrier, results = context.Barrier(2), context.Queue()
        processes = [context.Process(target=_process_insert, args=(url, name + "race", barrier, results, i)) for i in range(2)]
        try:
            for process in processes:
                process.start()
            outcomes = [results.get(timeout=40) for _ in processes]
            for process in processes:
                process.join(10)
                assert process.exitcode == 0
        finally:
            for process in processes:
                with suppress(ValueError):
                    if process.is_alive():
                        process.terminate()
                        process.join(10)
                process.close()
            results.close()
    else:
        table, barrier = space("race"), Barrier(2)
        with ThreadPoolExecutor(max_workers=2) as pool:
            writes = [pool.submit(_insert, table, barrier, i) for i in range(2)]
            outcomes = [(i, write.result(timeout=40)) for i, write in enumerate(writes)]
    assert sorted(won for _, won in outcomes) == [False, True]
    assert space("race").get("key") == {"value": next(i for i, won in outcomes if won)}


def test_d1_batch_rechecks_the_conditions(d1):
    """Leave every item unchanged when another writer wins after the initial read."""
    url, fake = d1
    table = open_table("records", url)
    table.transact({"delete": {"version": 1}})

    def other_writer():
        """Write the key that must be absent before the guarded batch."""
        table.transact({"guard": {"version": 2}})

    fake.before_batch = other_writer
    assert not table.transact({"new": {"version": 1}}, ["delete"], [Absent("guard")])
    assert table.query("") == [("delete", {"version": 1}), ("guard", {"version": 2})]
