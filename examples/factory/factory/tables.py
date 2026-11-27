"""One database for the factory records and each memory."""

from __future__ import annotations

import json
import sqlite3
import time
from collections.abc import Iterable, Mapping
from contextlib import closing, contextmanager
from pathlib import Path
from threading import Lock
from urllib.error import URLError
from urllib.parse import unquote, urlsplit
from urllib.request import Request, urlopen
from uuid import uuid4

from vis_optmem.shared import Absent, Condition, Item, Table

from factory import settings

_SCHEMA = """create table if not exists factory_items (
    space text collate {collation} not null,
    key text collate {collation} not null,
    item text not null,
    primary key (space, key)
)"""
_GET = "select item from factory_items where space = ? and key = ?"
_DELETE = "delete from factory_items where space = ? and key = ?"
_UPSERT = " on conflict (space, key) do update set item = excluded.item"
_PUT = "insert into factory_items (space, key, item) values (?, ?, ?)" + _UPSERT
_BUSY_S = 30


def _json(value: object) -> str:
    """Encode a value as compact JSON."""
    return json.dumps(value, separators=(",", ":"))


def _matches(condition: Condition, raw: str | None) -> bool:
    """Check a condition against the item as it was before any writes."""
    if isinstance(condition, Absent):
        return raw is None
    return raw is not None and json.loads(raw).get(condition.field) == condition.value


def _upper(prefix: str) -> str | None:
    """Give the first key after the prefix range, when there is one."""
    for i in range(len(prefix) - 1, -1, -1):
        point = ord(prefix[i])
        if point < 0x10FFFF:
            return prefix[:i] + chr(0xE000 if point == 0xD7FF else point + 1)
    return None


class _Backend:
    """Run SQL for all key spaces of one database."""

    location: str
    max_writes: int | None = None

    def read(self, sql, params, columns):
        """Run a read and return rows as tuples."""
        raise NotImplementedError

    def write(self, space, puts, deletes, conditions):
        """Write records in one transaction."""
        raise NotImplementedError


class _Table(Table):
    """A sorted range of keys in one database."""

    def __init__(self, name: str, backend: _Backend) -> None:
        """Keep the key space and its database."""
        self._name = name
        self._backend = backend
        self.location = f"{backend.location} ({name})"
        self.max_writes = backend.max_writes

    def get(self, key: str) -> Item | None:
        """Get one item from this key space."""
        rows = self._backend.read(_GET, (self._name, key), ("item",))
        return json.loads(rows[0][0]) if rows else None

    def query(self, prefix: str, start: str = "", limit: int | None = None) -> list[tuple[str, Item]]:
        """Get the range in key order, starting at or above start."""
        sql = "select key, item from factory_items where space = ? and key >= ?"
        params = [self._name, max(prefix, start)]
        if (upper := _upper(prefix)) is not None:
            sql += " and key < ?"
            params.append(upper)
        sql += " order by key"
        if limit is not None and limit >= 0:
            sql += " limit ?"
            params.append(limit)
        rows = self._backend.read(sql, params, ("key", "item"))
        return [(key, json.loads(item)) for key, item in rows[:limit]]

    def transact(self, puts: Mapping[str, Item] | None = None, deletes: Iterable[str] = (), conditions: Iterable[Condition] = ()) -> bool:
        """Apply the puts and deletes only when all the conditions are true."""
        puts = {key: _json(item) for key, item in (puts or {}).items()}
        deletes = list(deletes)
        if self.max_writes is not None and len(puts) + len(deletes) > self.max_writes:
            raise ValueError(f"One transaction takes at most {self.max_writes} writes.")
        return self._backend.write(self._name, puts, deletes, tuple(conditions))


class _Local(_Backend):
    """SQL with a lock that lasts until the transaction ends."""

    def read(self, sql, params, columns):
        """Read the database on a connection for this call."""
        with self.connect() as db:
            return self.execute(db, sql, params).fetchall()

    def write(self, space, puts, deletes, conditions):
        """Check the conditions and make the changes with the write lock held."""
        with self.transaction(space) as db:
            for condition in conditions:
                row = self.execute(db, _GET, (space, condition.key)).fetchone()
                if not _matches(condition, None if row is None else row[0]):
                    return False
            for key in deletes:
                self.execute(db, _DELETE, (space, key))
            for key, item in puts.items():
                self.execute(db, _PUT, (space, key, item))
        return True


class _SQLite(_Local):
    """SQLite in WAL mode with one write lock across all processes."""

    def __init__(self, path: str) -> None:
        """Open the database file and set WAL mode before use."""
        self.path = Path(path).resolve()
        self.location = f"sqlite:{self.path}"
        self.path.parent.mkdir(parents=True, exist_ok=True)
        with self.connect() as db:
            # A new database can be busy before SQLite starts its busy timer.
            deadline = time.monotonic() + _BUSY_S
            while True:
                try:
                    mode = db.execute("pragma journal_mode = wal").fetchone()[0]
                    if mode != "wal":
                        raise RuntimeError("The factory needs SQLite WAL mode.")
                    break
                except sqlite3.OperationalError as error:
                    if error.sqlite_errorcode != sqlite3.SQLITE_BUSY or time.monotonic() >= deadline:
                        raise
                    time.sleep(0.01)
            db.execute(_SCHEMA.format(collation="binary"))

    def connect(self):
        """Make a connection that closes after each call."""
        return closing(sqlite3.connect(self.path, timeout=_BUSY_S, isolation_level=None))

    @staticmethod
    def execute(db, sql, params):
        """Run SQL with SQLite parameter markers."""
        return db.execute(sql, params)

    @contextmanager
    def transaction(self, space):
        """Take SQLite's write lock before any reads."""
        with self.connect() as db, db:
            db.execute("begin immediate")
            yield db


class _Postgres(_Local):
    """PostgreSQL with a transaction lock for each key space."""

    location = "postgres"

    def __init__(self, url: str) -> None:
        """Load the optional driver and set up the table."""
        try:
            import psycopg
        except ImportError:
            raise RuntimeError("Install the factory postgres extra to use PostgreSQL.") from None
        self._driver = psycopg
        self._url = url
        with self.transaction("") as db:
            db.execute(_SCHEMA.format(collation='"C"'))

    def connect(self):
        """Make a connection without printing the database URL on failure."""
        try:
            return self._driver.connect(self._url, connect_timeout=_BUSY_S)
        except self._driver.Error:
            raise RuntimeError("Cannot connect to the PostgreSQL factory database.") from None

    @staticmethod
    def execute(db, sql, params):
        """Run SQL with PostgreSQL parameter markers."""
        return db.execute(sql.replace("?", "%s"), params)

    @contextmanager
    def transaction(self, space):
        """Wait for other writes in this key space before reading it."""
        with self.connect() as db:
            db.execute("set transaction isolation level read committed read write")
            db.execute("select pg_advisory_xact_lock(hashtext('factory_items'), hashtext(%s))", (space,))
            yield db


class _D1(_Backend):
    """Cloudflare D1 with one REST batch for each atomic write."""

    location = "d1"
    max_writes = 50

    def __init__(self, account: str, database: str, token: str) -> None:
        """Set up the table in the D1 database."""
        if not token:
            raise ValueError("Set CLOUDFLARE_API_TOKEN to use D1.")
        self._url = f"https://api.cloudflare.com/client/v4/accounts/{account}/d1/database/{database}/query"
        self._token = token
        self._batch([(_SCHEMA.format(collation="binary"), ())])

    def _batch(self, statements):
        """Send SQL statements and their parameters in one D1 request."""
        data = {"batch": [{"sql": sql, "params": [str(value) for value in params]} for sql, params in statements]}
        request = Request(self._url, _json(data).encode(), {"Authorization": f"Bearer {self._token}", "Content-Type": "application/json"})
        try:
            with urlopen(request, timeout=_BUSY_S) as response:
                data = json.load(response)
        except (URLError, ValueError):
            raise RuntimeError("Cannot run the request on the D1 factory database.") from None
        result = data.get("result") or []
        if not data.get("success") or len(result) != len(statements) or not all(row.get("success") for row in result):
            raise RuntimeError("The D1 factory database did not complete the batch.")
        return result

    def read(self, sql, params, columns):
        """Run one read and keep column order in the result."""
        result = self._batch([(sql, params)])
        return [tuple(row[column] for column in columns) for row in result[0]["results"]]

    def write(self, space, puts, deletes, conditions):
        """Check the conditions, then apply every change in one guarded batch."""
        if not conditions:
            batch = [(_DELETE, (space, key)) for key in deletes] + [(_PUT, (space, key, item)) for key, item in puts.items()]
            if batch:
                self._batch(batch)
            return True

        # Read the condition rows together. The batch checks that these rows did not change.
        keys = _json(list(dict.fromkeys(c.key for c in conditions)))
        before = dict(self.read("select key, item from factory_items where space = ? and key in (select value from json_each(?))", (space, keys), ("key", "item")))
        if not all(_matches(c, before.get(c.key)) for c in conditions):
            return False
        if not (puts or deletes):
            return True

        marker = uuid4().hex
        want = _json({c.key: before.get(c.key) for c in conditions})
        # The reserved key space holds the check result for this batch only.
        # Each write sees that result, even when it changes a row used by a condition.
        batch = [
            (
                "insert into factory_items (space, key, item) select '', ?, '{}' where not exists ("
                "select 1 from json_each(?) as want left join factory_items as have"
                " on have.space = ? and have.key = want.key where have.item is not want.value)",
                (marker, want, space),
            )
        ]
        guard = "exists (select 1 from factory_items where space = '' and key = ?)"
        batch += [(_DELETE + " and " + guard, (space, key, marker)) for key in deletes]
        batch += [("insert into factory_items (space, key, item) select ?, ?, ? where " + guard + _UPSERT, (space, key, item, marker)) for key, item in puts.items()]
        batch.append(("delete from factory_items where space = '' and key = ? returning key", (marker,)))
        return bool(self._batch(batch)[-1]["results"])


def _open(url: str) -> _Backend:
    """Choose a backend without reading the environment."""
    if url.startswith(("postgres://", "postgresql://")):
        return _Postgres(url)
    parsed = urlsplit(url)
    if parsed.scheme == "sqlite" and not parsed.netloc and parsed.path.startswith("/") and not (parsed.query or parsed.fragment):
        return _SQLite(unquote(parsed.path))
    if parsed.scheme == "d1" and not (parsed.query or parsed.fragment):
        account, database = parsed.netloc, parsed.path.removeprefix("/")
        if all(part and part.isascii() and part.replace("-", "").isalnum() for part in (account, database)):
            return _D1(account, database, settings.D1_TOKEN)
    raise ValueError("Use a valid sqlite, postgres or d1 factory database URL.")


_lock = Lock()
_backends: dict[str, _Backend] = {}
_tables: dict[tuple[str, str], Table] = {}


def open_table(name: str, url: str = settings.DATABASE) -> Table:
    """Get the shared table for this key space and database URL."""
    if not name:
        raise ValueError("Give the table a name.")
    key = (name, url)
    with _lock:
        if key not in _tables:
            if url not in _backends:
                _backends[url] = _open(url)
            _tables[key] = _Table(name, _backends[url])
        return _tables[key]
