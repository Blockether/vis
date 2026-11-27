"""The team and personal memories of the factory."""

from __future__ import annotations

import json
import re
from dataclasses import asdict
from itertools import islice
from threading import Lock
from typing import Any

from vis_optmem import remote
from vis_optmem.shared import SharedStore

from factory import settings
from factory.tables import open_table


class Memories:
    """Serve Vis memory calls and the factory memory page from the same stores."""

    def __init__(self, url: str = settings.DATABASE) -> None:
        """Keep the database URL and open each memory on first use."""
        self._url = url
        self._stores: dict[str, SharedStore] = {}
        self._lock = Lock()

    def store(self, name: str) -> SharedStore:
        """Open a memory of this factory and create it if it is missing."""
        if name not in settings.MEMORIES:
            raise LookupError("No memory has that name.")
        with self._lock:
            if name not in self._stores:
                store = SharedStore(open_table(f"memory_{name}", self._url))
                if not store.exists():
                    store.create()
                self._stores[name] = store
            return self._stores[name]

    def answer(self, body: bytes) -> tuple[int, dict[str, Any]]:
        """Answer a RemoteStore request after the server checks access."""
        try:
            request = json.loads(body)
        except ValueError:
            request = None
        return remote.answer(request, self.store)

    def listing(self) -> dict[str, Any]:
        """List the memories and the size and location of each one."""
        stores = ((name, self.store(name)) for name in settings.MEMORIES)
        return {"memories": [{"name": name, "count": store.count(), "location": store.location} for name, store in stores]}

    def entries(self, name: str, pattern: str = "", query: str = "", limit: int = 50, before: int | str | None = None) -> dict[str, Any]:
        """Read the newest matches before the given entry ID, or from the end."""
        store = self.store(name)
        count = store.count()
        end = count if before is None else max(0, min(count, int(before)))
        if limit < 0:
            raise ValueError("The memory limit must not be negative.")
        match = None
        if pattern:
            try:
                match = re.compile(pattern, re.IGNORECASE)
            except re.error:
                raise ValueError("The memory pattern is not a valid regular expression.") from None
        if not query.strip() and match is None:
            found = reversed(store.entries(max(0, end - limit), end))
        else:
            if query.strip():
                candidates = sorted(store.search(query, count), key=lambda entry: entry.id, reverse=True)
            else:
                candidates = reversed(store.entries(0, end))
            found = islice((entry for entry in candidates if entry.id < end and (match is None or match.search(entry.line))), limit)
        return {"name": name, "count": count, "entries": [asdict(entry) for entry in found]}
