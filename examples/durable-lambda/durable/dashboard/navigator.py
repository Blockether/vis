"""The list of the sessions in Vis web, with the sessions of each shard.

The gateway of `main` answers the list, its groups and the search. The front adds the rows
that each other shard recorded in S3 (`shards.show`), so a look at the list starts no worker.
Each page gets the rows that have their place in it, in the order of the gateway and in
the archive view of the call. A search finds such a session by its title, and by the
requests and the replies that its shard recorded (`shards.tell`).
Each home has its own ids: a row gets the project of `main` that has the same directory
and the group of `main` that has the same name.
"""

from __future__ import annotations

import base64
import dataclasses
import json
import re
import time
import urllib.parse
from collections.abc import Callable
from typing import Any

from durable import shards
from durable.dashboard.web import Request

LIST, GROUPS, SEARCH = "/v1/sessions", "/v1/session-groups", "/v1/sessions/actions/search"
# The fields of a list call and of a search call whose answer gets the rows of the other shards.
HEAD = {"order", "root", "limit", "grouped", "archived", "ids", "group_id", "after", "dirty"}
ASKED = {"q", "limit", "archived", "channel", "root", "after", "dirty", "project_id", "group_ids"}
# The start of a cursor of the front. Each other cursor is a cursor of the gateway.
MARK = "front."
# The function that gives the place of a row in the order of the gateway.
Rank = Callable[[dict[str, Any]], tuple[int, float, str]]
# A word of a text, the rank of a hit by its side as in the gateway, and the hits of one session in an answer.
WORD, RANK, HITS = re.compile(r"\w+"), {"request": 1, "reply": 2}, 6
# The words of a snippet, and the words of it that come before the first word of the search.
SNIPPET, LEAD = 20, 5
# A project of `main` gets a new read of its groups, for a name that it did not have, only after GROUPS_S.
GROUPS_S = 60
# The time of the last read of the groups of each project directory of `main`, and the id of each group by its name.
_groups: dict[str, tuple[float, dict[str, str]]] = {}
# The project of `main` for each project directory, and the time of the last read for a directory that was not known.
_projects: dict[str, dict[str, Any]] = {}
_asked = 0.0


def wanted(request: Request) -> bool:
    """Return True for a call whose answer gets the sessions of the other shards."""
    fields = dict(urllib.parse.parse_qsl(request.query))
    path = request.path.rstrip("/")
    return request.method == "GET" and ((path == LIST and set(fields) <= HEAD) or (path == SEARCH and set(fields) <= ASKED) or (path == GROUPS and "root" in fields))


def position(after: str | None) -> list[Any] | None:
    """Return the parts of a cursor of the front, or None for a cursor of the gateway and for no cursor.

    The parts: the cursor of `main`, the number of the rows of `main` after it that the
    client has, the place of the last row of another shard that the client has, and
    the size of a page.
    """
    if not after or not after.startswith(MARK):
        return None
    try:
        parts = json.loads(base64.urlsafe_b64decode(after[len(MARK) :]))
    except ValueError:
        return None
    return parts if isinstance(parts, list) and len(parts) == 4 else None


def place(after: str | None) -> list[Any] | None:
    """Return the place in the order that a cursor of the gateway names, or None."""
    band, _, rest = (after or "").partition(":")
    rank, _, name = rest.partition(":")
    try:
        return [int(band), float(rank), name]
    except ValueError:
        return None


def paged(request: Request) -> Request:
    """Return the call for `main` of a list call. A cursor of the front becomes the cursor of `main` and a larger limit."""
    fields = urllib.parse.parse_qsl(request.query, keep_blank_values=True)
    at = position(dict(fields).get("after"))
    if at is None:
        return request
    query = [(name, value) for name, value in fields if name not in ("after", "limit")] + [("limit", str(at[1] + at[3]))] + ([("after", at[0])] if at[0] else [])
    return dataclasses.replace(request, query=urllib.parse.urlencode(query))


def joined(request: Request, raw: bytes, ask: Callable[[str, str], bytes | None]) -> bytes:
    """Return an answer of `main` with the sessions of the other shards. `request` is the call of the client, and `ask` makes one more GET call to `main`."""
    rows = shards.shown()
    try:
        answer = json.loads(raw)
    except ValueError:
        return raw
    fields, path = dict(urllib.parse.parse_qsl(request.query)), request.path.rstrip("/")
    if not isinstance(answer, dict) or not (rows or position(fields.get("after"))):
        return raw
    active = [row for row in rows if not row.get("archived_at")]
    if path == GROUPS:
        return json.dumps(counted(fields["root"], answer, active)).encode()
    # The view of the archive: the active sessions, the archived sessions or both.
    view = (fields.get("archived") or "exclude").lower()
    seen = rows if view == "include" else active if view == "exclude" else [row for row in rows if row.get("archived_at")]
    if path == SEARCH:
        return json.dumps(found(fields, answer, seen, ask)).encode()
    return json.dumps(listed(fields, answer, active, seen, ask)).encode()


def known(root: str, answer: dict[str, Any]) -> dict[str, str]:
    """Record the groups of a project of `main` from an answer. Return the id of each group by its name."""
    at, found = _groups.get(root, (0.0, {}))
    found = {**found, **{group["name"]: group["id"] for group in answer.get("groups") or []}}
    _groups[root] = (at, found)
    return found


def group(root: str, name: str | None, ask: Callable[[str, str], bytes | None]) -> str | None:
    """Return the id of the group of `main` that has a name, or None."""
    at, found = _groups.get(root, (0.0, {}))
    if name and name not in found and time.time() - at > GROUPS_S:
        raw = ask(GROUPS, urllib.parse.urlencode({"root": root, "archived": "include"}))
        _groups[root] = (time.time(), found)
        found = known(root, json.loads(raw)) if raw else found
    return found.get(name) if name else None


def project(root: str | None, ask: Callable[[str, str], bytes | None]) -> dict[str, Any] | None:
    """Return the project of `main` that has a directory, or None."""
    global _asked
    if root not in _projects and time.time() - _asked > GROUPS_S:
        _asked, raw = time.time(), ask(LIST, "limit=1")
        recorded((json.loads(raw).get("overview") or {}) if raw else {})
    return _projects.get(root or "")


def recorded(overview: dict[str, Any]) -> dict[str, dict[str, Any]]:
    """Record the projects of `main` from the overview of a list. Return the project of each directory."""
    projects = {one.get("root"): one for one in overview.get("projects") or []}
    _projects.update(projects)
    return projects


def placed(row: dict[str, Any], home: dict[str, Any], ask: Callable[[str, str], bytes | None]) -> dict[str, Any]:
    """Return the row of a session of another shard with the project and the group of `main`."""
    own = {name: value for name, value in row.items() if name not in ("project_root", "group")}
    own.update(project_id=home["project_id"], project_name=home["name"], group_id=group(home["root"], row.get("group"), ask))
    return own


def counted(root: str, answer: dict[str, Any], rows: list[dict[str, Any]]) -> dict[str, Any]:
    """Add the sessions of the other shards to the counts of the groups of a project."""
    known(root, answer)
    for one in answer.get("groups") or []:
        inside = [row for row in rows if row.get("project_root") == root and row.get("group") == one["name"]]
        one["session_count"] = one.get("session_count", 0) + len(inside)
        one["active_at"] = max([one.get("active_at") or 0, *(row.get("modified_at") or 0 for row in inside)])
        answer["session_total"] = answer.get("session_total", 0) + len(inside)
    return answer


def order(fields: dict[str, str], is_recent: bool = False) -> Rank:
    """Return the function that gives the place of a row in the order of the gateway: its band, its rank in the band and its id.

    The list has the favorites first, then the sessions with words that are not sent
    (`dirty`), then the sessions with a running turn, then each other session, the
    newest first. A search has one band. The gateway does not read `order` on a list call.
    """
    dirty = set((fields.get("dirty") or "").split(","))

    def rank(row: dict[str, Any]) -> tuple[int, float, str]:
        pin = None if is_recent else row.get("favorite_rank")
        if pin is not None:
            return 0, pin, row["id"]
        band = 0 if is_recent else 1 if row["id"] in dirty else 2 if row.get("live") else 3
        return band, -(row.get("modified_at") or row.get("created_at") or 0), row["id"]

    return rank


def mixed(base: list[dict[str, Any]], more: list[dict[str, Any]], rank: Rank, size: int | None = None) -> tuple[list[dict[str, Any]], int, int]:
    """Put each row of `more` before the first row of `main` that comes after it. The rows of `main` keep their order, and `more` is in order.

    Return at most `size` rows, and the number of the rows of `main` and of `more` in them.
    """
    rows: list[dict[str, Any]] = []
    taken = used = 0
    while (taken < len(base) or used < len(more)) and (size is None or len(rows) < size):
        if used < len(more) and (taken == len(base) or rank(more[used]) < rank(base[taken])):
            rows.append(more[used])
            used += 1
        else:
            rows.append(base[taken])
            taken += 1
    return rows, taken, used


def window(fields: dict[str, str], answer: dict[str, Any], more: list[dict[str, Any]], rank: Rank) -> None:
    """Make one page from a page of `main` and the rows of the other shards, and name the next page with a cursor of the front.

    The page of `main` starts at the cursor of `main` (see `paged`), so its first rows can be rows that the client has.
    """
    asked = fields.get("after")
    after, skip, last, size = position(asked) or [asked, 0, place(asked), None]
    if size is None:
        size = int(fields["limit"]) if (fields.get("limit") or "").isdigit() else answer.get("limit")
    base = (answer.get("sessions") or [])[skip:]
    answer["total"] = answer.get("total", 0) + len(more)
    more = sorted((row for row in more if last is None or rank(row) > tuple(last)), key=rank)
    # Without a limit, the answer has each row.
    answer["sessions"], taken, used = mixed(base, more, rank, size or None)
    if not size:
        return
    if used:
        last = rank(more[used - 1])
    is_left = taken < len(base) or bool(answer.get("has_more"))
    if is_left and taken == len(base):
        after, skip = answer.get("next_cursor"), 0
    else:
        skip += taken
    cursor = MARK + base64.urlsafe_b64encode(json.dumps([after, skip, last, size]).encode()).decode()
    answer.update(limit=size, after=asked, has_more=is_left or used < len(more))
    answer["next_cursor"] = cursor if answer["has_more"] else None


def listed(fields: dict[str, str], answer: dict[str, Any], active: list[dict[str, Any]], seen: list[dict[str, Any]], ask: Callable[[str, str], bytes | None]) -> dict[str, Any]:
    """Add the sessions of the other shards to a page of the list and to its counts.

    The counts get the `active` sessions. The page gets the rows of `seen` that the call
    asks for and that have their place in it. As in the gateway, only the first page has the sessions of the groups.
    """
    overview = answer.get("overview") or {}
    projects = recorded(overview)
    for row in active:
        home = projects.get(row.get("project_root"))
        if home is None:
            continue
        for counts in (overview, home):
            for count, field in (("session_count", "id"), ("live_count", "live"), ("unread_count", "is_unread"), ("awaiting_count", "is_awaiting_input")):
                counts[count] = counts.get(count, 0) + bool(row.get(field))
        home["last_activity_ms"] = max(home.get("last_activity_ms") or 0, row.get("modified_at") or 0)
    ids = set(filter(None, fields["ids"].split(","))) if "ids" in fields else None
    filed, loose = [], []
    rank = order(fields)
    for row in seen:
        # A later page has no overview: the front knows the projects from an earlier answer.
        home = projects.get(row.get("project_root")) or project(row.get("project_root"), ask)
        if home is None or fields.get("root") not in (None, home["root"]) or (ids is not None and row["id"] not in ids):
            continue
        own = placed(row, home, ask)
        if fields.get("group_id") not in (None, own["group_id"]):
            continue
        (filed if fields.get("grouped") == "aside" and own["group_id"] else loose).append(own)
    if filed and "after" not in fields:
        filed.sort(key=rank)
        answer["grouped"] = mixed(answer.get("grouped") or [], filed, rank)[0]
    window(fields, answer, loose, rank)
    return answer


def snippet(tokens: list[str], text: str) -> str | None:
    """Return the part of a text that has the words of a search, or None when a word is not in the text.

    As in the gateway, each word is a complete word of the text, and the last word can be the start of a word.
    """
    words = list(WORD.finditer(text))
    lower = [word.group().lower() for word in words]
    for token in tokens[:-1]:
        if token not in lower:
            return None
    first = next((index for index, word in enumerate(lower) if word == tokens[-1] or (len(tokens[-1]) > 1 and word.startswith(tokens[-1]))), None)
    if first is None:
        return None
    start = max(0, first - LEAD)
    shown = words[start : start + SNIPPET]
    part = " ".join(text[shown[0].start() : shown[-1].end()].split())
    return ("…" if start else "") + part + ("…" if start + SNIPPET < len(words) else "")


def hit(query: str, title: str, texts: list[list[Any]]) -> dict[str, Any] | None:
    """Return what a search says of a session, in the form of the gateway, or None when the session does not have the words.

    The title has the words as one text. A request or a reply has each word.
    """
    tokens = [token.lower() for token in WORD.findall(query)]
    hits = [{"side": side, "snippet": part, "at": at} for side, at, text in (texts if tokens else []) if (part := snippet(tokens, text))]
    is_named = query.lower() in title.lower()
    if not (is_named or hits):
        return None
    hits.sort(key=lambda one: (RANK[one["side"]], -(one["at"] or 0)))
    first = {side: next((one["snippet"] for one in hits if one["side"] == side), None) for side in RANK}
    return {
        "rank": 0 if is_named else RANK[hits[0]["side"]],
        "is_in_title": is_named,
        "is_in_request": bool(first["request"]),
        "is_in_reply": bool(first["reply"]),
        "is_in_thinking": False,
        "request_snippet": first["request"],
        "reply_snippet": first["reply"],
        "hits": hits[:HITS],
    }


def found(fields: dict[str, str], answer: dict[str, Any], seen: list[dict[str, Any]], ask: Callable[[str, str], bytes | None]) -> dict[str, Any]:
    """Add the sessions of the other shards to a page of a search.

    A search without words lists the newest sessions. A search with words finds a session
    of another shard by its title, and by the requests and the replies that its shard recorded.
    """
    query, more = (fields.get("q") or "").strip(), []
    texts = shards.told() if query else {}
    groups = set(filter(None, fields["group_ids"].split(","))) if "group_ids" in fields else None
    for row in seen:
        home = project(row.get("project_root"), ask)
        if home is None or fields.get("root") not in (None, home["root"]) or fields.get("project_id") not in (None, home["project_id"]):
            continue
        match = hit(query, row.get("title") or "", texts.get(row["id"]) or []) if query else None
        own = placed(row, home, ask)
        if (query and match is None) or (groups is not None and own["group_id"] not in groups):
            continue
        more.append({**own, **({"match": match} if query else {})})
    window(fields, answer, more, order(fields, is_recent=True))
    return answer
