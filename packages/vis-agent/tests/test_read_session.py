"""`vis.read_session` and `vis.list_sessions` use the canonical host session reads."""

from __future__ import annotations

import json

import blockether.vis.extension as vis
import pytest
from blockether.vis import _outside


def test_answers_the_host_read_as_one_string_keyed_dict(monkeypatch):
    targets = []
    data = {
        "session": {"id": "6fccb4a2-bf20-448c-ac4f-9d170983c0f3"},
        "transcript": {"turns": [{"position": 1, "user_request": "Fix the typo."}]},
    }

    def read_session(target):
        targets.append(target)
        return json.dumps(data)

    monkeypatch.setattr(vis._host, "read_session", read_session)
    assert vis.read_session() == data
    assert vis.read_session("vis_session_id#6fccb4a2") == data
    assert targets == [None, "vis_session_id#6fccb4a2"]


def test_answers_the_host_search_rows_in_their_rank_order(monkeypatch):
    searches = []
    rows = [
        {"id": "b", "title": "Fix the typo", "rank": 0, "is_in_title": True},
        {"id": "a", "title": "Release", "rank": 1, "is_in_request": True},
    ]

    def list_sessions(search):
        searches.append(search)
        return json.dumps(rows)

    monkeypatch.setattr(vis._host, "list_sessions", list_sessions)
    assert vis.list_sessions() == rows
    assert vis.list_sessions(search="typo") == rows
    assert searches == [None, "typo"]


@pytest.mark.parametrize(
    "call, message",
    [
        (vis.read_session, "vis.read_session needs a bound Vis session"),
        (vis.list_sessions, "vis.list_sessions needs a bound Vis session"),
    ],
)
def test_refuses_outside_a_vis_session(call, message):
    with pytest.raises(_outside.Refused, match=message):
        call()
