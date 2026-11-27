"""Check that the Factory engine can read each workflow and its trigger."""

from __future__ import annotations

from pathlib import Path

import pytest


@pytest.mark.parametrize(
    "name,event,inputs",
    [
        ("suggest", "session.idle", [{"name": "session", "default": None, "required": True, "type": "str"}]),
        ("implement", "suggestion.accepted", [{"name": "suggestion", "default": None, "required": True, "type": "str"}, {"name": "comment", "default": "", "required": False, "type": "str"}]),
        ("remember", "suggestion.declined", [{"name": "title", "default": None, "required": True, "type": "str"}, {"name": "comment", "default": "", "required": False, "type": "str"}]),
    ],
)
def test_trigger_uses_event_fields(name, event, inputs):
    """Match each event to a workflow that takes the fields it needs."""
    factory = Path(__file__).resolve().parents[1]
    for module in ("workflows/source.py", "triggers.py"):
        if not (factory / "factory" / module).exists():
            pytest.skip(f"Engine module {module} is not present")
    from factory.workflows.source import describe

    about = describe(name, (factory / "workflows" / f"{name}.py").read_text())
    assert about["inputs"] == inputs
    assert about["triggers"] == [{"id": f"{name}/0", "kind": "on", "event": event, "input": {}}]


@pytest.mark.parametrize("approved", [True, False])
def test_implementation_needs_plan_approval(monkeypatch, approved):
    """Do not edit before approval, and use the approved attachment version."""
    from factory.workflows import Decision, Document
    from workflows import implement

    document = Document("PLAN.md", "command-session", 128, version=3)
    calls = []

    class API:
        """Record the Factory comment."""

        def call(self, method, path, body):
            """Keep the request to check the author and run ID."""
            calls.append((method, path, body))

    class Run:
        """Exercise the workflow's activities in order."""

        run_id = "run-1"

        def _step(self, function, _title, _retries, *args):
            """Execute one activity without a gateway."""
            return function(self, *args)

        def gate(self, key, _title, *, about, document):
            """Return the person's decision for this plan."""
            assert document.version == 3
            return Decision(key, approved, "Use the plan")

        def ask(self, request, model):
            """Record the step that could edit the project."""
            assert approved
            assert "version=3" in request
            return model(summary="Added a check.", checks="One test passed.", passed=True)

    monkeypatch.setattr(implement, "plan", lambda *_args: document)
    monkeypatch.setattr(implement, "Durable", API)
    result = implement.implement(Run(), "suggestion-1")
    assert result == {"approved": approved, "passed": approved}
    [(method, path, body)] = calls
    assert (method, path, body["author"]) == ("POST", "suggestions/suggestion-1/comments", "factory")
    assert body["text"].startswith("Run run-1:")
