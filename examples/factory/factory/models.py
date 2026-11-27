"""The types of the Factory API and the suggestions of a workflow."""

from __future__ import annotations

from typing import Any, ClassVar, Literal

from pydantic import BaseModel, ConfigDict, Field

from factory.settings import COMMENT_LIMIT

Kind = Literal["fix", "feature", "refactor", "test", "docs", "chore"]
Effort = Literal["small", "medium", "large"]
SuggestionStatus = Literal["open", "accepted", "declined"]
RunStatus = Literal["queued", "running", "waiting", "completed", "failed", "cancelled"]


class Model(BaseModel):
    """A frozen value with a stable type ID and no unknown fields."""

    model_config = ConfigDict(frozen=True, extra="forbid")
    sig: ClassVar[str]

    def __init_subclass__(cls, *, sig: str, **kwargs: Any) -> None:
        """Set the type ID from the class definition."""
        super().__init_subclass__(**kwargs)
        cls.sig = sig


class NewSuggestion(Model, sig="factory.new_suggestion@1"):
    """A change that a person can discuss, accept or decline."""

    title: str = Field(min_length=1)
    body: str = Field(min_length=1)
    kind: Kind
    effort: Effort
    session: str | None = None
    run: str | None = None


class Comment(Model, sig="factory.comment@1"):
    """One comment on a suggestion."""

    at: float
    author: Literal["person", "factory"]
    text: str = Field(max_length=COMMENT_LIMIT)


class Suggestion(NewSuggestion, sig="factory.suggestion@1"):
    """A suggestion as stored by the Factory API."""

    id: str
    status: SuggestionStatus
    talk: str | None
    comments: list[Comment]
    created: float
    decided: float | None


class Suggestions(Model, sig="factory.suggestions@1"):
    """The suggestions that match an API request."""

    suggestions: list[Suggestion]


class RunRef(Model, sig="factory.run_ref@1"):
    """The fields necessary to identify and follow a run."""

    id: str
    workflow: str
    title: str
    status: RunStatus
    session_id: str | None


class WorkflowStart(Model, sig="factory.workflow_start@1"):
    """The workflow and input to send when a person starts a run."""

    workflow: str = Field(min_length=1)
    input: dict[str, Any] | str = Field(default_factory=dict)
