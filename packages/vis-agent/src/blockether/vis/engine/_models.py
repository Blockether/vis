"""The model catalog that the gateway pickers show, with price, billing and limits."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any

from blockether.vis._contracts import validate


@dataclass(frozen=True, slots=True)
class Model:
    """One model with its price, billing, capabilities and provider limits.

    ``pricing`` gives USD rates for 1M tokens: ``input``, ``output`` and the optional
    ``cache_read`` and ``cache_write``. Each item in ``pricing["tiers"]`` applies when
    a request uses more than ``above_tokens`` input tokens. Turn costs use the same
    rates. ``pricing`` is None when no price is known.

    ``billing`` is ``metered`` for an API key, ``subscription`` for an account
    sign-in and ``local`` for a provider without credentials. ``limits`` is the
    limits report of the provider; all models of one provider share it.
    """

    provider: str
    name: str
    label: str
    pricing: dict[str, Any] | None
    billing: str
    is_reasoning: bool
    reasoning_options: tuple[str, ...]
    is_tool_call: bool
    is_image_input: bool
    modalities: dict[str, list[str]]
    is_available: bool
    is_default: bool
    limits: dict[str, Any]
    pricing_source: str | None = None
    family: str | None = None
    context: int | None = None
    input_limit: int | None = None
    output_limit: int | None = None
    knowledge_cutoff: str | None = None
    release_date: str | None = None

    @classmethod
    def from_wire(cls, value: dict[str, Any]) -> Model:
        validate("provider", "model", value)
        fields = {key: value[key] for key in cls.__dataclass_fields__ if key in value}
        fields["reasoning_options"] = tuple(value["reasoning_options"])
        return cls(**fields)

    @classmethod
    def many(cls, value: list[dict[str, Any]]) -> tuple[Model, ...]:
        validate("provider", "models", value)
        return tuple(cls.from_wire(row) for row in value)
