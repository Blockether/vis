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


def _pair(value: Any, name: str) -> tuple[str | None, str | None]:
    if value is None:
        return None, None
    if not isinstance(value, dict):
        raise ValueError(f"{name} must be an object or null")
    provider, model = value.get("provider"), value.get("model")
    if not all(item is None or isinstance(item, str) for item in (provider, model)):
        raise ValueError(f"{name} provider and model must be strings")
    return provider, model


@dataclass(frozen=True, slots=True)
class SessionModel:
    """The model of the next turn of a session.

    ``provider`` and ``model`` are the pin when ``is_pinned`` is true, else the
    default pair. ``default_provider`` and ``default_model`` are the pair that the
    turns use without a pin; a project configuration can change it.
    """

    provider: str | None
    model: str | None
    is_pinned: bool
    default_provider: str | None = None
    default_model: str | None = None

    @classmethod
    def from_wire(cls, value: Any) -> SessionModel:
        if not isinstance(value, dict):
            raise ValueError("session model must be an object")
        pinned = _pair(value.get("model"), "model")
        default = _pair(value.get("default"), "default")
        is_pinned = pinned[1] is not None
        provider, model = pinned if is_pinned else default
        return cls(provider, model, is_pinned, *default)
