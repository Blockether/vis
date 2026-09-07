"""Immutable JSON leaves and lossless wire conversion shared by SDK records."""

from collections.abc import Mapping
from dataclasses import fields, is_dataclass
from types import MappingProxyType
from typing import Any


def freeze(value: Any) -> Any:
    if isinstance(value, dict):
        return MappingProxyType({k: freeze(v) for k, v in value.items()})
    if isinstance(value, list):
        return tuple(freeze(v) for v in value)
    return value


def to_wire(value: Any) -> Any:
    if is_dataclass(value) and not isinstance(value, type):
        return {
            f.name: to_wire(v)
            for f in fields(value)
            if (v := getattr(value, f.name)) is not None
        }
    if isinstance(value, Mapping):
        return {k: to_wire(v) for k, v in value.items()}
    if isinstance(value, (tuple, list)):
        return [to_wire(v) for v in value]
    return value
