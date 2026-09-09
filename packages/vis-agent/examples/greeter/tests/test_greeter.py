"""The documented example's domain tests require no running Vis engine."""

from dataclasses import FrozenInstanceError

import pytest
from vis_greeter import Greeter


def test_greeting_is_typed_and_immutable():
    result = Greeter().hello("Ada", uppercase=True)
    assert result.text == "HELLO, ADA!"
    assert result.characters == len(result.text)
    with pytest.raises(FrozenInstanceError):
        result.text = "changed"


def test_blank_name_is_rejected():
    with pytest.raises(ValueError, match="blank"):
        Greeter().hello(" ")
