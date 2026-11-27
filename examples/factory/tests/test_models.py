"""Check validation of values that cross the Factory API."""

from __future__ import annotations

import pytest
from pydantic import ValidationError

from factory.models import NewSuggestion


@pytest.mark.parametrize("change, field", [({"extra": True}, "extra"), ({"kind": "other"}, "kind")])
def test_new_suggestion_refuses_invalid_data(change, field):
    """Refuse an unknown field or a kind that the API does not support."""
    values = {"title": "Add a check", "body": "Check the saved record.", "kind": "test", "effort": "small"}
    with pytest.raises(ValidationError) as failure:
        NewSuggestion.model_validate(values | change)
    assert failure.value.errors()[0]["loc"] == (field,)
