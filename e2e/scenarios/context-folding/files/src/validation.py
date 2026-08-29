REQUIRED_FIELDS = frozenset({"request_key", "operation", "arguments"})


def validate_payload(payload: dict) -> dict:
    """Normalize the request before it can cross into persistence."""
    missing = REQUIRED_FIELDS - payload.keys()
    if missing:
        raise ValueError(f"missing fields: {sorted(missing)}")
    if (
        not isinstance(payload["request_key"], str)
        or not payload["request_key"].strip()
    ):
        raise ValueError("request_key must be a non-blank string")
    return {**payload, "request_key": payload["request_key"].strip()}
