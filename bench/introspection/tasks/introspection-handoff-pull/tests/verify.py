from handoff_case import disclosure_failures, verify_transfer


def verify(evidence):
    """Check that the child recovered the value, then wrote the file."""
    return verify_transfer(evidence) or disclosure_failures(evidence)
