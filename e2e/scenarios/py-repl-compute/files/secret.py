def compute():
    """Sum of squares 1..100 — the caller can't know this without running it."""
    return sum(i * i for i in range(1, 101))
