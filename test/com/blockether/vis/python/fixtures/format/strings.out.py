single = "hello"
double = "hello"
escaped_single = "it's"
prefers_single = 'say "hi"'
escaped_double = 'say "hi"'
empty = ""
raw = R"\d+"
unicode = "text"
data = b"bytes"
formatted = f"{name!r:>10}"
nested = f"{mapping['key']}"
triple = """one
two"""
concat = "first part second part third part"
long_concat = (
    "a fairly long string literal that starts here "
    "and continues over here with more text"
)


def documented():
    """Docstring with extra spaces."""


def multiline_doc():
    """
    Summary line.

       Indented detail.
    """


class Documented:
    """Class docstring"""

    x = 1
