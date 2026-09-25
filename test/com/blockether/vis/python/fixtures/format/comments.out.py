#!/usr/bin/env python3
# comment without a space
import os  # trailing comment

values = [  # after the bracket
    1,
    2,
    3,  # last element
]


def add(
    a,  # first
    b,
):
    # leading body comment
    return a + b  # sum
    # trailing in the block


# standalone before the class
class A:  # after the colon
    pass


if values:
    pass
# between if and else
else:
    pass
call(
    # only a comment
)
wrapped = (  # comment
    1
)
