from collections import (
    OrderedDict,
    defaultdict,
    namedtuple,
    Counter,
    deque,
    ChainMap,
    UserDict,
)
import os, sys
from . import sibling
from ..parent import thing

with (
    open("input.txt") as source,
    open("output.txt", "w") as destination,
    open("log.txt") as log,
):
    pass
with open("a") as a, open("b") as b:
    pass
try:
    risky()
except (ValueError, TypeError) as error:
    raise RuntimeError("wrapped") from error
else:
    pass
finally:
    cleanup()
try:
    risky()
except* OSError:
    pass
for i in range(3):
    continue
else:
    pass
while True:
    break
del a, b
del c
assert condition, "message"
if a:
    pass
elif b:
    pass
else:
    pass


async def main():
    async with lock:
        async for item in stream:
            await process(item)


print("done")
x = 1
y = 2
