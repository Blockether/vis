import os
def first(): pass
class Base(object):
    def method(self, a, b = 1, *args, c: int=2, **kwargs) -> None:
        pass
    @property
    def value(self): return self._value
    async def fetch(self, url: str, timeout: float = 10.0, retries: int = 3, backoff: float = 0.5) -> dict[str, object]:
        pass
@decorator
@another.decorator(with_args=True)
def decorated(): ...
def long_parameters(first_parameter, second_parameter, third_parameter, fourth_parameter):
    def inner():
        pass
    return inner
def with_return_annotation(self) -> Set["Muy", "Long", "Annotation", "That", "Goes", "Beyond"]:
    ...
def generic[T: (int, str), *Ts, **P](x: T) -> T: return x
type Alias[T] = list[T] | None
add = lambda x,y: x+y
class Empty: ...
class WithBases(Base, metaclass=Meta): x: int=1
def defaults(a = -1, b = not True, c = (1, 2), *, d = {}): pass
