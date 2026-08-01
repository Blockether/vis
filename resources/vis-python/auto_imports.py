def __vis_auto_imports__():
    import builtins as _b
    import importlib as _il
    import json as _json, re as _re, os as _os, sys as _sys, time as _time
    _b.json = _json; _b.re = _re; _b.os = _os; _b.sys = _sys; _b.time = _time
    _b.builtins = _b
    class _LazyStd:
        def __init__(self, bind, mod, attr):
            object.__setattr__(self, '_bind', bind)
            object.__setattr__(self, '_mod', mod)
            object.__setattr__(self, '_attr', attr)
        def _resolve(self):
            bind = object.__getattribute__(self, '_bind')
            m = _il.import_module(object.__getattribute__(self, '_mod'))
            attr = object.__getattribute__(self, '_attr')
            val = getattr(m, attr) if attr else m
            setattr(_b, bind, val)
            return val
        def __getattr__(self, k):
            return getattr(_LazyStd._resolve(self), k)
        def __call__(self, *a, **k):
            return _LazyStd._resolve(self)(*a, **k)
    for bind, mod, attr in (
        ('shlex', 'shlex', None),
        ('hashlib', 'hashlib', None),
        ('glob', 'glob', None),
        ('collections', 'collections', None),
        ('Counter', 'collections', 'Counter'),
        ('pathlib', 'pathlib', None),
        ('Path', 'pathlib', 'Path'),
        ('textwrap', 'textwrap', None),
        ('base64', 'base64', None),
        ('math', 'math', None),
        ('socket', 'socket', None),
        ('datetime', 'datetime', None),
    ):
        setattr(_b, bind, _LazyStd(bind, mod, attr))
__vis_auto_imports__()
del __vis_auto_imports__
