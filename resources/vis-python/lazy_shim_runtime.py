def __vis_init_lazy__():
    import sys as _sys
    import builtins as _b
    import importlib.util as _u
    reg = {}
    loading = set()
    loaded = set()
    def _load(sid):
        if sid in loaded or sid in loading:
            return
        loading.add(sid)
        try:
            __vis_load_shim__(sid)
            loaded.add(sid)
        finally:
            loading.discard(sid)
    class _Preloaded:
        def __init__(self, m):
            self._m = m
        def create_module(self, spec):
            return self._m
        def exec_module(self, module):
            pass
    class _Finder:
        def find_spec(self, fullname, path=None, target=None):
            try:
                sid = reg.get(fullname)
                if sid is None:
                    sid = reg.get(fullname.split('.')[0])
                if sid is None:
                    return None
                _load(sid)
                m = _sys.modules.get(fullname)
                if m is None:
                    return None
                return _u.spec_from_loader(fullname, _Preloaded(m))
            except Exception:
                return None
    class _Lazy:
        def __init__(self, sid, name):
            object.__setattr__(self, '_sid', sid)
            object.__setattr__(self, '_name', name)
        def _resolve(self):
            _load(object.__getattribute__(self, '_sid'))
            real = getattr(_b, object.__getattribute__(self, '_name'), None)
            if real is None or real is self:
                raise AttributeError(object.__getattribute__(self, '_name'))
            return real
        def __getattr__(self, k):
            return getattr(_Lazy._resolve(self), k)
        def __call__(self, *a, **k):
            return _Lazy._resolve(self)(*a, **k)
    def register(spec_json):
        import json as _j
        spec = _j.loads(spec_json)
        sid = spec['sid']
        for n in (spec.get('provides') or []):
            reg[n] = sid
        for n in (spec.get('autoload') or []):
            setattr(_b, n, _Lazy(sid, n))
    _sys.meta_path.insert(0, _Finder())
    return register
__vis_register_lazy_shim__ = __vis_init_lazy__()
del __vis_init_lazy__
