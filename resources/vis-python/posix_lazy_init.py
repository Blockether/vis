def __vis_posix_lazy__():
    import sys as _sys
    import os as _os
    import importlib.util as _u
    _st = {'done': False}
    def _ensure():
        if _st['done']:
            return
        _st['done'] = True
        try:
            __vis_load_posix__()
        except Exception:
            pass
    class _PosixPreloaded:
        def __init__(self, m):
            self._m = m
        def create_module(self, spec):
            return self._m
        def exec_module(self, module):
            pass
    class _PosixFinder:
        def find_spec(self, fullname, path=None, target=None):
            if fullname != 'subprocess':
                return None
            _ensure()
            m = _sys.modules.get('subprocess')
            if m is None:
                return None
            return _u.spec_from_loader(fullname, _PosixPreloaded(m))
    _sys.meta_path.insert(0, _PosixFinder())
    def _mk(nm):
        def _thunk(*a, **k):
            _ensure()
            return getattr(_os, nm)(*a, **k)
        return _thunk
    try:
        _os.system = _mk('system')
        _os.popen = _mk('popen')
    except Exception:
        pass
__vis_posix_lazy__()
del __vis_posix_lazy__
