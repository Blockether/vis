import sys as _sys

def __vis_run_module__(name):
    import importlib, runpy
    mod = None
    try:
        mod = importlib.import_module(name)
    except ImportError:
        mod = None
    if mod is not None and getattr(mod, '__file__', None) is None:
        entry = getattr(mod, 'console_main', None) or getattr(mod, 'main', None)
        if callable(entry):
            try:
                rc = entry(_sys.argv[1:])
            except SystemExit as _e:
                rc = _e.code
            return 0 if rc is None else (rc if isinstance(rc, int) else 1)
    try:
        runpy.run_module(name, run_name='__main__', alter_sys=True)
        return 0
    except SystemExit as _e:
        return 0 if _e.code is None else (_e.code if isinstance(_e.code, int) else 1)
    except ImportError:
        _sys.stdout.write('vis python: No module named ' + str(name) + chr(10))
        return 1
