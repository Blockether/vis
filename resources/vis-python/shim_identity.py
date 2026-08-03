# Module IDENTITY for the sandbox shims.
#
# A shim module is synthesised by eval'ing its Python source into the context, so
# it has none of the attributes an imported module normally carries. Introspection
# that every real library supports then blows up in the agent's face:
#
#     import PIL; PIL.__file__      -> AttributeError
#
# `__vis_stamp_shim__` is called by the host right after a shim's source is
# eval'd (eager path) or lazily loaded (`__vis_load_shim__`), and stamps the
# modules that shim owns:
#
#     __file__     `<vis-shim>/vis-shims/<name>.py` - the CLASSPATH RESOURCE the
#                  module really came from. Angle-bracketed like CPython's own
#                  `<frozen importlib._bootstrap>`, because it is not a real path.
#     __vis_shim__ the shim id, the POSITIVE marker that this module is a vis
#                  shim (`module_runner` keys `-m <mod>` off it, since `__file__`
#                  is no longer the giveaway).
#     __version__  only when the shim declares none itself, and only on the
#                  top-level module - a submodule inherits nothing.
#
# Existing attributes are never overwritten: a shim's own `__version__` wins.


def __vis_init_shim_identity__():
    import sys as _sys

    def stamp(spec_json):
        import json as _j

        spec = _j.loads(spec_json)
        sid = spec.get("sid")
        origin = spec.get("file")
        version = spec.get("version")
        roots = set(spec.get("names") or ())
        if not roots:
            return
        for name, mod in list(_sys.modules.items()):
            if mod is None:
                continue
            root = name.split(".")[0]
            if root not in roots:
                continue
            try:
                if getattr(mod, "__file__", None) is None:
                    mod.__file__ = origin
                if getattr(mod, "__vis_shim__", None) is None:
                    mod.__vis_shim__ = sid
                if name == root and getattr(mod, "__version__", None) is None:
                    mod.__version__ = version
            except Exception:
                # A proxy / frozen module that refuses attributes is not worth
                # failing an import over.
                pass

    return stamp


__vis_stamp_shim__ = __vis_init_shim_identity__()
del __vis_init_shim_identity__
