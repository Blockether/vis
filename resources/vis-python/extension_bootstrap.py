"""Injector — build the `vis` module for ONE extension context.

The module BODY is `packages/vis-agent/src/vis/__init__.py`, the same file PyPI
ships as `vis-agent`. `internal.python-extensions/bootstrap-python` slurps it off
the classpath and prepends it here as `_vis_body`, so there is exactly one copy of
the extension API in the repository and the sandbox runs the code an author can
`pip install` and read.

This fragment does only what the package cannot do for itself: seed `_host` with
the polyglot callables the engine bound into this context, exec the body into a
module dict of its own (so the extension file's globals stay clean), and register
it in `sys.modules` so `import vis` works.

`_host` is an OBJECT with one attribute per op — the shape `vis_contract.Host`
declares, and the same shape `vis._outside` builds when nobody seeded one — so a
host is a thing anyone can implement, not a dict literal only this file knows how
to spell. Every attribute below is an op in
`packages/vis-contract/resources/vis-contract/python-host.edn`, and
`python_host_test` fails when this object and that document disagree.
"""

import sys as _vis_sys, types as _vis_types

_vis_mod = _vis_types.ModuleType("vis")
_vis_mod.__dict__["_host"] = _vis_types.SimpleNamespace(
    state_get=__vis_host_state_get__,
    state_put=__vis_host_state_put__,
    state_del=__vis_host_state_del__,
    state_keys=__vis_host_state_keys__,
    log=__vis_host_log__,
    notify=__vis_host_notify__,
    shell=__vis_host_shell__,
    jailed_shell=__vis_host_jailed_shell__,
    jailed_shell_session=__vis_host_jailed_shell_session__,
    request_input=__vis_host_request_input__,
    live=__vis_host_live__,
    reveal_secret=__vis_host_reveal_secret__,
    forget_secret=__vis_host_forget_secret__,
    declare_env=__vis_host_declare_env__,
)
exec(compile(_vis_body, "vis/__init__.py", "exec"), _vis_mod.__dict__)
_vis_sys.modules["vis"] = _vis_mod


def __vis_registration__():
    return _vis_sys.modules["vis"].__dict__["_registration"]["spec"]
