# vis sandbox POSIX refusal shim.
#
# `subprocess`, `os.system` and `os.popen` NEVER spawn here — not even when the
# shell tools are ON. Every process in this product starts through the ONE
# `shell` tool, which owns the containment (workspace cwd, process jail), the
# handle (`status`/`logs`/`wait`/`type`/`stop`), the log file on disk and the
# trace record. A second spawn door would be a second, weaker copy of all of
# that, and the model reaches for whichever door it remembers first.
#
# Without this shim the attempt dies as an opaque native-access error, so the
# shim exists ONLY to say where to go instead. WHICH sentence is true depends on
# the `shell` toggle, which can change between two blocks of one session, so the
# tool is looked up in globals() (or the `vis` module) at CALL time and never
# bound at import:
#
#   * shell ON  — name the tool and its invocation, because the work is doable.
#   * shell OFF — say that the toggle turned BOTH doors off, so nothing here
#     suggests `subprocess` is the way around a disabled shell.
#
# Installed once per sandbox context (main + every `sub_loop` fork) by
# env_python/build-agent-context, right after the apropos/doc introspection.


def __vis_install_posix_compat__():
    import sys
    import types

    _USE_SHELL = (
        "`subprocess`, `os.system` and `os.popen` never spawn in the vis "
        "sandbox — every process starts through the `shell` tool, which owns "
        "the jail, the log file and the handle. Use "
        "`await shell({'command': 'npm test'})`; the handle it returns has "
        "`status()`, `logs()`, `wait()`, `type()` and `stop()`."
    )
    _NO_SHELL = (
        "Shell commands are DISABLED in this vis sandbox, so the `shell` tool, "
        "`subprocess`, `os.system` and `os.popen` all refuse to run — there is "
        "no way to start a process here. Turn on 'Shell commands' in the "
        "settings dialog."
    )

    def _refuse(*args, **kwargs):
        fn = globals().get("shell")
        if fn is None:
            vis = sys.modules.get("vis")
            fn = getattr(vis, "shell", None) if vis is not None else None
        raise RuntimeError(_USE_SHELL if fn is not None else _NO_SHELL)

    # The exception types stay real classes: `except subprocess.CalledProcessError`
    # must keep working in code that is about to be told to use `shell` instead,
    # or the refusal is masked by a NameError from the handler line.
    class SubprocessError(Exception):
        pass

    class CalledProcessError(SubprocessError):
        pass

    class TimeoutExpired(SubprocessError):
        pass

    mod = types.ModuleType("subprocess")
    for name in (
        "run",
        "call",
        "check_call",
        "check_output",
        "getoutput",
        "getstatusoutput",
        "Popen",
        "CompletedProcess",
    ):
        setattr(mod, name, _refuse)
    mod.SubprocessError = SubprocessError
    mod.CalledProcessError = CalledProcessError
    mod.TimeoutExpired = TimeoutExpired
    mod.PIPE = -1
    mod.STDOUT = -2
    mod.DEVNULL = -3
    sys.modules["subprocess"] = mod

    # os.system / os.popen refuse identically (they reach the live os module via
    # sys.modules, so a later `import os` sees the patched callables).
    try:
        import os as _os

        _os.system = _refuse
        _os.popen = _refuse
    except Exception:
        pass


__vis_install_posix_compat__()
del __vis_install_posix_compat__
