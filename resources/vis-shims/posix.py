# vis sandbox POSIX-compat shim.
#
# The agent sandbox is deny-by-default (no native access), so CPython's real
# `subprocess` / `os.system` cannot spawn — they fail with an opaque error.
# This shim replaces them with a thin layer that DELEGATES to the vis shell
# TOOL `shell` and its sandbox verbs (`shell_logs`, `shell_type`,
# `shell_stop`), so the
# model's ordinary Python (`subprocess.run([...])`, `os.system(...)`) just works
# and still rides the workspace-cwd containment, timeout,
# process-tree kill, output bounding, render badge, and trace recording.
#
# It is tool-AGNOSTIC by construction: the tool callables are looked up in
# globals() at CALL time, not bound at import. So if the shell tools are absent
# (extension not installed), the shim raises a clear message instead of a
# confusing spawn failure; when present, subprocess/os.system route to it.
#
# Installed once per sandbox context (main + every sub_loop fork) by
# env_python/build-agent-context, right after the apropos/doc introspection.


def __vis_install_posix_compat__():
    import sys
    import types
    import shlex
    import time

    _SHELL_DISABLED = (
        "The shell tools are not enabled in this vis sandbox, so subprocess / "
        "os.system / os.popen cannot run. Turn on 'Shell commands' in the "
        "settings dialog."
    )

    def _tool(name):
        # Extension contexts expose `vis.shell` & co. instead of native
        # process access; agent sandboxes bind the same names bare. One lookup
        # helper serves every stage, so a stage is named exactly once below.
        fn = globals().get(name)
        if fn is None:
            vis = sys.modules.get("vis")
            fn = getattr(vis, name, None) if vis is not None else None
        if fn is None:
            raise RuntimeError(_SHELL_DISABLED)
        # Tools are async-DEFERRED (return a thunk); this internal bridge calls
        # them synchronously, so SETTLE the thunk into its real value here.
        settle = globals().get("__vis_settle__")
        return (lambda *a, **k: settle(fn(*a, **k))) if settle else fn

    def _to_cmd(args, shell):
        # Build the ONE display line for the command. A string is taken verbatim
        # (the `bash -lc` line); a list/tuple is quoted+joined so argv-style calls
        # run safely through the shell tool, whose input is one direct string.
        if isinstance(args, (list, tuple)):
            return " ".join(shlex.quote(str(a)) for a in args)
        return str(args)

    class CalledProcessError(Exception):
        def __init__(self, returncode, cmd, output=None, stderr=None):
            self.returncode = returncode
            self.cmd = cmd
            self.output = output
            self.stdout = output
            self.stderr = stderr
            super().__init__(
                "Command "
                + repr(cmd)
                + " returned non-zero exit status "
                + repr(returncode)
                + "."
            )

    class TimeoutExpired(Exception):
        def __init__(self, cmd, timeout, output=None, stderr=None):
            self.cmd = cmd
            self.timeout = timeout
            self.output = output
            self.stdout = output
            self.stderr = stderr
            super().__init__(
                "Command " + repr(cmd) + " timed out after " + str(timeout) + " seconds"
            )

    class CompletedProcess(object):
        def __init__(self, args, returncode, stdout, stderr):
            self.args = args
            self.returncode = returncode
            self.stdout = stdout
            self.stderr = stderr

        def __repr__(self):
            parts = ["args=" + repr(self.args), "returncode=" + repr(self.returncode)]
            if self.stdout is not None:
                parts.append("stdout=" + repr(self.stdout))
            if self.stderr:
                parts.append("stderr=" + repr(self.stderr))
            return "CompletedProcess(" + ", ".join(parts) + ")"

        def check_returncode(self):
            if self.returncode:
                raise CalledProcessError(
                    self.returncode, self.args, self.stdout, self.stderr
                )

    def run(
        args,
        capture_output=False,
        text=True,
        shell=False,
        cwd=None,
        timeout=None,
        check=False,
        input=None,
        encoding=None,
        errors=None,
        env=None,
        stdout=None,
        stderr=None,
        stdin=None,
        bufsize=-1,
        universal_newlines=None,
        **kwargs,
    ):
        # `text`/`universal_newlines` decide bytes-vs-str on the returned
        # streams; capture_output/stdout/stderr are accepted but the shell tool
        # always captures, so they only affect whether we surface the text.
        sr = _tool("shell")
        cmd = _to_cmd(args, shell)
        opts = {}
        if timeout is not None:
            opts["timeout_secs"] = int(timeout)
        if cwd is not None:
            opts["cwd"] = str(cwd)
        r = sr([cmd], opts)
        # A run is ALWAYS a batch, so this one command's own stdout/stderr/exit
        # is its entry in "commands"; the top level only summarises the group.
        cmds = r.get("commands") or []
        # A run is ALWAYS a batch, so the one command's own stdout/stderr/exit is
        # its first entry — the top level carries NO cmd/stdout/stderr/timed_out
        # of its own, so there is exactly one place each is read from.
        c = cmds[0] if cmds else {}
        if c.get("timed_out"):
            raise TimeoutExpired(
                cmd,
                r.get("timeout_secs") or timeout,
                c.get("stdout") or "",
                c.get("stderr") or "",
            )
        rc = c.get("exit")
        out = c.get("stdout") or ""
        err = c.get("stderr") or ""
        as_text = text if universal_newlines is None else universal_newlines
        if as_text is False:
            out = out.encode("utf-8", "replace")
            err = err.encode("utf-8", "replace")
        cp = CompletedProcess(args, rc, out, err)
        if check:
            cp.check_returncode()
        return cp

    def call(args, **kwargs):
        kwargs.pop("check", None)
        return run(args, **kwargs).returncode

    def check_call(args, **kwargs):
        kwargs.pop("check", None)
        cp = run(args, **kwargs)
        cp.check_returncode()
        return 0

    def check_output(args, text=True, **kwargs):
        kwargs.pop("capture_output", None)
        kwargs.pop("check", None)
        cp = run(args, capture_output=True, text=text, check=True, **kwargs)
        return cp.stdout

    def getstatusoutput(cmd):
        cp = run(cmd, shell=True, capture_output=True, text=True)
        out = cp.stdout or ""
        if out.endswith("\n"):
            out = out[:-1]
        return (cp.returncode or 0, out)

    def getoutput(cmd):
        return getstatusoutput(cmd)[1]

    class Popen(object):
        # Background process backed by `shell` with `wait` 0 — the wait that
        # expires immediately, which is what "background" always meant (a session
        # resource under a pty). Auto picks an id; terminate/kill use
        # `shell_stop`, poll/wait read `shell_logs` status, and communicate walks
        # the log from offset 0.
        _counter = [0]

        def __init__(self, args, shell=False, cwd=None, **kwargs):
            sr = _tool("shell")
            Popen._counter[0] += 1
            self._id = "popen_" + str(Popen._counter[0])
            self.args = args
            opts = {"id": self._id, "wait": 0}
            if cwd is not None:
                opts["cwd"] = str(cwd)
            reg = sr([_to_cmd(args, shell)], opts)
            self.pid = reg.get("pid")
            self.returncode = None

        def _logs(self):
            return _tool("shell_logs")(self._id, {"limit": 1})

        def _read(self, offset):
            return _tool("shell_logs")(self._id, {"offset": offset, "limit": 262144})

        def poll(self):
            r = self._logs()
            if r.get("status") == "exited":
                self.returncode = r.get("exit")
            return self.returncode

        def wait(self, timeout=None):
            deadline = None if timeout is None else time.time() + timeout
            while self.poll() is None:
                if deadline is not None and time.time() > deadline:
                    raise TimeoutExpired(self.args, timeout)
                time.sleep(0.1)
            return self.returncode

        def communicate(self, input=None, timeout=None):
            self.wait(timeout)
            out, off = [], 0
            while True:
                r = self._read(off)
                out.append(r.get("text", ""))
                nxt = r.get("next_offset", off)
                if r.get("is_eof") or nxt <= off:
                    break
                off = nxt
            out = "".join(out)
            return (out, "")

        def terminate(self):
            _tool("shell_stop")(self._id)
            self.returncode = self.returncode if self.returncode is not None else -15

        kill = terminate

        def __enter__(self):
            return self

        def __exit__(self, *exc):
            if self.poll() is None:
                self.terminate()
            return False

    # Assemble a module object and publish it so `import subprocess` finds it.
    mod = types.ModuleType("subprocess")
    mod.run = run
    mod.call = call
    mod.check_call = check_call
    mod.check_output = check_output
    mod.getoutput = getoutput
    mod.getstatusoutput = getstatusoutput
    mod.Popen = Popen
    mod.CompletedProcess = CompletedProcess
    mod.CalledProcessError = CalledProcessError
    mod.TimeoutExpired = TimeoutExpired
    mod.SubprocessError = Exception
    mod.PIPE = -1
    mod.STDOUT = -2
    mod.DEVNULL = -3
    sys.modules["subprocess"] = mod

    # Redirect os.system / os.popen to the same path (they reach the live os
    # module via sys.modules, so a later `import os` sees the patched callables).
    try:
        import os as _os

        def _os_system(command):
            try:
                return run(command, shell=True).returncode or 0
            except Exception:
                return 1

        def _os_popen(command, mode="r", buffering=-1):
            import io

            return io.StringIO(getoutput(command))

        _os.system = _os_system
        _os.popen = _os_popen
    except Exception:
        pass


__vis_install_posix_compat__()
del __vis_install_posix_compat__
