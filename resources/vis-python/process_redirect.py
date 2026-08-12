# Guest-side repair for the redirect GraalPy throws away.
#
# `subprocess.run(..., stdout=open(path, 'w'))` never reaches `path` in this
# runtime. GraalPy hands the host a plain INHERIT for every file or descriptor
# redirect, so the bytes go to whatever fd 1 the JVM owns — under a foreground
# gateway, the operator's terminal — and the file the extension named stays
# empty. A `stdin=` file is worse than lost: the child inherits the JVM's own
# stdin and blocks forever waiting for input the extension already supplied.
#
# The host `ProcessHandler` cannot repair this, because the guest's choice is
# already gone by the time a `ProcessCommand` reaches it. So the translation
# happens here, while the choice is still a Python object: a redirect GraalPy
# would discard becomes a pipe, and a daemon thread copies bytes between that
# pipe and the descriptor the extension actually asked for.


def __vis_install_process_redirect__():
    # Eager, not lazy: deferring to an import hook means trusting one, and
    # GraalPy hands `import subprocess` a module of its own rather than the
    # one a `spec_from_loader` hook prepared — a repair that silently fails to
    # apply is worse than the ~25 ms this import costs a context.
    import os
    import subprocess
    import sys
    import threading

    if getattr(subprocess.Popen, "__vis_redirect_repaired__", False):
        return

    chunk_bytes = 65536

    # Bytes still in flight when the child exits are copied while `wait()`
    # blocks, so the file is complete as soon as the call the extension made
    # returns. A grandchild holding the pipe open past this grace keeps the
    # pump running in the background rather than hanging the extension — the
    # same daemon-thread rule the host handler follows.
    drain_grace_seconds = 5.0

    # The `subprocess` sentinels are the negative ints; every other int is a
    # real descriptor the extension named.
    sentinels = (subprocess.PIPE, subprocess.DEVNULL, subprocess.STDOUT)

    def descriptor_of(value):
        """The fd `value` names, or None when subprocess handles it itself."""
        if value is None:
            return None
        if isinstance(value, int):
            return None if value in sentinels else value
        # Anything else has to answer fileno(); a sink that cannot (a BytesIO,
        # say) raises here exactly as it does on CPython, instead of silently
        # losing its bytes to a descriptor nobody named.
        return value.fileno()

    def standard_stream(fd):
        """The guest's own stream object for fd 1/2, else None.

        Writing to descriptor 1 or 2 would escape the context: those are the
        JVM's descriptors, not the polyglot `.out`/`.err` this context is
        built with. Writing through the stream OBJECT keeps the output inside
        the context, so `stdout=sys.stdout` reaches the extension's log rather
        than the operator's terminal.
        """
        stream = {1: sys.stdout, 2: sys.stderr}.get(fd)
        if stream is None:
            return None
        return getattr(stream, "buffer", stream)

    def close_quietly(stream):
        try:
            stream.close()
        except (OSError, ValueError):
            pass

    def close_fd(fd):
        try:
            os.close(fd)
        except OSError:
            pass

    def pump_out(pipe, sink, sink_fd):
        """Child pipe -> the sink the extension named."""
        # `read1` hands over whatever has arrived; plain `read(n)` would hold
        # a line back until the buffer filled, so a child streaming into a
        # file would only reach it in 64 KiB steps.
        read = getattr(pipe, "read1", None) or pipe.read
        try:
            while True:
                chunk = read(chunk_bytes)
                if not chunk:
                    return
                if sink_fd is None:
                    sink.write(chunk)
                    sink.flush()
                else:
                    os.write(sink_fd, chunk)
        except (OSError, ValueError):
            # The extension closed its own sink, or the pipe broke. Neither is
            # the child's problem, and neither may surface in the guest as an
            # exception on a thread it never started.
            return
        finally:
            close_quietly(pipe)
            if sink_fd is not None:
                close_fd(sink_fd)

    def pump_in(source_fd, pipe):
        """The file the extension named -> the child's stdin."""
        try:
            while True:
                chunk = os.read(source_fd, chunk_bytes)
                if not chunk:
                    return
                pipe.write(chunk)
                pipe.flush()
        except (OSError, ValueError):
            return
        finally:
            close_fd(source_fd)
            # The child sees EOF only once this end is closed — without it
            # `cat` never returns, which is the hang this repairs.
            close_quietly(pipe)

    def start_pump(name, target):
        thread = threading.Thread(target=target, name=name, daemon=True)
        thread.start()
        return thread

    # Positional order of `Popen.__init__`: args, bufsize, executable, stdin,
    # stdout, stderr. A caller may pass a redirect either way round.
    positions = {"stdin": 3, "stdout": 4, "stderr": 5}

    def take(name, args, kwargs):
        if name in kwargs:
            return kwargs[name], ("kwargs", name)
        index = positions[name]
        if len(args) > index:
            return args[index], ("args", index)
        return None, None

    def put(where, value, args, kwargs):
        target, key = where
        if target == "kwargs":
            kwargs[key] = value
        else:
            args[key] = value

    def piped(args, kwargs):
        """Swap every discarded redirect for a pipe; answer the fds taken."""
        taken = {}
        for name in ("stdin", "stdout", "stderr"):
            value, where = take(name, args, kwargs)
            fd = descriptor_of(value)
            if fd is None:
                continue
            # An extension handing the child descriptor 0 wants the terminal
            # Vis runs on; rewriting stdin is what makes `sudo` and `ssh`
            # hang, so that one stays exactly as it was asked for.
            if name == "stdin" and fd == 0:
                continue
            taken[name] = fd
            put(where, subprocess.PIPE, args, kwargs)
        return taken

    original_init = subprocess.Popen.__init__
    original_wait = subprocess.Popen.wait

    def pump_stdin(process, fd):
        pipe, process.stdin = process.stdin, None
        # Our own descriptor: the extension may close its file the moment the
        # call returns, and a recycled fd must never be read from or written
        # to.
        return start_pump(
            "vis-redirect-stdin", lambda source=os.dup(fd): pump_in(source, pipe)
        )

    def pump_output(process, name, fd):
        pipe = getattr(process, name)
        setattr(process, name, None)
        sink = standard_stream(fd)
        sink_fd = None if sink is not None else os.dup(fd)
        return start_pump("vis-redirect-" + name, lambda: pump_out(pipe, sink, sink_fd))

    def patched_init(self, *args, **kwargs):
        args = list(args)
        taken = piped(args, kwargs)
        original_init(self, *args, **kwargs)
        self.__vis_redirect_pumps__ = [
            pump_stdin(self, fd) if name == "stdin" else pump_output(self, name, fd)
            for name, fd in taken.items()
        ]

    def patched_wait(self, timeout=None):
        code = original_wait(self, timeout=timeout)
        # The child has exited, so every pipe is at EOF: joining here is what
        # makes the file complete before the extension reads it back.
        for pump in getattr(self, "__vis_redirect_pumps__", ()):
            pump.join(drain_grace_seconds)
        return code

    subprocess.Popen.__init__ = patched_init
    subprocess.Popen.wait = patched_wait
    subprocess.Popen.__vis_redirect_repaired__ = True


__vis_install_process_redirect__()
