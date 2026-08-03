import ast as __vis_ast__
import builtins as __vis_builtins__
import errno as __vis_errno__
import gc as __vis_gc__
import io as __vis_io__
import os as __vis_os__
import time as __vis_time__
import weakref as __vis_weakref__


# ── deterministic flush for handles a block leaves open. GraalPy does NOT
# refcount, so the CPython idiom `open(p, "w").write(text)` — a handle dropped
# without `close()` — is never finalized at the end of the statement: the bytes
# sit in the buffer and the file on disk stays EMPTY until a GC that may never
# come. That is silent data loss, and the very next tool (`git commit -F <file>`)
# reads the empty file. Every WRITABLE handle the sandbox opens is tracked
# WEAKLY (no lifetime is extended, no fd is held) and flushed before each tool
# call and at the end of the block, so what a block wrote is on disk by the time
# anything else looks at it.
def __vis_survivor__(__vis_name__, __vis_make__):
    # Runtime state that must OUTLIVE a reinstall. `ensure-async-runtime!` re-evals
    # this whole preamble in the SAME globals whenever a block loses
    # `__vis_run_async__` (`globals().clear()` is legal Python), and a plain
    # `x = {}` here would silently drop every pending write and every tracked
    # descriptor. `__vis_pin_runtime__` mirrors each `__vis_*` global into
    # builtins, so the FIRST value is still reachable there: re-adopt it.
    __vis_v__ = getattr(__vis_builtins__, __vis_name__, None)
    return __vis_make__() if __vis_v__ is None else __vis_v__


__vis_open_writes__ = __vis_survivor__("__vis_open_writes__", __vis_weakref__.WeakSet)
# The REAL opener, captured ONCE. By the time a reinstall re-runs this line, the
# name `open` — module global AND `builtins.open` — is already the shim, so a
# fresh capture would make `__vis_open__` call ITSELF forever: exactly the
# self-recursion `ensure-async-runtime!` already unwraps for `print`, one door
# further down.
__vis_real_open__ = __vis_survivor__("__vis_real_open__", lambda: __vis_builtins__.open)

# ── DESCRIPTOR RECLAMATION + CEILING: the same non-refcounting fact, with a much
# harsher failure. A dropped handle also keeps its PROCESS file descriptor: the
# object is collected but the fd is not closed, and neither `__del__` nor weakref
# callbacks ever run (measured: 200 dropped `open()`s = +200 live descriptors,
# two `gc.collect()`s reclaim none of them). A loop over a big tree
# (`open(p).read()` per file) therefore walks the WHOLE process into EMFILE, and
# the first casualty is not Python: `ProcessBuilder` can no longer fork, so every
# later `shell`/`git` call dies with the JDK's misleading "spawn helper / JDK
# version mismatch" text and the session is wedged for good.
#
# So the sandbox reclaims descriptors itself, where the leak is — the way
# CPython's refcount would. Every handle is registered under its fd with a WEAK
# ref; once that ref is dead the handle can never be read again, so its fd is
# closed by hand. GraalPy's weak refs do die on their own under ordinary JVM GC,
# so the common sweep is a cheap fstat pass with NO `gc.collect()` (a collect
# here costs ~270ms; it is the fallback, not the rule). Identity (`st_dev`,
# `st_ino`) is re-checked before every close, so a recycled fd number is never
# stolen from whoever owns it now. `__vis_fd_max__` is the ceiling that cannot be
# crossed: reaching it raises a normal Python `OSError(EMFILE)` naming the fix,
# instead of leaving the session to die later on an unrelated toolchain error.
__vis_fd_registry__ = __vis_survivor__(
    "__vis_fd_registry__", dict
)  # fd -> (weakref(owner), (st_dev, st_ino) | None)


def __vis_fd_env_int__(__vis_name__, __vis_default__, __vis_low__):
    try:
        __vis_n__ = int(__vis_os__.environ.get(__vis_name__) or 0)
    except Exception:
        __vis_n__ = 0
    return __vis_n__ if __vis_n__ >= __vis_low__ else __vis_default__


# Ceiling, and the mark where reclamation starts. The mark is HALF the ceiling so
# an honest workload (handles opened and closed) never sweeps twice for nothing,
# while a leaking one is caught long before the process limit.
__vis_fd_max__ = __vis_fd_env_int__("VIS_PY_MAX_OPEN_FILES", 512, 8)
__vis_fd_sweep_at__ = max(16, __vis_fd_max__ // 2)


def __vis_fd_owner__(__vis_h__):
    # The object that actually OWNS the descriptor. `open()` hands back a STACK
    # (TextIOWrapper -> BufferedReader -> FileIO) and a lower layer happily
    # outlives the one above it: `buf = open(p).buffer` drops the wrapper while
    # the file stays perfectly readable through `buf` (measured). Weak-referencing
    # the TOP layer would close a descriptor still in use, so track the BOTTOM.
    __vis_o__ = __vis_h__
    for _ in range(4):  # 2 in practice; the bound keeps a pathological cycle finite
        try:
            __vis_n__ = getattr(__vis_o__, "buffer", None)
            if __vis_n__ is None:
                __vis_n__ = getattr(__vis_o__, "raw", None)
        except Exception:
            __vis_n__ = None  # detached/closed layer: this is as deep as we get
        if __vis_n__ is None or __vis_n__ is __vis_o__:
            break
        __vis_o__ = __vis_n__
    return __vis_o__


def __vis_fd_track__(__vis_h__):
    # WEAK by construction: tracking must never keep a handle (or its buffer)
    # alive — that would turn a descriptor leak into a memory leak.
    try:
        __vis_fd__ = __vis_h__.fileno()
    except Exception:
        return  # StringIO & friends own no descriptor
    if not isinstance(__vis_fd__, int) or __vis_fd__ < 0:
        return
    try:
        __vis_st__ = __vis_os__.fstat(__vis_fd__)
        __vis_id__ = (__vis_st__.st_dev, __vis_st__.st_ino)
    except Exception:
        __vis_id__ = None
    try:
        __vis_fd_registry__[__vis_fd__] = (
            __vis_weakref__.ref(__vis_fd_owner__(__vis_h__)),
            __vis_id__,
        )
    except Exception:
        pass  # unweakrefable handle: nothing we can track, hand it back as-is


def __vis_fd_drop__(__vis_fd__, __vis_id__):
    # Close ONE unreachable descriptor, but only while it still is the file we
    # opened: if the number was recycled, `fstat` either fails (already closed)
    # or reports another file, and both mean hands off.
    __vis_fd_registry__.pop(__vis_fd__, None)
    try:
        __vis_st__ = __vis_os__.fstat(__vis_fd__)
    except Exception:
        return 0  # already closed; nothing to reclaim
    if __vis_id__ is not None and (__vis_st__.st_dev, __vis_st__.st_ino) != __vis_id__:
        return 0
    try:
        __vis_os__.close(__vis_fd__)
        return 1
    except Exception:
        return 0


__vis_fd_reapers__ = __vis_survivor__("__vis_fd_reapers__", list)


def __vis_run_reapers__():
    # Descriptors this module never opened. A shim (`sqlite3`) hands the block a
    # Python object wrapping a HOST handle, and dropping that object leaks the host
    # connection and its descriptor — measured 14 per 15 dropped `sqlite3.connect()`
    # — because nothing here can see it. A shim registers one cheap callable here
    # and reclaims its own on the same schedule; a reaper must never raise.
    for __vis_reaper__ in list(__vis_fd_reapers__):
        try:
            __vis_reaper__()
        except Exception:
            pass


def __vis_reclaim_fds__(force=False):
    # Drop entries the block closed itself, close the ones it dropped. Cheap:
    # one `fstat` per tracked handle, no collect. Returns descriptors closed.
    # Runs AFTER `__vis_flush_writes__`, and only ever closes a handle whose weak
    # ref is already dead — such a handle can no longer be flushed by anyone
    # (its buffer died with it), so closing its descriptor loses nothing that was
    # not lost the moment the block dropped it.
    __vis_run_reapers__()
    if not __vis_fd_registry__:
        return 0
    if not force and len(__vis_fd_registry__) < __vis_fd_sweep_at__:
        return 0
    __vis_closed__ = 0
    for __vis_fd__ in list(__vis_fd_registry__):
        __vis_e__ = __vis_fd_registry__.get(__vis_fd__)
        if __vis_e__ is None:
            continue
        __vis_h__ = __vis_e__[0]()
        if __vis_h__ is None:
            __vis_closed__ += __vis_fd_drop__(__vis_fd__, __vis_e__[1])
            continue
        try:
            if __vis_h__.closed:
                __vis_fd_registry__.pop(__vis_fd__, None)
        except Exception:
            __vis_fd_registry__.pop(__vis_fd__, None)
    return __vis_closed__


def __vis_fd_admit__():
    # Runs before every sandbox `open`. Under the mark it costs one int compare;
    # at the mark it reclaims, and only a workload that really holds the ceiling
    # open at once is refused — with the message that names the actual fix.
    if len(__vis_fd_registry__) < __vis_fd_sweep_at__:
        return
    __vis_reclaim_fds__(True)
    if len(__vis_fd_registry__) >= __vis_fd_max__:
        # Last resort: force the collect the cheap pass did not need, in case
        # this VM has not gotten around to clearing those weak refs yet.
        __vis_gc__.collect()
        __vis_reclaim_fds__(True)
    if len(__vis_fd_registry__) < __vis_fd_max__:
        return
    raise OSError(
        __vis_errno__.EMFILE,
        "too many open files in this sandbox: "
        + str(len(__vis_fd_registry__))
        + " handles are open at once and the ceiling is "
        + str(__vis_fd_max__)
        + ". Sandbox Python does NOT close a file when you drop it, so"
        " `open(p).read()` in a loop leaks one descriptor per iteration until no"
        " `shell`/`git` process can start at all. Use `with open(p) as f:` (or"
        " `Path(p).read_text()`), or close the handles you keep open."
        " VIS_PY_MAX_OPEN_FILES raises the ceiling.",
    )


def __vis_open__(*__vis_a__, **__vis_kw__):
    __vis_fd_admit__()
    __vis_h__ = __vis_real_open__(*__vis_a__, **__vis_kw__)
    try:
        if __vis_h__.writable():
            __vis_open_writes__.add(__vis_h__)
    except Exception:
        pass  # unweakrefable / no writable(): not ours to track, hand it back as-is
    # `closefd=False` (kwarg, or the 7th positional) says the CALLER owns that
    # descriptor and merely lent it to this wrapper; reclaiming it when the
    # wrapper dies would close a file the block is still using elsewhere.
    if __vis_kw__.get("closefd", True) and (len(__vis_a__) < 7 or __vis_a__[6]):
        __vis_fd_track__(__vis_h__)
    return __vis_h__


def __vis_flush_writes__():
    for __vis_h__ in list(__vis_open_writes__):
        try:
            if not __vis_h__.closed:
                __vis_h__.flush()
        except Exception:
            pass  # best-effort: one broken handle must never break the block


# EVERY door onto a descriptor, not just this module's global. `io.open` is a
# DIFFERENT object from `builtins.open` here, `pathlib.Path.open` and `tempfile.*`
# go through `io.open`, and any stdlib module calling bare `open()` reaches
# `builtins.open` — each of those three leaked 50 descriptors per 50 iterations
# while only the module global was shimmed (measured). `__vis_real_open__` is the
# pre-shim `builtins.open`, which does NOT delegate to `io.open` (measured), so no
# door leads back into the shim.
open = __vis_open__
__vis_builtins__.open = __vis_open__
__vis_io__.open = __vis_open__


# The RAW doors, which reach a descriptor without passing through any `open` at
# all (measured: 25 leaked descriptors per 25 iterations, seen by neither shim
# above): `io.FileIO(p)` IS the descriptor-owning class, and `io.open_code(p)`
# hands back one. `io.FileIO` is an immutable type — its `__init__` cannot be
# hooked (TypeError) — so the shim is a SUBCLASS whose metaclass forwards
# `isinstance`/`issubclass` to the real class: the raws built INSIDE `open` are
# real `FileIO`s, and code asking `isinstance(f.raw, io.FileIO)` must still get
# True after the swap. What stays the CALLER's: `os.open` hands back a bare int
# with no object to weak-ref, so that descriptor is theirs to `os.close`, exactly
# like `closefd=False`.
__vis_real_FileIO__ = __vis_survivor__("__vis_real_FileIO__", lambda: __vis_io__.FileIO)


class __vis_FileIOMeta__(type(__vis_real_FileIO__)):
    def __instancecheck__(cls, __vis_o__):
        return isinstance(__vis_o__, __vis_real_FileIO__)

    def __subclasscheck__(cls, __vis_c__):
        return issubclass(__vis_c__, __vis_real_FileIO__)


class __vis_FileIO__(__vis_real_FileIO__, metaclass=__vis_FileIOMeta__):
    # `FileIO(name, mode="r", closefd=True, opener=None)`: `closefd=False` (kwarg
    # or 3rd positional) means the caller merely lent us its descriptor, exactly
    # as in `__vis_open__`. Nothing joins `__vis_open_writes__` here — a FileIO is
    # unbuffered, so it never holds bytes that a flush could still rescue.
    def __init__(self, *__vis_a__, **__vis_kw__):
        __vis_fd_admit__()
        super().__init__(*__vis_a__, **__vis_kw__)
        if __vis_kw__.get("closefd", True) and (len(__vis_a__) < 3 or __vis_a__[2]):
            __vis_fd_track__(self)


def __vis_open_code__(__vis_p__):
    return __vis_FileIO__(__vis_p__, "rb")


__vis_io__.FileIO = __vis_FileIO__
__vis_io__.open_code = __vis_open_code__


def __vis_count_forms__(src):
    return len(__vis_ast__.parse(src).body)


def __vis_banned_name__(src, banned):
    banned = set(banned)
    return next(
        (
            n.id
            for n in __vis_ast__.walk(__vis_ast__.parse(src))
            if isinstance(n, __vis_ast__.Name) and n.id in banned
        ),
        None,
    )


class __vis_Raise__:
    # Driver -> awaitable signal that the tool/gather call the driver just ran
    # RAISED. The await point re-`raise`s the captured exception INSIDE the
    # coroutine (at the user's own `await`), so an in-block `try/except` around
    # `await tool(...)` CATCHES a tool failure like any other error; left
    # uncaught it escapes the driver exactly as before.
    __slots__ = ("exc",)

    def __init__(self, exc):
        self.exc = exc


class __vis_ToolError__(Exception):
    # A tool/gather failure normalized to a REAL Python exception. Host tool
    # callables raise a foreign exception that derives from BaseException but NOT
    # from Exception, so a plain `except Exception:` would MISS it. Wrapping gives
    # the model the ordinary contract (`except Exception` / `except BaseException`
    # both catch it) with a clean message, while `__vis_orig__` keeps the original
    # host exception so an UNCAUGHT failure still maps to the same host
    # tool-failure error (message + :data) at the sandbox boundary.
    def __init__(self, orig, msg):
        self.__vis_orig__ = orig
        super().__init__(msg)


def __vis_clean_msg__(exc):
    # The bare message of a foreign host exception. `str(exc)` on a host throwable
    # is `fully.qualified.ClassName: message`, and a deny-by-default sandbox does
    # NOT expose its Java `getMessage()`, so strip that leading dotted class name
    # to leave just the message. (The authoritative error channel still recovers
    # the exact host message via ex-message at the boundary.)
    try:
        m = exc.getMessage()
        if m:
            return str(m)
    except BaseException:
        pass
    s = str(exc)
    i = s.find(": ")
    if i > 0:
        head = s[:i]
        if "." in head and " " not in head:
            return s[i + 2 :]
    return s


def __vis_wrap_tool_exc__(exc):
    # A native Python exception passes through untouched (its own type/message are
    # the contract). A foreign host exception is wrapped so `except Exception`
    # catches it; the original rides along as `__vis_orig__` for boundary mapping.
    if isinstance(exc, Exception):
        return exc
    return __vis_ToolError__(exc, __vis_clean_msg__(exc))


class __vis_Call__:
    __slots__ = ("fn", "a", "k", "nm", "ran", "failed", "res")

    def __init__(self, fn, a, k, nm="tool"):
        self.fn = fn
        self.a = a
        self.k = k
        self.nm = nm
        self.ran = False
        self.failed = False
        self.res = None

    def __await__(self):
        __vis_r__ = yield self
        if type(__vis_r__) is __vis_Raise__:
            raise __vis_r__.exc
        return __vis_r__

    def __repr__(self):
        return "<unawaited async tool call: write `await " + self.nm + "(...)`>"

    # INLINE-USE auto-settle. Subscripting / `len(...)` / `in` a deferred call is
    # ALWAYS a single-expression use of that ONE call's result — there is no
    # concurrency to forfeit (unlike a batchable set of calls), so we settle it
    # synchronously right here instead of raising 'not subscriptable'. This kills
    # the `git(...)["stdout"]` / `cat(...)["anchors"]` papercut. We deliberately
    # do NOT add `__iter__`: iteration is exactly the batch-me-instead case the
    # loud repr must keep nudging toward `await gather(...)`.
    def __getitem__(self, k):
        return __vis_settle__(self)[k]

    def __len__(self):
        return len(__vis_settle__(self))

    def __contains__(self, k):
        return k in __vis_settle__(self)

    # ATTRIBUTE auto-settle, same reasoning as `__getitem__` above: `r.get(...)`
    # or `r.items()` on a still-deferred call is a single-expression use of that
    # ONE result, and an unresolved `__vis_Call__` reaching user space is exactly
    # the wedge issue #97 reported (a bare AttributeError naming an object the
    # caller never created). The names the engine's own plumbing PROBES with
    # `hasattr` stay ABSENT (`send`/`throw`/`close` for coroutines, `keys` for
    # pyify's mapping test) so a probe never silently RUNS the call, and dunders
    # keep normal python semantics — `repr` stays loud, iteration stays refused.
    __vis_never_settle__ = frozenset(("send", "throw", "close", "keys"))

    def __getattr__(self, name):
        if name.startswith("_") or name in __vis_Call__.__vis_never_settle__:
            raise AttributeError(name)
        return getattr(__vis_settle__(self), name)


class __vis_Gather__:
    __slots__ = ("aws", "return_exceptions")

    def __init__(self, aws, return_exceptions=False):
        self.aws = aws
        self.return_exceptions = bool(return_exceptions)

    def __await__(self):
        __vis_r__ = yield self
        if type(__vis_r__) is __vis_Raise__:
            raise __vis_r__.exc
        return __vis_r__


def gather(*aws, return_exceptions=False):
    if len(aws) == 1 and isinstance(aws[0], (list, tuple)):
        aws = list(aws[0])
    return __vis_Gather__(list(aws), return_exceptions)


class __vis_Already__:
    # A trivially-ready awaitable: `await __vis_Already__(v)` immediately yields
    # `v` (the `if False: yield` makes this `__await__` a generator, so the
    # object is awaitable, but it never suspends). Used to make `await` on an
    # already-resolved value a no-op that returns the value.
    __slots__ = ("v",)

    def __init__(self, v):
        self.v = v

    def __await__(self):
        if False:
            yield
        return self.v


def __vis_awaitable__(v):
    # Normalize the operand of `await` so awaiting a NON-awaitable just returns
    # it instead of raising `TypeError: object X can't be used in 'await'
    # expression`. The classic trap: `x = patch(...)` AUTO-SETTLES on assignment
    # (so `x` already holds the real ForeignList result), then `await x` blows
    # up. With this, the stray `await` is harmless — we simply don't care.
    # Real awaitables (a deferred `__vis_Call__`, a `gather` `__vis_Gather__`,
    # or anything with `__await__`) pass straight through so `await tool(...)` /
    # `await gather(...)` keep being driven by `__vis_drive__` exactly as before.
    if isinstance(v, (__vis_Call__, __vis_Gather__)):
        return v
    if hasattr(v, "__await__"):
        return v
    return __vis_Already__(v)


def __vis_exec_call__(c):
    if c.ran:
        if c.failed:
            # Disposed before it ever ran (a sibling in the same `await gather(...)`
            # failed, or the block abandoned it). Issue #97: the bare "has already
            # failed" text read like a mystery, so say what happened and how to
            # recover instead of leaving the caller to retry the same dead object.
            raise RuntimeError(
                c.nm
                + "(...) never ran: this deferred call was disposed when a sibling"
                + " in the same `await gather(...)` failed, so it holds no result."
                + " Issue the call again in a fresh `await` — do not reuse this object."
            )
        return c.res
    try:
        # Fold Python **kwargs into a TRAILING DICT positional. The host tool
        # callables are foreign ProxyExecutables that accept ONLY positional args, so
        # `c.fn(*a, **k)` would raise `__call__() got an unexpected keyword argument`.
        # vis tools already take a trailing opts dict — `find("x", paths=[...])`,
        # `rg(query="x")`, `struct_patch(op="delete", target="foo")` — so folding
        # kwargs to one dict matches their contract (all-kwargs collapses to a spec map).
        # Flush what the block wrote through a still-open handle FIRST: a tool
        # that reads a just-written file (`git commit -F /tmp/msg`) must not see
        # GraalPy's unflushed buffer.
        __vis_flush_writes__()
        # Same boundary, the descriptor half: a tool that spawns a process
        # (`shell`, `git`) needs free descriptors to fork with, so give back the
        # ones this block already dropped. Below the mark this is a no-op.
        __vis_reclaim_fds__()
        c.res = c.fn(*c.a, dict(c.k)) if c.k else c.fn(*c.a)
        return c.res
    except BaseException:
        # A failed thunk is one-shot too. Do not cache the exception here: a Python
        # traceback points back through this frame to `c`, which would make a retained
        # failed call retain itself plus the callable and payload graph.
        c.failed = True
        raise
    finally:
        c.ran = True
        # Success, failure, and cancellation all release host callable + arguments.
        c.fn = None
        c.a = ()
        c.k = {}


def __vis_key_hint__(__vis_d__, __vis_k__):
    # A missing key on a TOOL RESULT is a LOOKUP mistake, not a broken tool: shapes
    # differ per tool (shell -> stdout/stderr/exit/duration_ms, run_tests -> output,
    # grep -> matches/hit_count). A bare `KeyError: 'output'` reads as a broken tool, so
    # the model guesses another name and spins. Name the tool, the near miss, and every
    # key it DID return — one wrong guess then ends the guessing.
    __vis_keys__ = list(__vis_d__.keys())
    __vis_op__ = __vis_d__.get("op")
    __vis_who__ = (repr(__vis_op__) + " result") if __vis_op__ else "this result map"
    __vis_have__ = (
        ", ".join([repr(__vis_x__) for __vis_x__ in __vis_keys__]) or "(no keys)"
    )
    if not isinstance(__vis_k__, str):
        return (
            "cannot index "
            + __vis_who__
            + " with "
            + repr(__vis_k__)
            + ": a dict is not sliceable or positional — use list(d), d.items(), or a "
            "string key. Keys: " + __vis_have__
        )
    __vis_low__ = __vis_k__.lower()
    __vis_near__ = [
        __vis_x__
        for __vis_x__ in __vis_keys__
        if isinstance(__vis_x__, str)
        and (__vis_low__ in __vis_x__.lower() or __vis_x__.lower() in __vis_low__)
    ]
    __vis_tip__ = (
        (
            " Did you mean "
            + " / ".join([repr(__vis_x__) for __vis_x__ in __vis_near__])
            + "?"
        )
        if __vis_near__
        else ""
    )
    return (
        repr(__vis_k__)
        + " is not a key of "
        + __vis_who__
        + ". Keys: "
        + __vis_have__
        + "."
        + __vis_tip__
        + " Read the keys it returned instead of guessing another "
        "name; use .get(k, default) when the field is optional."
    )


class __VisDict__(dict):
    # EVERY map rebuilt from the host boundary: a tool result, each nested map inside
    # it, and `session`. Still a real dict (json / mutation / isinstance / {**d} all
    # work), but a missing key raises the self-describing KeyError above instead of a
    # bare one. Result shapes are per-tool by design; this makes the shape readable at
    # the moment of the miss instead of costing a re-run.
    def __missing__(self, __vis_k__):
        raise KeyError(__vis_key_hint__(self, __vis_k__))


class __VisResult__(__VisDict__):
    # A __VisDict__ that is a TOOL RESULT. `isinstance(x, __VisResult__)` is the
    # robust, UNFORGEABLE origin marker: a model can only build PLAIN dicts (even
    # one with an 'op' key is a plain dict, never a __VisResult__), so capture never
    # relies on the 'op' key alone. 'op' stays a normal key (the origin, for render).
    # It IS a dict, so it's invisible to the model — json/mutation/isinstance work.
    pass


class __VisResultList__(list):
    # A native tool result whose TOP-LEVEL shape is a LIST (patch / struct_patch /
    # write return one row per file; some tools return a list of hits). It stays a
    # REAL list — index / iterate / len / json.dumps / {**_}-free code all behave —
    # but ALSO answers the dict probes (.get/.keys/.items/.values) so a uniform
    # `for _id, res in ntr.items(): res.get('op')` sweep NEVER trips on it. A list has
    # no top-level 'op', so .get returns the default and each row stays reachable by
    # index (res[0]['op']).
    def get(self, __k__, __d__=None):
        return __d__

    def keys(self):
        return []

    def items(self):
        return []

    def values(self):
        return []


class __VisResultStr__(str):
    # A native tool result that is a bare STRING (a tool returning plain text). Still a
    # real str, but answers the same dict probes, so `.get('op')` yields None instead of
    # blowing up with a `'str' object has no attribute 'get'` when a mixed ntr sweep hits
    # it. .keys()/.items()/.values() are empty — a string has no fields.
    def get(self, __k__, __d__=None):
        return __d__

    def keys(self):
        return []

    def items(self):
        return []

    def values(self):
        return []


def __vis_as_result__(__vis_v__):
    # Normalize a STORED native result (ntr[id]) so EVERY value answers the dict probes
    # (.get/.keys/.items/.values) — the shape the model reaches for when it iterates the
    # store. A dict passes through untouched (a tool-result dict is already a
    # __VisResult__). A top-level list/tuple/str is re-typed to a probeable subclass that
    # KEEPS its native list/str behavior, so `res.get('op')` is safe on the whole set
    # without an isinstance guard. Rare scalars (int/float/None/bytes) pass through.
    if isinstance(__vis_v__, dict):
        return __vis_v__
    if isinstance(__vis_v__, (__VisResultList__, __VisResultStr__)):
        return __vis_v__
    if isinstance(__vis_v__, (list, tuple)):
        return __VisResultList__(__vis_v__)
    if isinstance(__vis_v__, str):
        return __VisResultStr__(__vis_v__)
    return __vis_v__


try:
    import polyglot as __vis_polyglot__

    __vis_Foreign__ = __vis_polyglot__.ForeignObject

    def __vis_is_foreign__(x):
        # A host/polyglot proxy (ProxyHashMap/ProxyArray/ForeignDict/…) that
        # crossed the Clojure->Python boundary. NATIVE python values (dict,
        # list, set, tuple, a user object) are NEVER a ForeignObject.
        return isinstance(x, __vis_Foreign__)
except Exception:

    def __vis_is_foreign__(x):
        # Fallback (no `polyglot` module, e.g. non-GraalPy): approximate the
        # old allowlist — treat anything outside real-python primitives as a
        # proxy so tool results still rebuild.
        return not (
            type(x) in (dict, list, str, bytes, int, float, bool)
            or isinstance(x, __VisDict__)
        )


def __vis_pyify__(x):
    # Tool results cross the host boundary as ProxyHashMap/ProxyArray. GraalPy lets
    # you subscript / iterate / .get them, but isinstance(_, dict), {**_},
    # json.dumps(_), dict(_) and type(_) all see a FOREIGN object — NOT a real
    # dict — a frequent source of friction. Rebuild proxies into REAL python
    # dict/list ONCE (at settle) so the model composes on true dicts. A HOST proxy
    # carrying 'op' is a tool result → mark its type __VisResult__. Order is
    # preserved (source is an ordered LinkedHashMap; comprehensions keep it).
    #
    # ONLY foreign proxies are rebuilt. A value the model itself built — set /
    # frozenset / tuple / defaultdict / Counter / any user object — is ALREADY
    # native python and passes through UNTOUCHED. (Blindly rebuilding by an
    # allowlist silently downgraded set/tuple/frozenset -> list and dict
    # subclasses -> dict, so `s = set(); s.add(1)` blew up with the
    # 'list' object has no attribute 'add' error.)
    try:
        if x is None or type(x).__name__ in ("NoneType", "ForeignNone"):
            return None
    except BaseException:
        # A RAW host null (not even wrapped as ForeignNone): every interop touch
        # on it - including type(x) - raises Truffle's "Null receiver values are
        # not supported by libraries". Treat it as python None.
        return None
    if not __vis_is_foreign__(x):
        return x
    if hasattr(x, "keys"):
        try:
            d = {__k__: __vis_pyify__(__v__) for __k__, __v__ in x.items()}
        except Exception:
            # NEVER hand back the RAW proxy: a proxy read of a key it does not have
            # yields a HOST NULL, and the next touch (print, slice, len) dies with
            # Truffle's null-receiver NPE instead of a normal KeyError. Rebuild
            # key-by-key so ONE hostile value degrades to None, not the whole map.
            d = {}
            try:
                for __k__ in list(x.keys()):
                    try:
                        __vis_v2__ = __vis_pyify__(x[__k__])
                    except Exception:
                        __vis_v2__ = None
                    try:
                        d[__k__] = __vis_v2__
                    except Exception:
                        pass
            except Exception:
                d = {}
        return __VisResult__(d) if "op" in d else __VisDict__(d)
    try:
        return [__vis_pyify__(__e__) for __e__ in x]
    except Exception:
        return x


def __vis_settle_gather__(v):
    # Normal gather uses the host's bounded worker pool and aggregated failure
    # contract. `return_exceptions=True` settles each slot in guest Python so a
    # native exception keeps its exact Python type instead of crossing the
    # polyglot boundary as a host exception. This uncommon diagnostic mode is
    # intentionally serial; ordinary gather remains concurrent.
    try:
        if v.return_exceptions:
            out = []
            for aw in v.aws:
                failure = None
                try:
                    out.append(__vis_settle__(aw))
                except BaseException as exc:
                    failure = exc
                if failure is not None:
                    # Cleared OUTSIDE the handler on purpose: while an exception is
                    # still being handled the interpreter re-attaches its traceback,
                    # so a returned failure would pin this settle frame (and every
                    # awaitable reachable from it) for the caller's whole lifetime.
                    out.append(__vis_clean_exception__(failure))
                    failure = None
            return out
        thunks = [(lambda a=a: __vis_settle__(a)) for a in v.aws]
        return __vis_pyify__(__vis_par__(thunks))
    except BaseException:
        # The host cancels outstanding futures, but user-retained guest Tasks would
        # otherwise keep coroutine frames after a sibling fails. Dispose every guest
        # awaitable before dropping gather's own references; this also clears deferred
        # calls that never started, including their host callable and payload graph.
        for aw in v.aws:
            try:
                __vis_dispose_awaitable__(aw)
            except BaseException:
                pass
        raise
    finally:
        # A completed gather must not retain coroutine frames/tool arguments.
        v.aws.clear()


# A plain generator is NOT a coroutine: `rows = (r for r in data)` must stay a
# lazy generator, exactly like real python. It only LOOKS awaitable because it
# has `.send`, and auto-settling used to DRIVE it to exhaustion and bind None.
__vis_gen_type__ = __import__("types").GeneratorType


def __vis_settle__(v):
    if isinstance(v, __vis_Call__):
        # TOP-LEVEL tool result: re-type a list/str payload to the probeable
        # subclass, exactly as a stored ntr[...] read does. Without this a
        # `patch`/`write`/`struct_patch` return was a PLAIN list, so the documented
        # uniform `res.get('op')` probe blew up with `'list' object has no attribute
        # 'get'` and the print-capture below could not recognise it as a result.
        return __vis_as_result__(__vis_pyify__(__vis_exec_call__(v)))
    if isinstance(v, __vis_Gather__):
        return __vis_settle_gather__(v)
    if hasattr(v, "__await__") or (
        hasattr(v, "send") and not isinstance(v, __vis_gen_type__)
    ):
        return __vis_pyify__(__vis_drive__(v))
    return __vis_pyify__(v)


def __vis_settle_binding__(name):
    g = globals()
    g[name] = __vis_settle__(g[name])
    return g[name]


def __vis_drive__(coro):
    it = coro.__await__() if hasattr(coro, "__await__") else coro
    send = None
    while True:
        try:
            y = it.send(send)
        except StopIteration as e:
            return e.value
        try:
            # PYIFY, exactly like the direct `__vis_settle__` path (see above): the
            # value sent back into the coroutine is what `x = await tool()` binds,
            # so it must be a REAL python value. Handing back a raw host proxy - or
            # a host NULL for a tool that returned nil - made the next interop touch
            # inside the coroutine die with Truffle's null-receiver NPE
            # (Null receiver values are not supported by libraries) instead of a
            # normal python error.
            if isinstance(y, __vis_Call__):
                send = __vis_as_result__(__vis_pyify__(__vis_exec_call__(y)))
            elif isinstance(y, __vis_Gather__):
                send = __vis_settle_gather__(y)
            else:
                send = y
        except BaseException as __vis_exc__:
            # The tool/gather call RAISED. Hand the exception to the awaitable via
            # the next send so it re-raises at the coroutine's OWN await point: an
            # in-block `try/except` can then catch it, and if uncaught it simply
            # propagates out of the driver just as it did before.
            send = __vis_Raise__(__vis_wrap_tool_exc__(__vis_exc__))


def __vis_error_pos__(e):
    # Deepest '<prog>' (user-code) traceback frame -> (line, col, end_col). The
    # async trampoline (__vis_drive__) unwinds the guest stack, so a GraalPy
    # PolyglotException.getPolyglotStackTrace() LOSES these frames; the Python
    # __traceback__ is the only place the failing user-code position survives.
    # col/end_col are 0-based (co_positions), None when column info is absent.
    tb = getattr(e, "__traceback__", None)
    line = None
    col = None
    end_col = None
    while tb is not None:
        f = tb.tb_frame
        if f.f_code.co_filename == "<prog>":
            line = tb.tb_lineno
            col = None
            end_col = None
            try:
                p = list(f.f_code.co_positions())[f.f_lasti // 2]
                if p[2] is not None:
                    col = p[2]
                    end_col = p[3]
            except Exception:
                pass
        tb = tb.tb_next
    return None if line is None else (line, col, end_col)


def __vis_err_pos_now__():
    # HOST-CALLED, right after a block failed: compute the failing <prog>
    # position from the exception stashed by `__vis_run_async__`, then release
    # it (a traceback pins frames). This deliberately does NOT run inside the
    # guest `except`: walking traceback frames touches `tb_frame`/`f_code`, and
    # once GraalPy has COMPILED the driver those accesses can raise an INTERNAL
    # Truffle `NullPointerException: Null receiver values are not supported by
    # libraries` that NO guest `except` can catch - it would replace the model's
    # real error at the host boundary (every uncaught error in a warm session
    # became an opaque host-null fault). Called from the host's PolyglotException
    # handler the same fault is catchable there, and costs only the caret.
    g = globals()
    e = g.get("__vis_err_obj__")
    g["__vis_err_obj__"] = None
    if e is None:
        return g.get("__vis_err_pos__")
    pos = __vis_error_pos__(e)
    g["__vis_err_pos__"] = pos
    return pos


class CancelledError(BaseException):
    pass


class InvalidStateError(Exception):
    pass


class __vis_Sleep__:
    # A real blocking sleep wrapped as an awaitable. There is deliberately no
    # selector/event-loop thread. Under gather it runs on the host's bounded,
    # self-reclaiming PLATFORM pool, so a Graal polyglot call cannot pin virtual
    # carriers or grow an unbounded virtual-thread scheduler.
    __slots__ = ("delay", "result")

    def __init__(self, delay, result=None):
        self.delay = float(delay)
        self.result = result

    def __await__(self):
        __vis_time__.sleep(max(0.0, self.delay))
        result = self.result
        # Like a completed coroutine frame, a retained sleep awaitable must not keep
        # an arbitrary result payload alive after handing it to its caller.
        self.delay = 0.0
        self.result = None
        if False:
            yield
        return result


def __vis_clean_exception__(exc):
    # Stored failures must not retain completed coroutine/driver frames through
    # traceback, context, or cause links. Clearing those attributes on the RAISED
    # object is not reliable here: GraalPy materializes `__traceback__` lazily from
    # the underlying host exception, so it can reappear after the handler unwinds.
    # Store a semantic COPY instead - same type, args and message, no frames.
    clean = __vis_clone_exception__(__vis_wrap_tool_exc__(exc))
    for attr in ("__traceback__", "__context__", "__cause__"):
        try:
            setattr(clean, attr, None)
        except BaseException:
            pass
    return clean


def __vis_clone_exception__(exc):
    # Raising the object stored on a Task would attach a fresh traceback to that same
    # retained object. Raise a semantic copy while `_exception` remains frame-free.
    if isinstance(exc, __vis_ToolError__):
        return __vis_ToolError__(exc.__vis_orig__, str(exc))
    try:
        return type(exc)(*getattr(exc, "args", (str(exc),)))
    except BaseException:
        return RuntimeError(str(exc))


class __vis_Task__:
    # A lazy Task-compatible awaitable. It intentionally has NO global task
    # registry or scheduler thread. Completion/cancellation clears the coroutine
    # reference, preventing finished frames and tool arguments from accumulating
    # in a long-lived sandbox Context.
    __slots__ = ("_aw", "_done", "_cancelled", "_result", "_exception", "_name")

    def __init__(self, aw, name=None):
        self._aw = aw
        self._done = False
        self._cancelled = False
        self._result = None
        self._exception = None
        self._name = name

    def __await__(self):
        if self._cancelled:
            raise CancelledError()
        if not self._done:
            try:
                self._result = yield from __vis_awaitable__(self._aw).__await__()
            except BaseException as exc:
                self._exception = exc
            finally:
                self._done = True
                self._aw = None
            if self._exception is not None:
                # Cleaned only AFTER the handler has exited: inside `except` the
                # interpreter re-attaches `__traceback__` on unwind, which would keep
                # the finished coroutine/driver frames alive on a retained Task.
                self._exception = __vis_clean_exception__(self._exception)
        if self._cancelled:
            raise CancelledError()
        if self._exception is not None:
            raise __vis_clone_exception__(self._exception) from None
        return self._result

    def cancel(self, msg=None):
        if self._done:
            return False
        self._cancelled = True
        self._done = True
        aw = self._aw
        self._aw = None
        if aw is not self:
            __vis_dispose_awaitable__(aw)
        return True

    def cancelled(self):
        return self._cancelled

    def done(self):
        return self._done

    def result(self):
        if not self._done:
            raise InvalidStateError("Result is not ready.")
        if self._cancelled:
            raise CancelledError()
        if self._exception is not None:
            raise __vis_clone_exception__(self._exception) from None
        return self._result

    def exception(self):
        if not self._done:
            raise InvalidStateError("Exception is not set.")
        if self._cancelled:
            raise CancelledError()
        return self._exception

    def get_name(self):
        return self._name or "Task"

    def set_name(self, name):
        self._name = str(name)

    def get_coro(self):
        return self._aw


def __vis_dispose_awaitable__(aw):
    # Idempotent, recursive disposal for work abandoned before settlement. There is
    # deliberately no registry: ownership follows only explicit Task/Gather links.
    if aw is None:
        return
    if isinstance(aw, __vis_Task__):
        if not aw.done():
            aw.cancel()
        return
    if isinstance(aw, __vis_Call__):
        if not aw.ran:
            aw.failed = True
            aw.ran = True
            aw.res = None
            aw.fn = None
            aw.a = ()
            aw.k = {}
        return
    if isinstance(aw, __vis_Gather__):
        for child in list(aw.aws):
            __vis_dispose_awaitable__(child)
        aw.aws.clear()
        return
    try:
        if hasattr(aw, "close"):
            aw.close()
    except BaseException:
        pass


class __vis_TaskGroup__:
    __slots__ = ("_tasks", "_entered")

    def __init__(self):
        self._tasks = []
        self._entered = False

    async def __aenter__(self):
        self._entered = True
        return self

    def create_task(self, coro, *, name=None, context=None):
        if not self._entered:
            raise RuntimeError("TaskGroup has not been entered")
        task = __vis_Task__(coro, name)
        self._tasks.append(task)
        return task

    async def __aexit__(self, typ, val, tb):
        try:
            if typ is not None:
                for task in self._tasks:
                    task.cancel()
                return False
            if self._tasks:
                await gather(*self._tasks)
            return False
        finally:
            self._tasks.clear()
            self._entered = False


def __vis_create_task__(coro, *, name=None, context=None):
    return coro if isinstance(coro, __vis_Task__) else __vis_Task__(coro, name)


async def __vis_wait_for__(aw, timeout):
    # No hidden timer/event-loop thread. Zero/negative deadlines cancel before
    # work starts; positive deadlines are checked cooperatively after each
    # awaitable completes (blocking host tools remain governed by Vis turn/eval
    # cancellation, which interrupts and cancels every gather child).
    task = __vis_create_task__(aw)
    if timeout is not None and float(timeout) <= 0:
        task.cancel()
        raise TimeoutError()
    started = __vis_time__.monotonic()
    result = await task
    if timeout is not None and __vis_time__.monotonic() - started > float(timeout):
        raise TimeoutError()
    return result


async def __vis_wait__(aws, *, timeout=None, return_when="ALL_COMPLETED"):
    tasks = {__vis_create_task__(aw) for aw in aws}
    if timeout is not None and float(timeout) <= 0:
        return set(), tasks
    if tasks:
        await gather(*tasks, return_exceptions=True)
    return tasks, set()


def __vis_to_thread__(func, /, *args, **kwargs):
    # The deferred call is dispatched by gather on the same bounded platform
    # executor as tools; it never creates a guest thread or a per-call executor.
    return __vis_Call__(func, args, kwargs, getattr(func, "__name__", "to_thread"))


def __vis_deferred__(realfn, nm="tool"):
    def __vis_tool__(*a, **k):
        return __vis_Call__(realfn, a, k, nm)

    __vis_tool__.__name__ = nm
    return __vis_tool__


class __vis_asyncio__:
    # Practical asyncio compatibility for Vis' coroutine trampoline. This is NOT
    # CPython's socket/select event loop: it owns no loop thread, timer thread,
    # task registry, or executor. Concurrent work is delegated only to the host's
    # bounded, self-reclaiming platform pool.
    CancelledError = CancelledError
    InvalidStateError = InvalidStateError
    TimeoutError = TimeoutError
    Task = __vis_Task__
    TaskGroup = __vis_TaskGroup__
    ALL_COMPLETED = "ALL_COMPLETED"
    FIRST_COMPLETED = "FIRST_COMPLETED"
    FIRST_EXCEPTION = "FIRST_EXCEPTION"

    @staticmethod
    def run(coro, *, debug=None):
        return __vis_drive__(coro)

    @staticmethod
    def run_until_complete(coro):
        return __vis_drive__(coro)

    @staticmethod
    def gather(*aws, return_exceptions=False):
        return gather(*aws, return_exceptions=return_exceptions)

    @staticmethod
    def create_task(coro, *, name=None, context=None):
        return __vis_create_task__(coro, name=name, context=context)

    @staticmethod
    def ensure_future(coro, *, loop=None):
        return __vis_create_task__(coro)

    @staticmethod
    def get_event_loop():
        return __vis_asyncio__

    @staticmethod
    def get_running_loop():
        return __vis_asyncio__

    @staticmethod
    def new_event_loop():
        return __vis_asyncio__

    @staticmethod
    def set_event_loop(*a, **k):
        return None

    @staticmethod
    def sleep(delay, result=None):
        return __vis_Sleep__(delay, result)

    @staticmethod
    def iscoroutine(v):
        return hasattr(v, "send") or hasattr(v, "__await__")

    @staticmethod
    def isfuture(v):
        return isinstance(v, __vis_Task__)

    @staticmethod
    def current_task(loop=None):
        return None

    @staticmethod
    def all_tasks(loop=None):
        return set()

    @staticmethod
    def shield(aw):
        return __vis_create_task__(aw)

    @staticmethod
    def wait_for(aw, timeout):
        return __vis_wait_for__(aw, timeout)

    @staticmethod
    def wait(aws, *, timeout=None, return_when="ALL_COMPLETED"):
        return __vis_wait__(aws, timeout=timeout, return_when=return_when)

    @staticmethod
    def to_thread(func, /, *args, **kwargs):
        return __vis_to_thread__(func, *args, **kwargs)

    @staticmethod
    def iscoroutinefunction(fn):
        return bool(getattr(getattr(fn, "__code__", None), "co_flags", 0) & 0x80)


asyncio = __vis_asyncio__

__vis_try_stmts__ = tuple(
    __vis_t__
    for __vis_t__ in (
        getattr(__vis_ast__, "Try", None),
        getattr(__vis_ast__, "TryStar", None),
    )
    if __vis_t__ is not None
)
__vis_match_stmt__ = getattr(__vis_ast__, "Match", None)

__vis_scope_nodes__ = tuple(
    __vis_t__
    for __vis_t__ in (
        getattr(__vis_ast__, "FunctionDef", None),
        getattr(__vis_ast__, "AsyncFunctionDef", None),
        getattr(__vis_ast__, "ClassDef", None),
        getattr(__vis_ast__, "Lambda", None),
    )
    if __vis_t__ is not None
)
__vis_type_alias__ = getattr(__vis_ast__, "TypeAlias", None)
__vis_named_expr__ = getattr(__vis_ast__, "NamedExpr", None)


def __vis_assigned_names__(body):
    names = []
    seen = set()

    def add(n):
        # `from x import *` yields the pseudo-name `*`; a `global *` would be a
        # compile error, so only real identifiers ever reach the global list.
        if isinstance(n, str) and n.isidentifier() and n not in seen:
            seen.add(n)
            names.append(n)

    def add_target(t):
        if t is None:
            return
        for nn in __vis_ast__.walk(t):
            if isinstance(nn, __vis_ast__.Name):
                add(nn.id)

    def add_pattern(p):
        # `case [a, *rest]` / `case {...., **rest}` / `case X() as hit` all BIND,
        # exactly like an assignment target.
        if p is None:
            return
        for nn in __vis_ast__.walk(p):
            add(getattr(nn, "name", None))
            add(getattr(nn, "rest", None))

    def add_walrus(node):
        # `(m := ...)` binds in the ENCLOSING scope wherever it appears: an
        # `if`/`while` test, a call argument, a comprehension element. Nested
        # def/lambda/class bodies are separate scopes and are never entered.
        stack = [node]
        while stack:
            nn = stack.pop()
            if __vis_named_expr__ is not None and isinstance(nn, __vis_named_expr__):
                add_target(nn.target)
            for ch in __vis_ast__.iter_child_nodes(nn):
                if not isinstance(ch, __vis_scope_nodes__):
                    stack.append(ch)

    def walk_stmts(stmts):
        # MODULE SCOPE is NOT just the top-level statement list: `if` / `while` /
        # `for` / `with` / `try` / `match` bodies execute in the SAME scope, so a
        # name bound inside one (`async with httpx.AsyncClient() as c:` then
        # `hk = json.loads(t)`) is a module global in real Python and must be
        # declared `global` here too. Otherwise it dies with this block's
        # `__vis_main__` frame and the NEXT block greets it with a NameError.
        for node in stmts:
            add_walrus(node)
            if isinstance(node, __vis_ast__.Assign):
                for t in node.targets:
                    add_target(t)
            elif isinstance(node, (__vis_ast__.AnnAssign, __vis_ast__.AugAssign)):
                add_target(node.target)
            elif isinstance(node, __vis_ast__.Delete):
                # `del x` on a module global must delete the GLOBAL; without this
                # the wrapper treats x as a frame local and raises
                # UnboundLocalError on a name that is plainly there.
                for t in node.targets:
                    if isinstance(t, __vis_ast__.Name):
                        add(t.id)
            elif isinstance(
                node,
                (
                    __vis_ast__.FunctionDef,
                    __vis_ast__.AsyncFunctionDef,
                    __vis_ast__.ClassDef,
                ),
            ):
                # A def/class binds its NAME in this scope; its BODY is another
                # scope entirely, so we never descend into it.
                add(node.name)
            elif __vis_type_alias__ is not None and isinstance(
                node, __vis_type_alias__
            ):
                add_target(node.name)
            elif isinstance(node, (__vis_ast__.Import, __vis_ast__.ImportFrom)):
                for al in node.names:
                    add((al.asname or al.name).split(".")[0])
            elif isinstance(node, __vis_ast__.Global):
                for __vis_gn__ in node.names:
                    add(__vis_gn__)
            elif isinstance(node, (__vis_ast__.If, __vis_ast__.While)):
                walk_stmts(node.body)
                walk_stmts(node.orelse)
            elif isinstance(node, (__vis_ast__.For, __vis_ast__.AsyncFor)):
                add_target(node.target)
                walk_stmts(node.body)
                walk_stmts(node.orelse)
            elif isinstance(node, (__vis_ast__.With, __vis_ast__.AsyncWith)):
                for __vis_it__ in node.items:
                    add_target(__vis_it__.optional_vars)
                walk_stmts(node.body)
            elif __vis_try_stmts__ and isinstance(node, __vis_try_stmts__):
                walk_stmts(node.body)
                walk_stmts(node.orelse)
                walk_stmts(node.finalbody)
                for __vis_h__ in node.handlers:
                    add(__vis_h__.name)
                    walk_stmts(__vis_h__.body)
            elif __vis_match_stmt__ is not None and isinstance(
                node, __vis_match_stmt__
            ):
                for __vis_c__ in node.cases:
                    add_pattern(__vis_c__.pattern)
                    walk_stmts(__vis_c__.body)

    # `for` / `with` / `except` / `case` TARGETS are bindings too: real module
    # scope keeps `for line in ...` and `with open(p) as fh` alive after the
    # statement, so they are declared global as well. Clobbering a TOOL is still
    # impossible: a protected name lands in `__vis_shadow__` below and stays
    # block-local, so `with open(p) as patch:` shadows `patch` only for this
    # block.
    walk_stmts(body)
    return names


def __vis_star_import__(module, level=0):
    # `from mod import *` is a SyntaxError inside a function, and EVERY block is
    # wrapped in `async def __vis_main__`. GraalPy raises that at compile time on
    # an AST-built module with no source text, which the host then cannot even
    # render (a bare UnsupportedOperationException). So the star import is
    # rewritten to this call, which does what module scope would: bind the
    # module's public names (or its `__all__`) straight into globals. A PROTECTED
    # tool name is never overwritten.
    g = globals()
    mod = __import__(module or "", g, g, ["*"], level)
    prot = set(g.get("__vis_protected_names__") or [])
    exported = getattr(mod, "__all__", None)
    if exported is None:
        exported = [k for k in dir(mod) if not k.startswith("_")]
    for k in exported:
        if k in prot:
            continue
        try:
            g[k] = getattr(mod, k)
        except AttributeError:
            pass
    return None


class __vis_StarImportFix__(__vis_ast__.NodeTransformer):
    # Replace every `from mod import *` (top level or nested in an if/try) with
    # `__vis_star_import__('mod', level)`.
    def visit_ImportFrom(self, node):
        if any(al.name == "*" for al in node.names):
            call = __vis_ast__.Call(
                func=__vis_ast__.Name(id="__vis_star_import__", ctx=__vis_ast__.Load()),
                args=[
                    __vis_ast__.Constant(value=node.module or ""),
                    __vis_ast__.Constant(value=node.level or 0),
                ],
                keywords=[],
            )
            return __vis_ast__.Expr(value=call)
        return node


__vis_future_mod__ = __import__("__future__")


def __vis_future_flags__(tree):
    # `from __future__ import annotations` is a MODULE-level compile directive and
    # must be the first statement of a FILE. Every block is wrapped in
    # `async def __vis_main__`, where the very same line is a hard SyntaxError
    # ("from __future__ imports must occur at the beginning of the file") — even
    # though the block IS the top of its module. So the future imports are lifted
    # out of the body and their compiler flags handed to `compile()` instead,
    # which is exactly what the directive means.
    flags = 0
    kept = []
    for node in tree.body:
        if (
            isinstance(node, __vis_ast__.ImportFrom)
            and node.module == "__future__"
            and not node.level
            and not any(al.name == "*" for al in node.names)
        ):
            for al in node.names:
                feat = getattr(__vis_future_mod__, al.name, None)
                if getattr(feat, "compiler_flag", None) is None:
                    raise SyntaxError(
                        "future feature " + str(al.name) + " is not defined"
                    )
                flags |= feat.compiler_flag
            continue
        kept.append(node)
    tree.body = kept
    return flags


def __vis_syntax_error__(msg, node, src):
    # A SyntaxError the HOST can actually RENDER. The boundary reads lineno/offset/
    # text off the exception object, so a raise from this preamble without them
    # reports a line number in code the user never wrote (`<prog>, line 1070`).
    ln = getattr(node, "lineno", None)
    col = getattr(node, "col_offset", None)
    txt = None
    if isinstance(ln, int) and ln >= 1:
        lines = src.splitlines()
        if ln <= len(lines):
            txt = lines[ln - 1]
    return SyntaxError(
        msg, ("<prog>", ln, (col + 1) if isinstance(col, int) else None, txt)
    )


def __vis_check_module_scope__(tree, src):
    # The `async def __vis_main__` wrapper would silently ACCEPT two statements a
    # real module rejects: a top-level `return` would just stop the block halfway
    # (the rest of the code never runs, no error), and a top-level `yield` would
    # turn the wrapper into an async generator whose body never executes at all —
    # reported, if at all, as a baffling "'return' with value in async generator".
    # Report what Python reports.
    # A `def`/`class`/`lambda` body is a scope of its own: its `return`/`yield` are
    # perfectly legal and are never inspected here.
    stack = [
        __vis_n__
        for __vis_n__ in tree.body
        if not isinstance(__vis_n__, __vis_scope_nodes__)
    ]
    while stack:
        node = stack.pop()
        if isinstance(node, __vis_ast__.Return):
            raise __vis_syntax_error__("'return' outside function", node, src)
        if isinstance(node, (__vis_ast__.Yield, __vis_ast__.YieldFrom)):
            raise __vis_syntax_error__("'yield' outside function", node, src)
        if isinstance(node, __vis_ast__.Nonlocal):
            raise __vis_syntax_error__(
                "nonlocal declaration not allowed at module level", node, src
            )
        for ch in __vis_ast__.iter_child_nodes(node):
            if not isinstance(ch, __vis_scope_nodes__):
                stack.append(ch)


def __vis_check_compile_traps__(tree, src):
    # Two ordinary CPython SyntaxErrors are UNCATCHABLE host faults on GraalPy:
    # compiling `await` inside a lambda dies with a bare Java NullPointerException
    # (null sourceRange), and a bare starred assignment target with
    # `UnsupportedOperationException: StoreVisitor: Starred`. Neither is a Python
    # exception, so `except SyntaxError` around compile() cannot see them and the
    # whole block is reported as an engine fault. Reject them up front, with the
    # message and position CPython gives. Unlike the module-scope pass this walks
    # EVERY scope: a lambda nested in a def is just as fatal.
    star = "starred assignment target must be in a list or tuple"
    for node in __vis_ast__.walk(tree):
        if isinstance(node, __vis_ast__.Lambda):
            for sub in __vis_ast__.walk(node):
                if isinstance(sub, __vis_ast__.Await):
                    raise __vis_syntax_error__(
                        "'await' outside async function", sub, src
                    )
        targets = ()
        if isinstance(node, __vis_ast__.Assign):
            targets = node.targets
        elif isinstance(node, (__vis_ast__.AugAssign, __vis_ast__.AnnAssign)):
            targets = (node.target,)
        elif isinstance(node, (__vis_ast__.For, __vis_ast__.AsyncFor)):
            targets = (node.target,)
        elif isinstance(node, __vis_ast__.comprehension):
            targets = (node.target,)
        elif isinstance(node, __vis_ast__.withitem):
            targets = (node.optional_vars,) if node.optional_vars is not None else ()
        elif isinstance(node, __vis_ast__.Delete):
            for t in node.targets:
                if isinstance(t, __vis_ast__.Starred):
                    raise __vis_syntax_error__("cannot delete starred", t, src)
        for t in targets:
            if isinstance(t, __vis_ast__.Starred):
                raise __vis_syntax_error__(star, t, src)


def __vis_annotate__(name, value):
    # Module scope records `x: int = 1` in the module's `__annotations__` (created
    # on first use); the binding itself is a plain assignment.
    g = globals()
    ann = g.get("__annotations__")
    if not isinstance(ann, dict):
        ann = {}
        g["__annotations__"] = ann
    ann[name] = value
    return None


class __vis_AnnFix__(__vis_ast__.NodeTransformer):
    # `x: int = 1` at module scope is a STORE plus an `__annotations__` entry. In
    # the wrapper it collides with the `global x` the block needs: CPython refuses
    # "annotated name 'x' can't be global", so a single `nums: list[int] = []`
    # killed the whole block. Rewrite module-scope annotated assignments to the
    # plain assignment plus the annotations bookkeeping; a valueless `x: int`
    # binds NOTHING, exactly like a real module. `def`/`class` bodies are other
    # scopes with their own annotation rules and are left completely alone.
    def __init__(self, lazy):
        self.lazy = lazy

    def visit_FunctionDef(self, node):
        return node

    visit_AsyncFunctionDef = visit_FunctionDef
    visit_ClassDef = visit_FunctionDef
    visit_Lambda = visit_FunctionDef

    def visit_AnnAssign(self, node):
        self.generic_visit(node)
        if not isinstance(node.target, __vis_ast__.Name):
            return node
        # Under `from __future__ import annotations` the annotation is never
        # evaluated — it is stored as its own source text.
        ann = (
            __vis_ast__.Constant(value=__vis_ast__.unparse(node.annotation))
            if self.lazy
            else node.annotation
        )
        record = __vis_ast__.Expr(
            value=__vis_ast__.Call(
                func=__vis_ast__.Name(id="__vis_annotate__", ctx=__vis_ast__.Load()),
                args=[__vis_ast__.Constant(value=node.target.id), ann],
                keywords=[],
            )
        )
        if node.value is None:
            return record
        return [__vis_ast__.Assign(targets=[node.target], value=node.value), record]


__vis_builtins_mod__ = __import__("builtins")
__vis_sysmod__ = __import__("sys")
__vis_real_exec__ = __vis_builtins_mod__.exec
__vis_real_vars__ = __vis_builtins_mod__.vars
# Frame-relative when called from a preamble function — whose globals ARE the
# session globals `g`, so this hands back exactly that dict.
__vis_globals__ = __vis_builtins_mod__.globals


def __vis_caller_frame__(depth):
    # The frame `depth` levels above the shim, or None when frame introspection
    # is unavailable.
    try:
        return __vis_sysmod__._getframe(depth + 1)
    except Exception:
        return None


def __vis_is_block_frame__(frame):
    return frame is not None and frame.f_code.co_name == "__vis_main__"


def exec(source, globals=None, locals=None, /, **kw):
    # MODULE-SCOPE `exec`: at real module level `exec('x = 1')` binds x in the
    # module globals. Every block runs inside `async def __vis_main__`, where the
    # implicit target is a frame-locals dict that is thrown away — so the name
    # vanished and the very next line raised NameError. Only the no-namespace
    # call made DIRECTLY from a block body is redirected; an explicit namespace,
    # and any call from a function the block defined, keeps real semantics.
    if (
        globals is None
        and locals is None
        and __vis_is_block_frame__(__vis_caller_frame__(1))
    ):
        globals = __vis_globals__()
        locals = globals
    return __vis_real_exec__(source, globals, locals, **kw)


def locals():
    # At module level `locals() is globals()` — `'{x}'.format(**locals())` and
    # `locals()['x']` are module idioms. In a block body report globals; inside a
    # real function report that function's own frame.
    frame = __vis_caller_frame__(1)
    if __vis_is_block_frame__(frame):
        return __vis_globals__()
    return frame.f_locals if frame is not None else __vis_globals__()


def vars(*obj):
    if obj:
        return __vis_real_vars__(*obj)
    frame = __vis_caller_frame__(1)
    if __vis_is_block_frame__(frame):
        return __vis_globals__()
    return frame.f_locals if frame is not None else __vis_globals__()


def __vis_strip_protected_imports__(src):
    # Rewrite imports so the sandbox can't break AND the model's habits still
    # work:
    #   • `import asyncio` / `import asyncio as aio`  ->  `aio = __vis_asyncio__`
    #     (our shim; real asyncio + `asyncio.run` trips a NATIVE
    #     `PosixSupportLibrary$UnsupportedPosixFeatureException: socket was
    #     excluded`). The shim routes run/gather/... onto our driver.
    #   • `from asyncio import run, sleep as s`        ->  `run = __vis_asyncio__.run`
    #     ; `s = __vis_asyncio__.sleep`. A name that is ALREADY a protected
    #     builtin (gather) is dropped so the builtin keeps showing through.
    #   • `import socket`                                ->  passthrough. socket is
    #     ALSO auto-imported onto builtins (always present); the module imports
    #     fine even with the network toggle off — only a live connect is gated by
    #     `allowHostSocketAccess`, which raises a clean UnsupportedOperation.
    #   • `import select` / `selectors` / `ssl` ...      ->  dropped (no shim; a
    #     later use is a clean NameError, not a native crash).
    #   • an import binding a tool name (`import doc`)  ->  KEPT; it just shadows
    #     that name for THIS block (the wrapper never declares it `global`).
    # Everything else (json, re, ...) is untouched; the ORIGINAL src is returned
    # when nothing changed (line numbers / formatting preserved).
    prot = set(globals().get("__vis_protected_names__") or [])
    drop = ("select", "selectors", "ssl")

    def bind(name, attr):
        val = __vis_ast__.Name(id="__vis_asyncio__", ctx=__vis_ast__.Load())
        if attr is not None:
            val = __vis_ast__.Attribute(value=val, attr=attr, ctx=__vis_ast__.Load())
        return __vis_ast__.Assign(
            targets=[__vis_ast__.Name(id=name, ctx=__vis_ast__.Store())], value=val
        )

    tree = __vis_ast__.parse(src)
    changed = False
    newbody = []
    for node in tree.body:
        if isinstance(node, __vis_ast__.Import):
            keep = []
            for a in node.names:
                base = a.name.split(".")[0]
                bound = (a.asname or a.name).split(".")[0]
                if base == "asyncio":
                    newbody.append(bind(a.asname or "asyncio", None))
                    changed = True
                elif base in drop:
                    changed = True
                else:
                    keep.append(a)
            if keep:
                node.names = keep
                newbody.append(node)
        elif isinstance(node, __vis_ast__.ImportFrom):
            base = (node.module or "").split(".")[0]
            if base == "asyncio":
                for a in node.names:
                    bound = a.asname or a.name
                    if bound not in prot:  # gather etc. stay the builtin
                        newbody.append(bind(bound, a.name))
                changed = True
            elif base in drop:
                changed = True
            else:
                newbody.append(node)
        else:
            newbody.append(node)
    if not changed:
        return src
    tree.body = newbody
    __vis_ast__.fix_missing_locations(tree)
    return __vis_ast__.unparse(tree)


class __vis_AwaitFix__(__vis_ast__.NodeTransformer):
    # Wrap the operand of every `await EXPR` as `await __vis_awaitable__(EXPR)`
    # so awaiting a value that is NOT a real awaitable (a tool result that
    # already settled — `x = patch(...); await x`) returns the value instead of
    # raising. Visits the WHOLE tree so a nested `await` (inside `print(...)`,
    # an arg, a comprehension) is fixed too; real awaitables are untouched.
    def visit_Await(self, node):
        self.generic_visit(node)
        node.value = __vis_ast__.Call(
            func=__vis_ast__.Name(id="__vis_awaitable__", ctx=__vis_ast__.Load()),
            args=[node.value],
            keywords=[],
        )
        return node


def __vis_pin_runtime__(g):
    # PIN the engine's own names into `builtins`.
    #
    # `globals().clear()`, `del __vis_settle__`, `for k in list(globals()): del
    # globals()[k]` — all legal Python, and CPython keeps the RUNNING block alive
    # through them: a frame captures its builtins at creation, so `print(...)`
    # still resolves after the module dict is emptied. Our rewritten block body
    # calls engine helpers by bare name (`__vis_settle__`, `__vis_Call__` around
    # every deferred tool call), and those lived ONLY in globals — so the very
    # statement that cleared them made the REST OF THE SAME BLOCK die with a
    # nonsense __vis_Call__-not-defined, extension-is-inactive
    # NameError, pointing the model at a tool that was never involved.
    # (Between blocks `ensure-async-runtime!` reinstalls the runtime; this is the
    # mid-block half of the same story.) Mirroring into builtins costs one dict
    # scan per block and gives the helpers exactly `print`'s survival rule.
    import builtins as __vis_b__

    for __n__ in list(g):
        if __n__.startswith("__vis_") or __n__.startswith("__Vis"):
            try:
                setattr(__vis_b__, __n__, g[__n__])
            except Exception:
                pass


def __vis_run_async__(src):
    g = globals()
    __vis_pin_runtime__(g)
    g["__vis_printed_results__"] = []  # per-block reset (real python list, appendable)
    g["__vis_only_results__"] = (
        True  # cleared if the block prints anything that isn't a tool result
    )
    g["__vis_err_pos__"] = (
        None  # deepest <prog> failing position, computed by __vis_err_pos_now__
    )
    g["__vis_err_obj__"] = (
        None  # the raised exception, stashed for that host-driven lookup
    )
    tree = __vis_ast__.parse(src)
    __vis_flags__ = __vis_future_flags__(tree)
    __vis_check_module_scope__(tree, src)
    __vis_check_compile_traps__(tree, src)
    tree = __vis_AwaitFix__().visit(tree)
    tree = __vis_StarImportFix__().visit(tree)
    tree = __vis_AnnFix__(
        bool(__vis_flags__ & __vis_future_mod__.annotations.compiler_flag)
    ).visit(tree)
    __vis_ast__.fix_missing_locations(tree)
    # PRE-SCAN (piggybacks the block parse — zero extra parse cost): collect every
    # literal id read via ntr[...] (or legacy native_tools_results[...]) and PRIME
    # them in ONE batched DB query, so N literal reads never fan out to N fetches.
    # Dynamic keys fall back to a lazy per-key fetch in __getitem__. Guarded: the
    # prime callback is only bound in the full agent context (a bare test context
    # has neither the map nor the callback).
    if "__vis_native_result_prime__" in g and "__vis_native_result_scan__" in g:
        __vis_scan_ids__ = __vis_native_result_scan__(tree)
        if __vis_scan_ids__:
            ntr.__vis_prime__(__vis_scan_ids__)
    assigned = __vis_assigned_names__(tree.body)
    # SHADOWING a bound tool / sandbox name is ALLOWED — but only for THIS block.
    # A protected name assigned here is LEFT OUT of the `global` list, so it
    # becomes a plain `__vis_main__` local (exactly like a `for`/`with` target):
    # `search = re.search(...)` reads naturally inside the block and the
    # persistent callable is still there for the next one. Each shadowed name is
    # pre-seeded from globals, so a READ that precedes the shadowing assignment
    # still sees the tool instead of raising UnboundLocalError.
    __vis_prot__ = set(g.get("__vis_protected_names__") or [])
    __vis_shadow__ = [n for n in assigned if n in __vis_prot__ and n in g]
    assigned = [n for n in assigned if n not in __vis_shadow__]
    body = list(tree.body)

    # AUTO-SETTLE inline, exactly like the sync per-form path: wrap the value of
    # every TOP-LEVEL assignment / bare expression in `__vis_settle__(...)` so a
    # bare deferred tool call (`res = patch(...)`, or a lone `patch(...)`) RUNS
    # in place — later statements (and `print(res)`) then see the real value,
    # not a `__vis_Call__` thunk. settle is identity for plain values and
    # idempotent for thunks already consumed by `await`/`gather`, so wrapping is
    # always safe. Nested calls still need an explicit `await` (we only touch
    # top-level statements, matching the sync contract).
    def __vis_wrap__(v):
        return __vis_ast__.Call(
            func=__vis_ast__.Name(id="__vis_settle__", ctx=__vis_ast__.Load()),
            args=[v],
            keywords=[],
        )

    for __vis_node__ in body:
        if isinstance(__vis_node__, (__vis_ast__.Assign, __vis_ast__.AnnAssign)):
            if __vis_node__.value is not None:
                __vis_node__.value = __vis_wrap__(__vis_node__.value)
        elif isinstance(__vis_node__, __vis_ast__.Expr):
            __vis_node__.value = __vis_wrap__(__vis_node__.value)

    if body and isinstance(body[-1], __vis_ast__.Expr):
        body[-1] = __vis_ast__.Return(value=body[-1].value)
    seed = [
        __vis_ast__.parse(n + " = globals()[" + repr(n) + "]").body[0]
        for n in __vis_shadow__
    ]
    inner = ([__vis_ast__.Global(names=assigned)] if assigned else []) + seed + body
    fn = __vis_ast__.AsyncFunctionDef(
        name="__vis_main__",
        args=__vis_ast__.arguments(
            posonlyargs=[],
            args=[],
            vararg=None,
            kwonlyargs=[],
            kw_defaults=[],
            kwarg=None,
            defaults=[],
        ),
        body=inner,
        decorator_list=[],
        returns=None,
        type_params=[],
    )
    mod = __vis_ast__.Module(body=[fn], type_ignores=[])
    __vis_ast__.fix_missing_locations(mod)
    try:
        __vis_code__ = compile(mod, "<prog>", "exec", __vis_flags__)
    except SyntaxError as __vis_se__:
        # A compile error on the SYNTHESIZED module has no source text, and the
        # host cannot render such a guest exception at all (it dies with a bare
        # UnsupportedOperationException). Re-raise the same message from THIS
        # source so the boundary reports a normal Python error.
        # Keep the ORIGINAL position: the synthesized module keeps every user node's
        # lineno, so `__vis_se__.lineno` is the user's line — dropping it made the
        # boundary report this preamble's line instead.
        __vis_msg__ = getattr(__vis_se__, "msg", None) or str(__vis_se__)
        __vis_ln__ = getattr(__vis_se__, "lineno", None)
        __vis_txt__ = getattr(__vis_se__, "text", None)
        if __vis_txt__ is None and isinstance(__vis_ln__, int) and __vis_ln__ >= 1:
            __vis_lines__ = src.splitlines()
            if __vis_ln__ <= len(__vis_lines__):
                __vis_txt__ = __vis_lines__[__vis_ln__ - 1]
        raise SyntaxError(
            __vis_msg__,
            ("<prog>", __vis_ln__, getattr(__vis_se__, "offset", None), __vis_txt__),
        ) from None
    exec(__vis_code__, g)
    try:
        g["__vis_async_result__"] = __vis_drive__(g["__vis_main__"]())
    except BaseException as __vis_err__:
        # Stash the exception ONLY, then re-raise UNCHANGED. Deriving the failing
        # position here would walk its traceback frames, which on a warm (JIT-ed)
        # interpreter can hit an uncatchable internal Truffle null-receiver NPE and
        # DESTROY this real error. The host asks for the position afterwards via
        # `__vis_err_pos_now__`, where that fault is catchable.
        g["__vis_err_obj__"] = __vis_err__
        raise
    finally:
        # The block is over: everything it wrote through a handle it never closed
        # is on disk now, success or failure alike (GraalPy would otherwise leave
        # the buffer unflushed until an arbitrary later GC).
        __vis_flush_writes__()
        # ... and every descriptor it dropped is handed back, so a block that
        # leaks handles cannot bleed into the next one (or into the next spawn).
        __vis_reclaim_fds__(True)
    return assigned


def __vis_defer_tools__():
    g = globals()
    for __vis_n__ in list(__vis_defer_names__):
        if __vis_n__ in g and callable(g[__vis_n__]):
            g[__vis_n__] = __vis_deferred__(g[__vis_n__], __vis_n__)


def __vis_direct_kwargs__(realfn, nm="verb"):
    # KWARGS for the DIRECT (never-deferred) host verbs — today `session_fold`.
    # Those stay raw foreign ProxyExecutables, which accept POSITIONAL args ONLY,
    # so `session_fold(t, gist='…')` used to die with `__call__() got an
    # unexpected keyword argument` BEFORE any fold validation ran. Fold **kwargs
    # into ONE trailing dict positional — exactly what `__vis_exec_call__` does
    # for the deferred tools — and the Clojure verb unwraps it (`compaction-verbs`),
    # so keyword and positional calls bind identically.
    def __vis_verb__(*a, **k):
        return realfn(*a, dict(k)) if k else realfn(*a)

    __vis_verb__.__name__ = nm
    return __vis_verb__


def __vis_kwargs_direct_tools__():
    g = globals()
    for __vis_n__ in list(__vis_direct_names__):
        if __vis_n__ in g and callable(g[__vis_n__]):
            g[__vis_n__] = __vis_direct_kwargs__(g[__vis_n__], __vis_n__)


# ── echo-diff strip for a printed edit result: a patch/write/struct_patch result
# printed to stdout merely re-describes the bytes the model just authored, so drop
# each file summary's redundant 'diff' for DISPLAY only. The captured original is
# untouched, so the host op-card still renders the full diff.
def __vis_is_file_summary__(__m__):
    return (
        isinstance(__m__, dict)
        and isinstance(__m__.get("path"), str)
        and isinstance(__m__.get("op"), str)
        and "changed" in __m__
    )


def __vis_strip_echo_diff__(__m__):
    return {__k__: __v__ for __k__, __v__ in __m__.items() if __k__ != "diff"}


def __vis_strip_echo_diffs__(__x__):
    if (
        isinstance(__x__, list)
        and __x__
        and all(__vis_is_file_summary__(__e__) for __e__ in __x__)
    ):
        return [__vis_strip_echo_diff__(__e__) for __e__ in __x__]
    if __vis_is_file_summary__(__x__):
        return __vis_strip_echo_diff__(__x__)
    return __x__


# ── print-capture: a printed TOOL RESULT (a dict carrying 'op', stamped by the
# host) is recorded on the side so the host can render ONE op-card per printed
# result. The model's stdout/context is UNCHANGED — we delegate to the real print;
# capture is a pure side-effect. The list is reset per block from Clojure.
__vis_printed_results__ = []
__vis_real_print__ = print


def __vis_print__(*__vis_a__, **__vis_kw__):
    # Pyify args FIRST: a printed tool-result proxy becomes a __VisResult__ (so
    # `print(await rg(...))` is captured even without an intervening assignment) and
    # prints as a clean real dict. Capture by TYPE (isinstance), NOT the 'op' key —
    # a model-built dict with 'op' is a plain dict and is correctly NOT captured.
    # Track whether the block printed ONLY tool results: cards may replace the raw
    # stdout for display ONLY then; otherwise show the full stdout (no text lost).
    # Auto-SETTLE a deferred call/gather handed to print WITHOUT `await` (e.g.
    # `print(rg(...))`): run it and show the real result instead of the loud
    # '<unawaited async tool call …>' repr. Only OUR OWN deferred thunks are
    # settled (never a stray generator/coroutine the model meant to print); every
    # other arg pyifies exactly as before.
    __vis_a__ = tuple(
        __vis_settle__(__a__)
        if isinstance(__a__, (__vis_Call__, __vis_Gather__))
        else __vis_pyify__(__a__)
        for __a__ in __vis_a__
    )
    if __vis_kw__.get("file") is None:
        for __vis_x__ in __vis_a__:
            # A LIST-shaped result (patch / write / struct_patch: one row per file)
            # is a tool result too — `__VisResultList__` is its unforgeable marker.
            # Missing it made a printed edit BOTH card-less and a card-killer: the
            # block no longer counted as results-ONLY, so every OTHER printed card in it
            # was dropped back to raw stdout.
            if isinstance(__vis_x__, (__VisResult__, __VisResultList__)):
                __vis_printed_results__.append(__vis_x__)
            else:
                globals()["__vis_only_results__"] = False
        if not __vis_a__:  # a bare print() (blank line) is not a result
            globals()["__vis_only_results__"] = False
    # DISPLAY strips echo-diffs from a printed edit result (stdout mirrors the model
    # wire); capture above kept the un-stripped originals for the host op-card.
    return __vis_real_print__(
        *tuple(__vis_strip_echo_diffs__(__a__) for __a__ in __vis_a__), **__vis_kw__
    )


print = __vis_print__


# ── ntr / native_tools_results: retrieve a PRIOR native tool's result by its
# provider tool_use id, WITHOUT re-running the tool. `ntr` is the short public
# name; `native_tools_results` remains as a backwards-compatible verbose alias.
# Every native tool call vis persisted (this turn's earlier iterations AND past
# turns) is reachable by the SAME id the model saw on its tool_result. A read is
# a single DB fetch (thaw + rehydrate to the EXACT __VisResult__ dict the fresh
# call returned), then cached in-process.
#
# `__vis_native_result_prime__(ids)` (Clojure) does ONE batched DB query for a
# list of ids → {id: result} (a proxy per hit; misses absent). `__vis_run_async__`
# calls it with the LITERAL ids AST-scanned from the block, so N literal reads cost
# ONE query. `__vis_native_result_fetch__(id)` (Clojure) is the lazy single-id
# fallback for a DYNAMIC key (a variable / comprehension the scan can't see).
# A miss → a clean KeyError, never a crash.
#
# It is ALSO a read-only mapping: `__vis_native_result_ids__()` (Clojure) lists
# every persisted tool_use id in the session (newest first), backing keys() /
# items() / values() / __iter__ / __len__ so the store is BROWSEABLE without
# knowing an id up front. Ids alone are opaque, so `describe()` labels a bounded
# newest-first window from the latest turn with each result's op and salient fields —
# browse by what a result HOLDS, then spend one fetch on the id worth fetching.
class __VisNativeResults__:
    def __init__(self):
        self.__vis_cache__ = {}  # id -> pyified __VisResult__ (already fetched)
        self.__vis_missing__ = set()  # ids proven absent this process (skip re-fetch)

    def __vis_store__(self, __vis_id__, __vis_raw__):
        # Stamp the rehydrated proxy into the SAME __VisResult__ shape a fresh
        # native call yields (a dict carrying 'op' → __VisResult__ via pyify).
        __vis_v__ = __vis_as_result__(__vis_pyify__(__vis_raw__))
        self.__vis_cache__[__vis_id__] = __vis_v__
        return __vis_v__

    def __vis_prime__(self, __vis_ids__):
        # Pre-populate from ONE batched host query. Only ids we have NOT already
        # resolved (cached hit OR proven missing) are queried — a re-read of an
        # id primed by an earlier block hits the in-process cache with NO new DB
        # round-trip. Absent ids are recorded as missing so a later __getitem__
        # raises immediately (no redundant fetch).
        __vis_need__ = [
            i
            for i in __vis_ids__
            if i not in self.__vis_cache__ and i not in self.__vis_missing__
        ]
        if not __vis_need__:
            return
        try:
            __vis_hits__ = __vis_native_result_prime__(__vis_need__)
        except Exception:
            __vis_hits__ = None
        try:
            # A host value that is neither a map nor None (a deferred call proxy
            # settling to None) must never break a browse.
            __vis_hits__ = __vis_hits__ or {}
        except Exception:
            __vis_hits__ = {}
        for __vis_id__ in __vis_need__:
            if __vis_id__ in __vis_hits__ and __vis_hits__[__vis_id__] is not None:
                self.__vis_store__(__vis_id__, __vis_hits__[__vis_id__])
            else:
                self.__vis_missing__.add(__vis_id__)

    def __getitem__(self, __vis_id__):
        if __vis_id__ in self.__vis_cache__:
            return self.__vis_cache__[__vis_id__]
        if __vis_id__ not in self.__vis_missing__:
            # Lazy single-id fetch (dynamic key the pre-scan couldn't see).
            try:
                __vis_raw__ = __vis_native_result_fetch__(__vis_id__)
            except Exception:
                __vis_raw__ = None
            if __vis_raw__ is not None:
                return self.__vis_store__(__vis_id__, __vis_raw__)
            self.__vis_missing__.add(__vis_id__)
        raise KeyError(
            "no native tool result for "
            + repr(__vis_id__)
            + " — that tool_use id is unknown or produced no return (a python_execution "
            "call returns what it print()s, not a stored value). Re-run the tool, or use "
            "the exact tool_use id shown on a prior tool_result."
        )

    def get(self, __vis_id__, __vis_default__=None):
        try:
            return self[__vis_id__]
        except KeyError:
            return __vis_default__

    def __contains__(self, __vis_id__):
        try:
            self[__vis_id__]
            return True
        except KeyError:
            return False

    def __vis_all_ids__(self):
        # Host list of EVERY native tool_use id persisted in this session branch
        # (newest first) so the store is BROWSEABLE. Degrades to the ids already
        # cached in-process when the callback isn't bound (bare test context).
        try:
            __vis_ids__ = __vis_native_result_ids__()
        except Exception:
            __vis_ids__ = None
        if __vis_ids__ is None:
            return list(self.__vis_cache__.keys())
        # De-dupe, preserving host (newest-first) order.
        __vis_seen__ = set()
        __vis_out__ = []
        for __vis_i__ in __vis_ids__:
            if __vis_i__ not in __vis_seen__:
                __vis_seen__.add(__vis_i__)
                __vis_out__.append(__vis_i__)
        return __vis_out__

    def __vis_index__(self):
        # LABELLED newest-first index from the host: [{'id','tool','gist'}, …].
        # Built from the same rows keys() walks, but it thaws NO result payload,
        # so a whole window of opaque ids can be named without one fetch.
        # None when the callback isn't bound (bare test context) → callers fall
        # back to labelling from fetched payloads.
        try:
            # list() SETTLES the host call into a plain list; a callback that is
            # unbound or yields nothing degrades to None, never to a raise.
            __vis_raw__ = __vis_native_result_index__()
            return list(__vis_raw__) if __vis_raw__ is not None else None
        except Exception:
            return None

    def keys(self):
        return self.__vis_all_ids__()

    def __iter__(self):
        return iter(self.__vis_all_ids__())

    def __len__(self):
        return len(self.__vis_all_ids__())

    def items(self):
        __vis_ids__ = self.__vis_all_ids__()
        self.__vis_prime__(__vis_ids__)  # ONE batched fetch for the whole set
        __vis_out__ = []
        for __vis_i__ in __vis_ids__:
            try:
                __vis_out__.append((__vis_i__, self[__vis_i__]))
            except KeyError:
                pass
        return __vis_out__

    def values(self):
        return [__vis_v__ for __vis_k__, __vis_v__ in self.items()]

    # ── Browse by MEANING, not by opaque id. keys() hands back 24-char tool_use
    # ids that say nothing about what they hold, and items()/values() thaw the
    # ENTIRE store to find out. describe() sits between them: ONE batched prime of
    # a bounded newest-first window from the latest turn, each id labelled with its
    # op plus a couple of that result's own salient fields, so a stored result can
    # be CHOSEN before it is fetched and read in full.
    def __vis_gist_of__(self, __vis_v__):
        try:
            if not isinstance(__vis_v__, dict):
                return type(__vis_v__).__name__
            __vis_bits__ = []
            __vis_op__ = __vis_v__.get("op")
            for __vis_k__ in (
                "path",
                "query",
                "cmd",
                "name",
                "target",
                "code",
                "cwd",
                "language",
            ):
                __vis_x__ = __vis_v__.get(__vis_k__)
                if isinstance(__vis_x__, str) and __vis_x__.strip():
                    __vis_s__ = " ".join(__vis_x__.split())
                    if len(__vis_s__) > 48:
                        __vis_s__ = __vis_s__[:47] + "…"
                    __vis_bits__.append(__vis_k__ + "=" + __vis_s__)
                    break
            for __vis_k__ in (
                "hit_count",
                "file_count",
                "line_count",
                "count",
                "total",
                "pass",
                "fail",
                "exit",
                "changed",
                "is_pass",
                "error",
            ):
                if len(__vis_bits__) >= 3:
                    break
                __vis_x__ = __vis_v__.get(__vis_k__)
                if isinstance(__vis_x__, (int, float, bool)):
                    __vis_bits__.append(__vis_k__ + "=" + str(__vis_x__))
            __vis_head__ = [str(__vis_op__)] if __vis_op__ else []
            return " · ".join(__vis_head__ + __vis_bits__) or "result"
        except Exception:
            return "result"

    def describe(self, limit=20, ids=None):
        # ['toolu_01Tc… · grep · session_fold, 6 file names', …]
        __vis_idx__ = self.__vis_index__()
        __vis_lbl__ = {}
        if __vis_idx__ is not None:
            for __vis_e__ in __vis_idx__:
                try:
                    __vis_lbl__[__vis_e__.get("id")] = (
                        " · ".join(
                            [
                                __vis_x__
                                for __vis_x__ in (
                                    __vis_e__.get("tool"),
                                    __vis_e__.get("gist"),
                                )
                                if __vis_x__
                            ]
                        )
                        or "result"
                    )
                except Exception:
                    pass
        if ids is not None:
            __vis_sel__ = [__vis_i__ for __vis_i__ in ids]
        elif __vis_idx__ is not None:
            __vis_sel__ = [__vis_e__.get("id") for __vis_e__ in __vis_idx__][
                : max(0, int(limit))
            ]
        else:
            __vis_sel__ = self.__vis_all_ids__()[: max(0, int(limit))]
        # Only ids the index could NOT label cost a payload — normally none, and
        # those go in ONE batched fetch.
        __vis_need__ = [
            __vis_i__ for __vis_i__ in __vis_sel__ if __vis_i__ not in __vis_lbl__
        ]
        if __vis_need__:
            self.__vis_prime__(__vis_need__)
        __vis_out__ = []
        for __vis_i__ in __vis_sel__:
            if __vis_i__ in __vis_lbl__:
                __vis_gist__ = __vis_lbl__[__vis_i__]
            else:
                __vis_v__ = self.get(__vis_i__)
                __vis_gist__ = (
                    "<missing>"
                    if __vis_v__ is None
                    else self.__vis_gist_of__(__vis_v__)
                )
            __vis_out__.append(str(__vis_i__) + " · " + __vis_gist__)
        return __vis_out__

    def __repr__(self):
        try:
            __vis_n__ = len(self.__vis_all_ids__())
        except Exception:
            __vis_n__ = len(self.__vis_cache__)
        return (
            "<ntr: "
            + str(__vis_n__)
            + " stored native results · ntr[tool_id] fetches one "
            "with no re-run · ntr.describe() lists the latest turn with what each holds>"
        )


ntr = __VisNativeResults__()
native_tools_results = ntr  # backwards-compatible verbose alias


# Literal-key ids a block reads via ntr[...] or native_tools_results[...]
# (STRING subscript only). Used by __vis_run_async__ to prime the whole batch in
# ONE query. A non-literal subscript (a variable / comprehension) is skipped here
# and served lazily by __getitem__.
def __vis_native_result_scan__(__vis_tree__):
    __vis_ids__ = []
    for __vis_n__ in __vis_ast__.walk(__vis_tree__):
        if (
            isinstance(__vis_n__, __vis_ast__.Subscript)
            and isinstance(__vis_n__.value, __vis_ast__.Name)
            and __vis_n__.value.id in ("ntr", "native_tools_results")
        ):
            __vis_k__ = __vis_n__.slice
            if isinstance(__vis_k__, __vis_ast__.Constant) and isinstance(
                __vis_k__.value, str
            ):
                __vis_ids__.append(__vis_k__.value)
    return __vis_ids__
