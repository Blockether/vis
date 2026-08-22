import ast as __vis_ast__
import builtins as __vis_builtins__
import errno as __vis_errno__
import gc as __vis_gc__
import io as __vis_io__
import linecache as __vis_linecache__
import os as __vis_os__
import socket as __vis_socket__
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
# later `shell` call dies with the JDK's misleading "spawn helper / JDK
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


# ── SOCKETS: the THIRD door onto a descriptor, and the one neither shim above can
# see. Every HTTP call in the sandbox rides the stdlib `urllib` -> `http.client`
# -> a real socket (`requests` is pure Python over urllib, and `httpx`/`urllib3`
# are layers on top of `requests`), so a response whose body is never fully read
# leaks a CONNECTED descriptor exactly the way a dropped `open()` leaks a file:
# measured, 20 dropped unread responses = +63 process descriptors, reclaimed by
# neither two `gc.collect()`s nor the block boundary, and counted by nothing — a
# socket is minted by `socket.socket(...)`, never by `open`, so the ceiling that
# exists to keep the session out of EMFILE never saw one.
#
# A socket cannot reuse the file identity: `fstat` on one reports
# `st_dev == st_ino == 0` (measured), so every socket looks like every other one
# and the recycled-number guard would eventually close the JVM's OWN connections.
# A socket's identity is its ADDRESS PAIR, `(getsockname(), getpeername())`,
# captured at every point the sandbox touches it while it is still alive —
# creation, `connect`, `bind`, and each sweep — because a socket is born ANONYMOUS
# and only becomes identifiable once it is connected or bound. The number is
# re-checked after the fact by REBINDING a probe onto it:
# `socket.socket(fileno=fd)` adopts a descriptor without duplicating it,
# `detach()` hands it back untouched and `close()` reclaims it (all measured).
__vis_sock_tag__ = "vis-socket"
# A depth counter, not a flag: reentrancy here is not hypothetical — the probe is
# built INSIDE a sweep, and a tracked probe would re-enter admit -> reclaim ->
# probe without end.
__vis_sock_hook_off__ = __vis_survivor__("__vis_sock_hook_off__", lambda: [0])
__vis_real_socket_init__ = __vis_survivor__(
    "__vis_real_socket_init__", lambda: __vis_socket__.socket.__init__
)
__vis_real_socket_connect__ = __vis_survivor__(
    "__vis_real_socket_connect__", lambda: __vis_socket__.socket.connect
)
__vis_real_socket_connect_ex__ = __vis_survivor__(
    "__vis_real_socket_connect_ex__", lambda: __vis_socket__.socket.connect_ex
)
__vis_real_socket_bind__ = __vis_survivor__(
    "__vis_real_socket_bind__", lambda: __vis_socket__.socket.bind
)


def __vis_sock_id__(__vis_s__):
    __vis_name__ = None
    __vis_peer__ = None
    try:
        __vis_name__ = __vis_s__.getsockname()
    except Exception:
        pass  # unbound, or already closed: that IS part of the identity
    try:
        __vis_peer__ = __vis_s__.getpeername()
    except Exception:
        pass  # not connected
    return (__vis_sock_tag__, __vis_name__, __vis_peer__)


def __vis_is_sock_id__(__vis_id__):
    return (
        isinstance(__vis_id__, tuple)
        and len(__vis_id__) == 3
        and __vis_id__[0] == __vis_sock_tag__
    )


def __vis_sock_track__(__vis_s__):
    # WEAK, like every other entry: tracking a socket must never keep the
    # connection open.
    try:
        __vis_fd__ = __vis_s__.fileno()
    except Exception:
        return
    if not isinstance(__vis_fd__, int) or __vis_fd__ < 0:
        return
    try:
        __vis_fd_registry__[__vis_fd__] = (
            __vis_weakref__.ref(__vis_s__),
            __vis_sock_id__(__vis_s__),
        )
    except Exception:
        pass  # unweakrefable socket: nothing we can track


def __vis_sock_drop__(__vis_fd__, __vis_id__):
    # Close ONE unreachable socket, and only while that number still carries the
    # same address pair: a recycled number belongs to whoever holds it NOW.
    __vis_sock_hook_off__[0] += 1
    try:
        __vis_p__ = __vis_socket__.socket(fileno=__vis_fd__)
    except Exception:
        return 0  # already closed, or not a socket any more: hands off
    finally:
        __vis_sock_hook_off__[0] -= 1
    try:
        if __vis_sock_id__(__vis_p__) == __vis_id__:
            __vis_p__.close()
            return 1
    except Exception:
        return 0
    try:
        __vis_p__.detach()  # somebody else's descriptor: give it back untouched
    except Exception:
        pass
    return 0


def __vis_fd_drop__(__vis_fd__, __vis_id__):
    # Close ONE unreachable descriptor, but only while it still is the file we
    # opened: if the number was recycled, `fstat` either fails (already closed)
    # or reports another file, and both mean hands off.
    __vis_fd_registry__.pop(__vis_fd__, None)
    if __vis_is_sock_id__(__vis_id__):
        return __vis_sock_drop__(__vis_fd__, __vis_id__)
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


def __vis_run_reapers__():
    # The boundary hook for every reclamation that is NOT a descriptor: the host
    # handles shims handed this block (`__vis_own__`) are freed here — at the end
    # of a block and at every tool call — so what a finished block dropped does
    # not wait for the next one to allocate. It must never raise.
    try:
        __vis_handle_reap__()
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
        if __vis_is_sock_id__(__vis_e__[1]):
            # A live socket has no `.closed`, and it may have CONNECTED since it
            # was tracked: refresh the address pair now, while there still is an
            # object to ask.
            try:
                if __vis_h__.fileno() < 0:
                    __vis_fd_registry__.pop(__vis_fd__, None)
                else:
                    __vis_fd_registry__[__vis_fd__] = (
                        __vis_e__[0],
                        __vis_sock_id__(__vis_h__),
                    )
            except Exception:
                __vis_fd_registry__.pop(__vis_fd__, None)
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
        + ". Sandbox Python does NOT close a file or a socket when you drop it,"
        " so `open(p).read()` in a loop leaks one descriptor per iteration — and"
        " so does every HTTP response whose body is never read — until no"
        " `shell` process can start at all. Use `with open(p) as f:` (or"
        " `Path(p).read_text()`), and close the sockets and responses you keep."
        " VIS_PY_MAX_OPEN_FILES raises the ceiling.",
    )


# ── HOST-HANDLE OWNERSHIP: the same non-refcounting fact, one level out from
# descriptors. A shim hands the block a small Python wrapper around a HOST id —
# a PIL raster (an int[], 4 bytes per pixel, ~12 MB for one phone screenshot),
# an SSH session, an SQLite connection — while the resource itself lives in a
# per-JVM registry keyed by that id. Dropping the wrapper frees NOTHING: no
# `__del__` runs, the id is a plain int that outlives its owner, and the host
# cannot see that the last Python reference died (measured: 20 dropped
# `Image.new`s = 20 live rasters after `gc.collect()`; 15 dropped
# `sqlite3.connect()`s = 14 leaked descriptors). Every shim used to invent its
# own weak-ref table, its own sweep policy and its own reaper, so a shim that
# forgot leaked for the life of the process. This is that machinery ONCE: a shim
# declares how its kind is freed (`__vis_handle_kind__`) and names each owner
# (`__vis_own__`); the runtime frees a handle when no owner can be reached any
# more — on the reaper schedule (tool-call boundary, block end), under
# allocation pressure, and eagerly when the shim itself closes or replaces one.
# A handle may have SEVERAL owners (PIL's `exif_transpose(in_place=True)` hands
# two Images one raster), so it dies with the LAST of them, never the first.
__vis_handle_freers__ = __vis_survivor__("__vis_handle_freers__", dict)
__vis_handles__ = __vis_survivor__("__vis_handles__", dict)
__vis_handle_state__ = __vis_survivor__(
    "__vis_handle_state__",
    lambda: {
        "live_bytes": 0,
        "new_bytes": 0,
        "new_owners": 0,
        "sweeping": False,
        "owned_since_sweep": False,
    },
)
# Sweep after this many freshly owned bytes or handles; collect only when what is
# still pinned crosses the budget — a `gc.collect()` costs ~270ms, so it is the
# fallback here exactly as it is for descriptors.
__vis_handle_sweep_bytes__ = 32 * 1024 * 1024
__vis_handle_sweep_owners__ = 64
__vis_handle_collect_bytes__ = 96 * 1024 * 1024
__vis_handle_collect_owners__ = 128
# What a block may leave pinned across a boundary before that boundary pays for a
# collection.
__vis_handle_boundary_bytes__ = 32 * 1024 * 1024


def __vis_handle_kind__(__vis_kind__, __vis_free__):
    # Declared ONCE per shim install: `__vis_free__(key)` releases one handle of
    # this kind host-side. A reinstalled shim rebinds it, because the old
    # callable closes over the old module's host bridge.
    __vis_handle_freers__[__vis_kind__] = __vis_free__
    __vis_handles__.setdefault(__vis_kind__, {})


def __vis_own__(__vis_owner__, __vis_kind__, __vis_key__, __vis_nbytes__=0):
    # Name one more OWNER of a host handle. `__vis_nbytes__` is what the host
    # holds for it (0 when the cost is a socket or a connection rather than
    # memory); it only ever drives WHEN the sweep runs.
    __vis_map__ = __vis_handles__.setdefault(__vis_kind__, {})
    __vis_e__ = __vis_map__.get(__vis_key__)
    if __vis_e__ is None:
        __vis_n__ = max(0, int(__vis_nbytes__))
        __vis_map__[__vis_key__] = [__vis_n__, [__vis_weakref__.ref(__vis_owner__)]]
        __vis_handle_state__["live_bytes"] += __vis_n__
        __vis_handle_state__["new_bytes"] += __vis_n__
        __vis_handle_state__["new_owners"] += 1
        __vis_handle_state__["owned_since_sweep"] = True
        __vis_handle_admit__()
    else:
        __vis_e__[1].append(__vis_weakref__.ref(__vis_owner__))


def __vis_forget__(__vis_kind__, __vis_key__):
    # Stop tracking a handle WITHOUT freeing it: the caller is freeing it itself
    # (an explicit `close()`, where an error belongs to the caller), or the host
    # id was replaced by an in-place op.
    __vis_e__ = __vis_handles__.get(__vis_kind__, {}).pop(__vis_key__, None)
    if __vis_e__ is not None:
        __vis_handle_state__["live_bytes"] -= __vis_e__[0]


def __vis_free_handle__(__vis_kind__, __vis_key__):
    # Untrack and release host-side. Best-effort by contract: this runs from
    # sweeps and reapers, where a stale handle must never break the block.
    __vis_forget__(__vis_kind__, __vis_key__)
    __vis_free__ = __vis_handle_freers__.get(__vis_kind__)
    if __vis_free__ is None:
        return False
    try:
        __vis_free__(__vis_key__)
        return True
    except Exception:
        return False


def __vis_disown__(__vis_owner__, __vis_kind__, __vis_key__):
    # Drop ONE owner, and free the handle when no other owner can still reach it.
    # This is the EAGER path — `close()`, and an in-place op that replaces a
    # handle — so neither waits for a sweep. Answers whether it was freed.
    __vis_e__ = __vis_handles__.get(__vis_kind__, {}).get(__vis_key__)
    if __vis_e__ is None:
        return False
    __vis_keep__ = []
    for __vis_ref__ in __vis_e__[1]:
        __vis_other__ = __vis_ref__()
        if __vis_other__ is not None and __vis_other__ is not __vis_owner__:
            __vis_keep__.append(__vis_ref__)
    if __vis_keep__:
        __vis_e__[1] = __vis_keep__
        return False
    return __vis_free_handle__(__vis_kind__, __vis_key__)


def __vis_reclaim_handles__(__vis_force__=False):
    # Free every handle whose owners are ALL unreachable; answers how many.
    # Only a collection clears GraalPy's weak refs, so a forced pass collects
    # first; the cheap pass frees whatever an ordinary JVM GC already cleared.
    if __vis_handle_state__["sweeping"]:
        return 0
    __vis_handle_state__["sweeping"] = True
    try:
        if __vis_force__:
            __vis_gc__.collect()
            __vis_handle_state__["owned_since_sweep"] = False
        __vis_handle_state__["new_bytes"] = 0
        __vis_handle_state__["new_owners"] = 0
        __vis_freed__ = 0
        for __vis_kind__ in list(__vis_handles__):
            __vis_map__ = __vis_handles__.get(__vis_kind__) or {}
            for __vis_key__ in list(__vis_map__):
                __vis_e__ = __vis_map__.get(__vis_key__)
                if __vis_e__ is None:
                    continue
                if any(__vis_ref__() is not None for __vis_ref__ in __vis_e__[1]):
                    continue
                if __vis_free_handle__(__vis_kind__, __vis_key__):
                    __vis_freed__ += 1
        return __vis_freed__
    finally:
        __vis_handle_state__["sweeping"] = False


def __vis_handle_pinned__():
    # How many handles the registry still holds, across every kind.
    __vis_pinned__ = 0
    for __vis_kind__ in list(__vis_handles__):
        __vis_pinned__ += len(__vis_handles__.get(__vis_kind__) or {})
    return __vis_pinned__


def __vis_handle_relieve__():
    # The IN-BLOCK policy. Cheap pass first — it frees whatever an ordinary JVM
    # GC already cleared — and a collection only when what is still pinned is
    # over budget. A kind whose cost is a socket or a connection reports no bytes
    # at all, so the byte budget can never fire for it: for those, COUNT is the
    # only pressure there is (200 dropped `sqlite3.connect()`s inside one block
    # are 200 host connections and their descriptors, with nothing to show for it
    # in bytes).
    __vis_reclaim_handles__(False)
    if __vis_handle_state__["live_bytes"] >= __vis_handle_collect_bytes__:
        __vis_reclaim_handles__(True)
    elif __vis_handle_pinned__() >= __vis_handle_collect_owners__:
        __vis_reclaim_handles__(True)


def __vis_handle_reap__():
    # The BOUNDARY policy (block end, every tool call), where the in-block one is
    # not enough: a block that finishes holding 50 MB of dropped rasters is under
    # no allocation pressure at all, and nothing would free them until the NEXT
    # block happened to allocate. Nothing is running here, so a collection costs
    # latency no statement is waiting on — but it is still ~270ms, so it is paid
    # only when something was owned since the last one (a boundary that follows a
    # boundary is free) and what is pinned is worth collecting for.
    if not __vis_handle_state__["owned_since_sweep"]:
        return
    __vis_reclaim_handles__(False)
    if (
        __vis_handle_state__["live_bytes"] >= __vis_handle_boundary_bytes__
        or __vis_handle_pinned__() >= __vis_handle_sweep_owners__
    ):
        __vis_reclaim_handles__(True)


def __vis_handle_admit__():
    # Runs when a shim owns a new handle. Under the mark it is two int compares.
    if (
        __vis_handle_state__["new_bytes"] < __vis_handle_sweep_bytes__
        and __vis_handle_state__["new_owners"] < __vis_handle_sweep_owners__
    ):
        return
    __vis_handle_relieve__()


def __vis_handle_census__():
    # What each kind still pins: `{kind: {"count": n, "bytes": b}}`. The seam the
    # lifetime tests assert on, and the first thing to print when a sandbox grows.
    __vis_out__ = {}
    for __vis_kind__ in list(__vis_handles__):
        __vis_map__ = __vis_handles__.get(__vis_kind__) or {}
        __vis_out__[__vis_kind__] = {
            "count": len(__vis_map__),
            "bytes": sum(__vis_e__[0] for __vis_e__ in list(__vis_map__.values())),
        }
    return __vis_out__


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


# The socket doors. `__init__` catches every socket the sandbox mints — including
# the `SSLSocket` that `wrap_socket` builds over an already-connected descriptor
# and the one `accept()` hands back (measured: both pass through here) — and
# `connect`/`connect_ex`/`bind` re-track it at the moment it stops being
# anonymous, which is the only moment a socket created and dropped inside ONE
# block is ever identifiable. Nothing hooks `close`: a closed socket reports
# `fileno() == -1` and the next sweep drops its entry.
def __vis_socket_init__(self, *__vis_a__, **__vis_kw__):
    if __vis_sock_hook_off__[0]:
        return __vis_real_socket_init__(self, *__vis_a__, **__vis_kw__)
    __vis_fd_admit__()
    __vis_r__ = __vis_real_socket_init__(self, *__vis_a__, **__vis_kw__)
    __vis_sock_track__(self)
    return __vis_r__


def __vis_socket_connect__(self, *__vis_a__, **__vis_kw__):
    try:
        return __vis_real_socket_connect__(self, *__vis_a__, **__vis_kw__)
    finally:
        __vis_sock_track__(self)


def __vis_socket_connect_ex__(self, *__vis_a__, **__vis_kw__):
    try:
        return __vis_real_socket_connect_ex__(self, *__vis_a__, **__vis_kw__)
    finally:
        __vis_sock_track__(self)


def __vis_socket_bind__(self, *__vis_a__, **__vis_kw__):
    try:
        return __vis_real_socket_bind__(self, *__vis_a__, **__vis_kw__)
    finally:
        __vis_sock_track__(self)


__vis_socket__.socket.__init__ = __vis_socket_init__
__vis_socket__.socket.connect = __vis_socket_connect__
__vis_socket__.socket.connect_ex = __vis_socket_connect_ex__
__vis_socket__.socket.bind = __vis_socket_bind__


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
    # the `shell(...)["out"]` / `run_tests(...)["output"]` papercut. We deliberately
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
    # expression`. The classic trap: `x = grep(...)` AUTO-SETTLES on assignment
    # (so `x` already holds the real ForeignList result), then `await x` blows
    # up. With this, the stray `await` is harmless — we simply don't care.
    # Real awaitables (a deferred `__vis_Call__`, a `gather` `__vis_Gather__`,
    # or anything with `__await__`) pass straight through so `await tool(...)` /
    # `await gather(...)` keep being driven by `__vis_drive__` exactly as before.
    if isinstance(v, (__vis_Call__, __vis_Gather__)):
        return v
    if __vis_is_awaitable__(v):
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
        # `rg(query="x")`, `run_tests(language="python")` — so folding
        # kwargs to one dict matches their contract (all-kwargs collapses to a spec map).
        # Flush what the block wrote through a still-open handle FIRST: a tool
        # that reads a just-written file (`git commit -F /tmp/msg`) must not see
        # GraalPy's unflushed buffer.
        __vis_flush_writes__()
        # Same boundary, the descriptor half: a tool that spawns a process
        # (`shell`) needs free descriptors to fork with, so give back the
        # ones this block already dropped. Below the mark this is a no-op.
        __vis_reclaim_fds__()
        c.res = c.fn(*c.a, dict(c.k)) if c.k else c.fn(*c.a)
        # COLLAPSE a call that answered with ANOTHER deferred call. Handing a TOOL
        # ITSELF to the pool — `asyncio.to_thread(grep, q)`,
        # `loop.run_in_executor(None, grep, q)`, `create_task(to_thread(grep, q))` —
        # only BUILDS grep's own thunk inside the worker, so the value that came back
        # was an unrun `__vis_Call__`. Binding it hid that (every statement
        # auto-settles), but a `gather` LIST does not settle its slots, so
        # `json.dumps(results[0])` refused an object the caller never created.
        # Collapse HERE: every settle path already funnels through this one call
        # (`__vis_drive__` for `await`, gather children, statements, `r[k]`,
        # `print`), and `__vis_settle__` re-enters this frame for the inner call, so
        # a longer chain unwinds by recursion and a callable that answers with
        # ITSELF dies on Python's own RecursionError instead of a hand-rolled cap.
        if type(c.res) is __vis_Call__ or type(c.res) is __vis_Gather__:
            c.res = __vis_settle__(c.res)
        return c.res
    except BaseException as __vis_exc__:
        # A failed thunk is one-shot too. Do not cache the exception here: a Python
        # traceback points back through this frame to `c`, which would make a retained
        # failed call retain itself plus the callable and payload graph.
        c.failed = True
        # CATCHABILITY is uniform across EVERY settle path. A host tool refusal
        # arrives as a foreign `ExceptionInfo`: a BaseException that is NOT an
        # Exception, so `except Exception:` could not catch it and a refusal blew
        # past the handler written for it and killed the whole block. `__vis_drive__`
        # already wrapped it for the `await` path; wrapping HERE covers the inline
        # ones too (statement settle, `r[k]`, `len(r)`, `print(r)`). It is a no-op
        # for a native Python exception, which keeps its own type and message.
        __vis_wrapped__ = __vis_wrap_tool_exc__(__vis_exc__)
        if __vis_wrapped__ is __vis_exc__:
            raise
        raise __vis_wrapped__ from None
    finally:
        c.ran = True
        # Success, failure, and cancellation all release host callable + arguments.
        c.fn = None
        c.a = ()
        c.k = {}


def __vis_key_hint__(__vis_d__, __vis_k__):
    # A missing key on a TOOL RESULT is a LOOKUP mistake, not a broken tool: shapes
    # differ per tool (shell -> out/exit/duration_ms, run_tests -> output,
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


class __VisShellLogs__:
    # The handle's log reader is callable as documented, but also tolerates the common
    # `sh.logs[-120:]` guess by reading a page and slicing its text. It is deliberately
    # private: `logs(...)` remains the only advertised shape.
    __slots__ = ("__vis_shell__",)

    def __init__(self, __vis_shell__):
        self.__vis_shell__ = __vis_shell__

    def __call__(self, offset=None, lines=None, limit=None, **aliases):
        return self.__vis_shell__.__vis_logs__(offset, lines, limit, **aliases)

    def __getitem__(self, __vis_key__):
        return self.__vis_shell__.__vis_logs__()[__vis_key__]


class __VisShell__(__VisResult__):
    # A SHELL RESULT IS A LIVE HANDLE. Every answer from the shell family (`shell`
    # itself, and each handle op) comes back as this dict-with-METHODS, so a process
    # is driven on the object the call already returned instead of by re-typing its id
    # into three loose verbs: `sh.logs(offset=0)`, `sh.wait(30)`, `sh.type("y")`,
    # `sh.stop()`. It IS a dict — `sh["exit"]`, `json.dumps(sh)`, `{**sh}` behave — so
    # nothing about reading a result changed; only driving it got shorter. The
    # underlying `_shell_*` transports stay private (underscore = absent from
    # `apropos`), and re-issuing a live `id` through `shell` hands the SAME handle back,
    # which is how a later block re-acquires one.
    __vis_shell_ops__ = frozenset(
        (
            "shell",
            "_shell_logs",
            "_shell_wait",
            "_shell_type",
            "_shell_stop",
        )
    )

    def __vis_op__(self, __vis_name__, __vis_args__):
        fn = globals().get(__vis_name__)
        if fn is None:
            # The host's ONE wording for a sandbox with no process surface
            # (`env_python/PROCESS_SURFACE`), the same sentence `subprocess` and
            # the prompt block say — a handle is just where it is met next.
            raise RuntimeError(globals()["__vis_process_surface__"]["off"])
        return __vis_settle__(fn(__vis_args__))

    @property
    def stdout(self):
        # Quiet compatibility aliases for the guesses Python process APIs invite.
        # Keep them out of the result map and docs: `out` remains canonical.
        return self["out"]

    stderr = stdout

    @property
    def logs(self):
        return __VisShellLogs__(self)

    __vis_logs_aliases__ = {
        "n": "lines",
        "tail": "lines",
        "start": "offset",
        "bytes": "limit",
    }
    __vis_log_page_cap__ = 10

    def __getitem__(self, __vis_key__):
        # A LOG PAGE is the map that carries status and cursors, but its payload is
        # text. Tail-clipping that text is common at the print boundary, so a slice
        # addresses `out` directly while every string key and ordinary dict operation
        # keeps mapping semantics. Other shell stages stay nonsliceable result maps.
        if isinstance(__vis_key__, slice) and dict.get(self, "op") == "_shell_logs":
            return dict.__getitem__(self, "out")[__vis_key__]
        return super().__getitem__(__vis_key__)

    def __vis_log_text_concat__(self, other, reverse):
        # A heading + log page is the unsliced twin of heading + page[-n:]. Preserve
        # the status map everywhere else; only string concatenation reads its payload.
        if dict.get(self, "op") != "_shell_logs" or not isinstance(other, str):
            return NotImplemented
        out = dict.__getitem__(self, "out")
        return other + out if reverse else out + other

    def __add__(self, other):
        return self.__vis_log_text_concat__(other, False)

    def __radd__(self, other):
        return self.__vis_log_text_concat__(other, True)

    def __vis_log_page__(self, __vis_args__):
        __vis_page__ = self.__vis_op__("_shell_logs", __vis_args__)
        __vis_page__.__vis_logs_spec__ = dict(__vis_args__)
        return __vis_page__

    def __vis_log_spec__(self):
        __vis_spec__ = getattr(self, "__vis_logs_spec__", None)
        if __vis_spec__ is None:
            raise RuntimeError(
                "shell log paging starts on a log page; call "
                "sh.logs(0, lines=100) first."
            )
        return __vis_spec__

    def __vis_logs__(self, offset=None, lines=None, limit=None, **aliases):
        # Read the log and return NOW — nothing blocks on the caller's behalf.
        # A NEGATIVE offset is the last n LINES (`sh.logs(-50)`), the same
        # reading `cat(path, -50)` has; a positive one is a byte cursor.
        # `lines=10` is the LINE window: the last ten with no offset, the NEXT
        # ten from one (`sh.logs(next_offset, 10)`), and `lines=-10` the ten
        # ABOVE that offset — the window scrolls a long log both ways instead
        # of only forward. Every answer captures this whole read, so `next(page)`
        # continues it without retyping a cursor and `page.pages()` walks lazily.
        # A near-miss keyword folds onto the one it means rather than costing the read.
        named = {"offset": offset, "lines": lines, "limit": limit}
        for key, value in aliases.items():
            canonical = self.__vis_logs_aliases__.get(key)
            if canonical is None:
                raise TypeError(
                    "logs: no keyword '%s'. Keywords: offset, lines, limit "
                    "(n/tail mean lines, start means offset)." % key
                )
            if named[canonical] is not None:
                raise TypeError("logs: %s named twice, as '%s' too." % (canonical, key))
            named[canonical] = value
        args = {"id": self["id"]}
        for key in ("offset", "lines", "limit"):
            if named[key] is not None:
                args[key] = int(named[key])
        return self.__vis_log_page__(args)

    def __next__(self):
        # Continue only what this page can read NOW. Forward paging ends at the
        # snapshot's EOF even while the process is live; a later `logs()` starts a
        # fresh snapshot. A negative line window walks toward byte zero instead.
        __vis_spec__ = self.__vis_log_spec__()
        __vis_lines__ = __vis_spec__.get("lines")
        __vis_backward__ = __vis_lines__ is not None and int(__vis_lines__) < 0
        if __vis_backward__:
            __vis_cursor__ = self.get("offset")
            if (
                __vis_cursor__ is None
                or int(__vis_cursor__) <= 0
                or __vis_cursor__ == __vis_spec__.get("offset")
            ):
                raise StopIteration
        else:
            if self.get("is_eof"):
                raise StopIteration
            __vis_cursor__ = self.get("next_offset")
            if (
                __vis_cursor__ is None
                or __vis_cursor__ == self.get("offset")
                or __vis_cursor__ == __vis_spec__.get("offset")
            ):
                raise StopIteration
        __vis_next__ = dict(__vis_spec__)
        __vis_next__["offset"] = int(__vis_cursor__)
        return self.__vis_log_page__(__vis_next__)

    def pages(self, max_pages=None):
        # This page, then each ready page after it, lazily and bounded to ten by
        # default. `__iter__` remains dict iteration; page walking is explicit.
        self.__vis_log_spec__()
        __vis_cap__ = self.__vis_log_page_cap__ if max_pages is None else int(max_pages)
        __vis_page__ = self
        __vis_seen__ = 0
        while __vis_page__ is not None:
            yield __vis_page__
            __vis_seen__ += 1
            if __vis_seen__ >= __vis_cap__:
                break
            __vis_page__ = next(__vis_page__, None)

    def type(self, text, is_enter=True):
        return self.__vis_op__(
            "_shell_type",
            {"id": self["id"], "text": str(text), "is_enter": bool(is_enter)},
        )

    def stop(self):
        return self.__vis_op__("_shell_stop", {"id": self["id"]})

    def wait(self, seconds=120, **aliases):
        # ONE wait, and it does NOT live here: the bounded poll loop is host code
        # (`_shell_wait`), so the sandbox handle, an extension's handle and the tests
        # all wait the same way instead of each re-typing a loop that can disagree
        # about the deadline. `timed_out` means the WAIT expired, not the process.
        # `secs=`/`timeout=` mean `seconds`: the docs say `sh.wait(secs)`, so the
        # word a reader carries over must not silently do nothing.
        for key in ("secs", "timeout"):
            if key in aliases:
                seconds = aliases.pop(key)
        if aliases:
            raise TypeError(
                "wait: no keyword '%s'. Keywords: seconds (secs/timeout mean it)."
                % sorted(aliases)[0]
            )
        return self.__vis_op__(
            "_shell_wait", {"id": self["id"], "seconds": int(seconds)}
        )


class __VisResultList__(list):
    # A host call result whose TOP-LEVEL shape is a LIST (a tool that answers
    # one row per hit). It stays a
    # REAL list — index / iterate / len / json.dumps / {**_}-free code all behave —
    # but ALSO answers the dict probes (.get/.keys/.items/.values) so a uniform
    # `res.get('op')` probe NEVER trips on it. A list has no top-level 'op', so .get
    # returns the default and each row stays reachable by index (res[0]['op']).
    def get(self, __k__, __d__=None):
        return __d__

    def keys(self):
        return []

    def items(self):
        return []

    def values(self):
        return []


class __VisResultStr__(str):
    # A host call result that is a bare STRING (a verb returning plain text). Still a
    # real str, but answers the same dict probes, so `.get('op')` yields None instead of
    # blowing up with a `'str' object has no attribute 'get'` when a probe hits it.
    # .keys()/.items()/.values() are empty — a string has no fields.
    def get(self, __k__, __d__=None):
        return __d__

    def keys(self):
        return []

    def items(self):
        return []

    def values(self):
        return []


def __vis_grep_next_offset__(__vis_text__):
    # Line 1 of a search answer names the LITERAL next call whenever the sweep was
    # capped:
    #
    #   grep 'q'  50 hits · 3 of 30 files  capped by limit → next(r) or grep({…, "offset": 50})
    #
    # The renderer OWNS that sentence (`grep-summary-line`, pinned verbatim by
    # `core_test`), so reading the offset back out of it reads the CONTRACT, not
    # scraped prose. No cap, no arrow, no offset — and None then means this page
    # already IS the whole answer.
    __vis_head__ = __vis_text__.split("\n", 1)[0]
    __vis_marker__ = '"offset": '
    __vis_at__ = __vis_head__.rfind(__vis_marker__)
    if __vis_at__ < 0:
        return None
    __vis_digits__ = ""
    for __vis_ch__ in __vis_head__[__vis_at__ + len(__vis_marker__) :]:
        if not __vis_ch__.isdigit():
            break
        __vis_digits__ += __vis_ch__
    return int(__vis_digits__) if __vis_digits__ else None


def __vis_paged_spec__(__vis_c__):
    # What a PAGED search would need to continue itself — `(tool-name, options map)`
    # — or None when this call is not one. Read BEFORE the call runs: `__vis_exec_call__`
    # releases `fn`/`a`/`k` in its `finally` the moment it has an answer, so a spec
    # taken from the settled thunk is always empty.
    #
    # grep takes exactly ONE options map (kwargs ARE that map, a positional query is
    # refused), so the next page is that map plus `offset`. A call shaped any other
    # way pages to None, and `next(g)` then refuses BY NAME instead of quietly
    # searching for something else.
    __vis_nm__ = getattr(__vis_c__, "nm", None)
    if __vis_nm__ not in __vis_paged_tools__:
        return None
    __vis_a__ = getattr(__vis_c__, "a", None) or ()
    __vis_k__ = getattr(__vis_c__, "k", None) or {}
    if len(__vis_a__) == 1 and isinstance(__vis_a__[0], dict):
        return (__vis_nm__, dict(__vis_a__[0]))
    if __vis_k__ and not __vis_a__:
        return (__vis_nm__, dict(__vis_k__))
    return (__vis_nm__, None)


class __VisGrep__(__VisResultStr__):
    # A SEARCH ANSWER THAT KNOWS HOW TO CONTINUE. `limit` caps a page (50 hits by
    # default), so a wide sweep answers a SLICE, and the only thing between the
    # caller and the rest of it used to be retyping the whole call with `offset` —
    # the step nobody takes, which is how a capped page gets read as "that is all
    # there is". Line 1 still names that next call, because a printed block is
    # head-clipped and the truncation must survive the clip; this makes the same
    # fact a METHOD, so walking the rest costs no retyping and no arithmetic.
    #
    # It IS the text: every str operation, slice, `in`, `print`, `.startswith`
    # behaves, and `.get`/`.keys` still answer the uniform result probe. `__iter__`
    # is deliberately NOT overridden — iterating a string means CHARACTERS
    # everywhere else in Python, and a page walk that quietly stole that would
    # break `"".join(g)` and `list(g)`. Pages are walked by name:
    #
    #   g.next_offset          where the next page starts, None when complete
    #   next(g)                the next page (a __VisGrep__ too); StopIteration
    #                          when there is none, so `next(g, None)` is the
    #                          sentinel form
    #   for page in g.pages(): every page from this one, BOUNDED
    #   g.all()                every page as ONE text, bounded, and it SAYS so
    #
    # Every page is a real host search, so the walk is bounded by default: an
    # unbounded one would pour a whole tree into the block that asked for a slice.
    __vis_page_cap__ = 10

    def __vis_paged__(self, __vis_tool__, __vis_spec__):
        self.__vis_tool__ = __vis_tool__
        self.__vis_spec__ = __vis_spec__
        self.next_offset = __vis_grep_next_offset__(self)
        self.is_capped = self.next_offset is not None
        return self

    def __next__(self):
        # THE NEXT PAGE. `next(g)` is what Python already calls this, so a walk over
        # pages needs no vocabulary of its own; a page that already IS the whole
        # answer raises StopIteration, which makes `next(g, None)` the sentinel form
        # and lets `pages()` end the way every Python walk ends. `__iter__` stays the
        # str's own (characters), so this is the PROTOCOL name, not a claim that a
        # search answer is an iterator.
        if self.next_offset is None:
            raise StopIteration
        if self.__vis_spec__ is None:
            raise RuntimeError(
                self.__vis_tool__ + ": this page cannot continue itself — it did not"
                " come from a single options map. Re-issue the search as"
                ' grep({"query": q, ...}) and page from that result.'
            )
        __vis_fn__ = globals().get(self.__vis_tool__)
        if __vis_fn__ is None:
            raise RuntimeError(
                self.__vis_tool__ + " is not available in this sandbox, so this page"
                " cannot continue."
            )
        __vis_next__ = dict(self.__vis_spec__)
        __vis_next__["offset"] = self.next_offset
        return __vis_settle__(__vis_fn__(__vis_next__))

    def pages(self, max_pages=None):
        # This page, then each following one, up to `max_pages` (10 by default).
        # A generator: stop early and the searches that would have followed are
        # never run.
        __vis_cap__ = self.__vis_page_cap__ if max_pages is None else int(max_pages)
        __vis_page__ = self
        __vis_seen__ = 0
        while __vis_page__ is not None:
            yield __vis_page__
            __vis_seen__ += 1
            if __vis_seen__ >= __vis_cap__:
                break
            __vis_page__ = next(__vis_page__, None)

    def all(self, max_pages=None):
        # Every page as ONE text. When the bound stops the walk before the search
        # runs out, the last line SAYS so and names the call that continues —
        # a silent stop here would be the very truncation this class exists to end.
        __vis_cap__ = self.__vis_page_cap__ if max_pages is None else int(max_pages)
        __vis_parts__ = []
        __vis_last__ = None
        for __vis_page__ in self.pages(__vis_cap__):
            __vis_parts__.append(str(__vis_page__))
            __vis_last__ = __vis_page__
        __vis_text__ = "\n".join(__vis_parts__)
        if __vis_last__ is not None and __vis_last__.next_offset is not None:
            __vis_text__ += (
                "\n… stopped after "
                + str(__vis_cap__)
                + " pages — more hits remain: "
                + self.__vis_tool__
                + '({…, "offset": '
                + str(__vis_last__.next_offset)
                + "}), a higher max_pages, or a narrower search."
            )
        return __VisResultStr__(__vis_text__)


__vis_paged_tools__ = frozenset(("grep", "find_files", "find"))


def __vis_as_result__(__vis_v__, __vis_paging__=None):
    # Normalize a tool result so EVERY value answers the dict probes
    # (.get/.keys/.items/.values) — the shape the model reaches for when it iterates the
    # store. A dict passes through untouched (a tool-result dict is already a
    # __VisResult__). A top-level list/tuple/str is re-typed to a probeable subclass that
    # KEEPS its native list/str behavior, so `res.get('op')` is safe on the whole set
    # without an isinstance guard. Rare scalars (int/float/None/bytes) pass through.
    #
    # `__vis_paging__` is `__vis_paged_spec__`'s answer for the call that produced this
    # value: a PAGED search answers text that can continue itself (see __VisGrep__),
    # and only the call knows the options map to continue WITH.
    if isinstance(__vis_v__, dict):
        return __vis_v__
    if isinstance(__vis_v__, (__VisResultList__, __VisResultStr__)):
        return __vis_v__
    if isinstance(__vis_v__, (list, tuple)):
        return __VisResultList__(__vis_v__)
    if isinstance(__vis_v__, str):
        if __vis_paging__ is not None:
            return __VisGrep__(__vis_v__).__vis_paged__(
                __vis_paging__[0], __vis_paging__[1]
            )
        return __VisResultStr__(__vis_v__)
    return __vis_v__


def __vis_settle_call__(__vis_c__):
    # THE one place a deferred tool call becomes a result. The paging spec is read
    # first, on purpose: `__vis_exec_call__` releases the callable and its arguments
    # as soon as it has an answer, so anything the answer needs to know about its OWN
    # call has to be taken while the call is still holding it.
    __vis_paging__ = __vis_paged_spec__(__vis_c__)
    return __vis_as_result__(
        __vis_pyify__(__vis_exec_call__(__vis_c__)), __vis_paging__
    )


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
    # Tool results cross the host boundary as ProxyHashMap/ProxyArray, and GraalPy
    # 25.1.3 shows those to Python as ForeignDict/ForeignList: subscript, len,
    # iteration, .keys()/.get, KeyError on a missing key, dict(_) and {**_} all
    # behave, and isinstance(_, dict) is even True. The ONE thing that does not
    # work is json.dumps(_) ("Object of type ForeignDict is not JSON
    # serializable"): the encoder dispatches on the EXACT type, and type(_) is not
    # dict. Rebuild proxies into REAL python dict/list ONCE (at settle) so the
    # model composes on true dicts AND can serialize them. A HOST proxy carrying
    # 'op' is a tool result -> mark its type __VisResult__. Order is preserved
    # (source is an ordered LinkedHashMap; comprehensions keep it).
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
        if "op" in d:
            # A shell answer is a HANDLE (see __VisShell__): same dict, plus the methods
            # that drive the process it names. `op` is stamped by the engine on results
            # only, so a model-built dict can never impersonate one.
            if d.get("op") in __VisShell__.__vis_shell_ops__ and "id" in d:
                return __VisShell__(d)
            return __VisResult__(d)
        return __VisDict__(d)
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
                    out.append(__vis_settle_child__(aw))
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
        thunks = [(lambda a=a: __vis_settle_child__(a)) for a in v.aws]
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


def __vis_is_awaitable__(v):
    # Probe the TYPE, never the instance. An object with a catch-all
    # `__getattr__` answers `hasattr(v, "send")` with a value that is not a
    # method at all: bs4's `Tag.__getattr__` maps ANY missing non-dunder
    # attribute to `self.find(name)`, so `soup.send` is None and the old
    # instance probe dragged every top-level `soup = BeautifulSoup(html, ...)`
    # into `__vis_drive__`, where `it.send(None)` died with
    # "TypeError: 'NoneType' object is not callable".
    # A real awaitable defines `__await__` on its class; a raw coroutine-like
    # driven by hand needs BOTH `send` and `throw` (that pair excludes
    # ordinary objects with an unrelated `.send`, e.g. sockets), and plain
    # generators are lazy values, not awaitables.
    t = type(v)
    if hasattr(t, "__await__"):
        return True
    return (
        hasattr(t, "send")
        and hasattr(t, "throw")
        and not isinstance(v, __vis_gen_type__)
    )


def __vis_settle__(v):
    if isinstance(v, __vis_Call__):
        # TOP-LEVEL tool result: re-type a list/str payload to the probeable
        # subclass. Without this a
        # LIST-shaped tool return was a PLAIN list, so the documented
        # uniform `res.get('op')` probe blew up with `'list' object has no attribute
        # 'get'` and the print-capture below could not recognise it as a result.
        return __vis_settle_call__(v)
    if isinstance(v, __vis_Gather__):
        return __vis_settle_gather__(v)
    if isinstance(v, __vis_Future__):
        # A Future is a PLACEHOLDER, not work. Every other awaitable a top-level
        # statement binds is something to RUN where it was written, but driving
        # `f = asyncio.Future()` would block the block itself on a value only a
        # sibling thread can set - and nothing else can run while it waits.
        # `await f` and a Future handed to `gather` still wait (see
        # `__vis_settle_child__`).
        return v
    if __vis_is_awaitable__(v):
        return __vis_pyify__(__vis_drive__(v))
    return __vis_pyify__(v)


def __vis_settle_child__(v):
    # GATHER-child settle. A `gather` slot is work by definition, so the one
    # awaitable the top-level settle refuses to drive - a Future - is exactly the
    # thing a sibling slot is expected to complete here.
    if isinstance(v, __vis_Future__):
        return __vis_pyify__(__vis_drive__(v))
    return __vis_settle__(v)


def __vis_settle_stmt__(v):
    # NESTED-statement settle: run OUR OWN deferred thunk right where the model
    # wrote the statement, and hand every other value straight back. Two identity
    # checks and no `__vis_pyify__` walk — a `for`/`while` body is hot code, and a
    # value that is not a thunk cannot become one. TOP-LEVEL statements keep the
    # full `__vis_settle__`, which also rebuilds foreign proxies into real dicts.
    if type(v) is __vis_Call__ or type(v) is __vis_Gather__:
        return __vis_settle__(v)
    return v


__vis_return_scan_max__ = 1024


def __vis_has_thunk__(v, __vis_d__=0):
    # Is there a deferred call anywhere in this value? EXACT plain containers only,
    # two levels of them, and a bounded element budget: `return rows` in a loop body
    # stays a scan of types with no allocation, so the rebuild below runs only when
    # there is something to settle.
    if type(v) is __vis_Call__ or type(v) is __vis_Gather__:
        return True
    if __vis_d__ > 1:
        return False
    __vis_t__ = type(v)
    if __vis_t__ is dict:
        __vis_seq__ = v.values()
    elif __vis_t__ is tuple or __vis_t__ is list or __vis_t__ is set:
        __vis_seq__ = v
    else:
        return False
    if len(v) > __vis_return_scan_max__:
        return False
    for __e__ in __vis_seq__:
        if __vis_has_thunk__(__e__, __vis_d__ + 1):
            return True
    return False


def __vis_settle_return__(v, __vis_d__=0):
    # RETURN-boundary settle: `return` hands the value to the CALLER, so a thunk
    # riding out in it has left the only scope that could still `await` it. Settle
    # the value AND the thunks inside a plain container the helper built to answer
    # with: `async def m(): g = grep(...); return sess, g` handed back a tuple whose
    # second slot was a raw `__vis_Call__`, and the model only found out blocks
    # later, when `json.dumps` refused an object it never created.
    #
    # This is the ONE settle that also fires inside an `async def`. A coroutine may
    # HOLD an awaitable (`t = asyncio.to_thread(f, x)` ... `await gather(t, u)`) —
    # that is what the nested-statement skip protects — but what it RETURNS is its
    # answer, and `await` on an already-settled value is a no-op
    # (`__vis_AwaitFix__`), so a caller that awaits the returned value still reads
    # the same thing. The cost is that a helper BUILDING a batch for its caller runs
    # it serially at the `return` instead of through the caller's `gather`.
    if type(v) is __vis_Call__ or type(v) is __vis_Gather__:
        return __vis_settle__(v)
    if __vis_d__ > 1 or not __vis_has_thunk__(v, __vis_d__):
        return v
    __vis_t__ = type(v)
    if __vis_t__ is dict:
        return {
            __k__: __vis_settle_return__(__e__, __vis_d__ + 1)
            for __k__, __e__ in v.items()
        }
    if __vis_t__ is list:
        return [__vis_settle_return__(__e__, __vis_d__ + 1) for __e__ in v]
    if __vis_t__ is tuple:
        return tuple(__vis_settle_return__(__e__, __vis_d__ + 1) for __e__ in v)
    if __vis_t__ is set:
        return {__vis_settle_return__(__e__, __vis_d__ + 1) for __e__ in v}
    return v


def __vis_settle_binding__(name):
    g = globals()
    g[name] = __vis_settle__(g[name])
    return g[name]


def __vis_drive__(coro):
    it = coro.__await__() if hasattr(type(coro), "__await__") else coro
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
                send = __vis_settle_call__(y)
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


def __vis_is_user_file__(name):
    # Every co_filename this runtime compiles a MODEL BLOCK under: the bare
    # `<prog>` the test runner execs under, and the per-block `<prog:N>` names
    # `__vis_register_source__` mints (see there for why they are unique).
    return name == "<prog>" or (name[:6] == "<prog:" and name[-1:] == ">")


def __vis_error_pos__(e):
    # Deepest user-code traceback frame -> (line, col, end_col). The
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
        if __vis_is_user_file__(f.f_code.co_filename):
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

    def _bounded(self, timeout):
        # `wait_for(sleep(10), 0.5)` must give up after 0.5 s, not sleep for ten
        # seconds and then report that it took too long.
        delay = max(0.0, self.delay)
        result = self.result
        self.delay = 0.0
        self.result = None
        if timeout is not None and float(timeout) < delay:
            __vis_time__.sleep(max(0.0, float(timeout)))
            raise TimeoutError()
        __vis_time__.sleep(delay)
        return result

    def __vis_bounded__(self, timeout):
        return __vis_Blocking__(self._bounded, timeout)

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
    if timeout is not None:
        # A blocking primitive takes the deadline INTO its own wait, so
        # `wait_for(q.get(), 0.5)` really does give up after 0.5 s instead of
        # discovering afterwards that the value never arrived.
        bounded = getattr(aw, "__vis_bounded__", None)
        if bounded is not None:
            aw = bounded(float(timeout))
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


class QueueEmpty(Exception):
    pass


class QueueFull(Exception):
    pass


class BrokenBarrierError(RuntimeError):
    pass


def __vis_sync_mod__():
    # LAZY on purpose: this runtime source is eval'd into EVERY sandbox context
    # and `import threading` costs ~13 ms there, so a block that never builds a
    # lock, queue or future must not pay for one that does. After the first build
    # it is a `sys.modules` hit.
    import threading

    return threading


class __vis_Blocking__:
    # An awaitable whose wait is a REAL blocking wait, exactly like
    # `__vis_Sleep__`: there is no event loop to hand control to, so a primitive
    # that must WAIT blocks the carrier thread it was awaited on. That is what
    # makes these primitives mean anything here - `gather` settles its children
    # on the host's bounded PLATFORM pool, so the awaitable that blocks and the
    # one that will release it really do run on two threads.
    #
    # `fn(timeout)` returns the result or raises TimeoutError when the deadline
    # expires. `__vis_bounded__` is the seam `__vis_wait_for__` pushes a real
    # deadline through, so `wait_for(q.get(), 0.5)` bounds the WAIT instead of
    # noticing afterwards that it already took too long.
    __slots__ = ("fn", "timeout")

    def __init__(self, fn, timeout=None):
        self.fn = fn
        self.timeout = timeout

    def __vis_bounded__(self, timeout):
        return __vis_Blocking__(self.fn, timeout)

    def __await__(self):
        fn = self.fn
        # Like a completed coroutine frame, a settled wait must not keep the
        # primitive - or a payload bound into the callable - alive.
        self.fn = None
        if fn is None:
            raise RuntimeError("this wait was already awaited")
        result = fn(self.timeout)
        if False:
            yield
        return result


def __vis_ident__():
    # WHICH gather child is calling. Ownership of a guest lock or condition is
    # per THREAD here, because `gather` settles its children on the host's real
    # platform pool - there is no single loop thread that could stand in for it.
    return __vis_sync_mod__().get_ident()


class __vis_Lock__:
    # asyncio.Lock over one guest lock: an uncontended acquire returns at once, a
    # contended one blocks until the holding sibling releases it. The OWNER is
    # tracked here, by thread ident: a guest lock is not owner-aware, so without
    # this a child could release a lock a SIBLING holds (issue #155).
    __slots__ = ("_lock", "_owner")

    def __init__(self):
        self._lock = __vis_sync_mod__().Lock()
        self._owner = None

    def locked(self):
        return self._lock.locked()

    def _is_owner(self):
        # Only the owner ever writes `_owner`, and it writes None before it
        # releases, so a sibling reading it can never see a stale ident.
        owner = self._owner
        return owner is not None and owner == __vis_ident__()

    def _acquire(self, timeout=None):
        if self._lock.acquire(True, -1.0 if timeout is None else float(timeout)):
            self._owner = __vis_ident__()
            return True
        raise TimeoutError()

    def acquire(self):
        return __vis_Blocking__(self._acquire)

    def release(self):
        # A guest lock refuses an unowned release exactly as asyncio's does - but
        # the guest lock underneath refuses only a FULLY unlocked one, so the
        # cross-child case is refused here.
        if not self._is_owner():
            raise RuntimeError(
                "cannot release un-acquired lock: this gather child does not hold it"
            )
        self._owner = None
        self._lock.release()

    async def __aenter__(self):
        await self.acquire()
        return None

    async def __aexit__(self, typ, val, tb):
        self.release()
        return False


class __vis_Event__:
    __slots__ = ("_event",)

    def __init__(self):
        self._event = __vis_sync_mod__().Event()

    def is_set(self):
        return self._event.is_set()

    def set(self):
        self._event.set()

    def clear(self):
        self._event.clear()

    def _wait(self, timeout=None):
        if self._event.wait(timeout):
            return True
        raise TimeoutError()

    def wait(self):
        return __vis_Blocking__(self._wait)


class __vis_Semaphore__:
    __slots__ = ("_sem",)
    _factory = "Semaphore"

    def __init__(self, value=1):
        self._sem = getattr(__vis_sync_mod__(), type(self)._factory)(value)

    def locked(self):
        # A take-and-give-back probe: threading's semaphore reports its count
        # only through a private attribute, and this answer is exactly as racy as
        # asyncio's own (the value can change the moment it is returned).
        if self._sem.acquire(False):
            self._sem.release()
            return False
        return True

    def _acquire(self, timeout=None):
        if self._sem.acquire(True, timeout):
            return True
        raise TimeoutError()

    def acquire(self):
        return __vis_Blocking__(self._acquire)

    def release(self):
        self._sem.release()

    async def __aenter__(self):
        await self.acquire()
        return None

    async def __aexit__(self, typ, val, tb):
        self._sem.release()
        return False


class __vis_BoundedSemaphore__(__vis_Semaphore__):
    __slots__ = ()
    _factory = "BoundedSemaphore"


class __vis_Condition__:
    # Built on a plain guest lock, never threading's default RLock: asyncio's
    # condition is NOT reentrant, and a reentrant one would answer `locked()`
    # with False on the very thread that holds it.
    #
    # Ownership is tracked by `__vis_Lock__`, never left to
    # `threading.Condition._is_owned()`. That one is an acquire-PROBE - it
    # answers "is this lock held by ANYONE" - which means nothing once `gather`
    # settles children on real threads: a non-owner's `notify()` passed the
    # probe while a SIBLING held the lock, and a non-owner's `wait()` passed it
    # too and then released that sibling's lock, after which the true owner's
    # own `notify()` died with threading's internal "cannot notify on
    # un-acquired lock" while the waiting child blocked forever. Owner-checked,
    # each of those is one clean refusal, raised in the child that made the call.
    __slots__ = ("_lock", "_cond")

    def __init__(self, lock=None):
        self._lock = lock if isinstance(lock, __vis_Lock__) else __vis_Lock__()
        self._cond = __vis_sync_mod__().Condition(self._lock._lock)

    def locked(self):
        return self._lock.locked()

    def _acquire(self, timeout=None):
        return self._lock._acquire(timeout)

    def acquire(self):
        return __vis_Blocking__(self._acquire)

    def release(self):
        self._lock.release()

    def _require_owner(self, verb):
        if not self._lock._is_owner():
            raise RuntimeError(
                "cannot "
                + verb
                + " on un-acquired lock: this gather child must hold the condition"
                + " (`async with cond:`) around the call"
            )

    def _wait_raw(self, timeout=None):
        self._require_owner("wait")
        # The wait RELEASES the lock and re-acquires it before returning, on the
        # error path too - so ownership is dropped and restored around it.
        self._lock._owner = None
        try:
            return self._cond.wait(timeout)
        finally:
            self._lock._owner = __vis_ident__()

    def _wait(self, timeout=None):
        if self._wait_raw(timeout):
            return True
        raise TimeoutError()

    def wait(self):
        return __vis_Blocking__(self._wait)

    def _wait_for(self, predicate, timeout=None):
        # threading's own `wait_for` loop, re-expressed on `_wait_raw` so every
        # slice keeps the ownership bookkeeping above.
        endtime = None
        waittime = timeout
        result = predicate()
        while not result:
            if waittime is not None:
                if endtime is None:
                    endtime = __vis_time__.monotonic() + waittime
                else:
                    waittime = endtime - __vis_time__.monotonic()
                    if waittime <= 0:
                        break
            self._wait_raw(waittime)
            result = predicate()
        if not result:
            raise TimeoutError()
        return result

    def wait_for(self, predicate):
        return __vis_Blocking__(lambda timeout, p=predicate: self._wait_for(p, timeout))

    def notify(self, n=1):
        self._require_owner("notify")
        self._cond.notify(n)

    def notify_all(self):
        self._require_owner("notify")
        self._cond.notify_all()

    async def __aenter__(self):
        await self.acquire()
        return None

    async def __aexit__(self, typ, val, tb):
        self._lock.release()
        return False


class __vis_Barrier__:
    __slots__ = ("_barrier",)

    def __init__(self, parties):
        self._barrier = __vis_sync_mod__().Barrier(parties)

    @property
    def parties(self):
        return self._barrier.parties

    @property
    def n_waiting(self):
        return self._barrier.n_waiting

    @property
    def broken(self):
        return self._barrier.broken

    def _wait(self, timeout=None):
        try:
            return self._barrier.wait(timeout)
        except __vis_sync_mod__().BrokenBarrierError as exc:
            raise BrokenBarrierError(*exc.args) from None

    def wait(self):
        return __vis_Blocking__(self._wait)

    def abort(self):
        self._barrier.abort()

    def reset(self):
        self._barrier.reset()


class __vis_Queue__:
    # asyncio.Queue over ONE guest condition. It deliberately does not wrap
    # `queue.Queue`: that one cannot bound `join()`, and an unbounded wait is the
    # single worst failure mode here - it hangs the whole turn, because no loop
    # can cancel the waiter from outside.
    __slots__ = ("_items", "_maxsize", "_cond", "_unfinished")

    def __init__(self, maxsize=0):
        mod = __vis_sync_mod__()
        self._maxsize = int(maxsize)
        self._unfinished = 0
        self._cond = mod.Condition(mod.Lock())
        self._init()

    def _init(self):
        import collections

        self._items = collections.deque()

    def _push(self, item):
        self._items.append(item)

    def _pop(self):
        return self._items.popleft()

    @property
    def maxsize(self):
        return self._maxsize

    def qsize(self):
        with self._cond:
            return len(self._items)

    def empty(self):
        with self._cond:
            return not self._items

    def _is_full(self):
        return 0 < self._maxsize <= len(self._items)

    def full(self):
        with self._cond:
            return self._is_full()

    def put_nowait(self, item):
        with self._cond:
            if self._is_full():
                raise QueueFull()
            self._push(item)
            self._unfinished += 1
            self._cond.notify_all()

    def _put(self, item, timeout=None):
        with self._cond:
            if not self._cond.wait_for(lambda: not self._is_full(), timeout):
                raise TimeoutError()
            self._push(item)
            self._unfinished += 1
            self._cond.notify_all()

    def put(self, item):
        return __vis_Blocking__(lambda timeout, v=item: self._put(v, timeout))

    def get_nowait(self):
        with self._cond:
            if not self._items:
                raise QueueEmpty()
            item = self._pop()
            self._cond.notify_all()
            return item

    def _get(self, timeout=None):
        with self._cond:
            if not self._cond.wait_for(lambda: bool(self._items), timeout):
                raise TimeoutError()
            item = self._pop()
            self._cond.notify_all()
            return item

    def get(self):
        return __vis_Blocking__(self._get)

    def task_done(self):
        with self._cond:
            if self._unfinished <= 0:
                raise ValueError("task_done() called too many times")
            self._unfinished -= 1
            self._cond.notify_all()

    def _join(self, timeout=None):
        with self._cond:
            if not self._cond.wait_for(lambda: self._unfinished == 0, timeout):
                raise TimeoutError()

    def join(self):
        return __vis_Blocking__(self._join)


class __vis_LifoQueue__(__vis_Queue__):
    __slots__ = ()

    def _init(self):
        self._items = []

    def _pop(self):
        return self._items.pop()


class __vis_PriorityQueue__(__vis_Queue__):
    __slots__ = ()

    def _init(self):
        self._items = []

    def _push(self, item):
        import heapq

        heapq.heappush(self._items, item)

    def _pop(self):
        import heapq

        return heapq.heappop(self._items)


class __vis_Future__:
    # The one primitive that exists so two THREADS can hand a value to each
    # other: a `gather` child completes it, a sibling awaits it. Callbacks run on
    # the completing thread - there is no loop to schedule them onto.
    __slots__ = ("_event", "_lock", "_result", "_exception", "_cancelled", "_callbacks")

    def __init__(self, *, loop=None):
        mod = __vis_sync_mod__()
        self._event = mod.Event()
        self._lock = mod.Lock()
        self._result = None
        self._exception = None
        self._cancelled = False
        self._callbacks = []

    def get_loop(self):
        return __vis_asyncio__

    def done(self):
        return self._event.is_set()

    def cancelled(self):
        return self._cancelled

    def _settle(self, cancelled=False, result=None, exception=None):
        with self._lock:
            if self._event.is_set():
                raise InvalidStateError("Future is already done.")
            self._cancelled = cancelled
            self._result = result
            self._exception = exception
            callbacks = self._callbacks
            self._callbacks = []
        self._event.set()
        for callback in callbacks:
            callback(self)

    def set_result(self, result):
        self._settle(result=result)

    def set_exception(self, exception):
        self._settle(exception=__vis_clean_exception__(exception))

    def cancel(self, msg=None):
        if self._event.is_set():
            return False
        self._settle(cancelled=True)
        return True

    def add_done_callback(self, callback, *, context=None):
        with self._lock:
            if not self._event.is_set():
                self._callbacks.append(callback)
                return None
        callback(self)
        return None

    def remove_done_callback(self, callback):
        with self._lock:
            kept = [c for c in self._callbacks if c != callback]
            removed = len(self._callbacks) - len(kept)
            self._callbacks = kept
        return removed

    def _wait(self, timeout=None):
        if not self._event.wait(timeout):
            raise TimeoutError()
        if self._cancelled:
            raise CancelledError()
        if self._exception is not None:
            raise __vis_clone_exception__(self._exception) from None
        return self._result

    def result(self, timeout=None):
        # asyncio's `Future.result()` refuses a pending future; the
        # `concurrent.futures` one blocks with a timeout, and
        # `run_coroutine_threadsafe` hands callers that shape. Both are honoured.
        if timeout is None and not self._event.is_set():
            raise InvalidStateError("Result is not set.")
        return self._wait(timeout)

    def exception(self, timeout=None):
        if timeout is None and not self._event.is_set():
            raise InvalidStateError("Exception is not set.")
        if not self._event.wait(timeout):
            raise TimeoutError()
        if self._cancelled:
            raise CancelledError()
        return self._exception

    def __vis_bounded__(self, timeout):
        return __vis_Blocking__(self._wait, timeout)

    def __await__(self):
        return __vis_Blocking__(self._wait).__await__()


class __vis_Settled__:
    # One finished slot of `as_completed`, handed back as an awaitable so the
    # `for aw in as_completed(...): await aw` idiom keeps working.
    __slots__ = ("ok", "value")

    def __init__(self, ok, value):
        self.ok = ok
        self.value = value

    def __await__(self):
        value = self.value
        self.value = None
        if False:
            yield
        if not self.ok:
            raise __vis_clone_exception__(value) from None
        return value


class __vis_Slot__:
    # Wraps ONE `as_completed` child so a failure is RECORDED instead of raised:
    # concurrent gather aborts every sibling on the first exception, and
    # `as_completed` owes the caller the other results.
    __slots__ = ("aw", "sink", "lock")

    def __init__(self, aw, sink, lock):
        self.aw = aw
        self.sink = sink
        self.lock = lock

    def __await__(self):
        ok = True
        try:
            value = yield from __vis_awaitable__(self.aw).__await__()
        except BaseException as exc:
            value = exc
            ok = False
        self.aw = None
        if not ok:
            value = __vis_clean_exception__(value)
        with self.lock:
            self.sink.append(__vis_Settled__(ok, value))
        return None


def __vis_as_completed__(aws, *, timeout=None):
    # Real asyncio STREAMS completions; this cannot - the driver is one thread
    # and `gather` is where concurrency lives - so the whole batch is settled
    # CONCURRENTLY first and handed back in COMPLETION order, which is the part
    # callers actually depend on. An early `break` therefore saves no work.
    mod = __vis_sync_mod__()
    lock = mod.Lock()
    done = []
    started = __vis_time__.monotonic()
    __vis_settle__(__vis_Gather__([__vis_Slot__(aw, done, lock) for aw in aws], False))
    if timeout is not None and __vis_time__.monotonic() - started > float(timeout):
        raise TimeoutError()
    yield from done


class __vis_Timeout__:
    # `async with asyncio.timeout(s)` on a runtime with no loop: nothing can
    # interrupt a blocking call from outside, so the deadline is checked on EXIT
    # - the same cooperative contract `wait_for` already documents. A single
    # bounded wait is better expressed as `wait_for`, which pushes the deadline
    # into the wait itself.
    __slots__ = ("_when", "_expired")

    def __init__(self, when):
        self._when = when
        self._expired = False

    def when(self):
        return self._when

    def reschedule(self, when):
        self._when = when

    def expired(self):
        return self._expired

    async def __aenter__(self):
        return self

    async def __aexit__(self, typ, val, tb):
        if self._when is not None and __vis_time__.monotonic() >= self._when:
            self._expired = True
            if typ is None or issubclass(typ, CancelledError):
                raise TimeoutError()
        return False


class __vis_Runner__:
    # `asyncio.Runner` is a context manager around `run`; there is no loop to own
    # or close, so it is exactly that and nothing more.
    __slots__ = ()

    def __init__(self, *, debug=None, loop_factory=None):
        pass

    def __enter__(self):
        return self

    def __exit__(self, typ, val, tb):
        return False

    def run(self, coro, *, context=None):
        return __vis_drive__(coro)

    def get_loop(self):
        return __vis_asyncio__

    def close(self):
        return None


class __vis_AsyncioMeta__(type):
    # Every name real asyncio has that this runtime deliberately does NOT. The
    # answer stays an AttributeError - feature probes must keep getting False
    # from `hasattr` - but one that says WHY and what to reach for instead of
    # "type object '__vis_asyncio__' has no attribute 'open_connection'".
    __vis_absent__ = {
        "open_connection": "no selector loop owns sockets here - use `requests`/`httpx`",
        "open_unix_connection": "no selector loop owns sockets here",
        "start_server": "the sandbox does not serve asyncio sockets",
        "start_unix_server": "the sandbox does not serve asyncio sockets",
        "StreamReader": "streams need a selector loop",
        "StreamWriter": "streams need a selector loop",
        "create_subprocess_exec": "guest processes are never spawned - use `shell(...)`",
        "create_subprocess_shell": "guest processes are never spawned - use `shell(...)`",
        "run_forever": "nothing runs a loop - drive a coroutine with `asyncio.run(...)`",
        "call_soon": "no loop schedules callbacks - call it, or `await` a coroutine",
        "call_soon_threadsafe": "no loop schedules callbacks - complete an `asyncio.Future`",
        "call_later": "no loop schedules callbacks - `await asyncio.sleep(...)` first",
        "call_at": "no loop schedules callbacks - `await asyncio.sleep(...)` first",
        "add_reader": "there is no selector to register a descriptor with",
        "add_writer": "there is no selector to register a descriptor with",
        "add_signal_handler": "the sandbox does not deliver signals to guest code",
        "get_event_loop_policy": "there is no loop object to configure",
        "set_event_loop_policy": "there is no loop object to configure",
    }

    def __getattr__(cls, name):
        if name.startswith("__"):
            raise AttributeError(name)
        why = __vis_AsyncioMeta__.__vis_absent__.get(
            name,
            "this shim covers coroutines, tasks, queues, synchronization and"
            " `to_thread`; concurrency itself is `gather` on the host's bounded"
            " platform pool",
        )
        raise AttributeError("asyncio." + name + " is unavailable in vis: " + why)


def __vis_tool_proto__(nm, params):
    # A stub carrying ONLY a parameter list. The tool itself has to stay
    # permissive — GraalPy folds `tool(a=1)` into ONE trailing dict positional,
    # so `(*a, **k)` is the only shape that can accept every call the host
    # supports — while `inspect.signature` follows `__wrapped__`. The stub is
    # therefore how a tool REPORTS the parameters the host declared for it
    # without narrowing what it accepts. It has no body and no registered
    # source, so `inspect.getsource` on a tool keeps refusing: the
    # implementation is a host callable, not Python.
    ns = {}
    try:
        __vis_real_exec__("def __vis_proto__(" + params + "): pass", ns)
    except SyntaxError:
        return None
    proto = ns["__vis_proto__"]
    proto.__name__ = nm
    proto.__qualname__ = nm
    return proto


def __vis_stamp_tool__(fn, nm):
    # Give ONE tool wrapper the two facts only the host knows: the contract
    # `doc(nm)` answers (as `__doc__`, so `help(tool)` and `inspect.getdoc`
    # answer it too) and the declared parameter list (through `__wrapped__`, so
    # `inspect.signature` stops reporting the trampoline's own `(*a, **k)`).
    # The host doc WINS every time: re-stamping is how a doc seeded AFTER the
    # binding still lands — an aliased extension binds per turn and seeds its
    # doc one step later.
    g = globals()
    doc = (g.get("__vis_docs__") or {}).get(nm)
    if doc:
        try:
            fn.__doc__ = doc
        except Exception:
            pass
    params = (g.get("__vis_sigs__") or {}).get(nm)
    if params is not None and getattr(fn, "__wrapped__", None) is None:
        proto = __vis_tool_proto__(nm, params)
        if proto is not None:
            fn.__wrapped__ = proto
    return fn


def __vis_stamp_tools__(names=None):
    # HOST-CALLED once docs/signatures are seeded, and again after a late
    # binding: (re)stamp every deferred tool, or only the ones named. A name the
    # model has since shadowed with its own function is skipped — the marker is
    # set by `__vis_deferred__` and by nothing else.
    g = globals()
    for nm in list(g.get("__vis_tool_names__") or [] if names is None else names):
        fn = g.get(nm)
        if callable(fn) and getattr(fn, "__vis_is_tool__", False):
            __vis_stamp_tool__(fn, nm)


def __vis_publish_tool__(fn, nm):
    # Name, MARK, stamp and register one wrapper as a sandbox tool — the single
    # place a callable becomes a name the model can introspect. The mark is what
    # `__vis_stamp_tools__` re-stamps and what keeps it off a name the model has
    # since shadowed with a function of its own.
    fn.__name__ = nm
    fn.__qualname__ = nm
    fn.__vis_is_tool__ = True
    __vis_stamp_tool__(fn, nm)
    g = globals()
    names = g.get("__vis_tool_names__")
    if names is None:
        names = []
        g["__vis_tool_names__"] = names
    if nm not in names:
        names.append(nm)
    return fn


def __vis_deferred__(realfn, nm="tool"):
    def __vis_tool__(*a, **k):
        return __vis_Call__(realfn, a, k, nm)

    return __vis_publish_tool__(__vis_tool__, nm)


class __vis_asyncio__(metaclass=__vis_AsyncioMeta__):
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
    Future = __vis_Future__
    Lock = __vis_Lock__
    Event = __vis_Event__
    Condition = __vis_Condition__
    Semaphore = __vis_Semaphore__
    BoundedSemaphore = __vis_BoundedSemaphore__
    Barrier = __vis_Barrier__
    BrokenBarrierError = BrokenBarrierError
    Queue = __vis_Queue__
    LifoQueue = __vis_LifoQueue__
    PriorityQueue = __vis_PriorityQueue__
    QueueEmpty = QueueEmpty
    QueueFull = QueueFull
    Runner = __vis_Runner__
    Timeout = __vis_Timeout__

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
        return __vis_is_awaitable__(v)

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
    def as_completed(aws, *, timeout=None):
        return __vis_as_completed__(aws, timeout=timeout)

    @staticmethod
    def timeout(delay):
        when = None if delay is None else __vis_time__.monotonic() + float(delay)
        return __vis_Timeout__(when)

    @staticmethod
    def timeout_at(when):
        return __vis_Timeout__(when)

    @staticmethod
    def create_future():
        return __vis_Future__()

    @staticmethod
    def run_coroutine_threadsafe(coro, loop=None):
        # There is no loop thread to hand the coroutine to, so it runs HERE, on
        # the calling thread, and the future comes back already settled - which
        # is exactly what the caller's `.result()` asks for.
        future = __vis_Future__()
        try:
            future.set_result(__vis_drive__(coro))
        except BaseException as exc:
            future.set_exception(exc)
        return future

    @staticmethod
    def run_in_executor(executor, func, *args):
        # `loop.run_in_executor(None, fn, ...)` is `to_thread`; the executor
        # argument is meaningless when the host owns the only pool.
        return __vis_to_thread__(func, *args)

    @staticmethod
    def time():
        return __vis_time__.monotonic()

    @staticmethod
    def is_running():
        return True

    @staticmethod
    def is_closed():
        return False

    @staticmethod
    def close():
        return None

    @staticmethod
    def stop():
        return None

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
    # block-local, so `with open(p) as grep:` shadows `grep` only for this
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


def __vis_check_tool_shadow__(tree, src):
    # A top-level `def cat(...)` or `class grep:` named after a BOUND TOOL used to
    # be accepted in silence and then quietly dropped: the name is left out of the
    # `global` list in `__vis_run_async__`, so the definition lived and died inside
    # THAT block while the next one silently got the tool back — and the helper was
    # never persisted either, because the snapshot skips protected names. A helper
    # the session cannot keep is not a helper. Refuse it where it is written, with
    # the one fix: give it its own name.
    # Only TOP-LEVEL defs are the trap. A nested `def doc(...)` inside another
    # function is an ordinary local and never touches the sandbox namespace, and a
    # plain `search = re.search(...)` stays a block-local shadow on purpose.
    prot = set(globals().get("__vis_protected_names__") or [])
    if not prot:
        return
    heads = (
        __vis_ast__.FunctionDef,
        __vis_ast__.AsyncFunctionDef,
        __vis_ast__.ClassDef,
    )
    for node in tree.body:
        if isinstance(node, heads) and node.name in prot:
            raise __vis_syntax_error__(
                "`"
                + node.name
                + "` is a bound tool: defining it here would shadow it for THIS"
                + " block only, would not persist, and the next block gets the"
                + " tool back. Name the helper something else (`"
                + node.name
                + "_mine`) and call the tool by its own name.",
                node,
                src,
            )


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
    #   • `import ssl` / `select` / `selectors`        ->  passthrough. They were
    #     once DELETED as native-crash risks; on this build they import and run
    #     (ssl reports "OpenSSL compatible GraalVM JSSE"), and deleting the
    #     statement made an unverified HTTPS request unfixable from a block: the
    #     escape hatch vanished silently and the later use raised a NameError
    #     nobody could explain. A real future incompatibility has to fail LOUDLY
    #     at import, never by editing the statement away.
    #   • an import binding a tool name (`import doc`)  ->  KEPT; it just shadows
    #     that name for THIS block (the wrapper never declares it `global`).
    # Everything else (json, re, ...) is untouched; the ORIGINAL src is returned
    # when nothing changed (line numbers / formatting preserved).
    prot = set(globals().get("__vis_protected_names__") or [])

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
                if base == "asyncio":
                    newbody.append(bind(a.asname or "asyncio", None))
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
            else:
                newbody.append(node)
        else:
            newbody.append(node)
    if not changed:
        return src
    tree.body = newbody
    __vis_ast__.fix_missing_locations(tree)
    return __vis_ast__.unparse(tree)


def __vis_body_awaits__(fn):
    # Does THIS function's own body await? Walks the body but stops at every
    # nested function scope (`def`, `async def`, `lambda`), whose awaits belong to
    # that scope, not this one. A comprehension is NOT a stop: `[await f(x) for x
    # in xs]` is exactly what makes the enclosing function async.
    stack = list(fn.body)
    while stack:
        node = stack.pop()
        if isinstance(
            node,
            (
                __vis_ast__.FunctionDef,
                __vis_ast__.AsyncFunctionDef,
                __vis_ast__.Lambda,
            ),
        ):
            continue
        if isinstance(
            node,
            (__vis_ast__.Await, __vis_ast__.AsyncFor, __vis_ast__.AsyncWith),
        ):
            return True
        stack.extend(__vis_ast__.iter_child_nodes(node))
    return False


class __vis_AsyncDefFix__(__vis_ast__.NodeTransformer):
    # PROMOTE a plain `def` that awaits to `async def`. The block itself already
    # runs as a coroutine (top-level `await` is the normal way to call a tool), so
    # a model factoring those same lines into the helper the system prompt asks
    # for hit a bare `SyntaxError: 'await' outside async function` — the ONE
    # keyword the sandbox exists to make ordinary, refused the moment it moved
    # inside a function. Promotion is the same answer the top level already gets:
    # the helper is awaited at its call site, like any coroutine.
    #
    # `visit` is depth-first, so a nested `def` is promoted before its parent is
    # judged, and an await that belongs to the inner helper never drags the outer
    # one along.
    def visit_FunctionDef(self, node):
        self.generic_visit(node)
        if not __vis_body_awaits__(node):
            return node
        promoted = __vis_ast__.AsyncFunctionDef(
            name=node.name,
            args=node.args,
            body=node.body,
            decorator_list=node.decorator_list,
            returns=node.returns,
            type_comment=getattr(node, "type_comment", None),
        )
        type_params = getattr(node, "type_params", None)
        if type_params is not None:
            promoted.type_params = type_params
        return __vis_ast__.copy_location(promoted, node)


class __vis_AwaitFix__(__vis_ast__.NodeTransformer):
    # Wrap the operand of every `await EXPR` as `await __vis_awaitable__(EXPR)`
    # so awaiting a value that is NOT a real awaitable (a tool result that
    # already settled — `x = grep(...); await x`) returns the value instead of
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


def __vis_normalize_module__(tree, flags):
    # THE block rewrite, and the ONLY one: every source this runtime runs is
    # normalized here — the block the model just wrote (`__vis_run_async__`) AND the
    # helpers a fresh process replays from the session snapshot
    # (`__vis_restore_defs__`). Replaying RAW source made a restored helper a
    # different language from the one the session had written: `await` on an
    # already-settled value RAISED instead of passing through (`__vis_AwaitFix__`),
    # a plain `def` whose body awaits did not compile at all and was dropped from the
    # toolbox (`__vis_AsyncDefFix__`), and a tool call inside it stayed a deferred
    # thunk (the settle wrap below). A helper must behave after a gateway restart
    # exactly as it did in the block that defined it.
    tree = __vis_AsyncDefFix__().visit(tree)
    tree = __vis_AwaitFix__().visit(tree)
    tree = __vis_StarImportFix__().visit(tree)
    tree = __vis_AnnFix__(
        bool(flags & __vis_future_mod__.annotations.compiler_flag)
    ).visit(tree)

    # AUTO-SETTLE inline, exactly like the sync per-form path: wrap the value of
    # every assignment / bare expression in `__vis_settle__(...)` so a bare
    # deferred tool call (`res = grep(...)`, or a lone `grep(...)`) RUNS in place
    # — later statements (and `print(res)`) then see the real value, not a
    # `__vis_Call__` thunk. settle is identity for plain values and idempotent for
    # thunks already consumed by `await`/`gather`, so wrapping is always safe.
    #
    # EVERY statement, at EVERY depth — not only `tree.body`. A statement nested
    # in `try:` / `if:` / `for:` / `with:` or inside a `def` used to keep its
    # thunk instead: `for p in paths: patch(p, edits)` built N thunks and ran
    # NONE of them, and `try: r = cat(p)` bound a thunk whose refusal then
    # surfaced OUTSIDE the very handler written for it. A tool call now runs
    # where it is written. Nested statements use the cheap `__vis_settle_stmt__`
    # (thunks only, no `__vis_pyify__` walk) because a loop body is hot code;
    # TOP-LEVEL keeps full `__vis_settle__`, which also rebuilds foreign proxies.
    # A call in EXPRESSION position still defers — that is the seam `gather`
    # needs, so `hs = [shell(c) for c in cmds]` still batches — and so does every
    # statement inside an `async def`: a coroutine is where HOLDING an awaitable
    # (`t = asyncio.to_thread(f, x)` ... `await gather(t, u)`) is the idiom, and
    # `await` is right there to spend it. The ONE statement that settles ANYWAY is
    # `return`: the value a function hands its CALLER has left the scope that could
    # await it, so it goes through `__vis_settle_return__` instead.
    def __vis_wrap__(v, __vis_fn__="__vis_settle__"):
        return __vis_ast__.Call(
            func=__vis_ast__.Name(id=__vis_fn__, ctx=__vis_ast__.Load()),
            args=[v],
            keywords=[],
        )

    __vis_nodes__ = list(__vis_ast__.walk(tree))
    __vis_top__ = {id(__vis_node__) for __vis_node__ in tree.body}
    __vis_coro__ = set()
    for __vis_node__ in __vis_nodes__:
        if isinstance(__vis_node__, __vis_ast__.AsyncFunctionDef):
            for __vis_sub__ in __vis_ast__.walk(__vis_node__):
                __vis_coro__.add(id(__vis_sub__))
    for __vis_node__ in __vis_nodes__:
        if not isinstance(
            __vis_node__,
            (
                __vis_ast__.Assign,
                __vis_ast__.AnnAssign,
                __vis_ast__.Expr,
                # `return cat(p)` in a helper is a STATEMENT too: without this the
                # caller got a thunk back and `'x' + helper(...)` died with a
                # TypeError naming `__vis_Call__` instead of doing the read.
                __vis_ast__.Return,
            ),
        ):
            continue
        __vis_v__ = __vis_node__.value
        if __vis_v__ is None:  # a bare `x: int` annotation has no value
            continue
        if id(__vis_node__) in __vis_top__:
            __vis_node__.value = __vis_wrap__(__vis_v__)
        elif isinstance(__vis_v__, __vis_ast__.Constant):
            # A bare string statement is a DOCSTRING: wrapping it in a call would
            # strip `__doc__` off the def or class it opens. A constant can never be
            # a thunk, so nothing is lost by leaving every one of them alone.
            continue
        elif isinstance(__vis_node__, __vis_ast__.Return):
            # A `return` LEAVES the scope that could still `await`, so it settles at
            # every helper kind, `async def` included, and reaches into the container
            # the helper answers with. See `__vis_settle_return__`.
            __vis_node__.value = __vis_wrap__(__vis_v__, "__vis_settle_return__")
        elif id(__vis_node__) in __vis_coro__:
            continue
        else:
            __vis_node__.value = __vis_wrap__(__vis_v__, "__vis_settle_stmt__")
    # Every wrap above is a NEW node with no position: `compile()` refuses an AST
    # whose expr is missing `lineno` (the whole snapshot replay silently restored
    # nothing), so locations are filled in once the tree is final.
    __vis_ast__.fix_missing_locations(tree)
    return tree


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


__vis_blocks_kept__ = 128


def __vis_source_is_live__(name):
    # Does a definition the model can still CALL come from this block? A `def`
    # persists for the WHOLE SESSION, across turns — so dropping its source made a
    # helper unreadable while it was still callable: `inspect.getsource(helper)`
    # raised OSError and the only way to change it was to re-paste it from memory.
    # Best effort, and deliberately cheap: a function bound to a global NAME, or a
    # CLASS whose body that block holds — a `@dataclass` has no method the session
    # wrote, so a class is pinned by its own `class <Name>` line instead.
    # Engine names are skipped — `__vis_main__` is THIS runtime's wrapper for the
    # block that ran last, so counting it would pin one dead block forever.
    for __n__, __v__ in list(globals().items()):
        if __n__.startswith("__vis_") or __n__.startswith("__Vis"):
            continue
        try:
            __c__ = __vis_def_code__(__v__)
            if __c__ is not None and __c__.co_filename == name:
                return True
            if isinstance(__v__, type):
                __e__ = __vis_linecache__.cache.get(name)
                if __e__ and ("class " + __v__.__name__) in "".join(__e__[2]):
                    return True
        except Exception:
            pass
    return False


def __vis_evict_sources__(kept):
    # Oldest-first, but a block that still backs a live definition is PINNED and
    # skipped instead of dropped: the cap bounds DEAD blocks, not live ones. A
    # redefinition unbinds the old function, so the block that held it stops being
    # live and evicts on the next pass — this cannot grow without a live referent.
    # `len(kept) - 1` because the NEWEST entry is the block about to RUN: nothing is
    # bound from it yet, so a liveness test would call it dead and drop the source
    # its own traceback and assert introspection are about to read.
    over = len(kept) - __vis_blocks_kept__
    i = 0
    while over > 0 and i < len(kept) - 1:
        if __vis_source_is_live__(kept[i]):
            i += 1
        else:
            __vis_linecache__.cache.pop(kept.pop(i), None)
            over -= 1


def __vis_register_source__(src):
    # Make a block READABLE BACK. Every `def`/`class` the model writes lands in
    # a code object whose co_filename is synthetic, and nothing on disk backs
    # it: `inspect.getsource`, `inspect.getsourcelines`, `traceback`'s source
    # echo and the pytest shim's assert introspection all read through
    # `linecache`, so without this entry each of them failed with
    # `OSError: could not get source code` — the sandbox could RUN code it
    # could not SHOW.
    #
    # The name is UNIQUE PER BLOCK. Two blocks share no line numbering, so one
    # shared `<prog>` would hand a LATER block's text back for an EARLIER
    # block's function — wrong source, silently, which is worse than the error.
    # Only the newest `__vis_blocks_kept__` sources stay resident, EXCEPT the
    # ones that still back a live definition — see `__vis_evict_sources__`.
    g = globals()
    # The namespace as the HOST handed it over — everything auto-imported or bound
    # before the session's first block. Captured here because this runs before that
    # block (and before a restore), and it is what keeps `__vis_defs_snapshot__`
    # from re-emitting shims and tools as if the session had defined them.
    if "__vis_boot_names__" not in g:
        g["__vis_boot_names__"] = set(g.keys())
    n = int(g.get("__vis_block_no__") or 0) + 1
    g["__vis_block_no__"] = n
    name = "<prog:" + str(n) + ">"
    # THIS block's source, for the pytest shim's inline assert introspection.
    g["__vis_src__"] = src
    # mtime None means "no file behind it", which is exactly what
    # `linecache.checkcache` needs to leave the entry alone instead of
    # dropping it on the next lookup.
    __vis_linecache__.cache[name] = (len(src), None, src.splitlines(True), name)
    kept = g.get("__vis_block_names__")
    if kept is None:
        kept = []
        g["__vis_block_names__"] = kept
    kept.append(name)
    __vis_evict_sources__(kept)
    return name


def __vis_def_code__(v):
    # The code object behind a callable, THROUGH a decorator: `functools.wraps` and
    # `lru_cache` hand back a wrapper with no `__code__` of its own, and a helper
    # that vanished from `defs()` the moment it was cached is a helper the session
    # cannot read back or restore.
    code = getattr(v, "__code__", None)
    if code is None:
        wrapped = getattr(v, "__wrapped__", None)
        if wrapped is not None:
            code = getattr(wrapped, "__code__", None)
    return code


def __vis_user_defs__():
    # Every FUNCTION this session's own blocks defined, sorted by name. A `def`
    # is recognized by its code object's SYNTHETIC filename — the `<prog:N>`
    # `__vis_register_source__` minted for the block — so an imported function
    # (backed by a real file) and every engine internal are out.
    __vis_out__ = []
    __vis_prot__ = set(globals().get("__vis_protected_names__") or [])
    for __n__, __v__ in list(globals().items()):
        if __n__.startswith("_") or __n__ in __vis_prot__:
            continue
        __c__ = __vis_def_code__(__v__)
        __f__ = getattr(__c__, "co_filename", "") if __c__ is not None else ""
        if isinstance(__f__, str) and __f__.startswith("<prog:"):
            __vis_out__.append((__n__, __v__))
    __vis_out__.sort()
    return __vis_out__


def __vis_def_gist__(text, limit=72):
    # The one line a LISTING can afford: the docstring's first non-blank line,
    # whitespace collapsed and bounded. The whole of it stays one `doc(name)`
    # away — the same push/pull a tool's page gets.
    for line in str(text or "").splitlines():
        line = " ".join(line.split())
        if line:
            return line if len(line) <= limit else line[: limit - 1].rstrip() + "…"
    return ""


def __vis_def_docs__():
    # The DOCSTRING of every helper this session defined, keyed by name, and ""
    # when it has none. The host reads this back as that helper's document: a
    # documented helper owns a `doc(name)` page and is findable by `apropos`,
    # while an undocumented one carries an EMPTY document, which is what keeps a
    # bare handle (`where`, `vars`) out of a described search.
    import inspect

    out = {}
    for n, fn in __vis_user_defs__():
        try:
            out[n] = (inspect.getdoc(fn) or "").strip()
        except Exception:
            out[n] = ""
    return out


def __vis_def_calls__():
    # The CALL LINE of every helper this session defined — `widen(a, b=2)`. A page
    # that never shows how to call what it documents is the one page nobody can
    # act on, so a helper's page opens with its signature like every tool's does.
    import inspect

    out = {}
    for n, fn in __vis_user_defs__():
        try:
            out[n] = n + str(inspect.signature(fn))
        except Exception:
            out[n] = n + "(...)"
    return out


def defs(name=None):
    """The helpers THIS session defined, and their source — plain text.

    `defs()` lists every function your blocks defined: name, signature, the
    block it came from, its length, and the first line of its DOCSTRING — write
    one and the listing says what each helper is FOR. `defs("name")` returns
    that one's source, so a helper is REFINED by reading back what it already
    says instead of being re-pasted from memory.

    A `def` persists for the whole session across turns, and its definitions
    are re-created automatically in a fresh sandbox after a restart — a
    restored one is marked `(restored)`.
    """
    import inspect

    live = __vis_user_defs__()
    if name is not None:
        fn = dict(live).get(name)
        if fn is None:
            known = ", ".join(n for n, _ in live) or "none"
            raise NameError(
                "defs: this session defined no function named "
                + repr(name)
                + " — defined here: "
                + known
            )
        try:
            return inspect.getsource(fn)
        except Exception as exc:
            return "# source unavailable for " + name + ": " + str(exc)
    if not live:
        return (
            "no functions defined by this session yet — a `def` in any block joins this "
            "list, persists across turns, and is restored into a fresh sandbox after a "
            "gateway restart"
        )
    restored = globals().get("__vis_restored_block__")
    docs = __vis_def_docs__()
    rows = []
    for n, fn in live:
        try:
            sig = str(inspect.signature(fn))
        except Exception:
            sig = "(...)"
        where = getattr(__vis_def_code__(fn), "co_filename", "?")
        if where == restored:
            where += " (restored)"
        try:
            size = str(len(inspect.getsourcelines(fn)[0])) + " lines"
        except Exception:
            size = "source unavailable"
        rows.append((n + sig, where, size, __vis_def_gist__(docs.get(n))))
    width = max(len(r[0]) for r in rows)
    place = max(len(r[1]) for r in rows)
    span = max(len(r[2]) for r in rows)
    body = "\n".join(
        (
            "  "
            + r[0].ljust(width)
            + "  "
            + r[1].ljust(place)
            + "  "
            + r[2].ljust(span)
            + "  "
            + r[3]
        ).rstrip()
        for r in rows
    )
    head = (
        str(len(rows))
        + (" definition" if len(rows) == 1 else " definitions")
        + " in this sandbox"
    )
    # The nudge belongs where the gap SHOWS: an undocumented helper is a row with
    # an empty last column, and one line would give it a page.
    bare = sum(1 for r in rows if not r[3])
    tail = 'defs("name") returns one\'s source.'
    if bare:
        tail += (
            " "
            + str(bare)
            + (" has" if bare == 1 else " have")
            + " no docstring — one line of it would be the gist above, the whole"
            + " of it a doc(name) page the next turn can search."
        )
    return head + "\n" + body + "\n" + tail


def __vis_bound_name__(src):
    # The module-level NAME a chunk actually binds — which is NOT always the global
    # it is stored under. `twice = outer()` reads back as the INNER `def inner`,
    # and `alias = public` reads back as `public`'s own source; emitting either
    # under its own name silently lost the name the session calls it by.
    try:
        mod = __vis_ast__.parse(src)
    except Exception:
        return None
    if not mod.body:
        return None
    st = mod.body[-1]
    if isinstance(
        st,
        (__vis_ast__.FunctionDef, __vis_ast__.AsyncFunctionDef, __vis_ast__.ClassDef),
    ):
        return st.name
    if (
        isinstance(st, __vis_ast__.Assign)
        and len(st.targets) == 1
        and isinstance(st.targets[0], __vis_ast__.Name)
    ):
        return st.targets[0].id
    return None


def __vis_chunk__(text, name):
    # ONE independently valid statement group for the snapshot. Every chunk is
    # DEDENTED and PARSE-CHECKED before it is written, because a helper defined
    # inside `if:`/`try:`/another `def` reads back INDENTED: one such line made the
    # whole file unparseable, and an unparseable file cost the session EVERY
    # helper, not just that one. A chunk that still will not parse is dropped here,
    # where dropping it is free.
    import textwrap

    try:
        src = textwrap.dedent(text).rstrip()
    except Exception:
        return None
    bound = __vis_bound_name__(src)
    if bound is None:
        return None
    if bound != name:
        src += "\n" + name + " = " + bound
    return (src + "\n", bound)


def __vis_class_source__(cls):
    # `inspect.getsource` cannot read a SANDBOX class: it resolves the file through
    # `cls.__module__`, and a block is not a module. The class body is in linecache
    # under the block that defined it, so find it THERE — a helper that returns a
    # session class is a NameError after a restart if the class does not come back
    # with it. Searched newest block first (a redefinition wins) and by NAME, not
    # through a method: a `@dataclass` has no method the session wrote, and reading
    # it through one dropped the whole class. The decorator lines are part of the
    # class — `@dataclass` is what gives it its `__init__`.
    name = getattr(cls, "__name__", "")
    if not name:
        return None
    for block_name in reversed(list(globals().get("__vis_block_names__") or [])):
        entry = __vis_linecache__.cache.get(block_name)
        if not entry:
            continue
        block = "".join(entry[2])
        if ("class " + name) not in block:
            continue
        try:
            tree = __vis_ast__.parse(block)
        except Exception:
            continue
        for node in __vis_ast__.walk(tree):
            if isinstance(node, __vis_ast__.ClassDef) and node.name == name:
                lines = block.splitlines()
                start = min([node.lineno] + [d.lineno for d in node.decorator_list])
                return "\n".join(lines[start - 1 : node.end_lineno])
    return None


def __vis_literal_ok__(rep):
    # Does this repr read back as a LITERAL? `float('nan')` reprs as bare `nan`,
    # which is a NameError at restore — and before the chunk check that single
    # statement failed the whole file.
    try:
        __vis_ast__.literal_eval(rep)
        return True
    except Exception:
        return False


def __vis_block_order__(filename, code):
    # Definition order: block number, then line. A decorator that is itself a
    # session helper has to be defined before the helper it decorates.
    try:
        return (int(filename[6:-1]), int(getattr(code, "co_firstlineno", 0)))
    except Exception:
        return (0, 0)


def __vis_defs_snapshot__():
    # Source text that RE-CREATES this session's helpers in a FRESH process.
    # The sandbox dies with the process, so a gateway restart used to lose every
    # helper the session had refined while its TRANSCRIPT still showed them —
    # the next call was a NameError. The host writes this snapshot beside the
    # session and feeds it back through `__vis_restore_defs__`.
    #
    # Only names the SESSION created are considered: `__vis_boot_names__` is the
    # global namespace as the host handed it over, so auto-imported shims and
    # bound tools are never re-emitted. Every chunk is validated on its own, so a
    # helper that cannot be re-created costs ONLY itself.
    import inspect
    import sys

    g = globals()
    prot = set(g.get("__vis_protected_names__") or [])
    boot = set(g.get("__vis_boot_names__") or [])
    module_type = type(inspect)
    imports = []
    consts = []
    classes = []
    funcs = []
    aliases = []
    seen = {}
    bound_names = set()
    for n, v in sorted(g.items()):
        if n.startswith("__") or n in prot or n in boot:
            continue
        if isinstance(v, module_type):
            mod = getattr(v, "__name__", "")
            if mod:
                imports.append(
                    "import " + mod if mod == n else "import " + mod + " as " + n
                )
            continue
        code = __vis_def_code__(v)
        f = getattr(code, "co_filename", "") if code is not None else ""
        if isinstance(f, str) and f.startswith("<prog:"):
            key = id(code)
            if key in seen:
                # Two names, ONE function: emit the source once. A name the chunk
                # already binds needs no alias line at all.
                if n not in bound_names:
                    aliases.append(n + " = " + seen[key] + "\n")
                continue
            try:
                raw = inspect.getsource(v)
            except Exception:
                continue
            chunk = __vis_chunk__(raw, n)
            if chunk:
                seen[key] = n
                bound_names.add(chunk[1])
                funcs.append((__vis_block_order__(f, code), chunk[0]))
            continue
        if isinstance(v, type):
            body = __vis_class_source__(v)
            chunk = __vis_chunk__(body, n) if body else None
            if chunk:
                classes.append(chunk[0])
                bound_names.add(chunk[1])
                continue
        if callable(v):
            # A callable this session IMPORTED from a real module — `from pathlib
            # import Path`. The helper that calls it needs the NAME at call time.
            mod = getattr(v, "__module__", "") or ""
            qual = getattr(v, "__qualname__", "") or ""
            owner = sys.modules.get(mod)
            if (
                owner is not None
                and qual
                and "." not in qual
                and getattr(owner, "__file__", None)
                and getattr(owner, qual, None) is v
            ):
                imports.append(
                    "from " + mod + " import " + qual
                    if qual == n
                    else "from " + mod + " import " + qual + " as " + n
                )
            continue
        # A plain constant — `root` as a string is the usual closed-over path, and
        # a small dict is the usual config a helper takes as a DEFAULT ARGUMENT
        # (`def run(p, cfg=CFG)`), which resolves at def time: without `CFG` that
        # helper does not come back at all. SIZE FIRST: `repr` of a multi-megabyte
        # blob costs more than the whole snapshot, every block, only to be thrown
        # away by the cap. Only a repr that reads back as a LITERAL is kept.
        if (
            v is None
            or isinstance(v, (int, float, bool))
            or (isinstance(v, str) and len(v) <= 500)
            or (isinstance(v, (dict, list, tuple, set, frozenset)) and len(v) <= 100)
        ):
            rep = repr(v)
            if len(rep) <= 500 and __vis_literal_ok__(rep):
                consts.append(n + " = " + rep)
    if not classes and not funcs:
        return ""
    parts = [
        "# Session helper definitions, re-created automatically in a fresh sandbox.\n"
    ]
    if imports:
        parts.append("\n".join(sorted(set(imports))) + "\n")
    if consts:
        parts.append("\n".join(consts) + "\n")
    parts.extend(classes)
    parts.extend(chunk for _key, chunk in sorted(funcs, key=lambda t: t[0]))
    parts.extend(aliases)
    # A closure is stored under a name its own source never binds (`twice =
    # outer()` reads back as `def inner`), so the chunk had to define that name to
    # rebind it. Drop it again unless the session has one too: a restored sandbox
    # should list the helpers the session HAS, not the private names inside them.
    strays = sorted(b for b in bound_names if b and b not in g)
    if strays:
        parts.append("\n".join("del " + s for s in strays) + "\n")
    return "\n".join(parts)


def __vis_restore_defs__(src):
    # Re-create a previous process's helpers. Registering the source FIRST gives
    # them a real `<prog:N>` entry, so `defs("name")` and `inspect.getsource`
    # read a restored helper back exactly like one defined in this process.
    name = __vis_register_source__(src)
    globals()["__vis_restored_block__"] = name
    # A name that is a BOUND TOOL *now* is never restored, whatever it was when the
    # snapshot was written. `def patch(...)` was an ordinary session helper before
    # the tool existed; exec'ing that file into a process that HAS the tool wrote
    # straight over it, for the whole process, and the count below never noticed
    # because it skips protected names. Statements are dropped by the names they
    # BIND, so an alias line or a constant goes the same way, and the next snapshot
    # no longer carries them — the file heals itself.
    prot = set(globals().get("__vis_protected_names__") or [])
    try:
        # The SAME rewrite a locally-defined helper gets (`__vis_normalize_module__`):
        # a restored helper is the one the session wrote, not a raw re-exec of its text.
        tree = __vis_ast__.parse(src)
        body = __vis_normalize_module__(tree, __vis_future_flags__(tree)).body
    except Exception:
        # A file that will not PARSE cannot be replayed at all — never let that
        # escape as a failure of the whole restore.
        return len(__vis_user_defs__())
    dropped = []
    kept = []
    for stmt in body:
        clash = [n for n in __vis_assigned_names__([stmt]) if n in prot]
        if clash:
            dropped.extend(clash)
        else:
            kept.append(stmt)
    globals()["__vis_restore_dropped__"] = sorted(set(dropped))
    try:
        exec(
            compile(__vis_ast__.Module(body=kept, type_ignores=[]), name, "exec"),
            globals(),
        )
    except Exception:
        # One bad statement must not cost the whole toolbox — a shim this build
        # no longer ships, a helper whose default argument no longer resolves.
        # Replay statement by statement and keep every definition that still
        # loads; each keeps its own line numbers, so its source reads back.
        for stmt in kept:
            try:
                exec(
                    compile(
                        __vis_ast__.Module(body=[stmt], type_ignores=[]), name, "exec"
                    ),
                    globals(),
                )
            except Exception:
                pass
    return len(__vis_user_defs__())


def __vis_run_async__(src):
    g = globals()
    __vis_pin_runtime__(g)
    g["__vis_err_pos__"] = (
        None  # deepest <prog> failing position, computed by __vis_err_pos_now__
    )
    g["__vis_err_obj__"] = (
        None  # the raised exception, stashed for that host-driven lookup
    )
    __vis_file__ = __vis_register_source__(src)
    tree = __vis_ast__.parse(src)
    __vis_flags__ = __vis_future_flags__(tree)
    __vis_check_module_scope__(tree, src)
    __vis_check_compile_traps__(tree, src)
    __vis_check_tool_shadow__(tree, src)
    tree = __vis_normalize_module__(tree, __vis_flags__)
    assigned = __vis_assigned_names__(tree.body)
    # SHADOWING a bound tool / sandbox name is ALLOWED — but only for THIS block.
    # A protected name assigned here is LEFT OUT of the `global` list, so it
    # becomes a plain `__vis_main__` local (exactly like a `for`/`with` target):
    # `search = re.search(...)` reads naturally inside the block and the
    # persistent callable is still there for the next one. Each shadowed name is
    # pre-seeded from globals, so a READ that precedes the shadowing assignment
    # still sees the tool instead of raising UnboundLocalError. A top-level `def`
    # of a tool name is the one exception and never reaches here — a helper that
    # cannot outlive its own block is refused above, by name.
    __vis_prot__ = set(g.get("__vis_protected_names__") or [])
    __vis_shadow__ = [n for n in assigned if n in __vis_prot__ and n in g]
    assigned = [n for n in assigned if n not in __vis_shadow__]
    body = list(tree.body)
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
        __vis_code__ = compile(mod, __vis_file__, "exec", __vis_flags__)
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
            (
                __vis_file__,
                __vis_ln__,
                getattr(__vis_se__, "offset", None),
                __vis_txt__,
            ),
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
    # KWARGS for the DIRECT (never-deferred) host verbs — today `fold_session`.
    # Those stay raw foreign ProxyExecutables, which accept POSITIONAL args ONLY,
    # so `fold_session(key, gist='…')` used to die with `__call__() got an
    # unexpected keyword argument` BEFORE any fold validation ran. Fold **kwargs
    # into ONE trailing dict positional — exactly what `__vis_exec_call__` does
    # for the deferred tools — and the Clojure verb unwraps it (`compaction-verbs`),
    # so keyword and positional calls bind identically.
    def __vis_verb__(*a, **k):
        return realfn(*a, dict(k)) if k else realfn(*a)

    return __vis_publish_tool__(__vis_verb__, nm)


def __vis_kwargs_direct_tools__():
    g = globals()
    for __vis_n__ in list(__vis_direct_names__):
        if __vis_n__ in g and callable(g[__vis_n__]):
            g[__vis_n__] = __vis_direct_kwargs__(g[__vis_n__], __vis_n__)


# ── print delegates to the REAL print: a printed tool-result proxy is pyified into
# a __VisResult__ so it prints as a clean real dict, and a deferred call handed to
# print WITHOUT `await` is settled first. Nothing is captured on the side — the
# block's stdout IS its one result.
__vis_real_print__ = print


def __vis_print__(*__vis_a__, **__vis_kw__):
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
    return __vis_real_print__(*__vis_a__, **__vis_kw__)


print = __vis_print__
