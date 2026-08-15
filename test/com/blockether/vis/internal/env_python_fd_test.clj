(ns com.blockether.vis.internal.env-python-fd-test
  "Descriptor discipline for the sandbox `open` (`resources/vis-python/async_runtime.py`).

   GraalPy does not refcount, so a handle the block DROPS is never finalized:
   its process file descriptor stays open forever. A loop like
   `open(p).read()` over a big tree therefore walks the whole JVM into EMFILE,
   and the first casualty is not Python — `ProcessBuilder` can no longer fork,
   so every later `shell` call dies with the JDK's misleading \"spawn
   helper / JDK version mismatch\" text and the session is wedged for good.

   So the sandbox does the reclamation CPython's refcount would: every handle is
   registered under its descriptor with a WEAK ref, and once that ref is dead
   the descriptor is closed by hand. `__vis_fd_max__` is the ceiling that cannot
   be crossed — reaching it with handles genuinely held open raises a normal
   Python `OSError(EMFILE)` naming the fix, in the block that caused it, instead
   of leaving the session to die later on an unrelated toolchain error."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.env-python :as ep]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.sun.management UnixOperatingSystemMXBean]
           [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
           [java.lang.management ManagementFactory OperatingSystemMXBean]
           [java.net InetSocketAddress]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- open-fd-count
  "Descriptors THIS process holds, straight from the JVM. The sandbox cannot read
   `/dev/fd` (outside the approved roots, by design), and a leak through a door
   that BYPASSES the shim is invisible to the registry by definition — so the only
   honest measure of such a door is the process's own descriptor count."
  ^long []
  (let [^OperatingSystemMXBean bean (ManagementFactory/getOperatingSystemMXBean)]
    (if (instance? UnixOperatingSystemMXBean bean)
      (.getOpenFileDescriptorCount ^UnixOperatingSystemMXBean bean)
      -1)))

(defn- temp-root
  ^String []
  (str (.toAbsolutePath (Files/createTempDirectory "vis-fd-test" (make-array FileAttribute 0)))))

(defn- sandbox
  "A sandbox confined to a fresh temp root, with `F` (readable, 6 bytes) and `W`
   (a writable path) already bound as globals, and the ceiling lowered to 8 so a
   test proves the contract in tens of opens instead of thousands."
  []
  (let
    [root
     (temp-root)

     f
     (str root "/probe.txt")

     ctx
     (:python-context (ep/create-python-context {}
                                                (fn []
                                                  [root])))]

    (spit f "probe\n")
    (ep/run-python-block ctx
                         (str "F = "
                              (pr-str f)
                              "\n"
                              "W = "
                              (pr-str (str root "/written.txt"))
                              "\n"
                              "__vis_fd_max__ = 8\n"
                              "__vis_fd_sweep_at__ = 4\n")
                         "t1/i1")
    ctx))

(defdescribe
  sandbox-fd-reclamation-test
  (it "reclaims the descriptors a block dropped, so a leaking loop cannot exhaust the process"
      ;; The exact shape that wedged a live session: a bare `open(...)` per
      ;; iteration, handle never closed. 40 leaked opens against a ceiling of 8
      ;; only survive if dropped descriptors are actually being reclaimed.
      (let
        [r (ep/run-python-block (sandbox)
                                (str "for _ in range(40):\n" "    h = open(F)\n"
                                     "    del h\n" "len(__vis_fd_registry__)")
                                "t1/i2")]
        (expect (nil? (:error r)))
        (expect (>= 8 (:result r)))))
  (it "reclaims across blocks, so one leaking block cannot poison the next"
      (let [ctx (sandbox)]
        (dotimes [i 6]
          (ep/run-python-block ctx "h = open(F)\ndel h" (str "t1/i" (+ 2 i))))
        (let [r (ep/run-python-block ctx "len(__vis_fd_registry__)" "t1/i9")]
          (expect (nil? (:error r)))
          (expect (>= 8 (:result r))))))
  (it "never refuses honest code that closes what it opens"
      ;; `with` returns every descriptor immediately: 64 opens against a ceiling
      ;; of 8 must be entirely unremarkable.
      (let
        [r (ep/run-python-block (sandbox)
                                (str "n = 0\n"
                                     "for _ in range(64):\n" "    with open(F) as fh:\n"
                                     "        n += len(fh.read())\n" "n")
                                "t1/i2")]
        (expect (nil? (:error r)))
        (expect (= 384 (:result r)))))
  (it "still flushes a dropped writable handle, so what a block wrote is on disk"
      ;; Reclamation must not cost the write-flush guarantee: the block-end
      ;; flush runs BEFORE the sweep.
      (let [ctx (sandbox)]
        (ep/run-python-block ctx "open(W, 'w').write('hello')" "t1/i2")
        (let [r (ep/run-python-block ctx "open(W).read()" "t1/i3")]
          (expect (nil? (:error r)))
          (expect (= "hello" (:result r)))))))

(defdescribe
  sandbox-fd-ceiling-test
  (it "refuses to cross the ceiling when the handles are genuinely held open"
      ;; Nothing is reclaimable here — the list holds every handle — so this is
      ;; the one case that must fail, and it must fail HERE rather than by
      ;; breaking process spawning somewhere else later.
      (let [r (ep/run-python-block (sandbox) "kept = [open(F) for _ in range(64)]" "t1/i2")]
        (expect (some? (:error r)))
        (let [m (str (get-in r [:error :message]))]
          ;; The message has to name the cause, the fix, and the escape hatch:
          ;; the JDK's spawn-helper text taught us what a misdiagnosis costs.
          (expect (str/includes? m "too many open files"))
          (expect (str/includes? m "with open("))
          (expect (str/includes? m "VIS_PY_MAX_OPEN_FILES")))))
  (it "keeps the ceiling reachable from the block that hit it"
      ;; A refused `open` is an ordinary catchable OSError, not a killed block.
      (let
        [r (ep/run-python-block (sandbox)
                                (str "import errno\n"
                                     "kept = []\n" "code = None\n"
                                     "try:\n" "    for _ in range(64):\n"
                                     "        kept.append(open(F))\n" "except OSError as e:\n"
                                     "    code = e.errno\n" "code == errno.EMFILE")
                                "t1/i2")]
        (expect (nil? (:error r)))
        (expect (true? (:result r)))))
  (it "ships a default ceiling well under any process limit, sweeping at half"
      (let
        [ctx
         (:python-context (ep/create-python-context {}))

         r
         (ep/run-python-block ctx "(__vis_fd_max__, __vis_fd_sweep_at__)" "t1/i1")]

        (expect (nil? (:error r)))
        (expect (= [512 256] (:result r))))))

(defdescribe
  sandbox-fd-hardening-test
  (it "reclaims descriptors opened through every door onto the filesystem"
      ;; Shimming only this module's global `open` left three doors wide open:
      ;; `io.open` (a DIFFERENT object from `builtins.open` here), `pathlib.Path.open`
      ;; / `tempfile` (both call `io.open`), and `builtins.open` reached through any
      ;; other module's globals. Each leaked one descriptor per call, and none of it
      ;; showed up in the registry — an untracked handle is invisible there by
      ;; definition — so the process's own count is what has to be watched: the leak
      ;; was ONE descriptor per door per iteration, so 12 iterations move it by ~36
      ;; while a reclaiming sandbox stays under 12. The block returning at all also
      ;; proves no door leads back INTO the shim; a self-call would be a
      ;; RecursionError on the very first `open`. (Each guest `open` costs ~20ms
      ;; interpreted, so the count is the smallest one that still separates the two
      ;; outcomes.)
      (let
        [ctx
         (sandbox)

         before
         (open-fd-count)

         r
         (ep/run-python-block ctx
                              (str "import builtins, io, pathlib\n"
                                   "for _ in range(12):\n" "    h = pathlib.Path(F).open()\n"
                                   "    h = io.open(F)\n" "    h = builtins.open(F)\n"
                                   "    del h\n" "len(__vis_fd_registry__)")
                              "t1/i2")

         grown
         (- (open-fd-count) before)]

        (expect (nil? (:error r)))
        (expect (>= 8 (:result r)))
        (expect (> 12 grown))))
  (it "tracks the layer that owns the descriptor, not the wrapper around it"
      ;; `open()` hands back a STACK (TextIOWrapper -> BufferedReader -> FileIO) and
      ;; dropping the top layer does not end the file: `raw = open(p, "rb").raw`
      ;; reads perfectly afterwards, while `os.close` on that fd is a hard EBADF
      ;; (measured, both). Weak-referencing the TOP layer therefore closed a
      ;; descriptor the block was still reading through. The identity check is the
      ;; contract itself and needs no collection to happen; the read is the
      ;; consequence.
      (let
        [r (ep/run-python-block (sandbox)
                                (str "import gc\n"
                                     "h = open(F, 'rb')\n" "raw = h.raw\n"
                                     "fd = h.fileno()\n"
                                     "owned = __vis_fd_registry__[fd][0]() is raw\n"
                                     "del h\n" "gc.collect()\n"
                                     "__vis_reclaim_fds__(True)\n" "[owned, raw.read().decode()]")
                                "t1/i2")]
        (expect (nil? (:error r)))
        (expect (= [true "probe\n"] (:result r)))))
  (it "leaves a borrowed descriptor to its owner"
      ;; `closefd=False` means the wrapper only BORROWED an fd the block opened
      ;; itself. Tracking it closes a descriptor the block still owns and still
      ;; reads through — EBADF from code that did nothing wrong.
      (let
        [r (ep/run-python-block (sandbox)
                                (str "import gc, os\n" "fd = os.open(F, os.O_RDONLY)\n"
                                     "h = open(fd, 'rb', closefd=False)\n"
                                     "tracked = fd in __vis_fd_registry__\n"
                                     "del h\n" "gc.collect()\n"
                                     "__vis_reclaim_fds__(True)\n" "out = os.read(fd, 5).decode()\n"
                                     "os.close(fd)\n" "[tracked, out]")
                                "t1/i2")]
        (expect (nil? (:error r)))
        (expect (= [false "probe"] (:result r)))))
  (it "keeps its state and its real opener across a runtime reinstall"
      ;; `globals().clear()` is legal Python and makes `ensure-async-runtime!`
      ;; re-eval this whole preamble in the SAME globals. A plain `x = {}` there
      ;; hands the session a brand-new registry and forgets every descriptor it is
      ;; still holding, and re-capturing the opener captures the SHIM now that
      ;; `builtins.open` IS one — a RecursionError on the very next `open`, the same
      ;; hazard the reinstall already unwraps for `print`, one door further down.
      (let
        [ctx
         (sandbox)

         f
         (:result (ep/run-python-block ctx "F" "t1/i2"))

         _
         (ep/run-python-block ctx "for _ in range(6):\n    h = open(F)\n    del h" "t1/i3")

         before
         (ep/run-python-block ctx "id(__vis_fd_registry__)" "t1/i4")

         _
         (ep/run-python-block ctx "globals().clear()" "t1/i5")

         r
         (ep/run-python-block ctx (str "open(" (pr-str f) ").read()") "t1/i6")

         after
         (ep/run-python-block ctx "id(__vis_fd_registry__)" "t1/i7")]

        (expect (nil? (:error r)))
        (expect (= "probe\n" (:result r)))
        (expect (nil? (:error after)))
        (expect (= (:result before) (:result after)))))
  (it "reclaims the raw doors, which never pass through any `open`"
      ;; `io.FileIO(p)` IS the descriptor-owning object and `io.open_code(p)` hands
      ;; one back: neither goes through `open`, so both leaked one descriptor per
      ;; call while only the `open` doors were shimmed (measured, 25 per 25) and
      ;; neither showed up in the registry. `io.FileIO` is an immutable type, so
      ;; the shim is a subclass — this is what proves the subclass is really the
      ;; one being constructed.
      (let
        [ctx
         (sandbox)

         before
         (open-fd-count)

         r
         (ep/run-python-block ctx
                              (str "import io\n"
                                   "h = io.FileIO(F)\n" "c = io.open_code(F)\n"
                                   "seen = [h.fileno() in __vis_fd_registry__,\n"
                                   "        c.fileno() in __vis_fd_registry__]\n"
                                   "h.close()\n" "c.close()\n"
                                   "for _ in range(12):\n" "    g = io.FileIO(F)\n"
                                   "    del g\n" "seen + [len(__vis_fd_registry__) <= 8]")
                              "t1/i2")

         grown
         (- (open-fd-count) before)]

        (expect (nil? (:error r)))
        (expect (= [true true true] (:result r)))
        (expect (> 12 grown))))
  (it "keeps `isinstance` honest after taking over `io.FileIO`"
      ;; The shim is a SUBCLASS, so the raw built INSIDE `open` is not one of its
      ;; instances. Its metaclass forwards the question to the real class, or every
      ;; library asking `isinstance(f.raw, io.FileIO)` would start answering False
      ;; the moment the sandbox loaded.
      (let
        [r (ep/run-python-block (sandbox)
                                (str "import io, pathlib\n" "raw = open(F, 'rb', buffering=0)\n"
                                     "out = [isinstance(raw, io.FileIO),\n"
                                     "       issubclass(io.FileIO, io.RawIOBase),\n"
                                     "       isinstance(io.FileIO(F), io.FileIO),\n"
                                     "       pathlib.Path(F).read_text()]\n"
                                     "raw.close()\n" "out")
                                "t1/i2")]
        (expect (nil? (:error r)))
        (expect (= [true true true "probe\n"] (:result r)))))
  (it "leaves a descriptor borrowed through `io.FileIO` to its owner"
      ;; Same contract as `open(fd, closefd=False)`, at the raw door: the block
      ;; opened that fd itself and still reads through it after the wrapper dies.
      (let
        [r (ep/run-python-block
             (sandbox)
             (str "import gc, io, os\n"
                  "fd = os.open(F, os.O_RDONLY)\n" "__vis_fd_registry__.pop(fd, None)\n"
                  "h = io.FileIO(fd, 'r', False)\n" "tracked = fd in __vis_fd_registry__\n"
                  "del h\n" "gc.collect()\n"
                  "__vis_reclaim_fds__(True)\n" "out = os.read(fd, 5).decode()\n"
                  "os.close(fd)\n" "[tracked, out]")
             "t1/i2")]
        (expect (nil? (:error r)))
        (expect (= [false "probe"] (:result r)))))
  (it "reclaims a HOST resource a block dropped, not only descriptors it opened"
      ;; `sqlite3` hands the block a Python object wrapping a host connection, and
      ;; dropping that object left the connection — and its descriptor — open for
      ;; the whole session (measured: 14 per 15), invisible to the registry because
      ;; no `open` was ever involved. The shim registers a reaper that the runtime
      ;; calls on its own schedule, so the process count is what has to be watched.
      (let
        [ctx
         (sandbox)

         before
         (open-fd-count)

         r
         (ep/run-python-block ctx
                              (str "import gc, sqlite3\n" "for i in range(12):\n"
                                   "    c = sqlite3.connect(W + str(i) + '.db')\n"
                                   "    c.execute('create table t(x)')\n"
                                   "    del c\n" "gc.collect()\n"
                                   "__vis_reclaim_fds__(True)\n" "'done'")
                              "t1/i2")

         grown
         (- (open-fd-count) before)]

        (expect (nil? (:error r)))
        (expect (= "done" (:result r)))
        (expect (> 6 grown)))))
(defn- loopback-server
  "A loopback HTTP server answering every request with `status` and a 5-byte body.
   The sandbox's HTTP rides a REAL socket, so proving what a response leaves
   behind needs a real server — and it must be THIS process's, never the network."
  ^HttpServer [^long status]
  (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]
    (.createContext server
                    "/"
                    (reify
                      HttpHandler
                        (handle [_ exchange]
                          (let [body (.getBytes "probe" StandardCharsets/UTF_8)]
                            (.sendResponseHeaders ^HttpExchange exchange status (alength body))
                            (with-open [out (.getResponseBody ^HttpExchange exchange)]
                              (.write out body))))))
    (.start server)
    server))

(defn- net-sandbox
  "A sandbox with the network capability ON — without it `socket.socket()` raises
   `UnsupportedOperation: socket was excluded` and nothing here can run. `URL`
   points at `server` when one is given."
  ([] (net-sandbox nil))
  ([^HttpServer server]
   (let
     [ctx (:python-context (ep/create-python-context {}
                                                     (fn []
                                                       [(temp-root)])
                                                     {:enabled? true}))]
     (when server
       (ep/run-python-block
         ctx
         (str "URL = "
              (pr-str (str "http://127.0.0.1:" (.getPort (.getAddress server)) "/probe"))
              "\n")
         "t1/i1"))
     ctx)))

(defn- live-socket-entries
  "The `__vis_reclaim_fds__(True)` call plus the number of SOCKET entries still
   tracked after it — the client side only, which is what the sandbox owns."
  ^String []
  (str "closed = __vis_reclaim_fds__(True)\n"
       "kept = len([e for e in __vis_fd_registry__.values()\n"
       "            if isinstance(e[1], tuple) and e[1][0] == 'vis-socket'])\n"))

(defdescribe
  sandbox-socket-reclamation-test
  (it "reclaims the connections a block dropped and leaves the live ones alone"
      ;; The third door onto a descriptor: a socket is minted by
      ;; `socket.socket(...)`, never by `open`, so before this it was tracked by
      ;; nothing at all. Six dropped clients must go; the six accepted peers and
      ;; the listener the block still holds must not.
      (let
        [r (ep/run-python-block
             (net-sandbox)
             (str "import gc, socket\n"
                  "srv = socket.socket()\n" "srv.bind(('127.0.0.1', 0))\n"
                  "srv.listen(8)\n" "port = srv.getsockname()[1]\n"
                  "live = []\n" "for _ in range(6):\n"
                  "    c = socket.create_connection(('127.0.0.1', port), timeout=5)\n"
                  "    peer, _addr = srv.accept()\n"
                  "    live.append(peer)\n" "    del c\n"
                  "gc.collect()\n" "[__vis_reclaim_fds__(True), len(live)]")
             "t1/i2")]
        (expect (nil? (:error r)))
        (expect (= [6 6] (:result r)))))
  (it "never closes a number another socket has taken over"
      ;; `fstat` on a socket reports `st_dev == st_ino == 0` — every socket looks
      ;; like every other one — so the file identity would have closed the JVM's
      ;; own connections. A socket is identified by its ADDRESS PAIR: a stale
      ;; entry whose owner is gone must leave the live connection on that number
      ;; alone.
      (let
        [r (ep/run-python-block
             (net-sandbox)
             (str "import gc, socket, weakref\n" "srv = socket.socket()\n"
                  "srv.bind(('127.0.0.1', 0))\n" "srv.listen(4)\n"
                  "c = socket.create_connection(('127.0.0.1', srv.getsockname()[1]), timeout=5)\n"
                  "peer, _addr = srv.accept()\n"
                  "fd = c.fileno()\n" "class Gone:\n"
                  "    pass\n" "stale = Gone()\n"
                  "__vis_fd_registry__[fd] = (weakref.ref(stale), ('vis-socket', None, None))\n"
                  "del stale\n"
                  "gc.collect()\n" "closed = __vis_reclaim_fds__(True)\n"
                  "c.sendall(b'ping')\n" "[peer.recv(4).decode(), closed]")
             "t1/i2")]
        (expect (nil? (:error r)))
        (expect (= ["ping" 0] (:result r)))))
  (it "counts a connection against the ceiling, which never saw one before"
      ;; The EMFILE ceiling exists so a leak fails HERE, with the message naming
      ;; the fix, instead of wedging the session later on a `shell` that can no
      ;; longer fork. It was blind to sockets.
      (let
        [r (ep/run-python-block
             (net-sandbox)
             (str "import socket\n"
                  "__vis_fd_max__ = 8\n" "__vis_fd_sweep_at__ = 4\n"
                  "srv = socket.socket()\n" "srv.bind(('127.0.0.1', 0))\n"
                  "srv.listen(32)\n" "port = srv.getsockname()[1]\n"
                  "held = []\n" "err = ''\n"
                  "try:\n" "    for _ in range(24):\n"
                  "        held.append(socket.create_connection(('127.0.0.1', port), timeout=5))\n"
                  "except OSError as e:\n"
                  "    err = str(e)\n" "[len(held) < 24, 'too many open files' in err]")
             "t1/i2")]
        (expect (nil? (:error r)))
        (expect (= [true true] (:result r)))))
  (it "leaves no connection behind after HTTP through the `requests` shim"
      ;; Every HTTP call in the sandbox rides urllib -> http.client -> a socket,
      ;; and a 4xx used to hand the block a live connection nobody closed: urllib
      ;; raises `HTTPError`, the shim read its body and dropped it.
      (let
        [server
         (loopback-server 404)

         r
         (try (ep/run-python-block (net-sandbox server)
                                   (str
                                     "import gc, requests\n"
                                     "codes = [requests.get(URL).status_code for _ in range(12)]\n"
                                     "gc.collect()\n"
                                     (live-socket-entries)
                                     "[codes[0], len(codes), kept]")
                                   "t1/i2")
              (finally (.stop server 0)))]

        (expect (nil? (:error r)))
        (expect (= [404 12 0] (:result r)))))
  (it "reclaims a response whose body the block never read"
      ;; The leak in its rawest form (measured on the shipped build: 20 dropped
      ;; unread responses = +63 process descriptors, untouched by two
      ;; `gc.collect()`s AND by the block boundary).
      (let
        [server
         (loopback-server 200)

         r
         (try (ep/run-python-block (net-sandbox server)
                                   (str "import gc, urllib.request as ur\n"
                                        "for _ in range(12):\n"
                                        "    ur.urlopen(URL, timeout=5)\n"
                                        "gc.collect()\n"
                                        (live-socket-entries)
                                        "[closed >= 6, kept]")
                                   "t1/i2")
              (finally (.stop server 0)))]

        (expect (nil? (:error r)))
        (expect (= [true 0] (:result r)))))
  (it "keeps the socket doors sane across a runtime reinstall"
      ;; These doors are METHODS on a class the reinstall does not own: capturing
      ;; `socket.socket.__init__` a second time captures the WRAPPER, and the next
      ;; socket then recurses until the stack ends. The real methods are survivors
      ;; for exactly that reason.
      (let
        [ctx
         (net-sandbox)

         _
         (ep/run-python-block ctx "globals().clear()" "t1/i2")

         r
         (ep/run-python-block
           ctx
           (str "import gc, socket\n"
                "srv = socket.socket()\n" "srv.bind(('127.0.0.1', 0))\n"
                "srv.listen(4)\n"
                "c = socket.create_connection(('127.0.0.1', srv.getsockname()[1]), timeout=5)\n"
                "tracked = c.fileno() in __vis_fd_registry__\n" "del c\n"
                "gc.collect()\n" "[tracked, __vis_reclaim_fds__(True)]")
           "t1/i3")]

        (expect (nil? (:error r)))
        (expect (= [true 1] (:result r))))))
