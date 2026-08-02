(ns com.blockether.vis.internal.env-python-fd-test
  "Descriptor discipline for the sandbox `open` (`resources/vis-python/async_runtime.py`).

   GraalPy does not refcount, so a handle the block DROPS is never finalized:
   its process file descriptor stays open forever. A loop like
   `open(p).read()` over a big tree therefore walks the whole JVM into EMFILE,
   and the first casualty is not Python — `ProcessBuilder` can no longer fork,
   so every later `shell`/`git` call dies with the JDK's misleading \"spawn
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
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

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
      ;; `io.open` (a DIFFERENT object here), `pathlib.Path.open` / `tempfile`
      ;; (which both call `io.open`), and `builtins.open` reached through any
      ;; other module's globals. Each leaked one descriptor per iteration. 120
      ;; leaked opens against a ceiling of 8 can only survive if every door is
      ;; tracked — and the block returning at all proves no door leads back into
      ;; the shim (a self-call would be a RecursionError on the first open).
      (let
        [r (ep/run-python-block (sandbox)
                                (str "import builtins, io, pathlib\n"
                                     "for _ in range(40):\n"
                                     "    h = pathlib.Path(F).open()\n"
                                     "    h = io.open(F)\n"
                                     "    h = builtins.open(F)\n"
                                     "    del h\n"
                                     "len(__vis_fd_registry__)")
                                "t1/i2")]
        (expect (nil? (:error r)))
        (expect (>= 8 (:result r)))))
  (it "never closes a descriptor a lower layer of the same handle still owns"
      ;; `open()` hands back a STACK (TextIOWrapper -> BufferedReader -> FileIO)
      ;; and dropping the top layer does not end the file: `raw = open(p,
      ;; "rb").raw` still reads perfectly afterwards (measured). Weak-referencing
      ;; the TOP layer would close this descriptor under the block's feet, so the
      ;; registry tracks the layer that actually owns the fd.
      (let
        [r (ep/run-python-block (sandbox)
                                (str "raw = open(F, 'rb').raw\n"
                                     "for _ in range(40):\n"
                                     "    h = open(F)\n"
                                     "    del h\n"
                                     "raw.read().decode()")
                                "t1/i2")]
        (expect (nil? (:error r)))
        (expect (= "probe\n" (:result r)))))
  (it "leaves a borrowed descriptor to its owner"
      ;; `closefd=False` means the wrapper only BORROWED an fd the block opened
      ;; itself; reclaiming it when the wrapper dies would close a file the block
      ;; is still reading through the descriptor it owns.
      (let
        [r (ep/run-python-block (sandbox)
                                (str "import os\n"
                                     "fd = os.open(F, os.O_RDONLY)\n"
                                     "h = open(fd, 'rb', closefd=False)\n"
                                     "del h\n"
                                     "for _ in range(40):\n"
                                     "    x = open(F)\n"
                                     "    del x\n"
                                     "out = os.read(fd, 5).decode()\n"
                                     "os.close(fd)\n"
                                     "out")
                                "t1/i2")]
        (expect (nil? (:error r)))
        (expect (= "probe" (:result r)))))
  (it "survives a runtime reinstall without making `open` call itself"
      ;; `globals().clear()` is legal Python, and it makes `ensure-async-runtime!`
      ;; re-eval the whole preamble in the SAME globals. Re-capturing the real
      ;; opener there would capture the SHIM (RecursionError on the very next
      ;; `open`), and rebuilding the registry would forget every descriptor the
      ;; session is still holding — the same hazard the reinstall already unwraps
      ;; for `print`, one door further down.
      (let
        [ctx (sandbox)
         f (:result (ep/run-python-block ctx "F" "t1/i2"))
         _ (ep/run-python-block ctx "for _ in range(6):\n    h = open(F)\n    del h" "t1/i3")
         _ (ep/run-python-block ctx "globals().clear()" "t1/i4")
         r (ep/run-python-block ctx (str "open(" (pr-str f) ").read()") "t1/i5")
         reg (ep/run-python-block ctx "len(__vis_fd_registry__)" "t1/i6")]
        (expect (nil? (:error r)))
        (expect (= "probe\n" (:result r)))
        (expect (nil? (:error reg)))
        (expect (>= 8 (:result reg))))))
