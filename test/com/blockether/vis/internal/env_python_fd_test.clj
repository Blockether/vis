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
