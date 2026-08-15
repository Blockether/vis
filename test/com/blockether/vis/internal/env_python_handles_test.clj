(ns com.blockether.vis.internal.env-python-handles-test
  "The sandbox's ONE registry for host handles (`resources/vis-python/async_runtime.py`).

   A shim hands the block a small Python wrapper around a HOST id — a PIL raster
   (an int[], 4 bytes per pixel), an SSH session, an SQLite connection — while the
   resource itself lives in a per-JVM registry keyed by that id. GraalPy does not
   refcount, so dropping the wrapper frees NOTHING: no `__del__` runs, and the
   host cannot see that the last Python reference died. Every shim used to invent
   its own weak-ref table, its own sweep policy and its own reaper — so a shim
   that forgot leaked for the life of the process, which is exactly how a loop
   over phone screenshots walked a live session into `Java heap space`.

   This is that machinery ONCE: a shim declares how its kind is freed
   (`__vis_handle_kind__`) and names each owner (`__vis_own__`), and the runtime
   frees a handle when no owner can be reached any more — at the block boundary,
   under allocation pressure, and eagerly on `close()`."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- sandbox
  "A fresh sandbox: the handle registry is per-Context state, so one suite's
   handles can never be another's."
  []
  (:python-context (ep/create-python-context {})))

(def ^:private probe-kind
  "A synthetic kind that records what the runtime freed, so a test asserts on the
   registry's OWN decisions instead of on some shim's side effects."
  (str "PROBE = []\n" "__vis_handle_kind__('probe', lambda h: PROBE.append(h))\n"
       "class Owner:\n" "    pass\n"))

(defn- run [ctx code id] (ep/run-python-block ctx (str probe-kind code) id))

(defdescribe
  sandbox-handle-registry-test
  (it "frees a handle once nothing can reach its owner any more"
      ;; The whole point: five wrappers dropped without `close()`, and five host
      ;; handles released — the reclamation CPython's refcount would have done.
      (let
        [r (run (sandbox)
                (str "for i in range(5):\n" "    o = Owner()\n"
                     "    __vis_own__(o, 'probe', i, 1024)\n" "    del o\n"
                     "__vis_reclaim_handles__(True)\n" "sorted(PROBE)")
                "t1/i1")]
        (expect (nil? (:error r)))
        (expect (= [0 1 2 3 4] (vec (:result r))))))
  (it "never frees a handle whose owner is still reachable"
      ;; The other half of the contract: a sweep must be invisible to live code.
      (let
        [r (run (sandbox)
                (str "kept = Owner()\n" "__vis_own__(kept, 'probe', 7, 1024)\n"
                     "__vis_reclaim_handles__(True)\n"
                     "[len(PROBE), __vis_handle_census__()['probe']['count'], kept is not None]")
                "t1/i1")]
        (expect (nil? (:error r)))
        (expect (= [0 1 true] (vec (:result r))))))
  (it "frees a shared handle with the LAST of its owners, never the first"
      ;; `exif_transpose(in_place=True)` genuinely hands two PIL Images one
      ;; raster; freeing on the first drop would be a use-after-free.
      (let
        [r (run (sandbox)
                (str "a = Owner()\n" "b = Owner()\n"
                     "__vis_own__(a, 'probe', 1, 0)\n" "__vis_own__(b, 'probe', 1, 0)\n"
                     "del a\n" "__vis_reclaim_handles__(True)\n"
                     "still_held = list(PROBE)\n" "del b\n"
                     "__vis_reclaim_handles__(True)\n" "[still_held, PROBE]")
                "t1/i1")]
        (expect (nil? (:error r)))
        (expect (= [[] [1]] (mapv vec (:result r))))))
  (it "frees eagerly on the close path, without waiting for a collection"
      ;; `__vis_disown__` is what a shim's `close()` calls: it must free the
      ;; handle the moment its last owner lets go, and stay idempotent — closing
      ;; twice must not free a handle a second time.
      (let
        [r (run (sandbox)
                (str "a = Owner()\n" "b = Owner()\n"
                     "__vis_own__(a, 'probe', 4, 0)\n" "__vis_own__(b, 'probe', 4, 0)\n"
                     "first = __vis_disown__(a, 'probe', 4)\n"
                     "last = __vis_disown__(b, 'probe', 4)\n"
                     "again = __vis_disown__(b, 'probe', 4)\n" "[first, last, again, PROBE]")
                "t1/i1")]
        (expect (nil? (:error r)))
        (expect (= [false true false] (vec (take 3 (:result r)))))
        (expect (= [4] (vec (nth (vec (:result r)) 3))))))
  (it "forgets a handle the shim closes itself, so no sweep can close it twice"
      ;; An explicit `close()` reports its own failure; the registry must only
      ;; stop tracking what the shim already released.
      (let
        [r (run (sandbox)
                (str "o = Owner()\n" "__vis_own__(o, 'probe', 9, 4096)\n"
                     "__vis_forget__('probe', 9)\n" "del o\n"
                     "__vis_reclaim_handles__(True)\n" "[PROBE, __vis_handle_census__()['probe']]")
                "t1/i1")]
        (expect (nil? (:error r)))
        (let [[freed census] (vec (:result r))]
          (expect (= [] (vec freed)))
          (expect (= 0 (get census "count")))
          (expect (= 0 (get census "bytes")))))))

(defdescribe
  sandbox-handle-pressure-test
  (it "sweeps under allocation pressure, so one block cannot pin a heap of them"
      ;; Report 5075808e died INSIDE one block — ~1000 iterations of open ->
      ;; convert -> resize, ~12 MB of raster each — so a policy that only runs at
      ;; the block boundary would still have let it exhaust the heap. Nothing here
      ;; asks for a sweep: 200 dropped handles of 2 MiB must not stay pinned.
      (let
        [r (run (sandbox)
                (str "for i in range(200):\n"
                     "    o = Owner()\n" "    __vis_own__(o, 'probe', i, 2 * 1024 * 1024)\n"
                     "    del o\n" "__vis_handle_census__()['probe']['bytes']")
                "t1/i1")]
        (expect (nil? (:error r)))
        (expect (> (* 400 1024 1024) (:result r)))
        (expect (>= (* 192 1024 1024) (:result r)))))
  (it "collects on COUNT for a kind that costs a socket rather than memory"
      ;; 200 dropped `sqlite3.connect()`s inside ONE block are 200 host
      ;; connections and their descriptors, and not one byte of accounted memory
      ;; — so a byte budget alone would never fire and the block would exhaust
      ;; the process the way a leaking `open()` loop does.
      (let
        [r (run (sandbox)
                (str "for i in range(300):\n"
                     "    o = Owner()\n" "    __vis_own__(o, 'probe', i, 0)\n"
                     "    del o\n" "__vis_handle_census__()['probe']['count']")
                "t1/i1")]
        (expect (nil? (:error r)))
        (expect (> 300 (:result r)))
        (expect (>= 192 (:result r)))))
  (it
    "reclaims at the block boundary, so a finished block does not wait for the next"
    ;; `__vis_run_reapers__` runs at every block end and every tool call. What
    ;; the first block dropped must be gone by the time the second one looks.
    (let
      [ctx
       (sandbox)

       _
       (run ctx
            (str "for i in range(8):\n"
                 "    o = Owner()\n" "    __vis_own__(o, 'probe', i, 16 * 1024 * 1024)\n"
                 "    del o\n" "None")
            "t1/i1")

       r
       (ep/run-python-block ctx "[len(PROBE), __vis_handle_census__()['probe']['count']]" "t1/i2")]

      (expect (nil? (:error r)))
      (expect (= [8 0] (vec (:result r)))))))

(defdescribe sandbox-handle-registry-is-shared-test
             ;; Regression, report 5075808e (the iOS screenshot session): PIL leaked every
             ;; raster a block dropped because reclamation was the SHIM's business, and each
             ;; shim reinvented it — so `sqlite3` had a weaker copy and `paramiko` had none
             ;; at all. There is one registry now, and this proves both shims are in it.
             (it "holds the handles of every shim, not one shim's"
                 (let
                   [r (ep/run-python-block
                        (sandbox)
                        (str "import sqlite3\n"
                             "from PIL import Image\n" "im = Image.new('RGB', (64, 48))\n"
                             "db = sqlite3.connect(':memory:')\n"
                             "before = __vis_handle_census__()\n"
                             "del im, db\n" "__vis_reclaim_handles__(True)\n"
                             "after = __vis_handle_census__()\n"
                             "[before['PIL.Image']['count'], before['PIL.Image']['bytes'],\n"
                             " before['sqlite3.Connection']['count'],\n"
                             " after['PIL.Image']['count'], after['sqlite3.Connection']['count']]")
                        "t1/i1")]
                   (expect (nil? (:error r)))
                   (expect (= [1 (* 64 48 4) 1 0 0] (vec (:result r)))))))
