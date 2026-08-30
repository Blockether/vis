(ns com.blockether.vis.internal.sandbox-fs-test
  "Security regression guard for the confined Python sandbox filesystem.
   These assertions are the safety net: a confinement bug = sandbox escape.
   Plus the OUTBOX tap: a WRITE under the engine outbox dir fires `on-close`;
   reads and writes anywhere else are untouched. That tap is DORMANT in the
   product — the engine passes no outbox (`mpl-capture/incidental-capture-enabled?`)
   — so these drive `confined-filesystem` directly to keep the kept-but-unwired
   machinery honest."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.env-python :as env-python]
            [com.blockether.vis.internal.foundation.mpl-capture :as mc]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.sandbox-fs :as sfs]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]
           [org.graalvm.polyglot.io IOAccess]
           [java.nio ByteBuffer]
           [java.nio.file Files Paths StandardOpenOption]
           [java.nio.file.attribute FileAttribute]))

(defn- tmp-root
  ^String []
  (let [d (Files/createTempDirectory "vis-fs-test" (make-array FileAttribute 0))]
    (spit (str d "/inside.txt") "ROOT-DATA")
    (str (.toRealPath d (make-array java.nio.file.LinkOption 0)))))

(defn- denied? [thunk] (try (thunk) false (catch java.io.IOException _ true)))

(defn- p ^java.nio.file.Path [s] (Paths/get s (make-array String 0)))

(defn- gated-root
  "A root the `:fs/access` gate actually covers. `tmp-root` sits under the system
   temp dir, which `confine!` deliberately does NOT gate: temp, the attachment
   outbox and `~/.vis` are engine surfaces lent to the guest, and a hook able to
   refuse them is a hook able to brick a live session."
  ^String []
  (let [target
        (java.io.File. (str (System/getProperty "user.dir")) "target")

        _
        (.mkdirs target)

        d
        (Files/createTempDirectory (p (str target))
                                   "vis-fs-gate-test"
                                   (make-array FileAttribute 0))]

    (spit (str d "/inside.txt") "ROOT-DATA")
    (str (.toRealPath d (make-array java.nio.file.LinkOption 0)))))

(defdescribe confine-test
             (it "allows paths under a root (existing AND not-yet-created); denies outside"
                 (let [root
                       (tmp-root)

                       roots-fn
                       (fn []
                         [root])

                       confine
                       #(@#'sfs/confine! roots-fn (atom {}) [] nil "file-read" (p %))]

                   ;; allowed
                   (expect (= (str root "/inside.txt") (str (confine (str root "/inside.txt")))))
                   (expect (= (str root "/sub/new.txt") (str (confine (str root "/sub/new.txt")))))
                   (expect (denied? #(confine "/etc/passwd")))
                   (expect (denied? #(confine (str root "/../etc/passwd")))) ; .. escape
                   (expect (denied? #(confine "/tmp")))))
             (it
               "denies a symlink (inside a root) that points OUTSIDE"
               (let [root
                     (tmp-root)

                     link
                     (str root "/evil")]

                 (Files/createSymbolicLink (p link) (p "/etc/passwd") (make-array FileAttribute 0))
                 (expect (denied? #(@#'sfs/confine!
                                     (fn []
                                       [root])
                                     (atom {})
                                     []
                                     nil
                                     "file-read"
                                     (p link))))))
             (it "fails CLOSED with zero roots (denies everything)"
                 (let [root (tmp-root)]
                   (expect (denied? #(@#'sfs/confine!
                                       (fn []
                                         [])
                                       (atom {})
                                       []
                                       nil
                                       "file-read"
                                       (p (str root "/inside.txt")))))
                   (expect (denied? #(@#'sfs/confine!
                                       (fn []
                                         nil)
                                       (atom {})
                                       []
                                       nil
                                       "file-read"
                                       (p (str root "/inside.txt")))))))
             (it "allows a path under the OUTBOX dir even though it is outside configured roots"
                 (let [root
                       (tmp-root)

                       outbox
                       (str (.toRealPath (Files/createTempDirectory "vis-outbox-t"
                                                                    (make-array FileAttribute 0))
                                         (make-array java.nio.file.LinkOption 0)))

                       confine
                       #(@#'sfs/confine!
                          (fn []
                            [root])
                          (atom {})
                          [(p outbox)]
                          nil
                          "file-read"
                          (p %))]

                   (expect (= (str outbox "/a.csv") (str (confine (str outbox "/a.csv")))))
                   ;; still denies outside both root and outbox
                   (expect (denied? #(confine "/etc/passwd"))))))

(defdescribe
  confined-graalpy-fs-test
  (it
    "GraalPy open()/listdir is confined to the root; stdlib still loads"
    (let [root
          (tmp-root)

          fs
          (sfs/confined-filesystem (fn []
                                     [root]))

          io
          (-> (IOAccess/newBuilder)
              (.fileSystem fs)
              (.build))

          ctx
          (-> (Context/newBuilder (into-array String ["python"]))
              (.allowIO io)
              (.allowAllAccess false)
              (.build))

          ev
          (fn [code]
            (.eval ctx "python" code))]

      (try
        ;; read inside the root works
        (expect (= "ROOT-DATA"
                   (.asString ^org.graalvm.polyglot.Value
                              (ev (str "open(" (pr-str (str root "/inside.txt")) ").read()")))))
        ;; write inside the root works
        (ev (str "open(" (pr-str (str root "/w.txt")) ",\"w\").write(\"x\")"))
        ;; read OUTSIDE the root is denied (PermissionError on the guest side)
        (expect (try (ev "open(\"/etc/passwd\").read()") false (catch Throwable _ true)))
        ;; listing outside is denied (no leak)
        (expect (try (ev "__import__(\"os\").listdir(\"/etc\")") false (catch Throwable _ true)))
        ;; GraalPy's own stdlib (outside the roots) still imports
        (expect (str/includes? (.asString ^org.graalvm.polyglot.Value
                                          (ev "__import__(\"json\").dumps({\"ok\":1})"))
                               "ok"))
        (finally (.close ctx true)))))
  (it
    "commits valid pathlib writes and atomically blocks broken ones"
    (let [root
          (tmp-root)

          target
          (str root "/guarded.clj")

          original
          "(defn guarded [] :ok)\n"

          changed
          "(defn guarded [] :changed)\n"

          fs
          (sfs/confined-filesystem (fn []
                                     [root]))

          io
          (-> (IOAccess/newBuilder)
              (.fileSystem fs)
              (.build))

          ctx
          (-> (Context/newBuilder (into-array String ["python"]))
              (.allowIO io)
              (.allowAllAccess false)
              (.build))]

      (spit target original)
      (try (.eval ctx
                  "python"
                  (str "from pathlib import Path; Path("
                       (pr-str target)
                       ").write_text("
                       (pr-str changed)
                       ")"))
           (expect (= changed (slurp target)))
           ;; GraalPy currently swallows a SeekableByteChannel close IOException. The
           ;; filesystem still rejects the staged candidate and preserves the target.
           (.eval ctx
                  "python"
                  (str "from pathlib import Path; Path(" (pr-str target) ").write_text(chr(41))"))
           (expect (= changed (slurp target)))
           (finally (.close ctx true))))))
(defdescribe
  syntax-guard-reported-effect-test
  (it
    "reports a swallowed close failure as the python_execution exception"
    (let [root
          (tmp-root)

          target
          (str root "/reported.clj")

          original
          "(defn reported [] :ok)\n"

          {:keys [python-context python-engine]}
          (env-python/create-python-context {} (constantly [root]))]

      (spit target original)
      (try
        (let [result
              (env-python/run-python-block python-context
                                           (str "from pathlib import Path; Path("
                                                (pr-str target)
                                                ").write_text(chr(41) + chr(10) + chr(41))"))

              message
              (get-in result [:error :message])

              diagnostics
              (get-in result [:error :data :diagnostics])]

          (expect (= :python/syntax-guard (get-in result [:error :data :phase])))
          (expect (str/includes? message "write was refused"))
          (expect (str/includes? message "line 1, column 0"))
          (expect (str/includes? message "line 2, column 0"))
          (expect (str/includes? message "Fix the syntax and retry the write"))
          (expect (str/includes? message "or use patch(...)"))
          (expect (= [{:line 1
                       :column 0
                       :end-line 1
                       :end-column 1
                       :node-type "ERROR"
                       :missing? false
                       :text ")"}
                      {:line 2
                       :column 0
                       :end-line 2
                       :end-column 1
                       :node-type "ERROR"
                       :missing? false
                       :text ")"}]
                     diagnostics))
          (expect (= "clojure" (get-in result [:error :data :language])))
          (expect (= target (get-in result [:error :data :path])))
          (expect (= original (slurp target)))
          ;; The rejection belongs to this block only; a later block is clean.
          (let [next-result (env-python/run-python-block python-context "print(40 + 2)")]
            (expect (= "42\n" (:stdout next-result)))
            (expect (nil? (:error next-result)))))
        (finally (.close ^Context python-context true)
                 (.close ^org.graalvm.polyglot.Engine python-engine))))))

(defn- write-channel!
  "Open `path` for write through `fs`, write `s`, close — driving the outbox tap."
  [^org.graalvm.polyglot.io.FileSystem fs ^java.nio.file.Path path ^String s]
  (let [ch (.newByteChannel fs
                            path
                            #{StandardOpenOption/WRITE StandardOpenOption/CREATE
                              StandardOpenOption/TRUNCATE_EXISTING}
                            (make-array FileAttribute 0))]
    (.write ch (ByteBuffer/wrap (.getBytes s)))
    (.close ch)))

(defdescribe
  syntax-guarded-write-channel-test
  (it "surfaces the parse refusal and leaves an existing file byte-for-byte unchanged"
      (let [root
            (tmp-root)

            target
            (p (str root "/direct.clj"))

            original
            "(def direct :ok)\n"

            _
            (spit (str target) original)

            fs
            (sfs/confined-filesystem (fn []
                                       [root]))

            ch
            (.newByteChannel fs
                             target
                             #{StandardOpenOption/WRITE StandardOpenOption/TRUNCATE_EXISTING}
                             (make-array FileAttribute 0))]

        (.write ch (ByteBuffer/wrap (.getBytes ")")))
        (let [message (try (.close ch) nil (catch java.io.IOException e (ex-message e)))]
          (expect (str/includes? message "[vis:syntax_guard]"))
          (expect (= original (slurp (str target)))))))
  (it "does not create a new guarded file when its candidate is broken"
      (let [root
            (tmp-root)

            target
            (p (str root "/new.clj"))

            fs
            (sfs/confined-filesystem (fn []
                                       [root]))

            ch
            (.newByteChannel fs
                             target
                             #{StandardOpenOption/WRITE StandardOpenOption/CREATE_NEW}
                             (make-array FileAttribute 0))]

        (.write ch (ByteBuffer/wrap (.getBytes ")")))
        (expect (try (.close ch) false (catch java.io.IOException _ true)))
        (expect (not (Files/exists target (make-array java.nio.file.LinkOption 0)))))))

(defdescribe
  outbox-tap-test
  (it
    "captures a WRITE under the outbox, but NOT a root write or an outbox read"
    (let [outdir
          (Files/createTempDirectory "vis-outbox-tap" (make-array FileAttribute 0))

          rootdir
          ;; A NON-temp root (under the repo): a write here is NOT captured.
          ;; NB not a createTempDirectory dir — that lives under $TMPDIR, which
          ;; the widened tap now (correctly) captures, so it wouldn't isolate
          ;; the "non-outbox root" case.
          (let [d (java.io.File. "target/vis-outbox-root-test")]
            (.mkdirs d)
            (.toPath (.getCanonicalFile d)))

          sink
          (atom [])

          seen
          (atom #{})

          fs
          (sfs/confined-filesystem (fn []
                                     [(str rootdir)])
                                   {:dir (str outdir)
                                    :on-close (fn [p]
                                                (mc/record-file! p))})]

      (binding [mc/*attachment-sink*
                sink

                mc/*outbox-seen*
                seen]

        (write-channel! fs (.resolve outdir "report.csv") "x,y\n1,2\n")
        ;; write under a normal (non-temp) root — untouched
        (write-channel! fs (.resolve rootdir "work.txt") "not captured")
        ;; read under the outbox — untouched
        (.close (.newByteChannel fs
                                 (.resolve outdir "report.csv")
                                 #{StandardOpenOption/READ}
                                 (make-array FileAttribute 0))))
      (let [[att] @sink]
        (expect (= 1 (count @sink)))
        (expect (= "report.csv" (:filename att)))
        (expect (= "text/csv" (:media-type att)))
        (expect (= "file" (:kind att)))
        (expect (= 8 (:size att))))))
  (it "de-dups the same outbox path re-closed within one block"
      (let [outdir
            (Files/createTempDirectory "vis-outbox-dedup" (make-array FileAttribute 0))

            sink
            (atom [])

            seen
            (atom #{})

            fs
            (sfs/confined-filesystem (fn []
                                       [(str outdir)])
                                     {:dir (str outdir)
                                      :on-close (fn [p]
                                                  (mc/record-file! p))})]

        (binding [mc/*attachment-sink*
                  sink

                  mc/*outbox-seen*
                  seen]

          (write-channel! fs (.resolve outdir "a.csv") "1")
          (write-channel! fs (.resolve outdir "a.csv") "22"))
        (expect (= 1 (count @sink))))))

(defdescribe
  temp-root-tap-test
  (it "captures a WRITE under a system temp root (/tmp, $TMPDIR), not just $VIS_OUTBOX"
      (let [outdir
            (Files/createTempDirectory "vis-tmptap-outbox" (make-array FileAttribute 0))

            sink
            (atom [])

            seen
            (atom #{})

            ;; A bogus configured root plus an unrelated outbox dir proves the tap
            ;; can ONLY fire via the system-temp-root widening.
            fs
            (sfs/confined-filesystem (fn []
                                       ["/no/such/workspace/root"])
                                     {:dir (str outdir)
                                      :on-close (fn [p]
                                                  (mc/record-file! p))})

            probe
            (str (System/getProperty "java.io.tmpdir") "/vis-tmptap-" (System/nanoTime) ".csv")]

        (binding [mc/*attachment-sink*
                  sink

                  mc/*outbox-seen*
                  seen]

          (write-channel! fs (p probe) "a,b\n1,2\n"))
        (let [[att] @sink]
          (expect (= 1 (count @sink)))
          (expect (= "text/csv" (:media-type att)))
          (expect (= "file" (:kind att)))))))

;; Regression: a file the sandbox merely READ through a write-CAPABLE channel
;; (`open(p, "r+")`, `sqlite3.connect`, any library that opens read-write to read)
;; was captured again as a session attachment, because the tap fired on close for
;; any channel opened with WRITE in its options.
(defdescribe
  temp-root-read-tap-test
  (it
    "does NOT capture a write-capable channel that only READ"
    (let [outdir
          (Files/createTempDirectory "vis-tmptap-readonly" (make-array FileAttribute 0))

          sink
          (atom [])

          seen
          (atom #{})

          fs
          (sfs/confined-filesystem (fn []
                                     ["/no/such/workspace/root"])
                                   {:dir (str outdir)
                                    :on-close (fn [p]
                                                (mc/record-file! p))})

          probe
          (str (System/getProperty "java.io.tmpdir") "/vis-tmptap-rw-" (System/nanoTime) ".csv")]

      (binding [mc/*attachment-sink*
                sink

                mc/*outbox-seen*
                seen]

        (write-channel! fs (p probe) "a,b\n1,2\n")
        (expect (= 1 (count @sink)))
        ;; READ+WRITE open, nothing written: the tap stays disarmed.
        (let [ch (.newByteChannel fs
                                  (p probe)
                                  #{StandardOpenOption/READ StandardOpenOption/WRITE}
                                  (make-array FileAttribute 0))]
          (.read ch (java.nio.ByteBuffer/allocate 8))
          (.close ch))
        (expect (= 1 (count @sink)))))))

(defdescribe
  confined-fs-temp-roots-test
  (it "ALWAYS allows the system temp dirs (/tmp, $TMPDIR) even when outside configured roots"
      (let [fs
            (sfs/confined-filesystem (fn []
                                       ["/no/such/workspace/root"]))

            probe
            (str (System/getProperty "java.io.tmpdir") "/vis-temproot-" (System/nanoTime) ".txt")]

        (try
          ;; write + read scratch under $TMPDIR works despite the bogus root
          (write-channel! fs (p probe) "temp-ok")
          (expect (= "temp-ok" (slurp probe)))
          ;; a literal /tmp path resolves through confinement (allowed)
          (expect (some? (.toRealPath fs (p "/tmp") (make-array java.nio.file.LinkOption 0))))
          ;; ...but a path outside every root AND every temp dir is still DENIED
          (expect (denied? #(write-channel! fs (p "/etc/vis-nope.txt") "x")))
          (finally (Files/deleteIfExists (p probe)))))))

(defdescribe
  confined-fs-vis-always-roots-test
  (it
    "ALWAYS allows all of ~/.vis (including config) even when outside configured roots"
    (let [home
          (System/getProperty "user.home")

          vis-dir
          (java.io.File. home ".vis")

          ext-dir
          (java.io.File. vis-dir "extensions")

          logs-dir
          (java.io.File. vis-dir "logs")

          _
          (do (.mkdirs ext-dir) (.mkdirs logs-dir))

          ;; A bogus configured root proves the always-on ~/.vis widening is sufficient.
          fs
          (sfs/confined-filesystem (fn []
                                     ["/no/such/workspace/root"]))

          ext-probe
          (str ext-dir "/vis-extroot-" (System/nanoTime) ".py")

          log-probe
          (str logs-dir "/vis-logroot-" (System/nanoTime) ".log")

          config-probe
          (str vis-dir "/vis-configroot-" (System/nanoTime) ".edn")]

      (try
        ;; Existing extension/log paths still work despite the bogus root.
        (write-channel! fs (p ext-probe) "print('ext-ok')")
        (expect (= "print('ext-ok')" (slurp ext-probe)))
        (write-channel! fs (p log-probe) "log-ok")
        (expect (= "log-ok" (slurp log-probe)))
        ;; So does a config-shaped file directly under ~/.vis.
        (write-channel! fs (p config-probe) "{:config true}")
        (expect (= "{:config true}" (slurp config-probe)))
        (expect (some? (.toRealPath fs (p (str vis-dir)) (make-array java.nio.file.LinkOption 0))))
        (finally (Files/deleteIfExists (p ext-probe))
                 (Files/deleteIfExists (p log-probe))
                 (Files/deleteIfExists (p config-probe)))))))

(defn- prefix-rule-hook
  "An extension's guard, in miniature. The rule VOCABULARY belongs to the guard —
   after Phase 2 the engine owns no globs and no access levels — so the test
   writes plain prefixes and answers with the sentence a refusal carries."
  [root rules]
  (fn [_env _op {:keys [operation path]}]
    (let [rel
          (str/replace-first (str path) (str root "/") "")

          intent
          (if (str/ends-with? (str operation) "-write") :write :read)]

      (some (fn [{:keys [prefix access hint]}]
              (when (and (str/starts-with? rel prefix)
                         (or (= :none access) (and (= :write intent) (= :read-only access))))
                hint))
            rules))))

(defn- with-fs-gate!
  "Register `hook-fn` as the one `:fs/access` gate, hand `body` the gate fn the
   engine builds from it (exactly as `loop.clj` does), then tear it down."
  [hook-fn body]
  (try (extension/register-op-hook! {:op :fs/access :owner :ext/test-sandbox-gate :fn hook-fn})
       (body (extension/fs-access-gate (constantly {:extensions (atom [])})))
       (finally (extension/unregister-op-hooks-for-owner! :ext/test-sandbox-gate))))

;; Regression, PLAN Phase 2: an extension's path boundary only reached the native
;; file verbs, so a path it declared protected still accepted a plain Python
;; `open(..., "w")` / `shutil.move` through the sandbox filesystem.
(defdescribe
  fs-access-gate-in-sandbox-test
  (it "refuses a WRITE an extension gate blocks, inside an approved root"
      (let [root (gated-root)]
        (with-fs-gate!
          (prefix-rule-hook root [{:prefix "secrets/" :access :none :hint "Use the vault API."}])
          (fn [gate-fn]
            (let [fs (sfs/confined-filesystem (fn []
                                                [root])
                                              nil
                                              gate-fn)]
              (Files/createDirectory (p (str root "/secrets")) (make-array FileAttribute 0))
              (expect (denied? #(.newByteChannel fs
                                                 (p (str root "/secrets/key.txt"))
                                                 #{StandardOpenOption/WRITE
                                                   StandardOpenOption/CREATE}
                                                 (make-array FileAttribute 0))))
              (expect (denied? #(.delete fs (p (str root "/secrets/key.txt")))))
              ;; :none also refuses the READ
              (expect (denied? #(.newByteChannel fs
                                                 (p (str root "/secrets/key.txt"))
                                                 #{StandardOpenOption/READ}
                                                 (make-array FileAttribute 0))))
              ;; an unprotected sibling is untouched
              (expect (some? (.newByteChannel fs
                                              (p (str root "/plain.txt"))
                                              #{StandardOpenOption/WRITE StandardOpenOption/CREATE}
                                              (make-array FileAttribute 0)))))))))
  (it "a read-only rule still reads, and the guest error names the owner's hint"
      (let [root (gated-root)]
        (with-fs-gate! (prefix-rule-hook root
                                         [{:prefix "inside.txt"
                                           :access :read-only
                                           :hint "Use (br/policy) instead."}])
                       (fn [gate-fn]
                         (let [fs (sfs/confined-filesystem (fn []
                                                             [root])
                                                           nil
                                                           gate-fn)
                               msg (try (.newByteChannel fs
                                                         (p (str root "/inside.txt"))
                                                         #{StandardOpenOption/WRITE}
                                                         (make-array FileAttribute 0))
                                        nil
                                        (catch java.io.IOException e (ex-message e)))]

                           (expect (some? (.newByteChannel fs
                                                           (p (str root "/inside.txt"))
                                                           #{StandardOpenOption/READ}
                                                           (make-array FileAttribute 0))))
                           (expect (str/includes? (str msg) "reason=path_protected"))
                           (expect (str/includes? (str msg) "Use (br/policy) instead.")))))))
  (it "fails CLOSED when the gate hook itself throws"
      (let [root (gated-root)]
        (with-fs-gate! (fn [_env _op _ctx]
                         (throw (ex-info "broken guard" {})))
                       (fn [gate-fn]
                         (let [fs (sfs/confined-filesystem (fn []
                                                             [root])
                                                           nil
                                                           gate-fn)]
                           (expect (denied? #(.newByteChannel fs
                                                              (p (str root "/inside.txt"))
                                                              #{StandardOpenOption/READ}
                                                              (make-array FileAttribute 0))))))))))
