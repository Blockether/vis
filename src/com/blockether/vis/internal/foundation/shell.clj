(ns com.blockether.vis.internal.foundation.shell
  "Foundation-core's shell implementation. Bound only when the user-owned `shell`
   toggle is ON (default ON; flip it OFF in Settings or in `vis.yml` via
   `toggles: {shell: false}` to drop the tools). The OS process jail is the
   containment layer while active.

   ONE model-facing entry point — the `shell` PYTHON verb, bound BARE in the flat
   sandbox next to `ls` / `grep`. A process is started from Python, and every verb
   after the spawn is a method on the handle the call returns. EVERY run is a
   background run: the call spawns
   under a real pty and returns the HANDLE now, so there is no `wait` on the
   request and no number that can select a second mode. ONE call runs ONE command:
   an ordered batch was a second budget, a second result shape and a second failure
   mode for what `&&` already says.

   1. `sh = await shell(\"ls\")` — `bash -lc` in the workspace root, spawned under
      a REAL pty, its merged output streamed verbatim to a log FILE and registered
      as a session resource. `sh.snapshot()` reads the current state. Output is
      bounded at READ time to a head+tail budget per stream, so only the MIDDLE of a
      huge stream is dropped, never its start or end. A non-zero exit is DATA the
      model reads, not an error.

   2. A server, watcher or long build is the SAME call — you simply do not wait for
      it, or you wait for less than it takes. A wait that expires is never a lost
      process: it keeps running under its id and its log keeps filling.

   3. The log OUTLIVES the run. Every shell keeps its log file and its index row by
      id for as long as the session does, so \"what did that build print\" is
      answerable a turn later from the id alone. That retention is the feature, not
      a leak to reap.

   The result IS the HANDLE: every shell answer is a dict-with-methods in the
   sandbox, so the process is driven on the object the call already returned —
   `sh.logs(-50)` reads the last 50 LINES (or a byte OFFSET, or `lines=10` for a
   ten-line window) and returns a page NOW; `page[-4000:]` slices that page's `out`
   text directly, `next(page)` continues the read and `page.pages()` walks its ready
   pages lazily, bounded to ten by default, while
   `lines=-10` walks back up. Use `await asyncio.wait([sh], timeout=30)` for a
   bounded wait. `sh.type(\"y\")` types into the terminal. `sh.cancel()` stops the process tree.
   There are no id-taking verbs to re-type an id into; re-issuing a LIVE id gives
   the same handle back.

   Task fields give the start result until settlement. The final result and log
   pages are fixed maps. `shell-dispatch` handles internal lifecycle operations.

   EVERY result of EVERY stage — including an argv run, which uses the same runner
   — is the one [[shell-result-base]] key set: `stage` names the producer and is
   the only thing that varies. A key a stage has nothing to say about is nil /
   false / 0 instead of absent, so model Python indexes any of them without a
   KeyError, and a run answers with its `command` and that command's own bytes at
   the TOP level — there is no entry to unwrap and no second shape to learn.
   The `shell` toggle is registered HERE and owned by Vis core. It closes the
   MODEL's door only: an installed extension keeps its own trusted process boundary
   (`vis.shell`, `subprocess`), which the toggle does not gate."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [clojure.string :as str]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.sandbox.egress-proxy :as egress]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.sandbox.gateway :as gateway-sandbox]
            [com.blockether.vis.internal.sandbox.jail :as process-jail]
            [com.blockether.vis.internal.gateway.resources :as resources]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.python.host :as python-host]
            [com.blockether.vis.internal.task :as task]
            [com.blockether.vis.internal.config.runtime-settings :as rt]
            [com.blockether.vis.internal.sandbox.policy :as security-policy]
            [com.blockether.vis.internal.foundation.shell-log :as shell-log]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [com.blockether.vis.internal.foundation.pty :as pty]
            [com.blockether.vis.internal.foundation.pty-bridge :as pty-bridge]
            [com.blockether.vis.internal.util :as util])
  (:import (java.io File)
           (java.nio.file Files LinkOption)
           (java.nio.file.attribute BasicFileAttributes)
           (java.util LinkedHashMap)
           (java.lang ProcessHandle)
           (java.util.concurrent TimeUnit)))

;; Limits

(def ^:private default-timeout-secs
  ;; ONE source of truth, shared with the Python eval watchdog: the watchdog must
  ;; sit a grace ABOVE this budget, or a shell call that names no timeout races a
  ;; watchdog that started first and can never return its own timeout envelope.
  rt/DEFAULT_SHELL_TIMEOUT_SECS)

(def ^:private max-timeout-secs
  ;; Thirty minutes, shared with the Python eval watchdog: the watchdog floors
  ;; itself above THIS, so no legal `run`/`wait` budget can be preempted from
  ;; outside.
  rt/MAX_SHELL_TIMEOUT_SECS)

(def ^:private max-sync-head-chars
  "Prefix of a SYNC stream always CAPTURED: the command's OPENING context —
   compile errors, the first failing assertion, the banner that says WHAT ran."
  100000)

(def ^:private max-sync-tail-chars
  "Suffix of a SYNC stream always CAPTURED: build / test failures and the final
   summary live at the END. Only the MIDDLE is dropped when a stream is huge —
   never the head, never the tail — so nothing important silently disappears.

   head+tail is the CAPTURE budget, NOT the display budget. The old 4k+12k pair
   mangled every ordinary machine-readable payload above 16k chars: a plain
   `gh issue list --json …` (21k chars) came back with the omitted-marker spliced
   at char 4000, so `json.loads(r[\"out\"])` died on
   \"Invalid control character\" EVERY time. The loop's wire projection clips
   separately, so capturing a parseable stream costs context nothing."
  300000)

;; Small helpers

(defn- capped-capture
  "A request-local bounded head/tail result, never background log storage.
   `:append!` accepts text read from disk; `:snapshot` returns
   `{:text :truncated :omitted}` with an exact omitted-character count."
  [^long head-limit ^long tail-limit]
  (let [sb
        (StringBuilder.)

        cap
        (+ head-limit tail-limit)

        total
        (atom 0)

        trunc
        (atom false)

        append!
        ;; Keep result assembly bounded while a wait reads successive disk windows.
        (fn [^String s]
          (when (pos? (.length s))
            (locking sb
              (swap! total (fn [t]
                             (+ (long t) (.length s))))
              (.append sb s)
              (when (> (.length sb) cap)
                (reset! trunc true)
                ;; keep the first `head-limit` chars + the last `tail-limit`;
                ;; excise the run between them so memory stays at ~cap.
                (.delete sb (int head-limit) (int (- (.length sb) tail-limit)))))))]

    {:append! append!
     :snapshot (fn []
                 (locking sb
                   (let [s
                         (.toString sb)

                         omitted
                         (if @trunc (- (long @total) cap) 0)]

                     {:text (if @trunc
                              (str (subs s 0 head-limit)
                                   "\n\n…[" omitted
                                   " chars omitted]…\n\n" (subs s head-limit))
                              s)
                      :truncated @trunc
                      ;; Exact dropped-char count: the text now carries an inline marker, so a
                      ;; caller can SEE both that it is no longer parseable and how much is gone.
                      :omitted omitted})))}))

(defn- capture-log
  "Read a fixed on-disk snapshot into the request's bounded head/tail result.
   A growing file cannot extend this read beyond the length observed at entry."
  [^File file]
  (let [capture
        (capped-capture max-sync-head-chars max-sync-tail-chars)

        length
        (.length file)]

    (loop [offset 0]
      (when (< offset length)
        (when (.isInterrupted (Thread/currentThread))
          (throw (InterruptedException. "Shell log read interrupted")))
        (let [chunk (shell-log/read-chunk "run"
                                          file
                                          {:offset offset
                                           :limit (min (long shell-log/max-chunk-bytes)
                                                       (- length offset))})
              next-offset (long (:next-offset chunk))]

          ((:append! capture) (:text chunk))
          (when (> next-offset offset) (recur next-offset)))))
    ((:snapshot capture))))

(defn- truncation-note
  "Note for a command whose capture lost a middle. Truncation splices a marker
   into the text, so the stream is no longer valid JSON/CSV — say so, and say
   what to do, instead of leaving a caller to decode a parser's
   \"Invalid control character\" on its own. nil when nothing was dropped."
  [out]
  (let [n (long (or (:omitted out) 0))]
    (when (pos? n)
      (str "output truncated · "
           n
           " chars dropped from the middle"
           " — the text carries an inline marker and no longer parses; narrow the output"
           " (`--jq`, `--limit`, `head -c`) or redirect it to a file and read that."))))

(defn- ->whole-long
  "Coerce a CPython-crossed numeric option to a WHOLE long, or throw a typed
   error. nil passes through (caller supplies the default).

   Rounding a fraction is a silent MODE CHANGE, not a convenience: an
   `offset: 0.4` rounds to a different byte. A number the caller cannot have
   meant is refused at the boundary, where the message can still name the option."
  [x what]
  (cond (nil? x) nil
        (number? x) (let [d (double x)]
                      (cond (or (Double/isNaN d) (Double/isInfinite d))
                            (throw (ex-info
                                     (str what " must be a finite number, got " (pr-str x) ".")
                                     {:type ::bad-option :option what :value x}))
                            (not= d (Math/rint d))
                            (throw (ex-info (str what
                                                 " must be a whole number, got "
                                                 (pr-str x)
                                                 " — a fraction rounds into a different value.")
                                            {:type ::bad-option :option what :value x}))
                            :else (long d)))
        :else (throw (ex-info (str what " must be a number, got " (pr-str x) ".")
                              {:type ::bad-option :option what :value x}))))

(defn- ->pos-long
  "[[->whole-long]] that also refuses a NEGATIVE, for an option where a negative
   has no reading: clamping a `limit: -5` into a real read is the same silent
   mode change as rounding a fraction.

   `logs` takes its `offset` and its `lines` through [[->whole-long]] instead,
   because there a negative IS a reading — the last n LINES back from the end, as
   in `tail -n 50`, and `lines: -10` the ten ABOVE an offset."
  [x what]
  (let [n (->whole-long x what)]
    (when (and n (neg? (long n)))
      (throw (ex-info (str what " must not be negative, got " (pr-str x) ".")
                      {:type ::bad-option :option what :value x})))
    n))

(defn- one-line
  "Collapse a command to a single display line capped at `limit` chars."
  [s ^long limit]
  (let [s (-> (str s)
              (str/replace #"\s+" " ")
              str/trim)]
    (if (> (count s) limit) (str (subs s 0 limit) "…") s)))

(defn- resolve-dir
  "Resolve a command's `cwd` against the primary workspace, then authorize the
   canonical result against every filesystem root in the immutable environment
   snapshot plus the live workspace overlay. An explicitly disabled process jail
   allows every host path; existence and directory checks still apply. A configured
   sibling such as `../svar` is valid when that canonical sibling is an allowed root."
  ^File [opts]
  (let [env
        (::environment opts)

        root
        (.getCanonicalFile (workspace/cwd))

        env-roots
        (workspace/env-filesystem-roots env)

        roots
        (->> (concat [{:root (.getPath root)}] env-roots workspace/*filesystem-roots*)
             (keep #(some-> (:root %)
                            io/file
                            .getCanonicalFile))
             distinct
             vec)

        dir-value
        ;; `cwd` is THE name for a working directory across the tool surface
        ;; (`shell`, extensions, and Python's own `os.getcwd`). There is no other
        ;; spelling: `dir` is gone, not aliased.
        (get opts "cwd")

        requested
        (when-not (str/blank? (str (or dir-value ""))) (str dir-value))]

    (if-not requested
      root
      (let [requested-file
            (io/file (paths/expand-home requested))

            dir
            (.getCanonicalFile
              (if (.isAbsolute requested-file) requested-file (io/file root requested)))

            allowed?
            (or (::unrestricted? opts)
                (some (fn [^File allowed-root]
                        ;; Path#startsWith is component-aware and handles a filesystem root
                        ;; correctly (`/` is not naively joined with another `/`).
                        (or (= dir allowed-root)
                            (.startsWith (.toPath dir) (.toPath allowed-root))))
                      roots))

            root-paths
            (mapv #(.getPath ^File %) roots)]

        (cond (not allowed?) (throw (ex-info (str
                                               "shell cwd '"
                                               requested
                                               "' resolves outside the allowed filesystem roots ("
                                               (str/join ", " root-paths)
                                               ").")
                                             {:type ::dir-unresolved
                                              :dir requested
                                              :resolved (.getPath dir)
                                              :exists (.exists dir)
                                              :roots root-paths}))
              (not (.exists dir))
              (throw (ex-info (str "shell cwd '" requested "' does not exist (" (.getPath dir) ").")
                              {:type ::dir-unresolved
                               :dir requested
                               :resolved (.getPath dir)
                               :exists false
                               :roots root-paths}))
              (not (.isDirectory dir))
              (throw
                (ex-info
                  (str "shell cwd '" requested "' is a file, not a directory (" (.getPath dir) ").")
                  {:type ::dir-unresolved
                   :dir requested
                   :resolved (.getPath dir)
                   :exists true
                   :roots root-paths}))
              :else dir)))))

(defn- last-redraw
  "The one frame of `line` a terminal would still be showing. A bare CR sends the
   cursor home and the text after it REDRAWS the line, so only the final non-empty
   segment was ever on screen; the empty segments a leading or trailing CR leaves
   behind are cursor moves, not blank frames."
  ^String [^String line]
  (or (last (remove empty? (str/split line #"\r" -1))) ""))

(defn- lf
  "Plain text from a PTY stream: CRLF to LF so captured output is byte-identical on
   every OS, and every bare CR resolved to the frame it left standing. A progress
   writer redraws ONE line — git sends 28 `Counting objects: N%` frames separated
   by CR — and a stream that keeps them all is 28 lines of noise that pushes the
   result the caller asked for out of the capped window."
  ^String [^String s]
  (when s
    (let [s (.replace s "\r\n" "\n")]
      (if (neg? (.indexOf s "\r")) s (str/join "\n" (map last-redraw (str/split s #"\n" -1)))))))

(def ^:private terminal-escape-re
  ;; ANSI/VT sequences a PTY-attached tool emits. Stripped both when MATCHING a
  ;; `wait` predicate and on every capture a caller reads, so the two agree.
  ;; The bare-ESC alternative ends at `[0-~]`, not `[@-~]`: the two-byte private
  ;; escapes `ESC =` / `ESC >` (keypad mode, written by `less` and every other
  ;; full-screen tool) end in 0x3D/0x3E, so a `@-~` final left their last byte
  ;; behind as a literal `=` or `>` in otherwise clean output. `ESC 7` / `ESC 8`
  ;; (save/restore cursor) were escaping the same way.
  #"(?s)(?:\u001B\].*?(?:\u0007|\u001B\\)|(?:\u001B\[|\u009B)[0-?]*[ -/]*[@-~]|\u001B[ -/]*[0-~])")

(def ^:private non-printing-control-re #"[\u0000-\u0008\u000B\u000C\u000E-\u001F\u007F-\u009F]")

(defn- normalize-terminal-output
  "The ONE reading of a PTY capture, shared by every consumer: ANSI/VT escapes and
   non-printing controls removed, tabs and line feeds kept, and a redrawn line
   resolved to the frame the terminal was left showing. What a human sitting at
   that terminal SAW is what `out` carries and every consumer displays — a colour
   reset git wrote ONLY because our pty made isatty() true is invisible to a human
   and a literal `[m` to anyone reading the string. The raw stream stays whole on
   disk at `log_path` for anyone who wants the bytes."
  [s]
  (when (some? s)
    (-> (str s)
        (str/replace terminal-escape-re "")
        ;; Controls go before the redraws collapse: an erase or a backspace is not a
        ;; frame, and leaving it in would make an empty segment look like one.
        (str/replace non-printing-control-re "")
        (lf))))

(defn- terminal-mode
  "Finite ANSI/VT state; :visible emits this character and returns to plain text."
  [mode ^long c]
  (case mode
    :osc
    (case c
      7
      :plain

      27
      :osc-escape

      :osc)

    :osc-escape
    (case c
      92
      :plain

      7
      :plain

      27
      :osc-escape

      :osc)

    :escape
    (cond (= c 91) :csi
          (= c 93) :osc
          (<= 0x20 c 0x2f) :escape-intermediate
          (<= 0x30 c 0x7e) :plain
          :else (terminal-mode :plain c))

    :escape-intermediate
    (cond (<= 0x20 c 0x2f) :escape-intermediate
          (<= 0x30 c 0x7e) :plain
          :else (terminal-mode :plain c))

    :csi
    (cond (<= 0x30 c 0x3f) :csi
          (<= 0x20 c 0x2f) :csi-intermediate
          (<= 0x40 c 0x7e) :plain
          :else (terminal-mode :plain c))

    :csi-intermediate
    (cond (<= 0x20 c 0x2f) :csi-intermediate
          (<= 0x40 c 0x7e) :plain
          :else (terminal-mode :plain c))

    (case c
      27
      :escape

      155
      :csi

      :visible)))

(defn- terminal-chunk
  "Strip escapes without retaining an OSC payload, even across byte windows."
  [mode ^String text emit?]
  (let [out (when emit? (StringBuilder.))]
    (loop [i 0
           mode mode]

      (if (= i (.length text))
        {:mode mode :text (when out (.toString out))}
        (let [c (.charAt text i)
              next-mode (terminal-mode mode (long (int c)))
              visible? (= :visible next-mode)]

          (when (and out visible?) (.append out c))
          (recur (inc i) (if visible? :plain next-mode)))))))

(defonce ^:private ^LinkedHashMap terminal-checkpoints (LinkedHashMap. 256 (float 0.75) true))

(defn- terminal-log-text
  "Normalize a raw log window without sharing a reader cursor. The bounded cache
   holds only parser modes, never output. A cold/random read replays its prefix
   in bounded chunks; following next_offset reuses the preceding checkpoint.
   Active logs use the spawn's exit atom as a generation token. Retired files
   use filesystem identity and revision, so replacement cannot reuse old state."
  [id ^File file entry {:keys [offset next-offset text]}]
  (if (empty? text)
    ""
    (let [^"[Ljava.nio.file.LinkOption;" options
          (make-array LinkOption 0)

          ^BasicFileAttributes attrs
          (Files/readAttributes (.toPath file) BasicFileAttributes options)

          ^LinkedHashMap checkpoints
          terminal-checkpoints

          generation
          [(.getPath file) (.fileKey attrs) (.creationTime attrs)
           (or (:exit entry) [(.size attrs) (.lastModifiedTime attrs)])]

          key
          [generation offset]

          mode
          (or (locking checkpoints (.get checkpoints key))
              (loop [at
                     0

                     mode
                     :plain]

                (if (>= at offset)
                  mode
                  (let [chunk
                        (shell-log/read-chunk id
                                              file
                                              {:offset at
                                               :limit (min shell-log/default-chunk-bytes
                                                           (- offset at))})

                        next-at
                        (:next-offset chunk)]

                    (when (<= next-at at)
                      (throw (ex-info "Shell log changed while reading terminal context."
                                      {:id id :offset offset :at at})))
                    (recur next-at (:mode (terminal-chunk mode (:text chunk) false)))))))

          result
          (terminal-chunk mode text true)]

      (locking checkpoints
        (.put checkpoints key mode)
        (.put checkpoints [generation next-offset] (:mode result))
        (while (> (.size checkpoints) 256)
          (.remove checkpoints (.next (.iterator (.keySet checkpoints))))))
      (normalize-terminal-output (:text result)))))

(defn- bash-command
  "Bash executable to run commands with — bash on EVERY platform, so the model
   writes one command dialect everywhere. Under WSL the JVM reports
   `os.name=Linux`, so this is WSL's own real `bash` (correct — we're a Linux
   process there)."
  []
  "bash")

(defn- jail-policy
  "Resolve the per-session jail policy carried by `env`.

   A present policy function is security-critical: failures propagate and deny the
   spawn instead of silently returning nil/unwrapped argv. `jail.enabled: false` is
   represented explicitly by `{:disabled? true}` and remains the sole escape hatch."
  [env]
  (when-let [f (:jail-policy-fn env)]
    (or (f)
        (throw (ex-info "Shell process denied: session jail policy is unavailable"
                        {:type ::jail-policy-missing :session-id (:session-id env)})))))

(defn- call-jail-policy
  "The policy for ONE spawn: this session's jail policy carrying THIS call's own
   `env` delta. The delta is an argument of the call that spawns, so it resolves
   here, at the spawn boundary — and a refused name (not a variable name, a
   pre-exec hijack, a source that produced nothing) denies that command by name
   instead of running it without the variable it asked for."
  [env opts]
  (let [delta (or (::validated-env opts) (process-jail/validate-call-env (get opts "env")))]
    (cond-> (process-jail/with-call-env (jail-policy env) (process-jail/resolve-call-env delta))
      (:session/scratch env) (assoc :session/scratch (:session/scratch env)))))

(defn- command-note
  "The single advisory line a command result carries. Truncation wins — it changes
   whether the text parses at all; otherwise a denied macOS Keychain lookup is
   named, since a confined `gh`/`git`/`security` otherwise fails with an opaque
   Security-framework message and no mention of the jail."
  [env out]
  (or (truncation-note out)
      (when (process-jail/keychain-denial? (:text out))
        ;; No policy fn means no jail: an opaque Security failure is then a real
        ;; Keychain miss and blaming confinement would send the caller the wrong way.
        (when-let [policy (try (jail-policy env) (catch Throwable _ nil))]
          (process-jail/keychain-denial-hint policy (:text out))))))

(defn- fd-exhaustion?
  "True when `t` (or any cause under it) is the OS refusing a spawn because THIS
   process ran out of file descriptors (EMFILE).

   The JDK reports EMFILE as a plain `IOException` whose text blames the spawn
   helper — \"Bad file descriptor\", \"Spawn helper ran into JDK version mismatch\",
   \"Re-install JDK\" — so the raw message sends the reader after a toolchain bug
   that does not exist. `error=24` / `error: 24` and the plain phrase are the
   reliable markers."
  [^Throwable t]
  (boolean (loop [^Throwable e t]
             (when e
               (let [m (str (.getMessage e))]
                 (if (or (str/includes? m "Too many open files")
                         (str/includes? m "error=24")
                         (str/includes? m "error: 24"))
                   true
                   (recur (.getCause e))))))))

(def ^:private fd-exhausted-message
  "What to SAY when a spawn is out of descriptors — the JDK's own text names the
   wrong culprit, so replace it with the real cause and the way out."
  (str "Out of file descriptors: this process hit its open-file limit, so no child "
       "process can be spawned (the JDK blames its spawn helper / a JDK mismatch; "
       "it is neither). Already retried once after asking the JVM to reclaim "
       "descriptors held by unreachable objects. Usual cause: sandbox Python that "
       "opened files without closing them — a dropped file object is NOT closed "
       "there, so always `with open(...) as f:` — or many live background shells "
       "(`sh.cancel()`). Free them and retry."))

(defn- spawn-retrying-fds
  "Run `spawn` (a thunk that starts an OS process) and, ONLY when it failed
   because this process is out of file descriptors, reclaim and try once more.

   A leaked descriptor is held by an unreachable object, so a GC + finalization
   pass genuinely returns it; without this, one leaky sandbox block wedges EVERY
   later `shell` call for the rest of the session. A persistent failure is
   rethrown as a typed `ex-info` carrying the real diagnosis, cause attached."
  [spawn]
  (try (spawn)
       (catch Throwable t
         (if-not (fd-exhaustion? t)
           (throw t)
           (do (System/gc)
               (System/runFinalization)
               (Thread/sleep 150)
               (try (spawn)
                    (catch Throwable t2
                      (throw (ex-info fd-exhausted-message {:type ::fd-exhausted} t2)))))))))

(defn- cleanup-jail-policy!
  [policy]
  (when-let [cleanup (::cleanup policy)]
    (try (cleanup) (catch Throwable _ nil))))

(defn- resolve-dir-for-policy
  ^File [opts env policy]
  (try (resolve-dir (cond-> (assoc (or opts {}) ::environment (or (::environment policy) env))
                      (:disabled? policy)
                      (assoc ::unrestricted? true)))
       (catch Throwable t (cleanup-jail-policy! policy) (throw t))))

(def ^:private child-env
  "What EVERY shell child is handed, pty or not, confined or not.

   `GIT_OPTIONAL_LOCKS=0` is this extension's half of the index-lock contract
   `internal.workspace.git/git-argv` keeps for Vis' own calls. A plain `git status` or
   `git diff` REFRESHES the index, and refreshing WRITES it through
   `.git/index.lock` — so an agent-run status competed for the one lock the
   repo has against the footer poll, the environment snapshot and the rewind
   baseline, and one that was killed mid-refresh left the lock behind for
   every later git to trip over. `0` drops only the OPTIONAL locks: `add`,
   `commit`, `stash` and every other writer still take the lock they need.

   The editor names are `true`, because no person can use an editor here. git
   then keeps its default message for a merge, a revert or `rebase --continue`.
   A `commit` without `-m` stops with an empty-message error. `GIT_EDITOR` also
   outranks a `core.editor` that the operator configured."
  {"GIT_OPTIONAL_LOCKS" "0" "GIT_EDITOR" "true" "VISUAL" "true" "EDITOR" "true"})

(defn- spawn!
  ^Process [cmd ^File dir policy]
  (try (let [args
             (if (sequential? cmd)
               (mapv str cmd)
               [(bash-command) "--noprofile" "--norc" "-lc" (str cmd)])

             ^Process p
             (spawn-retrying-fds #(process-jail/spawn! args
                                                       dir
                                                       policy
                                                       {:extra-environment child-env
                                                        :merge-stderr? true}))]

         (when (::cleanup policy)
           (.thenRun (.onExit p)
                     ^Runnable
                     (reify
                       Runnable
                         (run [_] (cleanup-jail-policy! policy)))))
         p)
       (catch Throwable t (cleanup-jail-policy! policy) (throw t))))

(def ^:private pty-child-env
  "What every PTY child gets ON TOP of the ambient (or allowlisted) environment.

   `TERM` is the whole point of the pty: `isatty()` is true and terminfo names a
   real terminal, so a CLI that refuses a dumb pipe runs.

   The PAGERS are the bill for that honesty, and it is paid here. `git`, `gh`,
   `man`, `psql` and friends check `isatty(stdout)`, conclude a human is
   watching and fork `less` THEMSELVES — the tool spawns it, not bash and not
   this extension. `less` then writes SCREEN CONTROL into the same byte stream
   the log file keeps: `ESC =` / `ESC >` (keypad application/numeric mode) on
   entry and exit, erase-to-end-of-line, and soft wrapping that breaks a long
   line mid-token with `space BS`. A live terminal eats those bytes; a log
   READER sees a stray `=` above the output, a `>` glued to the next command's
   first line and paths split in half — and reasonably reads it as corruption.
   `cat` as the pager makes the captured bytes the bytes the command printed.
   `GIT_PAGER` is set as well as `PAGER` because it also outranks a `core.pager`
   the operator configured; a caller who genuinely wants paging still prefixes
   its own assignment (`PAGER=less …`), which wins over an inherited value.

   [[child-env]] rides along: a pty child is a shell child like any other."
  (assoc child-env
    "TERM" "xterm-256color"
    "PAGER" "cat"
    "GIT_PAGER" "cat"))

(defn- pty-spawn!
  "Spawn `cmd` through the runtime-owned PTY and jail boundary."
  [cmd ^File dir policy]
  (try (assoc (spawn-retrying-fds
                #(pty/spawn! {:command [(bash-command) "--noprofile" "--norc" "-lc" (str cmd)]
                              :dir (.getPath dir)
                              :env (process-jail/process-environment policy pty-child-env)
                              :policy policy
                              :cols 120
                              :rows 40}))
         ::cleanup (::cleanup policy))
       (catch Throwable t (cleanup-jail-policy! policy) (throw t))))

(def ^:private kill-grace-ms
  "How long `kill-tree!` waits after TERM before KILL, and after KILL for the exit."
  2000)

(defn kill-tree!
  "Destroy an OWNED process and its reachable descendants: TERM, then KILL after
   a 2s grace, then wait up to 2s for exit and reap the owned Process. Used by
   shells and one-shot test runners; never use on an externally owned REPL.
   Accepts a java.lang.Process or a PTY handle map. Never throws; preserves
   interrupts without letting cancellation skip escalation. A child that has
   already detached/reparented before the snapshot is outside this tree."
  [p]
  (let [interrupted? (volatile! (Thread/interrupted))]
    (try (let [pid (if (map? p) (:pid p) (.pid ^Process p))
               destroy (if (map? p)
                         (:destroy p)
                         (fn [force?]
                           (if force? (.destroyForcibly ^Process p) (.destroy ^Process p))))
               ^ProcessHandle ph (.orElse (ProcessHandle/of pid) nil)
               descendants (if ph
                             (with-open [stream (.descendants ph)]
                               (vec (iterator-seq (.iterator stream))))
                             [])
               alive? (fn []
                        (or (and ph (.isAlive ph))
                            (some (fn [^ProcessHandle d]
                                    (.isAlive d))
                                  descendants)))
               await-exit! (fn []
                             (let [deadline (+ (System/nanoTime) (* (long kill-grace-ms) 1000000))]
                               (loop []

                                 (when (and (alive?) (< (System/nanoTime) deadline))
                                   (try (Thread/sleep 50)
                                        (catch InterruptedException _ (vreset! interrupted? true)))
                                   (recur)))))]

           ;; Keep handles before TERM: a launcher can exit before its children.
           (run! (fn [^ProcessHandle d]
                   (try (.destroy d) (catch Throwable _ nil)))
                 descendants)
           (destroy false)
           (await-exit!)
           (run! (fn [^ProcessHandle d]
                   (try (when (.isAlive d) (.destroyForcibly d)) (catch Throwable _ nil)))
                 descendants)
           (when (and ph (.isAlive ph)) (destroy true))
           (await-exit!)
           (when (instance? Process p)
             (try (.waitFor ^Process p 0 TimeUnit/MILLISECONDS)
                  (catch InterruptedException _ (vreset! interrupted? true)))))
         (catch Throwable t (cancellation/preserve-interrupt! t))
         (finally (when @interrupted? (.interrupt (Thread/currentThread))))))
  nil)

;; BLOCKING runner — INTERNAL only (`run-blocking`, `run-argv`, the bang path)

(defn- clamp-timeout-secs
  "Effective wait in seconds from the opts value: `rt/DEFAULT_SHELL_TIMEOUT_SECS`
   when unset, floor 1, capped at `rt/MAX_SHELL_TIMEOUT_SECS`.
   `label` is the option the CALLER spelled, so a refusal names the key that was
   actually passed (`seconds` on a handle wait) instead of an internal one."
  (^long [v] (clamp-timeout-secs v "timeout_secs"))
  (^long [v label]
   (-> (long (or (->pos-long v label) default-timeout-secs))
       (max 1)
       long
       (min (long max-timeout-secs)))))

(def ^:private shell-result-base
  "The ONE result shape of the whole shell family — every stage, every tool.

   Final results and operation results have this key set. A live Task read
   omits `timed_out` and `timeout_secs`: reading the fields does not wait.
   `stage` identifies the operation that gave the result.

    But a key DERIVABLE from another key is not carried at all — one fact, one
    field: `pid` already says the child was spawned, `status` \"stopped\" already
    says a stop landed, `is_eof` false already says the read has more to come,
    `keys` already shows what was typed, and `attach` is the whole bridge command
    a caller needs, so its socket path stays the bridge's own business.

   ONE call runs ONE command, so there is no `commands` array and no entry to index
   into, and a lifecycle stage carries the same `command` as the shell it acts on.

   NOT `\"op\"`: the extension boundary stamps `\"op\"` on EVERY tool result with the
   tool's own origin — stage detail must use a different key, exactly as
   `stamp-public-result-op` requires."
  {"stage" nil
   "id" nil
   "cwd" nil
   ;; The ONE command THIS result is about: the bash line the call ran, or the
   ;; line the shell it acts on is running. nil only when the stage genuinely has
   ;; no command left (a stopped shell whose registry entry is already gone).
   "command" nil
   "status" nil
   "pid" nil
   "exit" nil
   "duration_ms" nil
   "uptime_ms" nil
   ;; WHEN, in epoch milliseconds, so a caller can say how long ago a shell started
   ;; and whether it has finished at all. `finished_at` nil IS "still running" —
   ;; the same fact `exit` nil carries, spelled on the clock.
   "started_at" nil
   "finished_at" nil
   ;; WHERE the bytes are ON THIS MACHINE. The log is an ordinary file, so `cat`,
   ;; `grep` and a human's editor reach it without going through a handle at all.
   "log_path" nil
   ;; LIVE cost of the process TREE, sampled at the moment this result was built:
   ;; nil once the child is gone, because usage is a measurement and there is
   ;; nothing left to measure.
   "cpu_ms" nil
   "cpu_percent" nil
   "rss_bytes" nil
   "timed_out" false
   "timeout_secs" nil
   ;; A live process is waiting for input from its terminal.
   "waiting_for_input" false
   ;; OUTPUT, under ONE name everywhere: `out` — what a run captured, and what a
   ;; log read returned. Named for what it HOLDS, not for one of the two fds it
   ;; arrived on: every command runs under a real pty, where stdout and stderr ARE
   ;; ONE stream, so a `stdout`/`stderr` pair would be a choice with only one
   ;; answer — the second field nil, and misread as \"nothing went wrong\".
   ;; An empty string means THIS stage returned no output, not that the log is empty.
   "out" ""
   ;; A truncated stream has an inline \"…[N chars omitted]…\" marker spliced into
   ;; its MIDDLE, so it no longer parses — the count says exactly how much is gone,
   ;; and 0 means nothing was.
   "out_omitted_chars" 0
   ;; The log CURSOR: a read is a window on a file and the caller owns the cursor.
   "offset" 0
   "next_offset" 0
   "is_eof" true
   ;; Attach bridge for an interactive shell, and what `sh.type` wrote.
   "attach" nil
   "already_running" false
   "keys" nil
   "note" nil})

(defn- shell-result
  "One stage's fields merged onto [[shell-result-base]], with `stage` stamped last
   and anything outside the base dropped — so every result of every shell stage
   has exactly the same keys."
  [stage m]
  (assoc (merge shell-result-base (select-keys m (keys shell-result-base))) "stage" stage))

(defn- shell-quote
  "One argv token as a literal `bash -lc` word: plain words stay bare, anything
   else is single-quoted (with embedded quotes escaped), so a coerced argv keeps
   exactly the arguments it named."
  [^String token]
  (if (re-matches #"[A-Za-z0-9_@%+=:,./-]+" token)
    token
    (str "'" (str/replace token "'" "'\\''") "'")))

(defn- shell-run-impl
  "Run one command with disk-only output storage and a bounded result read.
   `cmd` is a bash line or literal argv. A supplied handle owns its sink and
   adopts a process whose wait expires. Standalone argv calls get a persistent
   log too, but still kill their process tree on timeout."
  ([env cmd] (shell-run-impl env cmd nil nil))
  ([env cmd opts] (shell-run-impl env cmd opts nil))
  ([env cmd opts {:keys [sink on-spawn]}]
   (let [argv
         (when (sequential? cmd) (mapv str cmd))

         cmd
         (if argv (str/join " " (map shell-quote argv)) (str cmd))]

     (when (str/blank? cmd)
       (throw (ex-info "shell needs a non-blank command — pass it as `command`, the first argument."
                       {:type ::blank-command})))
     (let [timeout-secs
           (clamp-timeout-secs (get opts "timeout_secs"))

           policy
           (call-jail-policy env opts)

           dir
           (resolve-dir-for-policy opts env policy)

           t0
           (util/now-ms)

           own-log?
           (nil? sink)

           id
           (when own-log? (str "run-" (random-uuid)))

           sink
           (or sink (shell-log/open! (:session-id env) id))

           drain-stopped?
           (atom false)

           ^Process p
           (try (spawn! (or argv cmd) dir policy)
                (catch Throwable t (shell-log/close! sink) (throw t)))]

       (try (when on-spawn (on-spawn p))
            ;; Nothing writes to this child's stdin, so it gets end of input at once: a
            ;; program that reads stdin must not wait for bytes that can never come.
            (try (.close (.getOutputStream p)) (catch Exception _ nil))
            (when own-log?
              (shell-log/index! (:db-info env)
                                (:session-id env)
                                id
                                {:command cmd
                                 :dir (.getPath dir)
                                 :log-path (:path sink)
                                 :started-at t0
                                 :ended-at nil
                                 :exit nil}))
            (let [out-f
                  (future (try (with-open [in (.getInputStream p)]
                                 (shell-log/drain! in sink))
                               (catch Throwable t
                                 ;; Deliberately closing a timed-out run can wake the
                                 ;; reader with IOException; a real IO failure must
                                 ;; still kill the undrained child and reach the caller.
                                 (when-not (and @drain-stopped? (instance? java.io.IOException t))
                                   (when (.isAlive p) (kill-tree! p))
                                   (throw t)))))

                  finished?
                  (.waitFor p timeout-secs TimeUnit/SECONDS)]

              (when (and (not finished?) own-log?)
                (reset! drain-stopped? true)
                (kill-tree! p)
                (try (.close (.getInputStream p)) (catch Throwable _ nil)))
              (when (or finished? own-log?) (deref out-f 5000 nil))
              (let [out
                    (capture-log (io/file (:path sink)))

                    exit
                    (when finished? (.exitValue p))

                    t1
                    (util/now-ms)]

                (when own-log?
                  (shell-log/index! (:db-info env)
                                    (:session-id env)
                                    id
                                    {:command cmd
                                     :dir (.getPath dir)
                                     :log-path (:path sink)
                                     :started-at t0
                                     :ended-at t1
                                     :exit (when-not (.isAlive p) (.exitValue p))}))
                (with-meta (shell-result "run"
                                         {"id" id
                                          "command" cmd
                                          "pid" (.pid p)
                                          "status" (if finished? "exited" "running")
                                          "out" (normalize-terminal-output (:text out))
                                          "exit" exit
                                          "duration_ms" (- t1 t0)
                                          "started_at" t0
                                          "finished_at" (when finished? t1)
                                          "log_path" (:path sink)
                                          "timed_out" (not finished?)
                                          "out_omitted_chars" (long (or (:omitted out) 0))
                                          "note" (command-note env out)})
                  {:dir (paths/unixify (.getPath dir))
                   :timeout-secs timeout-secs
                   :process (when-not finished? p)
                   :drains (when-not finished? [out-f])})))
            (catch Throwable t
              (reset! drain-stopped? true)
              (when (.isAlive p) (kill-tree! p))
              (try (.close (.getInputStream p)) (catch Throwable _ nil))
              (throw t))
            (finally (when own-log? (shell-log/close! sink))))))))

(defn- command-line
  "ONE bash line from the caller's `command`. A string IS the line. An array of tokens —
   the argv spelling a caller may take, and the shape a caller reaches for out of habit
   — is coerced by quoting each token and joining, instead of failing the call;
   a `java.util.List` from the Python surface is that same array.

   A MAP is the one wrong shape that is not a typo but a MISPLACED CALL: this
   op's own arguments written where its process lines belong. It is refused by
   NAME, so the caller is told where `op`/`id`/`n` actually go instead of being
   told again that commands are strings."
  [command]
  (cond (string? command) command
        (or (map? command) (instance? java.util.Map command))
        (throw (ex-info
                 (str "shell `command` is a bash line, not an options map: `op`, `id`, `n`, `until`"
                      " and `text` are TOP-LEVEL arguments — {\"op\": \"logs\", \"id\": \"build\","
                      " \"n\": 30} — and `command` carries only the line to run.")
                 {:type ::bad-commands}))
        (or (sequential? command) (instance? java.util.List command))
        (str/join " " (map (comp shell-quote str) command))
        :else (throw (ex-info "shell `command` must be a string — one bash -lc command line."
                              {:type ::bad-commands}))))

(defn- one-command
  "The caller's `command` as ONE bash line, via [[command-line]]. A blank line is
   refused — there is nothing to run. ONE call runs ONE command: an ordered batch
   was a second budget, a second result shape and a second failure mode for a
   thing `&&` already says, so it is gone."
  [command]
  (let [line (command-line command)]
    (when (str/blank? line)
      (throw (ex-info "shell needs a non-blank command." {:type ::blank-command})))
    line))

;; BACKGROUND — Python sandbox: `await shell("npm run dev", id="dev")`

(defonce ^:private bg-procs
  ;; { session-key -> { id -> {:proc :exit :pump :stopped? :command :dir
  ;; :started-at} } }. defonce so a dev `:reload` never orphans live processes.
  (atom {}))

(defonce ^:private bg-lifecycle-locks
  ;; A fixed stripe set keeps monitor identity stable across reloads without leaking
  ;; one lock per model-chosen id. Same session/id always lands on one stripe;
  ;; unrelated shells remain concurrent except for harmless hash collisions.
  (vec (repeatedly 64 #(Object.))))

(defn- bg-lifecycle-lock
  [session id]
  (nth bg-lifecycle-locks (mod (hash [(str session) (str id)]) (count bg-lifecycle-locks))))

(defonce ^:private _bridge-sweep
  ;; One-time GC at extension load: a prior vis crash/kill never ran serve!'s
  ;; :stop (the JVM held the AF_UNIX server), so stale attach sockets pile up in
  ;; bridge-dir. sweep-orphans! connect-probes each and unlinks the dead ones.
  (do (try (pty-bridge/sweep-orphans!) (catch Throwable _ nil)) true))

(defn- bg-entry [session id] (get-in @bg-procs [(str session) (str id)]))

(defn- prune-bg-session
  "Drop a session's map once its last shell is gone, so an idle session leaves no
   empty shell registry behind."
  [m sk]
  (if (empty? (get m sk)) (dissoc m sk) m))

(defn- live-entry?
  "True while this registry `entry` holds a RUNNING process. An entry reserved
   before its first process exists (a start claims its handle at the top of the
   batch) is not live yet."
  [entry]
  (boolean (when-let [alive? (:alive? (:proc entry))]
             (alive?))))

(defn- env-origin
  "The TRUST ORIGIN of the caller reaching the shell family.

   A handle crosses a trust boundary the moment it outlives the spawn: the jail is
   consulted only when a process is created, while `logs`/`type`/`stop` and a
   re-issue reach an existing process by id alone. Without an origin a JAILED
   extension in the same session could read a trusted shell's output, type at its
   PTY or kill it simply by naming the id. Every entry is stamped with the origin
   that created it and every later op must present the same one."
  [env]
  (str (or (:shell-origin env) "tool")))

(defn- authorize-origin!
  "Check the same entry that the operation will use. A stale token cannot
   read or change a replacement process under the same alias."
  ([env session id] (authorize-origin! env id (bg-entry session id) nil))
  ([env id entry token]
   (when (and token (not= token (:generation entry)))
     (throw (ex-info (str "Shell '" id "' is no longer the same process.")
                     {:type ::stale-shell :id id})))
   (let [want (env-origin env)
         have (or (:origin entry) want)]
     (when-not (= want have)
       (throw (ex-info (str "Shell '" id "' was started by a different trust origin.")
                       {:type ::foreign-shell :id id :origin have :caller want}))))))

(defn- canonical-dir
  [d]
  (when-let [d (some-> d
                       str
                       not-empty)]
    (try (.getCanonicalPath (io/file d)) (catch Throwable _ d))))

(defn- dir-conflict?
  "Do a requested `cwd` and a registry entry's own directory name DIFFERENT places?

   One rule answers it for the dedup that resolves a start to a live shell and for
   the re-issue that then accepts or refuses that shell. While they disagreed, the
   dedup handed back an id running elsewhere and the re-issue threw `id-in-use`
   over the id it had just been given. A directory absent on either side is no
   evidence of a conflict."
  [want have]
  (let [want
        (canonical-dir want)

        have
        (canonical-dir have)]

    (boolean (and want have (not= want have)))))

(defn- stopping-entry? [session {:keys [stopped? task]}]
  (or (some-> stopped? deref)
      (when-let [id (:id task)]
        (pos? (long (task/cancelling session id))))))

(defn- reissue-live-entry
  "The live entry `id` may be RE-ATTACHED to, or nil when the id is free.

   A re-issue is how a caller picks a shell it already started back up, so it is a
   success — but ONLY when it names the same work. Identity is (id, command, cwd):
   handing a live id a DIFFERENT command, or the same command with a different
   `cwd`, used to answer success for a process that never ran what was asked, in a
   directory nobody requested. That is a silent no-op, so it is refused."
  [env session id command cwd]
  (when-let [e (let [entry (bg-entry session id)]
                 (when (or (get-in entry [:proc :destroy]) (live-entry? entry)) entry))]
    (authorize-origin! env session id)
    (when (stopping-entry? session e)
      (throw (ex-info (str "Shell '" id "' is stopping. Wait for it to finish or use a different id.")
                      {:type ::id-stopping :id id})))
    (let [want-cmd (some-> command
                           str
                           not-empty)
          mismatch
          (cond (and want-cmd (not= want-cmd (str (:command e))))
                (str "it is running a different command (" (one-line (:command e) 60) ")")
                (dir-conflict? cwd (:dir e))
                (str "it is running in a different directory (" (canonical-dir (:dir e)) ")"))]

      (when mismatch
        (throw (ex-info (str "Shell '" id
                             "' is already running and " mismatch
                             " — nothing was started. Cancel it first with sh.cancel(),"
                             " or start this command under a different id.")
                        {:type ::id-in-use :id id})))
      e)))

(defn- bg-id-taken?
  "True while a claim, registered shell or retained log owns an automatic id.

   Exiting or retiring a process does not retire its handle: later reads still
   resolve by id. Reusing that name would truncate its log and silently redirect
   retained handles. Explicit named restarts remain the caller's choice."
  [session id entry]
  (boolean (or entry (.exists (shell-log/log-file session id)))))

(defn- drop-bg-entry!
  [session id]
  (let [sk
        (str session)

        id
        (str id)]

    (swap! bg-procs (fn [m]
                      (prune-bg-session (update m sk dissoc id) sk)))
    nil))

(defn- release-bg-claim!
  "Give a CLAIMED id back when its start never reached a process.

   Only an unspawned claim is dropped: registering a process replaces the entry,
   which then carries no `:claim`, so a failing start can neither erase the shell
   that took the id over nor push every later auto id one suffix along for the
   rest of the session."
  [session id]
  (let [sk
        (str session)

        id
        (str id)]

    (swap! bg-procs (fn [m]
                      (if (:claim (get-in m [sk id]))
                        (prune-bg-session (update m sk dissoc id) sk)
                        m)))
    nil))

(defn- releasing-claim
  "Run `f` on the id a start just claimed, releasing the claim if `f` throws
   before any process was registered under it."
  [session id f]
  (try (f id) (catch Throwable t (release-bg-claim! session id) (throw t))))

(defn- command->bg-id-slug
  "Name a background shell after the program it runs: `npm run dev` -> `npm`.
   Setup prefixes (`cd …`, `export …`, `VAR=1 …`) are skipped so the id names what
   is actually being watched, and a pathed binary keeps only its basename."
  [command]
  (let [program
        (->> (str/split (str command) #"&&|\|\||;|\n")
             (map str/trim)
             (remove str/blank?)
             (keep (fn [segment]
                     (->> (str/split segment #"\s+")
                          (remove str/blank?)
                          (remove #(str/includes? % "="))
                          first)))
             (remove #{"cd" "export" "source" "." "env" "sudo" "exec" "set" "nohup"})
             first)

        slug
        (-> (str (or program "shell"))
            (str/replace #"^.*/" "")
            str/lower-case
            (str/replace #"[^a-z0-9]+" "-")
            (str/replace #"^-+" "")
            (str/replace #"-+$" ""))]

    (if (str/blank? slug) "shell" (subs slug 0 (min 24 (count slug))))))

(defn- auto-bg-id
  "Derive AND CLAIM the handle for a background START that carried none.

   Starting a long process is the ONE background call that does not act on an
   existing handle, so rejecting `{op: \"background\", command: \"…\"}` for a missing
   `id` failed a well-formed call over a name the caller had to invent — the most
   frequent shell dead end there is.

   Re-issuing the SAME script in the SAME directory while it runs returns that
   shell's own id, so the duplicate start resolves to `already_running` instead of a
   second dev server; otherwise the program name is suffixed until no claim, shell
   or retained log owns it, so an auto id never hijacks an earlier run.

   `cwd` belongs in that key because a re-issue's identity is (id, command, cwd):
   matching on the command alone resolved `git status` in a second repo to the first
   repo's live shell, and `reissue-live-entry` then refused the id it had just been
   handed. Only the SEQUENTIAL spelling hit it — concurrent starts derive their ids
   inside the claim window, where nothing is live to match — so whether a start was
   legal depended on being batched.

   The id is committed by the same `bg-procs` update that finds it. Searching
   through a plain read and registering afterwards is a check-then-act: two
   concurrent starts of one program both saw `echo` free, both took it, and then
   shared a single registry entry, log file and attach socket — one handle read
   the other child's output, which is how a `gather` of shells over the same
   program crossed its results. A loser of the CAS searches again against a map
   that already holds the winner's claim, and the committed id is read back by the
   claim token because an earlier attempt may have picked a different candidate.
   `release-bg-claim!` hands the id back if the start never spawns."
  [env session command cwd]
  (let [origin
        (env-origin env)

        wanted
        (str command)

        sk
        (str session)

        ;; Only a shell THIS origin started IN THIS directory can be resolved to; a
        ;; live shell of another origin, or of another `cwd`, is skipped by the
        ;; suffix loop like any other taken id.
        running-same
        (->> (get @bg-procs sk)
             (filter (fn [[_ entry]]
                       (and (= wanted (:command entry))
                            (= origin (str (or (:origin entry) origin)))
                            (not (dir-conflict? cwd (:dir entry)))
                            (live-entry? entry)
                            (not (stopping-entry? session entry)))))
             ffirst)

        base
        (command->bg-id-slug wanted)

        token
        (str (java.util.UUID/randomUUID))]

    (or running-same
        (let [[_ committed]
              (swap-vals!
                bg-procs
                (fn [m]
                  (let [ids (get m sk)
                        id (loop [n 1]
                             (let [candidate (if (= 1 n) base (str base "-" n))]
                               (cond (not (bg-id-taken? session candidate (get ids candidate)))
                                     candidate
                                     (< n 100) (recur (inc n))
                                     :else (str base "-" (System/nanoTime)))))]

                    (assoc-in m
                      [sk id]
                      {:claim token :command wanted :script wanted :origin origin}))))]
          (some (fn [[id entry]]
                  (when (= token (:claim entry)) id))
                (get committed sk))))))

(defn- retire-bg-entry!
  "Keep completed handle metadata, never its process, streams or pump closures.
   The caller holds the lifecycle lock; an old pump cannot retire a successor."
  [session id p]
  (swap! bg-procs (fn [m]
                    (let [path
                          [(str session) (str id)]

                          entry
                          (get-in m path)]

                      (if (identical? p (:proc entry))
                        (assoc-in m
                          path
                          (-> entry
                              (dissoc :pump :send :bridge :final :usage)
                              (assoc :proc {:pid (:pid p)})))
                        m))))
  nil)

(defn- stop-bg-generation!
  "Stop exactly the generation that installed this callback. Do not join the
   pump under its lifecycle lock. The pump closes the log and completes the Task."
  [session id exit-atom]
  #_{:clj-kondo/ignore [:locking-suspicious-lock]}
  (locking (bg-lifecycle-lock session id)
    (let [{:keys [proc stopped? bridge] :as entry} (bg-entry session id)]
      (when (and (identical? exit-atom (:exit entry)) (:destroy proc)
                 (compare-and-set! stopped? false true))
        (when (live-entry? entry) (kill-tree! proc))
        (when-let [in (:in proc)]
          (try (.close ^java.io.InputStream in) (catch Throwable _ nil)))
        (when bridge (try ((:stop bridge)) (catch Throwable _ nil)))
        (resources/unregister! session id)))))

(defn- shell-resource-fns
  "Resource callbacks retain only identity and a path, not process/output state."
  [session id task-id exit-atom file]
  {:stop-fn (fn []
              (when (task/cancel! session task-id resources/*stop-reason*)
                (let [entry (bg-entry session id)
                      ^Thread pump (when (identical? exit-atom (:exit entry)) (:pump entry))]
                  (stop-bg-generation! session id exit-atom)
                  ;; A stop returns after the pump records the exit, outside the lifecycle lock.
                  (when (and pump (not (identical? pump (Thread/currentThread))))
                    (try (.join pump 3000) (catch InterruptedException _ nil))))))
   :alive-fn (fn []
               (identical? exit-atom (:exit (bg-entry session id))))
   :logs-fn (fn []
              (-> (shell-log/read-chunk id file)
                  :text
                  normalize-terminal-output
                  str/split-lines))
   :health-fn (fn []
                (let [entry (bg-entry session id)]
                  (cond (not (identical? exit-atom (:exit entry))) :down
                        (some-> (:log-error entry) deref) :failed
                        (nil? @exit-atom) :running
                        (zero? (long @exit-atom)) :exited
                        :else :failed)))})

(defn- start-pump!
  "Drain to disk first, then notify the process owner. The caller starts the
   returned thread only after it publishes this generation."
  ^Thread [id p stopped? sink log-error finish!]
  (doto (Thread.
          (fn []
            (try
              (try (with-open [in ^java.io.InputStream (:in p)]
                     (shell-log/drain! in sink))
                   (catch Throwable t
                     (when-not @stopped?
                       (reset! log-error (str "Shell log write failed: " (.getMessage t)))
                       (kill-tree! p))))
              (shell-log/close! sink)
              (finish! (try ((:wait p)) (catch Throwable _ nil)))
              (finally (cleanup-jail-policy! {::cleanup (::cleanup p)})))))
    (.setName (str "vis-shell-bg-" id))
    (.setDaemon true)))

(defn- tree-handles
  "This process and every descendant of it that is still alive. A shell's cost is
   the cost of the TREE it started — the `bash -lc` line is nearly free, the
   compiler it launched is not — so nothing here reports the wrapper alone."
  [pid]
  (when pid
    (when-let [^ProcessHandle h (.orElse (ProcessHandle/of (long pid)) nil)]
      (into [h] (.toList (.descendants h))))))

(defn- tree-cpu-ms
  "Total CPU time the tree has burned, in milliseconds, or nil when the platform
   will not say. This is CPU CONSUMED, not wall time: a shell asleep for an hour
   reports single-digit milliseconds, which is exactly how \"is it working or is it
   stuck\" is answered."
  [handles]
  (let [ms (keep (fn [^ProcessHandle h]
                   (some-> ^java.time.Duration (.orElse (.totalCpuDuration (.info h)) nil)
                           (.toMillis)))
                 handles)]
    (when (seq ms) (reduce + ms))))

(defn- cpu-time->ms
  "One `ps` CPU-time cell as milliseconds: `[[DD-]HH:]MM:SS[.ss]`. `ps` is the only
   portable source of a CHILD's CPU time — the JDK's `totalCpuDuration` is empty
   for a process this JVM did not measure — so the parse lives here rather than
   the number being dropped."
  [^String cell]
  (try (let [[days rest]
             (if (str/includes? cell "-") (str/split cell #"-" 2) [nil cell])

             parts
             (mapv #(Double/parseDouble %) (str/split rest #":"))

             secs
             (reduce (fn [acc p]
                       (+ (* 60.0 (double acc)) (double p)))
                     0.0
                     parts)]

         (long (* 1000.0 (+ (double secs) (* 86400.0 (double (if days (Long/parseLong days) 0)))))))
       (catch Exception _ nil)))

(defn- tree-ps-usage
  "Resident memory, CPU share and CPU time of the tree, read from `ps`. The JDK
   exposes neither RSS nor a child's CPU time, and RSS is the number a human means
   by \"how much RAM is this using\". Best effort by construction: a pid that exited
   between the listing and the read simply contributes nothing, and a platform
   without `ps` answers nil rather than failing the stage that asked."
  [pids]
  (when (seq pids)
    (try (let [p
               (.start (doto (ProcessBuilder.
                               ^java.util.List
                               (vector "ps" "-o" "rss=,pcpu=,time=" "-p" (str/join "," pids)))
                         (.redirectErrorStream true)))

               out
               (with-open [r (io/reader (.getInputStream p))]
                 (slurp r))

               _
               (.waitFor p 2 TimeUnit/SECONDS)

               rows
               (keep (fn [line]
                       (let [cells (str/split (str/trim line) #"\s+")]
                         (when (= 3 (count cells))
                           (try [(Long/parseLong (first cells)) (Double/parseDouble (second cells))
                                 (cpu-time->ms (nth cells 2))]
                                (catch Exception _ nil)))))
                     (str/split-lines out))]

           (when (seq rows)
             {"rss_bytes" (* 1024 (long (reduce + (map first rows))))
              ;; One decimal: `ps` reports hundredths of a percent that mean nothing and
              ;; make two consecutive samples look different when they are not.
              "cpu_percent" (/ (Math/round (* 10.0 (double (reduce + (map second rows))))) 10.0)
              "cpu_ms" (reduce + (keep #(nth % 2) rows))}))
         ;; CANCELLATION IS NOT A MEASUREMENT FAILURE. `.waitFor` throws
         ;; InterruptedException and CLEARS the flag, so folding it into the
         ;; best-effort `catch Throwable` below silently ate the interrupt: a
         ;; cancelled turn whose wait happened to be sampling here kept polling
         ;; until its own deadline instead of unwinding. Restore the flag and
         ;; answer nil — the sample is worthless, the cancellation is not.
         (catch InterruptedException _ (.interrupt (Thread/currentThread)) nil)
         (catch Throwable _ nil))))

(defn- process-usage
  "`cpu_ms` / `cpu_percent` / `rss_bytes` for a LIVE process tree, sampled now. An
   exited shell answers nil for all three — usage is a measurement, and a result
   that kept reporting the last sample would be claiming a process that no longer
   exists is still costing something."
  [pid]
  (when-let [handles (seq (tree-handles pid))]
    (merge {"cpu_ms" (tree-cpu-ms handles)}
           (into {}
                 (remove (comp nil? val))
                 (tree-ps-usage (mapv (fn [^ProcessHandle h]
                                        (.pid h))
                                      handles))))))

(defn- quiet-mark
  "The point since which the tree has used less than 1 s of CPU, after a sample of `cpu`
   ms at `now`. Not the `%CPU` of `ps`: on Linux it is a lifetime average, so startup CPU
   hides a later quiet time, and Linux gives CPU time in whole seconds. The sum counts
   only live processes, so a smaller sum means a child changed, which is activity. The
   first mark is the start."
  [mark started-at cpu now]
  (let [{:keys [cpu-ms] :as mark} (or mark {:at started-at :cpu-ms 0})
        grown (when cpu (- (long cpu) (long cpu-ms)))]
    (when cpu
      (if (< -1 grown 1000) mark {:at now :cpu-ms cpu}))))

(defn- cached-process-usage [{:keys [usage proc started-at]} defer?]
  (if-not usage
    (when-not defer? (process-usage (:pid proc)))
    (let [{:keys [until sample refreshing?] :as cached} @usage]
      (if (or refreshing? (and until (< (System/nanoTime) (long until))))
        sample
        (if-not (compare-and-set! usage cached (assoc cached :refreshing? true))
          (:sample @usage)
          (let [refresh! (fn []
                           (let [measured (volatile! sample)
                                 mark (volatile! (:quiet-mark cached))]
                             (try
                               (vreset! measured (process-usage (:pid proc)))
                               (vreset! mark (quiet-mark @mark started-at
                                                         (get @measured "cpu_ms")
                                                         (util/now-ms)))
                               @measured
                               (finally
                                 (reset! usage {:until (+ (System/nanoTime) 2000000000)
                                                :sample @measured
                                                :quiet-mark @mark
                                                :quiet-ms (some->> @mark :at (- (util/now-ms)))})))))]
            (if defer?
              ;; A refused submission (executor shut down) must not leave the cache
              ;; marked as refreshing forever, and a field read must not throw.
              (do (try (future (refresh!))
                       (catch java.util.concurrent.RejectedExecutionException _
                         (swap! usage dissoc :refreshing?)))
                  sample)
              (refresh!))))))))

(def ^:private read-syscall
  "Number of the `read` syscall in `/proc/<pid>/syscall`, by `os.arch`. x86-64 has its
   own table; arm64 uses the generic table."
  {"amd64" 0 "x86_64" 0 "aarch64" 63 "arm64" 63})

(defn- terminal-read-fd
  "The fd that one `/proc/<pid>/syscall` line shows a blocking `read` on, or nil. The
   line is the syscall number, then its arguments in hex. `running` and `-1` mean that
   the process is not in a syscall."
  [line read-nr]
  (let [[nr fd] (str/split (str/trim (str line)) #"\s+")]
    (when (and fd (= (str read-nr) nr))
      (try (Long/decode ^String fd) (catch NumberFormatException _ nil)))))

(defn- terminal-reader
  "The pid in the tree of `pid` that blocks in `read` on a terminal, or nil. Only Linux
   shows the blocking syscall: macOS gives no wait channel, so there a program that waits
   for input and a program that sleeps look the same, and this answers nil."
  [pid]
  (when-let [nr (and (= "Linux" (System/getProperty "os.name"))
                     (read-syscall (System/getProperty "os.arch")))]
    (some (fn [^ProcessHandle h]
            (let [p (.pid h)]
              (try (when-let [fd (terminal-read-fd (slurp (str "/proc/" p "/syscall")) nr)]
                     (let [target (str (Files/readSymbolicLink
                                         (.toPath (io/file (str "/proc/" p "/fd/" fd)))))]
                       (when (or (str/starts-with? target "/dev/pts/") (= "/dev/tty" target)) p)))
                   ;; The process exited, or the kernel refused the read: no answer.
                   (catch Exception _ nil))))
          (tree-handles pid))))

(def ^:private quiet-note-ms
  "A process live at least this long with no output and almost no CPU adds a note: the process
   can wait for input, a lock or the network. Only a note, because the signs are the same
   for all three."
  30000)

(defn- bg-core
  "Identity keys shared by EVERY background stage of `shell`, merged onto the
   TOTAL base. `op` names the stage that produced the result, so consumers and
   model Python read ONE declared field instead of sniffing which keys happen to
   exist. `exit` is nil while running, `attach` nil when no attach bridge was
   opened, and `command`/`cwd`/`pid` nil only once the entry itself is gone.

   The shell's own command line rides the SAME `command` key a run answers with,
   so a lifecycle stage says what the shell it acts on is running and the result
   never echoes it under a second name.

   All reads share a short-lived cost sample for this process."
  [op id entry & [{:keys [sample-usage? defer-usage?] :or {sample-usage? true}}]]
  (let [exit
        (some-> (:exit entry)
                deref)

        bridge
        (:bridge entry)]

    (shell-result
      op
      (merge
        (when (and sample-usage? (nil? exit)) (cached-process-usage entry defer-usage?))
        {"id" id
         "command" (:command entry)
         "cwd" (:dir entry)
         "pid" (:pid (:proc entry))
         "status" (if (some? exit) "exited" "running")
         "exit" exit
         ;; A LIVE shell's log is never complete: `is_eof` says "there is
         ;; nothing more to read", and while the child runs there always may
         ;; be. A read stage overrides this with what its own chunk saw.
         "is_eof" (some? exit)
         ;; LIFETIME, not the age of this read: it stops at the ending an
         ;; earlier stage stamped, so a job that ran 3s never reports the ten
         ;; minutes that passed before someone came back to read its logs.
         "uptime_ms" (let [t0
                           (long (or (:started-at entry) (util/now-ms)))

                           t1
                           (long (or (some-> (:exited-at entry)
                                             deref)
                                     (util/now-ms)))]

                       (max 0 (- t1 t0)))
         ;; The clock, in epoch ms: when it started, and when it ended — nil
         ;; while it has not.
         "started_at" (:started-at entry)
         "finished_at" (some-> (:exited-at entry)
                               deref)
         ;; The log FILE, by absolute path: readable with `cat`/`grep` by a
         ;; human, with no handle and no session in hand.
         "log_path" (:log-path entry)
         "note" (some-> (:log-error entry)
                        deref)
         "attach" (when bridge (str "vis-agent extension shell attach " id))}))))

(defn- result-window
  [stage id entry]
  (let [core (bg-core stage id entry)
        file (io/file (:log-path entry))
        end (.length file)
        {:keys [text omitted]} (capture-log file)]
    (assoc core
           "out" (normalize-terminal-output text)
           "out_omitted_chars" (long (or omitted 0))
           "duration_ms" (max 0 (- (long (or @(:exited-at entry) (util/now-ms)))
                                      (long (:started-at entry))))
           "next_offset" end)))

(defn- live-process-snapshot [id entry]
  (let [result (bg-core "run" id entry {:defer-usage? true})
        file (io/file (:log-path entry))
        chunk (shell-log/read-chunk id file {:limit shell-log/max-chunk-bytes})
        out (normalize-terminal-output (:text chunk))
        live? (live-entry? entry)
        uptime (get result "uptime_ms")
        quiet-ms (some-> (:usage entry) deref :quiet-ms)]
    (cond-> (assoc result "out" out
                          "duration_ms" uptime
                          "offset" (:offset chunk)
                          "next_offset" (:next-offset chunk)
                          "waiting_for_input" (boolean (and live? (:stdin-tty? entry)
                                                            (terminal-reader (:pid (:proc entry))))))
      (= "running" (get result "status")) (dissoc "timed_out" "timeout_secs")
      (and live? quiet-ms (>= (long quiet-ms) (long quiet-note-ms)) (str/blank? out))
      (assoc "note" (str "No output and almost no CPU for " (quot (long quiet-ms) 1000)
                         " s. Check the log with sh.logs(-20).")))))

(defn- register-process!
  [env id {:keys [exit generation log-path command] :as entry}]
  (let [session (:session-id env)
        final (atom nil)
        entry (assoc entry :usage (atom nil) :final final)
        snapshot (bg-core "run" id entry {:sample-usage? false})
        {:keys [row complete!]}
        (task/start-manual!
          session
          {:kind :shell :name (str "shell(" (pr-str (one-line command 72)) ")")
           :origin python-host/*task-origin*
           :guest? (= "tool" (env-origin env))
           :shell-snapshot #(or @final (let [current (live-process-snapshot id entry)] (or @final current)))
           :guest-data {:generation generation :snapshot snapshot :shell-origin (env-origin env)}}
          (fn [_] (stop-bg-generation! session id exit)))]
    (assoc entry :task row :complete! complete! :log-path log-path)))

(defn- finish-process!
  [session id {:keys [exit exited-at proc log-error stopped? bridge complete! final] :as entry} index-fn code]
  (let [retire (delay
                 (compare-and-set! exited-at nil (util/now-ms))
                 (reset! exit code)
                 (let [error (or @log-error (when (nil? code) "Shell exit code is unavailable."))
                       snapshot (cond-> (result-window "run" id (dissoc entry :bridge))
                                  @stopped? (assoc "status" "stopped")
                                  (and error (not @stopped?)) (assoc "status" "failed" "note" error)
                                  true (assoc "is_eof" true))]
                   (reset! final snapshot)
                   #_{:clj-kondo/ignore [:locking-suspicious-lock]}
                   (locking (bg-lifecycle-lock session id)
                     (when (identical? exit (:exit (bg-entry session id)))
                       (index-fn {:ended-at @exited-at :exit code})
                       (when (and bridge (not @stopped?))
                         (try ((:stop bridge)) (catch Throwable _ nil)))
                       (retire-bg-entry! session id proc)
                       (when-not @stopped?
                         (resources/update! session id {:status :exited
                                                        :detail (or error (str "exit " code " — log on disk"))}))))
                   (cond @stopped? {:state :cancelled :snapshot snapshot}
                         error {:state :failed :error error :snapshot snapshot}
                         :else {:state :done :value snapshot :snapshot snapshot})))]
    (try (complete! #(deref retire)) (finally @retire))))

(defn- process-descriptor
  [entry snapshot]
  {"id" (get-in entry [:task :id])
   "kind" "shell"
   "name" (get-in entry [:task :name])
   "snapshot" snapshot
   "token" (:generation entry)})

(defn task-result
  "Return the shell process descriptor at a Python-facing start boundary."
  [{:keys [result] :as envelope}]
  (if-let [entry (::process (meta envelope))]
    (let [snapshot (cond-> result
                     (= "running" (get result "status")) (dissoc "timed_out" "timeout_secs"))]
      (with-meta (assoc envelope :result {"__vis_shell_task__" (process-descriptor entry snapshot)}) nil))
    envelope))

(defn- stdin-tty?
  "Whether a new shell reads its stdin from the terminal. By default its stdin is empty:
   a program that read an open terminal stdin waited for hours for bytes that nothing
   typed. `{\"stdin\": \"tty\"}` keeps the terminal, so `sh.type` reaches the program."
  [opts]
  (let [v (get opts "stdin")]
    (case (some-> v
                  str)
      (nil "null")
      false

      "tty"
      true

      (throw (ex-info (str "shell `stdin` is \"tty\" or \"null\", not " (pr-str v) ".")
                      {:type ::bad-stdin :stdin v})))))

(defn- shell-bg-spawn!
  "Start a process under its alias reservation. Publish the entry before the pump runs."
  [env id script opts]
  (let [session (:session-id env)
        stdin-tty? (stdin-tty? opts)]
    (when (str/blank? id)
      (throw (ex-info "The shell background op needs a non-blank resource id ({\"id\": …})."
                      {:type ::blank-id})))
    (when (str/blank? script)
      (throw (ex-info "The shell background op needs its `command` as the first argument."
                      {:type ::blank-command})))
    (let [policy (call-jail-policy env opts)
          dir (resolve-dir-for-policy opts env policy)
          generation (task/call-id)
          ;; Do not truncate a log still used by another generation's pump or readers.
          sink (if (bg-id-taken? session id (when (:proc (bg-entry session id)) (bg-entry session id)))
                 (shell-log/open! session id generation)
                 (shell-log/open! session id))
          p (try (pty-spawn! (if stdin-tty? script (str "exec 0</dev/null; " script)) dir policy)
                 (catch Throwable t (shell-log/close! sink) (throw t)))
          t0 (util/now-ms)
          bridge (try (pty-bridge/serve!
                        {:pty p
                         :path (pty-bridge/socket-path session id)
                         :replay-fn (fn []
                                      (.getBytes ^String
                                                 (:text (shell-log/read-chunk id (io/file (:path sink))))
                                                 java.nio.charset.StandardCharsets/UTF_8))})
                      (catch Throwable _ nil))
          entry (try (register-process!
                  env id {:proc p :exit (atom nil) :exited-at (atom nil) :stopped? (atom false)
                          :log-error (atom nil) :send (:send p) :bridge bridge
                          :stdin-tty? stdin-tty? :command script :script script :dir (.getPath dir)
                          :log-path (:path sink) :origin (env-origin env) :started-at t0
                          :generation generation})
                     (catch Throwable t
                       (when bridge ((:stop bridge)))
                       (when ((:alive? p)) (kill-tree! p))
                       (try (.close ^java.io.InputStream (:in p)) (catch Throwable _ nil))
                       (shell-log/close! sink)
                       (cleanup-jail-policy! {::cleanup (::cleanup p)})
                       (throw t)))
          index-fn (fn [m]
                     (shell-log/index! (:db-info env) session id
                                       (merge {:command script :script script :dir (.getPath dir)
                                               :shell-origin (:origin entry) :log-path (:path sink)
                                               :started-at t0 :ended-at nil :exit nil}
                                              m)))
          pump (start-pump! id p (:stopped? entry) sink (:log-error entry)
                            (partial finish-process! session id entry index-fn))]
      (index-fn nil)
      (swap! bg-procs assoc-in [(str session) id] (assoc entry :pump pump))
      (try
        (resources/register! session {:id id :kind :shell :label (one-line script 48) :detail script
                                      :pid (:pid p) :owner "foundation-core" :status :running}
                             (shell-resource-fns session id (get-in entry [:task :id])
                                                 (:exit entry) (io/file (:path sink))))
        (.start pump)
        (catch Throwable t
          (stop-bg-generation! session id (:exit entry))
          (shell-log/close! sink)
          (cleanup-jail-policy! {::cleanup (::cleanup p)})
          (finish-process! session id entry index-fn nil)
          (throw t)))
      (with-meta
        (extension/success
          {:result (bg-core "background" id entry)
           :op :shell
           :metadata {:command script :pid (:pid p) :started-at-ms t0 :finished-at-ms t0 :duration-ms 0}})
        {::process entry}))))

(defn- reattached-process
  [stage id entry]
  (with-meta
    (extension/success
      {:result (assoc (bg-core stage id entry)
                 "already_running" true
                 "note" "This shell is ALREADY running. Read it with sh.logs() or await it.")
       :op :shell
       :metadata {:command (:script entry) :pid (:pid (:proc entry))
                  :started-at-ms (:started-at entry) :finished-at-ms (util/now-ms) :duration-ms 0}})
    {::process entry}))

(defn- shell-attach
  "Return the live generation of an alias. Never start a new process."
  [env id]
  (let [entry (bg-entry (:session-id env) id)]
    (authorize-origin! env id entry nil)
    (when-not (and (live-entry? entry) (not (stopping-entry? (:session-id env) entry)))
      (throw (ex-info (str "No running background shell '" id "' in this session.")
                      {:type ::unknown-bg-id :id id})))
    (reattached-process "run" id entry)))

(defn- shell-bg-impl
  "`await shell(\"npm run dev\", {\"id\": id})` — IDEMPOTENT on a live id.

   Re-using an id whose process is still running used to THROW. That reads as a
   plain tool failure: a model that already started the shell (or that lost the
   result) learns nothing actionable, invents a new id, and spins — the exact
   runaway seen on `companion-dev`. Instead, return the RUNNING shell flagged
   `already_running` with its pid/uptime and the `logs` handle, so \"start it\" is
   answered by \"it IS started, here is how to watch it\". No second process is
   spawned; a genuinely fresh one needs a `stop` op first."
  [env id command opts]
  (let [session
        (:session-id env)

        id
        (str id)]

    ;; Starting a PTY and registering its lifecycle are side effects, so atomically
    ;; updating `bg-procs` alone cannot make the preceding live check safe. Keep the
    ;; entire check/spawn/register transition under the id's stable lifecycle lock:
    ;; concurrent starts must observe the first process, never each spawn a child
    ;; and overwrite its only stop handle. Waits/logs/sends and other ids stay free.
    #_{:clj-kondo/ignore [:locking-suspicious-lock]}
    (locking (bg-lifecycle-lock session id)
      (let [live (reissue-live-entry env session id command (get opts "cwd"))]
        (if live
          (reattached-process "background" id live)
          (if-let [command (some-> command
                                   str
                                   not-empty)]
            (shell-bg-spawn! env id command opts)
            (throw (ex-info (str "No background shell '"
                                 id
                                 "' is running, so it must be STARTED:"
                                 " pass {\"command\": \"…\", \"op\": \"background\", \"id\": \""
                                 id
                                 "\"} as one shell map.")
                            {:type ::missing-command :op "background" :id id}))))))))

;; RUN as a handle — a timeout is a WAIT that expired, not a lost process

(defn- process-handle
  "A spawned `java.lang.Process` in the PTY handle SHAPE (`:pid :in :alive? :wait
   :destroy`) that `bg-core`, `kill-tree!` and the background registry already
   speak. A foreground run that outstays its wait is then an ORDINARY shell handle
   rather than a second kind of live process with its own lifecycle."
  [^Process p]
  {:pid (.pid p)
   :in (.getInputStream p)
   :alive? (fn []
             (.isAlive p))
   :wait (fn []
           (.waitFor p))
   :destroy (fn [force?]
              (if force? (.destroyForcibly p) (.destroy p)))})

(defn- adopt-run!
  "Register a foreground process that outlived its bounded wait."
  [{:keys [env session id proc sink script cwd drains index-fn]}]
  #_{:clj-kondo/ignore [:locking-suspicious-lock]}
  (locking (bg-lifecycle-lock session id)
    (let [entry (register-process!
                  env id (assoc (bg-entry session id) :dir cwd :log-error (atom nil)
                                :generation (task/call-id)))
          watcher (Thread.
                    (fn []
                      (doseq [f drains]
                        (try (deref f)
                             (catch Throwable t
                               (cancellation/preserve-interrupt! t)
                               (when-not @(:stopped? entry)
                                 (reset! (:log-error entry) (str "Shell log write failed: " (ex-message t)))))))
                      (shell-log/close! sink)
                      (finish-process! session id entry #(index-fn (assoc % :dir cwd))
                                       (try ((:wait proc)) (catch Throwable _ nil)))))]
      (index-fn {:dir cwd})
      (swap! bg-procs assoc-in [(str session) id] (assoc entry :pump watcher))
      (resources/register! session {:id id :kind :shell :label (one-line script 48) :detail script
                                    :pid (:pid proc) :owner "foundation-core" :status :running}
                           (shell-resource-fns session id (get-in entry [:task :id])
                                               (:exit entry) (io/file (:path sink))))
      (doto watcher (.setName (str "vis-shell-run-" id)) (.setDaemon true) (.start))))
  nil)

(defn- start-shell! [env id command cwd opts]
  (let [session (:session-id env)]
    (if id
      (shell-bg-impl env id command opts)
      (loop []
        (let [id (auto-bg-id env session command cwd)
              result (releasing-claim
                       session id
                       (fn [id]
                         (try (shell-bg-impl env id command opts)
                              (catch clojure.lang.ExceptionInfo e
                                (if (= ::id-stopping (:type (ex-data e)))
                                  ::id-stopping
                                  (throw e))))))]
          (if (= ::id-stopping result) (recur) result))))))

(defn- run-of-background
  "A background start answered in the RUN shape. Waiting is the ONLY difference
   between a run and a background shell, so the two must not answer with two
   different maps: `id`, `command` and `status` are read the same way on every
   stage. `timed_out` is FALSE — no wait was made, so none expired — and `exit` is
   nil until the process exits."
  [r]
  (let [m (:result r)]
    (assoc r
      :result (assoc m
                ;; The background start already answered the TOTAL shape through
                ;; `bg-core` — `pid`, `status`, `uptime_ms`, `already_running`. Rebuilding
                ;; the map from scratch dropped exactly those, so a spawn could not say
                ;; what it had started or that it was running. Only the stage-specific
                ;; keys are overridden here.
                "stage" "run"
                "duration_ms" 0
                ;; Nothing WAITED, so nothing timed out: `timed_out` only ever
                ;; means "the wait expired", and a run has no wait.
                "timed_out" false
                ;; `bg-core` writes an explicit nil note, so this is `or`, never
                ;; `get`'s default: a present-but-nil key would swallow the spawn note.
                "note" (or (get m "note")
                           (str "Spawned: the shell runs under id '"
                                (get m "id")
                                "' — read it with sh.logs(offset=0), type at it with"
                                " sh.type(text), wait for it with await sh, and"
                                " stop it with sh.cancel().")))
      :op :shell)))

(defn run-blocking
  "INTERNAL blocking runner — NOT the tool. Run ONE bounded foreground command and answer the tool's own total result: the
   command's own `run`-stage map with the handle's identity merged on, so `r[\"exit\"]`,
   `r[\"out\"]` and `r[\"command\"]` are read at the top level and there is no
   entry to index into. One call is one command — an ordered batch is what `&&`
   and a second call are for — so there is no shared budget to divide and no
   \"never started\" entry to explain.

   EVERY run IS a handle. It claims its `id` and its log file BEFORE it waits, so
   the result carries that id whether the command finished or not, and a wait that
   expires is no longer a lost process: it keeps running under its id,
   `sh.logs(offset=0)` reads everything the timed-out call never saw, and
   `sh.cancel()` ends it. A command that finished inside its wait drops the
   registry entry — there is no live process to account for — while its log file
   stays readable by id for as long as the session does."
  [env command opts]
  (let [session
        (:session-id env)

        command
        (one-command command)

        ;; The handle is named after the program, exactly as a `wait=0` start is,
        ;; and never hijacks an id a live shell already holds.
        id
        (or (some-> (get opts "id")
                    str
                    not-empty)
            (auto-bg-id env session command (get opts "cwd")))

        ;; The log is a FILE, opened BEFORE the spawn so not one byte of a fast
        ;; command's output can be printed before there is somewhere to keep it.
        sink
        (shell-log/open! session id)

        t0
        (util/now-ms)

        ;; The wait is how long the CALLER waits, and the only knob there is.
        wait-secs
        (clamp-timeout-secs (get opts "timeout_secs"))

        exit-atom
        (atom nil)

        exited-at
        (atom nil)

        stopped?
        (atom false)

        db-info
        (:db-info env)

        index-fn
        (fn [m]
          (shell-log/index! db-info
                            session
                            id
                            (merge {:command command
                                    :script command
                                    :dir nil
                                    :log-path (:path sink)
                                    :started-at t0
                                    :ended-at nil
                                    :exit nil}
                                   m)))

        on-spawn
        (fn [p]
          (swap! bg-procs assoc-in [(str session) id :proc] (process-handle p)))

        _
        (swap! bg-procs assoc-in
          [(str session) id]
          {:proc nil
           :exit exit-atom
           :exited-at exited-at
           :stopped? stopped?
           :command command
           :script command
           :dir nil
           :origin (env-origin env)
           :log-path (:path sink)
           :started-at t0})

        r
        ;; A start that dies before it spawns must not keep the id. The entry above is
        ;; what holds the handle against a concurrent auto id, so leaving it behind
        ;; would push every later run of this program one suffix along for good.
        (try (shell-run-impl env
                             command
                             (assoc opts "timeout_secs" wait-secs)
                             {:sink sink :on-spawn on-spawn})
             (catch Throwable t (shell-log/close! sink) (drop-bg-entry! session id) (throw t)))

        cwd
        (:dir (meta r))

        timed-out
        (when (get r "timed_out") (meta r))]

    (if timed-out
      (adopt-run! {:env env
                   :session session
                   :id id
                   :proc (:proc (bg-entry session id))
                   :sink sink
                   :script command
                   :cwd cwd
                   :drains (:drains timed-out)
                   :index-fn index-fn})
      (do (shell-log/close! sink)
          (reset! exited-at (util/now-ms))
          (reset! exit-atom (get r "exit"))
          (index-fn {:dir cwd :ended-at @exited-at :exit @exit-atom})
          (drop-bg-entry! session id)))
    (extension/success
      {:result (shell-result
                 "run"
                 (merge r
                        {;; ALWAYS a handle, finished or not — that is the whole point:
                         ;; no rule to remember about when an id is present.
                         "id" id
                         "cwd" cwd
                         "timeout_secs" wait-secs
                         ;; The STATUS block every other stage answers with, stamped on the
                         ;; run itself: WHERE the bytes are and HOW LONG the command lived.
                         ;; A run that reported neither made "what is this shell doing" a
                         ;; second call for a question this result already knew.
                         "log_path" (:path sink)
                         "uptime_ms" (max 0 (- (long (or @exited-at (util/now-ms))) (long t0)))
                         "note" (if timed-out
                                  (str "The WAIT expired, not the process: it is still running as"
                                       " shell '" id
                                       "\" — read everything it printed with sh.logs(offset=0),"
                                       " and stop it with sh.cancel().")
                                  (get r "note"))}))
       :op :shell})))

(defn- retired-log-core
  "The `logs` identity for an id whose registry entry is GONE but whose log file is
   not — a run that finished inside its wait keeps no live process, yet its bytes
   stay readable by id for as long as the session does. The sidecar index row is
   where the command lines, the cwd, the lifetime and the exit come from, so \"what
   did that build print\" is answerable a turn later with nothing but the id."
  [env session id]
  (let [row
        (->> (shell-log/session-logs (:db-info env) session)
             (filter #(= id (get % "id")))
             first)

        started
        (get row "started_at")

        ended
        (get row "ended_at")]

    (assoc (shell-result
             "logs"
             {"id" id
              "command" (get row "command")
              "cwd" (get row "dir")
              "exit" (get row "exit")
              "started_at" started
              "finished_at" ended
              "log_path" (or (get row "log_path") (.getPath (shell-log/log-file session id)))
              "uptime_ms" (when (and started ended) (max 0 (- (long ended) (long started))))})
      ;; The process is gone by construction: this stage reads a FILE, never a child.
      "status" "exited")))

(defn- shell-logs-impl
  "ONE read of a shell's log, by byte OFFSET or by LINES from the end.

   The log is a file, so a read is a WINDOW on it and the caller owns the cursor:
   feed `next_offset` back and the loop walks the whole stream, however long it
   grew. There is no `n`, no eviction and no `dropped` — nothing a shell printed
   is ever gone while the session lives.

   With no `:offset` the window is the TAIL, which is what someone watching a live
   command wants; `{:offset 0}` is the head, and a loop from there is the whole
   log. A NEGATIVE `:offset` is the last n LINES — `{:offset -50}` reads back 50
   lines, the same reading `tail -n 50` has, and clamps to the head when the
    log holds fewer. `is_eof` false means read again NOW rather than sleep.

   `:lines` N is the LINE switch: at most N lines come back, anchored at the LAST
   N when no `:offset` is given, so `{:lines 10}` is the last ten and
   `{:offset next_offset :lines 10}` is the next ten — walking a long log a
   screenful at a time instead of guessing byte counts. A NEGATIVE N walks UP
   instead: `{:offset o :lines -10}` is the ten lines ENDING at `o`, so the same
   window scrolls back through what already scrolled past.

   The id may name a LIVE shell or a finished RUN: every run claims a handle before
   it waits, so a batch that ended inside its wait still has a log to read, and the
   file is what answers once the registry entry is gone."
  ([env id] (shell-logs-impl env id nil))
  ([env id {:keys [offset limit lines generation sample-usage?] :or {sample-usage? true}}]
   (let [session
         (:session-id env)

         id
         (str id)

         entry
         (bg-entry session id)

         retired
         (when-not entry (retired-log-core env session id))

         ^java.io.File file
         (io/file (or (:log-path entry)
                      (get retired "log_path")
                      (.getPath (shell-log/log-file session id))))]

     (authorize-origin! env id entry generation)
     (when-not (or entry (.isFile file))
       (throw (ex-info (str "shell: unknown id '" id "'; use the handle returned by shell().")
                       {:type ::unknown-bg-id :id id})))
     (let [chunk
           (shell-log/read-chunk id file {:offset offset :limit limit :lines lines})

           t
           (util/now-ms)]

       (extension/success
         ;; Sharing `bg-core`'s identity keys with every other stage: `exit` None
         ;; while running. `out` is the window this read returned, already joined
         ;; — the SAME key a foreground run puts its bytes under, so "what did it
         ;; print" is one field whether the call waited or came back for it later.
         {:result (assoc (if entry (bg-core "logs" id entry {:sample-usage? sample-usage?}) retired)
                    ;; Every shell is a PTY, so a tool writes for a SCREEN: CRLF line ends,
                    ;; progress redrawn with a bare CR, colour and keypad escapes it only
                    ;; sent because isatty() was true. The model reads TEXT, so this window
                    ;; is the terminal's own reading of those bytes — the raw stream stays
                    ;; whole on disk at `log_path` for anyone who wants it.
                    "out"
                    (terminal-log-text id file entry chunk)
                    "offset" (:offset chunk)
                    "next_offset" (:next-offset chunk)
                    "is_eof" (:is-eof chunk))
          :op :_shell-logs
          :metadata {:id id :started-at-ms t :finished-at-ms t :duration-ms 0}})))))

(def ^:private control-key-names
  "Human names for the control characters `send` writes into a PTY, so its factual
   status can say WHAT was typed even when the payload prints nothing at all."
  {\newline "↵" \return "↵" \tab "⇥" (char 27) "Esc" (char 127) "Del"})

(defn- key-token
  "Named key for ONE control character, or nil when the character is printable."
  [c]
  (let [n (int c)]
    (or (control-key-names c)
        (cond (zero? n) "C-@"
              (< n 27) (str "C-" (char (+ 96 n)))
              (< n 32) (str "C-" (char (+ 64 n)))
              (<= 127 n 159) (format "\\u%04x" n)
              :else nil))))

(defn- keys-label
  "Human keystroke label for a `send` payload: printable runs stay literal (quoted),
   every control character becomes its key name — `\"y\" ↵`, `C-c`, `Esc`. A send is
   frequently ENTIRELY non-printing, where a char count says nothing about what the
   shell actually received, so the result metadata carries this label."
  [s]
  (when (some? s)
    (->> (str s)
         (partition-by #(some? (key-token %)))
         (mapcat
           (fn [run]
             (if (key-token (first run)) (map key-token run) [(str "\"" (str/join run) "\"")])))
         (str/join " ")
         not-empty)))

(defn- shell-send-impl
  "Write `text` to a background shell's STDIN (its PTY master). With enter (default
   true) a trailing newline SUBMITS the line — exactly what an interactive prompt
   (password, `read`, a REPL, a y/N confirm) waits for. The send-keys equivalent:
   the agent drives an interactive program whose output the pump captured. Read the
   response with `sh.logs()`. Returns the total shell result."
  ([env id text] (shell-send-impl env id text nil))
  ([env id text opts]
   (let [session
         (:session-id env)

         id
         (str id)

         entry
         (bg-entry session id)

         enter?
         (let [e (first (remove nil? (map #(get opts %) ["is_enter" "enter"])))]
           (if (nil? e) true (boolean e)))]

     (authorize-origin! env id entry (get opts "token"))
     (when-not entry
       (throw (ex-info (str "No background shell '" id
                            "' in this session — start one with"
                            " await shell({\"command\": \"…\", \"wait\": 0, \"id\": id}).")
                       {:type ::unknown-bg-id :id id})))
     (when-not (live-entry? entry)
       (throw (ex-info (str "Background shell '" id
                            "' has exited — nothing to send"
                            " to. Read its output with sh.logs(offset=0).")
                       {:type ::bg-exited :id id})))
     (let [send-fn (:send entry)]
       (when (nil? send-fn)
         (throw (ex-info (str "Background shell '" id "' has no writable stdin.")
                         {:type ::no-stdin :id id})))
       (let [payload (str text (when enter? "\n"))
             t (util/now-ms)]

         (send-fn (.getBytes payload java.nio.charset.StandardCharsets/UTF_8))
         (extension/success
           {:result (cond-> (assoc (bg-core "send" id entry) "keys" (keys-label payload))
                      (not (:stdin-tty? entry))
                      (assoc "note"
                        (str "The stdin of this shell is empty, so only a program that "
                             "opens its terminal reads this. To type into stdin, start "
                             "the command with {\"stdin\": \"tty\"}.")))
            :op :_shell-type
            :metadata {:id id :started-at-ms t :finished-at-ms t :duration-ms 0}}))))))

;; Internal lifecycle grammar — the model calls `shell` /

(defn- opts-arg?
  "Is `x` the trailing OPTIONS map rather than a positional string? A Python dict
   crosses as a java.util.Map; a Clojure caller passes a map."
  [x]
  (or (map? x) (instance? java.util.Map x)))

(defn- opt
  "Read `k` — or the first of its near-miss ALIASES that is present — from a
   string-keyed options map.

   A shell option map refuses nothing: a key nobody reads is silently DROPPED, so
   a near-miss spelling costs the caller the whole option and says nothing. The
   aliases are therefore part of the read, not a courtesy."
  [opts k & aliases]
  (when (some? opts)
    (loop [ks (cons k aliases)]
      (when (seq ks)
        (let [v (get opts (name (first ks)))]
          (if (some? v) v (recur (rest ks))))))))

(defn- validate-shell-options!
  "Check public start or attach options before any process preparation."
  [opts jailed?]
  (when-not (opts-arg? opts)
    (throw (ex-info "shell options must be a map." {:type ::bad-options})))
  (let [op (get opts "op")
        attach? (contains? opts "op")
        allowed (cond attach? #{"op" "id"}
                      jailed? #{"command" "cwd" "id" "env" "stdin" "allow_read_write" "unix_connect"}
                      :else #{"command" "cwd" "id" "env" "stdin"})]
    (when (and attach? (not= "attach" op))
      (throw (ex-info (str "Unknown shell op " (pr-str op) "; use 'attach' or omit op.")
                      {:type ::unknown-op :op op})))
    (doseq [[k v] opts]
      (when-not (allowed k)
        (throw (ex-info (str "Unknown shell option " (pr-str k) ".")
                        {:type ::bad-option :option k})))
      (when (and (some? v)
                 (not (case k
                        ("cwd" "id" "command") (string? v)
                        "env" (opts-arg? v)
                        "stdin" (contains? #{"tty" "null"} v)
                        true)))
        (throw (ex-info (str "Invalid value for shell `" k "`.")
                        {:type ::bad-option :option k}))))
    (if attach?
      (when-not (util/non-blank-string? (get opts "id"))
        (throw (ex-info "shell op 'attach' requires a non-blank id." {:type ::missing-id})))
      (when-not (util/non-blank-string? (get opts "command"))
        (throw (ex-info "shell needs a non-blank command." {:type ::blank-command}))))
    (cond-> (into {} opts)
      (not attach?) (assoc ::validated-env (process-jail/validate-call-env (get opts "env"))))))

(defn- public-shell-dispatch [env opts]
  (if (= "attach" (get opts "op"))
    (shell-attach env (str/trim (get opts "id")))
    (let [opts (if-let [prepare (:shell-options-fn env)] (prepare opts) opts)]
      (run-of-background (start-shell! env (some-> (get opts "id") str/trim not-empty)
                                       (get opts "command") (get opts "cwd") opts)))))

(defn shell-dispatch
  "Dispatch internal shell lifecycle operations, including capability-bound logs and input."
  [env opts]
  (when-not (opts-arg? opts)
    (throw (ex-info "shell takes one options map, e.g. shell({\"command\": \"ls\"})."
                    {:type ::bad-options})))
  (let [raw-opts
        (into {}
              (remove (fn [[_ v]]
                        (or (nil? v) (and (string? v) (str/blank? v)) (and (coll? v) (empty? v)))))
              opts)

        text
        (opt raw-opts :text)

        _
        (when (some? (opt raw-opts :cmd))
          (throw (ex-info "shell has no `cmd` option — put the bash line in {\"command\": \"…\"}."
                          {:type ::legacy-command-carrier})))

        id
        (some-> (opt raw-opts :id)
                str
                str/trim
                not-empty)

        op
        (or (some-> (opt raw-opts :op)
                    str
                    str/trim
                    str/lower-case
                    not-empty)
            ;; `run` is the only default there is now: a caller that names an `id` is
            ;; naming its handle, not selecting a second lifecycle.
            "run")

        ;; Lifecycle stages operate on an existing background shell.  A caller that
        ;; mechanically reuses the start shape may still carry `command`; it is not
        ;; relevant to these stages and must not make an otherwise valid call fail.
        opts
        (if (#{"logs" "send"} op) (dissoc raw-opts "command") raw-opts)

        command
        (opt opts :command)

        valid-command
        (when (some? command) (one-command command))

        need-command
        (fn []
          (or valid-command
              (throw (ex-info "shell: command required; use shell(command)."
                              {:type ::missing-command :op op}))))

        need-id
        (fn []
          (or id
              (throw (ex-info "shell: id required; use the handle returned by shell()."
                              {:type ::missing-id :op op}))))

        reject-command
        (fn []
          (when (some? command)
            (throw (ex-info
                     (str "shell op \"" op "\" takes no command — it acts on {\"id\": \"…\"}.")
                     {:type ::unexpected-command :op op}))))

        reject-text
        (fn []
          (when (some? text)
            (throw (ex-info (str "shell op \"" op "\" takes no text payload.")
                            {:type ::unexpected-text :op op}))))

        need-text
        (fn []
          (cond (nil? text) ""
                (string? text) text
                :else (throw (ex-info "shell `text` must be a string." {:type ::bad-text}))))]

    (case op
      "run"
      ;; There is no `wait` on the REQUEST. A run ALWAYS spawns under a real PTY and
      ;; returns NOW with its handle, so one call has one meaning and a number can
      ;; never select a second mode. A caller can await the handle. The log file every run
      ;; leaves behind is the FEATURE: "what did that build print" stays answerable
      ;; by id for as long as the session lives.
      (do (reject-text)
          (run-of-background (start-shell! env id (need-command) (opt opts :cwd) opts)))

      "background"
      ;; A start is the one background stage with no prior handle: derive the id from
      ;; the command rather than fail a well-formed start over a missing name. Every
      ;; other stage (no command) still names the shell it acts on.
      (do (reject-text)
          (start-shell! env (or id (when-not valid-command (need-id))) valid-command (opt opts :cwd) opts))

      "attach"
      (do (reject-command) (reject-text) (shell-attach env (need-id)))

      "logs"
      (do (reject-command)
          (reject-text)
          (shell-logs-impl env
                           (need-id)
                           {:offset (->whole-long (opt opts :offset :start) "offset")
                            :limit (->pos-long (opt opts :limit :bytes) "limit")
                            :lines (->whole-long (opt opts :lines :n :tail) "lines")
                            :generation (get opts "token")}))

      "send"
      (do (reject-command) (shell-send-impl env (need-id) (need-text) opts))

      (throw (ex-info (str "Unknown shell op " (pr-str op)
                           " — use \"run\" (default), \"background\", \"attach\", \"logs\" or \"send\".")
                      {:type ::unknown-op :op op})))))

(defn shell-snapshot
  "Read the process or its final result without waiting."
  [env opts]
  (or (task/shell-snapshot (:session-id env) (get opts "task_id") (get opts "token")
                           (env-origin env) (str (get opts "id")))
      (throw (ex-info "This shell handle has no process generation." {:id (get opts "id")}))))

(defn sdk-task-op
  "Drive the exact process that the SDK installation admitted."
  [{:keys [session task-id id origin token]} op opts]
  (let [env {:session-id session :shell-origin origin}]
    (case op
      "await" (if-let [outcome (when (task/owns-generation? session task-id token)
                                 (task/await! session task-id
                                   {:timeout-ms (min 250 (long (get opts "timeout_ms" 0)))}))]
                (python-host/task-reply outcome)
                {"state" "missing"})
      "cancel" (boolean (and (task/owns-generation? session task-id token)
                             (task/cancel! session task-id :agent)))
      "snapshot" (shell-snapshot env {"id" id "token" token "task_id" task-id})
      ("logs" "send") (:result (shell-dispatch env (assoc opts "op" op "id" id "token" token)))
      (throw (ex-info "Unknown SDK shell Task operation." {:op op})))))

(defn run-argv
  "Run ONE literal argv through the SAME bounded machinery `shell` runs its own
   command with: cwd authorization, process-jail policy, disk-backed output,
   head/tail bounded reads, timeout and kill-tree. Returns its own total entry — the
   SAME [[shell-result-base]] map `shell` itself answers a foreground call with,
   carrying the request's `:dir`/`:timeout-secs` as metadata, so an argv run and `shell`
   have ONE result shape and there is no envelope to unwrap.

   No shell is involved — each element reaches the process verbatim, so nothing
   needs quoting. An argv caller is a USER of this: every argv command is a
   bounded shell command, so both tools share one runner, one jail and one
   capture policy."
  ([env argv] (run-argv env argv nil))
  ([env argv opts] (shell-run-impl env (vec argv) opts)))

(defn trusted-extension-shell
  "Run a trusted Python extension's ordinary `vis.shell` request without applying
   a process jail. The extension context already has direct, unrestricted
   subprocess access; this is the result-shaped convenience API for the same trust
   boundary. Foreground calls therefore work outside a session too."
  [env opts]
  (let [opts (validate-shell-options! opts false)]
    (public-shell-dispatch (-> (or env {})
                               (assoc :shell-origin (or (:shell-origin env) "trusted-extension"))
                               ;; The project environment also applies to an unconfined child.
                               (assoc :jail-policy-fn (constantly {:disabled? true
                                                                   :env-values
                                                                   (config/child-environment-values)}))
                               (assoc-in [:security-policy :jail-enabled] false))
                           opts)))

(defn- granted-paths
  "Exact paths a trusted extension hands its OWN jailed child, on top of the session
   roots. A language pack spawns its runtime through [[jailed-shell]] and then has to
   talk to it: a pty cannot carry a JSON request/response protocol, so the pack creates
   a rendezvous endpoint — a FIFO or a unix socket in its own run directory — and the
   child needs that ONE path, not a wider workspace. Anything that is not a nonblank
   path string is refused here, where the message can still name the option."
  [opts k what]
  (let [v (opt opts k)]
    (cond (nil? v) nil
          (string? v) (when-not (str/blank? v) [v])
          (or (sequential? v) (instance? java.util.Collection v))
          (into []
                (map (fn [x]
                       (if (util/non-blank-string? x)
                         x
                         (throw (ex-info (str what " takes path strings, got " (pr-str x) ".")
                                         {:type ::bad-option :option what :value x})))))
                v)
          :else (throw (ex-info
                         (str what " must be a path string or a list of them, got " (pr-str v) ".")
                         {:type ::bad-option :option what :value v})))))

(defn- with-granted-paths
  "Merge a caller's explicit grants into a freshly built jail policy. A DISABLED policy
   is returned untouched: there is no boundary to widen."
  [policy {:keys [allow-read allow-read-write unix-connect]}]
  (if (:disabled? policy)
    policy
    (cond-> policy
      (seq allow-read)
      (update :allow-read (fnil into []) allow-read)

      (seq allow-read-write)
      (update :allow-read-write (fnil into []) allow-read-write)

      (seq unix-connect)
      (update :unix-connect (fnil into []) unix-connect))))

(defn- latest-jail-policy
  "Strictly build one process policy from the currently merged config. This is
   called at the spawn boundary: invalid current config throws, and no session or
   last-good snapshot is consulted."
  [env]
  (let [base-dir
        (or (workspace/workspace-root env) (str (workspace/cwd)))

        security
        (security-policy/snapshot (or (config/load-config-raw) {}) {:base-dir base-dir})

        configured-roots
        (security-policy/read-write-roots security)

        active-workspace
        (or (:workspace env) {:root base-dir})

        latest-env
        (assoc (or env {})
          :security-policy security
          :workspace active-workspace
          :security/filesystem-roots configured-roots)

        jail-config
        (:process-jail security)]

    (if (or (false? (:jail-enabled security)) (:disabled? jail-config))
      {:disabled? true :env-values (config/child-environment-values) ::environment latest-env}
      (let [roots-fn
            (fn []
              (vec (distinct (concat (when-let [root (:root active-workspace)]
                                       [(str root)])
                                     configured-roots))))

            network
            (:network security)

            compiled-network-policy
            (some-> (egress/compile-policy network)
                    (assoc :mitm? (boolean (seq (:rules network)))))

            token
            (str (java.util.UUID/randomUUID))

            cleaned?
            (atom false)

            cleanup
            (fn []
              (when (compare-and-set! cleaned? false true)
                (gateway-sandbox/unregister-session! token)))]

        (gateway-sandbox/register-session! token (constantly compiled-network-policy))
        (try (let [proxy-port
                   (gateway-sandbox/ensure-proxy!)

                   ca-file
                   (gateway-sandbox/ensure-ca!)]

               (merge jail-config
                      {:roots-fn roots-fn
                       :net-enabled? true
                       :proxy-port proxy-port
                       :proxy-token token
                       :ca-file ca-file
                       ;; Resolved per spawn: a `.env` edit lands on the next child.
                       :env-values (config/child-environment-values)
                       ::environment latest-env
                       ::cleanup cleanup}
                      (gateway-sandbox/loopback-grant compiled-network-policy)))
             (catch Throwable t (cleanup) (throw t)))))))

(defn jailed-shell
  "Run `vis.jailed_shell` through a strict policy read from the latest merged
   on-disk configuration at every process spawn. Works with or without a session;
   invalid current config refuses that spawn instead of using a snapshot.

   The calling extension's declared `vis.Grant` paths come first: its toolchain
   (a JDK, a package cache, a launcher) by mode, from the registry and never from
   the call. `allow_read_write` and `unix_connect` are the CALLER's own per-call
   grants, added by [[granted-paths]]: the extension hands its child the exact
   rendezvous endpoint it created and nothing else. A disabled jail ignores all of
   them."
  [env opts]
  (let [opts (validate-shell-options! opts true)
        declared
        (extension/current-extension-grants)

        grants
        {:allow-read (into [] (comp (filter #(= :read (:mode %))) (map :path)) declared)
         :allow-read-write
         (into (into [] (comp (filter #(= :read-write (:mode %))) (map :path)) declared)
               (granted-paths opts :allow_read_write "allow_read_write"))
         :unix-connect (granted-paths opts :unix_connect "unix_connect")}]

    (public-shell-dispatch (-> (or env {})
                        (assoc :shell-origin (or (:shell-origin env) "jailed-extension"))
                        (assoc :jail-policy-fn #(with-granted-paths (latest-jail-policy env)
                                                                    grants)))
                    opts)))

;; Env injection — the before-fn hands the impl its env as first arg

(defn- op-label
  "Human call name from a registered op keyword: :_shell-logs -> \"_shell_logs\"."
  [op]
  (str/replace (if (namespace op) (str (namespace op) "_" (name op)) (name op)) "-" "_"))

(defn- shell-on-error
  "Failure envelope for thrown impl errors; mirrors editing's interrupted-vs-
   throwable split so turn cancellation renders as a clean interruption."
  [op]
  (fn [err _env _f _args]
    (let [interrupted?
          (instance? InterruptedException err)

          t
          (util/now-ms)]

      {:result (extension/failure
                 {:result nil
                  :op op
                  :metadata (cond-> {:started-at-ms t :finished-at-ms t :duration-ms 0}
                              interrupted?
                              (assoc :interrupted?
                                true :status
                                :interrupted))
                  :error (cond interrupted? {:message (str
                                                        (op-label op)
                                                        " interrupted while running;"
                                                        " the spawned process tree was killed.")})
                  :throwable (when-not interrupted? err)})})))

;; Public, doc-bearing vars retain developer examples and fallback docs. Native
;; symbols below provide compact model-facing semantics; their schemas provide
;; exact inputs. The injected `env` first arg is hidden from both.

(defn- shell-call-opts
  "ONE options map from a call's arguments. Each shell tool takes named positional
   values (`command`, `id`, `text`) followed by an optional keyword dict, and an
   all-kwargs call collapses into a single map — normalize all three shapes here so
   no tool has to."
  [ks args]
  (let [[pos more]
        (split-with (complement opts-arg?) args)

        named
        (into {}
              (remove (fn [[_ v]]
                        (nil? v)))
              (zipmap ks pos))]

    (reduce (fn [m o]
              (merge m (into {} o)))
            named
            more)))

(defn shell
  "Start or attach to a managed shell. See the `shell` symbol for the sandbox contract."
  {:arglists '([command] [command opts])}
  [env & args]
  (let [[command opts] (if (and (= 1 (count args)) (opts-arg? (first args)))
                         (if (::python-host/keyword-arguments (meta (first args)))
                           [nil (first args)]
                           [(first args) nil])
                         args)
        expected (if (some? command) 2 1)]
    (when (or (> (count args) expected) (and (some? opts) (not (opts-arg? opts))))
      (throw (ex-info "shell takes one command and keyword options." {:type ::bad-options})))
    (when (and (some? command) (contains? opts "command"))
      (throw (ex-info "shell command is named twice." {:type ::bad-options})))
    (let [opts (cond-> (into {} opts) (some? command) (assoc "command" command))]
      (task-result (public-shell-dispatch env (validate-shell-options! opts false))))))

(defn shell-logs
  "`page = sh.logs()` — read a background shell's log from a byte offset, or the
   last n LINES with a negative one (`sh.logs(-50)`), and return NOW. `lines=10`
   is the LINE window — the last ten, or the NEXT ten from an offset, and `-10`
   the ten ABOVE it — so a long log is walked a screenful at a time, both ways.
   Each page is still the shell result map, while a slice addresses its `out` text
   directly (`page[-4000:]`). It remembers the whole read: `next(page)` gets the next
   ready page and `page.pages(max_pages=…)` lazily includes this page plus those after
   it, bounded to ten pages by default. Paging stops at the current snapshot's EOF
   the process is live; call `sh.logs()` later to read output written later.
   `out` is normalized text: terminal colours and controls are handled automatically.
   `log_path` preserves raw terminal bytes, not the text view used for parsing."
  {:arglists '([id] [id opts])}
  [env & args]
  (shell-dispatch env (assoc (shell-call-opts ["id"] args) "op" "logs")))

(defn shell-type
  "`sh.type(\"y\")` — type keystrokes at a background shell's stdin.
   `is_enter` (default true) submits the line, which is what an interactive prompt
   waits for; read the response with `sh.logs()`."
  {:arglists '([id text] [id text opts])}
  [env & args]
  (shell-dispatch env (assoc (shell-call-opts ["id" "text"] args) "op" "send")))

;; Compact shell labels used by the live process ticker.

(def ^:private shell-chip-max "Display-width-ish budget for one command in a live shell label." 72)

(defn- present-str
  "Stringify `x`, trim only the right edge (so logs keep indentation), and return
   nil when blank."
  [x]
  (let [s (str/trimr (str x))]
    (when-not (str/blank? s) s)))

(defn- shell-one-line
  "Collapse whitespace to one trimmed line for a shell ticker preview."
  [s]
  (some-> (present-str s)
          (str/replace #"\s+" " ")
          str/trim
          not-empty))

(defn- clip-chip
  "Clip a single-line shell label with an ellipsis."
  [s n]
  (let [s
        (str s)

        n
        (long n)]

    (if (> (count s) n) (str (subs s 0 (max 0 (dec n))) "…") s)))

;; Symbols + prompt + extension. ONE builtin symbol — `shell` — bound bare in the
;; flat Python sandbox next to `ls` / `grep`.

(defn- live-bg-script
  "The bash the LIVE background shell `id` is already running. A `logs`/`send`/
   `stop` call runs no command of its OWN, but the command it acts on is right
   there in the registry — the same `command` fact a finished result carries — so
   the live ticker can name that real command.

   nil when no live shell answers to that id, or when two sessions both do and the
   answer would be a guess."
  [id]
  (when id
    (let [hits (into [] (keep #(get % id)) (vals @bg-procs))]
      (when (= 1 (count hits)) (present-str (:script (first hits)))))))

(defn- shell-ticker
  "Show the command of a shell call while it is in progress."
  [op]
  (fn [_env args]
    (let [input
          (shell-call-opts (case op
                             "run"
                             ["command"]

                             "send"
                             ["id" "text"]

                             ["id"])
                           args)

          id
          (some-> (opt input :id)
                  str
                  str/trim
                  not-empty)

          ;; The spawn KNOWS its command — it is right there in the call — while a
          ;; lifecycle stage has to look up the shell it acts on.
          script
          (or (some-> (try (command-line (opt input :command)) (catch Throwable _ nil))
                      shell-one-line
                      not-empty)
              (some-> (live-bg-script id)
                      shell-one-line))

          ;; Every verb ends on the preposition, so the nameless case is the same
          ;; sentence with the generic noun: `stopping the shell`.
          verb
          (case op
            "run"
            "running"

            "logs"
            "reading the log of"

            "send"
            "typing into"

            "stop"
            "stopping"

            op)]

      (if (seq script) (str verb ": " (clip-chip script shell-chip-max)) (str verb " the shell")))))

(def shell-symbol
  (extension/symbol
    #'shell
    {:activity (presenter/for-tool :shell)
     :symbol 'shell
     ;; A process starts from PYTHON, which can retain the returned handle. Every
     ;; later operation (`wait`, `logs`, `type`, `cancel`) is a method on that handle.
     ;; `apropos('shell')` / `doc('shell')` are how the capability is found.
     :name "shell"
     :result
     (str
       "The first await gives a `ShellTask`. Await it for a `ShellResult` map: "
       "`{stage, id, cwd, command, status, exit, out, duration_ms, timed_out, offset, next_offset, "
       "is_eof, started_at, finished_at, log_path, cpu_ms, cpu_percent, rss_bytes, note, …}`. "
       "Read a field by key or attribute. Serialize the result map, not the Task. A fresh run has "
       "`exit=None`. A nonzero exit is data.\n\n"
       "`out` is bounded text that combines stdout and stderr in terminal order. There is no `stderr` key. "
       "An empty string means this result returned no output, not that the log is empty. Vis normalizes "
       "terminal colors and controls in `out`. `log_path` names the raw terminal log.")
     :description
     (str
       "Use `sh = await shell(command, cwd=dir)` to start one `bash -lc` command under a terminal. "
       "The call starts a Task that gives the running process handle. `result = await sh` waits for the exit. "
       "Use `sh.snapshot()` for current state. The Task fields give the start or final result. "
       "Use `done, pending = await asyncio.wait([sh], timeout=seconds)` to wait up to a limit "
       "without cancelling the process. `sh.cancel()` requests cancellation. "
       "Use `await shell(op=\"attach\", id=alias)` to recover a live handle from the same trust origin "
       "without starting a command. Starting with the same live alias also returns that shell.\n\n"
       "`sh.logs()` reads the tail, even after cancellation. `sh.logs(-50)` gives the last 50 lines. "
       "`sh.logs(0, lines=10)` reads ten lines from the start. `next(page)` continues from its cursor, "
       "and `lines=-10` reads ten lines above an offset. `page.pages(max_pages=…)` includes the current "
       "page, defaults to ten pages, and stops at the current end of the log. `page[-4000:]` slices "
       "the text. `is_eof` marks the end of this snapshot. Read the log again for new output. Do not rerun "
       "the command.\n\n"
       "Standard input gives end of input by default. Use `stdin=\"tty\"` to let the program read "
       "`sh.type(text)` or input from a person. `sh.type(text)` returns `sh`. "
       "`sh.snapshot().waiting_for_input` is true when a program waits for input from its terminal (Linux only). "
       "After 30 seconds, a process with little CPU use and no output can have a `note`.\n\n"
       "Keep command output. Read or filter the log in Python instead of using `| head`, `| tail`, "
       "`| grep`, `2>/dev/null`, or `> file` to trim it. A pipeline reports the exit of its last command.\n\n"
       "`env` sets variables for this run over the project variables. Use a literal for a switch. "
       "For a secret, use a source map with a `keychain`, `env`, `dotenv`, or `command` key. "
       "A `command` source is an argv list, not a shell string. A literal stays in the transcript. "
       "Use `None` to unset a variable.\n\n"
       "```python\nsh = await shell(\"npm test\", env={\"NODE_ENV\": \"test\"})\nresult = await sh\n```")
     :params [{:name "id" :note "the live shell alias"} {:name "cwd"}
              {:name "op" :note "attach to a live alias without starting a command"}
              {:name "env" :note "THIS run's variables, over the project's"}
              {:name "stdin" :note "\"tty\" lets the program read sh.type; empty by default"}]
     :ticker-fn (shell-ticker "run")
     :inject-env? true
     :tag :mutation
     :presenter :shell
     :on-error-fn (shell-on-error :shell)}))

(def shell-logs-symbol
  (extension/symbol
    #'shell-logs
    {:activity (presenter/for-tool :_shell-logs)
     :symbol '_shell-logs
     :name "_shell_logs"
     :result
     (str
       "The same shell result shape as every other stage (`stage` is \"logs\"). `out` is the window this "
       "read returned, and a slice addresses that text directly (`page[-4000:]`). The "
       "sandbox result keeps the whole request, so `next(page)` follows its cursor and "
       "`page.pages()` walks ready pages lazily. Ordinary dict iteration still yields keys. `is_eof` "
       "marks the end of the current snapshot.")
     :description
     "Transport for `sh.logs(offset=…, lines=…)`. Use that ShellTask method to read the current log without waiting for more output. See `doc(\"shell\")` for paging."
     :params [{:name "id" :required? true :note "the shell handle's own id"}
              {:name "offset" :note "byte cursor, and negative counts LINES"}
              {:name "limit" :note "byte cap for one read"}
              {:name "lines" :note "N lines, and negative scrolls up"}]
     :inject-env? true
     :tag :observation
     :presenter :shell
     :ticker-fn (shell-ticker "logs")
     :on-error-fn (shell-on-error :_shell-logs)}))

(def shell-type-symbol
  (extension/symbol
    #'shell-type
    {:activity (presenter/for-tool :_shell-type)
     :symbol '_shell-type
     :name "_shell_type"
     :result
     (str
       "The same shell result shape (`stage` \"send\"), with the `sent` character count and the `keys` "
       "label. `out` is an empty string. Use `sh.logs()` to read the process output.")
     :description
     (str
       "Transport for `sh.type(text, is_enter=True)`. Call the handle instead. It writes keystrokes "
       "to the stdin of a background shell.")
     :params [{:name "id" :required? true} {:name "text" :required? true} {:name "is_enter"}]
     :inject-env? true
     :tag :mutation
     :presenter :shell
     :ticker-fn (shell-ticker "send")
     :on-error-fn (shell-on-error :_shell-type)}))

(defn- shell-snapshot-tool
  "Read the current fields of a shell Task."
  [opts]
  (shell-snapshot extension/*current-environment* opts))

(def ^:private shell-snapshot-symbol
  (extension/symbol
    #'shell-snapshot-tool
    {:symbol '__vis-shell-snapshot__ :name "__vis_shell_snapshot__" :raw? true :hidden? true}))

(defn shell-enabled? [_env] (toggles/enabled? "shell"))

(def shell-symbols
  ;; Private reads are bound only when the model can start a shell.
  (mapv #(assoc % :ext.symbol/active-fn shell-enabled?)
        [(assoc shell-symbol :ext.symbol/python-task? true)
         shell-logs-symbol shell-type-symbol shell-snapshot-symbol]))

(defn prompt
  "Point to the shell contract while the shell layer is on."
  [env]
  (when (shell-enabled? env)
    (str "## Shell\n"
         "Use `shell` to run programs. Use Python for file and data work. "
         "`sh = await shell(cmd)` gives the running handle, not the result. "
         "Use `sh.snapshot()` for current state. The Task fields give the start or final result. "
         "Use `await asyncio.wait([sh], timeout=2)` to wait up to a limit without cancelling. "
         "`await sh` waits for the exit. See `doc(\"shell\")`.")))

(defn shell-attach-command
  "`vis-agent extension shell attach <id>` — the human-side passthrough: join a live
   background shell's PTY in your OWN terminal (finish a browser OAuth, answer a
   prompt only a person can), then Ctrl-] to detach with the child untouched.
   `--socket PATH` targets an explicit socket; otherwise the newest shell whose
   id matches. Returns the attach exit code."
  [_parsed residual]
  (let [args
        (vec residual)

        socket
        (loop [xs args]
          (cond (empty? xs) nil
                (= (first xs) "--socket") (second xs)
                :else (recur (rest xs))))

        id
        (first (remove #(str/starts-with? % "--") args))]

    (pty-bridge/attach! {:id id :socket socket})))

(def shell-cli
  "CLI surface mounted under `vis-agent extension shell`. Only `attach` for now — the human
   passthrough onto a background PTY the agent spawned."
  [{:cmd/name "shell"
    :cmd/doc "Attach a real terminal to a live background shell (shell op \"background\")."
    :cmd/usage "vis-agent extension shell attach <id>"
    :cmd/subcommands
    [{:cmd/name "attach"
      :cmd/doc
      "Join a background shell's PTY in your terminal; Ctrl-] detaches (child keeps running)."
      :cmd/usage "vis-agent extension shell attach <id> [--socket PATH]"
      :cmd/owns-tty? true
      :cmd/args [{:name "id" :kind :positional :type :string :doc "Background shell id."}
                 {:name "socket" :kind :flag :type :string :doc "Attach to this socket path."}]
      :cmd/examples ["vis-agent extension shell attach slack-auth"
                     "vis-agent extension shell attach dev-server"]
      :cmd/run-fn #'shell-attach-command}]}])

(toggles/register-toggle! {:id "shell"
                           :label "Shell commands"
                           ;; The full pty-handle, jail, and extension-boundary contract lives in this
                           ;; namespace's docstring; the settings row stays one line.
                           :description
                           "Expose the Python `shell` verb. Model commands run inside the OS jail."
                           :default true
                           :scopes ["global" "project" "group" "session"]
                           :owner :vis
                           :persist? true
                           :group :sandbox})
