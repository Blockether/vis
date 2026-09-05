(ns com.blockether.vis.internal.capability
  "What THIS machine can actually run, asked once and answered as data.

   A pack that REGISTERED exists in this process. Whether the machine behind it
   works is a different question, asked at a different time: landing a pack costs
   an atom write and happens in manifest order for every session, while a
   capability costs whatever its probe costs and is paid by the FIRST caller that
   needs it — so a session that never speaks never loads sherpa's 8-13 MB of
   natives, and a session that never speaks never waits for them either.

   No heuristic decides this. `maxMemory`, an OS name and an arch say nothing
   about a glibc version, a missing `libstdc++`, or a CPU without the instructions
   a wheel was built for: they refuse machines that would have worked and admit
   machines that die inside the linker. The probe asks the real question by doing
   the real thing, once.

   A verdict is remembered only when asking again cannot answer differently:

   - `:ready` — it worked, and nothing in this process will unwork it.
   - `:unavailable` `:kind :terminal` — the JVM already froze this answer. A class
     whose static initializer failed can NEVER load again in the same process, so
     probing again can only re-download a library and reprint a linker error
     nobody can act on. That state is the reported \"voice only works after
     restarting Vis\", which is why the verdict, not the caller, is what says so.
   - `:unavailable` `:kind :transient` — no network, a missing file, a full disk.
     NOT remembered: the next ask retries.

   The state atom starts EMPTY and stays empty at load. `graal-build-time`
   initializes every Clojure namespace inside the BUILDER, so a value computed by
   a top-level form is baked into the image heap of every installed binary: a
   probed verdict would ship the BUILD machine's answer to every user. That cost
   is already paid for once — `sherpa/default-native-dir` is a function for this
   reason, and `TTYDeviceControl` is the single `--initialize-at-run-time` in
   `build.clj` after a build-time `SUPPORTED=true` SIGSEGV'd v0.1.33-35."
  (:require [taoensso.telemere :as tel]))

(defonce ^:private verdicts
  ;; id -> verdict. An atom, never `(defonce _ (delay (probe)))`: a delay that threw
  ;; answers that same throw for the life of the JVM, which would make one lost
  ;; network permanent and give no surface a way to try again.
  (atom {}))

(defn terminal-error?
  "True when `t` is the JVM refusing to LINK, at any depth: a native library is
   missing, or a class that already met a missing library is permanently unusable.
   The walk STOPS at the end of the cause chain — a shallow failure must not become
   a NullPointerException out of the very code that explains it."
  [t]
  (boolean (some (fn [^Throwable x]
                   (or (instance? UnsatisfiedLinkError x)
                       (instance? NoClassDefFoundError x)
                       (instance? ExceptionInInitializerError x)))
                 (take 8
                       (take-while some?
                                   (iterate (fn [^Throwable x]
                                              (.getCause x))
                                            t))))))

(defn verdict
  "The verdict this process already reached for `id`, or nil when it has never been
   asked. NEVER probes: this is what a status line or a doctor check reads, and a
   diagnostic that provisions 13 MB of natives to print one line is a bug."
  [id]
  (get @verdicts id))

(defn- unavailable
  [id ^Throwable t]
  {:capability id
   :status :unavailable
   :kind (if (terminal-error? t) :terminal :transient)
   :error (or (ex-message t) (str t))
   :cause t})

(defn- remember!
  "Keep the verdicts that cannot change — `:ready` and `:terminal` — and only those."
  [id {:keys [status kind] :as v}]
  (when (or (= :ready status) (= :terminal kind)) (swap! verdicts assoc id v))
  v)

(defn fail!
  "Record a TERMINAL failure met somewhere other than the probe, and answer the
   verdict now in force.

   A native runtime rarely breaks where it is provisioned: the library loads from
   the static initializer of the first class that touches it, so the linker speaks
   from inside the call, long after `ensure!` answered `:ready`. Without this, every
   later call re-provisions and re-fails; with it, the next `ensure!` says restart.
   A transient failure is NOT recorded — only the JVM's own frozen answers are."
  [id ^Throwable t]
  (let [v (unavailable id t)]
    (when (= :terminal (:kind v))
      (tel/log! {:level :warn
                 :id ::capability-lost
                 :data {:capability id :error (:error v)}
                 :msg (str "Capability " id " can no longer be linked in this process")}))
    (remember! id v)))

(defn ensure!
  "This process's verdict for capability `id`, probing at most once per answer that
   cannot change:

     {:capability id :status :ready       :detail <whatever the probe returned>}
     {:capability id :status :unavailable :kind :terminal|:transient
      :error <message> :cause <throwable>}

   `probe` is a thunk that DOES the real thing and throws when the machine cannot:
   link the library, open the device, run the binary. It is called at most once
   while a `:ready` or `:terminal` verdict stands, and again after a `:transient`
   one. Probes are serialized against each other, because two callers racing to
   download the same 13 MB is the failure this replaces."
  [id probe]
  (or (verdict id)
      (locking verdicts
        (or (verdict id)
            (remember! id
                       (try {:capability id :status :ready :detail (probe)}
                            (catch Throwable t (unavailable id t))))))))

(defn forget-verdicts!
  "Forget every verdict. The seam a TEST drives — production never calls it, because
   a remembered verdict is exactly the answer this process can no longer change."
  []
  (reset! verdicts {}))
