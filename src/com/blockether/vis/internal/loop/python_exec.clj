(ns com.blockether.vis.internal.loop.python-exec
  "Execution of one model-written block in the session's persistent Python
   interpreter.

   Rejects blocks that are not code, runs a block with timing and Activity
   reporting, interrupts and unwinds a runaway block, and retires a worker that
   does not unwind. `policy-reload-epoch` counts `/reload`s; environments built
   under an older epoch are replaced."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.activity.block :as activity-block]
            [com.blockether.vis.internal.attachment.core :as attachments]
            [com.blockether.vis.internal.attachment.storage :as attachment-storage]
            [com.blockether.vis.internal.config.runtime-settings :as rt]
            [com.blockether.vis.internal.context.loop :as ctx-loop]
            [com.blockether.vis.internal.context.renderer :as ctx-renderer]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.python.env :as env]
            [com.blockether.vis.internal.sandbox.jail :as process-jail]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis.internal.view.core :as view]
            [com.blockether.vis.internal.view.materializer :as live]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [taoensso.telemere :as tel]))

(def ^:private BARE_STRING_RE #"^\s*\"[^\"]*\"\s*$")

(def ^:private MARKDOWN_FENCE_RE #"^\s*`{3,}[A-Za-z0-9_-]*\s*$")

(defn- bare-string-code-block? [expr] (boolean (re-matches BARE_STRING_RE (str expr))))

(defn- markdown-fence-line? [line] (boolean (re-matches MARKDOWN_FENCE_RE (str line))))

(defn- markdown-fence-block?
  [expr]
  (let [lines (->> (str/split-lines (str expr))
                   (map str/trim)
                   (remove str/blank?))]
    (boolean (and (seq lines) (every? markdown-fence-line? lines)))))

(defn- comment-only-block?
  [python-context ^String expr]
  (try (zero? (long (env/count-top-level-forms python-context (str/trim expr))))
       (catch Throwable _ false)))

(defn literal-code-block-error
  [python-context expr]
  (cond
    (bare-string-code-block? expr)
    "Your python_execution code is just a bare string literal. To ANSWER, reply with plain text and DON'T call python_execution — never pass a quoted string as the program."
    (markdown-fence-block? expr)
    "A Markdown fence (` ```… `) leaked into your python_execution code. Pass ONLY executable Python statements — no fence markers."
    (comment-only-block? python-context expr)
    "Your python_execution code is only `#` comments with no executable statement. Add a statement to run, or reply with plain text instead of calling python_execution."))

;; The engine is full-Python: a block's source is the program verbatim and
;; passes through to eval untouched — no parsing, unwrapping, or reformatting.

(defn- python-op-error
  "Map a throwable from the Python eval path to the op-error shape. A turn cancel
   is a normal, terse interruption — never the JVM stack that happened to be
   waiting when Cancel landed. What the INTERPRETER raised carries its own
   `Type: message` text and goes through env/map-python-error (proper
   :python/syntax|runtime|host phase + location); anything else falls back to
   extension/ex->op-error."
  [python-context e code cancel-token]
  (if (cancellation/cancelled? cancel-token)
    {:message "Python execution was interrupted" :type :vis/interrupted}
    (try (if (= "com.blockether.vispython.VisPythonException" (.getName (class e)))
           (env/map-python-error python-context (ex-message e) code)
           (extension/ex->op-error e {:form-source code}))
         (catch Throwable _ {:message (or (ex-message e) (.getName (class e)))}))))

;; ONE persistent interpreter per session. The Python sandbox is created ONCE
;; (`create-environment`) and reused across every turn, so the model's globals
;; (defs, imports, variables) carry across calls and turns NATURALLY, REPL-style.
;; (Resuming a session in a FRESH process starts with an empty sandbox; durable
;; file edits and conversation history persist, so the model recomputes what it
;; needs.)

(def ^:private INTERRUPT_UNWIND_MS
  "How long an acknowledged interrupt gets to unwind before its session worker
   is killed. The same window lets a timeout recover partial stdout."
  2000)

(defn- interrupt-guest!
  "Cancel whatever is EXECUTING in `python-context` right now, at a bytecode
   boundary. Protocol failures propagate: unlike a normal false reply, they mean
   the session worker itself cannot be trusted or reused."
  [python-context]
  (boolean (when python-context (env/interrupt-guest! python-context))))

(defn retire-python-context-once!
  "Kill one abandoned environment's Python process at most once. Process teardown
   does not enter the interpreter, so it is safe even while the turn thread still
   owns the environment lock."
  [python-context environment reason error]
  (let [retired
        (:python-context-retired-atom environment)

        first-retirement?
        (if retired (compare-and-set! retired false true) true)]

    (when (and python-context first-retirement?)
      (try (env/retire-python-context! python-context) (catch Throwable _ nil))
      (tel/log! {:level :warn
                 :id ::python-worker-retired
                 :error error
                 :data {:python-context python-context :reason reason}}
                "Python execution did not stop; retired its session process"))
    (boolean first-retirement?)))

(defn- retire-python-worker!
  [python-context exec-future environment reason error]
  (retire-python-context-once! python-context environment reason error)
  (try (.cancel ^java.util.concurrent.Future exec-future true) (catch Throwable _ nil))
  false)

(defn- watch-python-unwind!
  "Reclaim a worker unless both the block and its actual guest calls unwind.
   A cancelled host future is not proof that its trusted extension left C."
  [python-context exec-future environment pending-replies]
  (cancellation/worker-future
    "vis-python-interrupt-unwind"
    (fn []
      (let [deadline (+ (System/nanoTime) (* (long INTERRUPT_UNWIND_MS) 1000000))]
        (try (when-not (every? (fn [completion]
                                 (try (not= ::pending
                                            (deref
                                              completion
                                              (max 0 (quot (- deadline (System/nanoTime)) 1000000))
                                              ::pending))
                                      (catch InterruptedException error (throw error))
                                      (catch Throwable _ true)))
                               (cons exec-future pending-replies))
               (retire-python-worker! python-context
                                      exec-future
                                      environment
                                      :interrupt-unwind-timeout
                                      nil))
             (catch InterruptedException _ (.interrupt (Thread/currentThread))))))))

(defn- interrupt-block!
  "Interrupt the guest without ever leaving cancellation parked on its control
   plane. A normal false is the race where the block already finished. A worker
   that cannot answer, or cannot unwind an accepted interrupt, is killed and its
   environment retired so later work gets one fresh process."
  [python-context exec-future environment & {:keys [await-unwind?]}]
  (let [pending-replies
        (env/pending-guest-replies python-context)

        landed?
        (try (interrupt-guest! python-context)
             (catch Throwable t
               (retire-python-worker! python-context
                                      exec-future
                                      environment
                                      :interrupt-control-timeout
                                      t)))]

    (when (or landed? (seq pending-replies))
      (let [unwind (watch-python-unwind! python-context exec-future environment pending-replies)]
        ;; A timeout must settle retirement before another block or model request.
        ;; User cancellation keeps its non-blocking control path.
        (when await-unwind? @unwind)))
    (when-not landed?
      (try (.cancel ^java.util.concurrent.Future exec-future true) (catch Throwable _ nil)))
    (boolean landed?)))

(defn- unwound-stdout
  "What an INTERRUPTED block printed before it was killed, or nil.

   A block that unwinds in Python returns its normal captured stdout. Code parked
   inside C cannot return before its worker is retired, so the runtime also mirrors
   each write to the host and this drains that fallback after the wait."
  [python-context exec-future]
  (or (try (let [result
                 (.get ^java.util.concurrent.Future exec-future
                       (long INTERRUPT_UNWIND_MS)
                       java.util.concurrent.TimeUnit/MILLISECONDS)

                 out
                 (:stdout result)]

             (when (and (string? out) (seq out)) out))
           (catch Throwable _ nil))
      (env/take-partial-block-stdout! python-context)))

(defn- timeout-error
  "The error of a block that reached its time limit. The model reads that the stop
   is the normal limit for one block, not a fault, and which Python state the next
   block starts from: `:kept`, `:restarted` or `:retired`."
  [timeout-ms python-state]
  (let [ms
        (long timeout-ms)

        limit
        (if (and (pos? ms) (zero? (rem ms 1000))) (str (quot ms 1000) "s") (str ms " ms"))

        no-response
        "The block was waiting in native code and did not respond to the stop, so "]

    (cond-> {:message
             (str "Time limit reached: Vis stopped this block after " limit
                  ". This is the normal time limit for one python_execution block, not a fault. "
                  (case python-state
                    :kept
                    "Python state is kept."

                    :restarted
                    (str
                      no-response
                      "Vis restarted Python. Imports, functions, classes and small literal values "
                      "saved from earlier blocks are restored; other objects, such as open files, "
                      "connections and large data, are gone. The block was not run again: check "
                      "what it already did before you continue.")

                    :retired
                    (str no-response "Vis shut down its Python process.")))}
      (= :retired python-state)
      (assoc :type ::env/context-retired))))

(defn attachment-descriptor
  "One `session_attachment` row as the compact DESCRIPTOR `list_attachments()` and
   `get_attachment(id)` hand the model: identity, provenance and shape, and never
   any bytes.

   PROVENANCE STARTS AT THE TURN. Every row carries `session_turn_soul_id`, so
   every descriptor carries `:turn-id` — a user image and a tool artifact are
   placed the same way and a rail can be grouped by turn without a second
   lookup. `:iteration-id` / `:tool-call-id` are the FINER grain only a tool
   artifact has, so a user image omits both instead of carrying nils that say
   nothing.

   `:is-pending` is false here by construction: a stored row is stored. The
   sandbox reader answers the same key `true` for an artifact the RUNNING block
   just attached, which is not in the database yet."
  [a]
  (cond-> {:id (:id a)
           :source (:source a)
           :filename (:filename a)
           ;; VERSION: same filename in this session = one artifact
           ;; iterated. The rail is a set of version CHAINS, not loose files.
           :version (:version a)
           :media-type (:media-type a)
           :kind (:kind a)
           :commentable (true? (:commentable a))
           :size (:size a)
           :position (:position a)
           :turn-id (:turn-soul-id a)
           :is-pending false
           :audience (attachments/attachment-audience a)}
    (= :tool (:source a))
    (assoc :iteration-id
      (:iteration-id a) :tool-call-id
      (:tool-call-id a))))

(defn- envelope-with-settled-views
  "Fold the semantic live views a Python block ABANDONED onto its envelope.

   These are user-authored `vis.live` views, whose close verdict is a documented
   model result. Their records ride as `:attachments`, while the picture they ended
   on is appended to stdout because the interrupted block never returned that result
   itself."
  [envelope swept]
  (if-not swept
    envelope
    (let [document (str/join "\n\n"
                             (mapv (fn [verdict]
                                     (live/->markdown (:view verdict) {:result verdict}))
                                   (:verdicts swept)))]
      (cond-> envelope
        (seq (:attachments swept))
        (update :attachments (fnil into []) (:attachments swept))

        (not (str/blank? document))
        (update :stdout
                (fn [printed]
                  (if (str/blank? (str printed)) document (str printed "\n\n" document))))))))

(defn- activity-store
  "Durable Activity history for a block that belongs to a saved conversation."
  [env]
  (let [db
        (:db-info env)

        sid
        (:session-id env)]

    (when (and db sid)
      (let [history-id (str (random-uuid))]
        {:history-id history-id
         :apply! #(persistance/db-activity-apply! db sid history-id %)
         :settle! #(persistance/db-activity-settle! db history-id %1 %2)
         :page #(persistance/db-activity-page db sid history-id {})}))))

(defn- run-python-code
  "Run an agent code block through the embedded Python sandbox. Wraps the
   worker-future + cancellation + tool-event/render sinks + `*1`/`*e` recovery
   stack around `env/run-python-block` (whole-block; tools fire in order through
   their ProxyExecutable wrappers, which read the SAME dynamic sinks)."
  [python-context code & {:keys [tool-event-fn env]}]
  (let [thrown
        (atom nil)

        activity-block
        (activity-block/start! {:store (activity-store env)
                                :on-snapshot (:activity/on-snapshot env)
                                :on-event tool-event-fn})

        cancel-token
        (:cancel-token env)

        attachment-reader
        (let [d
              (:db-info env)

              sid
              (:session-id env)

              model-attachments
              (fn []
                (persistance/db-list-session-attachments-meta d sid))]

          (when (and d sid)
            {:list (fn []
                     (try (mapv attachment-descriptor (model-attachments)) (catch Throwable _ [])))
             :read (fn [id]
                     ;; Never turn a UUID into cross-session read authority: prove it
                     ;; belongs to this active session and is not presentation-only
                     ;; before the indexed row lookup.
                     (when (some #(= (str id) (str (:id %))) (model-attachments))
                       (attachment-storage/hydrate (persistance/db-read-attachment d id))))
             :reinspect (fn [id]
                          (when-let [a (when (some #(= (str id) (str (:id %))) (model-attachments))
                                         (attachment-storage/hydrate
                                           (persistance/db-read-attachment d id)))]
                            (when (and (str/starts-with? (str (:media-type a)) "image/")
                                       ;; An externally stored image whose backend is unavailable
                                       ;; has metadata but no bytes. Do not acknowledge it then emit
                                       ;; an invalid `data:image/...;base64,` block next request.
                                       (not (str/blank? (str (:base64 a)))))
                              (mpl-capture/queue-reinspection! a)
                              a)))}))

        reinspection-sink
        (atom [])

        timeout-ms
        (long (rt/eval-timeout-ms-for-code rt/*eval-timeout-ms* code))

        ;; MOVABLE wall: an input View pause inside the block parks this clock
        ;; instead of dying at it, and a live view LIFTS it entirely for as long
        ;; as the human is watching (see rt/parkable-wall).
        {eval-deadline :deadline eval-park :park eval-hold :hold}
        (rt/parkable-wall (util/now-ms) timeout-ms)

        ;; The views already open when this block started. Anything the block
        ;; opens on top of them is the block's own, and dies with it.
        views-before
        (view/open-live-ids)

        ;; A view the block opened and never closed. Sweeping it is the run's last
        ;; act, so a wall or a cancel cannot leave a pane painting a picture that
        ;; will never move again. Never at the cost of the block's own answer.
        ;; The close lands HERE, on the loop thread, because the guest is unwinding and
        ;; can no longer reach the host — so the record a settled view files would go
        ;; to the collector this block has already drained: a row listed nowhere, and
        ;; the picture the human watched lost at the moment they stopped watching. The
        ;; sweep carries a collector of its own and hands back both halves — the rows
        ;; to store, and the verdict every view it closed ended on.
        sweep-abandoned!
        (fn [ending]
          (let [sink (atom [])]
            (try (when-let [verdicts (seq (binding [mpl-capture/*attachment-sink* sink]
                                            (view/close-abandoned! views-before ending)))]
                   {:verdicts (vec verdicts) :attachments (mpl-capture/drain sink)})
                 (catch Throwable t
                   (tel/log! {:level :warn
                              :id ::abandoned-live-sweep-failed
                              :error t
                              :msg "Could not close the live views this block abandoned"})
                   nil))))

        exec-future
        (cancellation/worker-future
          "vis-python-eval"
          (fn []
            (try
              ;; THE session context, installed on the thread the guest actually runs
              ;; on. A sandbox SHIM bridge (`ls`, `attach`, …) reads the AMBIENT
              ;; context — only an extension SYMBOL installs its own around every
              ;; call — and this worker future starts bare, so without this the
              ;; block's shims ran session-less: `workspace/*filesystem-roots*` empty
              ;; (`ls` refusing a bound extra filesystem root that `cat`/`grep` on the
              ;; same path accept) and a nil environment reaching the `:fs/access`
              ;; gate that is supposed to hide a tree from the listing too.
              (extension/with-context
                {:env env}
                (binding [rt/*blocking-wall-park*
                          eval-park

                          rt/*blocking-wall-hold*
                          eval-hold

                          extension/*tool-event-sink*
                          (partial activity-block/record! activity-block)

                          extension/*tool-event-context*
                          (:context activity-block)

                          extension/*activity-history-id*
                          (:history-id activity-block)

                          mpl-capture/*attachment-reader*
                          attachment-reader

                          mpl-capture/*attachment-reinspection-sink*
                          reinspection-sink]

                  ;; One persistent interpreter per session: globals (defs,
                  ;; imports, vars) carry across calls/turns NATURALLY.
                  (let [result
                        (extension/invoke-operation
                          :python_execution
                          env
                          (fn [{:keys [code]}]
                            (let [out (env/run-python-block python-context
                                                            code
                                                            {:form-cap (:form-cap env)})]
                              (if (:error out)
                                (extension/failure {:result out :error (:error out)})
                                (extension/success {:result out}))))
                          [{:code code}])

                        out
                        (or (:result result) {:forms [] :error (:error result)})]

                    (assoc out
                      :lru {}
                      :reinspect-attachments (mpl-capture/drain-reinspections reinspection-sink)))))
              (catch Throwable e
                (reset! thrown e)
                {:lru {} :forms [] :error (python-op-error python-context e code cancel-token)}))))

        dispose-cancel-hook
        (when cancel-token
          (cancellation/on-cancel! cancel-token
                                   (fn []
                                     ;; The documented cancel first: it unwinds guest
                                     ;; frames and every host wait that polls
                                     ;; `rt/guest-safepoint!`. A context that refuses
                                     ;; that interrupt is retired before the Java
                                     ;; interrupt can strand its GIL.
                                     (interrupt-block! python-context exec-future env))))

        timeout-sentinel
        (Object.)

        raw-execution-result
        (try (rt/await-wall exec-future eval-deadline timeout-sentinel)
             (catch Throwable e
               (reset! thrown e)
               (interrupt-block! python-context exec-future env)
               {:lru {} :error (python-op-error python-context e code cancel-token)})
             (finally (when dispose-cancel-hook
                        (try (dispose-cancel-hook) (catch Throwable _ nil)))))

        execution-result
        (if (and (cancellation/cancelled? cancel-token) (:error raw-execution-result))
          (assoc raw-execution-result
            :error {:message "Python execution was interrupted" :type :vis/interrupted})
          raw-execution-result)]

    (if (identical? timeout-sentinel execution-result)
      ;; Eval timeout: interrupt the guest at a bytecode boundary. A guest the
      ;; exception cannot reach retires its environment before the Java interrupt.
      (let [retired-before?
            (some? (env/retired-context-error env))

            landed?
            (interrupt-block! python-context exec-future env :await-unwind? true)

            retired?
            (some? (env/retired-context-error env))

            ;; The unwinding guest cannot reach the host any more, so its `with` never
            ;; closes: the wall that killed the block ends its views too, and the model
            ;; still reads the picture they held.
            ;;
            ;; What the block PRINTED before the wall is real work — progress lines of
            ;; a fetch loop, results already computed. An interrupted block unwinds
            ;; through its own outcome and hands that capture buffer back, so the
            ;; envelope is never a bare `Timeout` and nothing else: that is
            ;; unactionable, and the model re-runs the whole block blind.
            out
            (when (or landed? retired?) (unwound-stdout python-context exec-future))

            ;; A block parked in native code did not respond to the stop, so its
            ;; worker was killed. The turn goes on in a fresh interpreter that the
            ;; next block builds. Only this block's own retirement renews, never a
            ;; cancelled turn's, and only after the old worker's stdout was drained
            ;; above, because renewal disposes it.
            restarted?
            (and retired?
                 (not retired-before?)
                 (not (cancellation/cancelled? cancel-token))
                 (env/renew-python-sandbox! env))

            envelope
            (cond-> {:lru {}
                     :error (timeout-error timeout-ms
                                           (cond restarted? :restarted
                                                 retired? :retired
                                                 :else :kept))
                     :timeout? true}
              out
              (assoc :stdout out))

            activity-envelope
            (activity-block/settle! activity-block envelope)

            swept
            (sweep-abandoned! {:reason :timeout
                               :error (str "the run watching this view was stopped at its "
                                           (/ timeout-ms 1000)
                                           "s wall")})]

        (envelope-with-settled-views activity-envelope swept))
      (let [;; A cancel unwinds the same way a wall does. An ordinary Python
            ;; exception does NOT — `with vis.live` closes on its way out — so this
            ;; finds nothing to sweep and says nothing.
            ;;
            ;; Cancellation may interrupt the host wait before the guest has returned
            ;; its outcome. Wait for that unwind exactly as the timeout path does, or
            ;; output printed before Cancel disappears from the transcript.
            interrupted?
            (= :vis/interrupted (get-in execution-result [:error :type]))

            out
            (when (and interrupted? (str/blank? (str (:stdout execution-result))))
              (unwound-stdout python-context exec-future))

            recovered-result
            (cond-> execution-result
              out
              (assoc :stdout out))

            activity-envelope
            (activity-block/settle! activity-block recovered-result)

            swept
            (when (:error recovered-result)
              (sweep-abandoned! {:reason :failed
                                 :error (or (:message (:error recovered-result))
                                            "the run that opened this view ended")}))]

        (envelope-with-settled-views activity-envelope swept)))))

(defn- run-with-timing
  [python-context code _sandbox-ns timeout-ms start-time tool-event-fn env]
  (let [run!
        (fn []
          (run-python-code python-context code :tool-event-fn tool-event-fn :env env))

        execution-result
        (if timeout-ms
          (binding [rt/*eval-timeout-ms* (rt/clamp-eval-timeout-ms timeout-ms)]
            (run!))
          (run!))

        finished-time
        (util/now-ms)

        execution-time
        (- (long finished-time) (long start-time))]

    (cond-> execution-result
      true
      (assoc :execution-started-at-ms
        start-time :execution-finished-at-ms
        finished-time :duration-ms
        execution-time)

      (:timeout? execution-result)
      (assoc :timeout? true)

      (not (:timeout? execution-result))
      (assoc :timeout? false))))

(defonce
  ^{:doc
    "Monotonic `/reload` epoch. Every `/reload` bumps it (via a reload hook).
   Stale idle sandboxes close immediately; busy ones close after their turn.
   The next turn rebuilds the immutable security-policy snapshot from the
   freshly-reloaded vis.yml. This is the sanctioned way `/reload` replaces the
   frozen network-domain / filesystem-root policy: the snapshot drives the
   Python session, egress proxy, and process jail at env-creation time, so it
   can only change by rebuilding the env — never by an in-place reseat."}
  policy-reload-epoch
  (atom 0))

(defn execute-code
  "Run a single :code block through the Python sandbox.

   Optional kwargs:
     :timeout-ms - hard-cap eval time, clamped at the
                   rt/*eval-timeout-ms* bounds.

   Every call performs a real Python eval. There is no result cache:
   forms with side effects MUST run their bodies on every
   invocation, and forms without side effects re-run cheaply enough
   that caching them is not worth the correctness footgun."
  [environment code & {:keys [timeout-ms tool-event-fn]}]
  (let [opts (get-in environment [:sandbox-caps :network])]
    (when (process-jail/draft-policy-expanded? environment)
      (swap! policy-reload-epoch inc)
      (throw
        (ex-info
          "A newly discovered repository needs a stricter worker policy. Start the next turn to rebuild the Python context before selecting or editing it; existing handles cannot be safely migrated."
          {:type :draft/policy-expanded})))
    (when (and (contains? opts :draft-required?)
               (not= (:draft-required? opts) (not= :off (workspace/draft-backend-setting))))
      (throw
        (ex-info
          "Draft policy changed. Start the next turn to rebuild the Python context safely; existing variables are not silently migrated."
          {:type :draft/policy-changed}))))
  ;; Running a block is exactly what a sandbox is FOR, so this is the ask that
  ;; builds one on a session whose first turn executes code.
  (let [python-context
        (env/python-context environment)

        sandbox-ns
        (env/sandbox-ns environment)]

    (binding [rt/*rlm-context* (merge rt/*rlm-context* {:rlm-phase :execute-code})]
      ;; Per-block-eval contract: feed original block source to `run-python-code`;
      ;; it parses, repairs delimiter slips when safe, then evaluates parsed
      ;; forms. Guard validators run against the repaired source when one exists
      ;; so a stray close paren does not block repair before eval.
      ;; Re-bind the live Python `context` snapshot BEFORE every eval. Sandbox
      ;; bindings are installed once at session start, so a static value would go
      ;; stale by iter 2; refreshing here keeps `context` aligned with the visible
      ;; `<context>` block and reflects intra-iter changes across blocks.
      ;; The snapshot is immutable/read-only — see ctx-loop/session-snapshot for
      ;; the guarantee. Re-binding also erases any model-created shadow binding.
      (let [opts (get-in environment [:sandbox-caps :network])]
        (when-let [policy-fn (:filesystem-policy-fn opts)]
          (env/refresh-confinement! python-context
                                    #(get (policy-fn) :read-write)
                                    (:jail-enabled? opts)
                                    policy-fn)))
      (when-let [snap (ctx-loop/session-snapshot environment)]
        ;; the agent gets real dict ergonomics (.get / comprehensions / [k]).
        (env/bind-ctx! python-context (ctx-renderer/project-ctx snap)))
      (let [start-time (util/now-ms)
            folds-before (long (or (get (some-> (:ctx-atom environment)
                                                deref)
                                        "engine_fold_count")
                                   0))
            exec (try
                   ;; The Python sandbox surfaces its own syntax/empty-block
                   ;; errors via env/run-python-block.
                   (run-with-timing python-context
                                    code
                                    sandbox-ns
                                    timeout-ms
                                    start-time
                                    tool-event-fn
                                    environment)
                   (catch Throwable e
                     {:lru {}
                      :error (try (extension/ex->op-error e {:form-source code})
                                  (catch Throwable _
                                    {:message (or (ex-message e) (.getName (class e)))
                                     :type (-> e
                                               ex-data
                                               :type)}))
                      :execution-started-at-ms start-time
                      :execution-finished-at-ms (util/now-ms)
                      :duration-ms (- (util/now-ms) start-time)
                      :timeout? false}))]

        ;; Helper definitions outlive the PROCESS. The sandbox dies with the
        ;; gateway, so this session's own `def`s are snapshotted after every block
        ;; and re-created by `restore-session-defs!` in the next process's fresh
        ;; sandbox. Best effort, after the outcome is in hand — never in its way.
        (env/persist-session-defs! python-context (:session-id environment))
        ;; Count successful operations even if their receipts were not printed or
        ;; the rest of this block failed. Summary supersession cannot erase usage.
        (let [folds (- (long (or (get (some-> (:ctx-atom environment)
                                              deref)
                                      "engine_fold_count")
                                 0))
                       folds-before)]
          (cond-> exec
            (pos? folds)
            (assoc :vis/fold-count folds)))))))

;; get-locals (read sandbox vars)

(defn get-locals
  "User-defined sandbox vars surface. Live-vars introspection is cosmetic-off
   for the Python engine (the agent uses its own Python scope + stdlib), so this
   returns an empty map. Kept as a stable seam for the trailer/renderer callers."
  [_environment]
  {})
