(ns com.blockether.vis.ext.language-clojure.nrepl-ctx
  "Per-turn `:ext/ctx-fn` contribution for the Clojure pack.

   Instead of forcing the model to call `repl_start()` over and over, the engine
   injects live nREPL state into context as standing knowledge, nested UNDER the
   active language so a polyglot repo accumulates
   `:languages {:clojure {...} :typescript {...}}`:

     {\"session_env\" {\"languages\" {\"clojure\"
        {\"nrepl\" {\"default\" <id|nil>
                  \"repls\"   [{\"id\" \"dir\" \"port\" \"aliases\" \"tool\"
                              \"status\" \"managed\" [\"dialect\" \"versions\"]
                              ;; when the (+ 1 1) health check failed:
                              [\"form\" \"hint\"]} ...]}}}}}

   OWNERSHIP: we surface ONLY the REPLs THIS session started + owns, PLUS any
   external nREPL the user EXPLICITLY attached via `connect` (both from
   `repl-manager/session-repls`). There is still NO external-port discovery and
   no `.nrepl-port` scanning — attachment is explicit consent, never a scan.

   The `default` is the id of the SINGLE owned REPL (nil when zero or many): with
   one REPL that id is the implicit eval target; with several, eval still resolves
   WITHOUT the model naming an id — it defaults to the workspace-root REPL (else the
   first) and the eval result reports which REPL ran under its `repl` field, so the
   model can pass an explicit `id` to override. Each REPL is mirrored into the
   session resource registry (footer badge
   + F4 stop/restart) and carries a liveness `status` from a per-turn probe.

   All best-effort: any failure degrades to an empty contribution and never
   blocks the render."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.language-clojure.nrepl-client :as nrepl-client]
            [com.blockether.vis.ext.language-clojure.repl-manager :as repl-manager]
            [taoensso.telemere :as tel]))

(def ^:private probe-timeout-ms 100)

(def ^:private health-timeout-ms
  "Budget for the per-turn `(+ 1 1)` eval health check (nrepl-client/health-check!).
   Longer than the `describe` probe because it drives a real eval round-trip; a
   healthy REPL answers in a few ms, a wedged one costs the full budget once."
  2000)

;; Per-turn liveness cache. `{:key [turn sorted-ports] :statuses {port status-map}}`.
;; defonce so it survives `(require :reload)` during REPL-driven dev.
(defonce ^:private liveness-cache (atom {:key nil :statuses {}}))

(defn- current-turn
  "Read the live turn off the engine ctx-atom for per-turn cache keying. Falls
   back to 0 when no ctx-atom is on env (e.g. tests / bare calls)."
  [env]
  (or (some-> (:ctx-atom env)
              deref
              :session/turn)
      0))

(defn- probe-one
  "Full per-turn liveness for ONE port: the cheap `describe` `probe!` (versions/
   dialect + is it even up?) then, when up, a REAL `(+ 1 1)` eval `health-check!`
   so a WEDGED eval executor is caught even though `describe` still answers. The
   eval result WINS — when the health check is not `:up`, its `:status`/`:form`/
   `:hint` override, so ctx shows `unresponsive` with the failing form + a clear
   kill/restart hint."
  [host port]
  (let [p (nrepl-client/probe! {:host host :port port :timeout-ms probe-timeout-ms})]
    (if (= :up (:status p))
      (let [h (nrepl-client/health-check! {:host host :port port :timeout-ms health-timeout-ms})]
        (if (= :up (:status h))
          (assoc p :ms (:ms h))
          (merge p (select-keys h [:status :form :hint :ms]))))
      p)))

(defn- probe-all
  "Probe every REPL in parallel — each at ITS host (an external attachment may
   not be on localhost) — each under a hard deadline so one slow host can never
   stall the render. Each probe is `describe` + a `(+ 1 1)` eval health check.
   Returns `{port {:status .. [:versions :dialect :form :hint :ms]}}`."
  [repls]
  (let [budget
        (+ (long probe-timeout-ms) (long health-timeout-ms) 200)

        futs
        (mapv (fn [{:keys [host port]}]
                [port (future (probe-one (or host "localhost") port))])
              repls)]

    (into {}
          (map (fn [[p f]]
                 [p (deref f budget {:status :down})]))
          futs)))

(defn- liveness-for
  "Statuses for `repls`' ports, reusing the per-turn cache when the
   `[turn port-set]` key is unchanged; otherwise probe and store."
  [turn repls]
  (let [k [turn (vec (sort (map :port repls)))]]
    (if (= k (:key @liveness-cache))
      (:statuses @liveness-cache)
      (let [statuses (probe-all repls)]
        (reset! liveness-cache {:key k :statuses statuses})
        statuses))))

(defn- ensure-resource!
  "Idempotently mirror one owned nREPL into `session-id`'s resource registry so
   the footer badge + stop/restart dialog see it. No-op when already registered.
   Managed REPLs get stop + restart thunks driving repl-manager."
  [session-id statuses {:keys [id dir port aliases log external? host]}]
  (let [existing
        (when (and session-id id) (vis/get-resource session-id id))

        status
        (or (:status (get statuses port)) :unknown)]

    (when (and session-id
               id
               (or (nil? existing)
                   (not= (name status) (get existing "status"))
                   (and log (not (get existing "can_logs")))
                   (not (get existing "can_health"))))
      (vis/register-resource!
        session-id
        {:id id
         :kind :nrepl
         :language :clojure
         :label (str "nREPL "
                     (.getName (io/file dir))
                     (when external? " (external)")
                     (when (seq aliases) (apply str (map #(str " :" (name %)) aliases))))
         :status status
         ;; STRING-keyed `:detail` — resources.clj/->data passes it through verbatim,
         ;; so it must be boundary-safe already.
         :detail (cond-> {"dir" dir "port" port}
                   external?
                   (assoc "host"
                     (or host "localhost") "external"
                     true)

                   (seq aliases)
                   (assoc "aliases" (mapv name aliases))

                   log
                   (assoc "log" log))
         :owner :ext/language-clojure}
        (cond-> {:stop-fn (fn []
                            (repl-manager/stop! session-id dir))
                 :restart-fn
                 (if external?
                   ;; External: re-CONNECT — never spawn a managed
                   ;; JVM over the user's REPL (stop! only detaches).
                   (fn []
                     (repl-manager/stop! session-id dir)
                     (let [r (repl-manager/connect! session-id dir {:host host :port port})]
                       (vis/unregister-resource! session-id id)
                       r))
                   (fn []
                     (repl-manager/stop! session-id dir)
                     (let [r (repl-manager/start! session-id dir {:aliases aliases})]
                       (vis/unregister-resource! session-id id)
                       r)))
                 ;; Keep a FAILED REPL visible (alive while a failure is on
                 ;; record) so the crash + its log tail stay inspectable in F4
                 ;; instead of being pruned the moment the pid dies.
                 :alive-fn (fn []
                             (boolean (or (repl-manager/repl-by-id session-id id)
                                          (repl-manager/last-failure session-id dir))))
                 ;; "alive, but is it WORKING?" — probed on every list/render,
                 ;; flips the stored `status` to :up/:starting/:failed/:down.
                 :health-fn (fn []
                              (repl-manager/health session-id dir))}
          log
          (assoc :logs-fn
            (fn []
              (repl-manager/tail-log log))))))))

(defn- nrepl-block
  "Build the `:nrepl` map from the session's owned REPLs + liveness statuses. This
   block crosses the strings-only Clojure->Python boundary, so it is built with
   STRING keys + STRING enum values. `default` is the SINGLE owned REPL's id."
  [repls statuses]
  {"default" (when (= 1 (count repls)) (:id (first repls)))
   "repls" (mapv (fn [{:keys [id dir port tool aliases external? host]}]
                   (let [st (get statuses port {:status :unknown})]
                     (cond-> {"id" id
                              "dir" dir
                              "port" port
                              "status" (name (:status st))
                              "managed" (not external?)}
                       external?
                       (assoc "external"
                         true "host"
                         (or host "localhost"))

                       tool
                       (assoc "tool" (name tool))

                       (seq aliases)
                       (assoc "aliases" (mapv name aliases))

                       (seq (:versions st))
                       (assoc "versions" (update-keys (:versions st) name))

                       (:dialect st)
                       (assoc "dialect" (name (:dialect st)))

                       ;; When the `(+ 1 1)` health check failed, carry the exact form
                       ;; that was run + a plain-language kill/restart hint so the model
                       ;; sees WHY a REPL is `unresponsive` and what to do about it.
                       (:form st)
                       (assoc "form" (:form st))

                       (:hint st)
                       (assoc "hint" (:hint st)))))
                 repls)})

(defn contribute
  "`:ext/ctx-fn` fn. Returns the `{\"session_env\" {\"languages\" {\"clojure\"
   {\"nrepl\" ...}}}}` slice (STRING-keyed) for THIS session's owned REPLs, and
   mirrors each into the session resource registry (footer + F4 dialog). `{}` when
   no workspace root is on env. Never throws — degrades to an empty contribution."
  [env]
  (try (if (:workspace/root env)
         (let [sid
               (:session-id env)

               repls
               (repl-manager/session-repls sid)

               statuses
               (when (seq repls) (liveness-for (current-turn env) repls))]

           (doseq [r repls]
             (try (ensure-resource! sid statuses r)
                  (catch Throwable e
                    (tel/log! {:level :warn
                               :id ::sync-resource-failed
                               :data {:id (:id r) :error (ex-message e)}}
                              "Failed to mirror nREPL into the session resource registry"))))
           {"session_env" {"languages" {"clojure" {"nrepl" (nrepl-block repls statuses)}}}})
         {})
       (catch Throwable e
         (tel/log! {:level :warn :id ::contribute-failed :data {:error (ex-message e)}}
                   "Clojure nREPL ctx contribution failed; degrading to empty")
         {})))
