(ns com.blockether.vis.ext.language-clojure.nrepl-ctx
  "Per-turn nREPL resource synchronization for the Clojure pack.

   Live state has one model-facing home:
   `session[\"resources\"][\"repls\"][\"clojure\"][dir]`. This extension hook
   probes owned nREPLs and mirrors them into the generic session registry before
   ctx-loop builds that nested resource view. It returns no legacy
   `session[\"env\"][\"languages\"]` contribution.

   OWNERSHIP: we surface ONLY the REPLs THIS session started + owns, PLUS any
   external nREPL the user EXPLICITLY attached via `connect` (both from
   `repl-manager/session-repls`). There is still NO external-port discovery and
   no `.nrepl-port` scanning — attachment is explicit consent, never a scan.

   Eval defaults to the workspace-root REPL (else the first) when several exist;
   the result reports which REPL ran. Each mirror carries liveness status and
   diagnostics from a per-turn probe.

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
  (let
    [budget
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
  [session-id statuses {:keys [id dir port tool aliases log external? host]}]
  (let
    [existing
     (when (and session-id id) (vis/get-resource session-id id))

     probe
     (get statuses port)

     status
     (or (:status probe) :unknown)

     detail
     (cond-> {"dir" dir "port" port "managed" (not external?)}
       external?
       (assoc "host"
         (or host "localhost") "external"
         true)

       tool
       (assoc "tool" (name tool))

       (seq aliases)
       (assoc "aliases" (mapv name aliases))

       log
       (assoc "log" log)

       (seq (:versions probe))
       (assoc "versions" (update-keys (:versions probe) name))

       (:dialect probe)
       (assoc "dialect" (name (:dialect probe)))

       (:form probe)
       (assoc "form" (:form probe))

       (:hint probe)
       (assoc "hint" (:hint probe)))]

    (when (and session-id
               id
               (or (nil? existing)
                   (not= (name status) (get existing "status"))
                   (not= detail (get existing "detail"))
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
         ;; STRING-keyed `:detail` — resources.clj/->data passes it through verbatim.
         :detail detail
         :owner :ext/language-clojure}
        (cond->
          {:stop-fn (fn []
                      (repl-manager/stop! session-id dir))
           :restart-fn (if external?
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

(defn contribute
  "`:ext/ctx-fn` side-effect hook. Probes this session's owned REPLs and mirrors
   each into the resource registry; always returns `{}` because ctx-loop projects
   the registry into `session[\"resources\"]`. Never throws."
  [env]
  (try (if (:workspace/root env)
         (let
           [sid
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
           {})
         {})
       (catch Throwable e
         (tel/log! {:level :warn :id ::contribute-failed :data {:error (ex-message e)}}
                   "Clojure nREPL ctx contribution failed; degrading to empty")
         {})))
