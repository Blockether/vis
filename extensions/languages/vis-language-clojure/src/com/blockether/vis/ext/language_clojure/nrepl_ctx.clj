(ns com.blockether.vis.ext.language-clojure.nrepl-ctx
  "Per-turn `:ext/ctx` contribution for the Clojure pack.

   Mirrors foundation-core's `workspace-ctx`: instead of forcing the model
   to call `(clj/ports)` over and over, the engine injects live nREPL state
   into context as standing knowledge, nested UNDER the active language so a
   polyglot repo accumulates `:languages {:clojure {...} :typescript {...}}`:

     :session/env {:languages {:clojure {:nrepl {:default <int|nil>
                                                 :ports [{:port :source
                                                          :via :status
                                                          [:versions]} ...]}}}}

   Discovery (cheap `.nrepl-port` file reads) runs every render so a REPL
   started mid-turn shows up immediately. The liveness probe (`describe`
   round-trip) is the only network cost, so it is cached per turn AND per
   discovered port-set — re-probing only when the turn advances or the set
   of ports changes. All best-effort: any failure degrades to discovery
   with `:status :unknown` (or an empty contribution) and never blocks the
   render."
  (:require
   [clojure.java.io :as io]
   [com.blockether.vis.ext.language-clojure.nrepl-client :as nrepl-client]
   [com.blockether.vis.ext.language-clojure.ports :as ports]))

(def ^:private probe-timeout-ms 100)

;; Per-turn liveness cache. `{:key [host turn sorted-ports] :statuses {port status-map}}`.
;; defonce so it survives `(require :reload)` during REPL-driven dev.
(defonce ^:private liveness-cache (atom {:key nil :statuses {}}))

(defn- via-of
  "Best-effort 'how was this REPL started' hint, derived from the
   `.nrepl-port` source path."
  [source]
  (let [s (str source)]
    (cond
      (re-find #"\.lein[/\\]repl-port$" s)        :lein
      (re-find #"\.clojure[/\\]\.nrepl-port$" s)   :clojure-cli
      (re-find #"\.nrepl-port$" s)                 :project
      :else                                        :unknown)))

(defn- current-turn
  "Read the live turn off the engine ctx-atom for per-turn cache keying.
   Falls back to 0 when no ctx-atom is on env (e.g. tests / bare calls)."
  [env]
  (or (some-> (:ctx-atom env) deref :session/turn) 0))

(defn- probe-all
  "Probe every port in parallel, each under a hard deadline so one
   slow/firewalled host can never stall the render. Returns
   `{port {:status .. [:versions ..]}}`."
  [host ports]
  (let [futs (mapv (fn [p]
                     [p (future
                          (nrepl-client/probe!
                            {:host host :port p :timeout-ms probe-timeout-ms}))])
               ports)]
    (into {}
      (map (fn [[p f]] [p (deref f (+ probe-timeout-ms 50) {:status :down})]))
      futs)))

(defn- liveness-for
  "Statuses for `ports`, reusing the per-turn cache when the
   `[host turn port-set]` key is unchanged; otherwise probe and store."
  [host turn ports]
  (let [k [host turn (vec (sort ports))]]
    (if (= k (:key @liveness-cache))
      (:statuses @liveness-cache)
      (let [statuses (probe-all host ports)]
        (reset! liveness-cache {:key k :statuses statuses})
        statuses))))

(defn- source-dir
  "Parent directory of a `.nrepl-port` source file — a free working-directory
   fallback for project-rooted ports when the server can't be queried."
  [source]
  (some-> source io/file .getParentFile .getAbsolutePath))

(defn- nrepl-block
  "Build the `:nrepl` map from discovery hits + liveness statuses. Each port
   carries `:via` (how started), `:status`, `:dialect` (clj/cljs) and `:cwd`
   (working dir) so the model can tell one REPL from another."
  [hits statuses]
  {:default (some-> hits first :port)
   :ports   (mapv (fn [{:keys [port source]}]
                    (let [st  (get statuses port {:status :unknown})
                          via (via-of source)
                          cwd (or (:cwd st)
                                (when (= :project via) (source-dir source)))]
                      (cond-> {:port   port
                               :source source
                               :via    via
                               :status (:status st)}
                        (seq (:versions st)) (assoc :versions (:versions st))
                        (:dialect st)        (assoc :dialect (:dialect st))
                        cwd                  (assoc :cwd cwd))))
              hits)})

(defn contribute
  "`:ext/ctx` fn. Returns the `:session/env {:languages {:clojure {:nrepl ...}}}`
   slice, or `{}` when no workspace root is on env. Never throws."
  [env]
  (try
    (if-let [root (:workspace/root env)]
      (let [hits     (vec (ports/discover-all root))
            host     "localhost"
            statuses (when (seq hits)
                       (liveness-for host (current-turn env) (map :port hits)))]
        {:session/env {:languages {:clojure {:nrepl (nrepl-block hits statuses)}}}})
      {})
    (catch Throwable _ {})))
