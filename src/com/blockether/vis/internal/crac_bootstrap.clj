(ns com.blockether.vis.internal.crac-bootstrap
  "Single pre-extension bootstrap used by normal CLI startup, dev startup,
   and CRaC image creation.

   Why this exists: a CRaC checkpoint must be taken after the expensive
   Clojure/JVM class loading has happened, but before Vis opens unstable
   process resources: TUI terminals, extension-owned sockets, database
   pools, provider clients, or dev nREPL ports. The command wrappers can
   then restore this warmed image and invoke the real `com.blockether.vis.core`
   or `com.blockether.vis.dev` main with fresh user arguments."
  (:require
   [clojure.string :as str])
  (:import
   (java.lang.reflect InvocationTargetException Method)))

(def default-image-dir
  "Default CRaC image directory used by `bin/vis` auto-CRaC."
  (str (System/getProperty "user.home") "/.vis/crac/bin-vis"))

(def bootstrap-namespaces
  "Namespaces loaded into the CRaC image.

   Keep this list host-only. Extension discovery must stay after restore so
   extension registrars, TUI setup, database pools, and provider clients are
   created per invocation rather than frozen into the image. Requiring the
   public facade pulls in the host runtime and Clojure deps without scanning
   `META-INF/vis-extension/vis.edn`."
  '[com.blockether.vis.core])

(defonce ^:private state*
  (atom {:ran? false
         :phases []}))

(defn state
  "Return bootstrap state for tests and diagnostics."
  []
  @state*)

(defn reset-state!
  "Reset bootstrap state. Intended for tests and REPL diagnostics."
  []
  (reset! state* {:ran? false :phases []}))

(defn- require-namespace!
  [ns-sym]
  (require ns-sym))

(defn pre-extension-bootstrap!
  "Run the idempotent pre-extension bootstrap.

   Options:
   - `:phase` — diagnostic keyword, e.g. `:cli`, `:dev`, `:crac-checkpoint`.
   - `:namespaces` — namespace symbols to require before the caller proceeds.

   The function is deliberately idempotent inside one JVM so `bin/dev cli`,
   direct REPL calls, and restored CRaC invocations can share it without
   double-loading the same host surface."
  ([] (pre-extension-bootstrap! {}))
  ([{:keys [phase namespaces]
     :or   {phase :runtime
            namespaces []}}]
   (let [phase (or phase :runtime)]
     (swap! state* update :phases conj phase)
     (when-not (:ran? @state*)
       (doseq [ns-sym namespaces]
         (require-namespace! ns-sym))
       (swap! state* assoc :ran? true))
     (state))))

(defn- crac-core-class
  "Return a CRaC Core class when the current JDK exposes one.

   The current CRaC JDK API is `jdk.crac.Core`. The compatibility library
   uses `org.crac.Core`; older experiments used `javax.crac.Core`. Reflection
   keeps normal JDKs free of a compile-time CRaC dependency."
  []
  (some (fn [class-name]
          (try
            (Class/forName class-name)
            (catch ClassNotFoundException _ nil)))
    ["jdk.crac.Core" "org.crac.Core" "javax.crac.Core"]))

(defn- no-arg-method
  ^Method [^Class cls method-name]
  (.getMethod cls method-name (make-array Class 0)))

(defn request-checkpoint-restore!
  "Call CRaC `Core.checkpointRestore` reflectively.

   On successful image creation the original JVM exits. On restore this call
   returns, after the launcher-provided main has run. On unsupported JDKs it
   throws an ex-info with `:type :vis/crac-unavailable`."
  []
  (if-let [cls (crac-core-class)]
    (try
      (.invoke (no-arg-method cls "checkpointRestore") nil (object-array []))
      {:status :restored
       :core-class (.getName cls)}
      (catch InvocationTargetException e
        (throw (or (.getTargetException e) e))))
    (throw (ex-info "CRaC Core API is unavailable on this JVM"
             {:type :vis/crac-unavailable
              :tried ["jdk.crac.Core" "org.crac.Core" "javax.crac.Core"]}))))

(defn checkpoint-bootstrap!
  "Load host runtime namespaces and request a CRaC checkpoint/restore.

   This is the function behind the `:vis-crac-bootstrap` and
   `:dev-crac-bootstrap` aliases. It intentionally does not call extension
   discovery, logging setup, nREPL startup, or any channel entry point."
  []
  (pre-extension-bootstrap! {:phase :crac-checkpoint
                             :namespaces bootstrap-namespaces})
  (request-checkpoint-restore!))

(defn -main [& _args]
  (try
    (checkpoint-bootstrap!)
    ;; If this line is reached after a restore with a launcher-supplied main,
    ;; that main has already completed. End the bootstrap stack quietly.
    (shutdown-agents)
    (System/exit 0)
    (catch Throwable t
      (binding [*out* *err*]
        (println (str "vis CRaC bootstrap failed: " (or (ex-message t) (.getName (class t)))))
        (when-let [data (not-empty (ex-data t))]
          (println (str/trim (pr-str data)))))
      (shutdown-agents)
      (System/exit 1))))
