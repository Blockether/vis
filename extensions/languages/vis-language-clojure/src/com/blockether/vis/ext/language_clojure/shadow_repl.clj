(ns com.blockether.vis.ext.language-clojure.shadow-repl
  "shadow-cljs as the ClojureScript REPL RUNTIME: attaching to the nREPL a
   `shadow-cljs watch` already runs, SELECTING the build whose JS runtime an eval
   must land in, and keeping that selection true for every later eval.

   Four facts about that server decide everything here, and every one of them was
   learned from a live shadow-cljs, not from its wire protocol:

   1. A shadow-cljs nREPL is INDISTINGUISHABLE from a plain JVM one over
      `describe`: it advertises no cljs/shadow op and reports only a `:clojure`
      version, so dialect detection from describe metadata answers `:clj` for it.
      The only honest question is a read-only eval that RESOLVES
      `shadow.cljs.devtools.api` inside the server.
   2. The build is selected PER nREPL SESSION — `(shadow…api/nrepl-select :app)` —
      and the selection STICKS to that one session. Replace the session (an
      evicted socket, a restarted server) and the very same code silently
      evaluates as JVM Clojure again, which is why every eval re-checks the
      session token it selected under instead of trusting a boolean.
   3. `nrepl-select` answers the SAME `watch for build not running` whether the
      build id does not exist or is merely not being watched. The two are told
      apart BEFORE selecting — by the server's own build config and
      `worker-running?` — because they need opposite fixes.
   4. A selected build with NO connected JS runtime neither errors nor hangs:
      every eval answers `No available JS runtime.` on `:err` with a `done`
      status. Nothing but a started runtime fixes it, so that answer is turned
      into the instruction that starts one."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.ext.language-clojure.nrepl-client :as nrepl-client]))

(def ^:private probe-timeout-ms
  "Budget for the read-only probe/select round-trips. Generous: the server may be
   mid-compile, and a false `not shadow-cljs` is worse than a slow answer."
  10000)

(def no-runtime-marker
  "The exact stderr shadow-cljs emits for EVERY eval on a build whose runtime is
   not connected. Its own text, matched as a substring — never re-worded here."
  "No available JS runtime")

(def port-file-path
  "Where `shadow-cljs watch` publishes its nREPL port, relative to the project."
  [".shadow-cljs" "nrepl.port"])

(defn nrepl-port
  "The nREPL port a `shadow-cljs watch` running in `dir` published, or nil.

   ONE deterministic file, written by the user's OWN server, under the directory
   the caller explicitly named — this is not port discovery: nothing is scanned,
   no other server can answer, and with no watch running there is no file and the
   caller is told to start one."
  [dir]
  (let [^java.io.File f (apply io/file (str dir) port-file-path)]
    (when (.isFile f) (try (Long/parseLong (str/trim (slurp f))) (catch Throwable _ nil)))))

(defn- probe-form
  "Source for ONE read-only round trip answering everything an attach must know:
   is this shadow-cljs, which builds does it know, is `build` being watched, and
   what does that build target. Every shadow Var is `resolve`d, never called by
   name, so the SAME form is safe on a plain JVM nREPL — an unloaded namespace
   resolves to nil instead of throwing `No namespace`."
  [build]
  (let [b (when build (keyword build))]
    (str "(let [wr (resolve 'shadow.cljs.devtools.api/worker-running?)"
         "      gb (resolve 'shadow.cljs.devtools.api/get-build-config)"
         "      cfg (resolve 'shadow.cljs.devtools.config/load-cljs-edn)"
         "      b "
         (pr-str b)
         "]"
         "  {:shadow (some? wr)"
         "   :builds (when cfg (try (vec (keys (:builds (cfg)))) (catch Throwable _ nil)))"
         "   :worker (boolean (when (and wr b) (wr b)))"
         "   :target (when (and gb b) (try (:target (gb b)) (catch Throwable _ nil)))})")))

(defn- read-value
  "The EDN value an eval result printed, or nil when it printed nothing readable."
  [result]
  (try (edn/read-string (str (get result "value"))) (catch Throwable _ nil)))

(defn probe!
  "What the nREPL at `host:port` IS, in one read-only eval:

     {:shadow? true :builds [\"npm\" \"app\"] :worker? true :target :node-script}

   `:builds` are the ids the SERVER loaded (its own config is authoritative — the
   caller's `cwd` may not be where it runs); `:worker?` is whether `build` has a
   live `watch`; `:target` is that build's shadow target. A plain JVM nREPL
   answers `:shadow? false`, and an unreachable one adds `:error`."
  [{:keys [host port build timeout-ms]}]
  (let
    [r
     (try (nrepl-client/eval! {:host (or host "localhost")
                               :port port
                               :code (probe-form build)
                               :timeout-ms (or timeout-ms probe-timeout-ms)})
          (catch Throwable e {"error_message" (.getMessage e)}))

     v
     (read-value r)]

    (if (map? v)
      {:shadow? (boolean (:shadow v))
       :builds (mapv name (or (:builds v) []))
       :worker? (boolean (:worker v))
       :target (:target v)}
      {:shadow? false
       :builds []
       :worker? false
       :error (or (get r "error_message") (not-empty (str/trim (str (get r "err")))))})))

(defn select!
  "Select `build` in the nREPL session `nrepl-client/eval!` reuses for `host:port`,
   by evaluating shadow's own `nrepl-select` IN that session — so every later eval
   through the same session lands in that build's JS runtime.

   Answers `{:selected? true :session-token t}` (the token the selection now
   belongs to) or `{:selected? false :message …}` with the server's own reason."
  [{:keys [host port build timeout-ms]}]
  (let
    [host
     (or host "localhost")

     r
     (try (nrepl-client/eval!
            {:host host
             :port port
             :code (str "(shadow.cljs.devtools.api/nrepl-select " (pr-str (keyword build)) ")")
             :timeout-ms (or timeout-ms probe-timeout-ms)})
          (catch Throwable e {"error_message" (.getMessage e)}))]

    ;; shadow answers `[:selected :app]`; anything else (an eval-error carrying
    ;; `watch for build not running`, a connect failure) leaves the session in
    ;; CLJ, exactly where it was.
    (if (str/includes? (str (get r "value")) ":selected")
      {:selected? true :session-token (nrepl-client/session-token host port)}
      {:selected? false
       :message (or (get r "error_message")
                    (not-empty (str/trim (str (get r "err"))))
                    "shadow-cljs did not select the build")})))

(defn no-runtime?
  "True when this eval result is shadow-cljs saying the build has no JS runtime
   attached — a `done` reply whose stderr carries shadow's own marker, never an
   exception and never a timeout."
  [result]
  (str/includes? (str (get result "err")) no-runtime-marker))

(defn runtime-hint
  "What actually starts the missing runtime, phrased for THIS build's `target`.
   The watch stays running either way — the runtime is the second process."
  [build target]
  (str "shadow-cljs build "
       build
       " has no JS runtime connected: it is attached and compiled, but nothing can"
       " evaluate until a runtime joins it — "
       (case target
         :node-script
         "run the built script (`node <:output-to>`)"

         :node-library
         "load the built library in a node process"

         (:browser :esm-web)
         "open the page this build serves in a browser"

         :browser-test
         "open the test page this build serves"

         :react-native
         "start the app in the simulator/device"

         (str "start this build's runtime (`node <:output-to>` for node targets,"
              " the page for browser targets)"))
       ", while `shadow-cljs watch "
       build
       "` keeps running."))

(defn eval!
  "Evaluate in `build`'s JS runtime over the shadow-cljs nREPL at `host:port`.

   `attachment` is `{:host :port :build :target :session-token}` — the token being
   the session the build was selected in. The selection is re-made ONLY when that
   session is gone (token nil or changed), so the hot path stays one round trip
   while a replaced session can never silently answer as JVM Clojure.

   Answers `{:selected? true :result <nrepl-client/eval! map> :session-token t}`,
   plus `:message` when the build has no runtime; a build that can no longer be
   selected answers `{:selected? false :message …}` and evaluates NOTHING."
  [{:keys [host port build target session-token]} eval-opts]
  (let
    [host
     (or host "localhost")

     current
     (nrepl-client/session-token host port)

     stale?
     (or (nil? session-token) (not= session-token current))

     sel
     (when stale? (select! {:host host :port port :build build}))]

    (if (and stale? (not (:selected? sel)))
      {:selected? false :message (:message sel)}
      (let
        [r (nrepl-client/eval! (assoc eval-opts
                                 :host host
                                 :port port))]
        (cond-> {:selected? true :result r :session-token (nrepl-client/session-token host port)}
          (no-runtime? r)
          (assoc :message (runtime-hint build target)))))))
