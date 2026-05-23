(ns com.blockether.vis.internal.slash
  "Channel-agnostic slash dispatch (PLAN.md section 3).

   Slashes are DECLARATIVE: every extension carries `:ext/slash-commands`
   on its manifest; the engine derives the active slash set by walking
   `(active-extensions environment)` at lookup time. NO global atom, NO
   `register-slash!` imperative call.

   Public surface (re-exported through `core.clj`):

     (active-slashes  env)              -> vec of slash specs
     (slash-by-path   env path)         -> slash spec or nil
     (slash-children  env parent)       -> vec of slash specs whose
                                           `:slash/parent` = parent
     (parse           text)             -> {:path :args :raw} | nil
                                           (raw tokenisation only;
                                            does NOT consult any registry)
     (dispatch        env ctx text)     -> envelope (see below)

   The dispatch envelope is the contract every channel renders against:

     {:handled? true  :result <slash result map> :path path}
     {:handled? true  :error msg :reason :unknown :tokens tokens}
     {:handled? true  :error msg :reason :requires-failed :missing #{} :path}
     {:handled? true  :error msg :reason :unavailable :path}
     {:handled? true  :error msg :reason :no-run-fn :path}
     {:handled? true  :error msg :reason :run-failed :ex t :path}
     {:handled? false}  -- text was not a slash; channel forwards to LLM.

   A slash text is any non-blank string starting with `/` followed by
   at least one word. Plain prose without the leading `/` is ALWAYS
   {:handled? false}.

   Slash run-fns may return an EXTENDED `:slash/*` envelope that lets
   the slash write to the CTX engine just like a normal SCI mutator:

     {:slash/status :ok | :error | :nothing-to-commit | :ff-failed
      :slash/title  short headline (string, plain)
      :slash/body   IR (vector starting with :ir ...) OR Markdown string
      :slash/actions [{:label :slash}]   ;; optional follow-ups
      :slash/specs   {entry-key partial-spec-map}    ;; -> :session/specs
      :slash/tasks   {entry-key partial-task-map}    ;; -> :session/tasks
      :slash/facts   {entry-key partial-fact-map}    ;; -> :session/facts
      :slash/data    arbitrary payload (workspace-id, sha, ...)}

   The engine routes `:slash/specs / :tasks / :facts` through
   `ctx-loop/apply-and-record!` so a slash leaves the same kind of
   trace on `:session/*` that a model-emitted `(spec-set! ...)` / etc.
   would have. Validation, hook-task dedup, FSM safety all flow the
   same way. PLAN.md section 12 step 7."

  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.prompt :as prompt]
   [taoensso.telemere :as tel]))

(set! *warn-on-reflection* true)

;; =============================================================================
;; Aggregation
;; =============================================================================

(defn active-slashes
  "Aggregate `:ext/slash-commands` from every active extension for `env`.
   Order: extension registration order (stable). Returns a vec.

   This is the ONLY source of truth. Channels never cache or copy this
   set; they call `active-slashes` (or the derived lookups below) per
   turn so a hot-reload of an extension takes effect immediately."
  [env]
  (vec (mapcat :ext/slash-commands (prompt/active-extensions env))))

(defn- index-by-path
  "Build {path-vec slash-spec} from a vec of slash specs."
  [slashes]
  (reduce (fn [acc s] (assoc acc (extension/slash-path s) s))
    {}
    slashes))

(defn slash-by-path
  "Return the slash spec whose full path = `path`, or nil. `path` is a
   non-empty vec of names like `[\"workspace\" \"apply\"]`."
  [env path]
  (let [path (vec path)]
    (when (seq path)
      (get (index-by-path (active-slashes env)) path))))

(defn slash-children
  "Return the vec of slash specs whose `:slash/parent` = `parent` vec.
   `parent` defaults to `[]` (top-level commands)."
  ([env] (slash-children env []))
  ([env parent]
   (let [parent (vec parent)]
     (->> (active-slashes env)
       (filter (fn [s] (= parent (vec (:slash/parent s)))))
       vec))))

;; =============================================================================
;; Parsing
;; =============================================================================

(defn- slash-text?
  "True when `text` begins with `/` followed by at least one non-space
   character. Matches the channel convention: `/word ...`."
  [text]
  (boolean (and (string? text)
             (re-find #"^/\S" text))))

(defn- tokenize
  [text]
  ;; Strip the leading `/`, then split on whitespace. Quoted args are
  ;; out of scope for this revision — the live channels (TUI palette,
  ;; Telegram bot) never carry shell quotes; their input layer hands
  ;; us pre-trimmed text.
  (->> (str/split (subs text 1) #"\s+")
    (remove str/blank?)
    vec))

(defn parse
  "Tokenise a slash `text` into `{:path :args :raw}` or nil.

   This function performs PURE tokenisation. It does NOT consult any
   slash registry; the engine resolves the longest matching prefix at
   dispatch time once it has the env. This keeps `parse` pure and
   testable in isolation."
  [text]
  (when (slash-text? text)
    (let [tokens (tokenize text)]
      (when (seq tokens)
        {:path (vec tokens)
         :args []
         :raw  text}))))

(defn- resolve-longest-prefix
  "Walk the token vec back-to-front and return the deepest prefix
   that resolves to a registered slash. Returns
   `{:path :args :slash}` or nil."
  [env tokens]
  (let [by-path (index-by-path (active-slashes env))]
    (loop [n (count tokens)]
      (when (pos? n)
        (let [prefix (subvec tokens 0 n)]
          (if-let [s (get by-path prefix)]
            {:path prefix
             :args (vec (subvec tokens n))
             :slash s}
            (recur (dec n))))))))

;; =============================================================================
;; Dispatch
;; =============================================================================

(defn- missing-requires
  "Return the set of `:slash/requires` entries unsatisfied by `ctx`."
  [slash ctx]
  (let [needs (set (:slash/requires slash))
        sat?  (fn [r]
                (case r
                  :session   (some? (or (:session/id ctx) (:session-id ctx)))
                  :workspace (some? (or (:workspace/id ctx) (:workspace-id ctx)))
                  :channel   (some? (or (:channel/id ctx) (:channel-id ctx)))
                  false))]
    (into #{} (remove sat?) needs)))

(defn dispatch
  "Dispatch slash `text` against `env`. `ctx` carries channel-side knobs:

     :channel/id            keyword, required (:tui, :telegram, ...)
     :session/id            session-soul UUID (optional unless slash
                            declares `:slash/requires #{:session}`)
     :workspace/id          workspace UUID (optional unless required)
     :db-info               persistence handle (always passed when
                            present in the channel env)
     :reply!                (fn [ir-or-string]) for channels that want
                            an immediate side-effect surface
     :publish!              (fn [event]) bus for cross-channel events

   Return shapes documented at the namespace docstring."
  [env ctx text]
  (cond
    (not (slash-text? text))
    {:handled? false}

    :else
    (let [tokens (tokenize text)]
      (if (empty? tokens)
        {:handled? true :error "Empty slash command" :reason :unknown}
        (if-let [{:keys [path args slash]} (resolve-longest-prefix env tokens)]
          (let [ctx*    (assoc ctx
                          :command/path path
                          :command/argv args
                          :command/raw  text)
                missing (missing-requires slash ctx*)]
            (cond
              (seq missing)
              {:handled? true
               :error    (str "Slash " (pr-str path) " requires " missing)
               :reason   :requires-failed
               :missing  missing
               :path     path}

              (and (:slash/availability-fn slash)
                (not (try (boolean ((:slash/availability-fn slash) ctx*))
                       (catch Throwable t
                         (tel/log! {:level :warn :id ::availability-fn-threw
                                    :data  {:path  path
                                            :error (ex-message t)}})
                         false))))
              {:handled? true
               :error    (str "Slash " (pr-str path) " not available in this context")
               :reason   :unavailable
               :path     path}

              (nil? (:slash/run-fn slash))
              {:handled? true
               :error    (str "Slash " (pr-str path) " has no :slash/run-fn")
               :reason   :no-run-fn
               :path     path}

              :else
              (try
                (let [result ((:slash/run-fn slash) ctx*)]
                  {:handled? true :result result :path path})
                (catch Throwable t
                  (tel/log! {:level :warn :id ::run-fn-threw
                             :data  {:path  path
                                     :error (ex-message t)}})
                  {:handled? true
                   :error    (or (ex-message t) (str t))
                   :reason   :run-failed
                   :ex       t
                   :path     path}))))
          {:handled? true
           :error    (str "Unknown slash command: " text)
           :reason   :unknown
           :tokens   tokens})))))
