(ns com.blockether.vis.internal.slash
  "Channel-agnostic slash dispatch.

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

   Slash run-fns may return an EXTENDED `:slash/*` envelope that carries
   a rendered result card back to the channel:

     {:slash/status :ok | :error | :nothing-to-commit | :ff-failed
      :slash/title  short headline (string, plain)
      :slash/body   IR (vector starting with :ir ...) OR Markdown string
      :slash/actions [{:label :slash}]   ;; optional follow-ups
      :slash/data    arbitrary payload (workspace-id, sha, ...)}"
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.prompt :as prompt]
            [com.blockether.vis.internal.prompt-templates :as prompt-templates]
            [taoensso.telemere :as tel]))

;; =============================================================================
;; Aggregation
;; =============================================================================

(defn active-slashes
  "Aggregate `:ext/slash-commands` from every active extension for `env`.
   Order: extension registration order (stable). Returns a vec.

   This is the ONLY source of truth for engine slash dispatch (which
   already has a turn env handy). Channels that lack a per-turn env
   should use `registered-slashes` (no activation filtering)."
  [env]
  (vec (mapcat :ext/slash-commands (prompt/active-extensions env))))

(defn registered-slashes
  "Walk every globally registered extension and return the union of
   their `:ext/slash-commands` specs. Activation-fn filtering is NOT
   applied here — channels that surface slash UX before a session is
   running (TUI palette overlay) use this
   env-less view. The engine dispatch path itself goes through
   `active-slashes env` so per-session activation-fn rules still hold."
  []
  (vec (mapcat :ext/slash-commands (extension/registered-extensions))))

(defn- spec-visible-for-channel?
  "True when slash `spec` is safe to surface in `channel`'s slash UX — not
   hidden, and its `:slash/availability-fn` (if any) admits the channel.
   `registered-slashes` is env-less and carries specs from every channel
   (e.g. non-TUI `/help`, `/model`), so each channel filters here
   before it renders suggestions."
  [channel spec]
  (and (not (:slash/hidden? spec))
       (if-let [available? (:slash/availability-fn spec)]
         (try (boolean (available? {:channel/id channel})) (catch Throwable _ false))
         true)))

(defn slash-palette
  "THE canonical typed-`/` palette for a channel — the single source every
   channel's `/` autocomplete / command menu consumes, so skills and file
   prompts appear uniformly instead of each channel re-deriving the set.
   Returns `[{:name \"/path\" :doc str} …]`:

     • LEAF registered slashes available in `channel` — group roots dropped
       (a spec whose path is some other visible spec's `:slash/parent`) and
       hidden / channel-unavailable specs filtered out — then
     • prompt-template entries: `.vis/prompts/*.md` file prompts and harness
       `/skill:<name>` commands, minus any name a registered slash already
       claimed (registered slashes always win).

   `extra` are CHANNEL-NATIVE entries (`{:name :doc}`) the channel handles
   itself (e.g. the web's `/new-session`); they PREPEND and win name
   collisions against templates."
  ([channel] (slash-palette channel nil))
  ([channel extra]
   (let
     [avail
      (filter #(spec-visible-for-channel? channel %) (registered-slashes))

      parent-paths
      (into #{}
            (keep #(let
                     [p
                      (vec (:slash/parent %))]

                     (when (seq p) p)))
            avail)

      leaf?
      (fn [s]
        (not (contains? parent-paths (conj (vec (:slash/parent s)) (:slash/name s)))))

      path-name
      (fn [s]
        (str "/" (str/join " " (concat (:slash/parent s) [(:slash/name s)]))))

      registered
      (->> avail
           (filter leaf?)
           (map (fn [s]
                  {:name (path-name s) :doc (str (:slash/doc s))})))

      specs
      (concat (vec extra) registered)

      taken
      (into #{} (map :name) specs)

      templates
      (try (->> (prompt-templates/templates)
                (keep (fn [{:keys [name description]}]
                        (let [nm (str "/" name)]
                          (when-not (contains? taken nm) {:name nm :doc (str description)})))))
           (catch Throwable _ nil))]

     (vec (concat specs templates)))))

(defn- index-by-path
  "Build `{path-vec [slash-spec ...]}` from a vec of slash specs.
   Multiple specs may share a path when their availability-fn
   predicates partition the channel set (e.g. TUI `/voice` and a
   non-TUI `/voice` registered by separate extensions). Per-path
   vec is in registration order."
  [slashes]
  (reduce (fn [acc s]
            (update acc (extension/slash-path s) (fnil conj []) s))
          {}
          slashes))

(defn- spec-available-for?
  "True when `spec` permits dispatch for `ctx` per its
   `:slash/availability-fn` (or always-true when absent)."
  [spec ctx]
  (if-let [f (:slash/availability-fn spec)]
    (try (boolean (f ctx)) (catch Throwable _ false))
    true))

(defn slash-by-path
  "Return the slash spec whose full path = `path`, or nil. `path` is a
   non-empty vec of names like `[\"workspace\" \"apply\"]`. When
   multiple specs share the path (per-channel partitioning), returns
   the first registered."
  [env path]
  (let [path (vec path)]
    (when (seq path) (first (get (index-by-path (active-slashes env)) path)))))

(defn slash-children
  "Return the vec of slash specs whose `:slash/parent` = `parent` vec.
   `parent` defaults to `[]` (top-level commands)."
  ([env] (slash-children env []))
  ([env parent]
   (let [parent (vec parent)]
     (->> (active-slashes env)
          (filter (fn [s]
                    (= parent (vec (:slash/parent s)))))
          vec))))

;; =============================================================================
;; Parsing
;; =============================================================================

(defn- slash-text?
  "True when `text` begins with `/` followed by a command-shaped token:
   at least one character, NO interior `/`. Matches the channel
   convention `/word ...` while rejecting absolute file paths — a
   terminal drop pastes `/var/folders/…/shot.png what is this` and that
   must run as a normal turn (image attachment scan included), not die
   as `Unknown slash command`. Slash names never contain `/`; nested
   commands are separate tokens (`/draft new`)."
  [text]
  (boolean (and (string? text) (re-find #"^/[^\s/]+(?:\s|$)" text))))

(defn- tokenize
  [text]
  ;; Strip the leading `/`, then split on whitespace. Quoted args are
  ;; out of scope for this revision — the live channels (the TUI
  ;; palette) never carry shell quotes; their input layer hands
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
      (when (seq tokens) {:path (vec tokens) :args [] :raw text}))))

(defn- resolve-longest-prefix
  "Walk the token vec back-to-front and return the deepest prefix
   that resolves to a registered slash. When multiple specs share
   the same path with disjoint availability predicates, prefer the
   first whose predicate accepts `ctx`; otherwise fall back to the
   first spec at that path so the dispatcher can surface
   `:reason :unavailable` (rather than masking the slash as
   `:unknown`). Returns `{:path :args :slash}` or nil."
  [env ctx tokens]
  (let [by-path (index-by-path (active-slashes env))]
    (loop [n (count tokens)]
      (when (pos? n)
        (let
          [prefix (subvec tokens 0 n)
           specs (get by-path prefix)]

          (if (seq specs)
            (let
              [chosen (or (some (fn [spec]
                                  (when (spec-available-for? spec ctx) spec))
                                specs)
                          (first specs))]
              {:path prefix :args (vec (subvec tokens n)) :slash chosen})
            (recur (dec n))))))))

;; =============================================================================
;; Dispatch
;; =============================================================================

(defn- missing-requires
  "Return the set of `:slash/requires` entries unsatisfied by `ctx`."
  [slash ctx]
  (let
    [needs
     (set (:slash/requires slash))

     sat?
     (fn [r]
       (case r
         :session
         (some? (or (:session/id ctx) (:session-id ctx)))

         :workspace
         (some? (or (:workspace/id ctx) (:workspace-id ctx)))

         :channel
         (some? (or (:channel/id ctx) (:channel-id ctx)))

         false))]

    (into #{} (remove sat?) needs)))

(defn dispatch
  "Dispatch slash `text` against `env`. `ctx` carries channel-side knobs:

     :channel/id            keyword, required (:tui, :cli, ...)
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
  (cond (not (slash-text? text)) {:handled? false}
        :else (let [tokens (tokenize text)]
                (if (empty? tokens)
                  {:handled? true :error "Empty slash command" :reason :unknown}
                  (if-let [{:keys [path args slash]} (resolve-longest-prefix env ctx tokens)]
                    (let
                      [ctx* (assoc ctx
                              :command/path path
                              :command/argv args
                              :command/raw text)
                       missing (missing-requires slash ctx*)]

                      (cond (seq missing) {:handled? true
                                           :error (str "Slash " (pr-str path) " requires " missing)
                                           :reason :requires-failed
                                           :missing missing
                                           :path path}
                            ;; resolve-longest-prefix already filtered on
                            ;; availability against the OUTER `ctx`; re-check
                            ;; against `ctx*` (with :command/path stamped) so a
                            ;; spec whose availability depends on path/args still
                            ;; has a chance to refuse.
                            (not (spec-available-for? slash ctx*))
                            {:handled? true
                             :error (str "Slash " (pr-str path) " not available in this context")
                             :reason :unavailable
                             :path path}
                            (nil? (:slash/run-fn slash))
                            {:handled? true
                             :error (str "Slash " (pr-str path) " has no :slash/run-fn")
                             :reason :no-run-fn
                             :path path}
                            :else (try (let [result ((:slash/run-fn slash) ctx*)]
                                         {:handled? true :result result :path path})
                                       (catch Throwable t
                                         (tel/log! {:level :warn
                                                    :id ::run-fn-threw
                                                    :data {:path path :error (ex-message t)}})
                                         {:handled? true
                                          :error (or (ex-message t) (str t))
                                          :reason :run-failed
                                          :ex t
                                          :path path}))))
                    {:handled? true
                     :error (str "Unknown slash command: " text)
                     :reason :unknown
                     :tokens tokens})))))
