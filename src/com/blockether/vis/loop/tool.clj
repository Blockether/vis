(ns com.blockether.vis.loop.tool
  "Tool definition helpers shared by tool producers and SCI runtime code."
  (:require [clojure.string :as str]
            [com.blockether.vis.loop.runtime.shared :as rt-shared]))

(defn- default-validate-input
  [{:keys [args]}]
  {:args (vec args)})

(defn- default-validate-output
  [{:keys [result]}]
  {:result result})

(def ^:private DEFAULT_FORMAT_CAP_CHARS
  "Safety cap for the default formatter. Matches loop.core/EXECUTION_SAFETY_CAP_CHARS.
   Only protects against pathological non-lazy dumps; realize-value already
   bounds lazy seqs upstream."
  200000)

(defn default-format-result
  "Default tool result formatter.

   Contract — MUST hold for every :format-result override:
   - Strictly 1-arity — receives ONLY the tool's raw return value.
   - Pure — result is a function of the input alone. No env, no dynamic vars,
     no other iteration state.
   - Returns a plain string safe to embed inside a mustache template
     (no active {{}} tags in output — callers treat this as escaped content).

   The default realizes lazy seqs (bounded at 100 items), pr-str's, and
   caps at DEFAULT_FORMAT_CAP_CHARS. Tool authors override this to produce
   a token-efficient, human-readable rendering for their specific return shape."
  [value]
  (-> value rt-shared/realize-value pr-str (rt-shared/truncate DEFAULT_FORMAT_CAP_CHARS)))

(defn- fn-arity-1?
  "Best-effort check that `f` accepts exactly one positional arg.

   Rejects variadic-only fns ([& args]) and multi-arity fns that don't
   include a 1-arg signature. We INTENTIONALLY don't reject multi-arity
   fns that happen to include a 1-arg overload — they satisfy the contract.

   The check probes :arglists metadata first; if absent, tries reflective
   `invoke(Object)` method presence on the fn class (works for fn literals
   produced by `fn` / `defn`)."
  [f]
  (if-let [arglists (:arglists (meta f))]
    (some (fn [al]
            (let [al (vec al)
                  amp-idx (.indexOf ^java.util.List al '&)]
              (or (and (neg? amp-idx) (= 1 (count al)))
                  (and (pos? amp-idx) (<= amp-idx 1)))))
      arglists)
    ;; No :arglists — reflect on the class.
    (let [klass (class f)
          methods (.getDeclaredMethods klass)]
      (boolean (some (fn [^java.lang.reflect.Method m]
                       (and (= "invoke" (.getName m))
                            (= 1 (count (.getParameterTypes m)))))
                 methods)))))

(defn- normalize-arglists
  [arglists]
  (cond
    (nil? arglists) nil
    (vector? arglists) arglists
    (seq? arglists) (vec arglists)
    :else nil))

(defn- arglist->example
  [sym arglist]
  (let [args (->> arglist
               (remove #{'&})
               (map str)
               (str/join " "))]
    (str "(" sym
      (when-not (str/blank? args)
        (str " " args))
      ")")))

(defn- default-examples
  [sym arglists]
  (if-let [arglist (first (seq arglists))]
    [(arglist->example sym arglist)]
    [(str "(" sym ")")]))

(defn- non-blank-string?
  [x]
  (and (string? x) (not (str/blank? x))))

(defn- normalize-prompt-fn
  "Accept a string or `(fn [env] string)` on :prompt and normalize to a fn.

   Returning a nil/blank string from the fn is legal — the renderer skips
   tools with no prompt text. This lets tools compute their prompt from the
   live env (e.g. git tools listing attached repos) without forcing every
   tool to write a lambda."
  [prompt]
  (cond
    (nil? prompt)        nil
    (fn? prompt)         prompt
    (string? prompt)     (constantly prompt)
    :else (throw (ex-info "tool-def :prompt must be a string or (fn [env] string)"
                   {:type :rlm/invalid-tool-def :field :prompt :got prompt}))))

(defn complete-fn-tool-def
  "Return a canonical function tool-def with all required keys populated.

   Required canonical keys:
   - :sym, :fn, :type, :doc, :arglists, :validate-input, :validate-output, :examples

   Optional keys:
   - :activation-fn — `(fn [env] bool)`. When present, the tool is only bound
     in the sandbox if `(activation-fn env)` returns truthy at query time.
     Receives the full env map (db-info, router, state-atom, etc.).
   - :prompt — string OR `(fn [env] string)`. Rich prompt-time documentation
     injected into the system prompt's <tools> block when the tool is active.
     Normalized to a fn internally; callers can always invoke `((:prompt tool-def) env)`.
     Distinct from :doc (one-liner summary) and :examples (call syntax). Use
     :prompt to tell the LLM when to reach for this tool, what its arguments
     mean, what it returns, and any gotchas."
  [sym f tool-def]
  (let [fn-meta (meta f)
        inferred-doc (:doc fn-meta)
        inferred-arglists (:arglists fn-meta)
        arglists (or (:arglists tool-def) inferred-arglists)
        normalized-arglists (or (normalize-arglists arglists) ['[& args]])
        examples (or (:examples tool-def) (default-examples sym normalized-arglists))
        prompt-fn (normalize-prompt-fn (:prompt tool-def))]
    (-> tool-def
      (assoc :sym sym
        :fn f
        :type :fn
        :doc (or (:doc tool-def) inferred-doc)
        :arglists normalized-arglists
        :validate-input (or (:validate-input tool-def) default-validate-input)
        :validate-output (or (:validate-output tool-def) default-validate-output)
        :format-result (or (:format-result tool-def) default-format-result)
        :activation-fn (or (:activation-fn tool-def) (constantly true))
        :examples (vec examples))
      (cond-> prompt-fn (assoc :prompt prompt-fn)))))

(defn assert-fn-tool-def!
  "Validate canonical function tool-def shape. Throws ex-info on invalid input."
  [tool-def]
  (let [{:keys [sym fn type doc arglists validate-input validate-output examples]} tool-def]
    (when-not (symbol? sym)
      (throw (ex-info "tool-def :sym must be a symbol"
               {:type :rlm/invalid-tool-def :field :sym :tool-def tool-def})))
    (when-not (fn? fn)
      (throw (ex-info "tool-def :fn must be a function"
               {:type :rlm/invalid-tool-def :field :fn :tool-def (dissoc tool-def :fn)})))
    (when-not (= :fn type)
      (throw (ex-info "tool-def :type must be :fn"
               {:type :rlm/invalid-tool-def :field :type :tool-def (dissoc tool-def :fn)})))
    (when-not (non-blank-string? doc)
      (throw (ex-info "tool-def :doc must be a non-blank string"
               {:type :rlm/invalid-tool-def :field :doc :tool-def (dissoc tool-def :fn)})))
    (when-not (and (vector? arglists) (seq arglists))
      (throw (ex-info "tool-def :arglists must be a non-empty vector"
               {:type :rlm/invalid-tool-def :field :arglists :tool-def (dissoc tool-def :fn)})))
    (when-not (fn? validate-input)
      (throw (ex-info "tool-def :validate-input must be a function"
               {:type :rlm/invalid-tool-def :field :validate-input :tool-def (dissoc tool-def :fn)})))
    (when-not (fn? validate-output)
      (throw (ex-info "tool-def :validate-output must be a function"
               {:type :rlm/invalid-tool-def :field :validate-output :tool-def (dissoc tool-def :fn)})))
    (let [fmt (:format-result tool-def)]
      (when-not (fn? fmt)
        (throw (ex-info "tool-def :format-result must be a function"
                 {:type :rlm/invalid-tool-def :field :format-result :tool-def (dissoc tool-def :fn)})))
      (when-not (fn-arity-1? fmt)
        (throw (ex-info "tool-def :format-result must accept exactly 1 arg (the raw tool return value)"
                 {:type :rlm/invalid-tool-def :field :format-result :tool-def (dissoc tool-def :fn)})))
      ;; Probe the formatter with nil to flush obvious crashes. nil is a legal
      ;; input (a tool may return nil) — the formatter MUST handle it without
      ;; throwing, and MUST return a string.
      (let [probe (try (fmt nil) (catch Throwable t t))]
        (when (instance? Throwable probe)
          (throw (ex-info "tool-def :format-result threw when called with nil"
                   {:type :rlm/invalid-tool-def :field :format-result
                    :tool-def (dissoc tool-def :fn)
                    :cause (ex-message ^Throwable probe)})))
        (when-not (string? probe)
          (throw (ex-info "tool-def :format-result must return a string"
                   {:type :rlm/invalid-tool-def :field :format-result
                    :tool-def (dissoc tool-def :fn)
                    :returned-type (some-> probe class .getName)})))))
    (when-not (and (vector? examples)
                (seq examples)
                (every? non-blank-string? examples))
      (throw (ex-info "tool-def :examples must be a non-empty vector of non-blank strings"
               {:type :rlm/invalid-tool-def :field :examples :tool-def (dissoc tool-def :fn)})))
    (when-not (fn? (:activation-fn tool-def))
      (throw (ex-info "tool-def :activation-fn must be a function (fn [env] -> boolean)"
               {:type :rlm/invalid-tool-def :field :activation-fn :tool-def (dissoc tool-def :fn)})))
    ;; :prompt is optional; when present it must be a 1-arity fn that returns
    ;; a string (or nil). Normalization already coerces string→fn.
    (when-let [prompt (:prompt tool-def)]
      (when-not (fn? prompt)
        (throw (ex-info "tool-def :prompt must normalize to a function (fn [env] -> string|nil)"
                 {:type :rlm/invalid-tool-def :field :prompt :tool-def (dissoc tool-def :fn)}))))
    tool-def))

(defn maybe-assert-fn-tool-def!
  "Validate when the map looks like a function tool-def.

   This lets hook-only updates pass through while still enforcing canonical
   shape once a tool record carries :fn / :type :fn / :sym."
  [tool-def]
  (if (or (= :fn (:type tool-def))
        (contains? tool-def :fn)
        (contains? tool-def :sym))
    (assert-fn-tool-def! tool-def)
    tool-def))

(defn make-tool-def
  "Build and validate a canonical function tool-def.

   This helper guarantees every required key is present before registration."
  [sym f tool-def]
  (-> (complete-fn-tool-def sym f tool-def)
    (assert-fn-tool-def!)))
