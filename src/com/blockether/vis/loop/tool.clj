(ns com.blockether.vis.loop.tool
  "Tool definition helpers shared by tool producers and SCI runtime code."
  (:require [clojure.string :as str]))

(defn- default-validate-input
  [{:keys [args]}]
  {:args (vec args)})

(defn- default-validate-output
  [{:keys [result]}]
  {:result result})

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

(defn complete-fn-tool-def
  "Return a canonical function tool-def with all required keys populated.

   Required canonical keys:
   - :sym, :fn, :type, :doc, :arglists, :validate-input, :validate-output, :examples

   Optional keys:
   - :activation-fn — `(fn [env] bool)`. When present, the tool is only bound
     in the sandbox if `(activation-fn env)` returns truthy at query time.
     Receives the full env map (db-info, router, state-atom, etc.)."
  [sym f tool-def]
  (let [fn-meta (meta f)
        inferred-doc (:doc fn-meta)
        inferred-arglists (:arglists fn-meta)
        arglists (or (:arglists tool-def) inferred-arglists)
        normalized-arglists (or (normalize-arglists arglists) ['[& args]])
        examples (or (:examples tool-def) (default-examples sym normalized-arglists))]
    (-> tool-def
      (assoc :sym sym
        :fn f
        :type :fn
        :doc (or (:doc tool-def) inferred-doc)
        :arglists normalized-arglists
        :validate-input (or (:validate-input tool-def) default-validate-input)
        :validate-output (or (:validate-output tool-def) default-validate-output)
        :examples (vec examples)))))

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
    (when-not (and (vector? examples)
                (seq examples)
                (every? non-blank-string? examples))
      (throw (ex-info "tool-def :examples must be a non-empty vector of non-blank strings"
               {:type :rlm/invalid-tool-def :field :examples :tool-def (dissoc tool-def :fn)})))
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
