(ns com.blockether.vis.internal.sci-symbols
  "Engine-owned SCI symbol introspection. Exposes two bare sandbox
   system calls — `doc` and `apropos` — that describe the sandbox
   itself (mirroring `clojure.repl/doc` + `source` and
   `clojure.repl/apropos`).

   These are NOT `v/`-aliased foundation tools: they are properties of
   the SCI sandbox the engine owns, so they live alongside `done` /
   `set-session-title!` and resolve against the live `:sci-ctx`.

   `(doc 'sym)`      → docstring + arglists + macro? + SOURCE in one map.
   `(apropos \"pat\")` → fuzzy match table over symbol names + docstrings.

   Both read pure metadata off the SCI env; failures return a
   `:found? false` / empty-matches map, never throw."
  (:require
   [clojure.string :as str]
   [sci.core :as sci]))

(def ^:private source-chars
  "Cap on rendered source text. Roughly 750 lines of dense Clojure."
  6000)

(def ^:private apropos-limit
  "Max apropos matches returned. Keeps a doc-text scan over the whole
   sandbox from flooding the trailer."
  100)

(defn- truncate
  [s n]
  (let [s (str s)]
    (if (> (count s) n)
      (str (subs s 0 n) " ...<+" (- (count s) n) " chars>")
      s)))

(defn- coerce-symbol
  "Normalize the `(doc ...)` arg. Accepts a symbol, keyword, or string;
   anything else is stringified. Lets the model pass `'v/cat`, `:v/cat`,
   or `\"v/cat\"` interchangeably."
  [x]
  (cond
    (symbol? x)  x
    (keyword? x) (symbol (namespace x) (name x))
    (string? x)  (symbol x)
    :else        (symbol (str x))))

(defn- ns-display
  [ns-v]
  (some-> ns-v str symbol))

(defn- resolved-display
  [requested {:keys [ns name]}]
  (if (and ns name)
    (symbol (str (ns-display ns)) (str name))
    requested))

(defn- safe-meta
  [v]
  (try (meta v) (catch Throwable _ nil)))

(defn doc
  "Everything about one SCI symbol in a single map: docstring,
   arglists, macro? flag, and SOURCE. `sym` may be quoted
   (`'v/cat`), a keyword, or a string.

   Returns `{:symbol :resolved-symbol :found? :doc :arglists :macro?
   :source :source-length}`, or `{:found? false :message ...}` when the
   symbol does not resolve. Never throws."
  [sci-ctx sym]
  (let [sym (coerce-symbol sym)]
    (if-not sci-ctx
      {:symbol sym :found? false :message "No SCI context available."}
      (try
        (if-let [v (sci/resolve sci-ctx sym)]
          (let [m      (or (safe-meta v) {})
                source (:vis/source m)]
            (cond-> {:symbol          sym
                     :resolved-symbol (resolved-display sym m)
                     :found?          true
                     :doc             (:doc m)
                     :arglists        (:arglists m)
                     :macro?          (boolean (:macro m))}
              source (assoc :source        (truncate source source-chars)
                       :source-length (count source))))
          {:symbol sym :found? false :message "Symbol not found in SCI sandbox."})
        (catch Throwable t
          {:symbol sym :found? false :message (or (ex-message t) (str t))})))))

(defn- namespace-aliases
  "Reverse the SCI `:ns-aliases` map to `{ns-sym [alias ...]}` so a
   resolved symbol can be re-displayed under the alias the model
   actually types (e.g. `vis.ext.foundation/cat` → `v/cat`)."
  [sci-env]
  (reduce-kv (fn [acc alias ns-sym]
               (update acc (ns-display ns-sym) (fnil conj []) alias))
    {}
    (or (:ns-aliases sci-env) {})))

(defn- model-symbol-name
  [aliases-by-ns ns-sym sym]
  (let [ns-sym (ns-display ns-sym)]
    (cond
      (#{'clojure.core 'sandbox} ns-sym) sym
      (seq (get aliases-by-ns ns-sym))   (symbol (str (first (get aliases-by-ns ns-sym))) (str sym))
      ns-sym                             (symbol (str ns-sym) (str sym))
      :else                              sym)))

(defn apropos
  "Fuzzy search across every SCI symbol's name + docstring. `query` is
   a plain STRING (case-insensitive substring), not a regex or quoted
   symbol — so there is no `'v/rg` vs `v/rg` trap.

   Returns `{:query :count :matches [{:symbol :doc :arglists :macro?
   :has-source?} ...]}`; matches are name-sorted and capped. Never
   throws."
  [sci-ctx query]
  (let [needle  (str/lower-case (str query))
        sci-env (some-> sci-ctx :env deref)
        aliases (namespace-aliases sci-env)
        matches (->> (or (:namespaces sci-env) {})
                  (mapcat (fn [[ns-sym bindings]]
                            (for [[sym v] bindings
                                  :let [m         (or (safe-meta v) {})
                                        model-sym (model-symbol-name aliases ns-sym sym)
                                        hay       (str/lower-case (str model-sym "\n" (:doc m)))]
                                  :when (str/includes? hay needle)]
                              (cond-> {:symbol model-sym}
                                (:doc m)        (assoc :doc (:doc m))
                                (:arglists m)   (assoc :arglists (:arglists m))
                                (:macro m)      (assoc :macro? true)
                                (:vis/source m) (assoc :has-source? true)))))
                  (sort-by (comp str :symbol))
                  (take apropos-limit)
                  vec)]
    {:query   (str query)
     :count   (count matches)
     :matches matches}))

(defn build-symbol-bindings
  "Bare sandbox bindings `{'doc fn 'apropos fn}`. `env-thunk` is a
   0-arity fn returning the live env map (so `:sci-ctx`, built AFTER
   these bindings, is dereferenced at call time, not capture time)."
  [env-thunk]
  {'doc      (fn doc-call [sym] (doc (:sci-ctx (env-thunk)) sym))
   'apropos  (fn apropos-call [query] (apropos (:sci-ctx (env-thunk)) query))})
