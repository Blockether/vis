(ns com.blockether.vis.ext.language-clojure.outline
  "Collapsed file view for Clojure source — token-cheap exploration.

   Surface: `(outline path)` -> structured map.

   Strategy: parse with rewrite-clj, walk top-level nodes, classify
   each form by its head symbol, extract:
     * `:ns` form              -> name + require/import counts
     * `def*` family           -> {:kind :name :line :arglists :doc :private?}
     * comments / blanks       -> skipped

   The MODEL receives names, arglists, the first non-blank docstring
   line, and source line numbers. Bodies are elided so a 2 KLOC file
   collapses to ~30 lines of catalog. For deep dives the caller falls
   back to `v/cat` with the line number from outline."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(def ^:private def-heads
  ;; Head sym -> normalized kind. Covers the conventional def family
  ;; plus protocol / record / multi machinery so the outline is
  ;; faithful to real-world Clojure files.
  {'def           :def
   'defonce       :def
   'defn          :defn
   'defn-         :defn
   'defmacro      :defmacro
   'defmulti      :defmulti
   'defmethod     :defmethod
   'defprotocol   :defprotocol
   'definterface  :definterface
   'defrecord     :defrecord
   'deftype       :deftype
   's/def         :spec
   'malli.core/=> :spec})

(defn- first-doc-line
  "First non-blank line of a docstring, trimmed and bounded to 160
   chars so an outline entry never explodes."
  [s]
  (when (string? s)
    (let [line (->> (str/split-lines s)
                 (map str/trim)
                 (remove str/blank?)
                 first)]
      (when line
        (if (> (count line) 160)
          (str (subs line 0 157) "...")
          line)))))

(defn- node->sexpr-safe
  "rewrite-clj `sexpr` can throw on uneval / reader-conditional
   shapes we don't care about. Wrap and return nil on failure so the
   walk keeps going."
  [node]
  (try (n/sexpr node) (catch Throwable _ nil)))

(defn- arglists-of
  "Best-effort `:arglists` for a top-level form. Recognizes:
     (defn name doc? attr? [args] ...)
     (defn name doc? attr? ([args] ...) ([args] ...))
     (defmacro ...)  - same shape as defn
   Returns a vec of vec-of-arg-symbol-strings, or nil when we can't
   tell. We render symbols only — destructuring forms collapse to
   their head symbol so the catalog stays readable."
  [form-sexpr]
  (when (sequential? form-sexpr)
    (let [tail (drop 2 form-sexpr) ;; skip head + name
          tail (cond->> tail
                 (string? (first tail)) (drop 1)    ;; docstring
                 (and (seq tail) (map? (first tail))) (drop 1)) ;; attr-map
          render-args (fn [args]
                        (when (vector? args)
                          (mapv (fn [a]
                                  (cond
                                    (symbol? a) (str a)
                                    (vector? a) "[...]"
                                    (map? a)    "{...}"
                                    :else       (pr-str a)))
                            args)))]
      (cond
        (vector? (first tail))
        [(render-args (first tail))]

        (and (sequential? (first tail)) (vector? (ffirst tail)))
        (vec (keep #(render-args (first %)) tail))

        :else nil))))

(defn- entry-for
  "Build one outline entry for a top-level node. Returns nil when
   the form isn't classifiable."
  [zloc]
  (let [pos       (z/position zloc)
        line      (first pos)
        node      (z/node zloc)
        sexpr     (node->sexpr-safe node)
        head      (when (sequential? sexpr) (first sexpr))
        kind      (get def-heads head)]
    (cond
      ;; Plain `(ns ...)` form is special-cased by the caller.
      (and (sequential? sexpr) (= 'ns head))
      {:kind :ns
       :name (some-> sexpr second str)
       :line line
       :doc  (first-doc-line (nth sexpr 2 nil))}

      (and kind (sequential? sexpr) (>= (count sexpr) 2))
      (let [nm        (nth sexpr 1 nil)
            dispatch  (when (= :defmethod kind) (nth sexpr 2 nil))
            tail      (drop (if (= :defmethod kind) 3 2) sexpr)
            doc       (when (string? (first tail)) (first tail))
            attr      (when (map? (first tail)) (first tail))
            doc       (or doc (:doc attr))
            private?  (or (= 'defn- head)
                        (true? (:private (meta nm)))
                        (true? (:private attr)))
            args      (case kind
                        (:defn :defmacro :defmethod) (arglists-of sexpr)
                        nil)]
        (cond-> {:kind     kind
                 :name     (some-> nm str)
                 :line     line
                 :private? (boolean private?)}
          dispatch  (assoc :dispatch (pr-str dispatch))
          args      (assoc :arglists args)
          doc       (assoc :doc (first-doc-line doc))))

      :else nil)))

(defn outline-string
  "Outline a Clojure source string. Returns:
     {:ns     {:name :line :doc} | nil
      :counts {:defn N :def N ...}
      :forms  [<entry> ...]
      :total  N}"
  [^String source]
  (let [zloc (try (z/of-string source {:track-position? true})
               (catch Throwable _ nil))]
    (if-not zloc
      {:ns nil :counts {} :forms [] :total 0 :error "parse-failed"}
      (loop [z   zloc
             ns* nil
             out []]
        (cond
          (nil? z) {:ns ns*
                    :counts (frequencies (map :kind out))
                    :forms (vec out)
                    :total (count out)}

          :else
          (let [entry (entry-for z)
                nxt   (z/right z)]
            (cond
              (and entry (= :ns (:kind entry)))
              (recur nxt entry out)

              entry
              (recur nxt ns* (conj out entry))

              :else
              (recur nxt ns* out))))))))

(defn outline-file
  "Outline the file at `path` (workspace-relative or absolute).
   Returns the same shape as `outline-string` plus `:path` and
   `:bytes`. Missing / unreadable files surface as an `:error`."
  [workspace-root path]
  (let [f (if (.isAbsolute (io/file path))
            (io/file path)
            (io/file workspace-root path))]
    (if-not (and (.exists f) (.isFile f) (.canRead f))
      {:path (.getPath f) :error "not-readable" :forms [] :total 0}
      (let [src   (slurp f)
            base  (outline-string src)]
        (assoc base
          :path  (.getPath f)
          :bytes (count src))))))
