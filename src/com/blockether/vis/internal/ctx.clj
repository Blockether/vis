(ns com.blockether.vis.internal.ctx
  "Engine-owned `ctx` snapshot bound under sandbox name `ctx` before every
   model call.

   Four keys, every nil/blank field stripped:

     :conversation {:id :title :turn-id :user-request}
     :iteration    {:id :position}
     :tree         vector of cwd-relative file paths (depth 8, gitignore-aware,
                   directories excluded so paths read cleanly)
     :defs         {sym {:doc <string?> :shape <malli|fn-shape>}}

   `:iteration` lives at the top level so `(:iteration ctx)` works directly
   and the per-iteration scope is not buried inside the conversation.

   `:defs` is an ordered map; newest sym first. History (prior turns,
   iterations, code, errors, stdout/stderr) is reachable via foundation
   tools, not embedded in ctx.

   No previews. No raw values. Real values live in the SCI sandbox; the
   model derefs symbols when it needs them.

   Shape inference uses `malli.provider/provide` cached by
   `System/identityHashCode` of the value."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [edamame.core :as edamame]
   [com.blockether.vis.internal.workspace :as workspace]
   [malli.provider :as mp])
  (:import
   [java.io File]
   [org.eclipse.jgit.ignore IgnoreNode IgnoreNode$MatchResult]))

(def ^:private hidden-syms
  '#{ctx done set-conversation-title!})

(def ^:private tree-ignored-dir-names
  #{".git" ".hg" ".svn" ".cache" ".idea" ".vscode" ".clj-kondo"
    "target" "node_modules" ".vis" ".cpcache" ".m2" ".gradle" "dist" "build"
    ".verification" ".nrepl-port" ".pi"})

(def ^:private TREE_MAX_DEPTH 8)
(def ^:private TREE_MAX_ENTRIES 2000)

(defn- ignored-dir-name? [^File f]
  (let [n (.getName f)]
    (or (contains? tree-ignored-dir-names n)
      (and (str/starts-with? n ".") (not= "." n)))))

(defn- list-children [^File dir]
  (try (vec (.listFiles dir)) (catch Throwable _ [])))

(defn- load-ignore-node ^IgnoreNode [^File root]
  (let [gi (io/file root ".gitignore")]
    (when (.exists gi)
      (try
        (let [n (IgnoreNode.)]
          (with-open [in (io/input-stream gi)]
            (.parse n in))
          n)
        (catch Throwable _ nil)))))

(defn- gitignored? [^IgnoreNode node ^File f ^File root]
  (when node
    (try
      (let [rel    (str (.relativize (.toPath root) (.toPath f)))
            dir?   (.isDirectory f)
            result (.isIgnored node rel dir?)]
        (= IgnoreNode$MatchResult/IGNORED result))
      (catch Throwable _ false))))

(defn- walk-tree
  "Walk the workspace returning a vec of cwd-relative FILE paths.
   Directories are traversed but not included in the result (model can
   infer dir layout from file path prefixes)."
  [^File root]
  (let [out      (volatile! [])
        root-abs (.getCanonicalPath root)
        root-len (inc (count root-abs))
        gi-node  (load-ignore-node root)
        rel-of   (fn [^File f]
                   (let [p (.getCanonicalPath f)]
                     (if (> (count p) root-len) (subs p root-len) p)))]
    (letfn [(step [^File dir ^long depth]
              (when (and (< (count @out) TREE_MAX_ENTRIES)
                      (<= depth TREE_MAX_DEPTH))
                (doseq [^File child (sort-by #(.getName ^File %) (list-children dir))]
                  (when (< (count @out) TREE_MAX_ENTRIES)
                    (cond
                      (.isDirectory child)
                      (when-not (or (ignored-dir-name? child)
                                  (gitignored? gi-node child root))
                        (step child (inc depth)))

                      (.isFile child)
                      (when-not (or (str/starts-with? (.getName child) ".")
                                  (gitignored? gi-node child root))
                        (vswap! out conj (rel-of child))))))))]
      (step root 0)
      @out)))

(defn- build-tree []
  (try
    (walk-tree (workspace/cwd))
    (catch Throwable _ [])))

(defonce ^:private shape-cache-atom
  ;; { env-id {sym {:identity int :shape <schema> :order long}} }
  (atom {}))

(defonce ^:private order-counter-atom
  ;; { env-id long }
  (atom {}))

(defn- next-order!
  [env-id]
  (-> (swap! order-counter-atom update env-id (fnil inc 0))
    (get env-id)))

(defn- non-blank? [v]
  (and (string? v) (not (str/blank? v))))

(defn- prune
  "Drop keys whose value is nil, empty string, empty coll."
  [m]
  (into {} (remove (fn [[_ v]]
                     (or (nil? v)
                       (and (string? v) (str/blank? v))
                       (and (coll? v) (empty? v))))
             m)))

(defn error-shape
  "Normalize an error map / Throwable to `{:message :trace :data}`. Drops
   blank/empty fields."
  [err]
  (let [base (cond
               (map? err) {:message (or (:message err) "")
                           :trace   (:trace err)
                           :data    (:data err)}
               (instance? Throwable err)
               (let [^Throwable t err]
                 {:message (or (.getMessage t) (.. t getClass getSimpleName))
                  :data    (ex-data t)})
               :else {:message (str err)})]
    (prune base)))

(defn- provide-shape [value]
  (try (mp/provide [value])
    (catch Throwable _ nil)))

(defn value-shape
  [cache-key sym value]
  (let [id     (System/identityHashCode value)
        cached (get-in @shape-cache-atom [cache-key sym])]
    (if (= id (:identity cached))
      (:shape cached)
      (let [shape (provide-shape value)]
        (swap! shape-cache-atom assoc-in [cache-key sym]
          (merge cached {:identity id :shape shape}))
        shape))))

(defn- safe-meta [v]
  (try (meta v) (catch Throwable _ nil)))

(defn- safe-deref [v]
  (cond
    (instance? clojure.lang.IDeref v) (try @v (catch Throwable _ nil))
    :else v))

(defn- fn-shape [v]
  (let [al (some-> (safe-meta v) :arglists)]
    (cond-> {:type :fn}
      (seq al) (assoc :arglists (vec al)))))

(defn- compute-shape
  [cache-key sym v]
  (let [val (safe-deref v)]
    (cond
      (nil? val) nil
      (fn?  val) (fn-shape v)
      :else      (value-shape cache-key sym val))))

(defn- ensure-order!
  [cache-key sym]
  (or (get-in @shape-cache-atom [cache-key sym :order])
    (let [n (next-order! cache-key)]
      (swap! shape-cache-atom assoc-in [cache-key sym :order] n)
      n)))

(defn- def-entry
  [cache-key [sym v]]
  (let [order (ensure-order! cache-key sym)
        doc   (:doc (safe-meta v))
        shape (compute-shape cache-key sym v)]
    [order sym (prune {:doc   (when (non-blank? doc) doc)
                       :shape shape})]))

(defn- visible-def?
  [initial-ns-keys [sym _]]
  (and (symbol? sym)
    (not (contains? hidden-syms sym))
    (not (contains? initial-ns-keys sym))))

(defn- build-defs
  [sci-ctx initial-ns-keys]
  (when sci-ctx
    (let [cache-key (System/identityHashCode (:env sci-ctx))
          sandbox   (get-in @(:env sci-ctx) [:namespaces 'sandbox])
          entries   (into []
                      (comp
                        (filter (partial visible-def? (or initial-ns-keys #{})))
                        (map (partial def-entry cache-key)))
                      sandbox)]
      ;; newest first by insertion order, then build an array-map so
      ;; iteration preserves order on render.
      (apply array-map
        (mapcat (fn [[_ sym entry]] [sym entry])
          (sort-by first > entries))))))

(defn build
  "Build the engine ctx snapshot.

   `:conversation` — pre-pruned map `{:id :title :turn-id :user-request}`.
   `:iteration`    — pre-pruned map `{:id :position}`."
  [{:keys [environment conversation iteration]}]
  (prune
    {:conversation (prune (or conversation {}))
     :iteration    (prune (or iteration {}))
     :tree         (build-tree)
     :defs         (or (build-defs (:sci-ctx environment)
                         (:initial-ns-keys environment))
                     {})}))

(defn forget!
  "Drop the shape + order caches for a sci-ctx."
  [sci-ctx]
  (when sci-ctx
    (let [env-id (System/identityHashCode (:env sci-ctx))]
      (swap! shape-cache-atom dissoc env-id)
      (swap! order-counter-atom dissoc env-id))))

;; =============================================================================
;; REPL-style iteration outcome rendering
;;
;; Each prior iteration of the current turn is rendered as a REPL transcript
;; (code lines, per-form `;; => shape`, `;; ! stdout> ...`, `;; ! ERROR ...`).
;; The whole block forms one user-message trailer the model sees between its
;; own assistant replays, plus the fresh `ctx` snapshot at the end.
;; =============================================================================

(defn- parse-forms
  [code]
  (when (string? code)
    (try (vec (edamame/parse-string-all code {:all true}))
      (catch Throwable _ []))))

(defn- form->source
  [form]
  (try (pr-str form) (catch Throwable _ "")))

(defn- side-effect-lines
  [{:keys [stdout stderr]}]
  (cond-> []
    (non-blank? stdout)
    (into (mapv #(str ";; ! stdout> " %) (str/split-lines stdout)))
    (non-blank? stderr)
    (into (mapv #(str ";; ! stderr> " %) (str/split-lines stderr)))))

(defn- error-lines
  [err]
  (when err
    (let [{:keys [message trace data]} (error-shape err)]
      (cond-> []
        (non-blank? message) (conj (str ";; ! ERROR " message))
        (non-blank? trace)
        (into (mapv #(str ";;   " %) (str/split-lines trace)))
        (some? data)
        (conj (str ";; ! data " (pr-str data)))))))

(defn- form-result-line
  [shape]
  (when (some? shape)
    (str ";; => shape " (pr-str shape))))

(defn- iteration->repl-text
  "Render one prior iteration as a REPL transcript block."
  [cache-key {:keys [position blocks]}]
  (let [block        (first blocks)
        code         (or (:code block) "")
        forms        (parse-forms code)
        result       (when (and (not (:error block))
                             (not= :vis/no-result (:result block)))
                       (:result block))
        last-shape   (when result
                       (value-shape cache-key (str "__iter" position "__") result))
        form-blocks  (if (seq forms)
                       ;; Only the last form's value is known; intermediate
                       ;; form values would require per-form eval capture.
                       (let [head (butlast forms)
                             tail (last forms)]
                         (concat
                           (map (fn [f] [(form->source f) nil]) head)
                           [[(form->source tail) last-shape]]))
                       [[code last-shape]])
        lines        (concat
                       [(str ";; iter " position)]
                       (mapcat (fn [[src shape]]
                                 (cond-> [src]
                                   shape (conj (form-result-line shape))))
                         form-blocks)
                       (side-effect-lines block)
                       (error-lines (:error block)))]
    (str/join "\n" lines)))

(defn render-iteration-trailer
  "Build the REPL-style user-message body for a model call.

   `:journal-iters` — vec of `[position {:blocks [...]}]` ordered ascending.
   `:ctx`           — the engine ctx snapshot map for the upcoming iteration.

   Returns the full trailer string: prior iterations as REPL transcripts,
   followed by `;; ctx = <edn>` for the fresh state. When no prior iterations
   exist, only the ctx block is emitted."
  [{:keys [environment journal-iters ctx]}]
  (let [cache-key (System/identityHashCode (:env (:sci-ctx environment)))
        prior     (mapv (fn [[pos data]] (iteration->repl-text cache-key
                                           {:position pos :blocks (:blocks data)}))
                    (or journal-iters []))
        ctx-block (str ";; ctx =\n" (pr-str ctx))]
    (str/join "\n\n" (conj prior ctx-block))))
