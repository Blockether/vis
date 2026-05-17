(ns com.blockether.vis.ext.foundation.editing.core
  "Filesystem tools exposed under the `v/` alias in the SCI sandbox.

   Two layers:

   1. Structured helpers for read / tree / search:

        (v/cat path)            ; -> {:path :lines [[N text]…] :next-offset N? :truncated? B}
        (v/cat path n)          ; first n lines from line 1
        (v/cat path offset n)   ; n lines starting at line `offset` (1-based)
        (v/cat path :tail)      ; last 400 lines (tail)
        (v/cat path :tail n)    ; last n lines
        (v/ls path)             ; -> nested {:name :path :type :size :children} tree
        (v/ls path opts)        ; opts is {:depth :hidden? :respect-gitignore?}
        (v/rg spec)            ; -> {:hits :truncated-by}; spec = {:any [literal] :paths [src]}
                               ; OR {:all [lit1 lit2]}; no regex/query+opts shorthand

   2. Cwd-safe wrappers over the babashka.fs file API. `v/patch` is
      the canonical text edit surface:

        (v/cat path)
        (v/patch [{:path path :search old :replace new}])
        (v/create-dirs path)
        (v/copy src dest)
        (v/move src dest)
        (v/delete path)
        (v/delete-if-exists path)
        (v/exists? path)
        (v/cwd)
        (v/parent path)
        (v/file-name path)
        (v/extension path)
        (v/relativize from to)

   Hard guard: every path must stay inside the conversation's working
   directory (`fs/cwd`); `..` traversal is rejected before any I/O."
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.foundation.editing.patch :as patch]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.workspace :as workspace])
  (:import
   (java.io File)
   (org.eclipse.jgit.ignore IgnoreNode IgnoreNode$MatchResult)))

;; =============================================================================
;; Tunables
;; =============================================================================

(def ^:private default-grep-limit 50)
(def ^:private default-list-depth 5)
(def ^:private render-preview-chars 3000)

;; v/cat pagination contract:
;;   `default-cat-limit`     - lines per window when the model omits `n`.
;;   `max-cat-window-bytes`  - hard ceiling on a single window's bytes.
;;                             Doubles as the persistence-blob ceiling:
;;                             each call writes one Nippy blob to
;;                             `definition_state.result`, bounded by this.
;;                             Not user-tunable; it is the storage contract.
(def ^:private default-cat-limit 400)
(def ^:private max-cat-window-bytes 65536)

;; =============================================================================
;; Path safety
;; =============================================================================

(defn- safe-path
  ^File [p]
  ;; Resolve `p` against `(fs/cwd)` and reject any traversal that escapes
  ;; the working directory.
  (let [cwd (workspace/cwd)
        resolved (.toAbsolutePath (fs/path cwd (str p)))
        normalized (.normalize resolved)
        cwd-norm (.normalize (.toAbsolutePath (fs/path cwd)))]
    (when-not (.startsWith normalized cwd-norm)
      (throw (ex-info (str "Path '" p "' escapes the working directory")
               {:type :ext.foundation.editing/path-escape :path (str p)})))
    (.toFile normalized)))

(defn- ensure-existing-file! [^File f]
  (when-not (.exists f)
    (throw (ex-info (str "File not found: " (.getPath f))
             {:type :ext.foundation.editing/file-not-found :path (.getPath f)})))
  (when (.isDirectory f)
    (throw (ex-info (str "Path is a directory, not a file: " (.getPath f))
             {:type :ext.foundation.editing/path-is-dir :path (.getPath f)})))
  f)

(defn- rel-path [^File f]
  (let [cwd (.toAbsolutePath (fs/path (workspace/cwd)))
        p   (.toAbsolutePath (.toPath f))
        rel (str (.relativize cwd p))]
    (if (str/blank? rel) "." rel)))

(defn- ensure-parent-dirs! [^File f]
  (when-let [parent (.getParentFile f)]
    (.mkdirs parent))
  f)

(defn- now-ms []
  (System/currentTimeMillis))

(defn- path->target
  [requested kind]
  (try
    (let [f (safe-path requested)]
      {:requested (str requested)
       :resolved  (rel-path f)
       :absolute  (.getPath f)
       :kind      kind})
    (catch Throwable _
      {:requested (str requested)
       :resolved  nil
       :absolute  nil
       :kind      kind})))

;; Engine contract lives in `com.blockether.vis.internal.extension`:
;;   `extension/op-tag`          - canonical op-keyword -> :op.tag/... value.
;;   `extension/op-presentation` - `:info` metadata `{:tag ...}` embedded in tool envelopes.
;; The iteration loop's final-answer gate rejects any registered extension op
;; in the same iteration as `(done ...)`; op tags remain mandatory for
;; audit/permission policy.
;; Editing used to keep its own copies; they were thin shims and crossed
;; the abstraction boundary (color-role lived here too). Use the engine
;; functions directly.

(doseq [[op tag] [[:v/cat :op.tag/observation]
                  [:v/ls :op.tag/observation]
                  [:v/rg :op.tag/observation]
                  [:v/patch-check :op.tag/observation]
                  [:v/exists? :op.tag/observation]
                  [:v/patch :op.tag/mutation]
                  [:v/create-dirs :op.tag/mutation]
                  [:v/copy :op.tag/mutation]
                  [:v/move :op.tag/mutation]
                  [:v/delete :op.tag/mutation]
                  [:v/delete-if-exists :op.tag/mutation]]]
  (extension/register-op! op {:tag tag}))

(defn- tool-success
  [{:keys [op path kind result info]}]
  (let [t (now-ms)]
    (extension/success
      {:result   result
       :op       op
       :metadata (merge {:target         (path->target path kind)
                         :started-at-ms  t
                         :finished-at-ms t
                         :duration-ms    0}
                   info)})))

(defn- tool-failure-on-error
  [op kind _render-fn]
  (fn [err _env _f args]
    (let [path         (first args)
          target       (path->target path kind)
          interrupted? (instance? InterruptedException err)
          t            (now-ms)
          error        (when interrupted?
                         {:message (str (name op)
                                     " interrupted while running; operation was cancelled.")})]
      {:result (extension/failure
                 {:result    nil
                  :op        op
                  :metadata  (cond-> {:target         target
                                      :started-at-ms  t
                                      :finished-at-ms t
                                      :duration-ms    0}
                               interrupted?
                               (assoc :interrupted? true
                                 :status :interrupted))
                  :error     error
                  :throwable (when-not error err)})})))

;; =============================================================================
;; .gitignore (cheap, lazy)
;; =============================================================================

(defn- load-ignore-node ^IgnoreNode [^File root]
  (let [gi (io/file root ".gitignore")]
    (when (.exists gi)
      (let [n (IgnoreNode.)]
        (with-open [in (io/input-stream gi)]
          (.parse n in))
        n))))

(defn- ignored? [^IgnoreNode node ^File f ^File root]
  (when node
    (let [rel (str (.relativize (.toPath root) (.toPath f)))
          dir? (.isDirectory f)
          result (.isIgnored node rel dir?)]
      (= IgnoreNode$MatchResult/IGNORED result))))

;; =============================================================================
;; cat
;; =============================================================================

(defn- validate-cat-args!
  [offset n]
  (when-not (and (integer? offset) (pos? offset))
    (throw (ex-info "v/cat offset must be a positive integer (1-based line number)."
             {:type :ext.foundation.editing/invalid-cat-args
              :offset offset})))
  (when-not (and (integer? n) (pos? n))
    (throw (ex-info "v/cat limit must be a positive integer line count."
             {:type :ext.foundation.editing/invalid-cat-args
              :limit n}))))

(defn- read-file
  "Read a window of a text file as pure structured data.

   Arities:
     (read-file path)              ; first `default-cat-limit` lines from line 1
     (read-file path n)            ; first n lines from line 1
     (read-file path offset n)     ; n lines starting at line `offset` (1-based)

   Returns {:path :lines [[N text]...] :next-offset N? :truncated? B}.
   `:lines` is a vec of `[line-number, text]` tuples — line number first so
   the model destructures `[ln t]` without offset arithmetic.
   `:next-offset` is nil at EOF, integer otherwise.
   `:truncated?` is true when the byte cap (64KB) chopped the window short;
   model paginates with `:next-offset` regardless.
   Each call's `:lines` payload is bounded by `max-cat-window-bytes`; that
   is also the persistence-blob ceiling (one Nippy row per call).
   Streaming: never slurps the whole file. Lines outside the window are
   discarded after a single `.readLine` pass."
  ([path] (read-file path 1 default-cat-limit))
  ([path n] (read-file path 1 n))
  ([path offset n]
   (validate-cat-args! offset n)
   (let [f        (ensure-existing-file! (safe-path path))
         byte-cap (long max-cat-window-bytes)
         skip     (dec (long offset))
         limit    (long n)]
     (with-open [^java.io.BufferedReader rdr (io/reader f)]
       (loop [skipped 0]
         (when (and (< skipped skip)
                 (some? (.readLine rdr)))
           (recur (inc skipped))))
       (loop [acc        (transient [])
              bytes-used 0
              read-count 0
              stop       nil]
         (cond
           stop
           (let [lines       (persistent! acc)
                 returned    (count lines)
                 eof?        (= stop :eof)
                 next-offset (when-not eof?
                               (+ (long offset) returned))]
             {:path        (rel-path f)
              :lines       lines
              :next-offset next-offset
              :truncated?  (= stop :bytes)})

           (>= read-count limit)
           (recur acc bytes-used read-count :limit)

           :else
           (let [line (.readLine rdr)]
             (if (nil? line)
               (recur acc bytes-used read-count :eof)
               (let [^String s line
                     line-bytes (+ 1 (alength (.getBytes s "UTF-8")))
                     new-bytes  (+ bytes-used line-bytes)
                     line-no    (+ (long offset) read-count)]
                 (if (and (pos? read-count) (> new-bytes byte-cap))
                   (recur acc bytes-used read-count :bytes)
                   (recur (conj! acc [line-no s])
                     new-bytes
                     (inc read-count)
                     nil)))))))))))

(defn- tail-file
  "Read the last n lines of a text file. Streams once via a fixed-size
   ring buffer (`java.util.ArrayDeque`), so memory stays bounded even for
   gigantic logs. After the scan, walks the kept window from the END to
   the start to honour `max-cat-window-bytes` — tail = most recent, so the
   byte cap fires by dropping older lines, not newer ones.

   Returns the same shape as `read-file`:
     {:path :lines [[N text]…] :next-offset nil :truncated? B}
   `:next-offset` is always nil — tail is a terminal request. `:truncated?`
   is true only when the byte cap dropped lines that would otherwise have
   fit inside the requested n; trimming older lines beyond n is the
   requested behaviour, not a truncation event."
  [path n]
  (when-not (pos-int? n)
    (throw (ex-info "tail n must be a positive integer"
             {:type :ext.foundation.editing/invalid-cat-args :limit n})))
  (let [n        (long n)
        f        (ensure-existing-file! (safe-path path))
        byte-cap (long max-cat-window-bytes)
        buf      (java.util.ArrayDeque.)]
    (with-open [^java.io.BufferedReader rdr (io/reader f)]
      (loop [total 0]
        (let [line (.readLine rdr)]
          (if (nil? line)
            (let [kept     (vec (.toArray buf))
                  kept-cnt (count kept)
                  start    (inc (- (long total) kept-cnt))
                  ;; Walk kept from the END backwards, accumulating until
                  ;; byte cap. Anything we drop off the front bumps
                  ;; truncated?.
                  [final-lines bytes-truncated?]
                  (loop [i          (dec kept-cnt)
                         bytes-used 0
                         acc        ()]
                    (if (neg? i)
                      [(vec acc) false]
                      (let [^String s (nth kept i)
                            lb        (+ 1 (alength (.getBytes s "UTF-8")))
                            nb        (+ bytes-used lb)]
                        (if (and (seq acc) (> nb byte-cap))
                          [(vec acc) true]
                          (recur (dec i) nb (cons s acc))))))
                  start-line   (+ (long start) (- kept-cnt (count final-lines)))
                  numbered     (mapv vector (iterate inc start-line) final-lines)]
              {:path        (rel-path f)
               :lines       numbered
               :next-offset nil
               :truncated?  bytes-truncated?})
            (do
              (when (>= (.size buf) n) (.removeFirst buf))
              (.addLast buf line)
              (recur (inc total)))))))))

;; =============================================================================
;; ls
;; =============================================================================

(defn- file->entry [^File f]
  {:name (.getName f)
   :path (rel-path f)
   :absolute-path (.getAbsolutePath f)
   :type (if (.isDirectory f) :dir :file)
   :size (if (.isDirectory f) nil (.length f))
   :hidden? (.isHidden f)})

(defn- visible-children [^File f {:keys [hidden? respect-gitignore? ignore-node root]}]
  (when (.isDirectory f)
    (let [kids (->> (.listFiles f)
                 (remove (fn [^File c]
                           (and (not hidden?) (.isHidden c))))
                 (remove (fn [^File c]
                           (and respect-gitignore?
                             (ignored? ignore-node c root)))))]
      (sort-by (juxt #(if (.isDirectory %) 0 1) #(.getName %)) kids))))

(defn- file->tree [^File f opts depth]
  (let [base (file->entry f)]
    (if (and (pos? depth) (.isDirectory f))
      (assoc base :children
        (mapv #(file->tree % opts (dec depth))
          (visible-children f opts)))
      base)))

(defn- list-files
  ([path] (list-files path nil))
  ([path opts]
   (let [{:keys [depth hidden? respect-gitignore?]
          :or {depth default-list-depth hidden? false respect-gitignore? true}} (or opts {})
         f (safe-path path)
         _ (when-not (.exists f)
             (throw (ex-info (str "Path not found: " (.getPath f))
                      {:type :ext.foundation.editing/path-not-found})))
         opts* {:hidden? hidden?
                :respect-gitignore? respect-gitignore?
                :ignore-node (when respect-gitignore? (load-ignore-node f))
                :root f}]
     (file->tree f opts* depth))))

;; =============================================================================
;; rg
;; =============================================================================

(defn- coerce-rg-spec
  "Coerce the single public v/rg spec map. Regex syntax is never
   interpreted. Exactly one of :all or :any is required. Every public
   collection field is a vector; no shorthand arities or scalar paths."
  [spec]
  (when-not (map? spec)
    (throw (ex-info "v/rg takes one spec map: {:all [...] :paths [...]}."
             {:type :ext.foundation.editing/invalid-rg-spec
              :got  (type spec)})))
  (let [allowed-keys #{:all :any :paths :include :exclude :hidden? :respect-gitignore?}
        unknown-keys (seq (remove allowed-keys (keys spec)))
        _ (when unknown-keys
            (throw (ex-info "v/rg spec has unknown keys."
                     {:type :ext.foundation.editing/invalid-rg-spec
                      :unknown (vec unknown-keys)})))
        has-all? (contains? spec :all)
        has-any? (contains? spec :any)
        _ (when (= has-all? has-any?)
            (throw (ex-info "v/rg spec must use exactly one of :all or :any."
                     {:type :ext.foundation.editing/invalid-rg-spec
                      :spec spec})))
        vector-of-strings (fn [k default]
                            (let [v (if (contains? spec k) (get spec k) default)]
                              (when-not (and (vector? v) (seq v) (every? string? v))
                                (throw (ex-info "v/rg spec fields must be non-empty vectors of strings."
                                         {:type :ext.foundation.editing/invalid-rg-spec
                                          :field k
                                          :got v})))
                              (when-not (every? #(not (str/blank? %)) v)
                                (throw (ex-info "v/rg spec string values must be non-blank."
                                         {:type :ext.foundation.editing/invalid-rg-spec
                                          :field k
                                          :got v})))
                              v))
        op (if has-all? :all :any)
        needles (vector-of-strings op nil)
        paths (vector-of-strings :paths ["."])
        include (when (contains? spec :include)
                  (vector-of-strings :include nil))
        exclude (when (contains? spec :exclude)
                  (vector-of-strings :exclude nil))]
    {:op op
     :needles needles
     :paths paths
     :include (or include [])
     :exclude (or exclude [])
     :hidden? (boolean (:hidden? spec))
     :respect-gitignore? (get spec :respect-gitignore? true)}))

(defn- grep-files
  "Search with the one public v/rg spec map. Exactly one of :all or
   :any is required. :paths defaults to the current directory.
   :include/:exclude are glob vectors. Acquisition has a private hard
   cap; bind the result and slice for tighter display."
  [spec]
  (let [{:keys [op needles paths include exclude hidden? respect-gitignore?]} (coerce-rg-spec spec)
        limit default-grep-limit
        glob-matcher (fn [pattern]
                       (.getPathMatcher (java.nio.file.FileSystems/getDefault)
                         (str "glob:" pattern)))
        include-matchers (mapv glob-matcher include)
        exclude-matchers (mapv glob-matcher exclude)
        match-globs? (fn [matchers ^File f]
                       (let [rel (rel-path f)
                             name (.getName f)
                             rel-path (fs/path rel)
                             name-path (fs/path name)]
                         (boolean
                           (some #(or (.matches % rel-path)
                                    (.matches % name-path))
                             matchers))))
        include-file? (fn [^File f]
                        (and (or (empty? include-matchers)
                               (match-globs? include-matchers f))
                          (not (match-globs? exclude-matchers f))))
        roots (->> paths
                (mapv (fn [p]
                        (let [f (safe-path p)]
                          (when-not (.exists f)
                            (throw (ex-info (str "Path not found: " (.getPath f))
                                     {:type :ext.foundation.editing/path-not-found
                                      :path p})))
                          (.getCanonicalFile f))))
                (sort-by (fn [^File f]
                           [(count (iterator-seq (.iterator (.toPath f))))
                            (.getPath f)]))
                (reduce (fn [acc ^File f]
                          (if (some (fn [^File parent]
                                      (.startsWith (.toPath f) (.toPath parent)))
                                acc)
                            acc
                            (conj acc f)))
                  []))
        matches-line? (fn [line]
                        (case op
                          :all (every? #(str/includes? line %) needles)
                          :any (boolean (some #(str/includes? line %) needles))))
        walk (fn walk [ignore-node root ^File f]
               (cond
                 (and (not hidden?) (.isHidden f)) []
                 (and respect-gitignore? (ignored? ignore-node f root)) []
                 (.isDirectory f) (mapcat #(walk ignore-node root %)
                                    (or (.listFiles f) (into-array File [])))
                 (and (.isFile f) (include-file? f)) [f]
                 :else []))
        files (->> roots
                (mapcat (fn [root]
                          (let [ignore-node (when respect-gitignore?
                                              (load-ignore-node root))]
                            (walk ignore-node root root))))
                (sort-by rel-path))
        hits (atom [])
        seen (atom #{})
        capped? (atom false)]
    (doseq [^File f files :while (not @capped?)]
      (try
        (with-open [r (io/reader f)]
          (loop [line-no 1
                 lines (line-seq r)]
            (when-let [line (first lines)]
              (when (matches-line? line)
                (let [hit {:path (rel-path f)
                           :line line-no
                           :text (subs line 0 (min 400 (count line)))}
                      hit-key [(:path hit) (:line hit) (:text hit)]]
                  (when-not (contains? @seen hit-key)
                    (swap! seen conj hit-key)
                    (swap! hits conj hit)
                    (when (>= (count @hits) limit)
                      (reset! capped? true)))))
              (when-not @capped?
                (recur (inc line-no) (rest lines))))))
        (catch Throwable _ nil)))
    {:hits (vec @hits)
     :truncated-by (if @capped? :internal-cap :end-of-results)}))

;; =============================================================================
;; Thin babashka.fs wrappers
;; =============================================================================

(def ^:private patch-required-keys #{:path :search :replace})

(defn- coerce-patch-edits
  [edits]
  (let [edits (if (map? edits) [edits] edits)]
    (when-not (sequential? edits)
      (throw (ex-info "v/patch expects a map or vector of edit maps"
               {:type :ext.foundation.editing/invalid-patch-edits
                :got  (type edits)})))
    (mapv (fn [edit]
            (when-not (map? edit)
              (throw (ex-info "v/patch edit must be a map"
                       {:type :ext.foundation.editing/invalid-patch-edit
                        :edit edit})))
            (let [missing (seq (remove #(contains? edit %) patch-required-keys))]
              (when missing
                (throw (ex-info "v/patch edit missing required keys"
                         {:type :ext.foundation.editing/invalid-patch-edit
                          :missing (vec missing)
                          :edit edit}))))
            (update edit :path str))
      edits)))

(defn- occurrence-count
  [^String haystack ^String needle]
  (when (str/blank? needle)
    (throw (ex-info "v/patch :search must be non-blank"
             {:type :ext.foundation.editing/invalid-patch-search})))
  (loop [idx 0 n 0]
    (let [hit (str/index-of haystack needle idx)]
      (if (nil? hit)
        n
        (recur (+ hit (count needle)) (inc n))))))

(def ^:private patch-search-preview-chars 180)

(defn- search-preview
  [s]
  (let [s (str s)]
    (if (<= (count s) patch-search-preview-chars)
      s
      (str (subs s 0 patch-search-preview-chars)
        "...<+" (- (count s) patch-search-preview-chars) " chars>"))))

(defn- patch-analysis
  [edits]
  (let [edits (coerce-patch-edits edits)]
    (loop [idx 0
           remaining edits
           states {}
           checks []
           failures []]
      (if-let [{:keys [path search replace]} (first remaining)]
        (let [file    (ensure-existing-file! (safe-path path))
              rel     (rel-path file)
              before  (or (get-in states [path :before]) (slurp file))
              current (or (get-in states [path :after]) before)
              search  (str search)
              replace (str replace)
              matches (occurrence-count current search)
              check   {:edit-index idx
                       :path rel
                       :matches matches
                       :search-preview (search-preview search)}]
          (if (= 1 matches)
            (recur (inc idx)
              (next remaining)
              (assoc states path {:file file
                                  :path rel
                                  :before before
                                  :after (str/replace-first current
                                           (re-pattern (java.util.regex.Pattern/quote search))
                                           (java.util.regex.Matcher/quoteReplacement replace))})
              (conj checks check)
              failures)
            (recur (inc idx)
              (next remaining)
              states
              (conj checks check)
              (conj failures check))))
        {:plans (vals states)
         :checks checks
         :failures failures
         :valid? (empty? failures)}))))

(defn- patch-failure-message
  [failures]
  (let [{:keys [edit-index path matches]} (first failures)]
    (if (= 1 (count failures))
      (str "v/patch edit " edit-index " failed in " path
        "; matched " matches " time(s)")
      (str "v/patch " (count failures) " edits failed; first edit " edit-index
        " in " path " matched " matches " time(s)"))))

(defn- patch-plan
  [edits]
  (let [{:keys [plans failures checks]} (patch-analysis edits)]
    (when (seq failures)
      (throw (ex-info (patch-failure-message failures)
               {:type :ext.foundation.editing/patch-search-not-unique
                :failures failures
                :checks checks})))
    plans))

(defn patch-check
  [edits]
  (let [{:keys [checks failures valid?]} (patch-analysis edits)]
    {:valid? valid?
     :checks checks
     :failures failures}))

(defn patch-safe
  [edits]
  (let [plans (vec (patch-plan edits))]
    (doseq [{:keys [file after]} plans]
      (spit file after))
    (mapv #(select-keys % [:path :before :after]) plans)))

;; =============================================================================
;; Codex `apply_patch` envelope mode
;; =============================================================================
;;
;; `(v/patch "*** Begin Patch ... *** End Patch")` accepts the OpenAI Codex
;; apply_patch grammar (Add/Update/Delete/Move) and applies all hunks
;; atomically: every hunk is validated against the live filesystem before
;; any write. This preserves Vis' all-or-nothing safety invariant; Codex
;; itself applies hunks sequentially and may leave partial writes on
;; failure.

(defn- envelope-plan-add
  [{:keys [path contents]}]
  (let [file   (safe-path path)
        rel    (rel-path file)
        exists (.exists file)]
    (when (.isDirectory file)
      (throw (ex-info (str "v/patch add target is a directory: " rel)
               {:type :ext.foundation.editing/patch-add-is-dir :path rel})))
    {:op :add
     :path rel
     :file file
     :before (when exists (slurp file))
     :after contents
     :existed? exists}))

(defn- envelope-plan-delete
  [{:keys [path]}]
  (let [file (ensure-existing-file! (safe-path path))
        rel  (rel-path file)
        before (slurp file)]
    {:op :delete
     :path rel
     :file file
     :before before
     :after nil}))

(defn- envelope-plan-update
  [{:keys [path move-to chunks]}]
  (let [file (ensure-existing-file! (safe-path path))
        rel  (rel-path file)
        before (slurp file)
        {:keys [new]} (patch/compute-update {:original before
                                             :chunks chunks
                                             :path rel})
        dest-file (when move-to (safe-path move-to))
        dest-rel  (when dest-file (rel-path dest-file))]
    {:op (if move-to :update-move :update)
     :path rel
     :file file
     :before before
     :after new
     :move-to dest-rel
     :dest-file dest-file}))

(defn- envelope-plan
  "Plan every hunk against the live filesystem. Pure with respect to
   writes — IO is read-only here, mutation happens in `envelope-commit!`."
  [hunks]
  (mapv (fn [hunk]
          (case (:op hunk)
            :add    (envelope-plan-add hunk)
            :delete (envelope-plan-delete hunk)
            :update (envelope-plan-update hunk)))
    hunks))

(defn- envelope-commit!
  [plans]
  (doseq [{:keys [op file after dest-file]} plans]
    (case op
      :add         (do (ensure-parent-dirs! file)
                     (spit file after))
      :delete      (fs/delete file)
      :update      (spit file after)
      :update-move (do (ensure-parent-dirs! dest-file)
                     (spit dest-file after)
                     (when-not (= (.getCanonicalPath file)
                                 (.getCanonicalPath dest-file))
                       (fs/delete file))))))

(defn patch-envelope-safe
  "Apply a Codex `apply_patch` envelope string. Validates every hunk
   before any write; throws ex-info on any failure with all writes
   skipped. Returns a vec of `{:op :path :before :after :move-to?}`."
  [^String patch-text]
  (let [{:keys [hunks]} (patch/parse-patch patch-text)
        _ (when (empty? hunks)
            (throw (ex-info "v/patch envelope contained no hunks"
                     {:type :ext.foundation.editing/empty-patch})))
        plans (envelope-plan hunks)]
    (envelope-commit! plans)
    (mapv (fn [{:keys [op path before after move-to]}]
            (cond-> {:op op :path path :before before :after after}
              move-to (assoc :move-to move-to)))
      plans)))

(defn patch-envelope-check
  "Plan a Codex envelope without writing. Returns the same shape as
   `patch-envelope-safe` plus `:valid?` and `:failures` for parity with
   `v/patch-check`."
  [^String patch-text]
  (try
    (let [{:keys [hunks]} (patch/parse-patch patch-text)
          plans (envelope-plan hunks)]
      {:valid? true
       :mode :codex-apply-patch
       :checks (mapv (fn [{:keys [op path move-to]}]
                       (cond-> {:op op :path path}
                         move-to (assoc :move-to move-to)))
                 plans)
       :failures []})
    (catch clojure.lang.ExceptionInfo e
      {:valid? false
       :mode :codex-apply-patch
       :checks []
       :failures [{:message (ex-message e)
                   :data (ex-data e)}]})))

(defn- create-dirs-safe [path]
  (let [f (safe-path path)]
    (fs/create-dirs f)
    (rel-path f)))

(defn- copy-safe
  ([src dest]
   (copy-safe src dest nil))
  ([src dest opts]
   (let [src-file  (safe-path src)
         dest-file (safe-path dest)]
     (ensure-parent-dirs! dest-file)
     (fs/copy src-file dest-file (or opts {}))
     (rel-path dest-file))))

(defn- move-safe
  ([src dest]
   (move-safe src dest nil))
  ([src dest opts]
   (let [src-file  (safe-path src)
         dest-file (safe-path dest)]
     (ensure-parent-dirs! dest-file)
     (fs/move src-file dest-file (or opts {}))
     (rel-path dest-file))))

(defn- delete-safe [path]
  (fs/delete (safe-path path))
  true)

(defn- delete-if-exists-safe [path]
  (fs/delete-if-exists (safe-path path)))

(defn- exists-safe? [path]
  (fs/exists? (safe-path path)))

(defn- bounded-render-text
  [s]
  (let [s (str s)]
    (if (> (count s) render-preview-chars)
      (str (subs s 0 render-preview-chars)
        "\n...<+" (- (count s) render-preview-chars) " chars>")
      s)))

;; =============================================================================
;; Tool-result facades
;; =============================================================================

(defn- cat-tool
  "Read a window of a text file. Returns a plain Clojure map:
     {:vis.op :v/cat :path P :lines [[<line> <text>]…] :next-offset N? :truncated? B}
   Arities:
     (v/cat path)               — first 400 lines from line 1.
     (v/cat path n)             — first n lines from line 1.
     (v/cat path offset n)      — n lines starting at ABSOLUTE 1-based offset.
     (v/cat path :tail)         — LAST 400 lines (tail).
     (v/cat path :tail n)       — last n lines.
   Result shape:
     {:path P :lines [[<line-number> <text>] …] :next-offset N? :truncated? B}
   `:lines` carries `[ln text]` tuples — destructure with `[n t]`; no
   offset arithmetic. Filter content with `(filter (fn [[_ t]] …) :lines)`.
   `:next-offset` is nil at EOF or for tail; integer otherwise — pass to a
   follow-up call to paginate. `:truncated?` is true when the 64KB byte
   cap chopped the window; paginate via `:next-offset` regardless."
  ([path]
   (cat-tool path 1 default-cat-limit))
  ([path arg]
   (if (= arg :tail)
     (cat-tool path :tail default-cat-limit)
     (cat-tool path 1 arg)))
  ([path arg-or-offset n]
   (let [tail? (= arg-or-offset :tail)
         out   (if tail?
                 (tail-file path n)
                 (read-file path arg-or-offset n))]
     (tool-success
       {:op :v/cat
        :path path
        :kind :file
        :result (assoc out :vis.op :v/cat)
        :info (cond-> {:next-offset (:next-offset out)
                       :truncated?  (:truncated? out)}
                tail? (assoc :tail? true))
        :presentation {:kind :source
                       :path (:path out)
                       :line-key :lines}}))))

(defn- ls-tool
  "List a directory as a plain Clojure map tree:
     {:vis.op :v/ls :path P :absolute-path AP :type :dir|:file
      :entry-count N :children [{:name :path :type :size :children} ...]}
   `:children` is a recursive vector bounded by `:depth` (default 5).
   `(v/ls path)` reads with defaults; `(v/ls path opts)` accepts
   `{:depth N :hidden? B :respect-gitignore? B}`. Children are fully
   materialised; slice via `(map :name (:children r))` or
   `(filter #(= :dir (:type %)) (:children r))`. No handles; the whole
   tree is a plain map."
  ([path]
   (ls-tool path nil))
  ([path opts]
   (let [tree (list-files path opts)
         entry-count (count (or (:children tree) []))
         result (-> tree
                  (assoc :vis.op :v/ls)
                  (assoc :entry-count entry-count))]
     (tool-success
       {:op :v/ls
        :path path
        :kind :dir
        :result result
        :info {:depth (:depth opts)
               :hidden? (:hidden? opts)
               :respect-gitignore? (get opts :respect-gitignore? true)
               :entry-count entry-count}
        :presentation {:kind :tree}}))))

(defn- rg-tool
  "Literal file-content search. Returns a plain Clojure map:
     {:vis.op :v/rg :hit-count N :truncated-by K :first-hit \"path:line\"
      :spec SPEC :paths [...] :hits [{:path :line :text} ...]}
   Use `(v/rg {:any [\"foo\" \"bar\"] :paths [\"src\"] :include [\"**/*.clj\"]})`
   for OR, or `(v/rg {:all [\"defn\" \"handler\"] :paths [\"src\"]})` for
   same-line AND. Exactly one of :all/:any. Strings are literal substrings
   (`|` is a pipe character, not regex alternation). No positional shorthand.
   :paths defaults to [\".\"]. All collection fields are vectors. Optional
   filters: :include / :exclude glob vectors, :hidden?, :respect-gitignore?.
   Unknown keys throw. For pure path discovery without content matching,
   use a vacuous spec like `(v/rg {:any [\"\"] :include [\"**/*.clj\"]})` or `v/ls`."
  ([spec]
   (let [{:keys [paths include exclude] :as coerced} (coerce-rg-spec spec)
         out (grep-files spec)
         hits (vec (:hits out))
         hit-count (count hits)
         first-hit (when (pos? hit-count)
                     (let [{:keys [path line]} (nth hits 0)]
                       (str path ":" line)))]
     (tool-success
       {:op :v/rg
        :path (if (= 1 (count paths))
                (first paths)
                ".")
        :kind :dir
        :result {:vis.op       :v/rg
                 :hit-count    hit-count
                 :truncated-by (:truncated-by out)
                 :first-hit    first-hit
                 :spec         spec
                 :paths        paths
                 :hits         hits}
        :info {:spec spec
               :query-op (:op coerced)
               :paths paths
               :include include
               :exclude exclude
               :hit-count hit-count
               :truncated-by (:truncated-by out)}
        :presentation {:kind :search-hits
                       :row-keys [:path :line :text]}})))
  ([_spec _opts]
   (throw (ex-info "v/rg takes exactly one spec map: {:all [\"literal\"] :paths [\"src\"] :include [\"**/*.clj\"]}. Use :any for OR. No query+opts shorthand."
            {:type :ext.foundation.editing/invalid-rg-arity
             :expected '([spec-map])}))))

(defn- diff-line-ops
  "LCS-based line ops between two line vectors. Returns a seq of
   `[:eq|:add|:del line]`. O(m*n) DP — fine for typical edits; the
   caller is expected to gate by size."
  [a b]
  (let [a (vec a) b (vec b)
        m (count a) n (count b)
        dp (make-array Long/TYPE (inc m) (inc n))]
    (dotimes [i m]
      (dotimes [j n]
        (aset dp (inc i) (inc j)
          (long (if (= (a i) (b j))
                  (inc (aget dp i j))
                  (max (aget dp i (inc j))
                    (aget dp (inc i) j)))))))
    (loop [i m j n ops ()]
      (cond
        (and (pos? i) (pos? j) (= (a (dec i)) (b (dec j))))
        (recur (dec i) (dec j) (cons [:eq (a (dec i))] ops))
        (and (pos? j)
          (or (zero? i) (>= (aget dp i (dec j)) (aget dp (dec i) j))))
        (recur i (dec j) (cons [:add (b (dec j))] ops))
        (pos? i)
        (recur (dec i) j (cons [:del (a (dec i))] ops))
        :else ops))))

(def ^:private patch-diff-max-lines 4000)

(defn- unified-diff-text
  "Compact unified-ish diff for two file blobs. Returns nil when both
   sides are equal. For oversized inputs returns a summary line so the
   channel preview stays bounded."
  [before after]
  (cond
    (= before after) nil
    (nil? before) (str "+++ (new file, "
                    (count (str/split-lines (or after ""))) " lines)")
    (nil? after)  (str "--- (deleted, "
                    (count (str/split-lines (or before ""))) " lines)")
    :else
    (let [a (str/split-lines before)
          b (str/split-lines after)]
      (if (or (> (count a) patch-diff-max-lines)
            (> (count b) patch-diff-max-lines))
        (str "(diff too large to render inline: "
          (count a) " -> " (count b) " lines)")
        (str/join "\n"
          (map (fn [[kind line]]
                 (case kind
                   :eq  (str "  " line)
                   :add (str "+ " line)
                   :del (str "- " line)))
            (diff-line-ops a b)))))))

(defn- patch-result-file-summary
  "Build a per-file summary map that lives on `:result` of `v/patch`.
   Keeps the model trailer compact (no raw before/after) while carrying
   enough info — path, op, line deltas, capped unified diff — for the
   channel renderer to paint a useful preview."
  [{:keys [op path before after move-to]}]
  (let [lines-before (if before (count (str/split-lines before)) 0)
        lines-after  (if after  (count (str/split-lines after))  0)
        diff-text    (unified-diff-text before after)]
    (cond-> {:path         path
             :op           (or op :update)
             :changed?     (not= before after)
             :lines-before lines-before
             :lines-after  lines-after
             :delta-lines  (- lines-after lines-before)}
      move-to   (assoc :move-to move-to)
      diff-text (assoc :diff diff-text))))

(defn- patch-tool
  "Edit files. Accepts two input shapes:

   1. Codex `apply_patch` envelope (string):
        (v/patch \"*** Begin Patch\\n*** Update File: src/foo.clj\\n@@\\n-old\\n+new\\n*** End Patch\\n\")
      Supports Add/Update/Delete/Move with `@@` context headers and
      ` `/`+`/`-` line prefixes. Fuzzy line match (exact -> rstrip ->
      trim -> Unicode-normalize) mirrors Codex' matcher.

   2. Exact-replace vec (canonical):
        (v/patch [{:path :search :replace}])
      Every `:search` must match exactly once in the current file.

   Both modes validate the full plan against the live filesystem
   before any write — a single failure aborts the entire batch and no
   file is touched."
  [edits]
  (cond
    (patch/looks-like-patch? edits)
    (let [plans     (patch-envelope-safe edits)
          summaries (mapv patch-result-file-summary plans)]
      (tool-success
        {:op :v/patch
         :path (or (:path (first plans)) ".")
         :kind :file
         :result summaries
         :info  {:mode          :codex-apply-patch
                 :file-count    (count summaries)
                 :changed-count (count (filter :changed? summaries))}}))

    :else
    (let [plans     (patch-safe edits)
          summaries (mapv patch-result-file-summary plans)]
      (tool-success
        {:op :v/patch
         :path (or (:path (first plans)) ".")
         :kind :file
         :result summaries
         :info  {:mode          :exact-replace
                 :file-count    (count summaries)
                 :changed-count (count (filter :changed? summaries))}}))))

(defn- patch-check-tool
  "Preflight a patch without writing. Accepts the same two input shapes
   as v/patch (Codex envelope string OR exact-replace vec). For exact
   mode returns match counts per edit; for envelope mode returns
   per-hunk validity plus parse/path errors."
  [edits]
  (cond
    (patch/looks-like-patch? edits)
    (let [out (patch-envelope-check edits)]
      (tool-success
        {:op :v/patch-check
         :path (or (:path (first (:checks out))) ".")
         :kind :file
         :result out
         :info {:mode :codex-apply-patch
                :valid? (:valid? out)
                :edit-count (count (:checks out))
                :failure-count (count (:failures out))}}))

    :else
    (let [out (patch-check edits)]
      (tool-success
        {:op :v/patch-check
         :path (or (:path (first (:checks out))) ".")
         :kind :file
         :result out
         :info {:mode :exact-replace
                :valid? (:valid? out)
                :edit-count (count (:checks out))
                :failure-count (count (:failures out))}}))))

(defn- create-dirs-tool
  "Ensure dir exists. Tool result."
  [path]
  (let [before (fs/exists? (safe-path path))
        out    (create-dirs-safe path)]
    (tool-success
      {:op :v/create-dirs
       :path path
       :kind :dir
       :result out
       :info {:created? (not before)
              :already-existed? before}})))

(defn- copy-tool
  "Copy path. Tool result."
  ([src dest]
   (copy-tool src dest nil))
  ([src dest opts]
   (let [out (copy-safe src dest opts)]
     (tool-success
       {:op :v/copy
        :path dest
        :kind :path
        :result out
        :info {:src (path->target src :path)
               :dest (path->target dest :path)
               :opts opts}}))))

(defn- move-tool
  "Move/rename path. Tool result."
  ([src dest]
   (move-tool src dest nil))
  ([src dest opts]
   (let [out (move-safe src dest opts)]
     (tool-success
       {:op :v/move
        :path dest
        :kind :path
        :result out
        :info {:src (path->target src :path)
               :dest (path->target dest :path)
               :opts opts}}))))

(defn- delete-tool
  "Delete path. Tool result."
  [path]
  (delete-safe path)
  (tool-success
    {:op :v/delete
     :path path
     :kind :path
     :result nil
     :info {:deleted? true}}))

(defn- delete-if-exists-tool
  "Delete path if present. Tool result."
  [path]
  (let [deleted? (delete-if-exists-safe path)]
    (tool-success
      {:op :v/delete-if-exists
       :path path
       :kind :path
       :result deleted?
       :info {:deleted? deleted?}})))

(defn- exists-tool
  "Existence check. Returns boolean."
  [path]
  (let [exists? (exists-safe? path)]
    (tool-success
      {:op :v/exists?
       :path path
       :kind :path
       :result exists?
       :info {:exists? exists?}})))

;; =============================================================================
;; Structured renderers
;; =============================================================================

;; Channel IR builders. No Markdown string round-trip on tool display.
(defn- ir-code [s] [:c {} (str s)])
(defn- ir-code-block [lang body] [:code (cond-> {} lang (assoc :lang lang)) (str body)])
(defn- ir-inline [x] (if (vector? x) x [:span {} (str x)]))
(defn- ir-p [& parts]
  (into [:p {}]
    (map ir-inline (filter some? parts))))
(defn- ir-root [& blocks]
  (into [:ir {}] (filter some? blocks)))

(defn- render-edn-block
  [value]
  (ir-root (ir-code-block "edn" (bounded-render-text (pr-str value)))))

(defn- tree-entry-line
  [depth {:keys [name path type size] :as entry}]
  (let [indent (apply str (repeat depth "  "))
        label  (or name path (pr-str (dissoc entry :children)))]
    (str indent "- " label
      (when type (str " (" (clojure.core/name type) ")"))
      (when size (str " " size "B")))))

(defn- tree-lines
  ([entry] (tree-lines 0 entry))
  ([depth entry]
   (cond
     (map? entry)
     (cons (tree-entry-line depth entry)
       (mapcat #(tree-lines (inc depth) %) (:children entry)))

     (sequential? entry)
     (mapcat #(tree-lines depth %) entry)

     :else
     [(str (apply str (repeat depth "  ")) "- " (pr-str entry))])))

(defn- numbered-line-block
  "Format a vec of `[line-number, text]` tuples as `<ln>: <text>` lines."
  [tuples]
  (->> tuples
    (map (fn [[ln s]] (str ln ": " s)))
    (str/join "\n")))

;; ---------------------------------------------------------------------------
;; Per-symbol renderers
;;
;; Engine contract:
;;   render-fn -> (fn [result] [:ir ...])
;;
;; `result` is the raw payload returned to SCI. Engine handles
;; `:success? false` separately — error fns are optional and fall back to
;; `default-error-ir`. The MODEL surface is the per-iteration trailer
;; (real SCI values via pr-str); there is no second model-side render.
;; ---------------------------------------------------------------------------

(defn- channel-render-cat
  "Channel preview: numbered-line-block + header. Reads the plain map
   directly; no handle/deref."
  [{:keys [path next-offset truncated? lines]}]
  (let [lines      (vec lines)
        line-count (count lines)
        first-ln   (ffirst lines)
        body       (numbered-line-block lines)]
    (ir-root
      (ir-p "Read " (ir-code path) " — " line-count
        " line(s)" (when first-ln (str " from line " first-ln))
        (cond
          next-offset (str " (next-offset " next-offset ""
                        (when truncated? ", byte-cap hit")
                        ").")
          truncated?  " (byte-cap hit)."
          :else       " (eof)."))
      (ir-code-block "text" (bounded-render-text body)))))

(defn- channel-render-ls
  "Channel preview: pretty tree from the plain map."
  [tree]
  (ir-root
    (ir-p "Directory tree of " (ir-code (:path tree)) " — "
      (or (:entry-count tree) (count (:children tree))) " top-level entries.")
    (ir-code-block "text"
      (bounded-render-text (str/join "\n" (tree-lines tree))))))

(defn- channel-render-rg
  "Channel preview: full hit list from the plain map."
  [{:keys [hits truncated-by]}]
  (ir-root
    (ir-p "Searched — " (count hits) " hit(s), truncated-by "
      (ir-code (name (or truncated-by :none))) ".")
    (when (seq hits)
      (ir-code-block "text"
        (bounded-render-text
          (str/join "\n"
            (map (fn [{:keys [path line text]}]
                   (str path ":" line " " text)) hits)))))))

(defn- channel-render-patch
  "Channel preview: one header line + per-file stats and (capped) unified
   diff. All the diff data lives on `:result` itself (see
   `patch-result-file-summary`) so this renderer is a pure projection."
  [result]
  (let [files   (if (sequential? result) result [result])
        changed (count (filter :changed? files))]
    (apply ir-root
      (ir-p "Patched " (count files) " file(s)"
        (when (pos? changed) (str ", " changed " changed")) ".")
      (mapcat
        (fn [{:keys [path op lines-before lines-after delta-lines diff move-to changed?]}]
          (let [delta-str (cond
                            (pos? delta-lines) (str " +" delta-lines)
                            (neg? delta-lines) (str " " delta-lines)
                            :else              "")
                header    (str (name (or op :update))
                            " " path
                            (when move-to (str " -> " move-to))
                            " [" (or lines-before 0) " -> " (or lines-after 0)
                            " lines" delta-str "]"
                            (when (false? changed?) " (no-op)"))]
            (cond-> [(ir-p (ir-code header))]
              diff (conj (ir-code-block "diff" (bounded-render-text diff))))))
        files))))

(defn- channel-render-patch-check
  [result]
  (render-edn-block result))

(defn- channel-render-create-dirs
  [result]
  (ir-root (ir-p "Ensured dir " (ir-code result) ".")))

(defn- channel-render-copy
  [result]
  (ir-root (ir-p "Copied to " (ir-code result) ".")))

(defn- channel-render-move
  [result]
  (ir-root (ir-p "Moved to " (ir-code result) ".")))

(defn- channel-render-delete
  [result]
  (ir-root (ir-p "Deleted. " (ir-code (pr-str result)))))

(defn- channel-render-delete-if-exists
  [result]
  (if result
    (ir-root (ir-p "Deleted."))
    (ir-root (ir-p "Already absent."))))

(defn- channel-render-exists?
  [result]
  (ir-root (ir-p "Exists? " (ir-code (pr-str result)))))

;; =============================================================================
;; Symbol declarations
;; =============================================================================

;; -----------------------------------------------------------------------------
;; Symbol declarations.
;;
;; Each underlying `xxx-tool` defn carries the canonical docstring + arglists
;; on its var. `vis/symbol` reads them straight from the var meta - the
;; SCI sandbox sees the same text the prompt-listing renders.
;; `:symbol` overrides the var name (`cat-tool` -> `cat`) for the model-facing
;; surface; everything else (examples, render-fn, error hook, result spec)
;; lives in opts because it has nothing to do with the function's signature.
;; -----------------------------------------------------------------------------

(def cat-symbol
  (vis/symbol #'cat-tool
    {:symbol 'cat
     :render-fn channel-render-cat
     :on-error-fn (tool-failure-on-error :v/cat :file nil)}))

(def ls-symbol
  (vis/symbol #'ls-tool
    {:symbol 'ls
     :render-fn channel-render-ls
     :on-error-fn (tool-failure-on-error :v/ls :dir nil)}))

(def rg-symbol
  (vis/symbol #'rg-tool
    {:symbol 'rg
     :render-fn channel-render-rg
     :on-error-fn (tool-failure-on-error :v/rg :dir nil)}))

(def patch-symbol
  (vis/symbol #'patch-tool
    {:symbol 'patch
     :render-fn channel-render-patch
     :on-error-fn (tool-failure-on-error :v/patch :file nil)}))

(def patch-check-symbol
  (vis/symbol #'patch-check-tool
    {:symbol 'patch-check
     :render-fn channel-render-patch-check
     :on-error-fn (tool-failure-on-error :v/patch-check :file nil)}))

(def create-dirs-symbol
  (vis/symbol #'create-dirs-tool
    {:symbol 'create-dirs
     :render-fn channel-render-create-dirs
     :on-error-fn (tool-failure-on-error :v/create-dirs :dir nil)}))

(def copy-symbol
  (vis/symbol #'copy-tool
    {:symbol 'copy
     :render-fn channel-render-copy
     :on-error-fn (tool-failure-on-error :v/copy :path nil)}))

(def move-symbol
  (vis/symbol #'move-tool
    {:symbol 'move
     :render-fn channel-render-move
     :on-error-fn (tool-failure-on-error :v/move :path nil)}))

(def delete-symbol
  (vis/symbol #'delete-tool
    {:symbol 'delete
     :render-fn channel-render-delete
     :on-error-fn (tool-failure-on-error :v/delete :path nil)}))

(def delete-if-exists-symbol
  (vis/symbol #'delete-if-exists-tool
    {:symbol 'delete-if-exists
     :render-fn channel-render-delete-if-exists
     :on-error-fn (tool-failure-on-error :v/delete-if-exists :path nil)}))

(def exists?-symbol
  (vis/symbol #'exists-tool
    {:symbol 'exists?
     :render-fn channel-render-exists?
     :on-error-fn (tool-failure-on-error :v/exists? :path nil)}))

(defn available-editing-symbols
  []
  [cat-symbol
   ls-symbol
   rg-symbol
   patch-symbol
   patch-check-symbol
   create-dirs-symbol
   copy-symbol
   move-symbol
   delete-symbol
   delete-if-exists-symbol
   exists?-symbol])

(defn available-editing-prompt
  []
  (str/join "\n"
    ["`v/` editing tools — RLM-shaped. Every tool returns a PLAIN CLOJURE MAP;"
     "destructure with :keys. The trailer shows the real value via pr-str (bounded);"
     "first lines/hits/entries); the full data lives in the bound def."
     ""
     "READ"
     "  (v/cat path)            — 400 lines from line 1."
     "  (v/cat path n)          — n lines from line 1."
     "  (v/cat path offset n)   — n lines starting at ABSOLUTE 1-based offset."
     "  (v/cat path :tail)      — last 400 lines (tail; explicit, no auto-magic)."
     "  (v/cat path :tail n)    — last n lines."
     "  result: {:vis.op :v/cat :path :lines :next-offset :truncated?}. :lines is a"
     "  vec of `[<line-number> <text>]` tuples — destructure with `[n t]`, no offset"
     "  math. Page with `(:next-offset prev)` until it is nil (EOF or tail)."
     "  :truncated? is true when the 64KB window cap fired; paginate via :next-offset."
     ""
     "  (v/ls path)             — directory tree. opts: {:depth :hidden? :respect-gitignore?}."
     "  result: {:vis.op :v/ls :path :type :entry-count :children [...]} where"
     "  each child is {:name :path :type :size :children}. Walk with"
     "  `(tree-seq map? :children r)`; filter with `(filter #(= :dir (:type %)) ...)`."
     ""
     "  (v/rg {:any [\"a\" \"b\"] :paths [\"src\"] :include [\"**/*.clj\"]})"
     "  (v/rg {:all [\"defn\" \"foo\"] :paths [\"src\"]})"
     "  Exactly one of :all/:any required. Literal substrings only (no regex);"
     "  `|` is a pipe character. All collection fields are vectors. Unknown keys"
     "  throw. result: {:vis.op :v/rg :hit-count :truncated-by :first-hit :spec"
     "  :paths :hits [{:path :line :text}]}."
     ""
     "EDIT"
     "  (v/patch [{:path :search :replace}])  — exact-replace; each :search must match exactly once."
     "  (v/patch \"*** Begin Patch\\n... *** End Patch\\n\")  — Codex envelope (Add/Update/Delete/Move)."
     "  Both modes validate the full plan before any write; one failure aborts the batch."
     "  v/patch returns diff + post-image — that IS the write evidence. Do NOT v/cat to verify."
     "  v/patch-check accepts the same two shapes (no writes)."
     ""
     "PATH OPS"
     "  v/create-dirs, v/copy, v/move, v/delete, v/delete-if-exists, v/exists?."
     ""
     "RULES"
     "  - Execute side effects (v/patch, v/move, v/create-dirs, etc.) in their"
     "    OWN iteration. Confirm the tool's return value, THEN call (done ...)"
     "    in the NEXT iteration. Never combine a write with (done ...) hoping"
     "    both run — only the (done) form will be observable on failure."
     "  - Defs persist across iterations. Reuse them; do not rebind identical"
     "    args."
     "  - After a write, any prior read of the same path is stale; re-read."]))

(def editing-symbols
  "Default editing symbol set for docs/tests."
  (available-editing-symbols))

(def editing-prompt
  "Default editing prompt for docs/tests."
  (available-editing-prompt))
