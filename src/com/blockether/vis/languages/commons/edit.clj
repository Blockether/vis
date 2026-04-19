(ns com.blockether.vis.languages.commons.edit
  "Base EDIT tool for RLM agents.
   Applies OpenAI-style multi-file patch envelopes."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.loop.tool :as sci-tool])
  (:import [com.github.difflib DiffUtils UnifiedDiffUtils]))

;;; ── Safety ─────────────────────────────────────────────────────────────

(def ^:private max-file-size
  "Max file size to edit (10 MB)."
  (* 10 1024 1024))

(defn- canonical-file
  [path]
  (.getCanonicalFile (io/file path)))

(defn- validate-existing-file!
  [path]
  (let [f (canonical-file path)]
    (when-not (.exists f)
      (throw (ex-info (str "File not found: " path)
               {:type :tool/edit-not-found :tool 'edit-file :path path})))
    (when (.isDirectory f)
      (throw (ex-info (str "Path is a directory: " path)
               {:type :tool/invalid-input :tool 'edit-file :path path})))
    (when (java.nio.file.Files/isSymbolicLink (.toPath f))
      (throw (ex-info (str "Refusing to edit through symlink: " path)
               {:type :tool/invalid-input :tool 'edit-file :path path})))
    (let [size (.length f)]
      (when (> size max-file-size)
        (throw (ex-info (str "File too large to edit: " (quot size 1024) "KB (max " (quot max-file-size 1024) "KB)")
                 {:type :tool/invalid-input :tool 'edit-file :path path :size size :max max-file-size}))))
    f))

(defn- split-lines-vec
  [s]
  (if (str/blank? s) [] (vec (str/split-lines s))))

(defn- join-lines
  [lines]
  (str/join "\n" lines))

(defn- starts-with?
  [^String s ^String prefix]
  (.startsWith s prefix))

;;; ── Patch Parsing ──────────────────────────────────────────────────────

(defn- parse-path-line
  [line prefix]
  (let [p (subs line (count prefix))]
    (when (str/blank? p)
      (throw (ex-info "Patch section path must not be empty"
               {:type :tool/invalid-input :tool 'edit-file :line line})))
    p))

(defn- parse-hunk-op
  [line file-path hunk-index line-no]
  (when (empty? line)
    (throw (ex-info "Hunk line must start with one of: space, +, -"
             {:type :tool/invalid-input :tool 'edit-file :path file-path :hunk-index hunk-index :line-no line-no})))
  (let [ch (.charAt ^String line 0)
        txt (subs line 1)]
    (case ch
      \space [:context txt]
      \+     [:add txt]
      \-     [:remove txt]
      (throw (ex-info "Hunk line must start with one of: space, +, -"
               {:type :tool/invalid-input
                :tool 'edit-file
                :path file-path
                :hunk-index hunk-index
                :line-no line-no
                :line line})))))

(def ^:private hunk-header-re
  "Strict unified-diff style hunk header.
   Requires explicit ranges: @@ -a[,b] +c[,d] @@"
  #"^@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@.*$")

(defn- parse-hunk-header
  [header file-path hunk-index]
  (let [m (re-matches hunk-header-re header)]
    (when-not m
      (throw (ex-info "Invalid hunk header format"
               {:type :tool/invalid-input :tool 'edit-file
                :path file-path :hunk-index hunk-index :header header})))
    (let [old-start (Long/parseLong (nth m 1))
          old-count (if (nth m 2) (Long/parseLong (nth m 2)) 1)
          new-start (if (nth m 3) (Long/parseLong (nth m 3)) 1)
          new-count (if (nth m 4) (Long/parseLong (nth m 4)) 1)]
      (when (< old-start 1)
        (throw (ex-info (str "Hunk old-start must be >= 1, got " old-start)
                 {:type :tool/invalid-input :tool 'edit-file
                  :path file-path :hunk-index hunk-index :old-start old-start})))
      (when (< new-start 1)
        (throw (ex-info (str "Hunk new-start must be >= 1, got " new-start)
                 {:type :tool/invalid-input :tool 'edit-file
                  :path file-path :hunk-index hunk-index :new-start new-start})))
      {:old-start old-start :old-count old-count
       :new-start new-start :new-count new-count})))

(defn- validate-hunk-ranges!
  [ops header file-path hunk-index]
  (let [{:keys [old-count new-count]} (parse-hunk-header header file-path hunk-index)
        actual-old (count (filter #(#{:context :remove} (first %)) ops))
        actual-new (count (filter #(#{:context :add} (first %)) ops))]
    (when-not (= actual-old old-count)
      (throw (ex-info (str "Hunk old-side count mismatch: header says " old-count
                        " but body has " actual-old " context+remove lines")
               {:type :tool/invalid-input :tool 'edit-file
                :path file-path :hunk-index hunk-index
                :header-count old-count :actual-count actual-old})))
    (when-not (= actual-new new-count)
      (throw (ex-info (str "Hunk new-side count mismatch: header says " new-count
                        " but body has " actual-new " context+add lines")
               {:type :tool/invalid-input :tool 'edit-file
                :path file-path :hunk-index hunk-index
                :header-count new-count :actual-count actual-new})))))

(defn- parse-update-section
  [lines i path]
  (let [n (count lines)
        [move-to i] (if (and (< i n) (starts-with? (nth lines i) "*** Move to: "))
                      [(parse-path-line (nth lines i) "*** Move to: ") (inc i)]
                      [nil i])]
    (loop [idx i
           hunks []
           hunk-index 0]
      (if (>= idx n)
        [{:type :update :path path :move-to move-to :hunks hunks} idx]
        (let [line (nth lines idx)]
          (cond
            (str/blank? line)
            (throw (ex-info "Unexpected empty line in update section. Use prefixed hunk lines (+, -, or space)."
                     {:type :tool/invalid-input :tool 'edit-file :path path :line-no idx}))

            (or (starts-with? line "*** Add File: ")
              (starts-with? line "*** Delete File: ")
              (starts-with? line "*** Update File: ")
              (= line "*** End Patch"))
            [{:type :update :path path :move-to move-to :hunks hunks} idx]

            (re-matches hunk-header-re line)
            (let [[ops next-idx]
                  (loop [j (inc idx)
                         ops []]
                    (if (>= j n)
                      [ops j]
                      (let [ln (nth lines j)]
                        (if (or (re-matches hunk-header-re ln)
                              (starts-with? ln "*** Add File: ")
                              (starts-with? ln "*** Delete File: ")
                              (starts-with? ln "*** Update File: ")
                              (= ln "*** End Patch"))
                          [ops j]
                          (recur (inc j) (conj ops (parse-hunk-op ln path hunk-index j)))))))]
              (when (empty? ops)
                (throw (ex-info "Hunk must contain operations"
                         {:type :tool/invalid-input :tool 'edit-file :path path :hunk-index hunk-index})))
              (validate-hunk-ranges! ops line path hunk-index)
              (recur next-idx
                (conj hunks {:header line :ops ops})
                (inc hunk-index)))

            :else
            (throw (ex-info "Update section expects strict hunk headers: @@ -a[,b] +c[,d] @@"
                     {:type :tool/invalid-input :tool 'edit-file :path path :line-no idx :line line}))))))))

(defn- parse-patch
  [patch-text]
  (let [lines (split-lines-vec patch-text)
        n (count lines)]
    (when (or (zero? n) (not= "*** Begin Patch" (first lines)))
      (throw (ex-info "Patch must start with '*** Begin Patch'"
               {:type :tool/invalid-input :tool 'edit-file})))
    (loop [i 1
           sections []]
      (when (>= i n)
        (throw (ex-info "Patch must end with '*** End Patch'"
                 {:type :tool/invalid-input :tool 'edit-file})))
      (let [line (nth lines i)]
        (cond
          (str/blank? line)
          (throw (ex-info "Unexpected empty line in patch envelope"
                   {:type :tool/invalid-input :tool 'edit-file :line-no i}))

          (= line "*** End Patch")
          (do
            (when (empty? sections)
              (throw (ex-info "Patch must contain at least one section"
                       {:type :tool/invalid-input :tool 'edit-file})))
            sections)

          (starts-with? line "*** Add File: ")
          (let [path (parse-path-line line "*** Add File: ")
                [plus-lines next-i]
                (loop [j (inc i)
                       acc []]
                  (if (>= j n)
                    [acc j]
                    (let [ln (nth lines j)]
                      (if (or (starts-with? ln "*** Add File: ")
                            (starts-with? ln "*** Delete File: ")
                            (starts-with? ln "*** Update File: ")
                            (= ln "*** End Patch"))
                        [acc j]
                        (do
                          (when-not (starts-with? ln "+")
                            (throw (ex-info "Add File content lines must start with '+'"
                                     {:type :tool/invalid-input :tool 'edit-file :path path :line-no j :line ln})))
                          (recur (inc j) (conj acc (subs ln 1))))))))]
            (recur next-i (conj sections {:type :add :path path :lines plus-lines})))

          (starts-with? line "*** Delete File: ")
          (let [path (parse-path-line line "*** Delete File: ")]
            (recur (inc i) (conj sections {:type :delete :path path})))

          (starts-with? line "*** Update File: ")
          (let [path (parse-path-line line "*** Update File: ")
                [section next-i] (parse-update-section lines (inc i) path)]
            (recur next-i (conj sections section)))

          :else
          (throw (ex-info "Unexpected patch line"
                   {:type :tool/invalid-input :tool 'edit-file :line-no i :line line})))))))

(defn- canonical-path-str
  [path]
  (.getCanonicalPath (canonical-file path)))

(defn- push-usage
  [m path usage]
  (update m (canonical-path-str path) (fnil conj []) usage))

(defn- validate-section-conflicts!
  [sections]
  (let [usages
        (reduce
          (fn [acc sec]
            (case (:type sec)
              :add
              (push-usage acc (:path sec) {:kind :add :path (:path sec)})

              :delete
              (push-usage acc (:path sec) {:kind :delete :path (:path sec)})

              :update
              (let [acc (push-usage acc (:path sec) {:kind :update-source :path (:path sec)})]
                (if-let [move-to (:move-to sec)]
                  (push-usage acc move-to {:kind :move-target :path move-to :from (:path sec)})
                  acc))))
          {}
          sections)]
    (doseq [[canonical-path uses] usages]
      (when (> (count uses) 1)
        (throw (ex-info "Patch has conflicting operations for the same file path"
                 {:type :tool/edit-conflict
                  :tool 'edit-file
                  :path canonical-path
                  :usages uses}))))
    sections))

;;; ── Patch Application ──────────────────────────────────────────────────

(defn- find-subseq-index
  [v sub start]
  (let [n (count v)
        m (count sub)]
    (cond
      (zero? m) start
      (> m (- n start)) nil
      :else
      (loop [i start]
        (cond
          (> (+ i m) n) nil
          (= sub (subvec v i (+ i m))) i
          :else (recur (inc i)))))))

(defn- apply-one-hunk
  [lines hunk path hunk-index]
  (let [ops (:ops hunk)
        before (vec (map second (remove #(= :add (first %)) ops)))
        start (or (find-subseq-index lines before 0)
                (throw (ex-info "Hunk context not found in file"
                         {:type :tool/edit-context-not-found
                          :tool 'edit-file
                          :path path
                          :hunk-index hunk-index
                          :before-preview (vec (take 6 before))})))
        out (transient [])
        cursor (atom start)]
    (doseq [[op txt] ops]
      (case op
        :context
        (let [got (get lines @cursor)]
          (when-not (= got txt)
            (throw (ex-info "Hunk context mismatch"
                     {:type :tool/edit-context-mismatch
                      :tool 'edit-file
                      :path path
                      :hunk-index hunk-index
                      :expected txt
                      :got got
                      :line @cursor})))
          (let [_ (conj! out txt)]
            (swap! cursor inc)))

        :remove
        (let [got (get lines @cursor)]
          (when-not (= got txt)
            (throw (ex-info "Hunk removal mismatch"
                     {:type :tool/edit-remove-mismatch
                      :tool 'edit-file
                      :path path
                      :hunk-index hunk-index
                      :expected txt
                      :got got
                      :line @cursor})))
          (swap! cursor inc))

        :add
        (conj! out txt)))
    (let [replacement (persistent! out)
          end @cursor]
      (vec (concat (subvec lines 0 start) replacement (subvec lines end))))))

(defn- apply-update-lines
  [content {:keys [hunks path]}]
  (let [initial-lines (split-lines-vec content)
        final-lines (reduce-kv
                      (fn [acc idx hunk]
                        (apply-one-hunk acc hunk path idx))
                      initial-lines
                      hunks)]
    (join-lines final-lines)))

(defn- parent-exists?
  [f]
  (let [p (.getParentFile ^java.io.File f)]
    (or (nil? p) (.exists p))))

(defn- ensure-parent!
  [f]
  (let [p (.getParentFile ^java.io.File f)]
    (when (and p (not (.exists p)))
      (.mkdirs p))))

(defn edit-file
  "Apply an OpenAI-style patch envelope.

   Supported section headers:
   - *** Add File: <path>
   - *** Delete File: <path>
   - *** Update File: <path>
   - *** Move to: <path>   (optional, immediately after Update File)

   Update sections use strict unified-diff hunk headers and prefixed lines:
   - @@ -a[,b] +c[,d] @@
   - space: context line
   - -: removed line
   - +: added line

   Minimal example:
   *** Begin Patch
   *** Update File: /tmp/demo.txt
   @@ -1,2 +1,2 @@
    hello
   -world
   +WORLD
   *** End Patch

   Returns map:
   {:files [{:action :add|:delete|:update|:move :path str ...}]
    :total-files int
    :total-hunks int}"
  [patch-text]
  (let [sections (-> patch-text parse-patch validate-section-conflicts!)
        staged
        (mapv
          (fn [sec]
            (case (:type sec)
              :add
              (let [target (canonical-file (:path sec))]
                (when (.exists target)
                  (throw (ex-info "Add File target already exists"
                           {:type :tool/edit-target-exists :tool 'edit-file :path (:path sec)})))
                (when-not (parent-exists? target)
                  (throw (ex-info "Add File parent directory does not exist"
                           {:type :tool/edit-parent-missing :tool 'edit-file :path (:path sec)})))
                (assoc sec
                  :target target
                  :content (join-lines (:lines sec))))

              :delete
              (let [target (validate-existing-file! (:path sec))]
                (assoc sec :target target))

              :update
              (let [source (validate-existing-file! (:path sec))
                    original (slurp source :encoding "UTF-8")
                    new-content (apply-update-lines original sec)
                    move-target (when-let [move-to (:move-to sec)]
                                  (canonical-file move-to))]
                (when (and move-target (.exists move-target))
                  (throw (ex-info "Move target already exists"
                           {:type :tool/edit-target-exists :tool 'edit-file :path (:move-to sec)})))
                (assoc sec
                  :source source
                  :original original
                  :new-content new-content
                  :move-target move-target))))
          sections)
        backups (atom [])
        created (atom [])]
    (try
      (doseq [sec staged]
        (case (:type sec)
          :add
          (let [f (:target sec)]
            (ensure-parent! f)
            (spit f (:content sec) :encoding "UTF-8")
            (swap! created conj f))

          :delete
          (let [f (:target sec)
                backup (java.io.File/createTempFile "vis-edit-delete-" ".bak")]
            (.deleteOnExit backup)
            (spit backup (slurp f :encoding "UTF-8") :encoding "UTF-8")
            (swap! backups conj {:type :restore :path f :backup backup})
            (.delete f))

          :update
          (let [source (:source sec)
                backup (java.io.File/createTempFile "vis-edit-update-" ".bak")
                move-target (:move-target sec)]
            (.deleteOnExit backup)
            (spit backup (:original sec) :encoding "UTF-8")
            (swap! backups conj {:type :restore :path source :backup backup})
            (if move-target
              (do
                (ensure-parent! move-target)
                (spit move-target (:new-content sec) :encoding "UTF-8")
                (swap! created conj move-target)
                (.delete source))
              (spit source (:new-content sec) :encoding "UTF-8")))))

      (let [file-results
            (mapv
              (fn [sec]
                (case (:type sec)
                  :add {:action :add :path (.getCanonicalPath ^java.io.File (:target sec))}
                  :delete {:action :delete :path (.getCanonicalPath ^java.io.File (:target sec))}
                  :update (let [action (if (:move-target sec) :move :update)
                                out-path (if (:move-target sec)
                                           (.getCanonicalPath ^java.io.File (:move-target sec))
                                           (.getCanonicalPath ^java.io.File (:source sec)))
                                old-lines (split-lines-vec (:original sec))
                                new-lines (split-lines-vec (:new-content sec))
                                patch (DiffUtils/diff old-lines new-lines)
                                diff-lines (vec (UnifiedDiffUtils/generateUnifiedDiff
                                                  (str "a/" (.getName ^java.io.File (:source sec)))
                                                  (str "b/" (.getName ^java.io.File (or (:move-target sec) (:source sec))))
                                                  old-lines patch 3))]
                            {:action action
                             :path out-path
                             :hunks (count (:hunks sec))
                             :diff diff-lines})))
              staged)]
        {:files file-results
         :total-files (count file-results)
         :total-hunks (reduce + 0 (map #(if (= :update (:type %)) (count (:hunks %)) 0) staged))})

      (catch Exception e
        (doseq [f @created]
          (when (.exists ^java.io.File f)
            (.delete ^java.io.File f)))
        (doseq [{:keys [path backup]} @backups]
          (spit path (slurp backup :encoding "UTF-8") :encoding "UTF-8"))
        (throw e)))))

(defn- validate-edit-input
  [{:keys [args]}]
  (let [[patch-text & extra] args]
    (when (seq extra)
      (throw (ex-info "edit-file expects exactly 1 positional arg: (edit-file patch-text)"
               {:type :tool/invalid-input :tool 'edit-file :args args})))
    (when-not (string? patch-text)
      (throw (ex-info "edit-file patch-text must be a string"
               {:type :tool/invalid-input :tool 'edit-file :got-type (type patch-text)})))
    {:args [patch-text]}))

(defn- validate-edit-output
  [{:keys [result]}]
  (when-not (map? result)
    (throw (ex-info "edit-file must return a map"
             {:type :tool/invalid-output :tool 'edit-file :got-type (type result)})))
  (when-not (vector? (:files result))
    (throw (ex-info "edit-file output :files must be a vector"
             {:type :tool/invalid-output :tool 'edit-file :result result})))
  (when-not (integer? (:total-files result))
    (throw (ex-info "edit-file output :total-files must be an int"
             {:type :tool/invalid-output :tool 'edit-file :result result})))
  {:result result})

(defn- format-edit-result
  "Pure formatter for edit-file's return map. Pattern:
     edited <n> file(s), <h> hunk(s)
       <action> <path> [<k> hunks]
         <first 10 diff lines...>

   Diff lines are shown once across files (first 10 total) to keep the
   summary compact. Callers who need full diffs introspect (:files result).
   Handles nil (no result) gracefully since the validator probes with nil."
  [result]
  (if (nil? result)
    ""
    (let [{:keys [files total-files total-hunks]} result
          header (str "edited " total-files " file" (when (not= 1 total-files) "s")
                   (when (and total-hunks (pos? total-hunks))
                     (str ", " total-hunks " hunk" (when (not= 1 total-hunks) "s"))))
          file-lines (map (fn [{:keys [action path hunks diff]}]
                            (let [diff-shown (when (seq diff) (take 5 diff))
                                  diff-more (when (seq diff) (- (count diff) (count diff-shown)))
                                  diff-block (when (seq diff-shown)
                                               (str "\n    " (str/join "\n    " diff-shown)
                                                 (when (pos? diff-more)
                                                   (str "\n    ... " diff-more " more diff lines"))))]
                              (str "  " (name action) " " path
                                (when hunks (str " [" hunks " hunk" (when (not= 1 hunks) "s") "]"))
                                (or diff-block ""))))
                       files)]
      (str header "\n" (str/join "\n" file-lines)))))

;;; ── Tool definition ────────────────────────────────────────────────────

(def tool-def
  (sci-tool/make-tool-def
    'edit-file
    edit-file
    {:doc (:doc (meta #'edit-file))
     :arglists (:arglists (meta #'edit-file))
     :validate-input validate-edit-input
     :validate-output validate-edit-output
     :format-result format-edit-result
     :activation-fn (constantly true)
     :group "filesystem" :activation-doc "always active"
     :examples ["(edit-file \"*** Begin Patch\\n*** Update File: /tmp/demo.txt\\n@@ -1,1 +1,1 @@\\n-old\\n+new\\n*** End Patch\")"
                "(edit-file \"*** Begin Patch\\n*** Add File: /tmp/new.txt\\n+hello\\n+world\\n*** End Patch\")"
                "(edit-file \"*** Begin Patch\\n*** Delete File: /tmp/old.txt\\n*** End Patch\")"]
     :prompt "Unified-diff patch. Envelope: `*** Begin Patch` / `*** Update|Add|Delete File: /abs/path` / `@@ -s,l +s,l @@` hunks / `*** End Patch`. Add-file has no hunk header (each line `+…`). Delete-file is empty section.
Surgical edits → edit-file. Whole-file rewrite → write-file.
Read the target region first — stale context = #1 fail reason."}))

