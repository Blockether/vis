(ns com.blockether.vis.web.presenter.tool-render
  "Custom presentation layers for tool execution results.
   Each tool gets a rich visual renderer instead of raw pr-str."
  (:require [clojure.string :as str]))

;;; ── Helpers ──────────────────────────────────────────────────────────

(defn- numbered-preview
  "Render numbered lines as hiccup pre block with gutter."
  [lines & [{:keys [start-line class]
             :or {start-line 1 class "tool-preview"}}]]
  (when (seq lines)
    [:pre {:class class}
     (for [[i line] (map-indexed vector lines)]
       [:span.tool-line
        [:span.tool-line-no (str (+ start-line i))]
        [:span.tool-line-content (str line "\n")]])]))

(defn- render-unified-diff
  "Render unified diff lines (from java-diff-utils) as styled hiccup."
  [diff-lines]
  (when (seq diff-lines)
    (let [;; Skip the --- and +++ header lines, keep @@ hunks and content
          body (drop-while #(or (str/starts-with? % "---") (str/starts-with? % "+++")) diff-lines)]
      [:div.tool-diff
       [:pre.tool-diff-content
        (for [line body]
          (let [cls (cond
                      (str/starts-with? line "@@") "tool-diff-hunk"
                      (str/starts-with? line "-")  "tool-diff-del"
                      (str/starts-with? line "+")  "tool-diff-add"
                      :else                        "tool-diff-ctx")]
            [:span {:class cls} (str line "\n")]))]])))

;;; ── list-dir ──────────────────────────────────────────────────────────

(defn- file-icon [type]
  (case type
    "directory" "folder"
    "symlink"   "link"
    "file"))

(defn- fmt-size [size]
  (cond
    (nil? size)       ""
    (< size 1024)     (str size " B")
    (< size 1048576)  (format "%.1f KB" (/ size 1024.0))
    :else             (format "%.1f MB" (/ size 1048576.0))))

(defn- extract-list-dir-args
  "Extract args from list-dir call code like (list-dir \"/path\" \"*.clj\" 2 100)."
  [code]
  (let [code-str (str code)
        ;; Extract all string args
        strings (re-seq #"\"([^\"]+)\"" code-str)
        path-arg (second (first strings))
        glob-arg (second (second strings))]
    {:call-path path-arg :call-glob glob-arg}))

(defn render-list-dir [result code]
  (let [{:keys [call-path call-glob]} (extract-list-dir-args code)]
    (cond
      ;; Normal structured result
      (and (map? result) (:entries result))
      (let [{:keys [path entries total truncated]} result]
        [:div.tool-list-dir
         [:div.tool-header
          [:i {:data-lucide "folder-open"}]
          [:span.tool-path (or path call-path "")]
          (when call-glob
            [:span.tool-badge.tool-badge-modified (str "glob: " call-glob)])
          [:span.tool-count (str total " item" (when (not= total 1) "s")
                                 (when truncated (str " (showing " (count entries) ")")))]]
         (if (empty? entries)
           [:div.tool-empty "Empty directory"]
           [:div.tool-file-list
            (for [{:keys [name type size permissions]} entries]
              [:div.tool-file-row {:class (str "tool-file-" type)}
               [:i.tool-file-icon {:data-lucide (file-icon type)}]
               [:span.tool-file-name name]
               (when permissions
                 [:span.tool-file-perms permissions])
               [:span.tool-file-size (fmt-size size)]])])])

      ;; Map without :entries — still show what we have
      (map? result)
      [:div.tool-list-dir
       [:div.tool-header
        [:i {:data-lucide "folder-open"}]
        [:span.tool-path (or (:path result) call-path "")]
        [:span.tool-count "result"]]
       [:pre.tool-preview (pr-str result)]]

      ;; String fallback
      (string? result)
      [:div.tool-list-dir
       [:div.tool-header
        [:i {:data-lucide "folder-open"}]
        [:span.tool-path (or call-path "list-dir")]]
       [:pre.tool-preview result]]

      :else nil)))

;;; ── shell-exec ────────────────────────────────────────────────────────

(defn render-shell-exec [result code]
  (when (and (map? result) (contains? result :exit-code))
    (let [{:keys [exit-code stdout stderr timed-out]} result
          ok? (zero? exit-code)]
      [:div.tool-shell
       [:div.tool-header
        [:i {:data-lucide "terminal"}]
        [:span.tool-shell-cmd code]
        [:span.tool-exit {:class (if ok? "tool-exit-ok" "tool-exit-err")}
         (if timed-out "TIMEOUT" (str "exit " exit-code))]]
       (when (and stdout (not (str/blank? stdout)))
         [:pre.tool-shell-out stdout])
       (when (and stderr (not (str/blank? stderr)))
         [:pre.tool-shell-err stderr])])))

;;; ── read-file ─────────────────────────────────────────────────────────

(defn render-read-file [result code]
  (let [path (second (re-find #"\"([^\"]+)\"" (str code)))]
    (cond
      ;; Normal string result
      (string? result)
      (let [lines      (str/split-lines result)
            has-footer (when (seq lines)
                         (re-find #"^\[(?:lines \d+-\d+ of \d+|empty file)\]$" (last lines)))
            content-lines (if has-footer (butlast lines) lines)
            footer-text   (when has-footer (last lines))
            line-count    (count content-lines)
            parsed (mapv (fn [line]
                           (if-let [[_ num content] (re-matches #"(\d+)\t(.*)" line)]
                             {:num num :content content}
                             {:num "" :content line}))
                         content-lines)]
        [:div.tool-read-file
         [:div.tool-header
          [:i {:data-lucide "file-text"}]
          [:span.tool-path (or path "")]
          [:span.tool-count (str line-count " line" (when (not= line-count 1) "s")
                                 (when footer-text (str " " footer-text)))]]
         (when (seq parsed)
           [:pre.tool-code-block
            (for [{:keys [num content]} parsed]
              [:span.tool-line
               [:span.tool-line-no num]
               [:span.tool-line-content (str content "\n")]])])])

      ;; nil result (def wrapper) — show at least the path
      (and (nil? result) path)
      [:div.tool-read-file
       [:div.tool-header
        [:i {:data-lucide "file-text"}]
        [:span.tool-path path]
        [:span.tool-count "(stored in var)"]]]

      :else nil)))

;;; ── write-file ────────────────────────────────────────────────────────

(defn render-write-file [result code]
  (let [code-path (second (re-find #"\"([^\"]+)\"" (str code)))]
    (cond
      (and (map? result) (:path result))
      (let [{:keys [path lines created? old-lines diff preview]} result]
        [:div.tool-write-file
         [:div.tool-header
          [:i {:data-lucide (if created? "file-plus" "file-pen")}]
          [:span.tool-path path]
          [:span.tool-badge {:class (if created? "tool-badge-created" "tool-badge-modified")}
           (cond
             created?  (str "CREATED · " lines " line" (when (not= lines 1) "s"))
             old-lines (str old-lines " → " lines " lines")
             :else     (str lines " line" (when (not= lines 1) "s") " written"))]]

         (cond
           ;; Overwrite with diff available — show unified diff
           (and diff (seq diff) (not created?))
           (render-unified-diff diff)

           ;; New file — show preview
           (and created? preview (seq preview))
           (let [truncated? (and lines (> lines (count preview)))]
             [:div.tool-write-preview
              (numbered-preview preview)
              (when truncated?
                [:div.tool-truncated (str "... " (- lines (count preview)) " more lines")])]))])

      ;; Fallback with map
      (map? result)
      [:div.tool-write-file
       [:div.tool-header
        [:i {:data-lucide "file-plus"}]
        [:span.tool-path (or (:path result) "")]
        [:span.tool-badge.tool-badge-modified (str (or (:lines result) "?") " lines written")]]]

      ;; nil result (def wrapper) — show path from code
      (and (nil? result) code-path)
      [:div.tool-write-file
       [:div.tool-header
        [:i {:data-lucide "file-pen"}]
        [:span.tool-path code-path]
        [:span.tool-count "(stored in var)"]]]

      :else nil)))

;;; ── edit-file ─────────────────────────────────────────────────────────

(defn render-edit-file [result code]
  (let [code-path (second (re-find #"\"([^\"]+)\"" (str code)))]
    (cond
      (and (map? result) (:path result))
      (let [{:keys [path replacements diff]} result]
        [:div.tool-edit-file
         [:div.tool-header
          [:i {:data-lucide "file-pen-line"}]
          [:span.tool-path path]
          [:span.tool-badge.tool-badge-modified
           (str replacements " replacement" (when (> replacements 1) "s"))]]
         ;; Unified diff from java-diff-utils
         (if (seq diff)
           (render-unified-diff diff)
           [:div.tool-diff
            [:pre.tool-diff-content
             [:span.tool-diff-ctx "  (no diff available)\n"]]])])

      (map? result)
      [:div.tool-edit-file
       [:div.tool-header
        [:i {:data-lucide "file-pen-line"}]
        [:span (or (:path result) "edit-file")]
        [:span.tool-count (pr-str result)]]]

      ;; nil result (def wrapper)
      (and (nil? result) code-path)
      [:div.tool-edit-file
       [:div.tool-header
        [:i {:data-lucide "file-pen-line"}]
        [:span.tool-path code-path]
        [:span.tool-count "(stored in var)"]]]

      :else nil)))

;;; ── shell-bg-read ─────────────────────────────────────────────────────

(defn render-shell-bg-read [result]
  (when (and (map? result) (contains? result :alive))
    (let [{:keys [pid alive exit-code stdout stderr]} result]
      [:div.tool-shell
       [:div.tool-header
        [:i {:data-lucide "terminal"}]
        [:span (str "PID " pid)]
        [:span.tool-exit {:class (if alive "tool-exit-bg" (if (and exit-code (zero? exit-code)) "tool-exit-ok" "tool-exit-err"))}
         (if alive "RUNNING" (str "exit " exit-code))]]
       (when (and stdout (not (str/blank? stdout)))
         [:pre.tool-shell-out stdout])
       (when (and stderr (not (str/blank? stderr)))
         [:pre.tool-shell-err stderr])])))

;;; ── Dispatch ──────────────────────────────────────────────────────────

(defn render-tool
  "Dispatch to a tool-specific renderer based on the tool name extracted from code.
   Returns hiccup or nil (nil = fall back to default rendering)."
  [tool-name result code]
  (case tool-name
    "list-dir"      (render-list-dir result code)
    "shell-exec"    (render-shell-exec result code)
    "read-file"     (render-read-file result code)
    "write-file"    (render-write-file result code)
    "edit-file"     (render-edit-file result code)
    "shell-bg-read" (render-shell-bg-read result)
    nil))
