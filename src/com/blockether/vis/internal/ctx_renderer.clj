(ns com.blockether.vis.internal.ctx-renderer
  "Pure-fn renderer: turn `{:ctx :warnings :progression :next-actions}` into
   the bare-EDN text that goes under the `;; ctx` marker in every user
   message. No IO. Deterministic. Bounded.

   Output shape (high-level):

     ;; ctx
     {:session/id        \"01HXYZ\"
      :session/turn      7
      :session/scope     {:turn 7 :iter 3 :next-form 1}

      :session/workspace {…}
      :session/symbols   {…}
      :session/hints     {…}

      :session/specs
      {:spec-1
       {:title \"…\" :status :doing :requirements […] :born \"t2/i1/f1\"}
       ;; ⚠ req :r1 facts ref nonexistent :missing
       ;; progression :spec-1 2/3 :partial; missing [:r2]

       :spec-2 {…}}

      :session/tasks {…}
      :session/facts {…}
      :session/trailer […]            ; capped + truncation hint when long
      :session/next-actions […]}      ; top-5 cap

   Warnings whose `:anchor` first element matches a top-level entry key
   surface inline directly after that entry's value. Progression is rendered
   per spec entry. The renderer never re-derives anything — it just inlines."
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]))

;; =============================================================================
;; Knobs
;; =============================================================================

(def ^:private TRAILER_BUDGET 16)
(def ^:private NEXT_ACTIONS_BUDGET 5)
(def ^:private VALUE_WIDTH 80)

;; =============================================================================
;; Low-level printing helpers
;; =============================================================================

(defn- pr-edn
  "pr-str a value to a single line, with `*print-namespace-maps*` off so
   `:git/branch` shows as `:git/branch`, not `:my/branch`-style shorthand."
  [v]
  (binding [*print-namespace-maps* false]
    (pr-str v)))

(defn- pp-str
  "Pretty-print a value to a (possibly multi-line) string. Trailing newline
   trimmed. `width` controls the right margin."
  ([v] (pp-str v VALUE_WIDTH))
  ([v width]
   (binding [pp/*print-right-margin* width
             *print-namespace-maps* false
             *print-length* nil
             *print-level* nil]
     (str/trim-newline (with-out-str (pp/pprint v))))))

(defn- pad [n] (apply str (repeat n \space)))

(defn- indent-rest
  "Indent every line of `s` after the first by `n` spaces. First line stays
   as-is. Used to make continuation lines align under the caller's column."
  [s n]
  (let [lines (str/split-lines s)]
    (if (= 1 (count lines))
      s
      (str (first lines) "\n"
        (str/join "\n" (map #(str (pad n) %) (rest lines)))))))

;; =============================================================================
;; Annotation lines
;; =============================================================================

(defn- warning-line [indent w]
  (str (pad indent) ";; ⚠ " (:message w)))

(defn- progression-line [indent spec-id p]
  (let [m (some-> p :missing seq sort vec)
        body (str (:proven p) "/" (:total p) " " (:state p))]
    (str (pad indent) ";; progression " (pr-edn spec-id) " " body
      (when m (str "; missing " m)))))

(defn- group-anchors-by-top-id [warnings]
  (group-by #(first (:anchor %)) warnings))

;; =============================================================================
;; Annotated subtree render — used for :session/specs / :session/tasks /
;; :session/facts, where each entry can be followed by `;; ⚠` and
;; `;; progression` lines.
;; =============================================================================

(defn- render-entry
  "Render one `<key> <value>` block plus its anchored warnings and (for specs)
   the progression line. `indent` is the column of the key."
  [k v indent anchors-by-id progression]
  (let [key-text (pr-edn k)
        ;; Print the value with a width that leaves room for the key prefix.
        val-text (pp-str v (max 20 (- VALUE_WIDTH (count key-text) 1)))
        inner (+ indent 1 (count key-text) 1)
        body (indent-rest val-text inner)
        warns (get anchors-by-id k [])
        prog (when (and progression (contains? progression k))
               (get progression k))
        ann-lines (concat
                    (map #(warning-line indent %) warns)
                    (when prog [(progression-line indent k prog)]))]
    (str (pad indent) key-text " " body
      (when (seq ann-lines) (str "\n" (str/join "\n" ann-lines))))))

(defn- render-annotated-map
  "Render `entries` (a map) with annotation lines between entries.
   `base-indent` is the column where each key starts.
   Returns a string starting with `{` and ending with `}`."
  [entries base-indent anchors-by-id progression]
  (if (empty? entries)
    "{}"
    (let [sorted (sort-by (comp pr-edn key) entries)
          blocks (for [[k v] sorted]
                   (render-entry k v (inc base-indent) anchors-by-id progression))]
      (str "{" (str/join "\n\n" (map (fn [b] (subs b (inc base-indent))) blocks)) "}"))))

;; =============================================================================
;; Trailer + next-actions bounded rendering
;; =============================================================================

(defn- render-trailer [trailer indent]
  (let [n (count trailer)
        capped (vec (take TRAILER_BUDGET trailer))
        body (if (empty? capped) "[]" (pp-str capped))]
    (cond-> body
      (> n TRAILER_BUDGET)
      (str "\n" (pad indent)
        ";; ⚠ trailer truncated: showing " TRAILER_BUDGET " of " n
        " entries; consider :trailer-summarize at next done"))))

(defn- render-next-actions [actions indent]
  (let [n (count actions)
        capped (vec (take NEXT_ACTIONS_BUDGET actions))
        body (if (empty? capped) "[]" (pp-str capped))]
    (cond-> body
      (> n NEXT_ACTIONS_BUDGET)
      (str "\n" (pad indent)
        ";; " (- n NEXT_ACTIONS_BUDGET) " more action(s) suppressed"))))

;; =============================================================================
;; Top-level
;; =============================================================================

(defn render-ctx
  "Render the engine view (ctx + derived warnings/progression/next-actions)
   as the bare-EDN text block embedded under `;; ctx` in the user message.

   Inputs:
     :ctx           the full session map (validated against ::cs/ctx upstream)
     :warnings      vec of {:code :anchor :message} from derive-warnings
     :progression   {spec-id {:total :proven :ratio :state :missing}} from derive-progression
     :next-actions  vec from derive-next-actions

   Output: a string that starts with `;; ctx\\n{` and ends with `}`."
  [{:keys [ctx warnings progression next-actions]}]
  (let [anchors (group-anchors-by-top-id (or warnings []))
        section
        (fn [k v]
          (str " " (pr-edn k) "\n " (indent-rest v 1)))
        annotated-section
        (fn [k entries anchors* progression*]
          (str " " (pr-edn k) "\n "
            (indent-rest (render-annotated-map entries 1 anchors* progression*) 1)))]
    (str
      ";; ctx\n"
      "{:session/id        " (pr-edn (:session/id ctx)) "\n"
      " :session/turn      " (:session/turn ctx) "\n"
      " :session/scope     " (pr-edn (:session/scope ctx)) "\n"
      "\n"
      (section :session/workspace (pp-str (or (:session/workspace ctx) {}))) "\n"
      "\n"
      (section :session/symbols   (pp-str (or (:session/symbols ctx) {})))   "\n"
      "\n"
      (section :session/hints     (pp-str (or (:session/hints ctx) {})))     "\n"
      "\n"
      (annotated-section :session/specs (or (:session/specs ctx) {}) anchors progression) "\n"
      "\n"
      (annotated-section :session/tasks (or (:session/tasks ctx) {}) anchors nil) "\n"
      "\n"
      (annotated-section :session/facts (or (:session/facts ctx) {}) anchors nil) "\n"
      "\n"
      (section :session/trailer (render-trailer (or (:session/trailer ctx) []) 1)) "\n"
      "\n"
      (section :session/next-actions (render-next-actions (or next-actions []) 1))
      "}")))
