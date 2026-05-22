(ns com.blockether.vis.internal.ctx-renderer
  "Pure-fn renderer: turn `{:ctx :warnings :progression :next-actions}` into
   the bare-EDN text block embedded under `;; ctx` in every user message.

   Design rules:

   1. **ONE printer.** Every Clojure VALUE the renderer emits goes through
      `zp` — a single locked entry into `safe-zprint-str` with a sealed
      `ZP_OPTS` config (no commas, sorted keys, width 80). No `pr-str`, no
      `pprint`, no ad-hoc string concat for value bodies.

   2. **Top-level structure is hand-assembled.** The renderer interleaves
      `:session/X` headers with their zp'd values and per-section annotation
      tails. That structural layer is the only thing the renderer itself
      writes; it never tries to inject text inside a zp'd value.

   3. **Annotations live in section tails.** Warnings and progression hints
      anchored to a top-level subtree (`:session/specs`, `:session/tasks`,
      `:session/facts`) follow that section's value as `;; ⚠ …` / `;;
      progression …` lines, indented to the section body's column. Model
      reads the data first, then the issues immediately below — clear
      locality without polluting the EDN.

   Output skeleton:

     ;; ctx
     {:session/id        \"01HXYZ\"
      :session/turn      7
      :session/scope     {:turn 7 :iter 3 :next-form 1}

      :session/workspace {…}
      :session/symbols   {…}
      :session/hints     {…}

      :session/specs
      {:auth {…}}
      ;; ⚠ spec :auth req :r1 refs nonexistent fact :foo
      ;; progression :auth 2/3 :partial; missing [:r2]

      :session/tasks {…}
      ;; ⚠ task :t1 :done but dep :t2 is :doing

      :session/facts   {…}
      :session/trailer […]
      :session/next-actions […]}"
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.format :as fmt]))

;; =============================================================================
;; Knobs
;; =============================================================================

(def ^:private TRAILER_BUDGET 16)
(def ^:private NEXT_ACTIONS_BUDGET 5)
(def ^:private WIDTH 80)

;; =============================================================================
;; The single value printer
;; =============================================================================

(def ^:private ZP_OPTS
  "Sealed zprint config — every value the renderer emits flows through this.
   No commas (bare EDN), deterministic key order (sort?), preserve namespace
   keywords (no shortening)."
  {:width WIDTH
   :map   {:comma? false :sort? true}
   :style :community})

(defn- zp
  "Render any Clojure value to a multi-line bare-EDN string. Trim trailing
   newline. This is the renderer's ONLY printer surface — see ns docstring."
  [v]
  (str/trim-newline (fmt/safe-zprint-str v ZP_OPTS)))

;; =============================================================================
;; Indent helpers (no value-string surgery — only structural padding)
;; =============================================================================

(defn- pad [n] (apply str (repeat n \space)))

(defn- indent-rest
  "Take a (possibly multi-line) string `s` and prefix every line AFTER the
   first with `n` spaces. The first line is returned untouched so the caller
   can position it under its key."
  [s n]
  (let [lines (str/split-lines s)]
    (if (= 1 (count lines))
      s
      (str (first lines) "\n"
        (str/join "\n" (map #(str (pad n) %) (rest lines)))))))

;; =============================================================================
;; Annotation lines (the only renderer output that isn't EDN data)
;; =============================================================================

(defn- warning-line [indent w]
  (str (pad indent) ";; ⚠ " (:message w)))

(defn- progression-line [indent spec-id p]
  (let [missing (some-> p :missing seq sort vec)]
    (str (pad indent) ";; progression " (zp spec-id) " "
      (:proven p) "/" (:total p) " " (zp (:state p))
      (when missing (str "; missing " (zp missing))))))

;; =============================================================================
;; Routing warnings to their owning section
;; =============================================================================

(defn- anchors-by-subtree
  "Group warnings by which top-level CTX subtree they anchor to. The :anchor
   first element is the routing key:
     - if it's a key in :session/specs → goes to :session/specs section
     - in :session/tasks → :session/tasks section
     - in :session/facts → :session/facts section
     - otherwise → :other (rendered after :session/next-actions)
   Returns {subtree-key [warnings…]}."
  [ctx warnings]
  (let [spec-keys (set (keys (or (:session/specs ctx) {})))
        task-keys (set (keys (or (:session/tasks ctx) {})))
        fact-keys (set (keys (or (:session/facts ctx) {})))]
    (reduce
      (fn [acc w]
        (let [a (first (:anchor w))
              bucket (cond
                       (contains? spec-keys a) :session/specs
                       (contains? task-keys a) :session/tasks
                       (contains? fact-keys a) :session/facts
                       :else :other)]
          (update acc bucket (fnil conj []) w)))
      {} warnings)))

(defn- section-annotations
  "Build the annotation block (a string of newline-joined ;; lines) for one
   subtree section. Returns nil when there is nothing to say."
  [warnings progression-entries indent]
  (let [lines (concat
                (map #(warning-line indent %) warnings)
                (map (fn [[k p]] (progression-line indent k p)) progression-entries))]
    (when (seq lines) (str/join "\n" lines))))

;; =============================================================================
;; Section + bounded subtrees
;; =============================================================================

(defn- render-section
  "One top-level `:session/X` key plus its value, with an optional annotation
   tail underneath. `value-text` is already zp'd (or hand-built for trailer /
   next-actions). Output column rules:

     :session/X
     <value-text indented by 1 space>
     ;; ⚠ …   (also indented by 1 space)
  "
  [k value-text annotation-block]
  (let [body (str " " (zp k) "\n " (indent-rest value-text 1))]
    (if annotation-block
      (str body "\n " annotation-block)
      body)))

(defn- render-trailer-value
  "Trailer is a vec, possibly large. Cap to TRAILER_BUDGET and append a
   `;; ⚠` truncation hint when over budget. Returns a multi-line string."
  [trailer]
  (let [n (count trailer)
        capped (vec (take TRAILER_BUDGET trailer))
        body (if (empty? capped) "[]" (zp capped))
        suffix (when (> n TRAILER_BUDGET)
                 (str "\n;; ⚠ trailer truncated: " TRAILER_BUDGET " of " n
                   " entries; consider :trailer-summarize at next done"))]
    (cond-> body suffix (str suffix))))

(defn- render-next-actions-value
  "Next-actions vec capped to NEXT_ACTIONS_BUDGET with an overflow hint."
  [actions]
  (let [n (count actions)
        capped (vec (take NEXT_ACTIONS_BUDGET actions))
        body (if (empty? capped) "[]" (zp capped))
        suffix (when (> n NEXT_ACTIONS_BUDGET)
                 (str "\n;; " (- n NEXT_ACTIONS_BUDGET)
                   " more action(s) suppressed"))]
    (cond-> body suffix (str suffix))))

;; =============================================================================
;; Top-level
;; =============================================================================

(defn render-ctx
  "Render the engine view as the bare-EDN text block embedded under `;; ctx`
   in the user message.

   Input map keys:
     :ctx           full ::cs/ctx (validated upstream)
     :warnings      vec of {:code :anchor :message} from derive-warnings
     :progression   {spec-id {:total :proven :ratio :state :missing}}
     :next-actions  vec from derive-next-actions

   Output: a string starting with `;; ctx\\n{` and ending with `}`."
  [{:keys [ctx warnings progression next-actions]}]
  (let [by-sub        (anchors-by-subtree ctx (or warnings []))
        prog-entries  (vec (or progression {}))
        specs-tail    (section-annotations
                        (get by-sub :session/specs [])
                        prog-entries 1)
        tasks-tail    (section-annotations (get by-sub :session/tasks []) [] 1)
        facts-tail    (section-annotations (get by-sub :session/facts []) [] 1)
        other-tail    (section-annotations (get by-sub :other []) [] 1)]
    (str
      ";; ctx\n"
      "{" (zp :session/id)    "        " (zp (:session/id ctx))    "\n"
      " " (zp :session/turn)  "      "   (zp (:session/turn ctx))  "\n"
      " " (zp :session/scope) "     "    (zp (:session/scope ctx)) "\n"
      "\n"
      (render-section :session/workspace
        (zp (or (:session/workspace ctx) {})) nil)                "\n\n"
      (render-section :session/symbols
        (zp (or (:session/symbols ctx) {})) nil)                  "\n\n"
      (render-section :session/hints
        (zp (or (:session/hints ctx) {})) nil)                    "\n\n"
      (render-section :session/specs
        (zp (or (:session/specs ctx) {})) specs-tail)             "\n\n"
      (render-section :session/tasks
        (zp (or (:session/tasks ctx) {})) tasks-tail)             "\n\n"
      (render-section :session/facts
        (zp (or (:session/facts ctx) {})) facts-tail)             "\n\n"
      (render-section :session/trailer
        (render-trailer-value (or (:session/trailer ctx) [])) nil) "\n\n"
      (render-section :session/next-actions
        (render-next-actions-value (or next-actions []))
        other-tail)
      "}")))
