(ns com.blockether.vis.ext.channel-tui.block-fixtures-test
  "Phase-6 BLOCK fixtures for the TUI revamp.

   These pin the user-visible contract for the canonical fence/block shapes
   the model emits in practice: a git status+add+commit!+push! fence, nested
   `let` tool calls, a `(def x (v/cat ...))` bind, plain values, value-only
   errors, and the cancellation / timeout / merged-fence edge cases.

   Each fixture builds ONE canonical iteration-entry (`:forms` + the per-form
   `:channel` sink slice carrying the `{:summary :display}` render contract)
   and asserts BOTH:

     - the pure `iteration/iteration-entry->display-block` projection (one
       block, real op counts, status), AND
     - the rendered TUI lines (`render/format-iteration-entry`) — one BLOCK
       header, the right op rows, inline carets, no fake badges.

   The display-block is the SAME shape the live progress tracker and the
   resume projection both feed (see `parity-test`), so a fixture that passes
   here passes identically live and on resume."
  (:require
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.channel-tui.render :as render]
   [com.blockether.vis.internal.iteration :as iteration]
   [clojure.string :as str]
   [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private format-iteration-entry @#'render/format-iteration-entry)

(defn- strip-ansi [s]
  (str/replace (or s "") #"\[[0-9;]*m" ""))

(defn- strip-sentinels
  "Drop the line-kind marker (first PUA codepoint) and inline-style sentinels
   so equality / substring checks compare the visible text only."
  [s]
  (->> (or s "")
    (remove #(<= 0xE000 (int %) 0xF8FF))
    (apply str)))

(defn- clean [s] (strip-sentinels (strip-ansi s)))

(defn- rendered
  "All visible lines for one iteration-entry, ANSI + marker stripped."
  ([entry] (rendered entry {}))
  ([entry opts]
   (mapv clean (format-iteration-entry entry 80 1 opts))))

(defn- op-rows
  "Visible `▶`/`▼` op rows in a rendered frame. Excludes the BLOCK header,
   which now also carries a ▶/▼ disclosure chevron (the whole block is
   collapsible) and would otherwise be miscounted as an op row."
  [lines]
  (filter #(and (or (str/includes? % "▶") (str/includes? % "▼"))
             (not (str/includes? % "ITERATION")))
    lines))

(defn- header-of
  [lines]
  (some #(when (str/includes? % "ITERATION") %) lines))

;; ---------------------------------------------------------------------------
;; Sink-entry + entry builders.
;; ---------------------------------------------------------------------------

(defn- ok-op
  "One successful tool sink op carrying the `{:summary :display}` contract.
   `label` is the summary's first [:strong] — the renderer op-row LABEL."
  [position op tag label detail]
  {:position position
   :form     (str "(" (name op) ")")
   :symbol   op
   :op       op
   :tag      tag
   :success? true
   :error    nil
   :result   {:summary (vis/ir-root (vis/ir-p (vis/ir-strong label) (str "  " detail)))
              :display (vis/ir-root (vis/ir-p (vis/ir-strong label) (str "  " detail)))}})

(defn- single-form-entry
  "An iteration-entry with ONE proof envelope (one fence) whose `:channel`
   slice is `channel`. `status` (when given) is honoured by `entry-status`."
  [code scope channel & {:keys [status error duration-ms]}]
  (cond-> {:position 0
           :code     code
           :forms    [{:scope       scope
                       :tag         (:tag (first channel) :observation)
                       :src         code
                       :channel     (vec channel)
                       :duration-ms (or duration-ms 100)
                       :success?    (nil? error)
                       :error       error}]}
    (some? status) (assoc :status status)
    (some? error)  (assoc :error error)))

;; ---------------------------------------------------------------------------
;; Fixture 1 — git status + add + commit! + push! : one fence, four op rows.
;; ---------------------------------------------------------------------------

(def ^:private git-fence-code
  "(git/status)\n(git/add \".\")\n(git/commit! \"msg\")\n(git/push!)")

(defn- git-fence-entry []
  (single-form-entry
    git-fence-code "t6/i1/f1"
    [(ok-op 0 :git/status   :observation "STATUS" "3 files")
     (ok-op 1 :git/add      :mutation    "ADD"    ".")
     (ok-op 2 :git/commit!  :mutation    "COMMIT" "1 file")
     (ok-op 3 :git/push!    :mutation    "PUSH"   "main")]
    :duration-ms 3300))

(defdescribe git-status-add-commit-push-fixture-test
  (describe "git status+add+commit!+push! fence"
    (it "projects ONE display-block with 1 observation, 3 mutations"
      (let [block (iteration/iteration-entry->display-block (git-fence-entry))]
        (expect (= "t6/i1" (:scope block)))
        (expect (= {:observations 1 :mutations 3} (:counts block)))
        (expect (= 4 (count (:ops block))))
        (expect (= :ok (:status block)))
        (expect (= [:git/status :git/add :git/commit! :git/push!]
                  (mapv :op (:ops block))))))

    (it "renders ONE header `1 observation · 3 mutations` with ✓ and four op rows"
      (let [lines (rendered (git-fence-entry))
            hdr   (header-of lines)]
        (expect (= 1 (count (filter #(str/includes? % "ITERATION") lines))))
        (expect (some? hdr))
        (expect (str/includes? hdr "ITERATION 1"))
        (expect (not (str/includes? hdr "/f1")))
        (expect (str/includes? hdr "1 observation"))
        (expect (str/includes? hdr "3 mutations"))
        (expect (str/includes? hdr "✓"))
        ;; Four op rows, one per tool call.
        (let [rows (op-rows lines)]
          (expect (= 4 (count rows)))
          (expect (some #(str/includes? % "STATUS") rows))
          (expect (some #(str/includes? % "ADD") rows))
          (expect (some #(str/includes? % "COMMIT") rows))
          (expect (some #(str/includes? % "PUSH") rows)))))

    (it "collapses the whole block (code + op rows) when the header toggle is off"
      ;; The BLOCK header is a disclosure toggle keyed on `<scope>:block`.
      ;; Collapsed → only the header (with ▶) survives; code body + op rows fold away.
      (let [lines (rendered (git-fence-entry)
                    {:session-id "s1"
                     :detail-expansions {["s1" "t6/i1:block"] false}})]
        ;; Header still present...
        (expect (some? (header-of lines)))
        ;; ...but every op row is gone, and the code body is folded.
        (expect (zero? (count (op-rows lines))))
        (expect (not (some #(str/includes? % "STATUS") lines)))
        (expect (not (some #(str/includes? % "git/status") lines)))))))

;; ---------------------------------------------------------------------------
;; Fixture 2 — nested `let`: (let [a (v/cat) b (v/cat)] (v/patch)).
;; Two observations + one mutation, ONE block, three op rows.
;; ---------------------------------------------------------------------------

(defn- nested-let-entry []
  (single-form-entry
    "(let [a (v/cat \"a\")\n      b (v/cat \"b\")]\n  (v/patch [{:path \"x\" :search \"a\" :replace \"b\"}]))"
    "t9/i2/f1"
    [(ok-op 0 :cat     :observation "CAT"   "a")
     (ok-op 1 :cat     :observation "CAT"   "b")
     (ok-op 2 :v/patch :mutation    "PATCH" "+1 -0")]))

(defdescribe nested-let-fixture-test
  (describe "nested (let [a (v/cat) b (v/cat)] (v/patch))"
    (it "projects ONE block, three ops, 2 observations · 1 mutation"
      (let [block (iteration/iteration-entry->display-block (nested-let-entry))]
        (expect (= 3 (count (:ops block))))
        (expect (= {:observations 2 :mutations 1} (:counts block)))
        (expect (= [:cat :cat :v/patch] (mapv :op (:ops block))))))

    (it "renders ONE header with `2 observations · 1 mutation` and three op rows"
      (let [lines (rendered (nested-let-entry))
            hdr   (header-of lines)]
        (expect (= 1 (count (filter #(str/includes? % "ITERATION") lines))))
        (expect (str/includes? hdr "2 observations"))
        (expect (str/includes? hdr "1 mutation"))
        (expect (= 3 (count (op-rows lines))))))))

;; ---------------------------------------------------------------------------
;; Fixture 3 — (def x (v/cat "x")): one block, one op row.
;; ---------------------------------------------------------------------------

(defn- def-bind-entry []
  (single-form-entry
    "(def x (v/cat \"x\"))" "t3/i1/f1"
    [(ok-op 0 :cat :observation "CAT" "x")]))

(defdescribe def-bind-fixture-test
  (describe "(def x (v/cat \"x\"))"
    (it "projects ONE block, ONE op"
      (let [block (iteration/iteration-entry->display-block (def-bind-entry))]
        (expect (= 1 (count (:ops block))))
        (expect (= {:observations 1 :mutations 0} (:counts block)))))

    (it "renders ONE header and ONE op row"
      (let [lines (rendered (def-bind-entry))]
        (expect (= 1 (count (filter #(str/includes? % "ITERATION") lines))))
        (expect (= 1 (count (op-rows lines))))
        (expect (some #(str/includes? % "CAT") (op-rows lines)))))))

;; ---------------------------------------------------------------------------
;; Fixture 4 — plain value (+ 1 2): one block, zero op rows, no header.
;; ---------------------------------------------------------------------------

(defn- plain-value-entry []
  {:position 0
   :forms [{:scope "t1/i1/f1" :code "(+ 1 2)" :src "(+ 1 2)"
            :result-render "3" :result-kind :value
            :channel [] :duration-ms 1 :success? true :error nil
            :render-segments nil}]})

(defdescribe plain-value-fixture-test
  (describe "plain value (+ 1 2)"
    (it "projects ONE block with zero ops"
      (let [block (iteration/iteration-entry->display-block (plain-value-entry))]
        (expect (= 0 (count (:ops block))))
        (expect (= {:observations 0 :mutations 0} (:counts block)))))

    (it "renders zero op rows and no BLOCK header (headerless plain value)"
      (let [lines (rendered (plain-value-entry))
            body  (str/join "\n" lines)]
        (expect (= 0 (count (op-rows lines))))
        ;; No fake op badge, no `3` value body (plain values are hidden).
        (expect (not (str/includes? body "ITERATION")))
        (expect (not-any? #(= "3" (str/trim %)) lines))))))

;; ---------------------------------------------------------------------------
;; Fixture 5 — value-only error (/ 1 0): inline caret + message, no fake badge.
;; ---------------------------------------------------------------------------

(defn- value-error-entry []
  (let [code "(/ 1 0)"
        err  {:message "Divide by zero"
              :trace "java.lang.ArithmeticException: Divide by zero"
              :block {:source code :row 1 :col 1}}]
    {:position 0
     :forms [{:scope "t1/i1/f1" :code code :src code
              :result-render nil :result-kind :error :result-detail nil
              :channel [] :error err :duration-ms 1 :success? false
              :render-segments nil}]}))

(defdescribe value-error-fixture-test
  (describe "value-only error (/ 1 0)"
    (it "projects an :error block with zero ops (no fake op badge)"
      (let [block (iteration/iteration-entry->display-block (value-error-entry))]
        (expect (= :error (:status block)))
        (expect (= 0 (count (:ops block))))))

    (it "renders an inline caret + message under the code, no op badge"
      (let [lines (rendered (value-error-entry))
            body  (str/join "\n" lines)]
        (expect (str/includes? body "^"))
        (expect (str/includes? body "Divide by zero"))
        ;; No `▶`/`▼` op row — value-only errors are inline, not a tool badge.
        (expect (= 0 (count (op-rows lines))))))))

;; ---------------------------------------------------------------------------
;; Fixture 6 — cancellation mid-block → :cancelled.
;; ---------------------------------------------------------------------------

(defn- cancelled-entry []
  (single-form-entry
    "(git/status)\n(git/add \".\")" "t4/i1/f1"
    [(ok-op 0 :git/status :observation "STATUS" "3 files")]
    :status :cancelled))

(defdescribe cancelled-fixture-test
  (describe "cancellation mid-block"
    (it "projects a :cancelled status block"
      (let [block (iteration/iteration-entry->display-block (cancelled-entry))]
        (expect (= :cancelled (:status block)))))

    (it "renders the ⊘ cancelled glyph in the header"
      (let [lines (rendered (cancelled-entry))
            hdr   (header-of lines)]
        (expect (some? hdr))
        (expect (str/includes? hdr (iteration/status-glyph :cancelled)))))))

;; ---------------------------------------------------------------------------
;; Fixture 7 — timeout mid-block → :timeout.
;; ---------------------------------------------------------------------------

(defn- timeout-entry []
  (single-form-entry
    "(slow-tool)" "t5/i1/f1"
    [(ok-op 0 :slow/tool :observation "SLOW" "running")]
    :status :timeout))

(defdescribe timeout-fixture-test
  (describe "timeout mid-block"
    (it "projects a :timeout status block"
      (let [block (iteration/iteration-entry->display-block (timeout-entry))]
        (expect (= :timeout (:status block)))))

    (it "renders the ⏱ timeout glyph in the header"
      (let [lines (rendered (timeout-entry))
            hdr   (header-of lines)]
        (expect (some? hdr))
        (expect (str/includes? hdr (iteration/status-glyph :timeout)))))))

;; ---------------------------------------------------------------------------
;; Fixture 8 — merged multi-fence → header annotates `merged N fences`.
;; ---------------------------------------------------------------------------

(defn- merged-fences-entry []
  {:position 0
   :forms [{:scope "t8/i1/f1" :code "(git/status)" :src "(git/status)"
            :channel [(ok-op 0 :git/status :observation "STATUS" "3 files")]
            :duration-ms 50 :success? true :error nil}
           {:scope "t8/i1/f2" :code "(git/add \".\")" :src "(git/add \".\")"
            :channel [(ok-op 0 :git/add :mutation "ADD" ".")]
            :duration-ms 50 :success? true :error nil}]})

(defdescribe merged-fences-fixture-test
  (describe "merged multi-fence"
    (it "projects ONE block with :merged-fences 2 and both ops"
      (let [block (iteration/iteration-entry->display-block (merged-fences-entry))]
        (expect (= 2 (:merged-fences block)))
        (expect (= 2 (count (:ops block))))
        (expect (= {:observations 1 :mutations 1} (:counts block)))))

    (it "renders ONE header annotating `merged 2 fences`"
      (let [lines (rendered (merged-fences-entry))
            hdr   (header-of lines)]
        (expect (= 1 (count (filter #(str/includes? % "ITERATION") lines))))
        (expect (str/includes? hdr "merged 2 fences"))))))
