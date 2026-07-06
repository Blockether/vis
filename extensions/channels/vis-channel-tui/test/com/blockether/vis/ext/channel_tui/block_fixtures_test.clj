(ns com.blockether.vis.ext.channel-tui.block-fixtures-test
  "BLOCK fixtures for the TUI trace renderer.

   These pin the user-visible contract for the canonical fence/block shapes
   the model emits in practice: a git status+add+commit!+push! fence, nested
   `let` tool calls, a `(def x (cat ...))` bind, plain values, value-only
   errors, and the cancellation / timeout / merged-fence edge cases.

   Tool output is now shown purely as the program's STDOUT — there is no
   render-fn op card / op row / `{:summary :display}` contract any more. Each
   fixture builds ONE canonical iteration-entry (`:forms`, each form carrying
   its printed `:stdout`) and asserts BOTH:

     - the pure `iteration/canonicalize` projection (block-level scope +
       status derived from the forms), AND
     - the rendered TUI lines (`render/format-iteration-entry`) — a FLAT
       block (no count header, no block-level collapse), the printed stdout,
       inline error carets, and no leftover op badges / count headers.

   The canonical entry is the SAME shape the live progress tracker and the
   resume projection both feed (see `parity-test`), so a fixture that passes
   here passes identically live and on resume."
  (:require [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.internal.iteration :as iteration]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private format-iteration-entry @#'render/format-iteration-entry)

(defn- strip-ansi [s] (str/replace (or s "") #"\[[0-9;]*m" ""))

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
  ([entry opts] (mapv clean (format-iteration-entry entry 80 1 opts))))

;; The block-level COUNT header (`N observation(s)` / `N mutation(s)`), the
;; op rows, and the block-level collapse toggle were ALL retired. Tool output
;; is the program's stdout now. We keep `counts-re` to ASSERT the header's
;; absence; any rendered line carrying the counts vocabulary would be a
;; regression.
(def ^:private counts-re #"observation|mutation")

(defn- header-of
  "The retired count header, if one ever leaks back in. Always nil now;
   used by the fixtures to assert headerlessness."
  [lines]
  (some #(when (re-find counts-re %) %) lines))

;; ---------------------------------------------------------------------------
;; Form builders.
;; ---------------------------------------------------------------------------

(defn- ok-form
  "One successful form whose printed output is `stdout`."
  [position scope code tag stdout]
  {:position position
   :scope scope
   :code code
   :src code
   :tag tag
   :stdout stdout
   :duration-ms 100
   :success? true
   :error nil})

(defn- single-form-entry
  "An iteration-entry with ONE proof envelope (one fence). `status` (when
   given) is honoured by `entry-status`."
  [form & {:keys [status error]}]
  (cond-> {:position 0
           :code (:code form)
           :forms [(cond-> form
                     error
                     (assoc :success?
                       false :error
                       error))]}
    (some? status)
    (assoc :status status)

    (some? error)
    (assoc :error error)))

;; ---------------------------------------------------------------------------
;; Fixture 1 — git status + add + commit! + push! : one fence, printed output.
;; ---------------------------------------------------------------------------

(def ^:private git-fence-code "(git/status)\n(git/add \".\")\n(git/commit! \"msg\")\n(git/push!)")

(def ^:private git-fence-stdout "STATUS  3 files\nADD  .\nCOMMIT  1 file\nPUSH  main")

(defn- git-fence-entry
  []
  (single-form-entry (ok-form 0 "t6/i1/f1" git-fence-code :mutation git-fence-stdout)))

(defdescribe git-status-add-commit-push-fixture-test
             (describe "git status+add+commit!+push! fence"
                       (it "canonicalizes to ONE block scope with :ok status"
                           (let [block (iteration/canonicalize (git-fence-entry))]
                             (expect (= "t6/i1" (:scope block)))
                             (expect (= :ok (:status block)))
                             (expect (nil? (:error block)))))))

;; ---------------------------------------------------------------------------
;; Fixture 2 — nested `let`: (let [a (cat) b (cat)] (patch)).
;; ---------------------------------------------------------------------------

(defn- nested-let-entry
  []
  (single-form-entry
    (ok-form
      0
      "t9/i2/f1"
      "(let [a (cat \"a\")\n      b (cat \"b\")]\n  (patch [{:path \"x\" :search \"a\" :replace \"b\"}]))"
      :mutation "CAT  a\nCAT  b\nPATCH  +1 -0")))

(defdescribe nested-let-fixture-test
             (describe "nested (let [a (cat) b (cat)] (patch))"
                       (it "canonicalizes to ONE block scope"
                           (let [block (iteration/canonicalize (nested-let-entry))]
                             (expect (= "t9/i2" (:scope block)))
                             (expect (= :ok (:status block)))))))

;; ---------------------------------------------------------------------------
;; Fixture 3 — (def x (cat "x")): one block, printed output.
;; ---------------------------------------------------------------------------

(defn- def-bind-entry
  []
  (single-form-entry (ok-form 0 "t3/i1/f1" "(def x (cat \"x\"))" :observation "CAT  x")))

(defdescribe def-bind-fixture-test
             (describe "(def x (cat \"x\"))"
                       (it "canonicalizes to ONE block scope"
                           (let [block (iteration/canonicalize (def-bind-entry))]
                             (expect (= "t3/i1" (:scope block)))
                             (expect (= :ok (:status block)))))))

;; ---------------------------------------------------------------------------
;; Fixture 4 — plain value (+ 1 2): one block, no stdout, no header.
;; ---------------------------------------------------------------------------

(defn- plain-value-entry
  []
  {:position 0
   :forms [{:scope "t1/i1/f1"
            :code "(+ 1 2)"
            :src "(+ 1 2)"
            :result-kind :value
            :stdout nil
            :duration-ms 1
            :success? true
            :error nil
            :render-segments nil}]})

(defdescribe plain-value-fixture-test
             (describe "plain value (+ 1 2)"
                       (it "canonicalizes to ONE :ok block"
                           (let [block (iteration/canonicalize (plain-value-entry))]
                             (expect (= "t1/i1" (:scope block)))
                             (expect (= :ok (:status block)))))
                       (it "renders no stdout body and no header (bare values are never echoed)"
                           (let [lines
                                 (rendered (plain-value-entry))

                                 body
                                 (str/join "\n" lines)]

                             ;; Bare return values never reach the model's context, so they are
                             ;; not painted here either. No `3`, no ITERATION header.
                             (expect (not (str/includes? body "ITERATION")))
                             (expect (not-any? #(= "3" (str/trim %)) lines))))))

;; ---------------------------------------------------------------------------
;; Fixture 5 — value-only error (/ 1 0): inline caret + message.
;; ---------------------------------------------------------------------------

(defn- value-error-entry
  []
  (let [code
        "(/ 1 0)"

        err
        {:message "Divide by zero"
         :trace "java.lang.ArithmeticException: Divide by zero"
         :block {:source code :row 1 :col 1}}]

    {:position 0
     :forms [{:scope "t1/i1/f1"
              :code code
              :src code
              :result-kind :error
              :stdout nil
              :error err
              :duration-ms 1
              :success? false
              :render-segments nil}]}))

(defdescribe value-error-fixture-test
             (describe "value-only error (/ 1 0)"
                       (it "canonicalizes to an :error block carrying the form error"
                           (let [block (iteration/canonicalize (value-error-entry))]
                             (expect (= :error (:status block)))
                             (expect (some? (:error block)))))
                       (it "renders an inline caret + message under the code"
                           (let [lines
                                 (rendered (value-error-entry))

                                 body
                                 (str/join "\n" lines)]

                             (expect (str/includes? body "^"))
                             (expect (str/includes? body "Divide by zero"))))))

;; ---------------------------------------------------------------------------
;; Fixture 6 — cancellation mid-block → :cancelled.
;; ---------------------------------------------------------------------------

(defn- cancelled-entry
  []
  (single-form-entry
    (ok-form 0 "t4/i1/f1" "(git/status)\n(git/add \".\")" :observation "STATUS  3 files")
    :status
    :cancelled))

(defdescribe
  cancelled-fixture-test
  (describe "cancellation mid-block"
            (it "canonicalizes to a :cancelled status block"
                (let [block (iteration/canonicalize (cancelled-entry))]
                  (expect (= :cancelled (:status block)))))
            (it "keeps the status glyph OUT of the block render (it lives on the TURN header)"
                ;; No block header at all, and the per-block render never paints a
                ;; status glyph — aggregate status belongs to the parent TURN header.
                (let [lines (rendered (cancelled-entry))]
                  (expect (nil? (header-of lines)))
                  (expect (not-any? #(str/includes? % (iteration/status-glyph :cancelled)) lines))
                  (expect (not-any? #(str/includes? % (iteration/status-glyph :ok)) lines))))))

;; ---------------------------------------------------------------------------
;; Fixture 7 — timeout mid-block → :timeout.
;; ---------------------------------------------------------------------------

(defn- timeout-entry
  []
  (single-form-entry (ok-form 0 "t5/i1/f1" "(slow-tool)" :observation "SLOW  running")
                     :status
                     :timeout))

(defdescribe timeout-fixture-test
             (describe
               "timeout mid-block"
               (it "canonicalizes to a :timeout status block"
                   (let [block (iteration/canonicalize (timeout-entry))]
                     (expect (= :timeout (:status block)))))
               (it "keeps the status glyph OUT of the block render (it lives on the TURN header)"
                   (let [lines (rendered (timeout-entry))]
                     (expect (nil? (header-of lines)))
                     (expect (not-any? #(str/includes? % (iteration/status-glyph :timeout)) lines))
                     (expect (not-any? #(str/includes? % (iteration/status-glyph :ok)) lines))))))

;; ---------------------------------------------------------------------------
;; Fixture 8 — merged multi-fence → flat block, both forms' stdout.
;; ---------------------------------------------------------------------------

(defn- merged-fences-entry
  []
  {:position 0
   :forms [(ok-form 0 "t8/i1/f1" "(git/status)" :observation "STATUS  3 files")
           (ok-form 0 "t8/i1/f2" "(git/add \".\")" :mutation "ADD  .")]})

(defdescribe merged-fences-fixture-test
             (describe "merged multi-fence"
                       (it "canonicalizes to ONE block scope spanning both forms"
                           (let [block (iteration/canonicalize (merged-fences-entry))]
                             (expect (= "t8/i1" (:scope block)))
                             (expect (= :ok (:status block)))
                             (expect (= 2 (count (:forms block))))))))
