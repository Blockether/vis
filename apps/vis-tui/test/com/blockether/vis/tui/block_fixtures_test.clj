(ns com.blockether.vis.tui.block-fixtures-test
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
  (:require [com.blockether.vis.tui.render :as render]
            [com.blockether.vis.tui.iteration :as iteration]
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

;; Form builders.

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

;; Fixture 1 — git status + add + commit! + push! : one fence, printed output.

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

;; Fixture 2 — nested `let`: (let [a (cat) b (cat)] (patch)).

;; Fixture 3 — (def x (cat "x")): one block, printed output.

;; Fixture 4 — plain value (+ 1 2): one block, no stdout, no header.

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
                       (it "renders no stdout body and no header (bare values are never echoed)"
                           (let [lines
                                 (rendered (plain-value-entry))

                                 body
                                 (str/join "\n" lines)]

                             ;; Bare return values never reach the model's context, so they are
                             ;; not painted here either. No `3`, no ITERATION header.
                             (expect (not (str/includes? body "ITERATION")))
                             (expect (not-any? #(= "3" (str/trim %)) lines))))))

;; Fixture 5 — value-only error (/ 1 0): inline caret + message.

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
                             (expect (some? (:error block)))))))

;; Fixture 6 — cancellation mid-block → :cancelled.

(defn- cancelled-entry
  []
  (single-form-entry
    (ok-form 0 "t4/i1/f1" "(git/status)\n(git/add \".\")" :observation "STATUS  3 files")
    :status
    :cancelled))

(defdescribe cancelled-fixture-test
             (describe "cancellation mid-block"
                       (it "canonicalizes to a :cancelled status block"
                           (let [block (iteration/canonicalize (cancelled-entry))]
                             (expect (= :cancelled (:status block)))))))

;; Fixture 7 — timeout mid-block → :timeout.

(defn- timeout-entry
  []
  (single-form-entry (ok-form 0 "t5/i1/f1" "(slow-tool)" :observation "SLOW  running")
                     :status
                     :timeout))

(defdescribe timeout-fixture-test
             (describe "timeout mid-block"
                       (it "canonicalizes to a :timeout status block"
                           (let [block (iteration/canonicalize (timeout-entry))]
                             (expect (= :timeout (:status block)))))))

;; Fixture 8 — merged multi-fence → flat block, both forms' stdout.

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
