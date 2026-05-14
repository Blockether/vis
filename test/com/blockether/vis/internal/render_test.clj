(ns com.blockether.vis.internal.render-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.prompt]
            [com.blockether.vis.internal.render :as render]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe answer-ir-lazy-seq-test
  (it "realizes lazy Hiccup children before canonicalization and caps infinite seqs"
    (let [ast (render/->ast
                [:ir [:ul (map (fn [n] [:li (str "item " n)]) (range))]])
          items (->> (nth ast 2) (drop 2) vec)
          rendered (render/render ast :plain)
          printed (pr-str ast)]
      (expect (= :ir (first ast)))
      (expect (= 101 (count items)))
      (expect (str/includes? rendered "item 0"))
      (expect (str/includes? rendered "item 99"))
      (expect (not (str/includes? rendered "item 100")))
      (expect (str/includes? rendered "… many more"))
      (expect (not (str/includes? printed "LazySeq"))))))

(defn- tags-in
  [x]
  (let [out (volatile! [])]
    (letfn [(walk [v]
              (when (vector? v)
                (vswap! out conj (first v))
                (doseq [c (drop 2 v)] (walk c))))]
      (walk x)
      @out)))

(defdescribe answer-ir-retired-disclosure-test
  (it "does not canonicalize :details or :summary as answer IR tags"
    (let [ast (render/->ast
                [:ir [:details {:open? false}
                      [:summary "Plan"]
                      [:p "body"]]])]
      (expect (render/canonical? ast))
      (expect (not-any? #{:details :summary} (tags-in ast)))
      (expect (= "Planbody" (render/render ast :plain)))
      (expect (not (str/includes? (render/render ast :markdown) "<details")))
      (expect (not (str/includes? (render/render ast :markdown) "<summary")))))

  ;; Removed: "removes disclosure tags from the system answer grammar".
  ;; The system prompt was slimmed and no longer carries the
  ;; `forbidden := :details | :summary` directive in prose; the
  ;; canonical grammar still excludes :details/:summary, asserted
  ;; structurally above (`canonical?` + `not-any? #{:details :summary}`).
  )

;; ============================================================================
;; Per-form silent rendering inside a mixed block (P1.1).
;;
;; With per-block eval, one Markdown code block can contain multiple top-level
;; forms. The renderer must split each block's source structurally and decide
;; per-form what to render and what to hide:
;;
;;   {:kind :code        :source "..."}   visible code (regular forms, comments)
;;   {:kind :title       :value "X"}      `(set-conversation-title! "X")` form
;;                                          — channel renders as banner, not code
;;   {:kind :answer-ref}                   `(turn-answer! ...)` form
;;                                          — channel hides; answer IR renders below
;;
;; Parser failure degrades gracefully: one `:kind :code` segment with the
;; full source. The model gets to self-correct from the SCI error next iter.
;; ============================================================================

(defdescribe code-block-segments-test
  (it "keeps a pure-code block as a single :code segment"
    (let [segs (render/code-block-segments "(def helper-x 1)\n(def helper-y 2)")]
      (expect (= 1 (count segs)))
      (expect (= :code (:kind (first segs))))
      (expect (= "(def helper-x 1)\n(def helper-y 2)" (:source (first segs))))))

  (it "extracts a lone (turn-answer! ...) form as a single :answer-ref segment"
    (let [segs (render/code-block-segments "(turn-answer! [:ir [:p \"hi\"]])")]
      (expect (= 1 (count segs)))
      (expect (= :answer-ref (:kind (first segs))))))

  (it "extracts a lone (set-conversation-title! ...) form as a :title segment carrying the title value"
    (let [segs (render/code-block-segments "(set-conversation-title! \"Mixed forms probe\")")]
      (expect (= 1 (count segs)))
      (expect (= :title (:kind (first segs))))
      (expect (= "Mixed forms probe" (:value (first segs))))))

  (it "splits a mixed block into ordered :code / :title / :answer-ref segments"
    (let [src (str "(def helper-x 1)\n"
                "(set-conversation-title! \"Mixed forms probe\")\n"
                "(turn-answer! [:ir [:p \"Done\"]])")
          segs (render/code-block-segments src)]
      (expect (= 3 (count segs)))
      (expect (= [:code :title :answer-ref] (mapv :kind segs)))
      (expect (= "(def helper-x 1)" (:source (first segs))))
      (expect (= "Mixed forms probe" (:value (second segs))))))

  (it "groups consecutive plain-code forms into one :code segment, splitting only at structural boundaries"
    ;; (def a 1) and (def b 2) are both regular code — collapse them; the
    ;; (turn-answer! ...) form is the boundary.
    (let [src (str "(def a 1)\n(def b 2)\n(turn-answer! [:ir [:p \"done\"]])")
          segs (render/code-block-segments src)]
      (expect (= 2 (count segs)))
      (expect (= [:code :answer-ref] (mapv :kind segs)))
      (expect (= "(def a 1)\n(def b 2)" (:source (first segs))))))

  (it "strips nested (turn-answer! ...) from a `do` body — surrounding code stays"
    ;; Nested filtered calls are pruned from displayed `:source`; the `do`
    ;; body keeps the rest. SCI still evaluates the original block, so the
    ;; answer-emission side effect runs as before.
    (let [segs (render/code-block-segments "(do (def x 1) (turn-answer! [:ir [:p \"hi\"]]))")]
      (expect (= 1 (count segs)))
      (expect (= :code (:kind (first segs))))
      (expect (not (re-find #"turn-answer!" (:source (first segs)))))
      (expect (re-find #"\(def x 1\)" (:source (first segs))))))

  (it "strips nested (set-conversation-title! ...) from a `do` body — actual repro"
    ;; The block-source that surfaced the bug — model wrote `(do <title> <work>)`
    ;; in a single iteration; the title call must not appear in the trace.
    (let [src  (str "(do\n"
                 "  (set-conversation-title! \"Polish casual greeting\")\n"
                 "  (def user-msg \"siema\")\n"
                 "  (= user-msg \"siema\"))")
          segs (render/code-block-segments src)]
      (expect (= 1 (count segs)))
      (expect (= :code (:kind (first segs))))
      (expect (not (re-find #"set-conversation-title!" (:source (first segs)))))
      (expect (re-find #"user-msg" (:source (first segs))))))

  (it "drops the :code segment when a wrapper hosts ONLY filtered calls"
    ;; `(do (set-conversation-title! "X"))` collapses to `(do)` after pruning;
    ;; the segment goes away entirely and the block reads as silent.
    (expect (= [] (render/code-block-segments "(do (set-conversation-title! \"X\"))")))
    (expect (= [] (render/code-block-segments
                    "(do (set-conversation-title! \"X\") (turn-answer! [:ir [:p \"d\"]]))"))))

  (it "preserves position-sensitive slots by replacing filtered calls with nil"
    ;; `if` / `let`-binding / `cond` / `case` etc. lose their meaning if you
    ;; just DROP a sub-form. Replace with nil so the shape stays intact and
    ;; the model can self-correct from a real evaluation trace.
    (let [if-segs   (render/code-block-segments "(if c (set-conversation-title! \"X\") :else)")
          let-segs  (render/code-block-segments "(let [_ (set-conversation-title! \"X\")] :ok)")
          cond-segs (render/code-block-segments "(cond a (set-conversation-title! \"X\") :else :y)")]
      (expect (re-find #"\(if c nil :else\)" (:source (first if-segs))))
      (expect (re-find #"\(let \[_ nil\] :ok\)" (:source (first let-segs))))
      (expect (re-find #"\(cond a nil :else :y\)" (:source (first cond-segs))))))

  (it "leaves quoted data untouched — (quote ...) is data, not a call"
    ;; A literal `'(set-conversation-title! "X")` inside a `do` is data the
    ;; model is reasoning about, not a call to suppress.
    (let [segs (render/code-block-segments "(do '(set-conversation-title! \"X\") :ok)")]
      (expect (= 1 (count segs)))
      (expect (re-find #"set-conversation-title!" (:source (first segs))))))

  (it "falls back to one :code segment when the source fails to parse"
    ;; Renderer never throws on bad source. Engine surfaces SCI's parse
    ;; error separately; renderer just shows the bytes verbatim.
    (let [segs (render/code-block-segments "(this is (not balanced")]
      (expect (= 1 (count segs)))
      (expect (= :code (:kind (first segs))))
      (expect (= "(this is (not balanced" (:source (first segs))))))

  (it "returns empty vec for blank source"
    (expect (= [] (render/code-block-segments "")))
    (expect (= [] (render/code-block-segments "   \n  \n")))
    (expect (= [] (render/code-block-segments nil))))

  (it "preserves leading comments alongside the following form in the same :code segment"
    ;; Comments are part of the model's natural prose; they belong with the
    ;; form they annotate, not as their own segment.
    (let [src (str ";; weakest assumption: a greeting\n"
                "(def msg \"witam\")\n"
                "(turn-answer! [:ir [:p \"hi\"]])")
          segs (render/code-block-segments src)]
      (expect (= 2 (count segs)))
      (expect (= [:code :answer-ref] (mapv :kind segs)))
      (expect (re-find #"weakest assumption" (:source (first segs)))))))

(defdescribe block-structurally-silent?-test
  "True iff a block's segments collapse to *only* structural forms
   (`:title` / `:answer-ref`). Engine stamps the persisted block + stream
   chunk with `:vis/silent? true` so channels that don't parse segments
   can still drop the entry from default display."
  (it "true for a pure-answer block"
    (expect (true? (render/block-structurally-silent?
                     "(turn-answer! [:ir [:p \"done\"]])"))))

  (it "true for a pure-title block"
    (expect (true? (render/block-structurally-silent?
                     "(set-conversation-title! \"X\")"))))

  (it "true for title-then-answer with no code"
    (expect (true? (render/block-structurally-silent?
                     "(set-conversation-title! \"X\")\n(turn-answer! [:ir [:p \"d\"]])"))))

  (it "false for a mixed block (def + answer)"
    (expect (false? (render/block-structurally-silent?
                      "(def x 1)\n(turn-answer! [:ir [:p \"d\"]])"))))

  (it "false for a pure-code block"
    (expect (false? (render/block-structurally-silent?
                      "(def x 1)"))))

  (it "true for a `(do …)` whose only payload is filtered calls"
    ;; Wrapper-only-filtered blocks now collapse to no segments via
    ;; `prune-filtered`; `block-structurally-silent?` must treat the
    ;; resulting empty segment list as silent, same as the top-level case.
    (expect (true? (render/block-structurally-silent?
                     "(do (set-conversation-title! \"X\"))")))
    (expect (true? (render/block-structurally-silent?
                     "(do (set-conversation-title! \"X\") (turn-answer! [:ir [:p \"d\"]]))")))))
