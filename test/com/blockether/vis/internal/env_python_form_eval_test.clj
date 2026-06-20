(ns com.blockether.vis.internal.env-python-form-eval-test
  "The Python form-eval contract, as executable tests. (Supersedes the old
   FORM_EVAL_CONTRACT.md spec — that was a pre-implementation agreement; the
   behavior is shipped, so these tests are the living record.)

   Two halves:
   1. `run-python-block` per-form AST eval semantics (E1–E7 / R1–R7): the reply
      splits into top-level statements via CPython `ast`; a bare expression
      echoes its value, `x = …` echoes x, a comment is not a form, the last
      form's value is the turn result, and evaluation stops at the first error.
   2. Prose-leading SyntaxError detection: when the model opens with PROSE, the
      whole reply parses as Python and fails with a CPython error whose text
      varies by which mangled token trips first (apostrophe → unterminated
      string, `×` → invalid character, apostrophe-pairs → unmatched ')').
      `map-polyglot-error` tags these `:prose-leading? true` with an actionable
      message, while NEVER mislabeling a genuine code typo as prose."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.env-python :as ep]
   [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot PolyglotException]))

(defn- classify
  "Parse `code` (parse-only — never evaluates the forms). On SyntaxError, run it
   through `map-polyglot-error` and return the op-error map; otherwise `:parsed`."
  [code]
  (try (ep/count-top-level-forms code) :parsed
    (catch PolyglotException e (ep/map-polyglot-error e code))))

(defn- prose-leading?
  [code]
  (boolean (get-in (classify code) [:data :prose-leading?])))

(defn- glued-forms?
  [code]
  (boolean (get-in (classify code) [:data :glued-forms?])))

(defdescribe prose-leading-guard-test
  ;; --- positives: real failing replies seen live (sessions 2e98be97 / 4c0eff03)
  (it "flags markdown heading + prose (apostrophe -> unterminated string)"
    (expect (prose-leading? "## Root cause found\n\n`/draft` clones via rift's CoW.\ndone(\"x\")")))

  (it "flags a prose sentence with a unicode char (invalid character ×)"
    (expect (prose-leading? "Both dialogs now resolve to an identical box at 120×40.\ndone(\"\"\"ok\"\"\")")))

  (it "flags a prose sentence whose apostrophes orphan a paren (unmatched ')')"
    (expect (prose-leading? "I've spent enough (removing them didn't help).\ngit_status()")))

  ;; --- negatives: valid code or genuine code typos must NOT be flagged
  (it "does NOT flag valid code"
    (expect (= :parsed (classify "git_status()"))))

  (it "does NOT flag a real code typo whose first line is valid code"
    (expect (not (prose-leading? "git_status()\nx = (1 + 2"))))

  (it "does NOT flag a multiline call cut mid-construct (no prose signature)"
    (expect (not (prose-leading? "cat(\"a.clj\"\nfoo"))))

  (it "does NOT flag a comment followed by a code typo"
    (expect (not (prose-leading? "# read the file\nx = (1 + 2"))))

  ;; --- the message must name PROSE (not unicode/typo) to break the misdiagnosis loop
  (it "actionable message names prose, not the character that tripped"
    (let [msg (:message (classify "Both dialogs resolve at 120×40 now.\ndone(\"\"\"ok\"\"\")"))]
      (expect (str/includes? msg "PROSE"))))

  ;; --- a stray non-ASCII char in code position ANYWHERE (not just line 1) — the
  ;;     em-dash-at-line-71 gap the prose-leading detector (first line only) missed
  (it "flags a non-ASCII char in code position even mid-reply, with its line"
    (let [r (classify "x = 5\n# a note\ny = 3 — 1")]
      (expect (true? (get-in r [:data :non-ascii-in-code?])))
      (expect (= 3 (get-in r [:data :line])))
      (expect (str/includes? (:message r) "non-ASCII"))))

  (it "a leading-prose failure stays tagged prose-leading, not non-ascii"
    (let [r (classify "I've spent enough (removing them didn't help).\ngit_status()")]
      (expect (nil? (get-in r [:data :non-ascii-in-code?])))
      (expect (true? (get-in r [:data :prose-leading?]))))))

(defdescribe glued-top-level-forms-guard-test
  ;; --- positives: the OpenAI/Codex missing-newline pattern seen live (982f3716)
  (it "flags a call glued directly onto done(...)"
    (expect (glued-forms? "cat(\"a.clj\")done(\"\"\"ok\"\"\")")))

  (it "flags a done(...) answer glued onto the next call (\"\"\")rg(...))"
    (expect (glued-forms? "done(\"\"\"summary\"\"\")rg({\"any\": [\"x\"]})")))

  (it "flags a dict-arg call glued onto another (})patch([)"
    (expect (glued-forms? "cat(\"a\", {\"range\": [1, 9]})patch([{\"file\": \"a\"}])")))

  (it "actionable message names the one-statement-per-line fix"
    (let [msg (:message (classify "cat(\"a\")done(\"\"\"ok\"\"\")"))]
      (expect (str/includes? msg "OWN line"))))

  ;; --- negatives: properly separated or unrelated syntax errors must NOT flag
  (it "does NOT flag properly newline-separated forms (they parse)"
    (expect (= :parsed (classify "cat(\"a\")\ndone(\"\"\"ok\"\"\")"))))

  (it "does NOT flag an unterminated string (no glue signature)"
    (expect (not (glued-forms? "done(\"\"\"abc"))))

  (it "does NOT flag a mid-construct cut (no glue signature)"
    (expect (not (glued-forms? "x = (1 + 2"))))

  ;; --- disjoint from the other two classes
  (it "a prose-leading failure is not mislabeled glued"
    (let [r (classify "I've spent enough (removing them didn't help).\ngit_status()")]
      (expect (nil? (get-in r [:data :glued-forms?])))
      (expect (true? (get-in r [:data :prose-leading?]))))))

(def ^:private py-ctx
  ;; One shared GraalPy sandbox for the eval cases; unique var names per case
  ;; avoid cross-test global contamination. No tools needed — plain Python
  ;; exercises the per-form eval semantics (R1–R7).
  (delay (:python-context (ep/create-python-context {}))))

(defdescribe verb-arg-boundary-test
  ;; Regression: a Python dict passed to a wrapped Clojure verb crosses the
  ;; boundary via `->clj`, which KEYWORDIZES every dict key (snake verbatim).
  ;; The `sub_loop` verb once read its opts with the STRING key "models" and so
  ;; silently got nil — the child ran on the DEFAULT model, not the proposed
  ;; one. This pins the shape so verb authors read `:models`, not "models".
  (it "dict args arrive with KEYWORD-snake keys; values pass through (strings, vectors)"
    (let [captured (atom nil)
          {:keys [python-context]}
          (ep/create-python-context
            {'capture_args (fn [prompt subctx & more]
                             (reset! captured {:prompt prompt
                                               :subctx subctx
                                               :opts   (first more)})
                             "ok")})]
      (.eval python-context "python"
        "capture_args('go', {'tasks': {'oauth': {'status': 'doing'}}, 'focus': 'oauth'}, {'models': ['haiku', 'sonnet']})")
      (let [{:keys [prompt subctx opts]} @captured]
        (expect (= "go" prompt))
        ;; subctx: top + nested keys keywordized, leaf string values intact
        (expect (= #{:tasks :focus} (set (keys subctx))))
        (expect (= "oauth" (:focus subctx)))
        (expect (= "doing" (get-in subctx [:tasks :oauth :status])))
        ;; opts: the THING the bug missed — keyword key, NOT the string
        (expect (= ["haiku" "sonnet"] (:models opts)))
        (expect (nil? (get opts "models")))))))

(defdescribe cross-turn-var-persistence-test
  "The GraalPy sandbox is FRESH per turn, so a variable the model bound in a
   PAST turn is restored BY NAME from the rebind store (its pickled value) — the
   'globals persist across turns like a REPL' promise. This GUARDS the by-name
   var rebind, which must survive the removal of the dead r[\"tN/iN/fN\"]
   form-result memory."
  (it "a by-name variable survives into a fresh sandbox via rebind!"
    (let [ctx1 (:python-context (ep/create-python-context {}))
          r1   (ep/run-python-block ctx1 "kept_v = 41" "t1/i1")
          kv   (first (filter #(= "kept_v" (:bound-name %)) (:forms r1)))
          ctx2 (:python-context (ep/create-python-context {}))]
      ;; the assignment pickled its value + recorded the bound name
      (expect (= "kept_v" (:bound-name kv)))
      (expect (some? (:result-pickle kv)))
      ;; a fresh sandbox doesn't know kept_v until we rebind it by name
      (ep/rebind! ctx2 ["kept_v"]
        (fn [k] (when (= k "kept_v") {:pickle (:result-pickle kv)})))
      (let [r2  (ep/run-python-block ctx2 "print(kept_v + 1)")
            out (some :stdout (:forms r2))]
        (expect (= "42" (clojure.string/trim (str out))))))))

(defdescribe run-python-block-form-eval-test
  ;; (R8 in-fence r["tN/iN/fF"] memory removed: context is print-only — a later
  ;; line uses ordinary Python variables, not an r[] dict.)
  (it "E1 — comment is not a form; assign + bare expr; last value is the result"
    (let [r (ep/run-python-block @py-ctx "# read it\ne1x = 41\ne1x")]
      (expect (= 41 (:result r)))
      (expect (= 2 (count (:forms r))))
      (expect (nil? (:error r)))))

  (it "E2 — a single value-returning expression echoes its value"
    (let [r (ep/run-python-block @py-ctx "40 + 2")]
      (expect (= 42 (:result r)))
      (expect (= 1 (count (:forms r))))))

  (it "E3 — multiple statements; the trailing tuple echoes both"
    (let [r (ep/run-python-block @py-ctx "e3a = 1\ne3b = 2\n(e3a, e3b)")]
      (expect (= [1 2] (:result r)))
      (expect (= 3 (count (:forms r))))))

  (it "E6 — a call expression echoes its return value"
    (let [r (ep/run-python-block @py-ctx "str(99)")]
      (expect (= "99" (:result r)))))

  (it "a def is one form; a following call evaluates"
    (let [r (ep/run-python-block @py-ctx "def e_f():\n    return 7\ne_f()")]
      (expect (= 7 (:result r)))
      (expect (= 2 (count (:forms r))))))

  (it "E7 — evaluation stops at the first erroring form; later forms do not run"
    (let [r (ep/run-python-block @py-ctx "e7x = 1\ne7_boom\ne7y = 2")]
      (expect (nil? (:result r)))
      (expect (= :python/runtime (get-in (:error r) [:data :phase])))
      (expect (= 2 (count (:forms r))))
      (expect (some? (:error (last (:forms r)))))))) (defdescribe repair-glued-top-level-forms-test
  ;; PURE source rewrite: insert a newline at each glued boundary the detector
  ;; finds, and -- critically -- leave already-valid source byte-identical.
                                                       (it "splits a call glued directly onto another at the closing-paren boundary"
                                                         (expect (= "len([1,2])\nabs(-3)"
                                                                   (ep/repair-glued-top-level-forms "len([1,2])abs(-3)"))))

                                                       (it "splits at a closing triple-quote abutting the next call"
                                                         (expect (= "done(\"\"\"ok\"\"\")\nrg(x)"
                                                                   (ep/repair-glued-top-level-forms "done(\"\"\"ok\"\"\")rg(x)"))))

                                                       (it "splits at a closing brace/bracket boundary (})patch([)"
                                                         (expect (= "cat(\"a\", {\"r\": [1]})\npatch([1])"
                                                                   (ep/repair-glued-top-level-forms "cat(\"a\", {\"r\": [1]})patch([1])"))))

                                                       (it "inserts a newline at EVERY boundary when three forms are glued"
                                                         (expect (= "a(1)\nb(2)\nc(3)"
                                                                   (ep/repair-glued-top-level-forms "a(1)b(2)c(3)"))))

  ;; --- must NOT rewrite valid source (no false positives)
                                                       (it "leaves newline-separated forms unchanged"
                                                         (expect (= "cat(\"a\")\ndone(\"x\")"
                                                                   (ep/repair-glued-top-level-forms "cat(\"a\")\ndone(\"x\")"))))

                                                       (it "leaves a single valid form unchanged"
                                                         (expect (= "git_status()"
                                                                   (ep/repair-glued-top-level-forms "git_status()"))))

                                                       (it "leaves method chaining (closing-paren then dot) unchanged"
                                                         (expect (= "x.upper().strip()"
                                                                   (ep/repair-glued-top-level-forms "x.upper().strip()"))))

                                                       (it "is idempotent: repairing repaired source is a no-op"
                                                         (let [once (ep/repair-glued-top-level-forms "a(1)b(2)")]
                                                           (expect (= once (ep/repair-glued-top-level-forms once)))))) (defdescribe auto-repair-glued-run-python-block-test
  ;; the recurring OpenAI/Codex missing-newline failure now RUNS instead of bouncing
                                                                                                                         (it "runs a glued reply as multiple forms and returns the last value"
                                                                                                                           (let [r (ep/run-python-block @py-ctx "len([1,2])abs(-3)")]
                                                                                                                             (expect (= 3 (:result r)))
                                                                                                                             (expect (= 2 (count (:forms r))))
                                                                                                                             (expect (nil? (:error r)))))

                                                                                                                         (it "discloses the repaired source under :auto-repaired"
                                                                                                                           (let [r (ep/run-python-block @py-ctx "len([1,2])abs(-3)")]
                                                                                                                             (expect (some? (:auto-repaired r)))
                                                                                                                             (expect (= :glued-forms (get-in r [:auto-repaired :kind])))))

  ;; --- THE SAFETY GUARD: valid Python whose STRING ARG carries the glued
  ;;     signature (exactly what a clj_eval / clj_edit code argument looks like)
  ;;     parses fine, so it must run UNTOUCHED - never auto-repaired.
                                                                                                                         (it "does NOT repair valid code whose string contains a )ident( signature"
                                                                                                                           (let [r (ep/run-python-block @py-ctx "arA = \"(foo)bar(baz)\"\nlen(arA)")]
                                                                                                                             (expect (= 13 (:result r)))
                                                                                                                             (expect (nil? (:auto-repaired r)))
                                                                                                                             (expect (nil? (:error r)))))

                                                                                                                         (it "does NOT repair a single call whose string arg looks like glued clj source"
                                                                                                                           (let [r (ep/run-python-block @py-ctx "len(\"(do (foo))bar(x)\")")]
                                                                                                                             (expect (number? (:result r)))
                                                                                                                             (expect (nil? (:auto-repaired r)))
                                                                                                                             (expect (nil? (:error r)))))

  ;; --- the repair is gated behind the dynamic flag
                                                                                                                         (it "leaves a glued reply as a SyntaxError when auto-repair is disabled"
                                                                                                                           (binding [ep/*auto-repair-glued-forms?* false]
                                                                                                                             (let [r (ep/run-python-block @py-ctx "len([1,2])abs(-3)")]
                                                                                                                               (expect (nil? (:result r)))
                                                                                                                               (expect (some? (:error r)))
                                                                                                                               (expect (nil? (:auto-repaired r)))))))





(defdescribe sanitize-cause-data-test
  "Tool ex-data rides into the op-error `:data` MINUS host noise: the nested
   `:tool-result` envelope (a verbatim copy of the same failure) and the Java
   `:trace` are dropped; an inner `:error` that adds nothing over the
   top-level message disappears entirely; actionable fields survive."
  (it "collapses a :vis/tool-failure to type+symbol when the inner error is just the message"
    (let [msg "rg spec has unknown keys: spec."
          out (#'ep/sanitize-cause-data
                {:type :vis/tool-failure :symbol :rg
                 :error {:message msg :trace "clojure.lang.ExceptionInfo: ...\nframe - f.clj:1"}
                 :tool-result {:result nil :success? false
                               :error {:message msg :trace "..."}}}
                msg)]
      (expect (= {:type :vis/tool-failure :symbol :rg} out))))
  (it "keeps actionable inner-error fields, sans trace"
    (let [out (#'ep/sanitize-cause-data
                {:type :tool/banned
                 :error {:message "blocked" :reason :policy :trace "..."}}
                "other message")]
      (expect (= {:type :tool/banned :error {:message "blocked" :reason :policy}} out))))
  (it "passes non-map :error through untouched"
    (expect (= {:type :x :error "boom"}
              (#'ep/sanitize-cause-data {:type :x :error "boom" :tool-result {}} "boom")))))

(defdescribe fabricated-transcript-truncation-test
  "truncate-fabricated-results cuts a reply at the FIRST fabricated
   transcript line and keeps the genuine prefix. The regex must catch
   every shape seen in the wild — session 372994ce regenerated the
   whole agent loop (`_results <results scope=…>`, bare tags,
   `assistant#` role markers, echoed `SyntaxError:` feedback) and NONE
   of it matched the original `_result{`-only pattern, so the entire
   reply bounced as a SyntaxError instead of truncating."
  (it "original shape: _result{...} invented tool output"
    (expect (= "git_status()"
              (ep/truncate-fabricated-results
                "git_status()\n_result{\"branch\": \"main\"}\ngit_push()"))))
  (it "session-372994ce shape: _results <results scope=...> envelope"
    (expect (= "git_status()"
              (ep/truncate-fabricated-results
                "git_status()\n_results <results scope=\"t4/i1/f1\">\n{\"branch\": \"main\"}\ngit_push()"))))
  (it "bare <results ...> / </results> tag lines"
    (expect (= "git_add([\"a.clj\"])"
              (ep/truncate-fabricated-results
                "git_add([\"a.clj\"])\n<results scope=\"t4/i2\">\nstuff\n</results>\ngit_push()"))))
  (it "fabricated assistant# role marker"
    (expect (= "git_commit({\"message\": \"x\"})"
              (ep/truncate-fabricated-results
                "git_commit({\"message\": \"x\"})\nassistant# push to remote\ngit_push()"))))
  (it "echoed SyntaxError: rejection feedback"
    (expect (= "git_push()"
              (ep/truncate-fabricated-results
                "git_push()\nSyntaxError: invalid syntax (<unknown>, line 7)\ngit_push()"))))
  (it "session-d5a81236 shape: parroted ```ctx fence re-emitted after the genuine calls"
    ;; The model echoed the wire's rendered transcript back as its own reply:
    ;; ```session[...] glued after git_status(), then the # tool results heading and
    ;; r[...] = {...} JSON echoes. svar swallowed the whole fence tail into one
    ;; `python` block that could not parse. Cut at the first ``` fence line.
    (expect (= "rg({\"any\": [\"x\"]})\ngit_status()"
              (ep/truncate-fabricated-results
                (str "rg({\"any\": [\"x\"]})\ngit_status()\n"
                  "```ctx\n[\"env\"][\"nrepl\"] = 7888\n# tool results\n"
                  "r[\"t4/i1/f1\"] = {\"files\": [\"a.clj\"]}")))))
  (it "parroted r[\"tN/iN/fN\"] = {...} / = [...] results-wire echo (no leading fence)"
    (expect (= "cat(\"a.clj\")"
              (ep/truncate-fabricated-results
                "cat(\"a.clj\")\nr[\"t4/i1/f1\"] = {\"path\": \"a.clj\"}\ndone(\"\"\"x\"\"\")")))
    (expect (= "rg({\"any\": [\"x\"]})"
              (ep/truncate-fabricated-results
                "rg({\"any\": [\"x\"]})\nr[\"t1/i1/f2\"] = [1, 2, 3]"))))
  (it "legit code does NOT match: _result assignment, except/raise SyntaxError, r[...] READ, session[...] delta"
    (expect (nil? (ep/truncate-fabricated-results
                    "_result = git_add([\"a.clj\"])\nx = 1")))
    (expect (nil? (ep/truncate-fabricated-results
                    "try:\n    f()\nexcept SyntaxError:\n    pass\nraise SyntaxError(\"x\")")))
    ;; `r[...]` is the read-only prior-results store: READING it (subscript, no
    ;; assignment) is the model's normal job and must NOT truncate.
    (expect (nil? (ep/truncate-fabricated-results
                    "prev = r[\"t1/i1/f1\"]\ndone(\"\"\"saw \"\"\" + str(prev))")))
    ;; `ctx[...] = ...` session-bag deltas ARE a legit model write — only an
    ;; assignment to the `r[...]` results store is the parroted echo.
    (expect (nil? (ep/truncate-fabricated-results
                    "session[\"plan\"] = [\"step1\"]\ngit_status()"))))
  (it "reply that OPENS fabricated has no genuine prefix -> nil"
    (expect (nil? (ep/truncate-fabricated-results
                    "_results <results scope=\"t1/i1\">\ngit_push()")))))

(defdescribe auto-repair-parroted-fence-run-python-block-test
  "End-to-end: a reply that parrots the wire's rendered transcript (a ```ctx
   fence + `# tool results` heading + r[...] = {...} echoes) AFTER its genuine
   code no longer bounces as a 60 KB SyntaxError — run-python-block truncates at
   the first parroted line and RUNS only the genuine prefix (session d5a81236)."
  (it "recovers + runs the genuine prefix, dropping the parroted ```ctx tail"
    (let [r (ep/run-python-block @py-ctx
              (str "x_e2e = 1\nx_e2e + 41\n"
                "```ctx\n[\"env\"][\"nrepl\"] = 7888\n# tool results\n"
                "r[\"t4/i1/f1\"] = {\"files\": [\"a.clj\"]}"))]
      (expect (= 42 (:result r)))
      (expect (= 2 (count (:forms r))))
      (expect (nil? (:error r)))
      (expect (= :fabricated-results (get-in r [:auto-repaired :kind])))))
  (it "leaves the parroted reply as a SyntaxError when auto-repair is disabled"
    (binding [ep/*auto-repair-fabricated-results?* false]
      (let [r (ep/run-python-block @py-ctx "x_e2e = 1\n```ctx\nr[\"t1/i1/f1\"] = {}")]
        (expect (nil? (:result r)))
        (expect (some? (:error r)))
        (expect (nil? (:auto-repaired r)))))))

;; ---------------------------------------------------------------------------
;; cap-forms — the per-iteration FORM budget (one-shot backstop). Pure: takes
;; split forms ({:src :kind}) + cap + drop-done?, returns the forms to RUN plus
;; a trim note. First iteration = cap 4 + drop a beside-forms done(); later = 8.
;; ---------------------------------------------------------------------------
(defn- expr [s] {:src s :kind "expr"})
(def ^:private rd (expr "cat(\"a\")"))
(def ^:private dn (expr "done(\"\"\"x\"\"\")"))

(defdescribe cap-forms-test
  (it "a lone done() (pure answer) is never touched, even on the first reply"
    (let [{:keys [forms note]} (ep/cap-forms [dn] 4 true)]
      (expect (= [dn] forms))
      (expect (nil? note))))

  (it "first reply: drops a done() sitting beside other forms, keeps the reads"
    (let [{:keys [forms note]} (ep/cap-forms [rd rd dn] 4 true)]
      (expect (= [rd rd] forms))
      (expect (:dropped-done? note))
      (expect (= 0 (:dropped-extra note)))))

  (it "first reply: caps at 4 forms AFTER dropping done; extras do not run"
    (let [forms-in (vec (concat (repeat 6 rd) [dn]))
          {:keys [forms note]} (ep/cap-forms forms-in 4 true)]
      (expect (= 4 (count forms)))
      (expect (= 4 (:kept note)))
      (expect (= 7 (:total note)))
      (expect (:dropped-done? note))
      (expect (= 2 (:dropped-extra note)))))

  (it "later reply: caps at 8 forms, no done() drop"
    (let [forms-in (vec (repeat 10 rd))
          {:keys [forms note]} (ep/cap-forms forms-in 8 false)]
      (expect (= 8 (count forms)))
      (expect (false? (:dropped-done? note)))
      (expect (= 2 (:dropped-extra note)))))

  (it "later reply: done() beside a few reads under cap is kept, untouched"
    (let [in [rd rd rd dn]
          {:keys [forms note]} (ep/cap-forms in 8 false)]
      (expect (= in forms))
      (expect (nil? note))))

  (it "later reply: keeps done() appended after the capped reads"
    (let [forms-in (vec (concat (repeat 10 rd) [dn]))
          {:keys [forms note]} (ep/cap-forms forms-in 8 false)]
      (expect (= 9 (count forms)))            ;; 8 reads + the done
      (expect (= dn (last forms)))
      (expect (false? (:dropped-done? note)))))

  (it "under the cap with no done to drop = no trim, no note"
    (let [in [rd rd rd]
          {:keys [forms note]} (ep/cap-forms in 4 true)]
      (expect (= in forms))
      (expect (nil? note))))

  (it "empty / nil form list is returned untouched"
    (expect (= [] (:forms (ep/cap-forms [] 4 true))))
    (expect (nil? (:note (ep/cap-forms [] 4 true))))))

;; The cap enforced through the REAL eval path (run-python-block opts arg), not
;; just the pure helper — extras truly never execute, and a dropped done() never
;; evaluates (so an unbound done is harmless when it's trimmed pre-eval).
(defdescribe cap-forms-integration-test
  (it "first-iteration cap of 4 runs only the first 4 forms in the real engine"
    (let [r (ep/run-python-block @py-ctx "1\n2\n3\n4\n5\n6" "t1/i1"
              {:form-cap 4 :drop-done? true})]
      (expect (nil? (:error r)))
      (expect (= 4 (count (:forms r))))
      (expect (= 4 (:result r)))                    ;; last KEPT form's value
      (expect (= 4 (:kept (:forms-capped r))))
      (expect (= 2 (:dropped-extra (:forms-capped r))))))

  (it "a dropped done() never evaluates (unbound done would error if it ran)"
    ;; `done` is not bound in this bare test context; if the trim didn't happen
    ;; pre-eval, calling it would raise NameError. It doesn't — proof it's gone.
    (let [r (ep/run-python-block @py-ctx "7\n8\ndone(\"hi\")" "t1/i1"
              {:form-cap 4 :drop-done? true})]
      (expect (nil? (:error r)))
      (expect (= 2 (count (:forms r))))
      (expect (= 8 (:result r)))
      (expect (:dropped-done? (:forms-capped r)))))

  (it "no opts (later-reply default) runs every form, no cap note"
    (let [r (ep/run-python-block @py-ctx "1\n2\n3" "t1/i2")]
      (expect (= 3 (count (:forms r))))
      (expect (nil? (:forms-capped r))))))
