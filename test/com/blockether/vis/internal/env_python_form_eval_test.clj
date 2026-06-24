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

(def ^:private py-ctx
  ;; One shared GraalPy sandbox for the eval cases; unique var names per case
  ;; avoid cross-test global contamination. No tools needed — plain Python
  ;; exercises the per-form eval semantics (R1–R7).
  (delay (:python-context (ep/create-python-context {}))))

(defdescribe sandbox-auto-import-test
  (it "makes shlex available without an import in run_python code"
    (let [r (ep/run-python-block @py-ctx "shlex.quote('a b')")]
      (expect (nil? (:error r)))
      (expect (= "'a b'" (:result r)))))

  (it "makes json available without an import in run_python code"
    (let [r (ep/run-python-block @py-ctx "json.dumps({'b': 2, 'a': 1}, sort_keys=True)")]
      (expect (nil? (:error r)))
      (expect (= "{\"a\": 1, \"b\": 2}" (:result r))))

    (it "does not expose auto-imported modules as apropos-listed tools/globals"
      (let [r (ep/run-python-block @py-ctx "bool(set(['shlex', 'json']) & set(apropos('shlex') + apropos('json')))")]
        (expect (nil? (:error r)))
        (expect (= false (:result r)))))))

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
      ;; Tools are async-deferred — run through run-python-block so the bare
      ;; top-level call is SETTLED (executed) before we inspect the capture.
      (ep/run-python-block python-context
        "capture_args('go', {'tasks': {'oauth': {'status': 'doing'}}, 'focus': 'oauth'}, {'models': ['haiku', 'sonnet']})"
        "t1/i1")
      (let [{:keys [prompt subctx opts]} @captured]
        (expect (= "go" prompt))
        ;; subctx: top + nested keys keywordized, leaf string values intact
        (expect (= #{:tasks :focus} (set (keys subctx))))
        (expect (= "oauth" (:focus subctx)))
        (expect (= "doing" (get-in subctx [:tasks :oauth :status])))
        ;; opts: the THING the bug missed — keyword key, NOT the string
        (expect (= ["haiku" "sonnet"] (:models opts)))
        (expect (nil? (get opts "models")))))))

(defdescribe protected-tool-name-test
  (let [mk (fn [] (:python-context (ep/create-python-context
                                     {'patch (fn [& _] "patched")})))]
    (it "refuses to overwrite a bound tool name and keeps the callable usable"
      (let [ctx (mk)
            r1  (ep/run-python-block ctx "patch = 'not callable'" "t1/i1")
            r2  (ep/run-python-block ctx "patch({'path': 'x'})" "t1/i2")]
        (expect (= :python/protected-name (get-in r1 [:error :data :phase])))
        (expect (str/includes? (get-in r1 [:error :message]) "patch"))
        (expect (nil? (:error r2)))
        (expect (= "patched" (:result r2)))))
    (it "allows ordinary variables while still awaiting protected tools"
      (let [r (ep/run-python-block (mk) "css = 'app.css'
await patch({'path': css})" "t1/i1")]
        (expect (nil? (:error r)))
        (expect (= "patched" (:result r)))))
    (it "also protects tools added after context creation"
      (let [ctx (:python-context (ep/create-python-context {}))]
        (ep/set-python-binding! ctx 'later_patch (fn [& _] "late"))
        (let [r1 (ep/run-python-block ctx "later_patch = 'oops'" "t1/i1")
              r2 (ep/run-python-block ctx "later_patch()" "t1/i2")]
          (expect (= :python/protected-name (get-in r1 [:error :data :phase])))
          (expect (nil? (:error r2)))
          (expect (= "late" (:result r2))))))))

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

(defdescribe async-runtime-test
  "Async-by-default (maki-style on GraalPy): tools are DEFERRED, so `await` runs
   them ANYWHERE (incl. nested), a bare top-level call auto-settles, and an
   unawaited call that leaks into output repr's a loud hint instead of silently
   misbehaving. The await path AST-wraps the program in an `async def` (GraalPy
   rejects top-level await) and drives it, persisting assigned vars by name."
  (let [mk (fn [] (:python-context (ep/create-python-context
                                     {'echo (fn [x] (str "<" x ">"))})))]
    (it "await runs a NESTED deferred tool call"
      (let [r (ep/run-python-block (mk) "print(await echo(\"hi\"))" "t1/i1")]
        (expect (nil? (:error r)))
        (expect (= "<hi>" (clojure.string/trim (str (some :stdout (:forms r))))))))
    (it "a bare top-level call auto-settles (runs without await)"
      (let [r (ep/run-python-block (mk) "echo(\"bare\")" "t1/i1")]
        (expect (nil? (:error r)))
        (expect (= "<bare>" (:result (first (:forms r)))))))
    (it "an UNawaited nested call repr's a loud hint, not the value"
      (let [r (ep/run-python-block (mk) "print(echo(\"oops\"))" "t1/i1")]
        (expect (nil? (:error r)))
        (expect (clojure.string/includes?
                  (str (some :stdout (:forms r))) "unawaited async tool call"))))
    (it "the await path persists assigned vars by name (bound-name + pickle)"
      (let [r  (ep/run-python-block (mk) "kept = await echo(\"x\")\nprint(kept)" "t1/i1")
            bn (filter :bound-name (:forms r))]
        (expect (nil? (:error r)))
        (expect (= "<x>" (clojure.string/trim (str (some :stdout (:forms r))))))
        (expect (some #(and (= "kept" (:bound-name %)) (:result-pickle %)) bn))))
    (it "auto-settles a bare deferred assignment in an await-bearing program"
      ;; `c = await echo("a")` forces the async path; the bare `res = echo("b")`
      ;; has NO await, yet must RUN (settle) so `res` is the value, not a thunk.
      (let [r (ep/run-python-block (mk)
                "c = await echo(\"a\")\nres = echo(\"b\")\nprint(res)" "t1/i1")]
        (expect (nil? (:error r)))
        (expect (= "<b>" (clojure.string/trim (str (some :stdout (:forms r))))))
        (expect (some #(and (= "res" (:bound-name %)) (:result-pickle %))
                  (filter :bound-name (:forms r))))))
    (it "auto-settles a bare deferred assignment EXACTLY once (no double-run)"
      (let [calls (atom 0)
            ctx   (:python-context
                    (ep/create-python-context
                      {'tick (fn [] (str "n" (swap! calls inc)))}))
            r     (ep/run-python-block ctx
                    "c = await tick()\nres = tick()\nprint(res)" "t1/i1")]
        (expect (nil? (:error r)))
        ;; the bare `res = tick()` settles inline exactly once — not twice from
        ;; a redundant wrap + post-drive pass (and `tick` from the awaited form
        ;; ran once too).
        (expect (= "n2" (clojure.string/trim (str (some :stdout (:forms r))))))
        (expect (= 2 @calls))))))

(defdescribe anchor-helper-test
  "Pure Python helpers make cat→patch ergonomic without adding a text-patch mode."
  (let [mk (fn [] (:python-context (ep/create-python-context {})))
        sample "c = {'anchors': {'10:aaa': 'alpha', '11:bbb': 'beta target', '12:ccc': 'target gamma'}}"]
    (it "selects substring and exact anchors"
      (let [r (ep/run-python-block (mk)
                (str sample "\nprint(anchor(c, 'beta') + '\\n' + anchor_exact(c, 'alpha'))")
                "t1/i1")]
        (expect (nil? (:error r)))
        (expect (= "11:bbb\n10:aaa"
                  (str/trim (str (some :stdout (:forms r))))))))
    (it "rejects too-short anchor needles"
      (let [r (ep/run-python-block (mk)
                (str sample "\nanchor(c, 'a b')")
                "t1/i1")]
        (expect (= :python/runtime (get-in r [:error :data :phase])))
        (expect (str/includes? (get-in r [:error :message]) "at least 3"))))
    (it "accepts exact lineno:hash anchors already seen in cat output"
      (let [r (ep/run-python-block (mk)
                (str sample "\nprint(anchor(c, '11:bbb'))")
                "t1/i1")]
        (expect (nil? (:error r)))
        (expect (= "11:bbb" (str/trim (str (some :stdout (:forms r))))))))
    (it "lists candidate lines for ambiguous substring matches"
      (let [r (ep/run-python-block (mk)
                (str sample "\nprint(anchor(c, 'target', nth=2))")
                "t1/i1")]
        (expect (nil? (:error r)))
        (expect (= "12:ccc" (str/trim (str (some :stdout (:forms r)))))))
      (let [r (ep/run-python-block (mk)
                (str sample "\nanchor(c, 'target')")
                "t1/i1")]
        (expect (= :python/runtime (get-in r [:error :data :phase])))
        (expect (str/includes? (get-in r [:error :message]) "ambiguous anchor"))
        (expect (str/includes? (get-in r [:error :message]) "11:bbb"))
        (expect (str/includes? (get-in r [:error :message]) "beta target"))))
    (it "offers an anchors inspector instead of forcing edit-by-trial-error"
      (let [r (ep/run-python-block (mk)
                (str sample "
print(anchors(c, 'target'))")
                "t1/i1")]
        (expect (nil? (:error r)))
        (expect (str/includes? (str (some :stdout (:forms r))) "'anchor': '11:bbb'"))
        (expect (str/includes? (str (some :stdout (:forms r))) "target gamma"))))

    (it "builds patch edit maps and inclusive span maps from selected anchors"
      (let [single (ep/run-python-block (mk)
                     "c = {'anchors': {'10:aaa': 'alpha'}}\nedit(c, 'p.clj', 'lph', 'A')"
                     "t1/i1")
            guarded (ep/run-python-block (mk)
                      "c = {'anchors': {'10:aaa': 'alpha'}, 'mtime': 123, 'size': 5}\nedit(c, 'p.clj', 'lph', 'A')"
                      "t1/i1")
            span   (ep/run-python-block (mk)
                     (str sample "\nedit_span(c, 'p.clj', 'beta', 'gamma', 'BG')")
                     "t1/i1")]
        (expect (nil? (:error single)))
        (expect (nil? (:error guarded)))
        (expect (nil? (:error span)))
        (expect (= {:path "p.clj" :from_anchor "10:aaa" :replace "A"}
                  (:result single)))
        (expect (= {:path "p.clj" :from_anchor "10:aaa" :replace "A"
                    :expected_mtime 123 :expected_size 5}
                  (:result guarded)))
        (expect (= {:path "p.clj" :from_anchor "11:bbb" :to_anchor "12:ccc" :replace "BG"}
                  (:result span))))))
  (it "exposes exists as a compatibility alias for is_exists"
    (let [ctx (:python-context
               (ep/create-python-context
                 {'exists? (fn [path]
                             {:path path
                              :exists? (= path "present.txt")})}))
          via-exists (ep/run-python-block ctx "await exists('present.txt')" "t1/i1")
          via-snake  (ep/run-python-block ctx "await is_exists('missing.txt')" "t1/i2")]
      (expect (nil? (:error via-exists)))
      (expect (nil? (:error via-snake)))
      (expect (= {:path "present.txt" :exists true}
                (:result via-exists)))
      (expect (= {:path "missing.txt" :exists false}
                (:result via-snake))))
    (let [ctx (:python-context (ep/create-python-context {}))]
      (ep/set-python-binding! ctx 'exists? (fn [path] {:path path :exists? true}))
      (expect (= {:path "dynamic.txt" :exists true}
                (:result (ep/run-python-block ctx "await exists('dynamic.txt')" "t1/i3"))))
      (ep/remove-python-binding! ctx 'exists?)
      (expect (str/includes? (get-in (ep/run-python-block ctx "exists('dynamic.txt')" "t1/i4")
                               [:error :message])
                "`exists` is not defined")))))

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
      (expect (some? (:error (last (:forms r)))))
      ;; the 1 statement AFTER the error (`e7y = 2`) was skipped — disclosed so
      ;; the loop can tell the model it was NOT applied.
      (expect (= 1 (:skipped-forms r)))))

  (it "E7b — a NameError for an undefined TOOL gets an enrichment hint (toggled-off extension)"
    (let [r   (ep/run-python-block @py-ctx "shell_run(\"echo hi\")")
          err (:error r)]
      (expect (true? (get-in err [:data :name-undefined?])))
      (expect (= "shell_run" (get-in err [:data :undefined-name])))
      (expect (str/includes? (:message err) "apropos"))
      (expect (str/includes? (:message err) ":shell/enabled")))))

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

;; ---------------------------------------------------------------------------
;; cap-forms — the per-iteration FORM budget (one-shot backstop). Pure: takes
;; split forms ({:src :kind}) + cap, returns the first `cap` forms to RUN plus
;; a trim note when the block exceeded the budget.
;; ---------------------------------------------------------------------------
(defn- expr [s] {:src s :kind "expr"})
(def ^:private rd (expr "cat(\"a\")"))

(defdescribe cap-forms-test
  (it "caps at N forms; extras do not run"
    (let [forms-in (vec (repeat 6 rd))
          {:keys [forms note]} (ep/cap-forms forms-in 4)]
      (expect (= 4 (count forms)))
      (expect (= 4 (:kept note)))
      (expect (= 6 (:total note)))
      (expect (= 2 (:dropped-extra note)))))

  (it "caps at 8 forms"
    (let [forms-in (vec (repeat 10 rd))
          {:keys [forms note]} (ep/cap-forms forms-in 8)]
      (expect (= 8 (count forms)))
      (expect (= 2 (:dropped-extra note)))))

  (it "under the cap = no trim, no note"
    (let [in [rd rd rd]
          {:keys [forms note]} (ep/cap-forms in 4)]
      (expect (= in forms))
      (expect (nil? note))))

  (it "no cap (nil) = every form kept, no note"
    (let [in (vec (repeat 10 rd))
          {:keys [forms note]} (ep/cap-forms in nil)]
      (expect (= in forms))
      (expect (nil? note))))

  (it "empty / nil form list is returned untouched"
    (expect (= [] (:forms (ep/cap-forms [] 4))))
    (expect (nil? (:note (ep/cap-forms [] 4))))))

;; The cap enforced through the REAL eval path (run-python-block opts arg), not
;; just the pure helper — extras truly never execute.
(defdescribe cap-forms-integration-test
  (it "cap of 4 runs only the first 4 forms in the real engine"
    (let [r (ep/run-python-block @py-ctx "1\n2\n3\n4\n5\n6" "t1/i1"
              {:form-cap 4})]
      (expect (nil? (:error r)))
      (expect (= 4 (count (:forms r))))
      (expect (= 4 (:result r)))                    ;; last KEPT form's value
      (expect (= 4 (:kept (:forms-capped r))))
      (expect (= 2 (:dropped-extra (:forms-capped r))))))

  (it "no opts (later-reply default) runs every form, no cap note"
    (let [r (ep/run-python-block @py-ctx "1\n2\n3" "t1/i2")]
      (expect (= 3 (count (:forms r))))
      (expect (nil? (:forms-capped r))))))

(defdescribe no-auto-repair-test
  "Auto-repair and fabrication/glued detection were REMOVED (2026-06-21). A reply
   that fails to split is NOT salvaged — it errors as a plain SyntaxError and the
   model resends clean code. With native tool calling (one run_python code arg)
   the block-concat causes don't arise; :auto-repaired is always nil."
  (it "GLUED top-level forms ERROR as a SyntaxError (not repaired)"
    (let [r (ep/run-python-block @py-ctx "len([1,2])abs(-3)")]
      (expect (nil? (:result r)))
      (expect (some? (:error r)))
      (expect (= :python/syntax (get-in r [:error :data :phase])))
      (expect (nil? (:auto-repaired r)))))

  (it "a PARROTED transcript tail (```ctx + r[...]= echoes) ERRORS as a SyntaxError, not salvaged"
    (let [r (ep/run-python-block @py-ctx
              (str "x_e2e = 1\nx_e2e + 41\n"
                "```ctx\n[\"env\"][\"nrepl\"] = 7888\n# tool results\n"
                "r[\"t4/i1/f1\"] = {\"files\": [\"a.clj\"]}"))]
      (expect (nil? (:result r)))
      (expect (some? (:error r)))
      (expect (= :python/syntax (get-in r [:error :data :phase])))
      (expect (nil? (:auto-repaired r)))))

  (it "clean Python still runs untouched (no false repair)"
    (let [r (ep/run-python-block @py-ctx "len([1,2,3])")]
      (expect (= 3 (:result r)))
      (expect (nil? (:error r)))
      (expect (nil? (:auto-repaired r))))))
