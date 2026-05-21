(ns com.blockether.vis.ext.foundation-core.editing.patch-test
  "Codex `apply_patch` envelope parity for `v/patch`.

   Covers parser shape, fuzzy seek-sequence (exact / rstrip / trim /
   Unicode-normalize), compute-update line-replacement, and the
   filesystem-applying surface (`patch-envelope-safe` /
   `patch-envelope-check`) wired through `patch-tool`.

   Temp files under `target/editing-test/` stay inside cwd so
   `safe-path` accepts them."
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [com.blockether.vis.ext.foundation-core.editing.core :as editing]
   [com.blockether.vis.ext.foundation-core.editing.patch :as patch]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- private-fn [name]
  (deref (resolve (symbol "com.blockether.vis.ext.foundation-core.editing.core" name))))

(defn- temp-root []
  (let [rel "target/editing-test/patch-suite"]
    (fs/create-dirs rel)
    rel))

(defn- temp-path [name]
  (let [rel (str (temp-root) "/" name)]
    (fs/create-dirs (fs/parent rel))
    rel))

(defn- write-temp! [name content]
  (let [rel (temp-path name)]
    (spit (fs/file rel) content)
    rel))

(defdescribe parse-patch-shape-test
  (it "looks-like-patch? recognizes envelope markers"
    (expect (true? (patch/looks-like-patch? "*** Begin Patch\n*** End Patch\n")))
    (expect (true? (patch/looks-like-patch? "  *** Begin Patch\n*** End Patch\n")))
    (expect (false? (patch/looks-like-patch? "hello world")))
    (expect (false? (patch/looks-like-patch? [{:path "x" :search "a" :replace "b"}])))
    (expect (false? (patch/looks-like-patch? nil))))

  (it "parses Add/Update/Delete/Move into structured hunks"
    (let [src (str "*** Begin Patch\n"
                "*** Add File: hello.txt\n"
                "+Hello\n"
                "+World\n"
                "*** Update File: src/app.py\n"
                "*** Move to: src/main.py\n"
                "@@ def greet():\n"
                "-print(\"Hi\")\n"
                "+print(\"Hello\")\n"
                "*** Delete File: old.txt\n"
                "*** End Patch")
          {:keys [hunks]} (patch/parse-patch src)]
      (expect (= 3 (count hunks)))
      (expect (= :add (-> hunks (nth 0) :op)))
      (expect (= "hello.txt" (-> hunks (nth 0) :path)))
      (expect (= "Hello\nWorld\n" (-> hunks (nth 0) :contents)))
      (expect (= :update (-> hunks (nth 1) :op)))
      (expect (= "src/main.py" (-> hunks (nth 1) :move-to)))
      (expect (= "def greet():" (-> hunks (nth 1) :chunks first :change-context)))
      (expect (= ["print(\"Hi\")"] (-> hunks (nth 1) :chunks first :old-lines)))
      (expect (= ["print(\"Hello\")"] (-> hunks (nth 1) :chunks first :new-lines)))
      (expect (= :delete (-> hunks (nth 2) :op)))
      (expect (= "old.txt" (-> hunks (nth 2) :path)))))

  (it "rejects malformed patch text with line-number diagnostics"
    (let [err (try (patch/parse-patch "no markers here")
                (catch clojure.lang.ExceptionInfo e e))]
      (expect (some? err))
      (expect (= :ext.foundation.editing/invalid-patch (:type (ex-data err))))))

  (it "preserves an empty Add File body"
    (let [src "*** Begin Patch\n*** Add File: empty.txt\n*** End Patch"
          h (-> (patch/parse-patch src) :hunks first)]
      (expect (= :add (:op h)))
      (expect (= "" (:contents h))))))

(defdescribe seek-sequence-fuzzy-test
  (it "exact match wins"
    (expect (= 1 (patch/seek-sequence ["a" "b" "c"] ["b"] 0 false))))

  (it "ignores trailing whitespace"
    (expect (= 0 (patch/seek-sequence ["foo   "] ["foo"] 0 false))))

  (it "ignores leading+trailing whitespace"
    (expect (= 0 (patch/seek-sequence ["   foo  "] ["foo"] 0 false))))

  (it "normalizes typographic punctuation"
    (expect (= 0 (patch/seek-sequence ["it\u2019s"] ["it's"] 0 false)))
    (expect (= 0 (patch/seek-sequence ["a\u2014b"] ["a-b"] 0 false))))

  (it "returns nil when pattern not present"
    (expect (nil? (patch/seek-sequence ["a" "b"] ["c"] 0 false))))

  (it "returns nil when pattern longer than input (no panic)"
    (expect (nil? (patch/seek-sequence ["only"] ["too" "long"] 0 false))))

  (it "prefers EOF position when eof? is true"
    (expect (= 2 (patch/seek-sequence ["x" "y" "x"] ["x"] 0 true)))))

(defdescribe seek-sequence-with-pass-test
  ;; seek-sequence-with-pass is what powers the line-based fuzzy fallback
  ;; in v/patch exact-replace mode. It MUST report which fuzzy strategy
  ;; actually fired so the model can tell the difference between an
  ;; expected exact hit and a salvage — the prompt nudges humans toward
  ;; that information.
  (it "reports :exact for clean matches"
    (expect (= {:start 1 :pass :exact}
              (patch/seek-sequence-with-pass ["a" "b" "c"] ["b"] 0 false))))

  (it "reports :rstrip when the file has trailing whitespace but the pattern does not"
    (let [hit (patch/seek-sequence-with-pass ["foo   "] ["foo"] 0 false)]
      (expect (= :rstrip (:pass hit)))))

  (it "reports :trim when both sides differ in leading/trailing whitespace"
    (let [hit (patch/seek-sequence-with-pass ["   foo  "] ["foo"] 0 false)]
      (expect (= :trim (:pass hit)))))

  (it "reports :unicode when typographic punctuation differs"
    (let [hit (patch/seek-sequence-with-pass ["it\u2019s late"] ["it's late"] 0 false)]
      (expect (= :unicode (:pass hit)))))

  (it "reports :relative-indent when only the absolute indentation differs"
    ;; Same relative structure (2-line block, deeper inner line) but
    ;; the file's block lives 4 spaces deeper than the SEARCH block.
    (let [hit (patch/seek-sequence-with-pass
                ["    def f():" "        return 1"]
                ["def f():" "    return 1"]
                0 false)]
      (expect (= :relative-indent (:pass hit)))
      (expect (= 4 (:indent-delta hit)))))

  (it "returns nil when no pass succeeds"
    (expect (nil? (patch/seek-sequence-with-pass ["a" "b"] ["c"] 0 false)))))

(defdescribe apply-indent-delta-test
  (it "is a no-op when delta is zero"
    (expect (= ["a" "b"] (patch/apply-indent-delta 0 ["a" "b"]))))

  (it "pads each non-blank line with N leading spaces for positive delta"
    (expect (= ["    a" "" "    b"]
              (patch/apply-indent-delta 4 ["a" "" "b"]))))

  (it "strips up to N leading whitespace chars for negative delta and leaves blanks alone"
    (expect (= ["a" "" "b"]
              (patch/apply-indent-delta -4 ["    a" "" "    b"]))))

  (it "never strips more than a line actually has"
    (expect (= ["a" "b"]
              (patch/apply-indent-delta -10 ["  a" " b"])))))

(defdescribe char-offset-helpers-test
  (it "char-offset-at-line returns 0 for the first line"
    (expect (= 0 (patch/char-offset-at-line "a\nb\nc\n" 0))))

  (it "char-offset-at-line points past the newline of the previous line"
    (expect (= 2 (patch/char-offset-at-line "a\nb\nc\n" 1)))
    (expect (= 4 (patch/char-offset-at-line "a\nb\nc\n" 2))))

  (it "char-offset-at-line clamps to (count content) when past EOF"
    (expect (= 6 (patch/char-offset-at-line "a\nb\nc\n" 99))))

  (it "split-content-lines drops the trailing empty element from a final newline"
    (expect (= ["a" "b" "c"] (patch/split-content-lines "a\nb\nc\n")))
    (expect (= ["a" "b" "c"] (patch/split-content-lines "a\nb\nc")))
    (expect (= [""] (patch/split-content-lines "\n")))))

(defdescribe compute-update-test
  (it "replaces a single matched line"
    (let [r (patch/compute-update
              {:original "alpha\nbeta\ngamma\n"
               :chunks [{:change-context nil
                         :old-lines ["beta"]
                         :new-lines ["BETA"]
                         :end-of-file? false}]
               :path "x"})]
      (expect (= "alpha\nBETA\ngamma\n" (:new r)))))

  (it "honors @@ change-context to disambiguate"
    (let [r (patch/compute-update
              {:original "def foo():\n  pass\n\ndef bar():\n  pass\n"
               :chunks [{:change-context "def bar():"
                         :old-lines ["  pass"]
                         :new-lines ["  return 1"]
                         :end-of-file? false}]
               :path "x"})]
      (expect (= "def foo():\n  pass\n\ndef bar():\n  return 1\n" (:new r)))))

  (it "supports pure additions at EOF when old-lines is empty"
    (let [r (patch/compute-update
              {:original "a\nb\n"
               :chunks [{:change-context nil
                         :old-lines []
                         :new-lines ["c"]
                         :end-of-file? false}]
               :path "x"})]
      (expect (str/includes? (:new r) "c"))))

  (it "raises a structured error when old lines cannot be located"
    (let [err (try (patch/compute-update
                     {:original "alpha\n"
                      :chunks [{:change-context nil
                                :old-lines ["missing"]
                                :new-lines ["x"]
                                :end-of-file? false}]
                      :path "x"})
                (catch clojure.lang.ExceptionInfo e e))]
      (expect (some? err))
      (expect (= :ext.foundation.editing/patch-old-lines-not-found
                (:type (ex-data err)))))))

(defdescribe envelope-apply-test
  (it "applies a Codex Update hunk through patch-envelope-safe"
    (let [f (write-temp! "envelope/a.txt" "def greet():\n    print(\"Hi\")\n    return 1\n")
          envelope (private-fn "patch-envelope-safe")
          env (str "*** Begin Patch\n"
                "*** Update File: " f "\n"
                "@@ def greet():\n"
                "-    print(\"Hi\")\n"
                "+    print(\"Hello\")\n"
                "*** End Patch\n")
          result (envelope env)]
      (expect (= 1 (count result)))
      (expect (= :update (-> result first :op)))
      (expect (= "def greet():\n    print(\"Hello\")\n    return 1\n" (slurp f)))))

  (it "applies Add + Delete in one envelope"
    (let [add-path (temp-path "envelope/added.txt")
          _ (fs/delete-if-exists add-path)
          del-path (write-temp! "envelope/del.txt" "bye\n")
          envelope (private-fn "patch-envelope-safe")
          env (str "*** Begin Patch\n"
                "*** Add File: " add-path "\n"
                "+hello\n"
                "+world\n"
                "*** Delete File: " del-path "\n"
                "*** End Patch\n")
          result (envelope env)]
      (expect (= 2 (count result)))
      (expect (= "hello\nworld\n" (slurp add-path)))
      (expect (false? (fs/exists? del-path)))))

  (it "applies Update + Move atomically"
    (let [src (write-temp! "envelope/old.txt" "line1\nline2\n")
          dest (temp-path "envelope/renamed.txt")
          _ (fs/delete-if-exists dest)
          envelope (private-fn "patch-envelope-safe")
          env (str "*** Begin Patch\n"
                "*** Update File: " src "\n"
                "*** Move to: " dest "\n"
                "@@\n"
                "-line1\n"
                "+LINE1\n"
                " line2\n"
                "*** End Patch\n")
          result (envelope env)]
      (expect (= :update-move (-> result first :op)))
      (expect (false? (fs/exists? src)))
      (expect (= "LINE1\nline2\n" (slurp dest)))))

  (it "rejects path escape outside cwd"
    (let [envelope (private-fn "patch-envelope-safe")
          err (try (envelope "*** Begin Patch\n*** Add File: ../escape.txt\n+hi\n*** End Patch\n")
                (catch clojure.lang.ExceptionInfo e e))]
      (expect (some? err))
      (expect (= :ext.foundation.editing/path-escape (:type (ex-data err))))))

  (it "validates entire envelope before any write (all-or-nothing)"
    (let [keep-path (write-temp! "envelope/keep.txt" "keep me\n")
          bad-target (write-temp! "envelope/bad.txt" "I have only this\n")
          envelope (private-fn "patch-envelope-safe")
          env (str "*** Begin Patch\n"
                "*** Update File: " keep-path "\n"
                "@@\n"
                "-keep me\n"
                "+kept\n"
                "*** Update File: " bad-target "\n"
                "@@\n"
                "-not in file\n"
                "+x\n"
                "*** End Patch\n")
          err (try (envelope env)
                (catch clojure.lang.ExceptionInfo e e))]
      (expect (some? err))
      (expect (= "keep me\n" (slurp keep-path)))
      (expect (= "I have only this\n" (slurp bad-target))))))

(defdescribe envelope-check-test
  (it "patch-envelope-check is read-only and reports valid? true"
    (let [f (write-temp! "envelope/check.txt" "x\ny\n")
          check (private-fn "patch-envelope-check")
          env (str "*** Begin Patch\n*** Update File: " f
                "\n@@\n-x\n+X\n*** End Patch\n")
          out (check env)]
      (expect (= true (:valid? out)))
      (expect (= :codex-apply-patch (:mode out)))
      (expect (= 1 (count (:checks out))))
      (expect (= "x\ny\n" (slurp f)))))

  (it "patch-envelope-check returns failures for unresolvable hunks"
    (let [f (write-temp! "envelope/check-fail.txt" "x\ny\n")
          check (private-fn "patch-envelope-check")
          env (str "*** Begin Patch\n*** Update File: " f
                "\n@@\n-NOPE\n+y\n*** End Patch\n")
          out (check env)]
      (expect (= false (:valid? out)))
      (expect (seq (:failures out))))))

(defdescribe patch-tool-dispatch-test
  (it "patch-tool routes string -> envelope, vec -> exact-replace"
    (let [f (write-temp! "dispatch/a.txt" "alpha\nbeta\ngamma\n")
          patch-tool (private-fn "patch-tool")
          exact-result (patch-tool [{:path f :search "beta" :replace "BETA"}])
          env (str "*** Begin Patch\n*** Update File: " f
                "\n@@\n-gamma\n+GAMMA\n*** End Patch\n")
          env-result (patch-tool env)]
      (expect (= "alpha\nBETA\nGAMMA\n" (slurp f)))
      (expect (some? exact-result))
      (expect (some? env-result))))

  (it "v/patch envelope mode appears in editing prompt"
    ;; RLM prompt mentions the envelope under EDIT / \"Codex envelope\".
    (expect (str/includes? editing/editing-prompt "Codex envelope"))))

(defdescribe codex-parity-edge-cases-test
  (it "strips Codex heredoc wrapper (<<'EOF' ... EOF) before parsing"
    (let [wrapped (str "<<'EOF'\n"
                    "*** Begin Patch\n"
                    "*** Add File: hello.txt\n"
                    "+hi\n"
                    "*** End Patch\n"
                    "EOF")
          {:keys [hunks]} (patch/parse-patch wrapped)]
      (expect (= 1 (count hunks)))
      (expect (= :add (-> hunks first :op)))
      (expect (= "hi\n" (-> hunks first :contents)))))

  (it "accepts the unquoted heredoc form <<EOF ... EOF"
    (let [wrapped (str "<<EOF\n*** Begin Patch\n*** Delete File: x.txt\n*** End Patch\nEOF")]
      (expect (= [:delete] (mapv :op (:hunks (patch/parse-patch wrapped)))))))

  (it "accepts the double-quoted heredoc form <<\"EOF\" ... EOF"
    (let [wrapped (str "<<\"EOF\"\n*** Begin Patch\n*** Delete File: x.txt\n*** End Patch\nEOF")]
      (expect (= [:delete] (mapv :op (:hunks (patch/parse-patch wrapped)))))))

  (it "parses optional *** Environment ID: <id> preamble"
    (let [src (str "*** Begin Patch\n"
                "*** Environment ID: env-abc-123\n"
                "*** Delete File: x.txt\n"
                "*** End Patch\n")
          parsed (patch/parse-patch src)]
      (expect (= "env-abc-123" (:environment-id parsed)))
      (expect (= [:delete] (mapv :op (:hunks parsed))))))

  (it "rejects empty environment-id"
    (let [src "*** Begin Patch\n*** Environment ID:   \n*** Delete File: x.txt\n*** End Patch\n"
          err (try (patch/parse-patch src)
                (catch clojure.lang.ExceptionInfo e e))]
      (expect (some? err))
      (expect (str/includes? (ex-message err) "environment_id cannot be empty"))))

  (it "tolerates leading whitespace on inner marker lines"
    (let [src (str "*** Begin Patch\n"
                "   *** Add File: hello.txt\n"
                "+hi\n"
                "  *** End Patch")
          {:keys [hunks]} (patch/parse-patch src)]
      (expect (= [:add] (mapv :op hunks)))
      (expect (= "hi\n" (-> hunks first :contents)))))

  (it "always emits exactly one trailing newline (Codex new-content invariant)"
    (let [r1 (patch/compute-update {:original "a\nb\n"
                                    :chunks [{:change-context nil
                                              :old-lines ["a"]
                                              :new-lines ["A"]
                                              :end-of-file? false}]
                                    :path "x"})
          r2 (patch/compute-update {:original "a\nb"
                                    :chunks [{:change-context nil
                                              :old-lines ["a"]
                                              :new-lines ["A"]
                                              :end-of-file? false}]
                                    :path "x"})]
      (expect (str/ends-with? (:new r1) "\n"))
      (expect (str/ends-with? (:new r2) "\n"))
      (expect (not (str/ends-with? (:new r1) "\n\n")))
      (expect (not (str/ends-with? (:new r2) "\n\n")))))

  (it "chained @@ headers each advance line_index (multi-context disambiguation)"
    ;; Codex docs: multiple @@ statements jump to nested contexts.
    (let [src (str "class A:\n"
                "  def f():\n"
                "    pass\n"
                "class B:\n"
                "  def f():\n"
                "    pass\n")
          r (patch/compute-update
              {:original src
               :chunks [{:change-context "class B:"
                         :old-lines []
                         :new-lines []
                         :end-of-file? false}
                        {:change-context "  def f():"
                         :old-lines ["    pass"]
                         :new-lines ["    return 1"]
                         :end-of-file? false}]
               :path "x"})]
      (expect (str/includes? (:new r) "class A:\n  def f():\n    pass"))
      (expect (str/includes? (:new r) "class B:\n  def f():\n    return 1"))))

  (it "looks-like-patch? recognizes heredoc-wrapped envelopes for dispatch"
    (expect (true? (patch/looks-like-patch? "<<'EOF'\n*** Begin Patch\n*** End Patch\nEOF")))
    (expect (true? (patch/looks-like-patch? "<<EOF\n*** Begin Patch\n*** End Patch\nEOF")))
    (expect (true? (patch/looks-like-patch? "<<\"EOF\"\n*** Begin Patch\n*** End Patch\nEOF")))))
