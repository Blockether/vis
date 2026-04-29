(ns com.blockether.vis.ext.foundation.editing.core-test
  "Tests for the editing extension.

   Smoke-checks the loaded extension surface (symbol vector, doc
   strings, prompt fragment) plus behavioral coverage of the two
   tools that returned strings under the legacy contract — vis/cat
   and vis/rg — now that both ship pure structured maps. The
   structured contracts are stable enough to assert key-by-key:

     (vis/cat path)             -> {:path :offset :total-lines :truncated-by :lines}
     (vis/rg patterns path)     -> {:hits :truncated-by}

   Tests reach `read-file` / `grep-files` directly through the
   private-fn registry to avoid bringing up a full SCI sandbox.
   Temp files land under `target/editing-test/` (always inside the
   repo cwd, so `safe-path` accepts them)."
  (:require
   [babashka.fs :as fs]
   [clojure.string :as string]
   [com.blockether.vis.ext.foundation.editing.core :as editing]
   [lazytest.core :refer [defdescribe expect it throws?]]))

(defn- private-fn [name]
  (deref (resolve (symbol "com.blockether.vis.ext.foundation.editing.core" name))))

(defn- temp-root
  "Cwd-relative path string for the shared temp root, idempotently
   created. Stays relative on purpose: `safe-path` resolves against
   `(fs/cwd)` and `fs/relativize` chokes when one arg is absolute and
   the other isn't."
  []
  (let [rel "target/editing-test"]
    (fs/create-dirs rel)
    rel))

(defn- write-temp! [name content]
  (let [rel (str (temp-root) "/" name)]
    (fs/create-dirs (fs/parent rel))
    (spit (fs/file rel) content)
    rel))

(defn- temp-dir-path
  "Cwd-relative directory path under the shared temp root, created if
   missing. Used when a vis/ tool takes a directory (vs. a file) and
   we must NOT spit into it."
  [name]
  (let [rel (str (temp-root) "/" name)]
    (fs/create-dirs rel)
    rel))

(defdescribe editing-extension-loads-test
  (it "exposes the canonical five-tool symbol vector"
    ;; cat / ls / rg / edit / write. zedit moved to vis-language-clojure.
    (expect (vector? editing/editing-symbols))
    (expect (= 5 (count editing/editing-symbols))))

  (it "every editing symbol carries a non-blank :doc and an :arglists vector"
    (doseq [s editing/editing-symbols
            :let [doc      (:ext.symbol/doc s)
                  arglists (:ext.symbol/arglists s)]]
      (expect (string? doc))
      (expect (not (string/blank? doc)))
      (expect (or (vector? arglists) (seq? arglists)))))

  (it "exposes a non-blank prompt fragment"
    (expect (string? editing/editing-prompt))
    (expect (not (string/blank? editing/editing-prompt)))))

(defdescribe vis-cat-structured-shape-test
  (it "returns a 5-key map: :path :offset :total-lines :truncated-by :lines"
    (let [path (write-temp! "small.txt" "alpha\nbeta\ngamma\n")
          read-file (private-fn "read-file")
          out  (read-file path)]
      (expect (= #{:path :offset :total-lines :truncated-by :lines}
                (set (keys out))))
      (expect (string? (:path out)))
      (expect (= 1 (:offset out)))
      ;; str/split-lines drops the trailing empty after final \n.
      (expect (= 3 (:total-lines out)))
      (expect (= ["alpha" "beta" "gamma"] (:lines out)))
      (expect (= :end-of-file (:truncated-by out)))))

  (it ":lines carries raw strings — no leading line-number prefix"
    (let [path (write-temp! "raw.txt" "   indented\nplain\n")
          read-file (private-fn "read-file")
          out  (read-file path)]
      ;; The pre-structured contract prepended `\"%4d  \"`. The new
          ;; contract preserves the file's exact bytes per line so
          ;; the model can compute on raw content without parsing.
      (expect (= ["   indented" "plain"] (:lines out)))))

  (it ":offset + :limit slice the file 1-based, :truncated-by signals state"
    (let [path (write-temp! "slice.txt"
                 (string/join "\n" (map #(str "line" %) (range 1 11))))
          read-file (private-fn "read-file")
          out  (read-file path {:offset 3 :limit 4})]
      (expect (= 3 (:offset out)))
      (expect (= 10 (:total-lines out)))
      (expect (= ["line3" "line4" "line5" "line6"] (:lines out)))
      ;; Took 4 of remaining 8 lines — limit hit, more file remains.
      (expect (= :line-limit (:truncated-by out)))))

  (it ":truncated-by :char-limit when the slice overflows :char-limit"
    (let [;; 10 lines * (line<n>=6 chars + \n) ~= 70 chars total. A
          ;; 20-char cap should let through ~3 lines (\"line1\\nline2\\nline3\")
          ;; before the budget runs out.
          path (write-temp! "chartrunc.txt"
                 (string/join "\n" (map #(str "line" %) (range 1 11))))
          read-file (private-fn "read-file")
          out  (read-file path {:char-limit 20})]
      (expect (= :char-limit (:truncated-by out)))
      (expect (pos? (count (:lines out))))
      (expect (every? #(string/starts-with? % "line") (:lines out)))))

  (it ":offset past EOF yields empty :lines + :truncated-by :end-of-file"
    (let [path (write-temp! "two.txt" "a\nb\n")
          read-file (private-fn "read-file")
          out  (read-file path {:offset 99})]
      (expect (= [] (:lines out)))
      (expect (= 99 (:offset out)))
      (expect (= 2 (:total-lines out)))
      (expect (= :end-of-file (:truncated-by out)))))

  (it "rejects non-positive :offset / :limit / :char-limit"
    (let [path (write-temp! "validate.txt" "x\n")
          read-file (private-fn "read-file")]
      (doseq [bad-opts [{:offset 0} {:offset -1} {:limit 0} {:char-limit 0}]]
        (expect (throws? clojure.lang.ExceptionInfo #(read-file path bad-opts)))))))

(defdescribe vis-rg-structured-shape-test
  (it "returns a 2-key map: :hits + :truncated-by"
    (let [_   (write-temp! "rg/a.txt" "alpha needle gamma\nbeta\n")
          _   (write-temp! "rg/b.txt" "plain line\nanother needle here\n")
          grep (private-fn "grep-files")
          out  (grep ["needle"] (temp-dir-path "rg"))]
      (expect (= #{:hits :truncated-by} (set (keys out))))
      (expect (vector? (:hits out)))
      ;; Every hit is a clean {:path :line :text} map, no sentinel.
      (expect (every? #(= #{:path :line :text} (set (keys %))) (:hits out)))
      (expect (= 2 (count (:hits out))))
      (expect (= :end-of-results (:truncated-by out)))))

  (it ":truncated-by :limit when results exceed the cap"
    (let [;; Seed 5 hits across two files, ask for limit=2.
          _ (write-temp! "rgcap/a.txt"
              (string/join "\n" ["needle 1" "needle 2" "needle 3"]))
          _ (write-temp! "rgcap/b.txt"
              (string/join "\n" ["needle 4" "needle 5"]))
          grep (private-fn "grep-files")
          out  (grep ["needle"] (temp-dir-path "rgcap") {:limit 2})]
      (expect (= 2 (count (:hits out))))
      (expect (= :limit (:truncated-by out)))))

  (it "empty result still has :truncated-by :end-of-results, never nil"
    (let [_ (write-temp! "rgmiss/a.txt" "nothing matches in here\n")
          grep (private-fn "grep-files")
          out  (grep ["definitely-not-present"] (temp-dir-path "rgmiss"))]
      (expect (= [] (:hits out)))
      (expect (= :end-of-results (:truncated-by out))))))
