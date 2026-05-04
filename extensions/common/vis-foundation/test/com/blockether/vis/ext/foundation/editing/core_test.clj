(ns com.blockether.vis.ext.foundation.editing.core-test
  "Tests for the editing extension.

   Smoke-checks the loaded extension surface (symbol vector, doc
   strings, prompt fragment) plus behavioral coverage of the
   structured preview/search helpers (`v/cat`, `v/rg`) and the new
   thin babashka.fs wrappers (`v/read-all-lines`, `v/write-lines`,
   `v/update-file`, `v/list-dir`, `v/glob`, ...).

   Tests reach private fns directly through the registry to avoid
   bringing up a full SCI sandbox. Temp files land under
   `target/editing-test/` (always inside the repo cwd, so
   `safe-path` accepts them)."
  (:require
   [babashka.fs :as fs]
   [clojure.string :as string]
   [com.blockether.vis.ext.foundation.editing.core :as editing]
   [com.blockether.vis.internal.extension :as extension]
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
   missing. Used when a v/ tool takes a directory (vs. a file) and
   we must NOT spit into it."
  [name]
  (let [rel (str (temp-root) "/" name)]
    (fs/create-dirs rel)
    rel))

(defdescribe editing-extension-loads-test
  (it "exposes structured helpers plus the required thin babashka.fs wrappers"
    (expect (vector? editing/editing-symbols))
    (expect (= 16 (count editing/editing-symbols)))
    (expect (not-any? #{'edit 'write 'cwd 'parent 'file-name 'extension 'relativize}
              (map :ext.symbol/sym editing/editing-symbols)))
    (expect (some #{'read-all-lines}
              (map :ext.symbol/sym editing/editing-symbols)))
    (expect (some #{'update-file}
              (map :ext.symbol/sym editing/editing-symbols)))
    (expect (some #{'bash}
              (map :ext.symbol/sym editing/editing-symbols)))
    (expect (some #{'silent!}
              (map :ext.symbol/sym editing/editing-symbols))))

  (it "every editing symbol carries a non-blank :doc and an :arglists vector"
    (doseq [s editing/editing-symbols
            :let [doc      (:ext.symbol/doc s)
                  arglists (:ext.symbol/arglists s)]]
      (expect (string? doc))
      (expect (not (string/blank? doc)))
      (expect (or (vector? arglists) (seq? arglists)))))

  (it "exposes a non-blank prompt fragment"
    (expect (string? editing/editing-prompt))
    (expect (not (string/blank? editing/editing-prompt))))

  (it "pushes search/read/path discovery to v tools instead of bash text slicing"
    (expect (string/includes? editing/editing-prompt
              "Do not use v/bash for grep/sed/nl/cat/find"))
    (expect (string/includes? editing/editing-prompt
              "Use v/rg/v/cat/v/glob"))
    (let [bash-symbol (some #(when (= 'bash (:ext.symbol/sym %)) %)
                        editing/editing-symbols)]
      (expect (not-any? #(string/includes? % "grep")
                (:ext.symbol/examples bash-symbol)))))

  (it "teaches the model that file and shell payloads live under the tool envelope :result"
    (let [bash-symbol (some #(when (= 'bash (:ext.symbol/sym %)) %)
                        editing/editing-symbols)]
      (expect (string/includes? editing/editing-prompt "(get-in c [:result :lines])"))
      (expect (string/includes? editing/editing-prompt "never (:lines c) / (:content c)"))
      (expect (string/includes? editing/editing-prompt "(get-in run [:result :stdout])"))
      (expect (string/includes? (:ext.symbol/doc bash-symbol) ":result :stdout"))
      (expect (some #(string/includes? % "[:result :exit]")
                (:ext.symbol/examples bash-symbol))))))

(defdescribe vis-cat-structured-shape-test
  (it "returns structured pagination metadata plus raw :lines"
    (let [path (write-temp! "small.txt" "alpha\nbeta\ngamma\n")
          read-file (private-fn "read-file")
          out  (read-file path)]
      (expect (= #{:path :offset :total-lines :truncated-by :next-offset
                   :effective-limit :effective-char-limit :lines}
                (set (keys out))))
      (expect (string? (:path out)))
      (expect (= 1 (:offset out)))
      ;; str/split-lines drops the trailing empty after final \n.
      (expect (= 3 (:total-lines out)))
      (expect (= ["alpha" "beta" "gamma"] (:lines out)))
      (expect (= :end-of-file (:truncated-by out)))
      (expect (nil? (:next-offset out)))
      (expect (nil? (:effective-limit out)))
      (expect (= 6000 (:effective-char-limit out)))))

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
      (expect (= :line-limit (:truncated-by out)))
      (expect (= 7 (:next-offset out)))
      (expect (= 4 (:effective-limit out)))))

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

  (it "accepts :max-lines as a compatibility alias for :limit"
    (let [path (write-temp! "max-lines-alias.txt" "a\nb\nc\nd\n")
          read-file (private-fn "read-file")
          out (read-file path {:offset 2 :max-lines 2})]
      (expect (= ["b" "c"] (:lines out)))
      (expect (= 2 (:effective-limit out)))
      (expect (= 4 (:next-offset out)))
      (expect (= :line-limit (:truncated-by out)))))

  (it "rejects unknown, non-positive, and conflicting v/cat opts"
    (let [path (write-temp! "validate.txt" "x\n")
          read-file (private-fn "read-file")]
      (doseq [bad-opts [{:offset 0}
                        {:offset -1}
                        {:limit 0}
                        {:char-limit 0}
                        {:max-lines 0}
                        {:max-line 2}
                        {:max-lines 2 :limit 3}]]
        (expect (throws? clojure.lang.ExceptionInfo #(read-file path bad-opts)))))))

(defdescribe vis-silent-tool-test
  (it "returns only shape and marks the result silent"
    (let [silent-tool (private-fn "silent-tool")
          out (silent-tool {:huge (vec (range 100))
                            :nested {:answer "x"}})]
      (expect (extension/tool-result? out))
      (expect (= :vis/silent (:rendering-kind out)))
      (expect (= :markdown (:journal (extension/presentation out))))
      (expect (= {:type :map
                  :count 2
                  :keys [:huge :nested]
                  :shape {:huge {:type :vector
                                 :count 100
                                 :items {:type :int}}
                          :nested {:type :map
                                   :count 1
                                   :keys [:answer]
                                   :shape {:answer {:type :string
                                                    :chars 1
                                                    :lines 1}}}}}
                (:result out))))))

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

(defdescribe thin-bbfs-wrapper-test
  (it "read-all-lines returns raw line strings"
    (let [path           (write-temp! "bbfs/read.txt" "alpha\nbeta\n")
          read-all-lines (private-fn "read-all-lines-safe")]
      (expect (= ["alpha" "beta"]
                (read-all-lines path)))))

  (it "write-lines creates parent dirs and writes text"
    (let [path        "bbfs/write/out.txt"
          rooted-path (str (temp-root) "/" path)
          write-lines (private-fn "write-lines-safe")]
      (expect (= rooted-path
                (write-lines rooted-path ["a" "b"])))
      (expect (= "a\nb\n"
                (slurp rooted-path)))))

  (it "update-file mutates existing text and returns the new contents"
    (let [path        (write-temp! "bbfs/update.txt" "hello")
          update-file (private-fn "update-file-safe")]
      (expect (= "HELLO"
                (update-file path string/upper-case)))
      (expect (= "HELLO" (slurp path)))))

  (it "list-dir and glob return cwd-relative path strings"
    (let [_        (write-temp! "bbfs/tree/a.clj" "(ns a)")
          _        (write-temp! "bbfs/tree/b.txt" "b")
          list-dir (private-fn "list-dir-safe")
          glob     (private-fn "glob-safe")
          root     (str (temp-root) "/bbfs/tree")]
      (expect (= #{"target/editing-test/bbfs/tree/a.clj"
                   "target/editing-test/bbfs/tree/b.txt"}
                (set (list-dir root))))
      (expect (= ["target/editing-test/bbfs/tree/a.clj"]
                (glob root "*.clj")))))

  (it "exists? and delete-if-exists work on cwd-relative paths"
    (let [path             (write-temp! "bbfs/meta/x.txt" "x")
          exists?          (private-fn "exists-safe?")
          delete-if-exists (private-fn "delete-if-exists-safe")]
      (expect (true? (exists? path)))
      (expect (true? (delete-if-exists path)))
      (expect (false? (exists? path)))))

  (it "bash runs bounded commands inside the working tree"
    (let [run-bash (private-fn "run-bash-safe")
          out      (run-bash "printf '%s' hello && printf '%s' err >&2" {:timeout-ms 5000})]
      (expect (= 0 (:exit out)))
      (expect (= "hello" (:stdout out)))
      (expect (= "err" (:stderr out)))
      (expect (= "." (:cwd out)))
      (expect (false? (:timed-out? out)))))

  (it "bash validates cwd through the same safe path guard"
    (let [run-bash (private-fn "run-bash-safe")]
      (expect (throws? clojure.lang.ExceptionInfo #(run-bash "pwd" {:cwd ".."}))))))

(defdescribe tool-envelope-test
  (it "tool wrappers return the required contract keys"
    (let [path (write-temp! "contract/read.txt" "alpha\nbeta\n")
          read-all-lines (private-fn "read-all-lines-tool")
          out (read-all-lines path)]
      (expect (= #{:ok? :result :result-shape :provenance :error}
                (set (keys out))))
      (expect (true? (:ok? out)))
      (expect (= ["alpha" "beta"] (:result out)))
      (expect (string? (:markdown (extension/presentation out))))
      (expect (nil? (:error out)))))

  (it "tool failure contract includes structured :error with normalized trace"
    (let [read-all-lines (private-fn "read-all-lines-symbol")
          on-error       (:ext.symbol/on-error-fn read-all-lines)
          out            (:result (on-error (ex-info "boom" {}) nil nil ["missing.txt"]))]
      (expect (false? (:ok? out)))
      (expect (= nil (:result out)))
      (expect (= "clojure.lang.ExceptionInfo" (get-in out [:error :type])))
      (expect (vector? (get-in out [:error :trace])))
      (expect (string? (:markdown (extension/presentation out)))))))
