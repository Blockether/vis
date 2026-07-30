(ns com.blockether.vis.ext.language-clojure.format-test
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.language-clojure.format :as fmt]
            [lazytest.core :refer [defdescribe expect it]]))

;; `format-string` runs cljfmt: it normalizes indentation + whitespace of
;; MULTI-LINE forms on write. It deliberately does NOT reflow a one-liner
;; into multiple lines (a cljfmt non-goal) — the line breaks must come from
;; the caller's source.
(defdescribe format-string-test
             (it "normalizes indentation of a mis-indented multi-line form"
                 (let
                   [src
                    "(defn foo [x]\n(let [y (inc x)]\n(* y 2)))"

                    out
                    (fmt/format-string src)]

                   (expect (string? out))
                   (expect (not= src out))
                   ;; nested forms indented under their parent, not flush-left
                   (expect (str/includes? out "\n  (let"))
                   (expect (str/includes? out "\n    (* y 2)"))))
             (it "leaves a one-liner on one line (cljfmt does not reflow)"
                 (let
                   [src
                    "(defn foo [x] (* x 2))"

                    out
                    (fmt/format-string src)]

                   (expect (= 1 (count (str/split-lines out))))))
             (it "returns the source unchanged when it cannot parse"
                 (let [bad "(defn foo [x"] ;; unbalanced — cljfmt throws, we keep source
                   (expect (= bad (fmt/format-string bad)))))
             (it "handles empty / nil safely"
                 (expect (= "" (fmt/format-string "")))
                 (expect (nil? (fmt/format-string nil)))))

;; `format-source` memoizes on (backend, governing config file + its mtime,
;; source). zprint/cljfmt are pure functions of (source, opts), so the key is
;; total: only an edited source, an edited config, or a different backend may
;; change the answer — and each of those MISSES.
(defdescribe format-source-cache-test
             (it "returns the very same string object on a repeat format"
                 (fmt/clear-result-cache!)
                 (let
                   [src
                    "(defn foo [x]\n(let [y (inc x)]\n(* y 2)))"

                    a
                    (fmt/format-source src nil)

                    b
                    (fmt/format-source src nil)]

                   (expect (not= src a))
                   ;; identical?, not = : a recomputation would build a new string
                   (expect (identical? a b))))
             (it "seeds the OUTPUT as its own fixed point, so re-formatting it hits"
                 (fmt/clear-result-cache!)
                 (let
                   [src
                    "(defn foo [x]\n(let [y (inc x)]\n(* y 2)))"

                    out
                    (fmt/format-source src nil)]

                   (expect (not= src out))
                   ;; never computed directly, yet already cached: identical?
                   ;; proves the seeded entry answered instead of zprint.
                   (expect (identical? out (fmt/format-source out nil)))))
             (it "recomputes after clear-result-cache!"
                 (fmt/clear-result-cache!)
                 (let
                   [src
                    "(defn foo [x]\n(let [y (inc x)]\n(* y 2)))"

                    a
                    (fmt/format-source src nil)

                    _
                    (fmt/clear-result-cache!)

                    b
                    (fmt/format-source src nil)]

                   (expect (= a b))
                   (expect (not (identical? a b)))))
             (it "misses on a different source"
                 (fmt/clear-result-cache!)
                 (let
                   [a
                    (fmt/format-source "(defn foo [x]\n(* x 2))" nil)

                    b
                    (fmt/format-source "(defn bar [x]\n(* x 3))" nil)]

                   (expect (not= a b))))
             (it "misses when the governing zprint config is edited"
                 (fmt/clear-result-cache!)
                 (let
                   [dir
                    (doto (java.io.File. (System/getProperty "java.io.tmpdir")
                                         (str "vis-fmt-cache-" (System/nanoTime)))
                      (.mkdirs))

                    cfg
                    (java.io.File. dir ".zprint.edn")

                    src-file
                    (java.io.File. dir "a.clj")

                    src
                    "(defn foo [aaaaaaaaaa bbbbbbbbbb] (+ aaaaaaaaaa bbbbbbbbbb aaaaaaaaaa))"]

                   (try (spit cfg "{:width 120}")
                        (spit src-file src)
                        (let [wide (fmt/format-source src (.getPath src-file))]
                          ;; a NEW mtime + new width must not serve the wide answer
                          (spit cfg "{:width 30}")
                          (.setLastModified cfg (+ (.lastModified cfg) 2000))
                          (let [narrow (fmt/format-source src (.getPath src-file))]
                            (expect (= :zprint (fmt/formatter-for (.getPath src-file))))
                            (expect (not= wide narrow))
                            (expect (< (count (str/split-lines wide))
                                       (count (str/split-lines narrow))))))
                        (finally (run! #(.delete ^java.io.File %) [cfg src-file dir]))))))
