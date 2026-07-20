(ns com.blockether.vis.ext.channel-tui.highlight-test
  "Regressions for the fence syntax-highlighter's caching + streaming path.

   The native tree-sitter `Highlighter` is stateless (full re-parse per call), so
   the streaming path memoizes the SETTLED prefix and re-parses only the growing
   last line. These tests pin two invariants: finalized (non-live) colouring is
   byte-identical to a bare native parse, and a live growing fence parses its
   settled prefix exactly once. Everything is guarded on `native-ready?` so a bare
   JVM without the platform lib (fail-open to nil) still passes."
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.highlight :as hl]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [dev.kreuzberg.treesitterlanguagepack Highlighter]))

(def ^:private native? @@#'hl/native-ready?)

(defn- native
  "Bare native colorize — the reference the non-live path must match byte-for-byte."
  [grammar ^String source]
  (Highlighter/highlightAnsi source grammar @@#'hl/theme-map))

(def ^:private multiline-fences
  [["clojure" "(defn foo\n  \"a docstring\n   spanning lines\"\n  [x]\n  (+ x 1))\n"]
   ["python" "def f(x):\n    '''\n    multi\n    line\n    '''\n    return x\n"]
   ["javascript" "const a = 1\nfunction g() { return a }\n"]
   ["json" "{\"a\": 1,\n \"b\": [2, 3]}\n"]])

(defdescribe highlight-safety-test
             (it "fails open: nil grammar / empty source never throw and yield nil"
                 (expect (nil? (hl/highlight nil "(+ 1 2)")))
                 (expect (nil? (hl/highlight "clojure" "")))
                 (expect (nil? (hl/highlight "clojure" nil)))))

(defdescribe highlight-finalized-exact-test
             (it "finalized (non-live) colouring is byte-identical to a bare native parse"
                 (when native?
                   (doseq [[g s] multiline-fences]
                     (expect (= (native g s) (hl/highlight g s))))))
             (it
               "a finalized fence re-parsed after a streaming run still matches native (self-heal)"
               (when native?
                 (reset! @#'hl/stream-memo {})
                 (let
                   [[g s]
                    (first multiline-fences)

                    lines
                    (str/split-lines s)

                    ;; walk growing prefixes of the fence under *live?* (the streaming ticks) ...
                    _
                    (binding [hl/*live?* true]
                      (doseq [n (range 1 (inc (count lines)))]
                        (hl/highlight g (str (str/join "\n" (take n lines)) "\n"))))]

                   ;; ... then the finalized (non-live) render is exact regardless of that history.
                   (expect (= (native g s) (hl/highlight g s)))))))

(defdescribe
  highlight-live-incremental-test
  (it
    "live streaming parses the settled prefix once, re-parsing only the growing tail"
    (when native?
      (reset! @#'hl/stream-memo {})
      (let
        [prefix
         (str/join "\n"
                   (for [i (range 20)]
                     (format "(def x%d %d)" i i)))

         ;; parser normalises a live fence body to end in a newline; the last,
         ;; still-typed line grows char by char while the 20-line prefix is stable.
         frames
         (vec (for [k (range 1 12)]
                (str prefix "\n" (subs "(println :growing)" 0 k) "\n")))

         prefix-parses
         (atom 0)

         tail-parses
         (atom 0)]

        (with-redefs-fn {#'hl/highlight* (let [orig @#'hl/highlight*]
                                           (fn [g ^String src]
                                             (if (str/includes? src "(def x19")
                                               (swap! prefix-parses inc)
                                               (swap! tail-parses inc))
                                             (orig g src)))}
          (fn []
            (binding [hl/*live?* true]
              (doseq [f frames]
                (hl/highlight "clojure" f)))))
        ;; the 20-line prefix is parsed exactly ONCE and reused across every tick;
        ;; only the short trailing line is (re)parsed, one cheap call per frame.
        (expect (= 1 @prefix-parses))
        (expect (= (count frames) @tail-parses)))))
  (it "the settled prefix of a live frame is coloured exactly as native colours it"
      (when native?
        (reset! @#'hl/stream-memo {})
        (let
          [prefix
           (str/join "\n"
                     (for [i (range 8)]
                       (format "(def y%d %d)" i i)))

           frame
           (str prefix "\n(println :tail)\n")

           live
           (binding [hl/*live?* true]
             (hl/highlight "clojure" frame))

           live-ls
           (str/split-lines (or live ""))

           full-ls
           (str/split-lines (or (native "clojure" frame) ""))]

          ;; the 8 settled prefix lines match native byte-for-byte (only the last,
          ;; still-typed line may differ mid-stream).
          (expect (= (take 8 live-ls) (take 8 full-ls)))))))
