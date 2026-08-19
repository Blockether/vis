(ns com.blockether.vis.internal.env-python-grep-paging-test
  "A CAPPED SEARCH PAGES ITSELF (`resources/vis-python/async_runtime.py`).

   `grep` answers TEXT, and `limit` caps a page — 50 hits by default — so a wide
   sweep answers a SLICE. Line 1 says so and names the literal next call, but the
   answer used to be a bare string: continuing meant retyping the whole call with
   `offset`, and the step nobody takes by hand is the step that turns a capped
   page into \"that is all there is\".

   The page is now a `__VisGrep__`: still the text (str operations, slicing,
   `print`, and the uniform `.get('op')` probe all behave, and iterating it still
   yields CHARACTERS), plus `next_offset` / `next(g)` / `pages()` / `all()`. These
   tests drive that class through the REAL settle path with a stub bound under the
   tool's own name, because the wrapper is chosen by the NAME of the call that
   produced the text."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.test-python-context :as tpc]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- page-text
  "One canned page in the shape the renderer really emits: `grep-summary-line`'s
   summary (pinned verbatim in `core_test`), then one anchored row. `next-offset`
   nil is an UNCAPPED page — no arrow, and so no next call."
  [offset next-offset]
  (str "grep 'q'  50 hits · 3 of 30 files"
       (when next-offset
         (str "  capped by limit → next(r) or grep({…, \"offset\": " next-offset "})"))
       "\nsrc/a.clj  (1)\n  1:aaa│ hit from offset " offset))

(defn- stub-grep
  "A `grep` that pages: offsets 0 → 50 → 100, the last one complete. Every spec it
   is called with is recorded, so a test can assert that the NEXT page carried the
   whole original options map and not just an offset."
  [calls]
  (fn [& args]
    (let [spec
          (first args)

          offset
          (long (or (get spec "offset") 0))]

      (swap! calls conj spec)
      (if (= "done" (get spec "query"))
        "grep 'done'  2 hits · 1 file\nsrc/a.clj  (1)\n  1:aaa│ complete"
        (page-text offset (when (< offset 100) (+ offset 50)))))))

(defn- run
  "Run `code` in this namespace's own sandbox with `grep` bound to [[stub-grep]],
   and answer `[stdout calls]`. Its own context key: the stub SHADOWS a real tool
   name, so its debris must not reach the shared sandbox."
  [code]
  (let [calls
        (atom [])

        ctx
        (tpc/context-with! ::ctx {'grep (stub-grep calls)})

        r
        (ep/run-python-block ctx code "t1/i1")]

    (expect (nil? (:error r)))
    [(str/trim (str (:stdout r))) @calls]))

;; Regression: a capped page was a DEAD END. The result was a bare `str`, so the
;; only way on was to retype the whole call with `offset` — and a sweep that
;; stopped at 50 hits read exactly like a tree that holds 50.
(defdescribe
  grep-page-continues-itself-test
  (it "a capped page is still the text, and knows where the next one starts"
      (let [[out] (run (str "g = grep({\"query\": \"q\", \"paths\": [\"src\"]})\n"
                            "print(type(g).__name__, isinstance(g, str),"
                            " g.get(\"op\") is None, g.next_offset, g.is_capped)"))]
        (expect (= "__VisGrep__ True True 50 True" out))))
  (it "next(g) walks to the end and carries the WHOLE options map, not just an offset"
      (let [[out calls]
            (run (str "g = grep({\"query\": \"q\", \"paths\": [\"src\"]})\n" "p2 = next(g)\n"
                      "p3 = next(p2)\n" "print(p2.next_offset, p3.next_offset, next(p3, None))"))]
        (expect (= "100 None None" out))
        (expect (= [{"query" "q" "paths" ["src"]} {"query" "q" "paths" ["src"] "offset" 50}
                    {"query" "q" "paths" ["src"] "offset" 100}]
                   calls))))
  (it "pages() walks every page and all() joins them"
      (let [[out] (run (str "g = grep({\"query\": \"q\", \"paths\": [\"src\"]})\n"
                            "print(len(list(g.pages())), g.all().count(\"grep 'q'\"))"))]
        (expect (= "3 3" out))))
  (it "a bound that stops the walk early SAYS so and names the call that continues"
      (let [[out] (run (str "g = grep({\"query\": \"q\", \"paths\": [\"src\"]})\n"
                            "print(g.all(max_pages=2).splitlines()[-1])"))]
        (expect (str/starts-with? out "… stopped after 2 pages"))
        (expect (str/includes? out "\"offset\": 100"))))
  (it "pages() is lazy: abandoning the walk never runs the searches behind it"
      (let [[out calls] (run (str "g = grep({\"query\": \"q\", \"paths\": [\"src\"]})\n"
                                  "for page in g.pages():\n"
                                  "    break\n" "print(len(g))"))]
        (expect (pos? (Long/parseLong out)))
        (expect (= 1 (count calls)))))
  (it "an UNCAPPED page already is the whole answer"
      (let [[out calls]
            (run (str "g = grep({\"query\": \"done\", \"paths\": [\"src\"]})\n"
                      "print(g.next_offset, g.is_capped, next(g, None), len(list(g.pages())))"))]
        (expect (= "None False None 1" out))
        (expect (= 1 (count calls)))))
  ;; `next` without a default is the protocol: the walk ends in StopIteration,
  ;; not in a None every caller has to test for.
  (it "the last page ends the walk the way Python ends every walk"
      (let [[out] (run (str "g = grep({\"query\": \"done\", \"paths\": [\"src\"]})\n" "try:\n"
                            "    next(g)\n" "    print(\"no stop\")\n"
                            "except StopIteration:\n" "    print(\"StopIteration\")"))]
        (expect (= "StopIteration" out))))
  ;; Iterating a string means CHARACTERS everywhere else in Python. A page walk
  ;; that quietly stole `__iter__` would break `\"\".join(g)` and `list(g)` for
  ;; every caller who never asked for paging.
  (it "iterating the page still yields characters"
      (let [[out] (run (str "g = grep({\"query\": \"q\", \"paths\": [\"src\"]})\n"
                            "print(list(g)[:4], \"\".join(g) == str(g), g[:4])"))]
        (expect (= "['g', 'r', 'e', 'p'] True grep" out)))))
