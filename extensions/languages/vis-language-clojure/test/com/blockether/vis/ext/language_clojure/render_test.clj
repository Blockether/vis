(ns com.blockether.vis.ext.language-clojure.render-test
  "Verify the channel renderers emit the `{:summary :display}` contract
   (`:com.blockether.vis.internal.extension/render-fn-result`):

     {:summary <zone-map-or-ir>   ; the badge row; first [:strong] = label
      :display [:ir {} <block>…]}  ; the full expanded body

   `:display` is canonical IR:
     [:ir {} <block> ...]   root
     [:p  {} <inline> ...]  paragraph
     [:c  {} <text>]        inline code
     [:code {:lang ...} <body>]  fenced code

   The renderers MUST tolerate the realistic failure shapes too (timeouts,
   missing keys, error result maps) without throwing, because they fire from
   the channel sink with no try/catch above."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.ext.language-clojure.render :as r]
   [com.blockether.vis.internal.extension :as extension]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- ir? [x] (and (vector? x) (= :ir (first x))))

(defn- contract?
  "Conforms to ::render-fn-result with a non-empty summary and an IR
   display root. An EMPTY root (`[:ir {}]`) is legitimate — headline-only
   renders (0/1 ports, timeout badge) put everything on the summary row.
   (The old `(> (count display) 2)` check only ever passed for these via
   a literal nil child that `ir-root` used to leak; it drops nils now.)"
  [result]
  (and (extension/render-fn-result? result)
    (some? (:summary result))
    (ir? (:display result))))

(defn- flat-text
  "Concatenate every string leaf in an IR tree. Used by tests so inline span
   boundaries (introduced by `ir-p` / `ir-strong` wrapping each arg) don't
   fragment regex matches."
  [node]
  (cond
    (string? node) node
    (vector? node) (str/join "" (map flat-text (rest node)))
    :else ""))

(defn- summary-text
  "Flatten the (zone-or-IR) summary to a single string for regex matching."
  [summary]
  (if (map? summary)
    (str/join "  " (map flat-text [(:left summary) (:center summary) (:right summary)]))
    (flat-text summary)))

(defdescribe render-ports-test
  (it "zero ports → contract; display is a single badge headline (no listing)"
    (let [res (r/render-ports {:default nil :ports []})]
      (expect (contract? res))
      (expect (re-find #"PORTS" (summary-text (:summary res))))
      (expect (re-find #"0 visible" (summary-text (:summary res))))
      ;; No listing body when n <= 1.
      (expect (not-any? #(and (vector? %) (= :code (first %))) (:display res)))))

  (it "one port → contract; display drops the listing (badge already carries default=)"
    (let [res  (r/render-ports {:default 7888
                                :ports   [{:port 7888 :source "/x/.nrepl-port"}]})
          flat (pr-str (:display res))]
      (expect (contract? res))
      (expect (re-find #"default=7888" (summary-text (:summary res))))
      ;; The path used to leak as a code-block row — verify it is gone.
      (expect (not (re-find #"\.nrepl-port" flat)))))

  (it "multiple ports → contract; display lists each port→source"
    (let [res  (r/render-ports {:default 7888
                                :ports   [{:port 7888 :source "/x/.nrepl-port"}
                                          {:port 60001 :source "/y/.nrepl-port"}]})
          flat (pr-str (:display res))]
      (expect (contract? res))
      (expect (re-find #"7888" flat))
      (expect (re-find #"60001" flat))
      (expect (re-find #"\.nrepl-port" flat)))))

(defdescribe render-eval-test
  (it "success → contract; EVAL badge + value in display"
    (let [res (r/render-eval {:value "42" :ns "user" :status #{"done"} :ms 5 :port 7888})
          st  (summary-text (:summary res))
          t   (flat-text (:display res))]
      (expect (contract? res))
      (expect (re-find #"EVAL" st))
      (expect (re-find #":7888" st))
      (expect (re-find #"42" t))))

  (it "timed-out? → TIMEOUT badge in summary"
    (let [res (r/render-eval {:timed_out true :status #{"done" "timeout"} :port 7888})]
      (expect (contract? res))
      (expect (re-find #"TIMEOUT" (summary-text (:summary res))))))

  (it "shows :out and :err blocks in display when present"
    (let [res (r/render-eval {:value nil :out "hi\n" :err "boom\n" :status #{"done"} :port 7888})
          t   (flat-text (:display res))]
      (expect (contract? res))
      (expect (re-find #":out" t))
      (expect (re-find #":err" t))
      (expect (re-find #"hi" t))
      (expect (re-find #"boom" t))))

  (it "error eval -> ONE consolidated :err block, nil value dropped, no duplicate ex line"
    (let [res  (r/render-eval {:value "nil"
                               :err "Execution error (ExceptionInfo) at x/y (z.cljc:26).\nUnexpected EOF.\n"
                               :ex "class clojure.lang.ExceptionInfo"
                               :root-ex "class clojure.lang.ExceptionInfo"
                               :status #{"done" "error"} :port 7888 :ms 12})
          body (filterv vector? (rest (:display res)))
          t    (flat-text (:display res))]
      (expect (contract? res))
      (expect (re-find #"ERROR" (summary-text (:summary res))))
      ;; exactly one display block: the :err text - no nil value, no ex bubble
      (expect (= 1 (count body)))
      (expect (re-find #"Execution error" t))
      (expect (not (re-find #"root=" t)))
      (expect (not (re-find #"(?m)^nil" t)))))

  (it "error eval keeps the ex class when :err does not already name it"
    (let [res (r/render-eval {:err "something broke\n"
                              :ex "class java.lang.IllegalStateException"
                              :status #{"done" "error"} :port 7888})
          t   (flat-text (:display res))]
      (expect (contract? res))
      (expect (re-find #"IllegalStateException" t))
      ;; still ONE block - the ex line rides inside the :err block
      (expect (= 1 (count (filterv vector? (rest (:display res)))))))))
