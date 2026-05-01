(ns com.blockether.vis.internal.tool-result-test
  "Contract tests for the tool-result envelope: required keys,
   success/failure shape, and sanitized trace normalization."
  (:require
   [com.blockether.vis.internal.extension :as tr]
   [lazytest.core :refer [defdescribe expect it throws?]]))

(defdescribe tool-result-contract-test
  (it "success requires :ok? :result :result-shape :provenance :markdown and nil :error"
    (let [out (tr/success {:result {:a 1}
                           :provenance {:op :demo}
                           :markdown "done"})]
      (expect (true? (:ok? out)))
      (expect (= nil (:error out)))
      (expect (= :demo (get-in out [:provenance :op])))
      (expect (integer? (get-in out [:provenance :started-at-ms])))
      (expect (integer? (get-in out [:provenance :finished-at-ms])))
      (expect (integer? (get-in out [:provenance :duration-ms])))
      (expect (= {:a 1} (:result out)))
      (expect (map? (:result-shape out)))
      (expect (= "done" (:markdown out)))))

  (it "failure requires structured :error with type/message/trace"
    (let [ex (try
               (throw (ex-info "boom" {:x 1}))
               (catch Throwable t t))
          out (tr/failure {:result nil
                           :provenance {:op :demo}
                           :markdown "failed"
                           :throwable ex})]
      (expect (false? (:ok? out)))
      (expect (= nil (:result out)))
      (expect (= "clojure.lang.ExceptionInfo" (get-in out [:error :type])))
      (expect (= "boom" (get-in out [:error :message])))
      (expect (vector? (get-in out [:error :trace])))))

  (it "invalid envelope throws"
    (expect (throws? clojure.lang.ExceptionInfo
              #(tr/assert-tool-result! {:ok? true :result 1}))))

  (it "metadata carries :vis/presentation without changing the data contract"
    (let [out (tr/with-presentation
                (tr/success {:result true :provenance {:op :exists?} :markdown "exists"})
                {:journal :hide})]
      (expect (= {:journal :hide} (tr/presentation out)))
      (expect (tr/tool-result? out))))

  (it "merge-provenance preserves metadata and re-validates the envelope"
    (let [base (tr/with-presentation
                 (tr/success {:result true
                              :provenance {:op :exists?}
                              :markdown "exists"})
                 {:journal :markdown})
          out  (tr/merge-provenance base
                 {:tool {:sym 'exists?
                         :call "v/exists?"}
                  :extension {:namespace 'com.acme.ext.fs}
                  :source {:paths ["/tmp/ext.clj"]
                           :mtime-max 1
                           :hash-sha256 nil}})]
      (expect (= {:journal :markdown} (tr/presentation out)))
      (expect (= 'exists? (get-in out [:provenance :tool :sym])))
      (expect (= 'com.acme.ext.fs (get-in out [:provenance :extension :namespace])))
      (expect (= ["/tmp/ext.clj"] (get-in out [:provenance :source :paths]))))))

(defdescribe tool-result-trace-test
  (it "normalizes a frame into {:class :method :file :line :origin}"
    (let [frame (StackTraceElement. "user" "anonymous-fn" "iteration" 1)
          ex    (ex-info "x" {})
          _     (.setStackTrace ex (into-array StackTraceElement [frame]))
          err   (tr/normalize-error ex)
          first-frame (first (:trace err))]
      (expect (= {:class "user"
                  :method "anonymous-fn"
                  :file "iteration"
                  :line 1
                  :origin :user-code}
                first-frame)))))
