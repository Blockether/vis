(ns com.blockether.vis.internal.tool-result-test
  "Contract tests for the tool-result envelope: required keys,
   success/failure shape, symbol-level rendering, and sanitized trace
   normalization."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.extension :as tr]
   [lazytest.core :refer [defdescribe expect it throws?]]))

(defdescribe tool-result-contract-test
  (it "success requires :ok? :result :result-shape :provenance and nil :error"
    (let [out (tr/success {:result {:a 1}
                           :provenance {:op :demo}})]
      (expect (true? (:ok? out)))
      (expect (= nil (:error out)))
      (expect (= :demo (get-in out [:provenance :op])))
      (expect (integer? (get-in out [:provenance :started-at-ms])))
      (expect (integer? (get-in out [:provenance :finished-at-ms])))
      (expect (integer? (get-in out [:provenance :duration-ms])))
      (expect (= {:a 1} (:result out)))
      (expect (map? (:result-shape out)))
      (expect (not (contains? out :markdown)))))

  (it "failure requires structured :error with type/message/trace"
    (let [ex (try
               (throw (ex-info "boom" {:x 1}))
               (catch Throwable t t))
          out (tr/failure {:result nil
                           :provenance {:op :demo}
                           :throwable ex})]
      (expect (false? (:ok? out)))
      (expect (= nil (:result out)))
      (expect (= "clojure.lang.ExceptionInfo" (get-in out [:error :type])))
      (expect (= "boom" (get-in out [:error :message])))
      (expect (vector? (get-in out [:error :trace])))))

  (it "invalid envelope throws"
    (expect (throws? clojure.lang.ExceptionInfo
              #(tr/assert-tool-result! {:ok? true :result 1}))))

  (it "result-shape reports nils and sampled homogeneity for collections"
    (let [shape (tr/result-shape [1 nil "x" {:a 1} nil])]
      (expect (= :vector (:type shape)))
      (expect (= 5 (:count shape)))
      (expect (= {:sample-size 5
                  :nil-count 2
                  :types [:int :nil :string :map]
                  :non-nil-types [:int :string :map]
                  :homogeneous-ratio (/ 1.0 3.0)}
                (:sample-stats shape)))))

  (it "result-shape reports sampled map value and key stats"
    (let [shape (tr/result-shape {:a nil :b 1 :c 2})]
      (expect (= :map (:type shape)))
      (expect (= {:sample-size 3
                  :types [:keyword]
                  :non-nil-types [:keyword]
                  :homogeneous-ratio 1.0}
                (:key-stats shape)))
      (expect (= {:sample-size 3
                  :nil-count 1
                  :types [:nil :int]
                  :non-nil-types [:int]
                  :homogeneous-ratio 1.0}
                (:sample-stats shape)))))

  (it "merge-provenance re-validates the envelope without presentation carriers"
    (let [base (tr/success {:result true
                            :provenance {:op :exists?}})
          out  (tr/merge-provenance base
                 {:tool {:sym 'exists?
                         :call "v/exists?"}
                  :extension {:namespace 'com.acme.ext.fs}
                  :source {:paths ["/tmp/ext.clj"]
                           :mtime-max 1
                           :hash-sha256 nil}})]
      (expect (not (contains? out :markdown)))
      (expect (= 'exists? (get-in out [:provenance :tool :sym])))
      (expect (= 'com.acme.ext.fs (get-in out [:provenance :extension :namespace])))
      (expect (= ["/tmp/ext.clj"] (get-in out [:provenance :source :paths])))))

  (it "renders through the owning symbol render-fn when registered"
    (let [sym (tr/symbol 'exists? (constantly nil)
                {:doc "exists"
                 :arglists '([path])
                 :render-fn (fn [{:keys [surface tool-result]}]
                              (str (name surface) ":" (:result tool-result)))})
          ext (tr/extension {:ext/namespace 'com.acme.ext.fs
                             :ext/doc "fs"
                             :ext/kind "filesystem"
                             :ext/ns-alias {:ns 'vis.ext.fs :alias 'fs}
                             :ext/symbols [sym]})]
      (try
        (tr/register-extension! ext)
        (let [out (tr/merge-provenance
                    (tr/success {:result true :provenance {:op :v/exists?}})
                    {:tool {:sym 'exists? :call "fs/exists?"}
                     :extension {:namespace 'com.acme.ext.fs}
                     :source {:paths [] :mtime-max -1 :hash-sha256 nil}})]
          (expect (= "journal:true" (tr/render-tool-result :journal out))))
        (finally
          (tr/deregister-extension! 'com.acme.ext.fs)))))

  (it "generic rendering includes structured result and provenance"
    (let [out (tr/merge-provenance
                (tr/success {:result {:lines ["a" "b"]}
                             :provenance {:op :demo}})
                {:tool {:sym 'cat
                        :call "v/cat"}
                 :extension {:namespace 'com.acme.ext.fs}
                 :source {:paths ["/tmp/ext.clj"]
                          :mtime-max 1
                          :hash-sha256 nil}})
          rendered (tr/render-tool-result :journal out)]
      (expect (str/includes? rendered "Tool `:demo` ok"))
      (expect (str/includes? rendered "; result {:lines [\"a\" \"b\"]}"))
      (expect (str/includes? rendered "; provenance {:tool {:sym cat, :call \"v/cat\"}"))
      (expect (str/includes? rendered ":extension {:namespace com.acme.ext.fs}"))
      (expect (str/includes? rendered ":source {:paths [\"/tmp/ext.clj\"], :mtime-max 1, :hash-sha256 nil}")))))

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
