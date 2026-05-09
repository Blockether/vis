(ns com.blockether.vis.internal.tool-result-test
  "Contract tests for the tool-result envelope: required keys,
   success/failure data, symbol-level rendering, and sanitized trace
   normalization."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.extension :as tr]
   [lazytest.core :refer [defdescribe expect it throws?]]))

(defdescribe tool-result-contract-test
  (it "success requires :success? :result :info and nil :error"
    (let [out (tr/success {:result {:a 1}
                           :info {:op :demo}})]
      (expect (true? (:success? out)))
      (expect (= nil (:error out)))
      (expect (= :demo (get-in out [:info :op])))
      (expect (integer? (get-in out [:info :started-at-ms])))
      (expect (integer? (get-in out [:info :finished-at-ms])))
      (expect (integer? (get-in out [:info :duration-ms])))
      (expect (= {:a 1} (:result out)))
      (expect (not (contains? out (keyword (str "result" "-" "shape")))))
      (expect (not (contains? out :markdown)))))

  (it "failure requires structured :error with type/message/trace"
    (let [ex (try
               (throw (ex-info "boom" {:x 1}))
               (catch Throwable t t))
          out (tr/failure {:result nil
                           :info {:op :demo}
                           :throwable ex})]
      (expect (false? (:success? out)))
      (expect (= nil (:result out)))
      (expect (= "clojure.lang.ExceptionInfo" (get-in out [:error :type])))
      (expect (= "boom" (get-in out [:error :message])))
      (expect (vector? (get-in out [:error :trace])))))

  (it "invalid envelope throws"
    (expect (throws? clojure.lang.ExceptionInfo
              #(tr/assert-tool-result! {:success? true :result 1}))))

  (it "merge-info re-validates the envelope without presentation carriers"
    (let [base (tr/success {:result true
                            :info {:op :exists?}})
          out  (tr/merge-info base
                 {:tool {:sym 'exists?
                         :call "v/exists?"}
                  :extension {:namespace 'com.acme.ext.fs}
                  :source {:paths ["/tmp/ext.clj"]
                           :mtime-max 1
                           :hash-sha256 nil}})]
      (expect (not (contains? out :markdown)))
      (expect (= 'exists? (get-in out [:info :tool :sym])))
      (expect (= 'com.acme.ext.fs (get-in out [:info :extension :namespace])))
      (expect (= ["/tmp/ext.clj"] (get-in out [:info :source :paths])))))

  (it "renders through the owning symbol's journal/channel render-fns when registered"
    (let [sym (tr/symbol 'exists? (constantly nil)
                {:doc "exists"
                 :arglists '([path])
                 :journal-render-fn (fn [result] (str "journal:" result))
                 :channel-render-fn (fn [result chan-id]
                                      (str (name chan-id) ":" result))})
          ext (tr/extension {:ext/namespace 'com.acme.ext.fs
                             :ext/doc "fs"
                             :ext/kind "filesystem"
                             :ext/ns-alias {:ns 'vis.ext.fs :alias 'fs}
                             :ext/symbols [sym]})]
      (try
        (tr/register-extension! ext)
        (let [out (tr/merge-info
                    (tr/success {:result true :info {:op :v/exists?}})
                    {:tool {:sym 'exists? :call "fs/exists?"}
                     :extension {:namespace 'com.acme.ext.fs}
                     :source {:paths [] :mtime-max -1 :hash-sha256 nil}})]
          (expect (= "journal:true" (tr/journal-render-tool-result out)))
          (expect (= "channel-tui:true" (tr/channel-render-tool-result out :channel-tui))))
        (finally
          (tr/deregister-extension! 'com.acme.ext.fs)))))

  (it "failure path falls back to the engine's default error formatters"
    (let [sym (tr/symbol 'noisy (constantly nil)
                {:doc "noisy"
                 :arglists '([])
                 :journal-render-fn (fn [_] "never called on failure")
                 :channel-render-fn (fn [_ _] "never called on failure")})
          ext (tr/extension {:ext/namespace 'com.acme.ext.noisy
                             :ext/doc "noisy"
                             :ext/kind "filesystem"
                             :ext/ns-alias {:ns 'vis.ext.noisy :alias 'n}
                             :ext/symbols [sym]})]
      (try
        (tr/register-extension! ext)
        (let [ex  (try (throw (ex-info "boom" {})) (catch Throwable t t))
              out (tr/merge-info
                    (tr/failure {:result nil :info {:op :n/noisy} :throwable ex})
                    {:tool {:sym 'noisy :call "n/noisy"}
                     :extension {:namespace 'com.acme.ext.noisy}
                     :source {:paths [] :mtime-max -1 :hash-sha256 nil}})]
          (expect (str/includes? (tr/journal-render-tool-result out) "ERROR"))
          (expect (str/includes? (tr/journal-render-tool-result out) ":n/noisy"))
          (expect (str/includes? (tr/channel-render-tool-result out :channel-tui) "**ERROR**")))
        (finally
          (tr/deregister-extension! 'com.acme.ext.noisy))))))

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
