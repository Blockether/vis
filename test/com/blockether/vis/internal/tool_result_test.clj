(ns com.blockether.vis.internal.tool-result-test
  "Contract tests for the `:op/envelope` tool-result (PLAN.md §2.1):
   required keys, success/failure data, symbol-level rendering, and
   structured error normalization."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.extension :as tr]
   [lazytest.core :refer [defdescribe expect it throws?]]))

(defdescribe tool-result-contract-test
  (it "success builds an :op/envelope with required op/* keys + nil :op/error"
    (let [out (tr/success {:result {:a 1} :op :demo})]
      (expect (true? (:op/success? out)))
      (expect (nil? (:op/error out)))
      (expect (= :demo (:op/symbol out)))
      ;; :op/tag derives via op-tag (defaults to :op.tag/observation
      ;; for unregistered :demo).
      (expect (= :op.tag/observation (:op/tag out)))
      (expect (integer? (get-in out [:op/metadata :started-at-ms])))
      (expect (integer? (get-in out [:op/metadata :finished-at-ms])))
      (expect (integer? (get-in out [:op/metadata :duration-ms])))
      (expect (= {:a 1} (:op/result out)))
      (expect (not (contains? out :markdown)))))

  (it "failure carries structured :op/error per PLAN §2.1 (:message + :trace string)"
    (let [ex (try (throw (ex-info "boom" {:x 1})) (catch Throwable t t))
          out (tr/failure {:result nil :op :demo :throwable ex})]
      (expect (false? (:op/success? out)))
      (expect (nil? (:op/result out)))
      ;; :message required (non-blank).
      (expect (= "boom" (get-in out [:op/error :message])))
      ;; :trace per PLAN §2.7 is a preformatted string (not a vec of
      ;; structured frames). First line carries the underlying class.
      (expect (string? (get-in out [:op/error :trace])))
      (expect (str/includes? (get-in out [:op/error :trace]) "ExceptionInfo"))))

  (it "invalid envelope throws"
    (expect (throws? clojure.lang.ExceptionInfo
              #(tr/assert-tool-result! {:op/success? true :op/result 1
                                        :op/error {:message "set on success"}}))))

  (it "merge-into-metadata folds extras into :op/metadata; envelope re-validates"
    (let [base (tr/success {:result true :op :exists?})
          out  (tr/merge-into-metadata base
                 {:tool {:sym 'exists? :call "v/exists?"}
                  :extension {:namespace 'com.acme.ext.fs}
                  :source {:paths ["/tmp/ext.clj"]
                           :mtime-max 1
                           :hash-sha256 nil}})]
      (expect (not (contains? out :markdown)))
      (expect (= 'exists? (get-in out [:op/metadata :tool :sym])))
      (expect (= 'com.acme.ext.fs (get-in out [:op/metadata :extension :namespace])))
      (expect (= ["/tmp/ext.clj"] (get-in out [:op/metadata :source :paths])))))

  (it "renders through the owning symbol's journal/channel render-fns when registered"
    (let [sym (tr/symbol 'exists? (constantly nil)
                {:doc "exists"
                 :arglists '([path])
                 :journal-render-fn (fn [result] (str "journal:" result))
                 :channel-render-fn (fn [result] (str "channel:" result))})
          ext (tr/extension {:ext/namespace 'com.acme.ext.fs
                             :ext/doc "fs"
                             :ext/kind "filesystem"
                             :ext/ns-alias {:ns 'vis.ext.fs :alias 'fs}
                             :ext/symbols [sym]})]
      (try
        (tr/register-extension! ext)
        (let [out (tr/merge-into-metadata
                    (tr/success {:result true :op :v/exists?})
                    {:tool {:sym 'exists? :call "fs/exists?"}
                     :extension {:namespace 'com.acme.ext.fs}
                     :source {:paths [] :mtime-max -1 :hash-sha256 nil}})]
          (expect (= "journal:true" (tr/journal-render-tool-result out)))
          (expect (= "channel:true" (tr/channel-render-tool-result out))))
        (finally
          (tr/deregister-extension! 'com.acme.ext.fs)))))

  (it "failure path falls back to the engine's default error formatters"
    (let [sym (tr/symbol 'noisy (constantly nil)
                {:doc "noisy"
                 :arglists '([])
                 :journal-render-fn (fn [_] "never called on failure")
                 :channel-render-fn (fn [_] "never called on failure")})
          ext (tr/extension {:ext/namespace 'com.acme.ext.noisy
                             :ext/doc "noisy"
                             :ext/kind "filesystem"
                             :ext/ns-alias {:ns 'vis.ext.noisy :alias 'n}
                             :ext/symbols [sym]})]
      (try
        (tr/register-extension! ext)
        (let [ex  (try (throw (ex-info "boom" {})) (catch Throwable t t))
              out (tr/merge-into-metadata
                    (tr/failure {:result nil :op :n/noisy :throwable ex})
                    {:tool {:sym 'noisy :call "n/noisy"}
                     :extension {:namespace 'com.acme.ext.noisy}
                     :source {:paths [] :mtime-max -1 :hash-sha256 nil}})]
          (expect (str/includes? (tr/journal-render-tool-result out) "ERROR"))
          (expect (str/includes? (tr/journal-render-tool-result out) ":n/noisy"))
          (expect (str/includes? (tr/channel-render-tool-result out) "**ERROR**")))
        (finally
          (tr/deregister-extension! 'com.acme.ext.noisy))))))

(defdescribe tool-result-trace-test
  (it "normalize-error returns {:message :trace} per PLAN §2.1 + §2.7"
    ;; Phase 4 reshape: error is structured {:message :trace?
    ;; :hint? :block?}. :trace is a PREFORMATTED string (babashka
    ;; style), NOT a vec of structured frames. The string's first
    ;; line carries the underlying class name + message; subsequent
    ;; lines are filtered frames (sci.impl + clojure.lang
    ;; reflection dropped via noisy-frame?).
    (let [frame (StackTraceElement. "user" "anonymous-fn" "iteration" 1)
          ex    (ex-info "boom" {:k 1})
          _     (.setStackTrace ex (into-array StackTraceElement [frame]))
          err   (tr/normalize-error ex)]
      (expect (= "boom" (:message err)))
      (expect (string? (:trace err)))
      (expect (str/starts-with? (:trace err) "clojure.lang.ExceptionInfo: boom"))
      ;; Frame line included (single user frame, not noisy-filtered).
      (expect (str/includes? (:trace err) "user/anonymous-fn"))
      (expect (str/includes? (:trace err) "iteration"))))

  (it "normalize-error skips Throwables without ex-message gracefully"
    (let [err (tr/normalize-error (Exception.))]
      ;; :message falls back to the class name when ex-message is nil.
      (expect (= "java.lang.Exception" (:message err)))
      (expect (string? (:trace err))))))
