(ns com.blockether.vis.internal.doctor-test
  "Unit tests for the cross-extension doctor aggregator. Covers
   plan §6: aggregator auto-injection of `:ext`, deterministic
   ordering, exit-code mapping, throwing-fn capture, empty-registry
   case. The contract: each extension provides a single
   `:ext/doctor-check-fn` that returns a seq of message maps; the
   host stamps `:ext` on every message, leaves `:check-id` to the
   extension to fill in."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.doctor :as doctor]
   [com.blockether.vis.internal.extension :as extension]
   [lazytest.core :refer [defdescribe expect it]]))

;; ---------------------------------------------------------------------------
;; Test scaffolding: temporarily install synthetic extensions into the
;; global registry so we can exercise `doctor/run-checks` against a
;; known input. Restored on teardown to avoid leaking state.
;; ---------------------------------------------------------------------------

(defn- with-registry*
  "Run `f` with the extension registry replaced by `synthetic-exts`
   (a vec of ext maps). Restores the prior contents on exit, even on
   throws."
  [synthetic-exts f]
  ;; Reach via a holder atom - the registry is private. We use the
  ;; public deregister/register surface to swap it.
  (let [prior (extension/registered-extensions)]
    (try
      ;; Clear current registry by deregistering everything.
      (doseq [ext prior]
        (extension/deregister-extension! (:ext/namespace ext)))
      ;; Install synthetics.
      (doseq [ext synthetic-exts]
        (extension/register-extension! ext))
      (f)
      (finally
        ;; Restore: clear synthetics, reinstall prior.
        (doseq [ext synthetic-exts]
          (extension/deregister-extension! (:ext/namespace ext)))
        (doseq [ext prior]
          (extension/register-extension! ext))))))

(defn- mk-ext
  "Build a minimal extension map with the given namespace + a
   `:ext/doctor-check-fn` that returns the provided messages."
  ([ns-sym] (mk-ext ns-sym (constantly [])))
  ([ns-sym check-fn]
   (extension/extension
     {:ext/namespace       ns-sym
      :ext/doc             (str "synthetic " ns-sym)
      :ext/doctor-check-fn check-fn})))

(defn- stamping-fn
  "Build a check fn that returns `msgs` with each pre-stamped with the
   given `:check-id`. Mirrors what real extensions do."
  [check-id msgs]
  (constantly (mapv #(assoc % :check-id check-id) msgs)))

(defn- throwing-fn [error-msg]
  (fn [_env] (throw (Exception. ^String error-msg))))

;; ---------------------------------------------------------------------------
;; run-checks
;; ---------------------------------------------------------------------------

(defdescribe run-checks-test
  (it "auto-injects :ext on every emitted message; preserves :check-id when stamped"
    (with-registry*
      [(mk-ext 'test.foo
         (stamping-fn ::probe [{:level :info :message "hello"}]))]
      #(let [msgs (doctor/run-checks {})]
         (expect (= 1 (count msgs)))
         (let [m (first msgs)]
           (expect (= 'test.foo (:ext m)))
           (expect (= ::probe (:check-id m)))
           (expect (= :info (:level m)))
           (expect (= "hello" (:message m)))))))

  (it "messages without :check-id stamp pass through with no :check-id key"
    (with-registry*
      [(mk-ext 'test.foo (constantly [{:level :info :message "raw"}]))]
      #(let [m (first (doctor/run-checks {}))]
         (expect (= 'test.foo (:ext m)))
         (expect (nil? (:check-id m))))))

  (it "preserves message order across extensions and within an extension"
    (with-registry*
      [(mk-ext 'test.alpha
         (constantly [{:level :info :message "a1" :check-id ::a}
                      {:level :info :message "a2" :check-id ::a}]))
       (mk-ext 'test.beta
         (constantly [{:level :info :message "b1" :check-id ::b}]))]
      #(let [msgs (doctor/run-checks {})]
         (expect (= ["a1" "a2" "b1"] (mapv :message msgs))))))

  (it "throwing check fn becomes a single :error message tagged with the extension"
    (with-registry*
      [(mk-ext 'test.bad (throwing-fn "kaboom"))]
      #(let [msgs (doctor/run-checks {})
             m    (first msgs)]
         (expect (= 1 (count msgs)))
         (expect (= :error (:level m)))
         (expect (str/includes? (:message m) "kaboom"))
         (expect (= 'test.bad (:ext m))))))

  (it "supports a check fn returning a single map (not wrapped in vec)"
    (with-registry*
      [(mk-ext 'test.single (constantly {:level :warn :message "watch out"}))]
      #(let [msgs (doctor/run-checks {})]
         (expect (= 1 (count msgs)))
         (expect (= :warn (:level (first msgs)))))))

  (it "nil return is treated as no messages"
    (with-registry*
      [(mk-ext 'test.nil (constantly nil))]
      #(expect (empty? (doctor/run-checks {})))))

  (it "coerces unknown :level into :error"
    (with-registry*
      [(mk-ext 'test.bad-level (constantly [{:level :purple :message "??"}]))]
      #(let [m (first (doctor/run-checks {}))]
         (expect (= :error (:level m))))))

  (it "extension declaring no :ext/doctor-check-fn contributes nothing"
    (with-registry*
      [(mk-ext 'test.silent)]
      #(expect (empty? (doctor/run-checks {})))))

  (it "empty registry returns empty messages"
    (with-registry* []
      #(expect (empty? (doctor/run-checks {}))))))

;; ---------------------------------------------------------------------------
;; exit-code
;; ---------------------------------------------------------------------------

(defdescribe exit-code-test
  (it "0 for empty messages"
    (expect (= 0 (doctor/exit-code []))))

  (it "0 for only :info"
    (expect (= 0 (doctor/exit-code [{:level :info :message "x"}
                                    {:level :info :message "y"}]))))

  (it "1 when any :warn (no :error)"
    (expect (= 1 (doctor/exit-code [{:level :info :message "x"}
                                    {:level :warn :message "y"}]))))

  (it "2 when any :error (regardless of warns)"
    (expect (= 2 (doctor/exit-code [{:level :info  :message "x"}
                                    {:level :warn  :message "y"}
                                    {:level :error :message "z"}]))))

  (it "2 when only :error"
    (expect (= 2 (doctor/exit-code [{:level :error :message "boom"}])))))

;; ---------------------------------------------------------------------------
;; format-output
;; ---------------------------------------------------------------------------

(defdescribe format-output-test
  (it "empty messages produces the no-checks placeholder"
    (let [out (doctor/format-output [] {:use-ansi? false})]
      (expect (str/includes? out "vis doctor"))
      (expect (str/includes? out "No diagnostic checks registered."))))

  (it "groups by :ext, preserving extension and within-ext ordering"
    (let [msgs [{:ext 'a :check-id ::p :level :info :message "a-1"}
                {:ext 'a :check-id ::p :level :info :message "a-2"}
                {:ext 'b :check-id ::q :level :info :message "b-1"}]
          out  (doctor/format-output msgs {:use-ansi? false})]
      (expect (str/includes? out "a"))
      (expect (str/includes? out "b"))
      ;; a-1 appears before a-2 appears before b-1
      (let [ai1 (.indexOf ^String out "a-1")
            ai2 (.indexOf ^String out "a-2")
            bi1 (.indexOf ^String out "b-1")]
        (expect (< ai1 ai2))
        (expect (< ai2 bi1)))))

  (it "renders summary with totals"
    (let [out (doctor/format-output
                [{:ext 'a :check-id ::p :level :info :message "i"}
                 {:ext 'a :check-id ::p :level :warn :message "w"}
                 {:ext 'a :check-id ::p :level :error :message "e"}]
                {:use-ansi? false})]
      (expect (str/includes? out "Summary: 1 errors, 1 warnings, 1 info"))))

  (it "renders remediation as indented arrow line"
    (let [out (doctor/format-output
                [{:ext 'a :check-id ::p
                  :level :error :message "broken"
                  :remediation "fix it like this"}]
                {:use-ansi? false})]
      (expect (str/includes? out "-> fix it like this"))))

  (it "icons present for every level"
    (let [out (doctor/format-output
                [{:ext 'a :check-id ::p :level :info :message "i"}
                 {:ext 'a :check-id ::p :level :warn :message "w"}
                 {:ext 'a :check-id ::p :level :error :message "e"}]
                {:use-ansi? false})]
      (expect (str/includes? out "ℹ"))
      (expect (str/includes? out "⚠"))
      (expect (str/includes? out "✗")))))

;; ---------------------------------------------------------------------------
;; startup-hint-line
;; ---------------------------------------------------------------------------

(defdescribe startup-hint-test
  (it "nil when registry has only :info messages"
    (with-registry*
      [(mk-ext 'test.clean
         (constantly [{:level :info :message "ok" :check-id ::ok}]))]
      #(expect (nil? (doctor/startup-hint-line {})))))

  (it "warns about issue count when warnings present"
    (with-registry*
      [(mk-ext 'test.warn
         (constantly [{:level :warn :message "w1" :check-id ::a}
                      {:level :warn :message "w2" :check-id ::b}]))]
      #(let [hint (doctor/startup-hint-line {})]
         (expect (some? hint))
         (expect (str/includes? hint "2 issues"))
         (expect (str/includes? hint "bin/vis doctor")))))

  (it "issues count includes errors"
    (with-registry*
      [(mk-ext 'test.error
         (constantly [{:level :warn  :message "w" :check-id ::a}
                      {:level :error :message "e" :check-id ::a}]))]
      #(let [hint (doctor/startup-hint-line {})]
         (expect (str/includes? hint "2 issues")))))

  (it "empty registry -> nil hint"
    (with-registry* []
      #(expect (nil? (doctor/startup-hint-line {}))))))

;; ---------------------------------------------------------------------------
;; Spec validation interplay
;; ---------------------------------------------------------------------------

(defdescribe spec-validation-test
  (it "extension with no :ext/doctor-check-fn is accepted (defaults to no-op)"
    (let [ext (extension/extension {:ext/namespace 'test.no-checks
                                    :ext/doc       "no checks declared"})]
      (expect (fn? (:ext/doctor-check-fn ext)))
      (expect (= [] ((:ext/doctor-check-fn ext) {})))))

  (it "extension with explicit no-op :ext/doctor-check-fn round-trips"
    (let [no-op (constantly [])
          ext   (extension/extension {:ext/namespace       'test.empty-checks
                                      :ext/doc             "explicit no-op"
                                      :ext/doctor-check-fn no-op})]
      (expect (identical? no-op (:ext/doctor-check-fn ext))))))
