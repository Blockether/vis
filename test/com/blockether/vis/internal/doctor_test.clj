(ns com.blockether.vis.internal.doctor-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.doctor :as doctor]
            [com.blockether.vis.internal.extension :as extension]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  doctor-format-output-test
  (it "renders doctor messages grouped by extension name"
      (let [out
            (doctor/format-output
              [{:ext "foundation" :check-id ::agents-md :level :info :message "AGENTS.md loaded"}
               {:ext "voice"
                :check-id ::piper
                :level :warn
                :message "Piper missing"
                :remediation "Install voice"}]
              {:use-ansi? false})]
        (expect (str/starts-with? out "vis doctor\n\n  foundation\n  ──────────"))
        (expect (str/includes? out "  ℹ agents-md: AGENTS.md loaded"))
        (expect (str/includes? out "\n\n  voice\n  ─────"))
        (expect (str/includes? out "  ⚠ piper: Piper missing"))
        (expect (str/includes? out "      -> Install voice"))
        (expect (str/ends-with? out "Summary: 0 errors, 1 warnings, 1 info"))))
  (it "uses question mark for anonymous diagnostics inside extension section"
      (let [out (doctor/format-output [{:ext "example-ext" :level :error :message "boom"}]
                                      {:use-ansi? false})]
        (expect (str/includes? out "  example-ext"))
        (expect (str/includes? out "  ✗ ?: boom")))))

(defdescribe doctor-run-checks-test
             (it "emits host system messages under vis before extension messages"
                 (with-redefs [extension/registered-extensions
                               (fn []
                                 [{:ext/name "sample"
                                   :ext/doctor-fn
                                   (fn [_]
                                     [{:level :info :check-id ::sample :message "ok"}])}])]
                   (let [msgs (doctor/run-checks {:db-info {:path "/tmp/test.db"}})]
                     (expect (= "vis" (:ext (first msgs))))
                     (expect (= ::doctor/system (:check-id (first msgs))))
                     (expect (= "sample" (:ext (last msgs))))
                     (expect (= ::sample (:check-id (last msgs))))))))
