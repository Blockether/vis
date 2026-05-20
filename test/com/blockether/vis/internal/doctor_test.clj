(ns com.blockether.vis.internal.doctor-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.doctor :as doctor]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe doctor-format-output-test
  (it "renders doctor messages flat without extension section banners"
    (let [out (doctor/format-output
                [{:ext :foundation
                  :check-id ::system
                  :level :info
                  :message "OS: test"}
                 {:ext :foundation
                  :check-id ::voice
                  :level :warn
                  :message "Voice missing"
                  :remediation "Install voice"}]
                {:use-ansi? false})]
      (expect (str/starts-with? out "vis doctor\n\n  ℹ system: OS: test"))
      (expect (str/includes? out "  ⚠ voice: Voice missing"))
      (expect (str/includes? out "      -> Install voice"))
      (expect (not (str/includes? out "foundation\n")))
      (expect (not (str/includes? out "──────────")))
      (expect (str/ends-with? out "Summary: 0 errors, 1 warnings, 1 info"))))

  (it "falls back to extension id for anonymous diagnostics"
    (let [out (doctor/format-output
                [{:ext :example-ext
                  :level :error
                  :message "boom"}]
                {:use-ansi? false})]
      (expect (str/includes? out "  ✗ :example-ext: boom")))))
