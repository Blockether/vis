(ns com.blockether.vis.internal.doctor-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.doctor :as doctor]
            [com.blockether.vis.internal.extension :as extension]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  doctor-format-output-test
  (it "renders doctor messages grouped by extension name"
      (let
        [out (doctor/format-output
               [{:ext "foundation" :check-id ::agents-md :level :info :message "AGENTS.md loaded"}
                {:ext "voice"
                 :check-id ::piper
                 :level :warn
                 :message "Piper missing"
                 :remediation "Install voice"}]
               {:use-ansi? false})]
        (expect (str/starts-with? out "vis-agent doctor\n\n  foundation\n  ──────────"))
        (expect (str/includes? out "  ℹ agents-md: AGENTS.md loaded"))
        (expect (str/includes? out "\n\n  voice\n  ─────"))
        (expect (str/includes? out "  ⚠ piper: Piper missing"))
        (expect (str/includes? out "      -> Install voice"))
        (expect (str/ends-with? out "Summary: 0 errors, 1 warnings, 1 info"))))
  (it "uses question mark for anonymous diagnostics inside extension section"
      (let
        [out (doctor/format-output [{:ext "example-ext" :level :error :message "boom"}]
                                   {:use-ansi? false})]
        (expect (str/includes? out "  example-ext"))
        (expect (str/includes? out "  ✗ ?: boom")))))

(defdescribe doctor-run-checks-test
             (it "emits host system messages under vis before extension messages"
                 (with-redefs
                   [extension/registered-extensions
                    (fn []
                      [{:ext/name "sample"
                        :ext/doctor-fn (fn [_]
                                         [{:level :info :check-id ::sample :message "ok"}])}])]
                   (let [msgs (doctor/run-checks {:db-info {:path "/tmp/test.db"}})]
                     (expect (= "vis" (:ext (first msgs))))
                     (expect (= ::doctor/system (:check-id (first msgs))))
                     (expect (= "sample" (:ext (last msgs))))
                     (expect (= ::sample (:check-id (last msgs))))))))

(defdescribe
  doctor-sandbox-deps-test
  (it "reports one info line when every sandbox shim's dependencies resolve"
      (with-redefs
        [extension/sandbox-shims (constantly [{:shim/name "yaml" :shim/source "vis-shims/yaml.py"}
                                              {:shim/name "numpy"
                                               :shim/source "vis-shims/numpy.py"
                                               :shim/bindings {"probe" (fn []
                                                                         :ok)}}])]
        (let [msgs (#'doctor/sandbox-shim-messages {})]
          (expect (= 1 (count msgs)))
          (expect (= "vis" (:ext (first msgs))))
          (expect (= ::doctor/sandbox-deps (:check-id (first msgs))))
          (expect (= :info (:level (first msgs))))
          (expect (str/includes? (:message (first msgs)) "2/2 Python sandbox dependencies resolve"))
          (expect (str/includes? (:message (first msgs)) "1 host bridges")))))
  (it "errors on a shim whose Python source is not on the classpath"
      (with-redefs
        [extension/sandbox-shims (constantly [{:shim/name "ghost"
                                               :shim/source "vis-shims/nope.py"}])]
        (let
          [msgs (#'doctor/sandbox-shim-messages {})
           err (first (filterv #(= :error (:level %)) msgs))]

          (expect (some? err))
          (expect (str/includes? (:message err) "Sandbox shim 'ghost' source is unavailable"))
          (expect (str/includes? (:remediation err) "vis-shims/nope.py"))
          (expect (= 2 (doctor/exit-code msgs))))))
  (it "errors on a shim whose host bindings cannot be realized"
      (with-redefs
        [extension/sandbox-shims (constantly [{:shim/name "ruffy"
                                               :shim/source "vis-shims/ruff.py"
                                               :shim/bindings (fn []
                                                                (throw (ex-info "ruff unavailable"
                                                                                {})))}])]
        (let
          [msgs (#'doctor/sandbox-shim-messages {})
           err (first (filterv #(= :error (:level %)) msgs))]

          (expect (some? err))
          (expect (str/includes? (:message err)
                                 "host bindings failed to resolve: ruff unavailable")))))
  (it "warns on duplicate shim names, which shadow each other at install time"
      (with-redefs
        [extension/sandbox-shims (constantly [{:shim/name "yaml" :shim/source "vis-shims/yaml.py"}
                                              {:shim/name "yaml"
                                               :shim/source "vis-shims/yaml.py"}])]
        (let
          [msgs (#'doctor/sandbox-shim-messages {})
           warn (first (filterv #(= :warn (:level %)) msgs))]

          (expect (some? warn))
          (expect (str/includes? (:message warn) "Duplicate sandbox shim name(s): yaml")))))
  (it "warns when nothing registered a shim and never throws on a broken registry"
      (with-redefs [extension/sandbox-shims (constantly [])]
        (let [msgs (#'doctor/sandbox-shim-messages {})]
          (expect (= [:warn] (mapv :level msgs)))
          (expect (str/includes? (:message (first msgs)) "No Python sandbox shims registered"))))
      (with-redefs
        [extension/sandbox-shims (fn []
                                   (throw (ex-info "registry exploded" {})))]
        (let [msgs (#'doctor/sandbox-shim-messages {})]
          (expect (= [:error] (mapv :level msgs)))
          (expect (str/includes? (:message (first msgs))
                                 "Sandbox shim registry unavailable: registry exploded"))))))
