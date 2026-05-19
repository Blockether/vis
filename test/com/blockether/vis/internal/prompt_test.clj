(ns com.blockether.vis.internal.prompt-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.prompt :as prompt]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe prompt-assembly-test
  (it "normalizes core addendum and extension prompt text"
    (let [ext {:ext/name "test.prompt"
               :ext/sci {:ext.sci/alias 't}
               :ext/prompt (fn [_]
                             "\n\n    Extension line\n\n\n\n      Nested extension line\n")}
          env {:extensions (atom [ext])}
          messages (prompt/assemble-stable-prompt-messages env
                     {:system-prompt "\n\n    Addendum line\n\n\n\n      Nested addendum line\n"
                      :active-extensions [ext]})
          text (prompt/stable-prompt-text messages)]
      (expect (str/includes? text "Addendum line\n\n  Nested addendum line"))
      (expect (str/includes? text "Extension line\n\n  Nested extension line"))
      (expect (not (str/includes? text "\n\n\n"))))))

(defdescribe prompt-core-test
  (it "documents engine-owned forms as bare, not extension tools"
    (let [text (prompt/build-system-prompt {})]
      (expect (str/includes? text "Engine-owned control forms are bare symbols"))
      (expect (str/includes? text "(set-session-title! ...)"))
      (expect (str/includes? text "Never namespace-qualify")))))

(defdescribe extension-activation-test
  (it "assembles from precomputed active extensions without activating again"
    (let [calls (atom 0)
          ext {:ext/name "test.activation"
               :ext/activation-fn (fn [_]
                                    (swap! calls inc)
                                    true)
               :ext/prompt (constantly "Active prompt")}
          env {:extensions (atom [ext])}
          active (prompt/active-extensions env)]
      (expect (= 1 @calls))
      (prompt/assemble-stable-prompt-messages env {:active-extensions active})
      (expect (= 1 @calls)))))
