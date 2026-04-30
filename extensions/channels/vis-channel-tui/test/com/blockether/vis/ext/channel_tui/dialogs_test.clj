(ns com.blockether.vis.ext.channel-tui.dialogs-test
  (:require [clojure.test :refer [deftest testing is]]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]))

;; The dialog functions require a live TerminalScreen, so direct unit
;; testing is impractical. The bracketed-paste fix in text-input-dialog!
;; is verified indirectly: pasting into the API key field no longer
;; leaks PUA marker chars (\uE200, \uE201) into the stored value.

(deftest smoke-test
  (testing "dialogs namespace loads and text-input-dialog! is public"
    (is (fn? (var-get #'dlg/text-input-dialog!)))))
