(ns com.blockether.vis.internal.foundation.editing.version-test
  (:require [com.blockether.vis.internal.foundation.editing.version :as version]
            [lazytest.core :refer [defdescribe it expect]]))

(defdescribe render-numbered-block-test
             (it "prints one `N│ text` row per line"
                 (expect (= "1│ alpha\n2│ beta"
                            (version/render-numbered-block [[1 "alpha"] [2 "beta"]]))))
             (it "prints each number as it is, without padding, and strips a CR"
                 (expect (= "9│ a\n10│ b" (version/render-numbered-block [[9 "a\r"] [10 "b"]]))))
             (it "indents every row when asked"
                 (expect (= "  7│ x" (version/render-numbered-block [[7 "x"]] "  ")))))
