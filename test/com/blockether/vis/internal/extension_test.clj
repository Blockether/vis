(ns com.blockether.vis.internal.extension-test
  (:require
   [com.blockether.vis.internal.extension :as extension]
   [lazytest.core :refer [defdescribe expect it]]))

(defn raw-add
  "Raw helper used by helper builder tests."
  [a b]
  (+ a b))

(defdescribe helper-builder-test
  (it "builds raw callable entries without tool wrapper keys"
    (let [entry (extension/helper #'raw-add)]
      (expect (= 'raw-add (:ext.symbol/sym entry)))
      (expect (identical? raw-add (:ext.symbol/val entry)))
      (expect (= "Raw helper used by helper builder tests." (:ext.symbol/doc entry)))
      (expect (= '([a b]) (:ext.symbol/arglists entry)))
      (expect (nil? (:ext.symbol/fn entry)))
      (expect (nil? (:ext.symbol/journal-render-fn entry)))
      (expect (nil? (:ext.symbol/channel-render-fn entry)))))

  (it "raw-var converts function vars to helpers with fallback metadata"
    (let [entry (extension/raw-var #'raw-add {:sym 'plus2})]
      (expect (= 'plus2 (:ext.symbol/sym entry)))
      (expect (identical? raw-add (:ext.symbol/val entry)))
      (expect (= '([a b]) (:ext.symbol/arglists entry)))
      (expect (nil? (:ext.symbol/fn entry))))))
