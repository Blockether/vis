(ns shop.pricing-test
  (:require [lazytest.core :refer [defdescribe expect it]]
            [shop.pricing :as pricing]))

(defdescribe pricing-test
  (it "adds the order lines"
    (expect (= 700 (pricing/subtotal [{:unit-cents 250 :quantity 2} {:unit-cents 200}]))))
  (it "rounds a discount down to a cent"
    (expect (= 899 (pricing/discount 999 10)))))
