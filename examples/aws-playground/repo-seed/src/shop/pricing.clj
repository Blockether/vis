(ns shop.pricing
  "Price rules of the shop, in cents.")

(defn line-total
  "Return the total of one order line."
  [{:keys [unit-cents quantity] :or {quantity 1}}]
  (* unit-cents quantity))

(defn subtotal
  "Return the sum of the order lines."
  [lines]
  (reduce + 0 (map line-total lines)))

(defn discount
  "Return the price after a discount of `percent`, rounded down to a cent."
  [cents percent]
  {:pre [(<= 0 percent 100)]}
  (quot (* cents (- 100 percent)) 100))
