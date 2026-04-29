(ns com.blockether.vis.ext.channel-tui.click-regions-test
  (:require
   [com.blockether.vis.ext.channel-tui.click-regions :as cr]
   [lazytest.core :refer [defdescribe it expect]]))

(defn- region [row col w url]
  {:bounds {:row row :col col :width w}
   :url url :kind :url :line nil :scheme :https :enabled? true})

(defdescribe register-and-lookup-test
  (it "round-trips a single region"
    (cr/reset!)
    (cr/register! (region 5 10 20 "https://a"))
    (let [hit (cr/lookup 12 5)]
      (expect (= "https://a" (:url hit)))))

  (it "lookup returns nil outside the bounds"
    (cr/reset!)
    (cr/register! (region 5 10 20 "https://a"))
    (expect (nil? (cr/lookup 9  5)))     ; just left of col
    (expect (nil? (cr/lookup 30 5)))     ; one past right edge (col + width = 30 exclusive)
    (expect (nil? (cr/lookup 12 4)))     ; row above
    (expect (nil? (cr/lookup 12 6))))    ; row below

  (it "edges: inclusive on left, exclusive on right"
    (cr/reset!)
    (cr/register! (region 0 5 10 "https://x"))
    (expect (some? (cr/lookup 5  0)))    ; left edge in
    (expect (some? (cr/lookup 14 0)))    ; last cell in
    (expect (nil?  (cr/lookup 15 0))))   ; first cell out

  (it "later registration wins on overlap"
    (cr/reset!)
    (cr/register! (region 1 0 10 "https://old"))
    (cr/register! (region 1 5 10 "https://new"))
    (expect (= "https://new" (:url (cr/lookup 7 1)))))

  (it "reset! drops every entry"
    (cr/reset!)
    (cr/register! (region 1 0 5 "https://a"))
    (cr/register! (region 2 0 5 "https://b"))
    (cr/reset!)
    (expect (= 0 (count (cr/current))))
    (expect (nil? (cr/lookup 0 1))))

  (it "current returns paint-order snapshot"
    (cr/reset!)
    (cr/register! (region 1 0 5 "https://a"))
    (cr/register! (region 2 0 5 "https://b"))
    (cr/register! (region 3 0 5 "https://c"))
    (expect (= ["https://a" "https://b" "https://c"]
              (mapv :url (cr/current))))))

(defdescribe register-validation-test
  (it "rejects malformed bounds"
    (cr/reset!)
    (expect (try (cr/register! {:bounds nil :url "x"}) false
              (catch AssertionError _ true)))
    (expect (try (cr/register! {:bounds {:row 1 :col "no" :width 5} :url "x"}) false
              (catch AssertionError _ true)))
    (expect (try (cr/register! {:bounds {:row 1 :col 1} :url "x"}) false
              (catch AssertionError _ true)))))

(defdescribe hover-test
  (it "set-hovered! returns true on change, false otherwise"
    (cr/reset!)
    (let [r (region 1 0 5 "https://x")]
      (expect (true?  (cr/set-hovered! r)))
      (expect (false? (or (cr/set-hovered! r) false)))
      (expect (true?  (cr/set-hovered! nil)))))

  (it "hovered reads back the current pointer"
    (cr/reset!)
    (let [r (region 1 0 5 "https://x")]
      (cr/set-hovered! r)
      (expect (= r (cr/hovered))))))
