(ns com.blockether.vis.ext.channel-tui.click-regions-test
  (:require [com.blockether.vis.ext.channel-tui.click-regions :as cr]
            [lazytest.core :refer [defdescribe it expect]]))

(defn- region
  [row col w url]
  {:bounds {:row row :col col :width w}
   :url url
   :kind :url
   :line nil
   :scheme :https
   :enabled? true})

(defn- frame!
  "Run the renderer-side dance - begin, register every region, commit
   - so the staged set becomes visible to `lookup`."
  [& regions]
  (cr/begin-frame!)
  (doseq [r regions]
    (cr/register! r))
  (cr/commit-frame!))

(defdescribe
  register-and-lookup-test
  (it "round-trips a single region"
      (cr/reset!)
      (frame! (region 5 10 20 "https://a"))
      (let [hit (cr/lookup 12 5)]
        (expect (= "https://a" (:url hit)))))
  (it "lookup returns nil outside the bounds"
      (cr/reset!)
      (frame! (region 5 10 20 "https://a"))
      (expect (nil? (cr/lookup 9 5)))   ; just left of col
      (expect (nil? (cr/lookup 30 5)))  ; one past right edge (col + width = 30 exclusive)
      (expect (nil? (cr/lookup 12 4)))  ; row above
      (expect (nil? (cr/lookup 12 6)))) ; row below
  (it "edges: inclusive on left, exclusive on right"
      (cr/reset!)
      (frame! (region 0 5 10 "https://x"))
      (expect (some? (cr/lookup 5 0)))  ; left edge in
      (expect (some? (cr/lookup 14 0))) ; last cell in
      (expect (nil? (cr/lookup 15 0)))) ; first cell out
  (it "later registration wins on overlap"
      (cr/reset!)
      (frame! (region 1 0 10 "https://old") (region 1 5 10 "https://new"))
      (expect (= "https://new" (:url (cr/lookup 7 1)))))
  (it "reset! drops every entry"
      (cr/reset!)
      (frame! (region 1 0 5 "https://a") (region 2 0 5 "https://b"))
      (cr/reset!)
      (expect (= 0 (count (cr/current))))
      (expect (nil? (cr/lookup 0 1))))
  (it "current returns paint-order snapshot"
      (cr/reset!)
      (frame! (region 1 0 5 "https://a") (region 2 0 5 "https://b") (region 3 0 5 "https://c"))
      (expect (= ["https://a" "https://b" "https://c"] (mapv :url (cr/current))))))

(defdescribe staged-commit-test
             (it "register! does not change lookup until commit-frame! runs"
                 (cr/reset!)
                 ;; Publish a previous frame so we can verify it survives an
                 ;; in-flight, uncommitted paint.
                 (frame! (region 1 0 5 "https://prev"))
                 (cr/begin-frame!)
                 (cr/register! (region 1 0 5 "https://next"))
                 ;; Mid-paint: lookup STILL returns the previous frame.
                 (expect (= "https://prev" (:url (cr/lookup 2 1))))
                 (cr/commit-frame!)
                 (expect (= "https://next" (:url (cr/lookup 2 1)))))
             (it "begin-frame! does NOT touch the published registry"
                 (cr/reset!)
                 (frame! (region 1 0 5 "https://a"))
                 (cr/begin-frame!)
                 ;; No register!, no commit-frame! - published set unchanged.
                 (expect (some? (cr/lookup 2 1))))
             (it "commit-frame! with empty staging clears the published set"
                 (cr/reset!)
                 (frame! (region 1 0 5 "https://a"))
                 (cr/begin-frame!)
                 (cr/commit-frame!)
                 (expect (= 0 (count (cr/current))))))

(defdescribe register-validation-test
             (it "rejects malformed bounds"
                 (cr/reset!)
                 (cr/begin-frame!)
                 (expect
                   (try (cr/register! {:bounds nil :url "x"}) false (catch AssertionError _ true)))
                 (expect (try (cr/register! {:bounds {:row 1 :col "no" :width 5} :url "x"})
                              false
                              (catch AssertionError _ true)))
                 (expect (try (cr/register! {:bounds {:row 1 :col 1} :url "x"})
                              false
                              (catch AssertionError _ true)))))

(defdescribe hover-test
             (it "set-hovered! returns true on change, false otherwise"
                 (cr/reset!)
                 (let [r (region 1 0 5 "https://x")]
                   (expect (true? (cr/set-hovered! r)))
                   (expect (false? (or (cr/set-hovered! r) false)))
                   (expect (true? (cr/set-hovered! nil)))))
             (it "hovered reads back the current pointer"
                 (cr/reset!)
                 (let [r (region 1 0 5 "https://x")]
                   (cr/set-hovered! r)
                   (expect (= r (cr/hovered)))))
             (it "hover survives begin-frame!/commit-frame!"
                 (cr/reset!)
                 (let [r (region 1 0 5 "https://x")]
                   (cr/set-hovered! r)
                   (cr/begin-frame!)
                   (cr/register! r)
                   (cr/commit-frame!)
                   (expect (= r (cr/hovered))))))

(defn- toggle-region
  [row col session-id node-id collapsed?]
  {:bounds {:row row :col col :width 8}
   :kind :toggle-details
   :session-id session-id
   :node-id node-id
   :collapsed? collapsed?})

(defdescribe
  assign-labels-test
  (it "labels only :toggle-details regions, home row first, in paint order"
      (let [pairs (cr/assign-labels [(region 1 0 5 "https://a") (toggle-region 3 4 "s" "n1" true)
                                     (toggle-region 6 4 "s" "n2" false)])]
        (expect (= [["a" "n1"] ["s" "n2"]]
                   (mapv (fn [[l r]]
                           [l (:node-id r)])
                         pairs)))))
  (it "dedupes by [session-id node-id] keeping the first glyph row"
      (let [pairs (cr/assign-labels [(toggle-region 3 4 "s" "n1" true)
                                     (toggle-region 9 4 "s" "n1" true) ; same fold, 2nd glyph row
                                     (toggle-region 6 4 "s" "n2" false)])]
        (expect (= 2 (count pairs)))
        (expect (= ["n1" "n2"] (mapv (comp :node-id second) pairs)))
        ;; first occurrence's row (3) wins, not the duplicate (9)
        (expect (= 3 (:row (:bounds (second (first pairs))))))))
  (it "caps at the alphabet length; extras stay unlabeled"
      (let [many
            (mapv #(toggle-region % 0 "s" (str "n" %) true) (range (+ 3 (count cr/label-alphabet))))

            pairs
            (cr/assign-labels many)]

        (expect (= (count cr/label-alphabet) (count pairs))))))
