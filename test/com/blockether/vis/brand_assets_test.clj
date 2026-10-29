(ns com.blockether.vis.brand-assets-test
  "The vis mark ships on paper AND on ink: GitHub, the docs site and the companion
   all render it against a dark background half of the time. These are pixel facts
   about the shipped PNGs, read back out of the files with `com.blockether.imaging`."
  (:require [clojure.java.io :as io]
            [com.blockether.imaging :as imaging]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private transparent-assets
  "Every logo that must sit on an unknown background. `logo.png` is deliberately
   absent: it is the opaque light-theme plate the dark ones replace."
  ["resources/vis-docs/assets/logo.png" "apps/vis-companion/public/vis-logo.png"])

(defn- rgba
  "`{:w :h :px}` with `:px` the raw RGBA bytes of the file on disk."
  [path]
  (let [f (io/file path)]
    (with-open [in (io/input-stream f)
                img (imaging/decode (.readAllBytes in))]

      {:w (imaging/width img)
       :h (imaging/height img)
       :is-opaque (:is-opaque (imaging/info img))
       :px (imaging/pixels img)})))

(defn- chan ^long [^bytes px ^long i ^long c] (bit-and (aget px (+ (* i 4) c)) 0xff))

(defn- corner-alphas
  [{:keys [^long w ^long h ^bytes px]}]
  (mapv (fn [[^long x ^long y]]
          (chan px (+ x (* y w)) 3))
        [[0 0] [(dec w) 0] [0 (dec h)] [(dec w) (dec h)]]))

;; Regression: the shipped mark was a white plate. `logo.png` is opaque white, so
;; GitHub's dark theme drew a white box around it, and the transparent copies were
;; produced by keying white out -- which left a white fringe on every edge and
;; punched the eye glint and smile highlight into see-through holes.
(defdescribe brand-assets-test
             (it "every transparent logo really is transparent, with clear corners"
                 (doseq [path
                         transparent-assets

                         :let [img
                               (rgba path)]]

                   (expect (.exists (io/file path)) path)
                   (expect (false? (:is-opaque img)) (str path " must carry an alpha channel"))
                   (expect (= [0 0 0 0] (corner-alphas img))
                           (str path " corners must be fully transparent, not white paper")))))
