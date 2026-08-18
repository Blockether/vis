(ns com.blockether.vis.brand-assets-test
  "The vis mark ships on paper AND on ink: GitHub, the docs site and the companion
   all render it against a dark background half of the time. These are pixel facts
   about the shipped PNGs, read back out of the files with `com.blockether.imaging`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
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

(defn- punched-holes
  "Number of transparent pixels enclosed by the artwork -- pixels no 8-connected
   path of transparency joins to the border. Keying white out of the source turns
   the eye glint and the smile highlight into exactly this: a hole that shows the
   page through the mark."
  [{:keys [^long w ^long h ^bytes px]}]
  (let [n
        (* w h)

        clear?
        (fn [^long i]
          (< (chan px i 3) 8))

        seen
        (boolean-array n)

        stack
        (java.util.ArrayDeque.)

        push!
        (fn [^long i]
          (when (and (clear? i) (not (aget seen i))) (aset seen i true) (.push stack (int i))))]

    (dotimes [x w]
      (push! x)
      (push! (+ x (* (dec h) w))))
    (dotimes [y h]
      (push! (* y w))
      (push! (+ (* y w) (dec w))))
    (while (not (.isEmpty stack))
      (let [i
            (long (.pop stack))

            x
            (rem i w)

            y
            (quot i w)]

        (doseq [dx
                [-1 0 1]

                dy
                [-1 0 1]]

          (let [nx
                (+ x (long dx))

                ny
                (+ y (long dy))]

            (when (and (>= nx 0) (< nx w) (>= ny 0) (< ny h)) (push! (+ nx (* ny w))))))))
    (count (filter (fn [^long i]
                     (and (clear? i) (not (aget seen i))))
                   (range n)))))

(defn- matte-residue
  "Number of semi-transparent pixels that are still near-WHITE. An un-matted mark
   fades out through its own colours; a mark whose white paper was merely keyed
   out keeps that paper in every soft edge, and it reads as a white fringe."
  [{:keys [^long w ^long h ^bytes px]}]
  (count (for [i
               (range (* w h))

               :let [a
                     (chan px i 3)]
               :when (and (< 8 a 250) (> (min (chan px i 0) (chan px i 1) (chan px i 2)) 235))]

           i)))

(defn- readme [] (slurp (io/file "README.md")))

;; Regression: the shipped mark was a white plate. `logo.png` is opaque white, so
;; GitHub's dark theme drew a white box around it, and the transparent copies were
;; produced by keying white out -- which left a white fringe on every edge and
;; punched the eye glint and smile highlight into see-through holes.
(defdescribe
  brand-assets-test
  (it "every transparent logo really is transparent, with clear corners"
      (doseq [path
              transparent-assets

              :let [img
                    (rgba path)]]

        (expect (.exists (io/file path)) path)
        (expect (false? (:is-opaque img)) (str path " must carry an alpha channel"))
        (expect (= [0 0 0 0] (corner-alphas img))
                (str path " corners must be fully transparent, not white paper"))))
  (it "no transparent hole is punched through the artwork"
      (doseq [path transparent-assets]
        (expect
          (zero? (punched-holes (rgba path)))
          (str
            path
            " has interior transparent pixels: the glint/smile show the page through the mark"))))
  (it "soft edges carry the mark's colours, never leftover white paper"
      (doseq [path transparent-assets]
        (expect
          (zero? (matte-residue (rgba path)))
          (str path
               " keeps near-white semi-transparent pixels: a white fringe on dark backgrounds"))))
  (it "the docs asset and the companion asset are the same file"
      (expect (= (seq (.readAllBytes (io/input-stream (io/file
                                                        "resources/vis-docs/assets/logo.png"))))
                 (seq (.readAllBytes (io/input-stream
                                       (io/file "apps/vis-companion/public/vis-logo.png")))))))
  (it "README offers the dark mark to dark-theme readers"
      (let [md (readme)]
        (expect (str/includes? md "prefers-color-scheme: dark"))
        (expect (str/includes? md "src=\"logo.png\"")))))
