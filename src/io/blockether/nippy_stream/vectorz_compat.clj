;; Adapted from Blockether/nippy-stream vectorz_compat.clj at
;; 0fe1f7f84051a25d5b1387e90a743bc5add9736f (MIT, Copyright 2024 Blockether).
(ns io.blockether.nippy-stream.vectorz-compat
  "Vectorz extensions for Nippy, adapted from Blockether/nippy-stream.

   Requiring this namespace performs no codec registration or matrix-selection
   side effects. `ensure-installed!` selects the Vectorz core.matrix implementation
   and registers the codecs exactly once, so compatibility setup remains deferred
   until Nippy is actually used."
  (:require [clojure.core.matrix :as matrix]
            [taoensso.nippy :as nippy])
  (:import [mikera.vectorz AVector Vector Vector1 Vector2 Vector3 Vector4 Vectorz]))

(defonce ^:private installation
  (delay
    (matrix/set-current-implementation :vectorz)
    #_{:clj-kondo/ignore [:unresolved-symbol]}
    (nippy/extend-freeze Vector
                         1
                         [^Vector x data-output]
                         (nippy/freeze-to-out! data-output (.asDoubleArray x)))
    #_{:clj-kondo/ignore [:unresolved-symbol]}
    (nippy/extend-thaw 1 [data-input] (Vector/wrap ^doubles (nippy/thaw-from-in! data-input)))
    (nippy/extend-freeze Vector1
                         2
                         [^Vector1 x data-output]
                         (nippy/freeze-to-out! data-output (.toDoubleArray x)))
    (nippy/extend-thaw 2 [data-input] (Vectorz/create ^doubles (nippy/thaw-from-in! data-input)))
    (nippy/extend-freeze Vector2
                         3
                         [^Vector2 x data-output]
                         (nippy/freeze-to-out! data-output (.toDoubleArray x)))
    (nippy/extend-thaw 3 [data-input] (Vectorz/create ^doubles (nippy/thaw-from-in! data-input)))
    (nippy/extend-freeze Vector3
                         4
                         [^Vector3 x data-output]
                         (nippy/freeze-to-out! data-output (.toDoubleArray x)))
    (nippy/extend-thaw 4 [data-input] (Vectorz/create ^doubles (nippy/thaw-from-in! data-input)))
    (nippy/extend-freeze Vector4
                         5
                         [^Vector4 x data-output]
                         (nippy/freeze-to-out! data-output (.toDoubleArray x)))
    (nippy/extend-thaw 5 [data-input] (Vectorz/create ^doubles (nippy/thaw-from-in! data-input)))
    true))

(defn ensure-installed!
  "Install the nippy-stream Vectorz codecs once, on first use."
  []
  @installation)

(defn ->clj-vector
  "Convert a Vectorz vector to a plain Clojure vector; leave other values intact."
  [value]
  (if (instance? AVector value) (vec (.toDoubleArray ^AVector value)) value))
