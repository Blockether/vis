(ns com.blockether.vis.internal.foundation.shim-nippy
  "Built-in sandbox SHIM exposing Vis's Nippy persistence codec to Python.

   `nippy_decode(bytes)` decodes trusted Vis-owned Nippy BLOBs (for example
   `session_turn_iteration.forms`, `session_turn_state.ctx`, and
   `session_turn_state.error`) into native Python data. Nippy-stream Vectorz
   vectors decode as Python lists. `nippy_encode(value)` performs the inverse for
   Python plain data. The same functions are available as `nippy.decode` /
   `nippy.encode` after `import nippy`.

   The Python module and Vectorz codec registration are lazy: neither runs during
   sandbox context initialization; Vectorz installs on the first codec call.
   Decoded Clojure values cross the normal sandbox boundary: map keys become
   canonical snake_case strings, keyword/symbol values become strings, dates
   become epoch milliseconds, and unsupported leaves stringify. This is for
   inspection and plain-data round trips, not exact Clojure type preservation.
   Java Serializable fallback is disabled in both directions."
  (:require [clojure.core.matrix :as matrix]
            [clojure.walk :as walk]
            [com.blockether.vis.core :as vis]
            [taoensso.nippy :as nippy])
  (:import [java.util Base64]
           [mikera.vectorz AVector Vector Vector1 Vector2 Vector3 Vector4 Vectorz]))

;; --- Vectorz Nippy codecs ---
;; Adapted from Blockether/nippy-stream vectorz_compat.clj at
;; 0fe1f7f84051a25d5b1387e90a743bc5add9736f (MIT, Copyright 2024 Blockether).
;; Registration is deferred: loading this namespace performs no codec
;; registration or matrix-implementation selection, so sandbox context creation
;; stays free of Vectorz side effects.

(defonce ^:private vectorz-installation
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

(defn ensure-vectorz-installed!
  "Install the Vectorz Nippy codecs once, on first codec use."
  []
  @vectorz-installation)

(defn- ->clj-vector
  "Convert a Vectorz vector to a plain Clojure vector; leave other values intact."
  [value]
  (if (instance? AVector value) (vec (.toDoubleArray ^AVector value)) value))

(defn- nippy-envelope
  "Return `[true payload]`, or `[false message]` so Python can raise a catchable
   `nippy.NippyError` instead of leaking an uncatchable host exception."
  [f]
  (try [true (f)] (catch Throwable t [false (str (or (.getMessage t) t))])))

(defn- nippy-python-value
  "Convert Vectorz values nested in decoded Nippy data to ordinary vectors before
   applying Vis's canonical Python boundary conversion."
  [value]
  (->> value
       (walk/postwalk ->clj-vector)
       vis/wire-canonical))

(defn- nippy-bridge-bindings
  "Host Nippy codec callables. Bytes cross as base64; decoded data crosses through
   Vis's canonical string-keyed wire shape. Vectorz compatibility is installed on
   the first codec call only. Serializable fallback stays disabled because sandbox
   input is not a trusted Java object graph."
  []
  {"__vis_nippy_decode__" (fn [encoded]
                            (nippy-envelope #(do (ensure-vectorz-installed!)
                                                 (-> (.decode (Base64/getDecoder) ^String encoded)
                                                     (nippy/thaw {:serializable-allowlist #{}})
                                                     nippy-python-value))))
   "__vis_nippy_encode__"
   (fn [value]
     (nippy-envelope #(do (ensure-vectorz-installed!)
                          (.encodeToString (Base64/getEncoder)
                                           (nippy/freeze value {:serializable-allowlist #{}})))))})


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-nippy"
     :ext/description
     "Sandbox shim: nippy_decode(bytes) / nippy_encode(value), plus nippy.decode/encode, backed by Vis's JVM Nippy codec for persistence BLOB inspection."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "nippy"
       :shim/imports ["nippy"]
       :shim/globals ["nippy_decode" "nippy_encode"]
       :shim/description
       "Lazy globals `nippy_decode`/`nippy_encode` (also nippy.decode/encode) round-trip trusted Vis Nippy bytes and plain Python data; nippy-stream Vectorz vectors decode as lists and codecs install on first use. Not supported: exact Clojure type preservation, Java Serializable fallback, encryption, untrusted input."
       :shim/bindings nippy-bridge-bindings
       :shim/source "vis-shims/nippy.py"}]}))

(vis/register-extension! vis-extension)
