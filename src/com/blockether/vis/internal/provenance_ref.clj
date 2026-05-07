(ns com.blockether.vis.internal.provenance-ref
  "Compatibility facade for canonical proof-facing provenance refs.

   New code should require `com.blockether.vis.internal.proof` directly. This
   namespace remains temporarily while internal callers migrate; no new proof
   semantics live here."
  (:require
   [com.blockether.vis.internal.proof :as proof]))

(def canonical-ref-pattern proof/canonical-ref-pattern)

(defn canonical-ref?
  "True when `s` exactly matches the canonical provenance reference grammar."
  [s]
  (proof/canonical-ref? s))

(defn parse-ref
  "Parse a canonical provenance reference into data.

   Returns nil for non-canonical references. Compact aliases such as `i4.2`,
   `i4.2/tool`, `E1`, or `G1` are intentionally rejected."
  [s]
  (proof/parse-ref s))

(defn format-ref
  "Format canonical provenance reference data."
  [ref-data]
  (proof/format-ref ref-data))

(defn display-ref
  "Return presentation data for a canonical provenance reference."
  [canonical-ref]
  (proof/display-ref canonical-ref))

(defn display-provenance
  "Display helper for a provenance map containing at least `:ref`."
  [provenance]
  (proof/display-provenance provenance))
