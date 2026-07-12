(ns com.blockether.vis.internal.gateway.workspace-roundtrip-test
  "END-TO-END guard for the filesystem-root add the TUI picker / web footer show.

   The bug: you add a directory, the daemon persists it and says \"Added…\", but
   the header's `FILESYSTEM (n)` list never grew. Root cause was the wire seam —
   `wire/->wire` munges kebab map keys `-`->`_` on the way out while `parse-json`
   keywordizes VERBATIM, so `:filesystem-roots` came back `:filesystem_roots` and
   every kebab reader (`(:filesystem-roots ws)`, `(:trunk e)`, `(:root ws)`) saw
   nil. `client/decode-workspace` re-hydrates the kebab shape at the ONE client
   boundary every channel goes through.

   This exercises the REAL chain a C-a keystroke drives — `workspace/add-filesystem-root!`
   (server DB) → the `state/session-workspace-info` map shape → `wire/json-str`
   (server encoder) → `wire/parse-json` (client decoder) → the REAL, private
   `client/decode-workspace` — and asserts the added root arrives in the kebab
   shape the picker reads. A raw hop (no decode) is asserted to LOSE it, so the
   guard fails loudly if the decode is ever dropped."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.ext.persistance-sqlite.core :as ps]
            [com.blockether.vis.internal.gateway.client :as client]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.workspace :as ws]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private decode-workspace
  "The real, private client decoder — the seam under test."
  (deref #'client/decode-workspace))

(defn- with-store
  [f]
  (let [store (assoc (ps/db-open! :memory) :backend :sqlite)]
    (try (f store) (finally (ps/db-close! store)))))

(defn- temp-dir
  [prefix]
  (.getCanonicalPath (.toFile (java.nio.file.Files/createTempDirectory
                                prefix
                                (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- delete-tree!
  [root]
  (doseq [f (reverse (file-seq (io/file root)))]
    (io/delete-file f true)))

(defn- session-workspace-shape
  "The exact map `state/session-workspace-info` emits for a workspace."
  [ws-rec]
  {:id (:id ws-rec)
   :draft? (ws/draft? ws-rec)
   :root (:root ws-rec)
   :repo-root (:repo-root ws-rec)
   :label (:label ws-rec)
   :fork-ms (:fork-ms ws-rec)
   :filesystem-roots (ws/filesystem-roots ws-rec)})

(defn- hop
  "Server-encode then client-decode, exactly like the gateway HTTP boundary."
  [info]
  (decode-workspace (wire/parse-json (wire/json-str info))))

(defdescribe
  filesystem-add-survives-the-wire-test
  (it
    "an added root reaches the picker's kebab shape through server-encode → client-decode"
    (with-store
      (fn [store]
        (let [base
              (temp-dir "vis-rt-base")

              extra
              (temp-dir "vis-rt-extra")]

          (try
            (let [seed
                  (ps/db-workspace-insert! store
                                           {:id (str (random-uuid))
                                            :repo-id "rt"
                                            :repo-root base
                                            :root base
                                            :state :active
                                            :fork-ms 0})

                  wid
                  (:id seed)]

              ;; C-a: add `extra` as an extra filesystem root (trunk session → live).
              (ws/add-filesystem-root! store wid extra)
              (let [info
                    (session-workspace-shape (ws/get store wid))

                    decoded
                    (hop info)

                    raw
                    (wire/parse-json (wire/json-str info))]

                ;; The picker reads these kebab keys off `@ws-info`:
                (expect (= [(ws/normalize-root extra)] (mapv :trunk (:filesystem-roots decoded))))
                (expect (= 1 (count (:filesystem-roots decoded))))
                (expect (= base (:root decoded)))
                (expect (= base (:repo-root decoded)))
                ;; `:backend` rides the wire as a stringified keyword VALUE; the
                ;; decoder re-coerces it back to a keyword (never a bare string).
                (expect (keyword? (:backend (first (:filesystem-roots decoded)))))
                ;; And the raw hop (the bug) demonstrably loses the kebab keys —
                ;; if someone drops the decode, this guard fails.
                (expect (nil? (:filesystem-roots raw)))
                (expect (some? (:filesystem_roots raw)))))
            (finally (delete-tree! base) (delete-tree! extra))))))))
