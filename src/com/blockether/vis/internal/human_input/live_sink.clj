(ns com.blockether.vis.internal.human-input.live-sink
  "The STORE OF RECORD of one live view: an append-only NDJSON file per view.

   A live view is a STREAM, and nothing else in this process keeps one. The
   channel bus is a ring of 2000 events and the session journal is truncated per
   turn and past 16 MB, so a log that ran for twenty minutes would already be
   gone by the time the human scrolls back to its beginning. Here every line the
   engine ACCEPTED is on disk, in the order it accepted them.

   One file, three kinds of line: the OPEN line is the view as it was declared,
   one PATCH line per accepted patch, and the CLOSE line is the verdict. A patch
   is appended BEFORE it is published, so a crash keeps everything the engine
   accepted rather than everything a surface managed to paint, and the file is
   opened in APPEND mode, so a resumed process never truncates a view it did not
   open.

   Lines are wire JSON, which is what lets the file BE the artifact instead of a
   re-encoded copy of one, and what lets a reader hand a patch line straight back
   to `human-input/normalize-patch`: every live vocabulary is a closed table read
   in either spelling."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [com.blockether.vis.internal.gateway.wire :as wire])
  (:import (java.io File)))

(set! *warn-on-reflection* true)

(defn- views-dir
  "Where the records live. `~/.vis/gateway/live` is TAKEN — the bus keeps one
   turn-liveness marker per session there and LISTS that directory — so a view's
   stream gets a directory of its own rather than a subdirectory somebody else's
   listing has to learn to skip."
  ^File []
  (io/file (System/getProperty "user.home") ".vis" "gateway" "views"))

(defn view-file
  "The record of view `view-id`, kept under the session that opened it."
  ^File [session-id view-id]
  (io/file (views-dir) (str session-id) (str view-id ".ndjson")))

(defn- append-line!
  "One line, flushed and closed. A live view writes rarely (a patch is a human's
   heartbeat, not a byte stream), and a handle held open for the length of a CI
   run is a handle leaked when the run is killed."
  ^File [^File file line]
  (io/make-parents file)
  (with-open [writer (io/writer file :append true)]
    (.write writer ^String (wire/json-str line))
    (.write writer "\n"))
  file)

(defn open!
  "Start the record of `view` and return the file it is kept in. The whole
   declared view is the first line, so a reader that has the file needs nothing
   else to rebuild what the patches were applied to."
  ^File [view]
  (append-line! (view-file (:session-id view) (:id view))
                {:kind :open :at (System/currentTimeMillis) :view view}))

(defn append!
  "Record one ACCEPTED patch. Called after the materializer took it and before
   any surface is told, so the file is never behind a screen."
  ^File [^File file patch]
  (append-line! file {:kind :patch :at (System/currentTimeMillis) :patch patch}))

(defn close!
  "Seal the record with the verdict the model reads."
  ^File [^File file result]
  (append-line! file {:kind :close :at (System/currentTimeMillis) :result result}))

(defn read-range
  "`limit` lines of the record from 0-based `from`, each decoded into the
   engine's keyword-keyed shape.

   A range past the end is an empty vector rather than a refusal: a surface
   asking for scrollback a crash never wrote is asking an honest question, and a
   view that is still open has no last line to bound the ask with."
  [^File file from limit]
  (if-not (and file (.isFile file))
    []
    (with-open [reader (io/reader file)]
      (into []
            (comp (drop (max 0 (long from)))
                  (take (max 0 (long limit)))
                  (map (fn [line]
                         (wire/->engine (json/read-json line)))))
            (line-seq reader)))))
