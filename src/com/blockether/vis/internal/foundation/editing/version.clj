(ns com.blockether.vis.internal.foundation.editing.version
  "Versioned line addresses: the vocabulary `cat` and `grep` print and `patch` spends.

   A file version is the first 8 hex chars of the SHA-256 of its UTF-8 text. A read
   prints the version ONCE in its header (`path @3f9a1c2b`) and each line as a plain
   number (`131│ text`). `patch` names that version and its line numbers. When the
   file changed after the read, the edit is moved through a diff from the shown
   version to the current file, and refused only when its own lines changed.

     split-content-lines / char-offset-at-line   blob <-> line/char coordinates
     content-version / parse-version             text -> version, input -> version
     render-numbered-block / header              [[ln text]…] -> `N│ text` rows
     remember! / recall                          bounded cache of shown versions
     note-shown! / shown-ranges / covers?        the lines that each version showed
     edit-span                                   line span -> char span

   Everything is pure except the cache. The cache holds the text of each version
   that a read showed, keyed by canonical path and version, so a remap can diff
   from it. It drops the oldest texts above `store-max-chars`."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.util :as util]))

(set! *warn-on-reflection* true)

(def ^:const version-width
  "Hex chars in a file version. 32 bits make an accidental match between two texts
   of one file negligible, and the version appears once per file, not per line."
  8)

(def ^:const gutter
  "Separator between the line number and the line text in every rendered row.
   U+2502 plus a space: it never occurs in source, so `line.split(\"│ \", 1)` is exact."
  "│ ")

(defn version? [x] (and (string? x) (boolean (re-matches #"[0-9a-f]{8}" x))))

;; Blob <-> coordinates

(defn split-content-lines
  "Split a file blob into a vec of lines. A trailing empty element (from the
   file's final newline) is dropped, so the vector's count IS the file's line
   count and index 0 is line 1."
  [^String s]
  (let [v (vec (.split s "\n" -1))]
    (if (and (pos? (count v)) (= "" (peek v))) (pop v) v)))

(defn char-offset-at-line
  "Char offset in `content` where 0-based line `line-idx` starts. Returns
   `(count content)` when `line-idx` reaches past the last line."
  ^long [^String content ^long line-idx]
  (loop [pos
         0

         i
         0]

    (if (= i line-idx)
      pos
      (let [nl (str/index-of content "\n" pos)]
        (if nl (recur (inc (long nl)) (inc i)) (count content))))))

;; Versions

(defn content-version
  "The `version-width`-hex-char version of `content`: a prefix of its UTF-8 SHA-256."
  ^String [^String content]
  (subs (util/sha256-hex content) 0 version-width))

(defn parse-version
  "The bare version in `x`, which may carry the header's leading `@`, or nil."
  [x]
  (let [s
        (str/lower-case (str/trim (str x)))

        s
        (if (str/starts-with? s "@") (subs s 1) s)]

    (when (version? s) s)))

(defn header
  "The header line of a read: the path and its version."
  [rel version]
  (str rel " @" version))

(defn render-numbered-block
  "Render `[[line-number text] …]` as `N│ text` rows, each prefixed by `indent`.
   A CRLF line renders without its CR: the text a reader copies back must not
   carry a second line ending."
  (^String [tuples] (render-numbered-block tuples ""))
  (^String [tuples indent]
   (str/join "\n"
             (map (fn [[ln text]]
                    (let [s
                          (str text)

                          s
                          (if (str/ends-with? s "\r") (subs s 0 (dec (count s))) s)]

                      (str indent ln gutter s)))
                  tuples))))

;; The cache of shown versions

(def ^:private ^:const store-max-chars
  "Total chars of shown text that the cache keeps before it drops the oldest."
  (* 32 1024 1024))

(def ^:private ^:const store-max-file-chars
  "A larger file is not cached. A patch on a changed version of it is refused."
  (* 4 1024 1024))

(defonce ^:private store (atom {:texts {} :order clojure.lang.PersistentQueue/EMPTY :chars 0}))

(defn- store-key [path version] (str path "@" version))

(defn- evict
  [{:keys [texts order] :as state}]
  (if (and (> (long (:chars state)) (long store-max-chars)) (seq order))
    (let [k (peek order)]
      (recur (-> state
                 (update :texts dissoc k)
                 (update :order pop)
                 (update :chars - (count (get texts k))))))
    state))

(defn remember!
  "Cache `content` as a shown version of the file at canonical `path`. Answers its version."
  [path ^String content]
  (let [version
        (content-version content)

        k
        (store-key path version)]

    (when (<= (count content) (long store-max-file-chars))
      (swap! store (fn [state]
                     (if (contains? (:texts state) k)
                       state
                       (evict (-> state
                                  (assoc-in [:texts k] content)
                                  (update :order conj k)
                                  (update :chars + (count content))))))))
    version))

(defn recall
  "The cached text of `version` of the file at canonical `path`, or nil."
  [path version]
  (get-in @store [:texts (store-key path version)]))

;; The lines that each version showed

(def ^:private ^:const shown-max-keys
  "File versions whose shown lines the cache keeps before it drops the oldest."
  4096)

(defonce ^:private shown-store (atom {:lines {} :order clojure.lang.PersistentQueue/EMPTY}))

(defn- merge-ranges
  "Sorted, disjoint inclusive `[from to]` ranges that cover `ranges`. Adjacent ranges join."
  [ranges]
  (reduce (fn [acc [from to]]
            (let [[prev-from prev-to] (peek acc)]
              (if (and prev-to (<= (long from) (inc (long prev-to))))
                (conj (pop acc) [prev-from (max (long prev-to) (long to))])
                (conj acc [(long from) (long to)]))))
          []
          (sort-by first ranges)))

(defn note-shown!
  "Record that a `cat`, `grep` or `patch` result showed `ranges` (inclusive `[from to]`
   line pairs) of `version` of the file at canonical `path`. The record is for the
   whole process, like the text cache."
  [path version ranges]
  (let [ranges
        (filter (fn [[from to]]
                  (<= 1 (long from) (long to)))
                ranges)

        k
        (store-key path version)]

    (when (seq ranges)
      (swap! shown-store (fn [{:keys [lines] :as state}]
                           (let [state
                                 (if (contains? lines k)
                                   (update-in state [:lines k] #(merge-ranges (concat % ranges)))
                                   (-> state
                                       (assoc-in [:lines k] (merge-ranges ranges))
                                       (update :order conj k)))]
                             (if (> (count (:order state)) (long shown-max-keys))
                               (-> state
                                   (update :lines dissoc (peek (:order state)))
                                   (update :order pop))
                               state)))))
    nil))

(defn shown-ranges
  "The merged line ranges that results showed of `version` of the file at canonical
   `path`, or nil when no result recorded one."
  [path version]
  (get-in @shown-store [:lines (store-key path version)]))

(defn covers?
  "Whether merged `ranges` hold every line of `from`..`to`."
  [ranges from to]
  (boolean (some (fn [[a b]]
                   (<= (long a) (long from) (long to) (long b)))
                 ranges)))

(defn forget-all!
  "Empty the caches. Tests use it to model a version that the host no longer knows."
  []
  (reset! shown-store {:lines {} :order clojure.lang.PersistentQueue/EMPTY})
  (reset! store {:texts {} :order clojure.lang.PersistentQueue/EMPTY :chars 0}))

;; Line span -> char span

(defn- line-span->char-span
  "Convert a 0-based [line-start line-end) span to a [char-start char-end]
   substring span in `content`, keeping the region's trailing terminator
   OUTSIDE the replaced region."
  [^String content ^long line-start ^long line-end]
  (let [char-start
        (char-offset-at-line content line-start)

        char-end-raw
        (char-offset-at-line content line-end)

        char-end
        (let [e (long (if (and (< char-end-raw (count content))
                               (pos? char-end-raw)
                               (= \newline (.charAt content (dec char-end-raw))))
                        (dec char-end-raw)
                        char-end-raw))]
          ;; CRLF: the terminator is TWO chars. Keep the whole `\r\n` outside the
          ;; span, so a line replace does not rewrite that line's ending to bare LF.
          (if (and (> e char-start) (= \return (.charAt content (dec e)))) (dec e) e))]

    [char-start char-end]))

(defn edit-span
  "The CHAR SPAN that replaces lines `from-line`..`to-line` (1-based, inclusive) of
   `content` with `replacement`: `{:start S :end E :replacement R :from-line N :to-line N}`.
   The caller checks the range.

   Newline semantics: a replacement need NOT end in `\\n` — the matched region's
   terminator is preserved (`\\r\\n` stays `\\r\\n` on a CRLF file) — and one that
   DOES is not doubled: exactly one trailing terminator is dropped. An EMPTY
   replacement consumes the trailing newline, so the lines actually vanish."
  [^String content ^long from-line ^long to-line ^String replacement]
  (let [line-start (dec from-line)]
    (if (= "" (str replacement))
      ;; DELETION: take the WHOLE physical lines INCLUDING the trailing newline.
      {:start (char-offset-at-line content line-start)
       :end (char-offset-at-line content to-line)
       :replacement ""
       :from-line from-line
       :to-line to-line}
      (let [[char-start char-end] (line-span->char-span content line-start to-line)
            ;; The terminator is INSIDE the span only at EOF. Reading the last char of
            ;; a span that ends anywhere else answers the PREVIOUS line's `\n` when the
            ;; span's last line is EMPTY, and grew one extra blank line per replace.
            matched-ends-nl? (and (= (long char-end) (.length content))
                                  (< (long char-start) (long char-end))
                                  (= \newline (.charAt content (dec (long char-end)))))
            matched-ends-crlf? (and matched-ends-nl?
                                    (< (long char-start) (dec (long char-end)))
                                    (= \return (.charAt content (- (long char-end) 2))))
            ;; The replacement's OWN terminator is redundant. Exactly ONE is dropped,
            ;; so `"…\n\n"` still says "and then one blank line".
            body (cond (str/ends-with? replacement "\r\n")
                       (subs replacement 0 (- (.length replacement) 2))
                       (str/ends-with? replacement "\n")
                       (subs replacement 0 (dec (.length replacement)))
                       :else replacement)]

        {:start char-start
         :end char-end
         :replacement (if matched-ends-nl? (str body (if matched-ends-crlf? "\r\n" "\n")) body)
         :from-line from-line
         :to-line to-line}))))
