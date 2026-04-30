(ns com.blockether.vis.ext.channel-tui.links
  "Walk a rendered message's text and pull every clickable reference
   out as a structured vec the renderer can paint as a chrome row.

   Three kinds we recognise:

     :image  `![alt](url)`                  \u2014 from `(md/image \u2026)`
     :url    `[text](url)`                  \u2014 from `(md/link \u2026)` / `(md/anchor \u2026)`
     :file   `[path:line](path#Lline)` etc. \u2014 from `(md/file-link \u2026)`

   `:url` and `:file` are distinguished AFTER the bracket parse:
   when the URL has no scheme prefix and the link text contains the
   same path (or the URL is a `path#Lline` shape) we classify it as
   `:file`. Everything else stays `:url`.

   Pure. Heavily tested \u2014 the ref extraction is the trust
   boundary the click handler delegates the URL to.

   Anchor-only links (`[text](#slug)`) are dropped: there's nothing
   for the OS opener to do with an in-document anchor on a chat
   surface that doesn't render anchors."
  (:require
   [com.blockether.vis.ext.channel-tui.external-opener :as opener]))

(set! *warn-on-reflection* true)

;; =============================================================================
;; Bracket-link tokeniser
;; =============================================================================

(def ^:private link-re
  "Match `[text](url)` and `![alt](url)` shapes. `text` may not
   contain `]`; `url` may not contain `)`. Pre-CommonMark simple,
   but covers everything the `md/` builders emit. The optional `!`
   prefix is captured separately so we can flag images.

   We use a non-greedy quantifier on the text portion to handle
   `[a](b)[c](d)` runs without backtracking the whole line."
  #"(!)?\[([^\]]*?)\]\(([^)\s]+)(?:\s+\"[^\"]*\")?\)")

(defn- file-shape?
  "True when `text`/`url` look like the `md/file-link` shape:
   either text is `path:line` and url is `path#Lline`, or both are
   the same plain path. Pure."
  [^String text ^String url]
  (or
    ;; (md/file-link path) -> [path](path)
    (= text url)
    ;; (md/file-link path line) -> [path:N](path#LN)
    (let [m (re-matches #"^(.+?):(\d+)$" text)
          n (re-matches #"^(.+?)#L(\d+)$" url)]
      (and m n (= (nth m 1) (nth n 1)) (= (nth m 2) (nth n 2))))))

(defn- anchor-only?
  "True when `url` is a same-document anchor (`#foo`)."
  [^String url]
  (boolean (re-matches #"^#.*" url)))

(defn parse-md-refs
  "Return a vec of `{:kind :text :url :line? :scheme :enabled?}`
   entries, one per recognised reference in `s`. Order preserved.

   - `:kind`     :image | :url | :file
   - `:text`     visible link text (or alt text for images)
   - `:url`      raw target as the model wrote it
   - `:line`     integer line number (only for :file with `#Lline`),
                 else nil
   - `:scheme`   classification from `external-opener/classify-scheme`
                 OR `:rejected` for anchor-only links
   - `:enabled?` true when `:scheme` is on the whitelist AND the
                 path-escape guard would pass; false otherwise"
  [^String s]
  (if (or (nil? s) (zero? (.length s)))
    []
    (loop [m       (re-matcher link-re s)
           out     (transient [])]
      (if (.find m)
        (let [bang (.group m 1)
              text (.group m 2)
              url  (.group m 3)
              kind (cond
                     (= bang "!")        :image
                     (file-shape? text url) :file
                     :else                :url)
              [_ line] (when (= kind :file)
                         (re-matches #"^.+#L(\d+)$" url))
              scheme  (cond
                        (anchor-only? url) :rejected
                        :else              (opener/classify-scheme url))
              ;; Compute :enabled? once at extraction time so the
              ;; renderer + click handler agree on the answer
              ;; without having to re-classify on every paint /
              ;; lookup. `safe-target` does the cwd-escape check.
              enabled? (and (not= scheme :rejected)
                         (some? (opener/safe-target url)))]
          (recur m (conj! out {:kind     kind
                               :text     text
                               :url      url
                               :line     (some-> line parse-long)
                               :scheme   scheme
                               :enabled? enabled?})))
        (persistent! out)))))
