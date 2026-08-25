(ns com.blockether.vis.internal.docs-test
  "Docs renderer: cross-page markdown links must resolve in BOTH output
   modes (live `/docs/<slug>`, static `<slug>.html`), and the live
   handler tolerates literal `<slug>.md` deep links with a redirect.
   Plus the CONTENT invariants: `extending.md` is where an extension author learns
   what `doc(name)` renders, what an `apropos` row previews, and how a live view is
   driven from Python."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.docs :as docs]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private rewrite-md-links @#'docs/rewrite-md-links)

(defdescribe rewrite-md-links-test
             (it "live mode: relative page.md -> /docs/page, fragment preserved"
                 (expect (= "<a href=\"/docs/skills\">x</a>"
                            (rewrite-md-links "<a href=\"skills.md\">x</a>" :live)))
                 (expect (= "<a href=\"/docs/configuration#router\">x</a>"
                            (rewrite-md-links "<a href=\"configuration.md#router\">x</a>" :live))))
             (it "static mode: relative page.md -> page.html"
                 (expect (= "<a href=\"skills.html\">x</a>"
                            (rewrite-md-links "<a href=\"skills.md\">x</a>" :static))))
             (it "absolute URLs and absolute paths pass through untouched"
                 (let [ext
                       "<a href=\"https://example.com/readme.md\">x</a>"

                       abs
                       "<a href=\"/raw/readme.md\">x</a>"]

                   (expect (= ext (rewrite-md-links ext :live)))
                   (expect (= abs (rewrite-md-links abs :live))))))

(defdescribe
  rendered-pages-test
  (it "no live page body carries a dangling .md href (regression: every cross-link resolves)"
      (let [{:keys [pages] :as site} (docs/collect)]
        (expect (seq pages))
        (doseq [page pages]
          (let [html (docs/page-html site page :live)]
            (expect (not (re-find #"href=\"[^\"/:][^\":]*\.md[\"#]" html))
                    (str "dangling .md link in live page " (:slug page))))))))

(defdescribe handle-md-redirect-test
             (it "GET /docs/<slug>.md permanent-redirects to /docs/<slug>"
                 (let [resp (docs/handle {:uri "/docs/skills.md" :headers {}})]
                   (expect (= 301 (:status resp)))
                   (expect (= "/docs/skills" (get-in resp [:headers "location"])))))
             (it "an unknown .md path still falls through as nil"
                 (expect (nil? (docs/handle {:uri "/docs/nope-zzz.md" :headers {}})))))

(defdescribe
  collect-memoization-test
  "Every docs request and every corpus rebuild re-read and re-rendered all 16
   pages, so serving `/docs` and asking `apropos` a question both paid ~8 ms of
   markdown rendering that nothing had invalidated."
  (it "renders the site once and answers the same value"
      (expect (identical? (docs/collect) (docs/collect)))))

(defn- extending-md [] (:md (first (filter #(= "extending" (:slug %)) (:pages (docs/collect))))))

 ;; A tool page is rendered from the entry contract and previewed as a three-field
 ;; apropos row. `extending.md` is the author-facing contract for both renderings,
 ;; so this test names the page that must change with either.
(defdescribe
  extending-page-teaches-its-renderings-test
  (it "names every entry key `doc(name)` renders, and both structural lines"
      (let [md (extending-md)]
        (expect (string? md))
        (doseq [needle [":description" ":params" ":result" ":call" "Keys:" "(REQUIRED)"
                        "Raw result:"]]
          (expect (str/includes? md needle) (str "extending.md never mentions " needle)))))
  (it "shows an `apropos` item with all three fields"
      (let [md (extending-md)]
        (doseq [needle ["AproposItem(" "type=" "name=" "body="]]
          (expect (str/includes? md needle)
                  (str "extending.md never shows " needle " in an apropos item")))))
  (it "names the shim keys that drive discovery and the page they answer with"
      (let [md (extending-md)]
        (doseq [needle [":shim/imports" ":shim/globals" ":shim/source" ":shim/docs"]]
          (expect (str/includes? md needle) (str "extending.md never mentions " needle))))))

;; A live view is the one primitive an author cannot infer from the field builders:
;; its verbs differ per node type, and `vis.output` deliberately does not match the
;; `log` node it builds (`vis.log` is the engine log line). When that Python surface
;; is renamed, this test names the page that has to be renamed with it.
(defdescribe
  extending-page-teaches-the-live-view-test
  (it "names the opener, the log builder that could not be called `log`, and what a loop reads"
      (let [md (extending-md)]
        (doseq [needle ["vis.live(" "vis.output(" "upsert(" "is_interrupted" "vis.Interrupted"
                        "flush_ms" "view.is_from_human" "view.note"]]
          (expect (str/includes? md needle) (str "extending.md never mentions " needle)))))
  ;; Layout is the half an author cannot infer: without these two paragraphs a run writes a
  ;; second node where one paragraph BESIDE the table was meant, and marks up a string the
  ;; page never promised to paint.
  (it "teaches where a node stands and what a human-facing string may carry"
      (let [md (extending-md)]
        (doseq [needle ["vis.row(" "vis.column(" "inline markdown" "wraps and justifies"
                        "stay verbatim" "`Escape` or `Enter` sends the stop"]]
          (expect (str/includes? md needle) (str "extending.md never mentions " needle))))))

;;; ── The page contract ───────────────────────────────────────────────────────
;; Every rule below is one the RENDERER already assumes (see the `docs` ns
;; docstring, which states the contract): a title that disagrees with the
;; sidebar, a heading too deep to be given an anchor, a `#fragment` pointing at
;; nothing and a page nothing links to are all invisible until a reader walks
;; into them.

(def ^:private fence-languages
  "Languages a fenced block may declare. ONE set, so the same kind of block is
   highlighted the same way on every page."
  #{"bash" "clojure" "edn" "ini" "json" "markdown" "python" "text" "toml" "yaml"})

(defn- scan
  "PURE: `{:headings [[line level text] …] :fences [[line info] …]}` for `md`.
   Fenced blocks are skipped, so a `#` comment inside a shell example is not
   mistaken for a heading."
  [^String md]
  (loop [ls
         (str/split-lines md)

         n
         1

         in-fence?
         false

         acc
         {:headings [] :fences []}]

    (if-let [l (first ls)]
      (cond (str/starts-with? l "```")
            (recur (rest ls)
                   (inc n)
                   (not in-fence?)
                   (if in-fence? acc (update acc :fences conj [n (str/trim (subs l 3))])))
            in-fence? (recur (rest ls) (inc n) in-fence? acc)
            :else (if-let [[_ hashes text] (re-matches #"(#{1,6}) (.*)" l)]
                    (recur (rest ls)
                           (inc n)
                           in-fence?
                           (update acc :headings conj [n (count hashes) (str/trim text)]))
                    (recur (rest ls) (inc n) in-fence? acc)))
      acc)))

(defn- lead-paragraph
  "PURE: the prose between a page's H1 and its first `##`."
  [^String md]
  (->> (str/split-lines md)
       (drop-while #(not (str/starts-with? % "# ")))
       (drop 1)
       (take-while #(not (str/starts-with? % "## ")))
       (str/join "\n")
       str/trim))

(defn- see-also-links
  "PURE: the relative page links under a page's `## See also` heading."
  [^String md]
  (let [tail (second (str/split md #"(?m)^## See also$" 2))]
    (re-seq #"\]\(([A-Za-z0-9._-]+\.md)" (str tail))))

(def ^:private max-unit-chars
  "Characters one paragraph — or one list item with its continuation lines — may
   carry before it stops being prose and becomes a table nobody drew. Roughly 120
   words: past that a reader scans instead of reading, and the structure is
   already inside the sentence."
  800)

(defn- text-units
  "PURE: `[[line text] …]` — every prose paragraph of `md`, plus every list item
   with the lines that continue it, joined into one string. Fenced blocks,
   headings, tables and quotes carry their own shape and are skipped."
  [^String md]
  (let [close (fn [acc {:keys [line buf]}]
                (if (seq buf) (conj acc [line (str/join " " buf)]) acc))]
    (loop [ls (str/split-lines md)
           n 1
           in-fence? false
           cur {:line 0 :buf []}
           acc []]

      (if (empty? ls)
        (close acc cur)
        (let [l (str/trim (str (first ls)))
              item? (boolean (re-matches #"(?s)([-*+]|\d+[.)])\s.*" l))
              skip? (or (str/blank? l)
                        (str/starts-with? l "#")
                        (str/starts-with? l "|")
                        (str/starts-with? l ">"))]

          (cond (str/starts-with? l "```")
                (recur (rest ls) (inc n) (not in-fence?) {:line 0 :buf []} (close acc cur))
                in-fence? (recur (rest ls) (inc n) in-fence? cur acc)
                skip? (recur (rest ls) (inc n) in-fence? {:line 0 :buf []} (close acc cur))
                item? (recur (rest ls) (inc n) in-fence? {:line n :buf [l]} (close acc cur))
                :else (recur (rest ls)
                             (inc n)
                             in-fence?
                             (if (seq (:buf cur)) (update cur :buf conj l) {:line n :buf [l]})
                             acc)))))))

(defn- page-canon
  "PURE: every way `page` breaks the page contract, as reader-facing lines.
   `anchors` is `{slug #{anchor-id}}` for the whole site, so a cross-page
   fragment is checked against the toc of the page it points AT, and `pages` is
   the whole page list, which the landing page has to be a map of."
  [{:keys [slug title md blurb toc]} anchors pages]
  (let [home?
        (= "index" slug)

        {:keys [headings fences]}
        (scan md)

        h1s
        (filter (fn [[_ lvl _]]
                  (= 1 lvl))
                headings)

        h2-texts
        (keep (fn [[_ lvl text]]
                (when (= 2 lvl) text))
              headings)

        first-line
        (str/trim (str (first (remove str/blank? (str/split-lines md)))))

        ids
        (map :id toc)

        say
        (fn [& parts]
          (str slug ": " (apply str parts)))]

    (concat
      (if home?
        (concat (when (seq h1s)
                  [(say
                     "the landing page carries no H1 of its own — the themed hero is its title")])
                (for [{other-slug :slug other-title :title}
                      pages

                      :when (not= "index" other-slug)
                      :let [link
                            (str "[" other-title "](" other-slug ".md)")]
                      :when (not (str/includes? md link))]

                  (say "the landing page never links " link " — it is the map of this manual")))
        (concat
          (when-not (= 1 (count h1s)) [(say "wants exactly one H1, has " (count h1s))])
          (when-not (= (str "# " title) first-line)
            [(say "opens with " (pr-str first-line)
                  ", not with its manifest title " (pr-str (str "# " title)))])
          (when (< (count (lead-paragraph md)) 60)
            [(say "has no lead paragraph between its H1 and the first `##`")])
          (when-not (= "See also" (last h2-texts))
            [(say "ends on `## " (last h2-texts) "` — the last `##` of a page is `See also`")])
          (when (< (count (see-also-links md)) 2)
            [(say "`See also` names fewer than two sibling pages")])))
      (for [[line lvl text]
            headings

            :when (> (long lvl) 3)]

        (say "line " line " is an h" lvl " (" text ") — too deep to be given an anchor"))
      (->> headings
           (map (fn [[_ lvl _]]
                  lvl))
           (partition 2 1)
           (keep (fn [[a b]]
                   (when (> (long b) (inc (long a))) (say "a heading jumps h" a " → h" b)))))
      (for [[id n]
            (frequencies ids)

            :when (> (long n) 1)]

        (say "anchor #" id " is claimed " n " times"))
      (for [[line lang]
            fences

            :when (not (contains? fence-languages lang))]

        (say "the fence on line " line
             " declares " (if (str/blank? lang) "no language" (pr-str lang))))
      (for [[line text]
            (text-units md)

            :when (> (count text) (long max-unit-chars))]

        (say "the paragraph on line "
             line
             " runs "
             (count text)
             " characters — over "
             max-unit-chars
             ", so it is a list or a table wearing prose"))
      (when (str/blank? (str blurb)) [(say "has no `:blurb` in the manifest")])
      (for [[_ target frag]
            (re-seq #"\]\((?!https?:|/|#)([A-Za-z0-9._-]+\.md)(#[A-Za-z0-9._-]+)?\)" md)

            :let [target-slug
                  (str/replace target #"\.md$" "")]
            :when (or (not (contains? anchors target-slug))
                      (and frag (not (contains? (get anchors target-slug) (subs frag 1)))))]

        (say "links to " target (str frag) ", which no page answers"))
      (for [[_ frag]
            (re-seq #"\]\((#[A-Za-z0-9._-]+)\)" md)

            :when (not (contains? (set ids) (subs frag 1)))]

        (say "links to " frag " on this page, which is not a heading here")))))

(defdescribe
  docs-page-canon-test
  "One canonical page shape, so the pages read as ONE manual instead of sixteen
   documents: the contract is stated in the `docs` ns docstring and checked here."
  (it "every page keeps it"
      (let [{:keys [pages]}
            (docs/collect)

            anchors
            (into {} (map (juxt :slug #(set (map :id (:toc %))))) pages)

            broken
            (mapcat #(page-canon % anchors pages) pages)]

        (expect (seq pages))
        (expect (empty? broken)
                (str/join "\n" (cons "pages that break the docs page contract:" broken))))))
