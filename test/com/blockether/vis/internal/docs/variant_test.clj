(ns com.blockether.vis.internal.docs.variant-test
  "Paired Python and HTTP variants. The agent's `doc` and `apropos` read only the
   Python variant, without markup. The site shows both variants behind one switch."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.internal.docs.core :as docs]
            [com.blockether.vis.internal.docs.corpus :as dc]
            [com.blockether.vis.internal.python.env :as env]
            [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private fixture
  "A paired page: shared text, a Python block, an HTTP block, Python code with two
   blank lines that must stay, and the markup shown as an example in fenced code."
  (str "# Fixture API\n\n" "Shared lead.\n\n"
       "<div data-variant=\"python\">\n\n" "Python prose.\n\n"
       "```python\ndef a():\n    pass\n\n\ndef b():\n    pass\n```\n\n" "</div>\n\n"
       "<div data-variant=\"http\">\n\n" "HTTP prose.\n\n"
       "```bash\ncurl http://127.0.0.1/v1/x\n```\n\n" "</div>\n\n"
       "## Example\n\n" "```markdown\n<div data-variant=\"http\">\n</div>\n```\n"))

(defdescribe
  variant-text-test
  (it "gives the Python variant without markup, the HTTP block or extra blank lines"
      (expect (= (str "# Fixture API\n\nShared lead.\n\nPython prose.\n\n"
                      "```python\ndef a():\n    pass\n\n\ndef b():\n    pass\n```\n\n"
                      "## Example\n\n```markdown\n<div data-variant=\"http\">\n</div>\n```\n")
                 (dc/variant-text fixture "python"))))
  (it "gives the HTTP variant the same way"
      (expect (= (str "# Fixture API\n\nShared lead.\n\nHTTP prose.\n\n"
                      "```bash\ncurl http://127.0.0.1/v1/x\n```\n\n"
                      "## Example\n\n```markdown\n<div data-variant=\"http\">\n</div>\n```\n")
                 (dc/variant-text fixture "http"))))
  (it "returns text without variant blocks unchanged"
      (let [text "# Plain\n\nNo blocks.\n"]
        (expect (identical? text (dc/variant-text text "python")))))
  (it "names the variants in page order, each once, and ignores fenced markup"
      (expect (= ["python" "http"] (dc/variants fixture)))
      (expect (= [] (dc/variants "```markdown\n<div data-variant=\"http\">\n</div>\n```\n")))))

(defn- merged-pages
  "The site pages that give paired variants."
  []
  (filter (comp seq :variants) (:pages (docs/collect))))

(defn- http-only-lines
  "The trimmed text lines of the HTTP blocks of `md` that the Python variant never
   says."
  [md]
  (let [python (dc/variant-text md "python")]
    (into []
          (comp (filter #(and (= "http" (:variant %)) (not (:tag %))))
                (map (comp str/trim :text))
                (remove str/blank?)
                (remove #(str/includes? python %)))
          (dc/variant-lines md))))

(defdescribe
  agent-reads-python-variant-test
  "The agent writes Python, so `doc` and `apropos` give it the Python variant of a
   merged API page, never the HTTP block or the variant markup."
  (it "covers the merged API pages" (expect (seq (merged-pages))))
  (it "doc() prints a merged page as its Python variant, with no markup and no HTTP-only text"
      (doseq [{:keys [slug md]}
              (merged-pages)

              :let [text
                    (#'env/doc-text {} slug nil)

                    http-only
                    (http-only-lines md)]]

        (expect (str/includes? text (str/trim (dc/variant-text md "python"))) slug)
        (expect (not (str/includes? text "data-variant")) slug)
        (expect (not-any? #(= "</div>" (str/trim %)) (str/split-lines text)) slug)
        (expect (seq http-only) (str slug " has no HTTP-only text to check"))
        (doseq [line http-only]
          (expect (not (str/includes? text line)) (str slug " shows HTTP-only text: " line)))))
  (it "publishes the Python variant as the agent's Markdown, beside the whole page"
      ;; The site build writes `:agent-md` to `<slug>.md` and `llms-full.txt`, which
      ;; agents read. The user's rule is the same as for `doc`: always Python.
      (doseq [{:keys [slug md agent-md]}
              (merged-pages)]

        (expect (= (dc/variant-text md dc/agent-variant) agent-md) slug)
        (expect (not (str/includes? agent-md "data-variant")) slug)
        (expect (str/includes? md "<div data-variant=\"http\">") slug)
        (doseq [line (http-only-lines md)]
          (expect (not (str/includes? agent-md line)) (str slug " publishes HTTP-only text: " line)))))
  (it "keeps both variants for the site and filters only what the agent reads"
      (doseq [{:keys [name text]}
              (filter #(seq (dc/variants (:text %))) (dc/pages))

              :let [agent
                    (first (filter #(= name (:name %)) (dc/entries)))]]

        (expect (str/includes? text "<div data-variant=\"http\">") name)
        (expect (= (dc/variant-text text dc/agent-variant) (:text agent)) name)))
  (describe
    "apropos"
    (let [page
          (str "# Variant fixture\n\n"
               "<div data-variant=\"python\">\n\nThe zebrafish opening.\n\n</div>\n\n"
               "<div data-variant=\"http\">\n\nThe quokka opening.\n\n</div>\n\n"
               "## When to use\n\n- **First problem.** Read on.\n- **Second problem.** Read on.\n")

          rows
          (fn [pattern]
            (filter #(= "variant-fixture" (get % "name")) (#'env/apropos-rows {} pattern)))]

      (it "finds a page by its Python variant and never by its HTTP block"
          ;; Render the site first: it refuses a page that its navigation never names.
          (docs/collect)
          (try (dc/register-source! ::fixture
                                    (constantly [{:name "variant-fixture" :kind "doc" :text page}]))
               (expect (= ["The zebrafish opening."] (map #(get % "body") (rows "zebrafish"))))
               (expect (empty? (rows "quokka")))
               (expect (empty? (rows "data-variant")))
               (let [text (#'env/doc-text {} "variant-fixture" nil)]
                 (expect (str/includes? text "zebrafish"))
                 (expect (not (str/includes? text "quokka"))))
               (finally (dc/register-source! ::fixture (constantly []))))))))

(defn- article
  "The rendered article of a full page, without the inlined theme and scripts."
  [html]
  (second (re-find #"(?s)<article class=\"content\">(.*?)</article>" html)))

(defn- code-words
  "Long words of the fenced code in the `variant` blocks of `md`, such as a route
   or a method path. They hold no character that HTML escapes, so search text
   holds them verbatim."
  [md variant]
  (into []
        (comp (filter #(and (= variant (:variant %)) (:fenced? %)))
              (remove #(str/starts-with? (str/trim (:text %)) "```"))
              (mapcat #(re-seq #"[A-Za-z0-9_./$-]{10,}" (:text %)))
              (distinct))
        (dc/variant-lines md)))

(defdescribe
  site-variant-switch-test
  "The site shows both variants. One switch selects the variant that a reader
   sees, and without JavaScript both variants show with their labels."
  (let [{:keys [pages] :as site} (docs/collect)]
    (it "renders one hidden switch after the H1 of a merged page, with Python first"
        (doseq [{:keys [slug] :as page} (merged-pages)
                mode [:static :live]
                :let [body (article (docs/page-html site page mode))]]

          (expect (= 1 (count (re-seq #"class=\"variant-switch\"" body))) slug)
          (expect (str/includes? body
                                 (str "</h1><div class=\"variant-switch\" role=\"group\""
                                      " aria-label=\"Show examples for\" hidden>"
                                      "<button type=\"button\" data-variant-choice=\"python\""
                                      " aria-pressed=\"true\">Python</button>"
                                      "<button type=\"button\" data-variant-choice=\"http\""
                                      " aria-pressed=\"false\">HTTP</button></div>"))
                  slug)))
    (it "renders every block of both variants with its label and its Markdown"
        (doseq [{:keys [slug md] :as page} (merged-pages)
                mode [:static :live]
                :let [body (article (docs/page-html site page mode))
                      opened (frequencies (keep #(when (= :open (:tag %)) (:variant %))
                                                (dc/variant-lines md)))]]

          (expect (pos? (long (get opened "http" 0))) slug)
          (expect (= (get opened "python") (get opened "http")) slug)
          (doseq [[variant label] [["python" "Python"] ["http" "HTTP"]]]
            ;; A block element after the label shows that commonmark rendered the
            ;; Markdown in the block instead of passing it through as raw HTML.
            (expect (= (get opened variant)
                       (count (re-seq (re-pattern (str
                                                    "<div data-variant=\""
                                                    variant
                                                    "\"><p class=\"variant-label\">"
                                                    label
                                                    "</p>\n<(?:p|pre|ul|ol|table|blockquote)[ >]"))
                                      body)))
                    (str slug " " variant)))
          (expect (not (str/includes? body "```")) slug)))
    (it "gives a page without variants no switch and no labels"
        (doseq [{:keys [slug variants] :as page} pages
                :when (empty? variants)
                mode [:static :live]
                :let [body (article (docs/page-html site page mode))]]

          (expect (some? body) slug)
          (expect (not (str/includes? body "variant-switch")) slug)
          (expect (not (str/includes? body "variant-label")) slug)))
    (it "loads the switch behaviour and its styles in both modes"
        (let [page (first (merged-pages))]
          (expect (str/includes? (docs/page-html site page :static)
                                 "<script src=\"assets/docs.js\" defer></script>"))
          (expect (str/includes? (docs/page-html site page :live) "'vis-docs-variant'"))
          (expect (str/includes? (docs/page-html site page :live) ".variant-switch[hidden]"))))
    (it "indexes both variants of a merged page for site search"
        (let [rows (get (json/read-json (:body (docs/handle {:uri "/docs/assets/search.json"
                                                             :headers {}})))
                        "pages")]
          (expect (seq rows))
          (doseq [{:keys [slug md]} (merged-pages)
                  :let [text (str/join " "
                                       (keep #(when (str/starts-with? (get % "href")
                                                                      (str "/docs/" slug))
                                                (get % "text"))
                                             rows))]]

            (expect (not (str/includes? text "data-variant")) slug)
            (doseq [variant ["python" "http"]
                    :let [words (code-words md variant)]]

              (expect (seq words) (str slug " has no " variant " code to find"))
              (expect (every? #(str/includes? text %) words)
                      (str slug " search text misses the " variant " variant"))))))))
