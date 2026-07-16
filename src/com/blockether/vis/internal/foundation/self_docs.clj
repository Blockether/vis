(ns com.blockether.vis.internal.foundation.self-docs
  "Vis self-documentation lookup — the `vis_docs` sandbox tool.

   Vis ships its documentation as embedded markdown pages under
   `vis-docs/` on the classpath (the same corpus the website and the
   gateway `/docs` site render, discovered via each artifact's
   `vis-docs/vis-docs.edn` manifest — so extensions' doc pages are
   lookup-able too). This namespace exposes that corpus to the MODEL
   through one observation tool so vis can answer questions about
   ITSELF — features, configuration, how to write an extension — from
   its real docs instead of guessing.

   Progressive disclosure: a short always-on prompt fragment says the
   docs exist and when to reach for them; page content is only paid
   for when actually fetched."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.docs :as docs]
            [com.blockether.vis.internal.extension :as extension]))

(defn- pages
  "All embedded doc pages from the classpath-wide `vis-docs` manifests.
   Re-collected per call (docs are tiny; matches the live `/docs` site's
   re-read-on-request behavior so dev edits show without a restart)."
  []
  (:pages (docs/collect)))

(defn- listing
  "Lean model-facing page index: slug/title/section + a one-line blurb,
   never the content, so listing stays cheap. Returned directly across the
   Python boundary, so keys are strings."
  [ps]
  (mapv (fn [{:keys [slug title section blurb]}]
          (cond-> {"slug" slug "title" title}
            section
            (assoc "section" section)

            blurb
            (assoc "blurb" blurb)))
        ps))

(defn- normalize-slug
  "Coerce a caller's slug arg to a bare, comparable slug: unwrap the
   map/kwargs shape, trim, drop a trailing `.md` (pages cross-link by
   filename), lower-case. Blank/nil → \"\" so the caller can route an empty ask to the page list."
  [slug]
  (-> (if (map? slug) (or (get slug "slug") (get slug :slug) "") slug)
      str
      str/trim
      (str/replace #"(?i)\.md$" "")
      str/lower-case))

(defn- vis-docs-tool
  "await vis_docs()      -> {\"pages\": [{\"slug\", \"title\", \"section\", \"blurb\"}, ...]} — list vis's own embedded doc pages, each with a one-line blurb of what it covers.
await vis_docs(slug)  -> {\"slug\", \"title\", \"section\", \"content\"} — one page's full markdown.
Slug matching is forgiving: the map/kwargs shape, a trailing `.md`, surrounding whitespace, and case are all tolerated (`vis_docs(\"extending.md\")` == `vis_docs(\"Extending\")` == `vis_docs({\"slug\": \"extending\"})`).
Vis's OWN documentation (features, configuration, extending vis). Use ONLY for questions about vis itself, never for the host project."
  ([] (extension/success {:result {"pages" (listing (pages))}}))
  ([slug]
   (let [ps
         (pages)

         want
         (normalize-slug slug)]

     (cond
       ;; blank/absent slug (nil, "", {}, whitespace-only) == the no-arg
       ;; "list pages" ask the help advertises — return the index, don't error.
       (str/blank? want) (extension/success {:result {"pages" (listing ps)}})
       :else (if-let [page (some #(when (= want (normalize-slug (:slug %))) %) ps)]
               (extension/success
                 {:result (cond-> {"slug" (:slug page) "title" (:title page) "content" (:md page)}
                            (:section page)
                            (assoc "section" (:section page)))})
               (extension/failure {:result nil
                                   :error {:message (str "Unknown vis docs slug " (pr-str want) ".")
                                           :hint (str
                                                   "Valid slugs: "
                                                   (str/join ", " (map :slug ps))
                                                   ". Call vis_docs() to list all pages.")}}))))))

(defn- docs-cell
  "One-line, pipe-escaped, length-capped text for a GFM table cell (the TUI
   table painter draws cells as plain text, so no inline markdown here)."
  [s max-len]
  (let [s (-> (str s)
              (str/replace #"\s+" " ")
              str/trim
              (str/replace "|" "\\|"))]
    (if (> (count s) (long max-len)) (str (subs s 0 (max 0 (dec (long max-len)))) "…") s)))

(defn- render-vis-docs
  "Op-card renderer for `vis_docs`. The page LISTING (no-arg / blank slug)
   paints a GFM table — slug · section · title · blurb — the channels draw as
   a boxed grid; a single fetched page shows a titled headline over the page's
   full markdown body."
  [r]
  (cond (get r "pages")
        (let [ps
              (get r "pages")

              n
              (count ps)

              header
              ["| Slug | Section | Title | Blurb |" "|------|---------|-------|-------|"]

              rows
              (map (fn [p]
                     (str "| "
                          (docs-cell (get p "slug") 28)
                          " | "
                          (docs-cell (or (not-empty (str (get p "section"))) "—") 16)
                          " | "
                          (docs-cell (get p "title") 40)
                          " | "
                          (docs-cell (or (get p "blurb") "—") 90)
                          " |"))
                   ps)]

          {:summary (str n " vis docs page" (when (not= 1 n) "s"))
           :body (str/join "\n" (concat header rows))})
        (get r "content") (let [title
                                (not-empty (str (get r "title")))

                                section
                                (not-empty (str (get r "section")))

                                slug
                                (str (get r "slug"))]

                            {:summary (str (or title slug) (when section (str " · " section)))
                             :body (str (get r "content"))})
        :else nil))

(def vis-docs-symbol
  (vis/symbol #'vis-docs-tool {:symbol 'vis-docs :tag :observation :render render-vis-docs}))

(def symbols [vis-docs-symbol])

(def prompt
  "Always-on fragment for the foundation prompt: the docs exist, when to
   use them, and the two or three slugs that answer the common asks.
   Content stays out of the prompt until a page is fetched."
  (str "Vis self-docs: vis ships its own documentation as embedded pages. ONLY when "
       "the user asks about vis ITSELF — what it is, its features, how to configure "
       "it, or how to extend it (write an extension, add tools or doc pages) — look "
       "the answer up instead of guessing: `await vis_docs()` lists the pages "
       "(slug/title/section + a one-line blurb of what each covers); "
       "`await vis_docs(\"<slug>\")` returns that page's full "
       "markdown. Key slugs: \"extending\" (create Clojure extensions), "
       "\"python-extensions\" (drop-in .py extensions: tools/slash/hooks), "
       "\"configuration\" (config files/providers), \"index\" (overview/features). "
       "Read the relevant "
       "page before answering questions about vis's own behavior; these docs "
       "describe vis, NOT the host project."))
