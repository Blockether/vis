(ns com.blockether.vis.internal.prompt-templates
  "File-based prompt templates — pi-style slash-expandable markdown prompts.

   A template is a `*.md` file whose body becomes the user message when
   the user types `/<name> [args…]`. Discovery, project wins over global:

     1. `<workspace>/.vis/prompts/*.md`   (project)
     2. `~/.vis/prompts/*.md`             (user-global)

   Frontmatter is the same minimal `---` fenced `key: value` block the
   harness discovery reads: `name` (defaults to the filename stem) and
   `description`. The body is the template.

   Argument handling matches the common harness convention: when the
   body contains `$ARGUMENTS` every occurrence is substituted with the
   raw argument string (empty when none given); otherwise non-blank
   args are appended after the body on their own paragraph.

   Extensions can contribute DYNAMIC templates through
   `register-provider!` — e.g. the harness extension exposes every
   discovered skill as `/skill:<name>`. File templates win on a name
   collision; among providers, registration order wins.

   Dispatch: the engine consults `expand` ONLY for slash texts no
   registered extension slash claimed (`slash/dispatch` returned
   `:reason :unknown`), so real slash commands always win — same
   precedence pi uses."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.internal.workspace :as workspace]
   [taoensso.telemere :as tel]))

;; =============================================================================
;; Frontmatter — minimal `---` fenced `key: value`, no YAML dependency
;; =============================================================================

(defn parse-frontmatter
  "Split a markdown doc into `{:meta {kw str} :body str}`. A leading
   `---`-fenced block is parsed as `key: value` lines. No frontmatter →
   `{:meta {} :body <whole>}`. Keys are lower-cased keywords."
  [content]
  (let [content (str content)]
    (if-let [[_ fm body] (re-find #"(?s)\A---\r?\n(.*?)\r?\n---\r?\n?(.*)\z" content)]
      {:meta (into {}
               (keep (fn [line]
                       (when-let [[_ k v] (re-matches #"\s*([A-Za-z][\w-]*)\s*:\s*(.*)" line)]
                         [(keyword (str/lower-case k)) (str/trim v)])))
               (str/split-lines fm))
       :body (str/triml body)}
      {:meta {} :body (str/triml content)})))

(defn- non-blank [s] (let [s (some-> s str str/trim)] (when-not (str/blank? s) s)))

;; =============================================================================
;; File template discovery
;; =============================================================================

(defn- name-stem [^String filename]
  (str/replace filename #"\.md\z" ""))

(defn- md-files
  [^java.io.File d]
  (when (.isDirectory d)
    (->> (.listFiles d)
      (filter #(and (.isFile ^java.io.File %)
                 (str/ends-with? (.getName ^java.io.File %) ".md")))
      (sort-by #(.getName ^java.io.File %)))))

(defn- parse-template-file
  [scope ^java.io.File f]
  (try
    (let [{:keys [meta body]} (parse-frontmatter (slurp f))
          nm (or (non-blank (:name meta)) (non-blank (name-stem (.getName f))))]
      (when (and nm (not (str/blank? body)))
        {:name        nm
         :description (or (non-blank (:description meta)) "")
         :body        body
         :scope       scope
         :path        (.getAbsolutePath f)}))
    (catch Throwable t
      (tel/log! {:level :warn :id ::template-parse-failed
                 :data  {:path (.getAbsolutePath f) :error (ex-message t)}})
      nil)))

(defn dedup-by-name
  "First occurrence of each `:name` wins (precedence = input order)."
  [entries]
  (->> entries
    (reduce (fn [[seen out] e]
              (if (contains? seen (:name e))
                [seen out]
                [(conj seen (:name e)) (conj out e)]))
      [#{} []])
    second))

(defn discover-in
  "Parse every `*.md` template under `project-dir` then `global-dir`
   (project wins on a name collision). Pure I/O; exposed for testing
   against fixture roots. Either dir may be nil/missing."
  [^java.io.File project-dir ^java.io.File global-dir]
  (dedup-by-name
    (vec (concat
           (keep #(parse-template-file :project %) (when project-dir (md-files project-dir)))
           (keep #(parse-template-file :global %) (when global-dir (md-files global-dir)))))))

(defn- project-prompts-dir ^java.io.File []
  (try (io/file (workspace/cwd) ".vis" "prompts")
    (catch Throwable _ nil)))

(defn- global-prompts-dir ^java.io.File []
  (io/file (System/getProperty "user.home") ".vis" "prompts"))

;; ── marker cache: stat-only revalidation, content re-read on change ─────────

(defonce ^:private cache (atom nil))

(defn- dir-marker
  [^java.io.File d]
  (when d
    [(.getAbsolutePath d)
     (mapv (fn [^java.io.File f] [(.getName f) (.lastModified f) (.length f)])
       (or (md-files d) []))]))

(defn- template-marker []
  [(dir-marker (project-prompts-dir)) (dir-marker (global-prompts-dir))])

(defn file-templates
  "Discovered file templates, marker-cached: re-parsed only when the
   prompts dirs (or any file in them) change."
  []
  (let [m (template-marker)
        c @cache]
    (if (and c (= m (:marker c)))
      (:templates c)
      (:templates (reset! cache {:marker m
                                 :templates (discover-in (project-prompts-dir)
                                              (global-prompts-dir))})))))

(defn reload!
  "Drop the file-template cache and rescan. Returns the template vec."
  []
  (reset! cache nil)
  (file-templates))

;; =============================================================================
;; Providers — dynamic templates contributed by extensions
;; =============================================================================

(defonce ^:private providers (atom {}))

(defn register-provider!
  "Register a dynamic template provider under `id` (idempotent —
   re-registering replaces). `f` is a zero-arg fn returning a seq of
   template maps `{:name :description …}` carrying either `:body`
   (expanded like a file template) or `:expand-fn` `(fn [env args] ->
   string)`. Providers are consulted after file templates."
  [id f]
  (swap! providers assoc id f)
  id)

(defn- provider-templates []
  (vec (mapcat (fn [[id f]]
                 (try (f)
                   (catch Throwable t
                     (tel/log! {:level :warn :id ::provider-failed
                                :data  {:provider id :error (ex-message t)}})
                     nil)))
         @providers)))

(defn templates
  "All available templates: file templates first (they win name
   collisions), then provider-contributed dynamic templates."
  []
  (dedup-by-name (into (file-templates) (provider-templates))))

;; =============================================================================
;; Expansion
;; =============================================================================

(defn parse-invocation
  "Parse `/name args…` into `{:name \"name\" :args \"args…\"}` or nil.
   Pure tokenisation; does not consult the template registry."
  [text]
  (when (string? text)
    (when-let [[_ nm args] (re-matches #"(?s)/(\S+)(?:\s+(.*))?" (str/trim text))]
      {:name nm :args (str/trim (or args ""))})))

(defn- expand-body
  [body args]
  (let [body (str body)]
    (if (str/includes? body "$ARGUMENTS")
      (str/replace body "$ARGUMENTS" args)
      (if (str/blank? args) body (str body "\n\n" args)))))

(defn expand
  "Expand a `/name args…` prompt-template invocation against the
   available templates. Returns `{:name :text :path?}` when a template
   matched and produced non-blank text, else nil. Never throws — a
   failing `:expand-fn` logs and yields nil so the engine falls back to
   normal slash error handling."
  [env text]
  (when-let [{:keys [name args]} (parse-invocation text)]
    (when-let [t (first (filter #(= name (:name %)) (templates)))]
      (let [body (try
                   (if-let [f (:expand-fn t)]
                     (f env args)
                     (expand-body (:body t) args))
                   (catch Throwable ex
                     (tel/log! {:level :warn :id ::expand-failed
                                :data  {:template name :error (ex-message ex)}})
                     nil))]
        (when (and (string? body) (not (str/blank? body)))
          (cond-> {:name name :text body}
            (:path t) (assoc :path (:path t))))))))
