(ns com.blockether.vis.internal.config
  "Configuration: paths, JVM lifecycle, provider presets, svar-native
   coercion, config file I/O, and the active-provider state every
   channel reads through.

   Two halves:

     - On-disk config under `~/.vis/`: `config.edn`, `vis.mdb/`, `vis.log`.
       `init!` / `init-cli!` / `shutdown!` redirect stdout/stderr into
       the log file and bring up Telemere's file handler.
     - Live process state: the `active-config` atom holds the
       currently-selected provider config; `current-config`,
       `active-provider`, `active-model`, `provider-ids`,
       `has-provider?` are the read API. `reload-config!` re-reads
       from disk.

   The `->svar-provider` helper resolves `:api-key` lazily by calling
   the registered provider's `:provider/get-token-fn`, so the
   token-refresh policy stays inside each provider implementation
   instead of leaking up here."
  (:require [clojure+.error]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.svar.internal.router :as svar-router]
            [com.blockether.vis.internal.registry :as registry]
            [taoensso.telemere :as tel]
            [yamlstar.core :as yamlstar])
  (:import (java.io ByteArrayOutputStream FileInputStream FileOutputStream OutputStream)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files OpenOption)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)))

(def config-dir (str (System/getProperty "user.home") "/.vis"))

(def config-path (str config-dir "/config.edn"))

(defn project-config-path
  "Project-local config override path. `bin/vis` preserves the invocation cwd
   as JVM `user.dir`, so this resolves to `<project>/.vis/config.edn` for the
   user project, not the Vis install checkout."
  []
  (str (System/getProperty "user.dir") "/.vis/config.edn"))

(defn project-root-config-path
  "Project-local `vis.edn` at the invocation cwd root. Same `user.dir`
   basis as `project-config-path`, so this resolves to `<project>/vis.edn`
   for the user project — a visible, root-level alternative to the hidden
   `.vis/config.edn`."
  []
  (str (System/getProperty "user.dir") "/vis.edn"))

(def db-path (str config-dir "/vis.mdb"))

(def default-db-spec {:backend :sqlite :path db-path})

(def ^:private ^String log-path (str config-dir "/vis.log"))

(def tty-in (delay (FileInputStream. "/dev/tty")))

(def ^:private ^"[B" sync-update-begin
  ;; DEC private mode 2026 "synchronized update" — the terminal HOLDS
  ;; rendering from `h` to `l`, so everything between paints as ONE frame.
  ;; Terminals without 2026 support ignore both marks (unknown private
  ;; modes are no-ops), so emitting them unconditionally is safe.
  (.getBytes "\u001b[?2026h" "UTF-8"))

(def ^:private ^"[B" sync-update-end (.getBytes "\u001b[?2026l" "UTF-8"))

(defn- cursor-report-query?
  "Is this chunk Lanterna's CSI 6n cursor-position query? `reportPosition`
   writes it as ONE 4-byte chunk and then BLOCKS (up to 5s) for the
   terminal's reply WITHOUT flushing — the raw unbuffered FileOutputStream
   used to smuggle it out immediately. The frame buffer must flush it
   through on sight or every resize/size-probe stalls to the 5s timeout."
  [^bytes b]
  (and (= 4 (alength b))
       (= (aget b 0) (byte 0x1b))
       (= (aget b 1) (byte 0x5b)) ;; [
       (= (aget b 2) (byte 0x36)) ;; 6
       (= (aget b 3) (byte 0x6e)))) ;; n

(defn frame-buffered-tty-out
  "Wrap the raw tty stream so a whole repaint reaches the terminal as ONE
   atomic write instead of one write(2) syscall PER CELL.

   Lanterna's `refreshByDelta` calls `putString`/`setCursorPosition` per
   changed cell and only `flush`es once at the end of `refresh`. On a raw
   `FileOutputStream` every one of those calls is its own syscall straight
   to the tty, so the terminal renders PARTIAL frames mid-repaint: a fold
   toggle that shifts the transcript reads as a whole-screen flicker and a
   transient content jump. Buffering until `flush` collapses the frame to
   one write, and the DEC 2026 bracket makes the terminal hold rendering
   until the frame is complete even when the kernel chunks the write.

   Everything vis writes to the tty outside Lanterna (SGR-mouse /
   bracketed-paste toggles, OSC 11 background, the `:bell` fx, the panic
   PrintStream) already flushes explicitly, so nothing can sit in the
   buffer across frames."
  ^OutputStream [^OutputStream raw]
  (let
    [initial-capacity
     (* 64 1024)

     ;; `ByteArrayOutputStream/reset` keeps the grown backing array forever,
     ;; so one outsized frame (full repaint on a huge terminal) would pin
     ;; megabytes. Over the retention cap the buffer is REPLACED after the
     ;; flush instead of reset. Mutable holder because the swap needs a new
     ;; instance; all access goes through `lock`.
     retain-capacity
     (* 512 1024)

     lock
     (Object.)

     buf-holder
     (java.util.concurrent.atomic.AtomicReference. (ByteArrayOutputStream. initial-capacity))]

    (proxy [OutputStream] []
      (write
        ([b]
         (if (bytes? b)
           (do (locking lock
                 (.write ^ByteArrayOutputStream (.get buf-holder) ^bytes b 0 (alength ^bytes b)))
               (when (cursor-report-query? b) (.flush ^OutputStream this)))
           (locking lock (.write ^ByteArrayOutputStream (.get buf-holder) (int b)))))
        ([b off len]
         (locking lock
           (.write ^ByteArrayOutputStream (.get buf-holder) ^bytes b (int off) (int len)))))
      (flush []
        (locking lock
          (let
            [^ByteArrayOutputStream buf
             (.get buf-holder)

             n
             (.size buf)]

            (when (pos? n)
              (.write raw sync-update-begin)
              (.writeTo buf raw)
              (.write raw sync-update-end)
              (if (> n retain-capacity)
                (.set buf-holder (ByteArrayOutputStream. initial-capacity))
                (.reset buf)))
            (.flush raw))))
      (close [] (.flush ^OutputStream this) (.close raw)))))

(def tty-out (delay ^OutputStream (frame-buffered-tty-out (FileOutputStream. "/dev/tty"))))

(def ^java.io.PrintStream original-stdout System/out)

(defn init!
  "Redirect System/out and System/err to the log file. Lanterna uses
   tty-in / tty-out for terminal I/O. Call from the TUI entry point."
  []
  (clojure+.error/install!)
  ;; Process-wide print caps via the ROOT binding (not `set!`): under the
  ;; gen-class `-main` / native-image there is no `clojure.main` thread
  ;; binding for these vars, so `set!` throws "Can't change/establish root
  ;; binding". alter-var-root matches the intent (process-wide) and the
  ;; adjacent `*out*`/`*err*` root rebinds.
  (alter-var-root #'*print-level* (constantly 10))
  (alter-var-root #'*print-length* (constantly 100))
  (.mkdirs (io/file config-dir "logs"))
  (let
    [raw-out
     (FileOutputStream. log-path true)

     log-stream
     (java.io.PrintStream. raw-out true)]

    (System/setOut log-stream)
    (System/setErr log-stream))
  (alter-var-root #'*out* (constantly (io/writer log-path :append true)))
  (alter-var-root #'*err* (constantly (io/writer log-path :append true)))
  (tel/remove-handler! :default/console)
  ;; `main/configure-logging!` may have already installed a `:file`
  ;; handler that points at the same `vis.log` path. Without this
  ;; removal both handlers stay live and every signal is appended
  ;; twice. We own `:file/vis` here
  ;; (rolling, sized, retention-aware); drop the simpler `:file`
  ;; handler so only one writer remains.
  (tel/remove-handler! :file)
  (tel/add-handler! :file/vis
                    (tel/handler:file {:path log-path
                                       :interval :monthly
                                       :max-file-size 4000000
                                       :max-num-parts 8
                                       :max-num-intervals 6})
                    {:min-level :info})
  (tel/call-on-shutdown! (fn []
                           (tel/stop-handlers!))))

(defn init-cli!
  "Logging init for non-TUI processes. Same redirects as init! but
   without the shutdown hook (CLI commands run to completion and exit)."
  []
  (clojure+.error/install!)
  ;; Process-wide print caps via the ROOT binding (not `set!`): under the
  ;; gen-class `-main` / native-image there is no `clojure.main` thread
  ;; binding for these vars, so `set!` throws "Can't change/establish root
  ;; binding". alter-var-root matches the intent (process-wide) and the
  ;; adjacent `*out*`/`*err*` root rebinds.
  (alter-var-root #'*print-level* (constantly 10))
  (alter-var-root #'*print-length* (constantly 100))
  (.mkdirs (io/file config-dir "logs"))
  (let
    [raw-out
     (FileOutputStream. log-path true)

     log-stream
     (java.io.PrintStream. raw-out true)]

    (System/setOut log-stream)
    (System/setErr log-stream))
  (alter-var-root #'*out* (constantly (io/writer log-path :append true)))
  (alter-var-root #'*err* (constantly (io/writer log-path :append true)))
  (tel/remove-handler! :default/console)
  ;; Mirror `init!`: `main/configure-logging!` already installed a
  ;; `:file` handler pointing at the same path. Leaving it alive
  ;; doubles every signal. Drop it before our `:file/vis` handler
  ;; takes over as the single writer.
  (tel/remove-handler! :file)
  (tel/add-handler! :file/vis
                    (tel/handler:file {:path log-path
                                       :interval :monthly
                                       :max-file-size 4000000
                                       :max-num-parts 8
                                       :max-num-intervals 6})
                    {:min-level :info}))

(defn shutdown!
  "Flush and stop all telemere handlers. Call after the TUI screen
   stops."
  []
  (tel/stop-handlers!))

;;; ── Provider presets ──────────────────────────────────────────────────────

(def ^:private removed-provider-ids #{:blockether :openrouter :github-models :github-copilot})

(def ^:private PRESET_ORDER
  "Stable display order in the 'Add Provider' picker. Most-likely-used
   first. Anything not in this vec lands at the end."
  [:openai :anthropic :anthropic-coding-plan :openai-codex :github-copilot-business
   :github-copilot-individual :zai :zai-coding-plan :ollama :lmstudio])

(defn- registered-provider-metadata
  "Provider-owned preset metadata. First-party provider extensions put
   labels, base URLs, default models, and transport overrides here so
   internal config stays provider-agnostic."
  [pid]
  (when-let [provider (registry/provider-by-id pid)]
    (merge (:provider/preset provider)
           (when-let [label (:provider/label provider)]
             {:label label}))))

(defn- known-provider-base-url
  "Base URL for a provider id: provider extension first, svar table last."
  [pid]
  (or (:base-url (registered-provider-metadata pid))
      (:base-url (get svar-router/KNOWN_PROVIDERS pid))))

(defn provider-template
  "Preset descriptor for a provider id, merged from a provider
   extension's metadata and svar's catalog. Returns nil for unknown or
   intentionally removed ids."
  [pid]
  (when-not (contains? removed-provider-ids pid)
    (let
      [provider-md
       (registered-provider-metadata pid)

       svar-md
       (get svar-router/KNOWN_PROVIDERS pid)]

      (when (or provider-md svar-md (registry/provider-by-id pid))
        (cond-> {:id pid}
          (:label provider-md)
          (assoc :label (:label provider-md))

          (known-provider-base-url pid)
          (assoc :base-url (known-provider-base-url pid))

          (or (:api-style provider-md) (:api-style svar-md))
          (assoc :api-style (or (:api-style provider-md) (:api-style svar-md)))

          (:default-models provider-md)
          (assoc :default-models (:default-models provider-md))

          (:extra-body provider-md)
          (assoc :extra-body (:extra-body provider-md))

          (:hidden? provider-md)
          (assoc :hidden? true))))))

(defn provider-presets
  "All known provider presets, sorted for the 'Add Provider' picker."
  []
  (let
    [order-rank
     (zipmap PRESET_ORDER (range))

     ids
     (into #{}
           (concat (keys svar-router/KNOWN_PROVIDERS)
                   (map :provider/id (registry/registered-providers))))]

    (->> ids
         (remove removed-provider-ids)
         (keep provider-template)
         (remove :hidden?)
         ;; Drop presets with no human label. A label is only set when a vis
         ;; provider extension is registered for the id; svar `KNOWN_PROVIDERS`
         ;; keys with no matching extension (e.g. :github-copilot-enterprise,
         ;; :zai-coding) would otherwise render as blank, selectable rows after
         ;; the last named preset in the "Add Provider" picker — and the TUI has
         ;; no handling for them anyway.
         (remove #(str/blank? (:label %)))
         (sort-by #(or (order-rank (:id %)) Long/MAX_VALUE))
         vec)))

(defn display-label
  "Human-readable label for a provider id. Never persisted."
  [pid]
  (or (:label (registered-provider-metadata pid))
      (some-> pid
              name
              str/capitalize)
      "Provider"))

(defn- trim-trailing-slashes [s] (str/replace (or s "") #"/+$" ""))

(defn- catalog-base-url?
  "True when `url` is just Vis/svar catalog metadata for `provider-id`,
   not a caller-owned custom endpoint. OAuth providers may receive a
   fresher LLM endpoint from token exchange (for Copilot, the proxy host),
   and catalog defaults must not pin traffic to the stale bootstrap host."
  [provider-id url]
  (= (some-> url
             trim-trailing-slashes)
     (some-> (known-provider-base-url provider-id)
             trim-trailing-slashes)))

(defn- provider-token-base-url
  [provider-id explicit-url api-url]
  (cond (and api-url (or (nil? explicit-url) (catalog-base-url? provider-id explicit-url))) api-url
        explicit-url explicit-url
        :else api-url))

(defn- github-copilot-provider-id?
  [provider-id]
  (contains? #{:github-copilot-individual :github-copilot-business :github-copilot-enterprise}
             provider-id))

(defn provider-model-visible?
  "True when svar's provider-scoped model filters allow this model id."
  [provider-id model-id]
  (let [catalog-id (if (github-copilot-provider-id? provider-id) :github-copilot provider-id)]
    (if-let [visible? (ns-resolve 'com.blockether.svar.internal.router 'provider-model-visible?)]
      (boolean (visible? catalog-id model-id))
      true)))

(defn provider-base-url
  "Resolve base-url for a provider: explicit field on the provider
   map first (so user-supplied URLs win), then the merged catalog."
  [provider]
  (or (:base-url provider) (known-provider-base-url (:id provider))))

;;; ── Svar-native data helpers ────────────────────────────────────────────

(defn model-name
  "Extract the model name string from a model (string or `{:name str}`)."
  [model]
  (cond (string? model) model
        (map? model) (:name model)
        :else nil))

(defn ->svar-model
  "Coerce a model representation to svar-native `{:name str}`."
  ([model] (->svar-model nil model))
  ([_provider-id model]
   (when-let
     [n (some-> (model-name model)
                str
                str/trim
                not-empty)]
     (let [m (when (map? model) model)]
       (cond-> {:name n}
         ;; Carry through model metadata svar honors but vis historically
         ;; dropped. `:context` is the override knob for providers whose API
         ;; can't report a window (LM Studio's OpenAI-compatible /v1/models);
         ;; it's persisted into config from `svar/models!`'s native-endpoint
         ;; detection so the router uses the real window instead of svar's
         ;; conservative DEFAULT_CONTEXT_LIMIT. No value → svar falls back.
         (:context m)
         (assoc :context (:context m))

         (:output-limit m)
         (assoc :output-limit (:output-limit m))

         (some? (:tool-call? m))
         (assoc :tool-call? (:tool-call? m)))))))

(def ^:private boot-token-timeout-ms
  "Upper bound on a synchronous boot-time token fetch (OAuth `get-token-fn`).
   `->svar-provider` resolves a token per keyless provider WHILE the router
   builds, and the router builds on the startup path — so a hung/slow token
   endpoint would stall the first frame indefinitely. Bounding it lets the
   provider be skipped (→ onboarding) instead of blocking startup."
  15000)

(defn- with-boot-token-timeout
  "Run token-resolving `thunk` on a worker bounded by `boot-token-timeout-ms`.
   On timeout, cancel and throw so the router build skips this provider (a
   timeout is transport-shaped, never auth-shaped, so it never triggers the
   refresh-before-drop path) rather than hanging first paint."
  [pid thunk]
  (let
    [fut
     (future (thunk))

     v
     (deref fut boot-token-timeout-ms ::timeout)]

    (if (= v ::timeout)
      (do (future-cancel fut)
          (throw (ex-info (str "Provider "
                               (some-> pid
                                       name)
                               " token fetch timed out after "
                               boot-token-timeout-ms
                               "ms")
                          {:type :vis/token-timeout :provider pid})))
      v)))


(defonce
  ^{:doc
    "Access token last baked into a built router, keyed by provider id.
  `->svar-provider` records the token it resolved via `:provider/get-token-fn`
  here at router-build time. On a 401 the runtime reads it back as the REJECTED
  token — the exact token the failing request sent — so single-flight refresh
  reuse won't hand that same dead token straight back (see loop.clj OAuth 401
  recovery). Correct across multi-tab/multi-process rotation, where the current
  ON-FILE token may already be a peer's fresh one."}
  router-baked-tokens
  (atom {}))

(defn baked-token
  "The access token `->svar-provider` last baked into a router for provider `pid`
   (nil if none). This is the token the live router's requests actually send, so
   it's the correct REJECTED token on a 401 — unlike the current on-file token,
   which a peer tab/process may already have rotated to something fresh."
  [pid]
  (get @router-baked-tokens pid))

(defn ->svar-provider
  "Coerce a provider map to svar-native shape (`:id`, `:api-key`,
   `:base-url`, `:api-style`, `:models`, optional `:responses-path`,
   optional `:llm-headers`).

   svar's `make-router` calls `normalize-provider` which auto-resolves
   `:base-url` from svar's `KNOWN_PROVIDERS` table for built-in
   providers, so we forward `:base-url` ONLY when the provider map
   has one explicitly (vis-only providers like `:github-models`,
   user overrides, or OAuth-supplied URLs). For known providers
   svar fills in the URL itself - stop fighting it.

   When `:api-key` is nil, look the provider up in the global
   provider registry (registry.clj) and call its
   `:provider/get-token-fn` to resolve a usable token. Each provider
   implementation handles its own auth lifecycle (OAuth refresh,
   env-var fallback, provider-specific headers, ...) so this fn stays
   provider-agnostic and never references a concrete provider ns by
   name."
  [provider]
  (let
    [pid
     (:id provider)

     template
     (provider-template pid)

     api-key
     (:api-key provider)

     ;; Local no-auth presets (ollama, lmstudio) ship a dummy api-key in
     ;; svar's catalog; svar's `models!` sends it as an HTTP header, and a
     ;; nil value throws (null HTTP header value) — the reason local model
     ;; catalogs come back empty. Forward the catalog key when the caller
     ;; configured none. Cloud presets have no catalog key, so unaffected.
     catalog-api-key
     (:api-key (get svar-router/KNOWN_PROVIDERS pid))

     models
     (->> (:models provider)
          (keep #(->svar-model pid %))
          vec)

     explicit-url
     (:base-url provider)

     explicit-api-style
     (or (:api-style provider) (:api-style template))

     explicit-headers
     (:llm-headers provider)

     explicit-responses
     (:responses-path provider)

     ;; Provider-default request-body params (e.g. LM Studio sampler
     ;; defaults from the preset). svar merges these as the lowest
     ;; precedence layer, so an explicit per-provider config override
     ;; and any per-turn :extra-body still win.
     merged-extra-body
     (not-empty (merge (:extra-body template) (:extra-body provider)))

     get-token-fn
     (when (nil? api-key)
       (some-> (registry/provider-by-id pid)
               :provider/get-token-fn))]

    (if get-token-fn
      (let
        [{:keys [token api-url llm-headers responses-path]}
         (with-boot-token-timeout pid get-token-fn)

         url
         (provider-token-base-url pid explicit-url api-url)

         merged-headers
         (or explicit-headers llm-headers)

         merged-response
         (or explicit-responses responses-path)]

        ;; Remember the token this router bakes in, so a later 401 can hand the
        ;; single-flight refresh the EXACT token that failed as `rejected`.
        (swap! router-baked-tokens assoc pid token)
        (cond-> {:id pid :models models :api-key token}
          url
          (assoc :base-url url)

          explicit-api-style
          (assoc :api-style explicit-api-style)

          merged-response
          (assoc :responses-path merged-response)

          merged-headers
          (assoc :llm-headers merged-headers)

          merged-extra-body
          (assoc :extra-body merged-extra-body)))
      (cond-> {:id pid :models models}
        (or api-key catalog-api-key)
        (assoc :api-key (or api-key catalog-api-key))

        explicit-url
        (assoc :base-url explicit-url)

        explicit-api-style
        (assoc :api-style explicit-api-style)

        explicit-responses
        (assoc :responses-path explicit-responses)

        explicit-headers
        (assoc :llm-headers explicit-headers)

        merged-extra-body
        (assoc :extra-body merged-extra-body)))))

;;; ── Config I/O ──────────────────────────────────────────────────────────

(defn- read-config-map
  [path]
  (let [f (io/file path)]
    (when (.exists f)
      (try (let [raw (edn/read-string (slurp f))]
             (when (map? raw) raw))
           (catch Exception _ nil)))))

(def ^:private verbatim-key-subtrees
  "Config subtrees whose MAP KEYS are user-owned strings — env var names,
   HTTP header names, provider wire fields. YAML keywordization must never
   touch them: `ANTHROPIC_API_KEY` under `:environment` stays a verbatim
   string key, never `:anthropic-api-key`."
  #{:environment :llm-headers :extra-body})

(def ^:private keyword-valued-keys
  "Config keys whose VALUES are keywords in the EDN shape. YAML has no keyword
   literal, so `id: anthropic` arrives as the string \"anthropic\" — coerce it
   to `:anthropic` so a YAML provider block behaves exactly like its EDN twin."
  #{:id :backend :api-style})

(defn- yaml-key->keyword
  "Normalize one YAML map key onto the config's keyword vocabulary: both
   `snake_case` and `kebab-case` spellings land on the SAME kebab keyword
   (`system_prompt` ≡ `system-prompt` ≡ `:system-prompt`)."
  [k]
  (if (string? k) (keyword (str/replace k "_" "-")) k))

(defn- keywordize-yaml
  "Recursively map a YAMLStar-parsed value (string keys — the YAML boundary
   contract) onto the EDN config shape: keys → kebab keywords, keyword-valued
   fields (`keyword-valued-keys`, plus any explicit `\":foo\"` string) coerced
   to keywords, and `verbatim-key-subtrees` kept EXACTLY as parsed — string
   keys, string values, no case-mangling."
  [v]
  (cond (map? v) (into {}
                       (map (fn [[k val]]
                              (let [kw (yaml-key->keyword k)]
                                [kw
                                 (cond (contains? verbatim-key-subtrees kw) val
                                       (and (string? val) (contains? keyword-valued-keys kw))
                                       (keyword (str/replace-first val ":" ""))
                                       :else (keywordize-yaml val))])))
                       v)
        (sequential? v) (mapv keywordize-yaml v)
        (and (string? v) (str/starts-with? v ":")) (keyword (subs v 1))
        :else v))

(defn- read-yaml-config-map
  "Parse one YAML config file into the EDN config shape (kebab keywords,
   verbatim string subtrees), or nil when absent / malformed / not a map —
   the same failure contract as `read-config-map`."
  [path]
  (let [f (io/file path)]
    (when (.exists f)
      (try (let [raw (yamlstar/load (slurp f))]
             (when (map? raw) (keywordize-yaml raw)))
           (catch Exception _ nil)))))

(defn- project-config-yaml-paths
  "YAML candidates for the hidden `.vis/` project overlay tier."
  []
  [(str (System/getProperty "user.dir") "/.vis/config.yml")
   (str (System/getProperty "user.dir") "/.vis/config.yaml")])

(defn- project-root-yaml-paths
  "YAML candidates for the visible project-root tier: `vis.yml` / `vis.yaml`."
  []
  [(str (System/getProperty "user.dir") "/vis.yml")
   (str (System/getProperty "user.dir") "/vis.yaml")])

(defn- global-config-yaml-paths
  "YAML candidates for the hand-written global tier under `~/.vis`:
   `config.yml` / `config.yaml` (the `config.edn` twins) plus `vis.yml` /
   `vis.yaml` for symmetry with the project-root spelling. First existing
   file wins."
  []
  (mapv (fn [n]
          (str config-dir "/" n))
        ["config.yml" "config.yaml" "vis.yml" "vis.yaml"]))

(defn- read-tier-config-map
  "Read ONE project config tier that accepts either format: the EDN file when
   present, else the first existing YAML candidate. Both present → EDN wins and
   the YAML file is IGNORED (logged, never merged): two spellings of the same
   tier must not deep-merge into a config neither file describes."
  [edn-path yaml-paths]
  (let
    [edn-file
     (io/file ^String edn-path)

     yaml-path
     (first (filter (fn [^String p]
                      (.exists (io/file p)))
                    yaml-paths))]

    (when (and (.exists edn-file) yaml-path)
      (tel/log! :warn
                ["config: both" edn-path "and" yaml-path
                 "exist at the same tier; EDN wins, the YAML file is ignored"]))
    (cond (.exists edn-file) (read-config-map edn-path)
          yaml-path (read-yaml-config-map yaml-path)
          :else nil)))

(defn- deep-merge-config
  [& maps]
  (letfn [(merge* [a b]
            (cond (nil? a) b
                  (nil? b) a
                  (and (map? a) (map? b)) (merge-with merge* a b)
                  :else b))]
    (reduce merge* nil maps)))

(defn load-global-config-raw
  "Load only the machine-written global `~/.vis/config.edn` map (or nil on
   read/parse error). EDN ONLY on purpose: Vis itself read-modify-writes this
   exact file (`save-config!`, provider add/remove, OAuth token persistence),
   so the RMW cycle must never fold hand-written YAML into the file it spits
   back out."
  []
  (read-config-map config-path))

(defn load-global-yaml-config-raw
  "Load only the hand-written global YAML tier: the first existing of
   `~/.vis/config.yml` / `config.yaml` / `vis.yml` / `vis.yaml`, or nil.
   Unlike the project tiers, this file COEXISTS with `~/.vis/config.edn`
   instead of being shadowed by it: the EDN file there is machine-written
   (OAuth tokens, TUI-added providers), the YAML file is user-written —
   different authors, not two spellings of one file — so `load-config-raw`
   deep-merges them with the EDN file winning per key."
  []
  (some read-yaml-config-map (global-config-yaml-paths)))

(defn load-project-config-raw
  "Load only the hidden project overlay tier: `<invocation-cwd>/.vis/config.edn`,
   or its YAML twin `.vis/config.yml` / `.yaml` when no EDN file exists (EDN
   wins when both exist; nil on read/parse error)."
  []
  (let
    [global-file
     (io/file config-path)

     project-file
     (io/file (project-config-path))]

    (when-not (= (.getCanonicalPath global-file) (.getCanonicalPath project-file))
      (read-tier-config-map (.getPath project-file) (project-config-yaml-paths)))))

(defn load-project-root-config-raw
  "Load only the visible project-root tier: `<invocation-cwd>/vis.edn`, or its
   YAML twin `vis.yml` / `vis.yaml` when no EDN file exists (EDN wins when both
   exist; nil on read/parse error)."
  []
  (read-tier-config-map (project-root-config-path) (project-root-yaml-paths)))

(defn load-config-raw
  "Load raw config as the deep-merge of four sources — later sources win,
   nested maps merge, scalar/vector values replace:

   1. `~/.vis/config.yml` (or `.yaml` / `vis.yml` / `vis.yaml`) — hand-written
      global YAML base
   2. `~/.vis/config.edn` — machine-written global; wins over its YAML twin
   3. `<cwd>/vis.edn` (or `vis.yml` / `vis.yaml`) — visible project root,
      the committed team config
   4. `<cwd>/.vis/config.edn` (or `.vis/config.yml`) — hidden project overlay;
      the NESTED overlay wins over the root file (personal beats committed)"
  []
  (deep-merge-config (load-global-yaml-config-raw)
                     (load-global-config-raw)
                     (load-project-root-config-raw)
                     (load-project-config-raw)))

(def default-search-always-exclude
  "Default `:search :always-exclude` patterns (`.gitignore` syntax) guarding
   the subtrees an `:include-gitignored-paths` overlay re-includes:
   machine-generated dirs nobody wants surfaced even inside a rescued vendored
   repo. Setting `:always-exclude` in config REPLACES this list (vectors
   replace on merge, like everywhere else in config)."
  [".git/" "node_modules/" "target/" "build/" "dist/" "__pycache__/" ".venv/" ".gradle/" "vendor/"
   ".next/" "out/"])

(defn search-overlay
  "The `:search` overlay from raw config (issue #23), or nil when unset.
   Returns `{:include-gitignored-paths [pattern…] :always-exclude [pattern…]}`
   with patterns in `.gitignore` syntax. `:always-exclude` falls back to
   `default-search-always-exclude` when include patterns are set without an
   explicit exclude list. nil ⇒ no overlay: search behaves exactly as before
   (this is what keeps the feature zero-cost for unconfigured projects)."
  []
  (let [{:keys [include-gitignored-paths always-exclude]} (:search (load-config-raw))]
    (when (or (seq include-gitignored-paths) (seq always-exclude))
      {:include-gitignored-paths (mapv str include-gitignored-paths)
       :always-exclude
       (mapv str (if (some? always-exclude) always-exclude default-search-always-exclude))})))

(defn- apply-provider-metadata
  "Attach catalog metadata needed by the runtime while preserving the
   user's provider map exactly otherwise."
  [provider]
  (let [template (provider-template (:id provider))]
    (cond-> provider
      (and (nil? (:base-url provider)) (:base-url template))
      (assoc :base-url (:base-url template))

      (and (nil? (:api-style provider)) (:api-style template))
      (assoc :api-style (:api-style template)))))

(defn- apply-config-metadata [config] (update config :providers #(mapv apply-provider-metadata %)))

(defn load-config
  "Load provider config in svar-native syntax from `~/.vis/config.edn`."
  []
  (some-> (load-config-raw)
          ((fn [raw]
             (when (seq (:providers raw)) raw)))
          apply-config-metadata))

(defn- active-provider-entry [config] (first (:providers config)))

(defn- provider-selection-changed?
  [previous-provider selected-provider]
  (and selected-provider (not= (:id previous-provider) (:id selected-provider))))

(defn- emit-provider-selected!
  [{:keys [previous-provider provider config source]}]
  (when-let
    [hook (some-> (:id provider)
                  registry/provider-by-id
                  :provider/on-selected-fn)]
    (try (hook
           {:previous-provider previous-provider :provider provider :config config :source source})
         (catch Throwable t
           (tel/log! {:level :warn
                      :id ::provider-on-selected-failed
                      :data {:provider (:id provider)
                             :source source
                             :error (ex-message t)
                             :ex-class (.getName (class t))}
                      :msg (str "Provider on-selected hook for "
                                (:id provider)
                                " threw; selection continues")})))))

(defn- ensure-private-dir!
  "Create `dir` (and parents) if absent, then tighten it to owner-only (700)
   so files written inside — provider API keys in `config.edn` — are not
   readable by other local users on a shared host. Best-effort: silently a
   no-op on a non-POSIX filesystem."
  [^String dir]
  (let [f (io/file dir)]
    (when-not (.exists f) (.mkdirs f))
    (try (Files/setPosixFilePermissions (.toPath f) (PosixFilePermissions/fromString "rwx------"))
         (catch Throwable _ nil))))

(defn- spit-private!
  "Write `content` to `path` as an owner-only (600) file. Creates the file
   with the restrictive mode set ATOMICALLY (create-with-attribute, not
   write-then-chmod) so a secret is never briefly world-readable, falling
   back to plain `spit` on a non-POSIX filesystem."
  [^String path ^String content]
  (let
    [p
     (.toPath (io/file path))

     attr
     (PosixFilePermissions/asFileAttribute (PosixFilePermissions/fromString "rw-------"))]

    (try (Files/deleteIfExists p)
         (Files/createFile p (into-array FileAttribute [attr]))
         (Files/write p (.getBytes content StandardCharsets/UTF_8) (make-array OpenOption 0))
         (catch UnsupportedOperationException _ (spit path content))
         (catch Throwable _
           (spit path content)
           (try (Files/setPosixFilePermissions p (PosixFilePermissions/fromString "rw-------"))
                (catch Throwable _ nil))))))

(defn save-config!
  "Persist provider config to `~/.vis/config.edn`.

   When the first provider (the active provider) changes, the newly
   selected provider's optional `:provider/on-selected-fn` is invoked
   after the file write. Hook failures are logged and never prevent
   config persistence."
  ([config] (save-config! config nil))
  ([config source]
   (let
     [previous-provider
      (active-provider-entry (load-global-config-raw))

      selected-provider
      (active-provider-entry config)]

     (ensure-private-dir! config-dir)
     (spit-private! config-path (pr-str config))
     (when (provider-selection-changed? previous-provider selected-provider)
       (emit-provider-selected! {:previous-provider previous-provider
                                 :provider selected-provider
                                 :config config
                                 :source source})))))

(defn remove-config-provider!
  "Remove every persisted provider entry for `provider-id` from
   `~/.vis/config.edn`, preserving unrelated global config keys. Project-local
   `.vis/config.edn` is an overlay and is not edited by this writer. Returns
   true when the global file changed."
  ([provider-id] (remove-config-provider! provider-id nil))
  ([provider-id source]
   (let
     [raw
      (or (load-global-config-raw) {})

      providers
      (vec (:providers raw))

      providers*
      (vec (remove #(= provider-id (:id %)) providers))]

     (when (not= providers providers*)
       (save-config! (if (seq providers*) (assoc raw :providers providers*) (dissoc raw :providers))
                     source)
       true))))

(defn resolve-config
  "Resolve provider config: explicit -> `~/.vis/config.edn`.
   Throws when nothing is available."
  ([] (resolve-config nil))
  ([explicit-config]
   (or explicit-config
       (load-config)
       (throw (ex-info "No AI provider is configured yet." {:type :vis/no-provider})))))

(defn provider-configured?
  "True when at least one provider is configured (global or project config).
   The single predicate entry points use to branch onboarding vs normal start —
   never trips the `resolve-config` throw."
  []
  (boolean (some-> (load-config)
                   :providers
                   seq)))

(defn first-run?
  "True on a genuine FIRST run: no provider configured AND no global
   `~/.vis/config.edn` has ever been written. Distinguishes the full welcome
   (brand-new user) from a returning user who merely has no provider right now
   (e.g. removed their only one)."
  []
  (and (not (provider-configured?)) (not (.exists (io/file config-path)))))

(def ^:private router-opts-keys
  "Keys forwarded from Vis config `:router` block into `svar/make-router`'s
   opts map. Anything else is silently dropped so unknown keys can't crash
   the router build."
  #{:rate-limit :network :budget :tokens :failure-threshold :recovery-ms :transient-status-codes
    :window-ms :cooldown-ms :max-wait-ms})

(defn router-opts
  "Extracts `svar/make-router` opts from a Vis config map.

   Reads the `:router` block from `~/.vis/config.edn`:

   ```clojure
   {:router
    {:rate-limit {:same-provider-delays-ms [2000 3000 6000]
                  :fallback-after-ms 30000
                  :respect-retry-after? true
                  :fallback-provider? true}
     :network    {:timeout-ms 300000 :idle-timeout-ms 45000}
     :budget     {:max-tokens 1000000 :max-cost 5.0}}}
   ```

   Returns `{}` when no `:router` block is present so svar's built-in
   defaults win. Unknown keys are dropped — only the keys svar's
   `make-router` knows about flow through.

   See `com.blockether.svar.internal.router/make-router` for the
   authoritative key reference."
  [config]
  (let [block (:router config)]
    (if (map? block) (select-keys block router-opts-keys) {})))

(def ^:private extension-env-config-key :environment)

(defn extension-env-overrides
  "Persisted extension environment overrides from `~/.vis/config.edn`
   under `:environment`. Keys are environment variable names as
   strings. Values are strings. This does NOT mutate the process
   environment; extension code should call `extension-env-value` when
   it wants config-over-env resolution."
  []
  (let
    [raw
     (load-config-raw)

     m
     (when (map? raw) (get raw extension-env-config-key))]

    (if (map? m)
      (into {}
            (keep (fn [[k v]]
                    (when (and (string? k) (string? v) (not (str/blank? v))) [k v])))
            m)
      {})))

(defn extension-env-status
  "Return source and value metadata for an extension-declared env var.
   `:source` is one of `:config`, `:env`, or `:unset`."
  [name]
  (let
    [name'
     (str name)

     overrides
     (extension-env-overrides)]

    (if-let [configured (get overrides name')]
      {:name name' :source :config :value configured}
      (if-let [from-env (not-empty (str/trim (or (System/getenv name') "")))]
        {:name name' :source :env :value from-env}
        {:name name' :source :unset :value nil}))))

(defn extension-env-value
  "Resolve an extension-declared env var as config override -> real env.
   Blank/missing values return nil."
  [name]
  (:value (extension-env-status name)))

(defn save-extension-env-var!
  "Persist or clear one extension env override in `~/.vis/config.edn`.
   Blank/nil `value` removes the override, revealing the process env
   value again if one exists. Preserves all other config keys."
  [name value]
  (let
    [name'
     (str name)

     raw
     (or (load-global-config-raw) {})

     value'
     (when (string? value) (not-empty (str/trim value)))

     envs
     (cond-> (or (get raw extension-env-config-key) {})
       value'
       (assoc name' value')

       (not value')
       (dissoc name'))]

    (save-config! (if (seq envs)
                    (assoc raw extension-env-config-key envs)
                    (dissoc raw extension-env-config-key))
                  :environment)
    (extension-env-status name')))

(defn resolve-db-spec
  "Resolve DB spec: explicit -> `vis.db.path` JVM property -> VIS_DB_PATH env ->
   `:db-spec` from config.edn -> default sqlite at `~/.vis/vis.mdb`."
  ([] (resolve-db-spec nil))
  ([explicit-db-spec]
   (or explicit-db-spec
       (when-let [prop-path (System/getProperty "vis.db.path")]
         {:backend :sqlite :path prop-path})
       (when-let [env-path (System/getenv "VIS_DB_PATH")]
         {:backend :sqlite :path env-path})
       (:db-spec (load-config-raw))
       default-db-spec)))

;; =============================================================================
;; Active provider state
;;
;; The active provider config is mirrored from disk into the
;; `active-config` atom for fast reads. Every mutation goes through
;; the iteration loop's `set-provider!` (which writes to disk AND
;; rebuilds the global router AND reseats it on every cached
;; session env so long-lived envs stop talking to the previous
;; model). The router-rebuild and cached-env reseat are owned by
;; the runtime; this namespace owns the on-disk + atom state.
;; =============================================================================

(defonce active-config
  ;; Public atom (no #'private guard) so the iteration loop's
  ;; `set-provider!` can update it directly without going through a
  ;; setter; everyone else reads through `current-config`.
  (atom nil))

(defn current-config
  "Return the current provider config. Loads from disk on first call."
  []
  (or @active-config
      (let [cfg (load-config)]
        (reset! active-config cfg)
        cfg)))

(defn active-provider
  "Return the first (primary) provider from config, or nil."
  []
  (first (:providers (current-config))))

(defn active-model
  "Return the primary model name string, or nil."
  []
  (some-> (active-provider)
          :models
          first
          model-name))

(defn provider-ids
  "Set of configured provider `:id` keywords."
  []
  (into #{} (map :id) (:providers (or (current-config) {:providers []}))))

(defn has-provider? [provider-id] (contains? (provider-ids) provider-id))

(defn reload-config! [] (reset! active-config (load-config)))
