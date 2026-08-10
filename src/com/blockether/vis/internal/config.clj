(ns com.blockether.vis.internal.config
  "Configuration: paths, JVM lifecycle, provider presets, svar-native
   coercion, config file I/O, and the active-provider state every
   channel reads through.

   Two halves:

     - On-disk config under `~/.vis/`: `state.yml` (machine-written), `vis.mdb/`, `vis.log`.
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
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.svar.internal.router :as svar-router]
            [com.blockether.vis.internal.config-spec :as config-spec]
            [com.blockether.vis.internal.credential-command :as cred]
            [com.blockether.vis.internal.registry :as registry]
            [taoensso.telemere :as tel]
            [taoensso.trove :as trove]
            [taoensso.trove.telemere :as trove-telemere]
            [yamlstar.core :as yamlstar])
  (:import (java.io ByteArrayOutputStream FileInputStream FileOutputStream OutputStream)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files OpenOption)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)))

(defn config-dir
  "Vis' per-user config directory, `~/.vis`.

   A FUNCTION, never a top-level `def`: `native-image` initializes this namespace
   at BUILD time, so a `def` reading `user.home` would fold the BUILDING machine's
   home into the binary and every installed copy would read and write that path.
   Everything below that touches the environment follows the same rule."
  ^String []
  (str (System/getProperty "user.home") "/.vis"))

(defn state-path
  "Machine-owned RMW config store `~/.vis/state.yml` (YAML). Vis read-modify-writes
   this exact file — OAuth tokens, TUI-added providers, extension env overrides — so
   it is kept SEPARATE from the hand-written `~/.vis/config.yml` tier: the RMW cycle
   must never fold (and thus clobber) a user's hand-written YAML."
  ^String []
  (str (config-dir) "/state.yml"))

(defn db-path ^String [] (str (config-dir) "/vis.mdb"))

(defn default-db-spec [] {:backend :sqlite :path (db-path)})

(defn ^:private log-path ^String [] (str (config-dir) "/vis.log"))

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

(def ^java.io.PrintStream original-stderr
  "The process's REAL stderr, captured at load — before `init!` / `init-cli!` point
   `System/err` at the log file. `vis-agent python` wires this into the GraalPy
   Context so guest `sys.stderr` reaches the terminal instead of vis.log."
  System/err)

(defn route-svar-logs!
  "Point svar's Trove facade at Telemere so library logs reach `vis.log`
   and the `:db` handler like every vis signal.

   Trove's DEFAULT backend is `taoensso.trove.console`, which writes to
   `*out*`. `init!` / `init-cli!` rebind `*out*` to a BUFFERED
   `io/writer` over the log file, so a console-backed signal sits in
   that buffer and is never flushed - svar logged nothing at all,
   including its SSE stream trace (`-Dsvar.stream.trace=true`, the one
   instrument that says whether a `thinking_delta` reached
   `reasoning-acc`). Idempotent; call before anything talks to a
   provider."
  []
  (trove/set-log-fn! (trove-telemere/get-log-fn)))

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
  (.mkdirs (io/file (config-dir) "logs"))
  (let
    [raw-out
     (FileOutputStream. (log-path) true)

     log-stream
     (java.io.PrintStream. raw-out true)]

    (System/setOut log-stream)
    (System/setErr log-stream))
  (alter-var-root #'*out* (constantly (io/writer (log-path) :append true)))
  (alter-var-root #'*err* (constantly (io/writer (log-path) :append true)))
  ;; svar logs through Trove; its console default would write into the
  ;; buffered `*out*` above and never be flushed. See `route-svar-logs!`.
  (route-svar-logs!)
  (tel/remove-handler! :default/console)
  ;; `main/configure-logging!` may have already installed a `:file`
  ;; handler that points at the same `vis.log` path. Without this
  ;; removal both handlers stay live and every signal is appended
  ;; twice. We own `:file/vis` here
  ;; (rolling, sized, retention-aware); drop the simpler `:file`
  ;; handler so only one writer remains.
  (tel/remove-handler! :file)
  (tel/add-handler! :file/vis
                    (tel/handler:file {:path (log-path)
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
  (.mkdirs (io/file (config-dir) "logs"))
  (let
    [raw-out
     (FileOutputStream. (log-path) true)

     log-stream
     (java.io.PrintStream. raw-out true)]

    (System/setOut log-stream)
    (System/setErr log-stream))
  (alter-var-root #'*out* (constantly (io/writer (log-path) :append true)))
  (alter-var-root #'*err* (constantly (io/writer (log-path) :append true)))
  ;; Mirror `init!`: svar's Trove logs must reach Telemere, not the
  ;; buffered console writer. See `route-svar-logs!`.
  (route-svar-logs!)
  (tel/remove-handler! :default/console)
  ;; Mirror `init!`: `main/configure-logging!` already installed a
  ;; `:file` handler pointing at the same path. Leaving it alive
  ;; doubles every signal. Drop it before our `:file/vis` handler
  ;; takes over as the single writer.
  (tel/remove-handler! :file)
  (tel/add-handler! :file/vis
                    (tel/handler:file {:path (log-path)
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

(def ^:private removed-provider-ids #{:blockether :github-models :github-copilot})

(def ^:private PRESET_ORDER
  "Stable display order in the 'Add Provider' picker. Most-likely-used
   first. Anything not in this vec lands at the end."
  [:openai :anthropic :anthropic-coding-plan :openai-codex :github-copilot-business
   :github-copilot-individual :github-copilot-enterprise :zai :zai-coding-plan :openrouter :ollama
   :lmstudio])

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

          (:is-hidden provider-md)
          (assoc :is-hidden true))))))

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
         (remove :is-hidden)
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
  "Human-readable label for a provider id. Never persisted.

   A REGISTERED provider extension owns its own branding (`Anthropic (API Key)`,
   `LM Studio`, `OpenAI`) and wins. For every other id — anything a caller wrote
   as `providers: - id: …` in `vis.yml` — that id IS the author's chosen
   spelling, so it is echoed VERBATIM.

   Never `str/capitalize` here: it force-uppercases the first letter AND
   lowercases the rest, so an authored `openAI` rendered as `Openai`, `ACME` as
   `Acme`, and `GPT4All` as `Gpt4all`. A provider entry has no `label` key
   (see `config-spec/provider-keys`), so the id is the only casing signal the
   author has — mangling it means the TUI, the gateway `/v1/providers` label,
   and the companion all disagree with the file on disk."
  [pid]
  (or (:label (registered-provider-metadata pid))
      (some-> pid
              name
              not-empty)
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

(def compatibility->api-style
  "The user-facing `compatibility:` knob -> svar's low-level `:api-style`.

   A provider speaks exactly one wire dialect, and that is all a user should
   have to state: Anthropic Messages, or OpenAI-compatible (chat completions by
   default, the Responses API when the endpoint serves only that). `api_style`
   stays as the raw svar escape hatch and wins when both are set."
  {:anthropic :anthropic
   :openai :openai-compatible-chat
   :openai-responses :openai-compatible-responses})

(defn compatibility-api-style
  "`:api-style` implied by a provider's `compatibility` value. nil when absent or
   unknown - config validation rejects unknown values long before this."
  [compatibility]
  (when compatibility
    (get compatibility->api-style (keyword (str/replace (name compatibility) "_" "-")))))

(defn provider-api-style
  "Effective `:api-style` for a provider map: explicit `api_style` first (raw
   svar value), then the `compatibility` alias, then catalog/preset metadata."
  ([provider] (provider-api-style provider (provider-template (:id provider))))
  ([provider template]
   (or (:api-style provider)
       (compatibility-api-style (:compatibility provider))
       (:api-style template))))

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
         (assoc :tool-call? (:tool-call? m))

         ;; Per-model `:api-style` override. svar reads `(or (:api-style
         ;; model-map) (:api-style provider))` at request build, so a single
         ;; provider can serve models on DIFFERENT wires (e.g. one endpoint that
         ;; routes some models to /chat/completions and others to /messages).
         ;; Without this carry-through the override is silently dropped.
         (some? (:api-style m))
         (assoc :api-style (:api-style m)))))))

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

(defonce
  ^{:doc
    "Argv of the `api_key_command` a built router last resolved for a provider,
  keyed by provider id. `->svar-provider` records it (and drops it again when the
  provider stops being command-backed) so the RUNTIME can re-run the same helper
  on a 401: the durable provider map is not in hand at the request boundary, and
  the token itself lives only in the credential cache."}
  router-credential-argv
  (atom {}))

(defn command-backed?
  "True when provider `pid`'s live router credential came from an
   `api_key_command`. This is what makes an auth rejection RECOVERABLE for such a
   provider: a short-lived SSO token that expired mid-session can be re-minted by
   asking the helper again, with no OAuth hook involved."
  [pid]
  (contains? @router-credential-argv pid))

(defn command-token
  "Provider `pid`'s CURRENT command-backed token, served from the credential
   cache — the helper re-runs only after `invalidate-credential-command!` or a
   TTL lapse, so this is cheap at request frequency.

   nil when `pid` is not command-backed or its helper is currently failing, so a
   caller keeps whatever credential it already holds and normal error handling
   stays authoritative."
  [pid]
  (when-let [argv (get @router-credential-argv pid)]
    (:token (cred/resolve! pid argv))))

(defn- forget-credential-argv!
  "Stop treating `pid` as command-backed: it now resolves its credential some
   other way (a literal key, an OAuth hook, or not at all). Without this a
   removed `api_key_command` would keep overriding the new credential."
  [pid]
  (swap! router-credential-argv dissoc pid)
  nil)

(defn provider-credential-message
  "The message shown when a provider's `api_key_command` cannot produce a token.

   Mirrors `provider-env-message`'s shape — `can't use <provider>: <reason>` — so
   every channel renders both credential gaps identically. `reason` is the
   executor's own non-secret verdict (missing program, exit code, timeout, blank
   output); the helper's STDOUT is the credential and never reaches here."
  [provider-id reason]
  (str "can't use " (if (keyword? provider-id) (name provider-id) (str provider-id)) ": " reason))

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

     ;; A literal `:api-key` always wins. Otherwise a configured
     ;; `:api-key-command` is exec'd (single-flight, cached, no shell) and its
     ;; trimmed stdout becomes the token — the short-lived-SSO credential path.
     ;; It is baked into the router like any other token but NEVER travels back
     ;; onto the provider map, so no write path can persist it.
     api-key
     (if-let [literal (:api-key provider)]
       (do (forget-credential-argv! pid) literal)
       (if-let [argv (:api-key-command provider)]
         (let [{:keys [token error]} (cred/resolve! pid argv)]
           (when error
             (throw (ex-info (provider-credential-message pid error)
                             {:type :vis/credential-command-failed :provider pid})))
           ;; Remember the ARGV (never the token): a mid-turn 401 must be able to
           ;; ask THIS helper again at the next request boundary, exactly as an
           ;; OAuth provider force-refreshes its token.
           (swap! router-credential-argv assoc pid argv)
           (swap! router-baked-tokens assoc pid token)
           token)
         (forget-credential-argv! pid)))

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
     (provider-api-style provider template)

     explicit-headers
     (:llm-headers provider)

     explicit-responses
     (:responses-path provider)

     ;; `is_stateless: true` — this endpoint cannot resolve item ids minted by
     ;; another backend (LiteLLM/Azure multi-resource), so svar must not replay
     ;; server-minted Responses item ids to it.
     explicit-stateless
     (:stateless-items? provider)

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

          (some? explicit-stateless)
          (assoc :stateless-items? (boolean explicit-stateless))

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

        (some? explicit-stateless)
        (assoc :stateless-items? (boolean explicit-stateless))

        explicit-headers
        (assoc :llm-headers explicit-headers)

        merged-extra-body
        (assoc :extra-body merged-extra-body)))))

;;; ── Config I/O ──────────────────────────────────────────────────────────

(def ^:private verbatim-key-subtrees
  "String-keyed subtrees owned by users or wire protocols. Their keys stay exact."
  #{"environment" "env" "headers" "llm_headers" "extra_body" "toggles" "pricing" "context_limits"})

(def ^:private keyword-valued-keys
  "Known scalar fields whose internal runtime representation is a keyword."
  #{"id" "backend" "api_style" "compatibility"})

(def svar-wire->runtime
  "svar owns these ?-suffixed keyword contracts (`:tool-call?`, `:check-context?`,
   `:respect-retry-after?`, `:fallback-provider?`); every wire surface that feeds
   svar — vis.yml and a `vis.provider(...)` extension alike — spells them `is_*`,
   because a wire key carries no `?`. They are the ONE place the mechanical
   `wire/engine-key` mirror does not apply, so they map through this table
   EXPLICITLY, at whatever seam decodes them. A foreign contract earns a named
   table; it never earns a convention that rewrites every other key with it."
  {"is_tool_call" :tool-call?
   "is_check_context" :check-context?
   "is_respect_retry_after" :respect-retry-after?
   "is_fallback_provider" :fallback-provider?
   "is_stateless" :stateless-items?})

(def ^:private runtime->svar-yaml
  "Write-path inverse of `svar-wire->runtime`."
  {:tool-call? "is_tool_call"
   :check-context? "is_check_context"
   :respect-retry-after? "is_respect_retry_after"
   :fallback-provider? "is_fallback_provider"
   :stateless-items? "is_stateless"})

(def ^:private runtime-keywords
  "Finite YAML key vocabulary used by internal keyword-keyed domain maps.
   Unknown/user-owned keys remain strings; no YAML key is passed to `keyword`."
  (merge (into {}
               (map (juxt (comp #(str/replace % "-" "_") name) identity))
               #{:providers :default-provider :default-model :fallback-provider :fallback-model
                 :router :system-prompt :workspace :enabled :filesystem :jail :network :environment
                 :db-spec :grep :toggles :tui-settings :mcp :name :context :output-limit :id
                 :api-key :api-key-command :models :base-url :api-style :compatibility
                 :responses-path :llm-headers :extra-body :rate-limit :budget :tokens
                 :same-provider-delays-ms :fallback-after-ms :timeout-ms :ttft-timeout-ms
                 :idle-timeout-ms :semantic-timeout-ms :max-retries :initial-delay-ms :max-delay-ms
                 :multiplier :max-tokens :max-cost :pricing :context-limits :output-reserve
                 :failure-threshold :recovery-ms :transient-status-codes :window-ms :cooldown-ms
                 :max-wait-ms :allow-read-write :allow-read :allow-write :deny-read :deny-write
                 :path :access :description :inbound-ports :deny-exec :allowed-domains
                 :denied-domains :exclude-domains :allow-private :rules :host :methods :allow
                 :method :text :is-replace :include-gitignored-paths :always-exclude :backend
                 :theme-name :contributors-disabled :servers :transport :command :args :cwd :env
                 :url :headers :python :resource-cache :source-paths :titling :mode :provider})
         svar-wire->runtime))

(defn runtime-config
  "Adapt an already-validated string-keyed YAML map to Vis' internal domain maps.
   Only the finite keys in `runtime-keywords` become keywords. User-defined map keys
   remain strings, and parsing/validation never uses this adapter."
  [v]
  (cond (map? v) (into {}
                       (map (fn [[k val]]
                              (let [runtime-key (get runtime-keywords k k)]
                                [runtime-key
                                 (cond (contains? verbatim-key-subtrees k) val
                                       (and (string? val) (contains? keyword-valued-keys k))
                                       (keyword val)
                                       :else (runtime-config val))])))
                       v)
        (sequential? v) (mapv runtime-config v)
        :else v))

;; =============================================================================
;; `${NAME}` environment interpolation
;;
;; A config file must never have to CARRY a secret. `api_key: ${OPENAI_API_KEY}`
;; resolves from the process environment when config is read, in every tier
;; (`vis.yml`, `.vis/config.yml`, `~/.vis/config.yml`, `~/.vis/state.yml`).
;;
;; An UNSET var is deliberately NOT a load failure. Vis is a long-lived gateway
;; whose config is re-read live and on `/reload`, so aborting the load would let
;; ONE unused provider's missing var kill a session running happily on a healthy
;; provider — the same class of collateral damage `read-yaml-config-map-lenient`
;; exists to prevent. The reference is left VERBATIM instead, which makes the
;; result self-describing: after interpolation, anything still matching
;; `env-ref-pattern` IS, by construction, an unresolved reference. That is what
;; `unresolved-env-refs` reads back for the provider verdict
;; (`providers/provider-status`), for `vis-agent doctor`, and for the hard error raised
;; the moment a user EXPLICITLY selects such a provider
;; (`providers/save-default-selection!`). Fail at the point of intent, never
;; globally.
;; =============================================================================

(def ^:private env-ref-pattern
  "`${NAME}` — the ONE supported spelling. Bare `$NAME` is deliberately NOT
   recognised: it cannot be told apart from a value that legitimately starts
   with `$`, and two spellings for one feature is exactly the ambiguity the
   snake_case-only key contract exists to rule out."
  #"\$\{([A-Za-z_][A-Za-z0-9_]*)\}")

(defn env-refs
  "Distinct env var names referenced as `${NAME}` inside string `s`, in order.
   nil for a non-string."
  [s]
  (when (string? s) (into [] (comp (map second) (distinct)) (re-seq env-ref-pattern s))))

(defonce ^:private env-ref-values
  ;; resolved-value -> the `${NAME}` spelling that produced it, recorded ONLY for
  ;; WHOLE-value references (`api_key: ${K}`, never `url: https://${H}/v1`) —
  ;; the only shape a write can unambiguously map back. Read by
  ;; `restore-env-refs`; see its docstring for why the inverse must exist.
  (atom {}))

(defn- interpolate-env-string
  [^String s]
  (if-let [whole (re-matches env-ref-pattern s)]
    (if-let [v (System/getenv (second whole))]
      (do (swap! env-ref-values assoc v s) v)
      s)
    (str/replace s
                 env-ref-pattern
                 (fn [[all var-name]]
                   (or (System/getenv var-name) all)))))

(defn interpolate-env
  "Replace every `${NAME}` inside every STRING of `v` with `(System/getenv NAME)`.
   Map KEYS are left alone — the contract's key set is finite and snake_case, so
   a `${}` key is a typo, not a feature. An unset var is left verbatim."
  [v]
  (cond (string? v) (interpolate-env-string v)
        (map? v) (into (empty v)
                       (map (fn [[k val]]
                              [k (interpolate-env val)]))
                       v)
        (sequential? v) (mapv interpolate-env v)
        :else v))

(defn restore-env-refs
  "Inverse of `interpolate-env`, applied on the WRITE path: any string equal to a
   value this process resolved from a WHOLE-value `${NAME}` reference is written
   back as `${NAME}`.

   Without this the feature would silently DE-reference itself. Every
   read-modify-write into `~/.vis/state.yml` — a theme flip, a toggle listener, a
   provider edit — re-serializes maps that may have travelled through
   `load-config`, and would bake the PLAINTEXT secret onto disk. The entire point
   of `${NAME}` is that the secret never lands in a file, so the guard belongs at
   the single write boundary (`save-config!`) rather than in every caller."
  [v]
  (let [by-value @env-ref-values]
    (if (empty? by-value)
      v
      (letfn [(walk [x]
                (cond (string? x) (get by-value x x)
                      (map? x) (into (empty x)
                                     (map (fn [[k val]]
                                            [k (walk val)]))
                                     x)
                      (sequential? x) (mapv walk x)
                      :else x))]
        (walk v)))))

(defn unresolved-env-refs
  "Env var names still spelled `${NAME}` anywhere in `v` AFTER interpolation —
   i.e. exactly the referenced vars this process has no value for. Sorted and
   distinct. Walks strings, map VALUES, and sequences."
  [v]
  (letfn [(walk [acc x]
            (cond (string? x) (into acc (env-refs x))
                  (map? x) (reduce walk acc (vals x))
                  (sequential? x) (reduce walk acc x)
                  :else acc))]
    (vec (sort (walk #{} v)))))

(defn provider-env-gap
  "Sorted vec of env vars ONE provider map still references but that are unset,
   or nil when the provider resolved completely."
  [provider]
  (not-empty (unresolved-env-refs provider)))

(defn provider-env-message
  "The message shown when an env-gapped provider is reached for. Names the
   provider and every unset var and NOTHING else — it carries no config value, so
   a half-resolved secret can never leak through it into a log or a dialog."
  [provider-id env-vars]
  (str "can't use "
       (if (keyword? provider-id) (name provider-id) (str provider-id))
       ": "
       (str/join ", " env-vars)
       (if (next env-vars) " are not set" " is not set")))

(defn provider-credential-gap
  "The ONE non-secret reason `provider` cannot currently produce a credential,
   or nil when it can.

   Two sources, checked in that order:

     - an unresolved `${NAME}` anywhere in the entry (`provider-env-gap`);
     - an `api_key_command` that cannot currently produce a token — missing
       executable, non-zero exit, timeout, or blank stdout.

   Returns `{:reason <human string> :env-vars [...]|nil}`. `:reason` is safe to
   log, render and put in an error: it names the provider, the unset vars or the
   PROGRAM, and never the command's stdout.

   This is the single seam every availability decision reads — provider status,
   `vis-agent doctor`, router-build exclusion and the hard error raised when a
   user explicitly selects the provider — so a command-backed credential behaves
   exactly like an unset `${NAME}` everywhere, by construction."
  [provider]
  (if-let [env-vars (provider-env-gap provider)]
    {:reason (provider-env-message (:id provider) env-vars) :env-vars env-vars}
    (when-let [error (:error (cred/resolve! (:id provider) (:api-key-command provider)))]
      {:reason (provider-credential-message (:id provider) error) :env-vars nil})))

(defn provider-credential-gap-cached
  "`provider-credential-gap` restricted to what is ALREADY known: the env check is
   pure, and a credential command is only consulted through its cache.

   Paint paths (`providers/initial-provider-status`) must never fork a subprocess
   to draw a frame, so an unprobed helper yields nil here and the card renders as
   loading until the real background probe answers."
  [provider]
  (if-let [env-vars (provider-env-gap provider)]
    {:reason (provider-env-message (:id provider) env-vars) :env-vars env-vars}
    (when-let [error (:error (cred/peek-token (:id provider) (:api-key-command provider)))]
      {:reason (provider-credential-message (:id provider) error) :env-vars nil})))

(defn provider-credential-error
  "`provider-credential-gap`'s message, or nil. Convenience for call sites that
   only render the reason."
  [provider]
  (:reason (provider-credential-gap provider)))

(defn invalidate-credential-command!
  "Forget the memoized `api_key_command` token for provider `pid`, so the next
   router build re-execs the helper.

   The 401 recovery path lives in `loop.clj` and must not reach past this ns into
   the credential executor, so the one-line hook belongs here beside the rest of
   the credential seam."
  [pid]
  (cred/invalidate! pid))

(defn provider-env-gaps
  "`provider-id -> reason` for every provider in `config` that cannot currently
   authenticate; an empty map when the whole fleet resolved. Covers unset
   `${NAME}` references AND failing `api_key_command` helpers."
  [config]
  (into (sorted-map)
        (keep (fn [p]
                (when-let [{:keys [reason env-vars]} (provider-credential-gap p)]
                  [(:id p) (or env-vars [reason])])))
        (:providers config)))

(defonce ^:private warned-env-gaps
  ;; Sets of missing vars already warned about. `load-config` runs on the live
  ;; per-turn path, so an unconditional log would repeat the same line every turn.
  (atom #{}))

(defn- warn-unresolved-env-refs!
  [resolved]
  (when-let [env-vars (not-empty (unresolved-env-refs resolved))]
    (when-not (contains? @warned-env-gaps env-vars)
      (swap! warned-env-gaps conj env-vars)
      (tel/log! {:level :warn
                 :id ::config-env-unresolved
                 :data {:env-vars env-vars}
                 :msg (str "config references unset environment variables: "
                           (str/join ", " env-vars)
                           " — left verbatim; providers that need them stay unusable")}))))

(defn- resolve-env-config
  "Interpolate a merged raw config and warn ONCE per missing-var set."
  [raw]
  (doto (interpolate-env raw) warn-unresolved-env-refs!))

(defn- parse-yaml-config-map
  "Parse+normalize one YAML file to its string-keyed representation WITHOUT spec
   validation. nil when absent / malformed / not a map. Shared by the strict
   `read-yaml-config-map` and the lenient machine-store fallback."
  [path]
  (let [f (io/file path)]
    (when (.exists f)
      (let [raw (try (yamlstar/load (slurp f)) (catch Exception _ nil))]
        (when (map? raw)
          (let
            [legacy-filesystem (get raw "filesystem")
             legacy-sandbox (get raw "sandbox")
             legacy-jail (or (get raw "jail") {})
             legacy-jail* (if (map? legacy-jail) legacy-jail {})
             normalized-filesystem (if legacy-filesystem
                                     (-> raw
                                         (dissoc "filesystem")
                                         (assoc "jail" (assoc legacy-jail*
                                                         "filesystem"
                                                         (if (map? (get legacy-jail* "filesystem"))
                                                           (merge (get legacy-jail* "filesystem")
                                                                  legacy-filesystem)
                                                           legacy-filesystem))))
                                     (dissoc raw "filesystem"))
             existing-jail (get normalized-filesystem "jail")
             with-legacy-sandbox
             (if (and (boolean? legacy-sandbox)
                      (or (not (map? existing-jail)) (not (contains? existing-jail "enabled"))))
               (assoc-in normalized-filesystem ["jail" "enabled"] legacy-sandbox)
               normalized-filesystem)
             strip-legacy-sandbox? (boolean? legacy-sandbox)]

            (if strip-legacy-sandbox?
              (dissoc with-legacy-sandbox "sandbox")
              with-legacy-sandbox)))))))

(defn- read-yaml-config-map
  "Parse one YAML file and validate its original string-keyed representation.
   No keys or values are keywordized before clojure.spec sees them. Absent,
   malformed, and non-map documents return nil; invalid maps throw."
  [path]
  (when-let [normalized (parse-yaml-config-map path)]
    (config-spec/assert-config! normalized path)))

(defn- read-yaml-config-map-lenient
  "Like `read-yaml-config-map` but never lets an invalid config FILE crash the
   live per-turn load path. On a `:vis/invalid-config` violation it logs one
   warning and returns the leniently parsed (unvalidated) map instead of
   throwing, so a transient/out-of-sync `vis.yml`, overlay, or machine store
   never kills a running session mid-turn — the next `save!`/`/reload`
   re-validates. Strict validation still guards the WRITE path (`save-config!`)."
  [path]
  (try (read-yaml-config-map path)
       (catch clojure.lang.ExceptionInfo e
         (if (= :vis/invalid-config (:type (ex-data e)))
           (do (tel/log!
                 {:level :warn
                  :id ::config-file-invalid
                  :data {:source path :problems (:problems (ex-data e))}
                  :msg
                  "config file failed the contract; loading leniently so the session survives"})
               (parse-yaml-config-map path))
           (throw e)))))

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
   `config.yml` / `config.yaml` plus `vis.yml` / `vis.yaml` for symmetry with the
   project-root spelling. First existing file wins."
  []
  (mapv (fn [n]
          (str (config-dir) "/" n))
        ["config.yml" "config.yaml" "vis.yml" "vis.yaml"]))

(defn- deep-merge-config
  [& maps]
  (letfn [(merge* [a b]
            (cond (nil? a) b
                  (nil? b) a
                  (and (map? a) (map? b)) (merge-with merge* a b)
                  :else b))]
    (reduce merge* nil maps)))

(defn load-global-config-raw
  "Load the machine-written global store as a config map (or nil): `~/.vis/state.yml`,
   the YAML file Vis read-modify-writes. Machine-owned on purpose — kept out of the
   hand-written YAML merge so the RMW cycle never clobbers user files."
  []
  (read-yaml-config-map-lenient (state-path)))

(defn load-global-yaml-config-raw
  "Load only the hand-written global YAML tier: the first existing of
   `~/.vis/config.yml` / `config.yaml` / `vis.yml` / `vis.yaml`, or nil. This
   hand-written base is deep-merged UNDER the machine-written `~/.vis/state.yml`
   store (`state.yml` wins per key), keeping user-authored config separate from
   the RMW machine file."
  []
  (some read-yaml-config-map-lenient (global-config-yaml-paths)))

(defn load-project-config-raw
  "Load the hidden project overlay tier: the first existing of
   `<invocation-cwd>/.vis/config.yml` / `.vis/config.yaml`, or nil. Skipped when
   the overlay dir resolves to the global `~/.vis` store, so running Vis from
   $HOME never aliases a global file as a project overlay."
  []
  (let [overlay-dir (io/file (System/getProperty "user.dir") ".vis")]
    (when-not (= (.getCanonicalPath overlay-dir) (.getCanonicalPath (io/file (config-dir))))
      (some read-yaml-config-map-lenient (project-config-yaml-paths)))))

(defn load-project-root-config-raw
  "Load the visible project-root tier: the first existing of
   `<invocation-cwd>/vis.yml` / `vis.yaml`, or nil."
  []
  (some read-yaml-config-map-lenient (project-root-yaml-paths)))

(defn- config-source-paths
  "Every YAML path that can contribute to `load-config-raw`, existing or not."
  []
  (-> []
      (into (global-config-yaml-paths))
      (conj (state-path))
      (into (project-root-yaml-paths))
      (into (project-config-yaml-paths))))

(def ^:private config-raw-cache
  "`{:stamp … :value …}` memo for `load-config-raw`. Parsing the four YAML tiers
   costs ~60ms; `search-overlay` (and friends) call it on EVERY grep/tool call,
   so it dominated warm search latency. Keyed by an mtime+size stamp of every
   candidate file, so an EDIT to any tier — or `/reload` — is still picked up
   live: 9 `stat`s (~50µs) replace 9 YAML parses."
  (atom nil))

(defn invalidate-config-cache!
  "Drop the `load-config-raw` memo. Called on every config WRITE, because two
   writes inside one filesystem mtime tick could otherwise stamp identically."
  []
  (reset! config-raw-cache nil))

(defn- config-source-stamp
  "mtime+size fingerprint of every config source. Uses NIO's NANOSECOND mtime
   (not `File.lastModified`'s millisecond truncation) so two writes inside one
   millisecond still invalidate; `invalidate-config-cache!` covers our own
   writes regardless."
  []
  (mapv (fn [^String p]
          (let [f (io/file p)]
            (if (.isFile f)
              (let
                [^java.nio.file.attribute.FileTime ft
                 (try (Files/getLastModifiedTime (.toPath f)
                                                 (make-array java.nio.file.LinkOption 0))
                      (catch Throwable _ nil))]
                [p (if ft (.to ft java.util.concurrent.TimeUnit/NANOSECONDS) (.lastModified f))
                 (.length f)])
              [p nil nil])))
        (config-source-paths)))

(defn load-config-raw
  "Load raw config as the deep-merge of four YAML sources — later sources win,
   nested maps merge, scalar/vector values replace:

   1. `~/.vis/config.yml` (or `.yaml` / `vis.yml` / `vis.yaml`) — hand-written
      global base
   2. `~/.vis/state.yml` — machine-written global store (OAuth tokens, TUI-added
      providers); wins over the hand-written base
   3. `<cwd>/vis.yml` (or `vis.yaml`) — visible project root, the committed team
      config
   4. `<cwd>/.vis/config.yml` (or `.yaml`) — hidden project overlay; the NESTED
      overlay wins over the root file (personal beats committed)

   Memoized against the sources' mtime+size (see `config-raw-cache`)."
  []
  (let
    [stamp
     (config-source-stamp)

     cached
     @config-raw-cache]

    (if (and cached (= (:stamp cached) stamp))
      (:value cached)
      (let
        [value (deep-merge-config (load-global-yaml-config-raw)
                                  (load-global-config-raw)
                                  (load-project-root-config-raw)
                                  (load-project-config-raw))]
        (reset! config-raw-cache {:stamp stamp :value value})
        value))))

(defn config-problems
  "Model-readable, per-top-level-key reasons the currently merged live config
   fails the contract (`config-spec/explain-problems` over `load-config-raw`),
   or [] when it is valid. Loads leniently so this never throws even while the
   config on disk is broken — it is the diagnostic surfaced as `config_error`."
  []
  (config-spec/explain-problems (load-config-raw)))

(def default-search-always-exclude
  "Default `:grep :always-exclude` patterns (`.gitignore` syntax) guarding
   the subtrees an `:include-gitignored-paths` overlay re-includes:
   machine-generated dirs nobody wants surfaced even inside a rescued vendored
   repo. Setting `:always-exclude` in config REPLACES this list (vectors
   replace on merge, like everywhere else in config)."
  [".git/" "node_modules/" "target/" "build/" "dist/" "__pycache__/" ".venv/" ".gradle/" "vendor/"
   ".next/" "out/"])

(defn search-overlay
  "Return the grep overlay as an internal keyword-keyed map, or nil when unset.
   The source configuration remains string-keyed and spec-validated."
  []
  (let
    [grep
     (get (load-config-raw) "grep")

     include-gitignored-paths
     (get grep "include_gitignored_paths")

     always-exclude
     (get grep "always_exclude")]

    (when (or (seq include-gitignored-paths) (seq always-exclude))
      {:include-gitignored-paths (mapv str include-gitignored-paths)
       :always-exclude
       (mapv str (if (some? always-exclude) always-exclude default-search-always-exclude))})))

(defn- apply-provider-metadata
  "Attach catalog metadata and the provider's complete preset model catalog.
   Persisted model maps win by name so custom metadata survives, while an old
   narrowed list can no longer hide models supplied by the provider preset."
  [provider]
  (let
    [template
     (provider-template (:id provider))

     models
     (->> (concat (:models provider) (:default-models template))
          (reduce (fn [{:keys [seen models] :as acc} model]
                    (if-let
                      [model-name (some-> (model-name model)
                                          str
                                          str/trim
                                          not-empty)]
                      (if (contains? seen model-name)
                        acc
                        {:seen (conj seen model-name)
                         :models (conj models
                                       (if (map? model)
                                         (assoc model :name model-name)
                                         {:name model-name}))})
                      acc))
                  {:seen #{} :models []})
          :models)]

    (cond-> provider
      (and (nil? (:base-url provider)) (:base-url template))
      (assoc :base-url (:base-url template))

      (and (nil? (:api-style provider)) (provider-api-style provider template))
      (assoc :api-style (provider-api-style provider template))

      (seq models)
      (assoc :models models))))

(defn- apply-config-metadata [config] (update config :providers #(mapv apply-provider-metadata %)))

(defn load-config
  "Load the validated YAML config and adapt its finite schema keys to internal
   keyword-keyed domain maps. `load-config-raw` retains the original string keys.

   This is also the `${NAME}` interpolation boundary. It is done HERE and not in
   `load-config-raw` on purpose: the raw loaders are the read half of every
   read-modify-write into `~/.vis/state.yml`, so resolving there would write the
   plaintext secret straight back to disk. `save-config!` runs
   `restore-env-refs` as the matching guard for values that still reach a write
   through this keywordized view."
  []
  (some-> (load-config-raw)
          ((fn [raw]
             (when (seq (get raw "providers")) raw)))
          resolve-env-config
          runtime-config
          apply-config-metadata))

(defn- active-provider-entry
  [config]
  (let
    [provider-entries
     (or (:providers config) (get config "providers"))

     default-id
     (or (:default-provider config) (get config "default_provider"))

     id-str
     (fn [value]
       (cond (keyword? value) (name value)
             (some? value) (str value)))]

    (or (when default-id
          (some #(when (= (id-str default-id) (id-str (or (:id %) (get % "id")))) %)
                provider-entries))
        (first provider-entries))))

(defn- provider-selection-changed?
  [previous-provider selected-provider]
  (letfn [(provider-id [provider] (or (:id provider) (get provider "id")))]
    (and selected-provider (not= (provider-id previous-provider) (provider-id selected-provider)))))

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
   so files written inside — provider API keys in `state.yml` — are not
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
         (Files/write p
                      (.getBytes content StandardCharsets/UTF_8)
                      ^"[Ljava.nio.file.OpenOption;" (make-array OpenOption 0))
         (catch UnsupportedOperationException _ (spit path content))
         (catch Throwable _
           (spit path content)
           (try (Files/setPosixFilePermissions p (PosixFilePermissions/fromString "rw-------"))
                (catch Throwable _ nil))))))

(defn- ->yaml-safe
  "Convert an internal domain map to the string-keyed YAML contract.
   Existing string keys remain exact; keyword keys/values become plain strings."
  [v]
  (cond (map? v) (into {}
                       (map (fn [[k val]]
                              [(cond (and (keyword? k) (contains? runtime->svar-yaml k))
                                     (runtime->svar-yaml k)
                                     (keyword? k) (str/replace (name k) "-" "_")
                                     :else (str k)) (->yaml-safe val)]))
                       v)
        (sequential? v) (mapv ->yaml-safe v)
        (keyword? v) (name v)
        :else v))

(defn save-config!
  "Persist configuration to `~/.vis/state.yml` using the string-keyed YAML contract.
   Callers may supply internal keyword-keyed domain maps; validation always runs on
   the exact string-keyed map that is written."
  ([config] (save-config! config nil))
  ([config source]
   ;; `restore-env-refs` FIRST: a caller may hand us a map that travelled through
   ;; `load-config`, where `${NAME}` was already resolved. Writing that verbatim
   ;; would bake the secret into `state.yml` and quietly destroy the reference.
   (let [wire-config (restore-env-refs (->yaml-safe config))]
     (config-spec/assert-config! wire-config (state-path))
     (let
       [previous-provider (some-> (active-provider-entry (load-global-config-raw))
                                  runtime-config)
        selected-provider (some-> (active-provider-entry wire-config)
                                  runtime-config)
        runtime-config (runtime-config wire-config)]

       (ensure-private-dir! (config-dir))
       (spit-private! (state-path) (yamlstar/dump wire-config))
       (invalidate-config-cache!)
       (when (provider-selection-changed? previous-provider selected-provider)
         (emit-provider-selected! {:previous-provider previous-provider
                                   :provider selected-provider
                                   :config runtime-config
                                   :source source}))))))

(defn remove-config-provider!
  "Remove every persisted provider entry for `provider-id` from the string-keyed
   machine config, preserving unrelated keys.

   A FALLBACK tag naming that provider goes with it. Unlike `default_provider`,
   which degrades to the fleet's first provider, the fallback root is never
   implicit: a tag left behind names nobody, is invisible to every UI, and
   silently resurrects the moment that provider is authenticated again."
  ([provider-id] (remove-config-provider! provider-id nil))
  ([provider-id source]
   (let
     [raw
      (or (load-global-config-raw) {})

      providers
      (vec (get raw "providers"))

      provider-id'
      (if (keyword? provider-id) (name provider-id) (str provider-id))

      providers*
      (vec (remove #(= provider-id' (get % "id")) providers))

      ;; `fallback_model` may carry the qualified `provider/model` form, which
      ;; WINS over `fallback_provider` on the read path, so both spellings have
      ;; to be consulted before deciding whose tag this is.
      fallback-model
      (some-> (get raw "fallback_model")
              str
              str/trim
              not-empty)

      provider-ids
      (into #{}
            (keep #(some-> (get % "id")
                           str
                           str/trim
                           not-empty))
            providers)

      ;; A slash INSIDE a model id (`z-ai/glm-4.6v`) is not a provider tag: the
      ;; prefix only tags a provider when the config actually has that provider.
      tagged-provider
      (or (when-let
            [prefix (when (str/includes? (str fallback-model) "/")
                      (not-empty (str/trim (first (str/split fallback-model #"/" 2)))))]
            (when (contains? provider-ids prefix) prefix))
          (some-> (get raw "fallback_provider")
                  str
                  str/trim
                  not-empty))

      raw*
      (cond-> raw
        (seq providers*)
        (assoc "providers" providers*)

        (empty? providers*)
        (dissoc "providers")

        (= provider-id' tagged-provider)
        (dissoc "fallback_provider" "fallback_model"))]

     (when (not= raw raw*) (save-config! raw* source) true))))

(defn resolve-config
  "Resolve provider config: explicit -> merged YAML config.
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
  "True on a genuine FIRST run: no provider configured AND no global machine store
   (`~/.vis/state.yml`) has ever been written. Distinguishes the full welcome
   (brand-new user) from a returning user who merely has no provider right now
   (e.g. removed their only one)."
  []
  (and (not (provider-configured?)) (not (.exists (io/file (state-path))))))

(def ^:private router-opts-keys
  "Keys forwarded from Vis config `:router` block into `svar/make-router`'s
   opts map. Anything else is silently dropped so unknown keys can't crash
   the router build."
  #{:rate-limit :network :budget :tokens :failure-threshold :recovery-ms :transient-status-codes
    :window-ms :cooldown-ms :max-wait-ms})

(defn router-opts
  "Extracts `svar/make-router` opts from a Vis config map.

   Reads the `:router` block from the merged YAML config:

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

(def ^:dynamic *extension-dotenv-path*
  "Project `.env` consulted for extension-declared variables after the process environment.
   It takes precedence over `.env.local`. `:cwd` (the default) resolves to
   `<process working directory>/.env` when the lookup RUNS — resolving it in this
   `def` would bake the native-image build directory into the binary. Bind a string
   to point elsewhere, or nil to consult no file."
  :cwd)

(def ^:dynamic *extension-dotenv-local-path*
  "Project `.env.local` consulted after `.env` and before an unset result.
   Same `:cwd` / string / nil contract as [[*extension-dotenv-path*]]."
  :cwd)

(defn- dotenv-path
  "Resolve a dotenv var: `:cwd` becomes `<user.dir><suffix>` NOW, anything else is used as is."
  [v ^String suffix]
  (if (= :cwd v) (str (System/getProperty "user.dir") suffix) v))

(def ^:dynamic *extension-getenv*
  "Function used to read process environment variables. Bind in tests."
  System/getenv)

(defn- dotenv-value-text
  [value]
  (let [value (str/trim value)]
    (if (#{(char 39) (char 34)} (first value))
      (let
        [quote (first value)
         closing-index (.indexOf ^String value (str quote) 1)]

        (if (neg? closing-index) value (subs value 1 closing-index)))
      (some-> (first (str/split value #"\s+#" 2))
              str/trim))))

(defn- dotenv-assignment
  "Return the final assignment for `name` in `path`, including a blank value."
  [path name]
  (when path
    (try (with-open [reader (io/reader path)]
           ;; `.env` follows shell assignment semantics: a later declaration wins.
           ;; Preserve an explicitly blank final assignment so it masks lower-precedence files.
           (some (fn [line]
                   (let
                     [line (-> line
                               (str/replace-first #"^﻿" "")
                               str/trim
                               (str/replace-first #"^export\s+" ""))]
                     (when-let
                       [[_ key value] (re-matches #"([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(.*)" line)]
                       (when (= key name) {:value (dotenv-value-text value)}))))
                 (reverse (vec (line-seq reader)))))
         (catch java.io.FileNotFoundException _ nil)
         (catch java.io.IOException _ nil))))

(defn- dotenv-value
  [name]
  ;; `.env` deliberately takes precedence over `.env.local`; an explicit blank
  ;; assignment in `.env` masks a value from `.env.local`.
  (some-> (some #(dotenv-assignment % name)
                [(dotenv-path *extension-dotenv-path* "/.env")
                 (dotenv-path *extension-dotenv-local-path* "/.env.local")])
          :value
          not-empty))

(defn extension-env-status
  "Return source and value metadata for an extension-declared variable.
   The process environment wins over the working directory's `.env`, then `.env.local`;
   Vis config is deliberately never consulted. `:source` is `:env`, `:dotenv`, or `:unset`."
  [name]
  (let [name' (str name)]
    (if-some [from-env (*extension-getenv* name')]
      (if-let [value (not-empty (str/trim from-env))]
        {:name name' :source :env :value value}
        {:name name' :source :unset :value nil})
      (if-let [from-dotenv (dotenv-value name')]
        {:name name' :source :dotenv :value from-dotenv}
        {:name name' :source :unset :value nil}))))

(defn extension-env-value
  "Resolve an extension-declared variable from the process environment or `.env`.
   Blank/missing values return nil."
  [name]
  (:value (extension-env-status name)))

(defn resolve-db-spec
  "Resolve DB spec: explicit -> JVM property -> environment -> validated YAML -> default."
  ([] (resolve-db-spec nil))
  ([explicit-db-spec]
   (or explicit-db-spec
       (when-let [prop-path (System/getProperty "vis.db.path")]
         {:backend :sqlite :path prop-path})
       (when-let [env-path (System/getenv "VIS_DB_PATH")]
         {:backend :sqlite :path env-path})
       (some-> (get (load-config-raw) "db_spec")
               runtime-config)
       (default-db-spec))))

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
