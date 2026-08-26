(ns com.blockether.vis.internal.provider-key-store
  "The STATIC API-KEY provider shape, owned once.

   A vendor that authenticates with a plain key per plan (Alibaba Model Studio,
   Z.ai) needs the same things: a file under `~/.vis`, a per-plan slice inside
   it, one lookup order (TUI/config key, env var, that file), a token envelope
   for the router, a status report that never prints the key, the interactive
   `vis-agent providers auth` flow, a per-plan logout and the extension entry
   map. Only the STRINGS and the plan table differ, so a provider extension
   declares a BOOK and this namespace owns the behaviour:

     {:vendor     \"Alibaba\"                      ; how a message names it
      :file       \"alibaba-auth.json\"            ; lives under ~/.vis
      :key-hint   \"<your-alibaba-api-key>\"       ; the export line's value
      :error-type :vis/alibaba-not-authenticated ; ex-info :type when no key
      :auth-notes [\"  The key is plan-scoped …\"]  ; extra prompt lines, optional
      :plans      {:coding {:provider-id :alibaba-coding-plan
                            :label \"Alibaba (Coding Plan)\"
                            :base-url \"https://…\"
                            :default-models [\"…\"]
                            :env-keys [\"ALIBABA_CODING_PLAN_API_KEY\"]}}}

   The plan TAG (`:coding`) is local to the file and the `:provider-id` is the
   catalog id; the two never merge and no lookup ever falls back to a sibling
   plan, because a key issued for one plan is rejected by the other's endpoint.

   What a provider still owns: its plan table, its `:provider/limits-fn` (a
   quota endpoint is vendor-specific) and its own namespace docstring."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [taoensso.telemere :as tel]))

(defn auth-file
  "Where the book persists its keys. A FUNCTION, never a top-level `def`:
   native-image folds constants at build time, which would bake the builder's
   home directory into the binary."
  ^String [book]
  (str (System/getProperty "user.home") "/.vis/" (:file book)))

(defn- plan-of [book plan-tag] (get (:plans book) plan-tag))

(defn- auth-json-key
  "JSON key -> engine keyword. What we write is snake_case (`api_key`); the
   kebab spelling older builds persisted reads back onto the same key."
  [k]
  (keyword (str/replace (name k) "_" "-")))

(defn load-auth
  "The WHOLE persisted map (every plan) or nil, so one read serves a caller
   asking about any sibling plan. Never throws: an unreadable file reads as
   'no key'."
  [book]
  (let [f (io/file (auth-file book))]
    (when (.exists f)
      (try (json/read-json (slurp f) :key-fn auth-json-key) (catch Exception _ nil)))))

(defn- save-auth!
  "Persist the WHOLE map through the ONE JSON boundary (`wire/json-str`):
   snake_case string keys, total encoding."
  [book auth-state]
  (let [dir (io/file (str (System/getProperty "user.home") "/.vis"))]
    (when-not (.exists dir) (.mkdirs dir))
    (spit (auth-file book) (wire/json-str auth-state))))

(defn update-plan!
  "Merge `slice` into the file under `plan-tag`; a nil `slice` REMOVES that
   plan. An emptied file is deleted so its mere existence keeps reading as
   'authenticated' for `detect-fn` semantics. Returns the new map."
  [book plan-tag slice]
  (let [current
        (or (load-auth book) {})

        next-state
        (if (nil? slice) (dissoc current plan-tag) (assoc current plan-tag slice))]

    (if (seq next-state)
      (save-auth! book next-state)
      (let [f (io/file (auth-file book))]
        (when (.exists f) (.delete f))))
    next-state))

(defn- env-key
  "First non-blank env var from the plan's `:env-keys` priority list, or nil."
  [book plan-tag]
  (some (fn [env-name]
          (let [v (System/getenv env-name)]
            (when-not (str/blank? v) v)))
        (:env-keys (plan-of book plan-tag))))

(defn- configured-key
  "API key from Vis provider config (`~/.vis/config.edn`) for this plan, or
   nil. This covers the TUI flow, which stores static API keys in the normal
   provider config instead of the provider auth file."
  [book plan-tag]
  (let [provider-id (:provider-id (plan-of book plan-tag))]
    (try (when-let [current-config-fn (requiring-resolve 'com.blockether.vis.core/current-config)]
           (some (fn [provider]
                   (when (= provider-id (:id provider))
                     (when-let [k (:api-key provider)]
                       (when-not (str/blank? k) k))))
                 (:providers (current-config-fn))))
         (catch Throwable _ nil))))

(defn- file-key
  [book plan-tag]
  (when-let [from-file (get (load-auth book) plan-tag)]
    (when-let [k (:api-key from-file)]
      (when-not (str/blank? k) k))))

(defn detect-key
  "Lookup priority for one plan:
     1. TUI/config provider `:api-key` for this plan.
     2. The plan's env-var chain.
     3. The book's file slice for this plan.
   Returns `{:api-key str :source kw}` or nil. Never throws.

   `:source` is `:config`, `:env-var` or `:auth-file`, so a status report can
   tell the user WHERE the key came from."
  [book plan-tag]
  (or (when-let [k (configured-key book plan-tag)]
        {:api-key k :source :config})
      (when-let [k (env-key book plan-tag)]
        {:api-key k :source :env-var})
      (when-let [k (file-key book plan-tag)]
        {:api-key k :source :auth-file})))

(defn key-preview
  "Short non-secret preview for status output: these are long opaque tokens,
   so show the first 8 characters and an ellipsis."
  [api-key]
  (let [n (count api-key)]
    (if (<= n 12) (str (subs api-key 0 (min 4 n)) "...") (str (subs api-key 0 8) "..."))))

(defn token-envelope
  "The uniform runtime credential the central router adapter consumes:
   `{:token … :api-url …}`. Throws when no source has a key, so the runtime
   fails fast pointing at `vis-agent providers auth <plan>` instead of a
   confusing upstream 401."
  [book plan-tag]
  (let [{:keys [provider-id base-url env-keys]} (plan-of book plan-tag)]
    (if-let [{:keys [api-key]} (detect-key book plan-tag)]
      {:token api-key :api-url base-url}
      (throw (ex-info (str "No "
                           (:vendor book)
                           " API key for plan "
                           plan-tag
                           ". Run `vis-agent providers auth "
                           (name provider-id)
                           "` to authenticate, or set "
                           (str/join " / " env-keys)
                           ".")
                      {:type (:error-type book) :plan plan-tag :provider-id provider-id})))))

(defn status-report
  "One plan's authentication state, with the key PREVIEWED, never printed."
  [book plan-tag]
  (let [{:keys [provider-id label]}
        (plan-of book plan-tag)

        detected
        (detect-key book plan-tag)]

    (cond-> {:is-authenticated (some? detected) :provider-id provider-id :label label}
      detected
      (assoc :source
        (:source detected) :api-key-preview
        (key-preview (:api-key detected))))))

(defn logout-plan!
  "Clear ONE plan's persisted key; the sibling plan stays intact."
  [book plan-tag]
  (update-plan! book plan-tag nil)
  (tel/log! {:level :info
             :id ::logout
             :data {:vendor (:vendor book) :plan plan-tag}
             :msg (str "Cleared persisted " (:vendor book) " key for plan " plan-tag)})
  :logged-out)

(defn auth-instruction-lines
  "What `vis-agent providers auth <plan>` prints when no key exists anywhere:
   the two ways to provide one, this plan's own env vars, and its endpoint."
  [book plan-tag]
  (let [{:keys [provider-id label env-keys base-url]} (plan-of book plan-tag)]
    (vec (concat ["" (str "  " label " requires a static API key.") ""]
                 (:auth-notes book)
                 ["  Two ways to authenticate:" ""
                  (str "    1. Set the env var, then re-run `vis-agent providers auth "
                       (name provider-id)
                       "`:")]
                 (mapv (fn [env-name]
                         (str "         export " env-name "=" (:key-hint book)))
                       env-keys)
                 ["" "    2. Add the provider through the TUI (Ctrl+K -> Providers)."
                  "       The TUI prompts for the key directly and writes it to the config." ""
                  (str "  Endpoint: " base-url)]))))

(defn auth!
  "The interactive auth flow, invoked by the runtime with ONE `printer-fn`
   argument (an `(fn [line] …)` writing one user-visible line). `read-line` is
   unavailable because the CLI dispatcher captures stdout/stderr to a log file,
   so the shared pattern is: print instructions, and accept the key from the
   env var the user set in the shell that ran the command. A key already in the
   config or on disk is a no-op; a key found only in the env is written through
   so later runs no longer depend on the user's shell."
  [book plan-tag printer-fn]
  (let [print!
        (or printer-fn (constantly nil))

        {:keys [provider-id label env-keys base-url]}
        (plan-of book plan-tag)

        existing
        (detect-key book plan-tag)]

    (cond
      (and existing (contains? #{:config :auth-file} (:source existing)))
      (do (print! (str "  Already authenticated with " label "."))
          (print! (str "  Source: " (name (:source existing)) "."))
          (print! (str "  Run `vis-agent providers status " (name provider-id) "` for details."))
          (print! (str "  Run `vis-agent providers logout "
                       (name provider-id)
                       "` first to switch stored keys."))
          :already-authenticated)
      (and existing (= :env-var (:source existing)))
      (do
        (update-plan!
          book
          plan-tag
          {:api-key (:api-key existing) :saved-at (System/currentTimeMillis) :from :env-var})
        (print!
          (str "  Persisted " (:vendor book) " key from env var (" (str/join " / " env-keys) ")."))
        (print! (str "  " label " is ready (endpoint: " base-url ")."))
        :ok)
      :else (do (doseq [line (auth-instruction-lines book plan-tag)]
                  (print! line))
                :no-credentials))))

(defn provider-entries
  "One `:ext/providers` entry per plan in the book, in plan order.
   `limits-fn` is `(fn [plan-tag] (fn [] report))` because a quota endpoint -
   or the absence of one - is the vendor's own business."
  [book limits-fn]
  (mapv (fn [plan-tag]
          (let [{:keys [provider-id label base-url default-models]} (plan-of book plan-tag)]
            {:provider/id provider-id
             :provider/label label
             :provider/preset {:base-url base-url :default-models default-models}
             :provider/status-fn #(status-report book plan-tag)
             :provider/logout-fn #(logout-plan! book plan-tag)
             :provider/detect-fn #(detect-key book plan-tag)
             :provider/auth-fn #(auth! book plan-tag %)
             :provider/auth-prompt-fn #(auth-instruction-lines book plan-tag)
             :provider/get-token-fn #(token-envelope book plan-tag)
             :provider/limits-fn (limits-fn plan-tag)}))
        (keys (:plans book))))
