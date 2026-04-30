(ns com.blockether.vis.internal.main
  "vis CLI binary — :db Telemere handler, one-shot agent helper,
   built-in CLI commands, and the `-main` dispatcher entry point.

   Everything in this file is binary-only. The library surface
   (iteration loop, query engine, environment lifecycle, conversation
   cache) lives in `com.blockether.vis.internal.loop`; this namespace requires
   that one and wires it into the command tree the `vis` binary
   exposes.

   Public entry point:

     (-main & args)   — invoked by the `:vis` alias / `bin/vis`.
                        Configures logging, runs the unified extension
                        discovery scan, redirects stderr to ~/.vis/vis.log
                        for any TTY-owning channel, then dispatches to
                        the resolved command's `:cmd/run-fn`.

   Built-in commands registered here:
     vis run                — one-shot agent query (CLI agent helper)
     vis auth               — provider authentication
     vis conversations      — list persisted conversations
     vis extensions list    — list registered extensions
     vis channels <name>    — auto-mounted via the channel registry

   `vis doctor` is registered by vis-foundation (extension-owned)
   so every extension can plug its `:ext/doctor-check-fn` into the
   aggregator. See plan §1 Q18 + plans/2026-04-30-..."

  (:refer-clojure :exclude [agent run!])
  (:require
   [charred.api :as json]
   [clojure.string :as str]
   [com.blockether.svar.core :as svar]
   [com.blockether.vis.internal.commandline :as commandline]
   [com.blockether.vis.internal.config :as config]
   [com.blockether.vis.internal.error :as error]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.format :as fmt]
   [com.blockether.vis.internal.loop :as lp]
   [com.blockether.vis.internal.manifest :as manifest]
   [com.blockether.vis.internal.persistance :as persistance]
   [com.blockether.vis.internal.progress :as progress]
   [com.blockether.vis.internal.registry :as registry]
   [taoensso.telemere :as tel]))

;; =============================================================================
;; Persistence-backed Telemere :db handler
;; =============================================================================

;; =============================================================================
;; Signal → log entry
;; =============================================================================

(defn- signal->entry
  "Transform a telemere signal into the entry map accepted by the
   persistence facade's `db-log!`. The facade fills in `:id`/`:created_at`
   and converts ids/keywords through `persistance.base`, so this fn
   only carries the semantic payload."
  [signal]
  (let [ctx   (or (:ctx signal) {})
        level (or (:level signal) :info)
        event (or (some-> (:id signal) str)
                (some-> (:ns signal) str)
                "unknown")
        data  (try
                (json/write-json-str
                  (cond-> {}
                    (:msg_ signal)  (assoc :msg (force (:msg_ signal)))
                    (:data signal)  (assoc :data (:data signal))
                    (:ns signal)    (assoc :ns (str (:ns signal)))
                    (:error signal) (assoc :error (str (:error signal)))))
                (catch Throwable _ nil))]
    (cond-> {:level level
             :event event
             :data  data}
      (:conversation-soul-id ctx) (assoc :conversation-soul-id (:conversation-soul-id ctx))
      (:conversation-turn-id ctx)             (assoc :query-soul-id (:conversation-turn-id ctx))
      (:iteration-id ctx)         (assoc :iteration-id (:iteration-id ctx)))))

;; =============================================================================
;; Handler
;; =============================================================================

(defn handler:db
  "Telemere handler that persists every signal through the
   `com.blockether.vis.core/log!` facade.

   The handler reads `:db-info` from the signal's telemere context
   (`*ctx*`). When `:db-info` is absent (no DB connection active in
   scope), the signal is silently dropped — the console handler still
   prints it.

   Usage:
     (tel/add-handler! :db (handler:db))

     (tel/with-ctx+ {:db-info db-info :conversation-soul-id conversation-id}
       (tel/log! :info \"something happened\"))"
  ([] (handler:db nil))
  ([_opts]
   (fn handler
     ([signal]
      (when-let [db-info (get-in signal [:ctx :db-info])]
        (try
          (persistance/db-log! db-info (signal->entry signal))
          (catch Throwable _ nil))))
     ([] nil))))

(defn setup-db-handler!
  "Install the `:db` Telemere handler. Idempotent — reusing the same
   handler key replaces the previous registration. Call once at
   process startup, after the persistence backend is loaded
   (otherwise the handler will silently drop signals because no
   backend is registered with the facade yet).

   The handler is asynchronous (dropping mode, 2048-entry buffer,
   single drain thread) so a slow DB write never back-pressures the
   call site that emitted the signal."
  []
  (tel/add-handler! :db
    (handler:db)
    {:async     {:mode :dropping :buffer-size 2048 :n-threads 1}
     :min-level :info}))

;; =============================================================================
;; Extension CLI dispatcher
;; =============================================================================

;;; ── Extension introspection ─────────────────────────────────────────────

(def ^:private ^String ext-ns-prefix "com.blockether.vis.ext.")

(defn- short-ext-ns
  "Render an extension namespace symbol with the `v/` prefix instead of
   the canonical `com.blockether.vis.ext.` package, so the table column
   stays narrow:

     com.blockether.vis.ext.foundation.core      → v/foundation.core
     com.blockether.vis.ext.provider-github-copilot → v/provider-github-copilot

   Anything that doesn't start with the canonical prefix is returned
   unchanged."
  [ns-sym]
  (let [s (str ns-sym)]
    (if (str/starts-with? s ext-ns-prefix)
      (str "v/" (subs s (count ext-ns-prefix)))
      s)))

(defn- per-kind-group
  "Per-row \"Group\" cell — a finer label *inside* `:ext/kind`. Pulled
   from the extension's contribution slot that matches its kind:

     - providers   → joined `:provider/label`s
     - channels    → joined `:channel/cmd`s
     - persistance → joined `:persistance/id` names
     - everything else (foundation, languages, uncategorized) → blank

   Joined with `, ` so an extension contributing multiple of one
   surface (e.g. `provider-zai` exporting both Coding-Plan and
   pay-as-you-go) reads as a single comma-separated cell instead of
   a wrapped multi-line column."
  [e]
  (case (:ext/kind e)
    "providers"   (->> (:ext/providers e) (keep :provider/label) (str/join ", "))
    "channels"    (->> (:ext/channels e)  (keep :channel/cmd)    (str/join ", "))
    "persistance" (->> (:ext/persistance e) (keep (comp name :persistance/id)) (str/join ", "))
    ""))

(defn list-extensions
  "Return all registered extensions with their metadata (table rows).

   `:namespace` is shortened with the `v/` prefix (see
   `short-ext-ns`). `:kind` carries the categorical bucket
   (`providers`, `channels`, `foundation`, …) used to render the
   table in grouped sections. `:group` is a finer label *inside* the
   kind (provider label / channel cmd / persistance id), blank for
   kinds that don't have one. `:author`, `:owner`, and `:license` come
   straight from the extension manifest; `:owner` identifies the
   package's distribution (\"vis\" for everything bundled here), and `:license` carries the SPDX
   identifier (e.g. `Apache-2.0`)."
  []
  (mapv (fn [e]
          {:namespace (short-ext-ns (:ext/namespace e))
           :doc       (:ext/doc e)
           :kind      (or (:ext/kind e) "uncategorized")
           :group     (per-kind-group e)
           :author    (or (:ext/author e) "—")
           :owner     (or (:ext/owner e) "—")
           :license   (or (:ext/license e) "—")
           :version   (or (:ext/version e) "—")})
    (extension/registered-extensions)))

(defn find-extension-cmd
  "Find an extension CLI command by name. Returns {:ext ext :cmd cmd-map} or nil."
  [cmd-name]
  (some (fn [e]
          (some (fn [cmd]
                  (when (= (:cmd cmd) cmd-name)
                    {:ext e :cmd cmd}))
            (:ext/cli e)))
    (extension/registered-extensions)))

(defn all-extension-cmds
  "Return a flat vec of {:cmd :doc :ext-ns :args} for every registered extension CLI command."
  []
  (into []
    (mapcat (fn [e]
              (map (fn [c] (assoc c :ext-ns (str (:ext/namespace e))))
                (or (:ext/cli e) []))))
    (extension/registered-extensions)))

;;; ── Arg parsing & validation ───────────────────────────────────────────

(defn- flag-arg? [s] (str/starts-with? (str s) "--"))

(defn- coerce-arg [value type]
  (case (or type :string)
    :string  value
    :int     (if-let [n (parse-long value)] n
               (throw (ex-info (str "Expected integer, got: " value) {:value value})))
    :boolean (contains? #{"true" "1" "yes"} (str/lower-case (str value)))
    :file    value
    value))

(defn parse-ext-args
  "Parse CLI args against an arg spec. Returns a map of {arg-name value}.

   :kind :positional args are matched in declaration order.
   :kind :flag args are matched by --name. Boolean flags need no value."
  [arg-specs raw-args]
  (let [positional (vec (filter #(= :positional (:kind %)) arg-specs))
        flags      (into {} (map (fn [a] [(:name a) a]))
                     (filter #(= :flag (:kind %)) arg-specs))]
    (loop [args     (seq raw-args)
           pos-idx  0
           result   {}]
      (if-not args
        result
        (let [arg  (first args)
              more (next args)]
          (if (flag-arg? arg)
            ;; Flag
            (if-let [spec (get flags arg)]
              (if (= :boolean (:type spec))
                (recur more pos-idx (assoc result (:name spec) true))
                (recur (next more) pos-idx
                  (assoc result (:name spec) (coerce-arg (first more) (:type spec)))))
              (recur more pos-idx result))
            ;; Positional
            (if (< pos-idx (count positional))
              (let [spec (nth positional pos-idx)]
                (recur more (inc pos-idx)
                  (assoc result (:name spec) (coerce-arg arg (:type spec)))))
              (recur more pos-idx result))))))))

(defn validate-ext-args
  "Validate parsed args against spec. Returns nil on success, error string on failure."
  [arg-specs parsed]
  (let [required (filter :required arg-specs)
        missing  (remove #(contains? parsed (:name %)) required)]
    (when (seq missing)
      (str "Missing required argument(s): "
        (str/join ", " (map :name missing))))))

;;; ── Help rendering ─────────────────────────────────────────────────────

(defn- pad [s w]
  (let [s (str s)] (if (>= (count s) w) s (str s (apply str (repeat (- w (count s)) \space))))))

(defn format-cmd-help
  "Build help text for a single extension CLI command."
  [{:keys [cmd doc args ext-ns]}]
  (let [positional (filter #(= :positional (:kind %)) (or args []))
        flags      (filter #(= :flag (:kind %)) (or args []))
        usage-pos  (str/join " "
                     (map (fn [{:keys [name required]}]
                            (if required (str "<" name ">") (str "[" name "]")))
                       positional))
        usage-flags (when (seq flags) "[flags]")
        usage      (str/join " " (remove nil? [usage-pos usage-flags]))
        fmt-arg    (fn [{:keys [name type required doc]}]
                     (str "    " (pad name 20)
                       (pad (or (some-> type clojure.core/name) "string") 10)
                       (if required "required  " "optional  ")
                       (or doc "")))]
    (str "  vis ext " cmd (when (seq usage) (str " " usage))
      "\n\n  " (or doc "")
      (when ext-ns (str "\n  Extension: " ext-ns))
      (when (seq positional)
        (str "\n\n  Positional arguments:\n"
          (str/join "\n" (map fmt-arg positional))))
      (when (seq flags)
        (str "\n\n  Flags:\n"
          (str/join "\n" (map fmt-arg flags)))))))

(defn extension-help []
  (let [cmds (all-extension-cmds)]
    (if (empty? cmds)
      "No extension commands available. Run 'vis extensions' to see registered extensions."
      (str "Extension commands:\n\n"
        (str/join "\n\n"
          (map (fn [{:keys [cmd doc ext-ns]}]
                 (str "  vis ext " (pad cmd 20) (or doc "") "  (" ext-ns ")"))
            cmds))))))

;;; ── Dispatch ───────────────────────────────────────────────────────────

(defn run-extension-cmd!
  "Parse args, validate, and run an extension CLI command.
   Returns {:ok result} or {:error message}."
  [cmd-name raw-args]
  (if-let [{:keys [cmd]} (find-extension-cmd cmd-name)]
    (let [arg-specs (or (:args cmd) [])
          ;; --help on any command
          help?    (some #{"--help" "-h"} raw-args)]
      (if help?
        {:help (format-cmd-help (assoc cmd :ext-ns (:ext-ns cmd)))}
        (let [parsed (parse-ext-args arg-specs raw-args)
              err    (validate-ext-args arg-specs parsed)]
          (if err
            {:error (str err "\n\n" (format-cmd-help cmd))}
            {:ok ((:fn cmd) parsed)}))))
    {:error (str "Unknown command: " cmd-name "\n\n" (extension-help))}))

;; =============================================================================
;; Agent helper (one-shot `vis run`)
;; =============================================================================

;;; ── Agent Definition ─────────────────────────────────────────────────────

(defn agent
  "Create an agent definition (data map).

   Options:
   - :name        — Agent name (string, default \"default\")
   - :description — What the agent does
   - :constants   — Map of {symbol value} constants for SCI sandbox
   - :model       — Override default model selection

   The iteration loop runs until the model emits `:answer` or the
   user cancels. There is no per-agent budget and no iteration cap.

   Example:
     (agent {:name \"code-reviewer\"
             :description \"Reviews Clojure code for quality\"
             :model \"gpt-4o\"})"
  [{:keys [name] :as opts}]
  (let [agent-name (or name "default")]
    (merge {:name      agent-name
            :constants {}}
      opts)))

;;; ── Execution ────────────────────────────────────────────────────────────

(defn run!
  "Execute a one-shot agent query.

   Creates a conversation → runs the query → returns result.

   Returns map with:
   - :conversation-id — Conversation ID (UUID string)
   - :answer       — The agent's response
   - :iteration-count — Number of iterations executed
   - :duration-ms  — Total wall-clock time
   - :tokens       — {:input N :output N :reasoning N :cached N :total N}
   - :cost         — {:input-cost N :output-cost N :total-cost N :model str}
   - :trace        — Full iteration trace
   - :confidence   — :high/:medium/:low (when present)
   - :status — Only on failure (`:error` or `:cancelled`).
   - :error  — Error message (only on failure).

   Options:
   - :spec        — Output spec for structured responses
   - :model       — Override model
   - :on-chunk    — Streaming callback fn
   - :debug?      — Enable debug logging (default false)
   - :config      — Provider config override (skips ~/.vis/config.edn)
   - :no-persist? — Run without writing anything to disk. Spins up an
                    ephemeral environment backed by an in-memory SQLite
                    DB (`:db :memory`), runs the query, disposes the
                    env (which vaporizes the DB). Result has
                    `:conversation-id nil`. Useful for CI, scripting,
                    sensitive prompts.

   Each persistent call creates a fresh conversation in the `:cli`
   channel. Past runs are browsable via
   `(conversations/by-channel :cli)`. Ephemeral (`:no-persist?`) calls
   leave no trace."
  [agent-def prompt & [{:keys [spec model on-chunk
                               debug? config no-persist?]
                        :as _opts}]]
  (let [_cfg      (config/resolve-config config)
        prompt-s  (if (string? prompt) prompt (pr-str prompt))
        title     (let [t (str/trim prompt-s)]
                    (if (> (count t) 100) (str (subs t 0 97) "…") t))
        mdl       (or model (:model agent-def))
        tracker   (when on-chunk
                    (progress/make-progress-tracker {:on-update (fn [_timeline chunk] (on-chunk chunk))}))
        on-chunk* (when tracker (:on-chunk tracker))
        q-opts    (cond-> {}
                    spec      (assoc :spec spec)
                    mdl       (assoc :model mdl)
                    on-chunk* (assoc :hooks {:on-chunk on-chunk*})
                    debug?    (assoc :debug? true))
        messages  (if (string? prompt) [(svar/user prompt)] prompt)]
    (if no-persist?
      ;; Ephemeral path: build a fresh env on a `:memory` SQLite DB so
      ;; nothing touches `~/.vis/vis.mdb`. Disposing the env tears the
      ;; in-memory DB down with it. Bypasses `lp/create!`/`lp/send!`
      ;; (both go through the shared conversations cache + the on-disk
      ;; SQLite handle) on purpose. We use `:memory` instead of nil
      ;; because the iteration loop requires a non-nil `:db-info` (it
      ;; persists turns + iterations + expression history; nil would
      ;; reject in `prepare-query-context`).
      (let [env (lp/create-environment (lp/get-router) {:db :memory})]
        (try
          (let [result (lp/query! env messages q-opts)]
            (cond-> {:conversation-id nil
                     :answer          (:answer result)
                     :iteration-count (:iteration-count result)
                     :duration-ms     (:duration-ms result)
                     :tokens          (:tokens result)
                     :cost            (:cost result)
                     :trace           (:trace result)}
              (:status result)     (assoc :status (:status result))
              (:confidence result) (assoc :confidence (:confidence result))))
          (catch Exception e
            {:conversation-id nil
             :error           (persistance/db-error->user-message e)
             :type            (str (type e))
             :exception       e})
          (finally
            (try (lp/dispose-environment! env) (catch Exception _ nil)))))
      ;; Persistent path: route through the shared conversation cache
      ;; so the run shows up in `(conversations/by-channel :cli)` and
      ;; survives process restarts.
      (let [{conversation-id :id} (lp/create! :cli {:title title})]
        (try
          (let [result (lp/send! conversation-id messages q-opts)]
            (cond-> {:conversation-id conversation-id
                     :answer          (:answer result)
                     :iteration-count (:iteration-count result)
                     :duration-ms     (:duration-ms result)
                     :tokens          (:tokens result)
                     :cost            (:cost result)
                     :trace           (:trace result)}
              (:status result)     (assoc :status (:status result))
              (:confidence result) (assoc :confidence (:confidence result))))
          (catch Exception e
            {:conversation-id conversation-id
             :error           (persistance/db-error->user-message e)
             :type            (str (type e))
             :exception       e}))))))

;;; ── Output Formatting ───────────────────────────────────────────────────

(defn result->json [result]
  (json/write-json-str result))

(defn result->edn [result]
  (pr-str result))

;; =============================================================================
;; Built-in CLI commands
;; =============================================================================

;;; ── Output helpers ──────────────────────────────────────────────────────

(defn- stdout!
  "Print to the real terminal via the saved original stdout. Other
   output (telemere, SLF4J) is redirected to the log file."
  [^String s]
  (.println ^java.io.PrintStream config/original-stdout s)
  (.flush ^java.io.PrintStream config/original-stdout))

(defn- wrap-str
  "Word-wrap `s` into a vector of lines, each ≤ `width` chars. Splits on
   whitespace; tokens longer than `width` are hard-broken so a single
   long URL or symbol can't blow the column out."
  [s width]
  (let [s (str s)]
    (cond
      (str/blank? s)            [""]
      (<= (count s) width)      [s]
      :else
      (let [tokens (str/split s #"\s+")]
        (loop [tokens tokens
               line   ""
               lines  []]
          (if-let [tok (first tokens)]
            (cond
              ;; token longer than the column → hard-split it
              (> (count tok) width)
              (let [head (subs tok 0 width)
                    tail (subs tok width)
                    lines' (cond-> lines (seq line) (conj line))]
                (recur (cons tail (rest tokens)) head lines'))

              ;; fits on the current line
              (or (str/blank? line)
                (<= (+ (count line) 1 (count tok)) width))
              (recur (rest tokens)
                (if (str/blank? line) tok (str line " " tok))
                lines)

              ;; doesn't fit → push current line, start a new one
              :else
              (recur (rest tokens) tok (conj lines line)))
            (cond-> lines (seq line) (conj line))))))))

(defn- print-table!
  "Print a formatted table to stdout!.
   `cols` is `[{:key :k :label \"L\" :width N :align :left|:right}]`.
   Cells are word-wrapped (not truncated) so long descriptions stay
   visible across multiple physical lines."
  [cols rows]
  (let [align-line (fn [s {:keys [width align]}]
                     (if (= align :right)
                       (commandline/pad-left s width)
                       (commandline/pad-right s width)))
        sep    (str "─" (str/join "─┼─"
                          (map #(apply str (repeat (:width %) \─)) cols)) "─")
        header (str " " (str/join " │ "
                          (map #(commandline/pad-right (:label %) (:width %)) cols)) " ")]
    (stdout! header)
    (stdout! sep)
    (doseq [row rows]
      (let [wrapped   (mapv (fn [c]
                              (wrap-str (get row (:key c)) (:width c)))
                        cols)
            row-lines (apply max 1 (map count wrapped))]
        (dotimes [i row-lines]
          (stdout!
            (str " "
              (str/join " │ "
                (map (fn [lines col]
                       (align-line (or (nth lines i nil) "") col))
                  wrapped cols))
              " ")))))))

(defn- print-section-heading!
  "Render a section heading line for a grouped table — used when
   `vis extensions list` breaks the rows into per-`:ext/kind`
   sub-tables. `width` is the total visible width of the surrounding
   table so the rule under the label spans the same column run."
  [label width]
  (let [label-str (str " " label " ")
        rule-len  (max 4 (- width (count label-str) 2))]
    (stdout! "")
    (stdout! (str "── " label " " (apply str (repeat rule-len \─))))))

;;; ── `vis run` — handler + bespoke arg parser ────────────────────────────

(defn- parse-run-args
  "Parse `vis run` arguments into {:prompt str :json? bool …}.

   Bespoke instead of `commandline.base/parse-args` because everything
   that ISN'T a known flag is glued together as the prompt body."
  [args]
  (loop [args         (seq args)
         opts         {}
         prompt-parts []]
    (if-not args
      (assoc opts :prompt (str/join " " prompt-parts))
      (let [arg  (first args)
            more (next args)]
        (case arg
          "--json"           (recur more (assoc opts :json? true) prompt-parts)
          "--edn"            (recur more (assoc opts :edn? true) prompt-parts)
          "--trace"          (recur more (assoc opts :trace? true) prompt-parts)
          ("--help" "-h")    (assoc opts :help? true :prompt "")
          "--debug"          (recur more (assoc opts :debug? true) prompt-parts)
          "--model"          (recur (next more) (assoc opts :model (first more)) prompt-parts)
          "--name"           (recur (next more) (assoc opts :agent-name (first more)) prompt-parts)
          "--db"             (recur (next more) (assoc opts :db (first more)) prompt-parts)
          "--no-persist"     (recur more (assoc opts :no-persist? true) prompt-parts)
          (recur more opts (conj prompt-parts arg)))))))

(defn- print-run-usage! []
  (stdout! "Usage: vis run [FLAGS] \"prompt\"")
  (stdout! "")
  (stdout! "Flags:")
  (stdout! "  --json            Print result as a single JSON envelope.")
  (stdout! "  --edn             Print result as EDN.")
  (stdout! "  --trace           Log the full iteration trace via Telemere.")
  (stdout! "  --debug           Enable verbose debug logging.")
  (stdout! "  --model NAME      Override the configured model.")
  (stdout! "  --name NAME       Set the agent name (default: cli).")
  (stdout! "  --db PATH|:memory Override the SQLite path (or :memory).")
  (stdout! "  --no-persist      Skip writes to ~/.vis/vis.mdb.")
  (stdout! "                    Runs in an ephemeral env; no row in the")
  (stdout! "                    `:cli` channel, no resume, no trace on disk.")
  (stdout! "")
  (stdout! "Examples:")
  (stdout! "  vis run \"What is 2+2?\"")
  (stdout! "  vis run --json --model gpt-4o \"Explain auth flow\"")
  (stdout! "  vis run --no-persist \"Throwaway one-shot probe\""))

(defn- cli-run!
  "`vis run` handler. `_parsed` is unused — we re-parse the residual
   ourselves so anything that isn't a flag falls into the prompt."
  [_parsed residual]
  (config/init-cli!)
  (let [{:keys [prompt json? edn? trace? help? agent-name db] :as opts}
        (parse-run-args residual)]
    (when (or help? (str/blank? prompt))
      (print-run-usage!)
      (System/exit 0))
    (let [agent-def (agent {:name (or agent-name "cli")})
          run-opts  (cond-> (dissoc opts :prompt :json? :edn? :trace? :compact?
                              :agent-name :db)
                      db (assoc :db (config/resolve-db-spec
                                      (if (= db ":memory") :memory
                                        {:backend :sqlite :path db}))))
          result    (run! agent-def prompt run-opts)]
      (cond
        json? (stdout! (result->json result))
        edn?  (stdout! (result->edn result))

        trace?
        (do (tel/log! {:level :info :id ::cli-trace
                       :data  (select-keys result [:answer :trace :iteration-count
                                                   :duration-ms :tokens :cost
                                                   :error :type])}
              "CLI trace result")
          (stdout! (str (:answer result)))
          (when (:error result)
            (when-let [ex (:exception result)]
              (stdout! "\nStack trace:")
              (.printStackTrace ^Throwable ex ^java.io.PrintStream config/original-stdout))
            (shutdown-agents)
            (System/exit 1)))

        (:error result)
        (do (stdout! (error/format-error (:error result)))
          (shutdown-agents)
          (System/exit 1))

        :else
        (do (stdout! (str (:answer result)))
          (when (:duration-ms result)
            (stdout! (str "\n[" (fmt/format-meta-line result) "]")))))
      (shutdown-agents))))

;;; ── `vis conversations` ─────────────────────────────────────────────────

(def ^:private known-channels #{"tui" "telegram" "cli"})

(defn- resolve-conversation-by-prefix
  "Resolve a user-supplied conversation reference (full UUID or an
   unambiguous prefix) to the canonical UUID. Scans every channel
   because forks are channel-agnostic; the user typed an id, we find
   it. Returns nil on miss or ambiguous prefix."
  [d input]
  (let [s (str input)]
    (or (try (persistance/db-resolve-conversation-id d s) (catch Throwable _ nil))
      (let [all     (mapcat #(lp/by-channel %) [:tui :telegram :cli])
            matches (vec (filter #(str/starts-with? (str (:id %)) s) all))]
        (when (= 1 (count matches))
          (persistance/db-resolve-conversation-id d (str (:id (first matches)))))))))

(defn- cli-fork-conversation!
  "Fork a conversation by id. Creates a new `conversation_state` row
   that points at the latest state as its parent, optionally with a
   user-supplied title. Prints the new state UUID; the conversation
   id (soul-id) stays the same so `vis channels tui --conversation-id
   <ID>` keeps working and now resumes from the fork."
  [cid-input title]
  (let [d        (lp/db-info)
        resolved (resolve-conversation-by-prefix d cid-input)]
    (cond
      (nil? resolved)
      (do (stdout! (str "Conversation not found: " cid-input))
        (stdout! "")
        (stdout! "List existing conversations with:")
        (stdout! "  vis conversations")
        (shutdown-agents)
        (System/exit 1))

      :else
      (let [opts      (cond-> {} (and title (not (str/blank? title)))
                        (assoc :title title))
            new-state (persistance/db-fork-conversation! d resolved opts)]
        (if new-state
          (do (stdout! "")
            (stdout! (str "  Forked conversation " resolved))
            (when title (stdout! (str "  Title:        " title)))
            (stdout! (str "  New state-id: " new-state))
            (stdout! "")
            (stdout! (str "  Resume with: vis channels tui --conversation-id " resolved))
            (stdout! ""))
          (do (stdout! (str "Failed to fork conversation " resolved
                         "; no existing state to fork from."))
            (shutdown-agents)
            (System/exit 1)))
        (shutdown-agents)))))

(defn- cli-list-conversations!
  "List persisted conversations for one channel. `channel-input` may
   be nil (defaults to `tui`)."
  [channel-input]
  (let [channel (or channel-input "tui")
        ch-kw   (keyword channel)
        convs   (lp/by-channel ch-kw)
        d       (lp/db-info)]
    (if (empty? convs)
      (stdout! (str "No " channel " conversations found."))
      (let [rows (mapv (fn [c]
                         (let [queries (persistance/db-list-conversation-turns d (:id c))
                               turns   (count queries)
                               last-q  (last queries)]
                           {:id        (str (:id c))
                            :title     (or (:title c) "—")
                            :turns     turns
                            :last-turn (or (some-> last-q :created-at fmt/format-date) "—")
                            :created   (or (fmt/format-date (:created-at c)) "—")}))
                   convs)]
        (stdout! (str "\n  " (str/upper-case channel) " Conversations\n"))
        (print-table!
          [{:key :id        :label "ID"        :width 36 :align :left}
           {:key :title     :label "Title"     :width 24 :align :left}
           {:key :turns     :label "Turns"     :width 5  :align :right}
           {:key :last-turn :label "Last Turn" :width 16 :align :left}
           {:key :created   :label "Created"   :width 16 :align :left}]
          rows)
        (stdout! (str "\n  " (count rows) " conversation(s)\n"))
        (stdout! "  Resume with: vis channels tui --conversation-id <ID>  (full or short)")
        (stdout! "  Or latest:   vis channels tui --resume")
        (stdout! "  Fork:        vis conversations --fork <ID> [--title TITLE]"))))
  (shutdown-agents))

(defn- cli-conversations!
  "`vis conversations` handler.

   Two modes:
   - List   --  `vis conversations [tui|telegram|cli]`
   - Fork   --  `vis conversations --fork <CONVERSATION-ID> [--title TITLE]`

   The `parsed` map carries the spec'd flags; the bare positional
   `channel` (if present) is also in `parsed`. Anything not in the
   spec is rejected upstream by `commandline/dispatch!`."
  [parsed _residual]
  (config/init-cli!)
  (let [fork-target (get parsed "fork")
        title       (get parsed "title")
        channel     (get parsed "channel")]
    (cond
      (and (some? fork-target) (not (str/blank? fork-target)))
      (cli-fork-conversation! fork-target title)

      :else
      (let [ch (when channel
                 (when (contains? known-channels channel) channel))]
        (when (and channel (nil? ch))
          (stdout! (str "Unknown channel: " channel
                     ". Expected one of: " (str/join ", " (sort known-channels))
                     ". Defaulting to tui."))
          (stdout! ""))
        (cli-list-conversations! ch)))))

;;; ── `vis auth` ──────────────────────────────────────────────────────────

(defn- print-auth-status! [provider]
  (let [s ((:provider/status-fn provider))]
    (stdout! (str "\n  " (:provider/label provider) " Auth Status"))
    (stdout! "  ─────────────────────────────────")
    (if (:authenticated? s)
      (do (stdout! "  Authenticated: yes")
        (when-let [src (:source s)]
          (stdout! (str "  Source:        " (name src))))
        (when-let [tp (:oauth-token-preview s)]
          (stdout! (str "  Token:         " tp)))
        (when (contains? s :copilot-token-valid?)
          (stdout! (str "  API token:     "
                     (if (:copilot-token-valid? s) "valid" "expired")))
          (when (:copilot-token-valid? s)
            (stdout! (str "  Expires in:    "
                       (int (/ (:expires-in-ms s) 60000)) " min")))))
      (stdout! "  Authenticated: no"))
    (stdout! "")))

(defn- cli-auth!
  "Look the requested provider up through the registry and dispatch
   --status / --logout / interactive auth via its registered fns.
   Concrete providers live in their own packages
   (`vis-provider-github-copilot`, future `vis-provider-anthropic`,
   …); vis-runtime never references them by namespace."
  [parsed residual]
  (config/init-cli!)
  (let [provider-name (or (get parsed "provider")
                        (first (remove #(str/starts-with? % "--") residual)))
        provider-id   (some-> provider-name keyword)
        flags         (set (rest residual))
        status?       (or (true? (get parsed "status")) (contains? flags "--status"))
        logout?       (or (true? (get parsed "logout")) (contains? flags "--logout"))
        provider      (when provider-id (registry/provider-by-id provider-id))
        all           (registry/registered-providers)]
    (cond
      (nil? provider-id)
      (do (stdout! "Usage: vis auth <provider> [--status | --logout]")
        (stdout! "")
        (if (seq all)
          (do (stdout! "Available providers:")
            (doseq [p (sort-by :provider/id all)]
              (stdout! (str "  " (commandline/pad-right (name (:provider/id p)) 22)
                         (:provider/label p)))))
          (stdout! "No providers registered. Drop a vis-provider-* jar onto the classpath.")))

      (nil? provider)
      (do (stdout! (str "Unknown auth provider: " (name provider-id)))
        (when (seq all)
          (stdout! (str "Available: "
                     (str/join ", " (map (comp name :provider/id)
                                      (sort-by :provider/id all)))))))

      status?
      (when (:provider/status-fn provider)
        (print-auth-status! provider))

      logout?
      (when-let [f (:provider/logout-fn provider)]
        (f)
        (stdout! (str "  Logged out of " (:provider/label provider) ". Tokens cleared.")))

      :else
      (when-let [auth-fn (:provider/auth-fn provider)]
        (try (auth-fn stdout!)
          (catch Exception e
            (stdout! (error/format-error (str "Authentication failed: " (ex-message e)))))))))
  (shutdown-agents))

;;; ── `vis doctor` ────────────────────────────────────────────────────────

;; The `vis doctor` command lives in vis-foundation now — see plan §1 Q18.
;; Foundation contributes its own `:ext/doctor-check-fn` (sections: system,
;; agents-md, skills, scan-warnings) and registers the top-level `vis doctor`
;; command via `register-cmd!`. Other extensions plug into the same
;; aggregator by declaring their own `:ext/doctor-check-fn`.

;;; ── `vis extensions` ────────────────────────────────────────────────────

(def ^:private extensions-table-cols
  [{:key :namespace :label "Namespace"   :width 28 :align :left}
   {:key :group     :label "Group"       :width 18 :align :left}
   {:key :author    :label "Author"      :width 12 :align :left}
   {:key :owner     :label "Owner"       :width  8 :align :left}
   {:key :license   :label "License"     :width 10 :align :left}
   {:key :doc       :label "Description" :width 36 :align :left}
   {:key :version   :label "Version"     :width 10 :align :left}])

(defn- extensions-table-width
  "Visible width of the rendered table (sum of column widths +
   separators + leading/trailing pad), used to size group-section
   rules so they line up with the header rule."
  [cols]
  (+ 2                                                  ; outer pads
    (reduce + (map :width cols))
    (* 3 (dec (count cols)))))                          ; " │ " between cols

(defn- cli-extensions! [_parsed _residual]
  (config/init-cli!)
  (let [exts  (list-extensions)
        cols  extensions-table-cols
        width (extensions-table-width cols)]
    (if (empty? exts)
      (stdout! "No extensions registered.")
      (do (stdout! "\n  Extensions\n")
        (doseq [[kind rows] (sort-by key (group-by :kind exts))]
          (print-section-heading! kind width)
          (print-table! cols
            (sort-by (juxt :group :namespace) rows)))
        (stdout! (str "\n  " (count exts) " extension(s)\n")))))
  (shutdown-agents))

;;; ── Top-level binary built-ins (registry/register-cmd! direct) ─────────
;;
;; `run`, `auth`, `conversations`, `doctor` are the binary's own
;; commands. They live at the top of the command tree -- `vis run
;; "..."`, NOT `vis extensions run "..."` -- so they bypass
;; `:ext/cli` (the extensions-subcommand slot, see below).
;; Direct `register-cmd!` is the right plumbing here; vis-runtime
;; is the host, not an extension contributing to `vis extensions`.

(doseq [spec
        [{:cmd/name  "run"
          :cmd/doc   "Run a one-shot agent query and print the answer."
          :cmd/usage "vis run [FLAGS] \"prompt\""
          :cmd/args  [{:name "json"       :kind :flag :type :boolean :doc "Output result as JSON."}
                      {:name "edn"        :kind :flag :type :boolean :doc "Output result as EDN."}
                      {:name "trace"      :kind :flag :type :boolean :doc "Show full execution trace."}
                      {:name "debug"      :kind :flag :type :boolean :doc "Enable svar debug logging."}
                      {:name "model"      :kind :flag :type :string  :doc "Override the LLM model."}
                      {:name "name"       :kind :flag :type :string  :doc "Agent name."}
                      {:name "db"         :kind :flag :type :string  :doc "DB target: PATH or :memory."}
                      {:name "no-persist" :kind :flag :type :boolean :doc "Run ephemerally; never write to ~/.vis/vis.mdb."}]
          :cmd/examples ["vis run \"What is 2+2?\""
                         "vis run --json --model gpt-4o \"Explain the auth flow\""
                         "vis run --no-persist \"Throwaway one-shot probe\""]
          :cmd/run-fn cli-run!}

         {:cmd/name  "auth"
          :cmd/doc   "Authenticate with an LLM provider."
          :cmd/usage "vis auth <provider> [--status | --logout]"
          :cmd/args  [{:name "provider" :kind :positional :type :string
                       :doc  "Provider id (for example: github-copilot or openai-codex)."}
                      {:name "status" :kind :flag :type :boolean
                       :doc  "Show current authentication state without logging in."}
                      {:name "logout" :kind :flag :type :boolean
                       :doc  "Clear saved credentials for the provider."}]
          :cmd/examples ["vis auth github-copilot"
                         "vis auth github-copilot --status"
                         "vis auth github-copilot --logout"
                         "vis auth openai-codex"
                         "vis auth openai-codex --status"
                         "vis auth openai-codex --logout"]
          :cmd/run-fn cli-auth!}

         {:cmd/name  "conversations"
          :cmd/doc   "List conversations stored on disk, or fork one."
          :cmd/usage "vis conversations [tui|telegram|cli] [--fork ID [--title TITLE]]"
          :cmd/args  [{:name "channel" :kind :positional :type :string
                       :doc  "Channel to list (tui|telegram|cli; default tui)."}
                      {:name "fork"  :kind :flag :type :string
                       :doc  "Fork the conversation with the given id (full UUID or unambiguous prefix)."}
                      {:name "title" :kind :flag :type :string
                       :doc  "Title to set on the new fork (used with --fork)."}]
          :cmd/examples ["vis conversations"
                         "vis conversations telegram"
                         "vis conversations --fork 3a7b2c1d-..."
                         "vis conversations --fork 3a7b2c1d --title \"Branch A\""]
          :cmd/run-fn cli-conversations!}]]
  (registry/register-cmd! spec))

;;; ── Extensions-namespaced subcommand: `vis extensions list` ─────────────
;;
;; `extensions list` introspects the extension registry, so it
;; naturally lives under the `vis extensions <cmd>` parent. But it is
;; a HOST-owned built-in, NOT a third-party contribution -- vis core
;; is the host, never an extension. So it registers directly via
;; `registry/register-cmd!` with `:cmd/parent ["extensions"]`, the
;; same plumbing the four top-level built-ins above use.

(registry/register-cmd!
  {:cmd/name   "list"
   :cmd/parent ["extensions"]
   :cmd/doc    "List every registered extension with metadata."
   :cmd/usage  "vis extensions list"
   :cmd/run-fn cli-extensions!})

;; =============================================================================
;; Dispatcher entry point (-main)
;; =============================================================================

;; =============================================================================
;; Logging routing
;;
;; Telemere ships with a `:default/console` handler that prints EVERY
;; signal to stdout. That fills the terminal with registration noise
;; before the user ever sees the help text -- painful UX for a CLI.
;;
;; Default behavior:
;;   - stdout stays clean
;;   - every signal is appended to `~/.vis/vis.log`
;;
;; Pass `--debug` / `--verbose` / `-v` (or set `VIS_DEBUG=1`) to KEEP
;; the console handler in addition to the file handler.
;; =============================================================================

(def ^:private debug-flags #{"--debug" "--verbose" "-v"})

(defn- debug-mode? [args]
  (or (some debug-flags args)
    (= "1" (System/getenv "VIS_DEBUG"))))

(defn- log-file-path []
  (let [log-dir (java.io.File. (str (System/getProperty "user.home") "/.vis"))]
    (when-not (.exists log-dir) (.mkdirs log-dir))
    (str log-dir "/vis.log")))

(defn- configure-logging!
  "Route Telemere signals: file handler always on, persistence-backed
   `:db` handler always on (so the loop's `tel/with-ctx+ {:db-info …}`
   bindings land in the conversation_log table), and the
   `:default/console` handler is OFF by default — it was removed by
   `internal.registry` at namespace load so boot-time registration
   logs never spray to stdout. We re-add it here only when `--debug`
   / `--verbose` / `-v` / `VIS_DEBUG=1` is set. Idempotent."
  [args]
  (let [debug? (debug-mode? args)
        path   (log-file-path)]
    ;; File handler ALWAYS on, so post-mortem reads always have data.
    (try
      (tel/add-handler! :file
        (tel/handler:file {:path path})
        {:min-level :info})
      (catch Throwable _ nil))
    ;; Console handler: re-add only when the user asked for verbosity.
    ;; Boot-time noise is already gone (registry.clj removed it during
    ;; namespace load); this restores the stdout stream for debugging.
    (when debug?
      (try (tel/add-handler! :default/console
             (tel/handler:console))
        (catch Throwable _ nil)))
    ;; Persistence handler: scopes signals to the right DB rows via
    ;; `:db-info` / `:conversation-soul-id` / `:conversation-turn-id` /
    ;; `:iteration-id` carried in telemere `*ctx*`. Wrapped because
    ;; the persistence facade is loaded lazily; if no backend has
    ;; registered yet, the handler will silently drop signals until
    ;; one does.
    (try (setup-db-handler!)
      (catch Throwable _ nil))))

;; =============================================================================
;; Extension discovery
;;
;; ONE call. The unified loader lives in the extension facade.
;; =============================================================================

(defn- print-extension-load-failures!
  "Print every classpath extension namespace whose `(require)` blew
   up during the most recent scan to stderr, along with the
   user-actionable hint. Pre-fix the failure was a buried
   `~/.vis/vis.log` ERROR line and the user had no surface clue
   that an entire alias namespace was unbound — the LLM in the
   sandbox would loop on `Unable to resolve symbol: v/cat` until
   the user manually dug through the log file. Now the launcher
   shouts the failure on every startup so the user can `git diff`
   the broken extension and fix the typo.

   No-op when every extension loaded cleanly."
  []
  (let [failures (manifest/load-failures)]
    (when (seq failures)
      (binding [*out* *err*]
        (println)
        (println "⚠  vis: " (count failures) "extension namespace(s) failed to load.")
        (println "   The associated alias namespace will be UNBOUND in the sandbox.")
        (println "   The agent will see `Unable to resolve symbol` for every call into it.")
        (println)
        (doseq [{:keys [extension-id extension-ns reason path]} failures]
          (println (str "   • extension '" extension-id "' (" extension-ns ")"))
          (println (str "     " reason))
          (when path
            (println (str "     manifest: " path))))
        (println)))))

(defn discover-all!
  "Run the unified extension discovery scan. Idempotent through
   Clojure's `require` cache. Returns nil.

   Prints a stderr banner enumerating every extension namespace
   whose `(require)` failed during discovery. The same warnings are
   also fed into the per-turn `<scan-warnings>` system-prompt block
   by `vis-foundation/combined-scan-warnings`, so both the user (at
   the terminal) and the LLM (in its prompt) see the failure
   immediately instead of bouncing off `Unable to resolve symbol`
   for an entire conversation."
  []
  (extension/discover-extensions!)
  (print-extension-load-failures!)
  nil)

;; =============================================================================
;; Root command
;;
;; The dispatcher's root has NO hard-coded subcommands. Every entry
;; comes from the global commandline registry. Built-ins (run, auth,
;; doctor, ...) are registered by vis-runtime; the `vis channel` and
;; `vis ext` parents are registered by the channel and extension
;; facades. Add a third-party jar with its own `register-cmd!`
;; calls and its commands appear here without any code change.
;; =============================================================================

(def ^:private DEFAULT_DOC "vis — iterative coding agent CLI")

(defn root-command
  "Build the root `vis` command tree. Subcommands are pulled fresh on
   every call so newly registered extensions show up immediately."
  []
  (registry/command
    {:cmd/name        "vis"
     :cmd/doc         DEFAULT_DOC
     :cmd/subcommands #(registry/registered-under [])}))

;; =============================================================================
;; Pre-redirect stderr for TTY-owning channels
;;
;; Some leaves (TUI, ncurses) take over the controlling terminal and
;; need stderr re-routed to a log file BEFORE any further class loading
;; triggers JVM warnings. The check is data-driven via
;; `:cmd/owns-tty?`. Channels mark themselves through the channel
;; bridge; nothing here is channel-aware.
;; =============================================================================

(defn- pre-redirect-stderr! [args]
  (when-let [{:keys [command]} (commandline/find-leaf (root-command) (cons "vis" args))]
    (when (:cmd/owns-tty? command)
      (let [log-dir (java.io.File. (str (System/getProperty "user.home") "/.vis"))]
        (when-not (.exists log-dir) (.mkdirs log-dir))
        (System/setErr (java.io.PrintStream.
                         (java.io.FileOutputStream.
                           (str log-dir "/vis.log") true) true))))))

;; =============================================================================
;; Main
;; =============================================================================

(defn- unknown-command?
  "True when the user typed something the tree doesn't recognize.
   Detected by walking the tree: if `find-leaf` resolves only to the
   ROOT (path length 1) AND there's a residual that isn't a help
   request, the user gave us an unknown command."
  [root args]
  (when (seq args)
    (let [{:keys [path residual]} (commandline/find-leaf root (cons (:cmd/name root) args))]
      (and (= 1 (count path))
        (seq residual)
        (not-any? #{"--help" "-h"} residual)))))

(defn -main
  "Discover extensions, walk the command tree, dispatch.

   Behavior:
     - No args                  -> top-level help
     - `help` / `--help` / `-h` -> help for the resolved command
     - Recognized command       -> invoke its `:cmd/run-fn`
     - Unknown command          -> top-level help + exit 1

   No magical fallback to a `run`-as-prompt shortcut -- the dispatcher
   is a pure command tree. Anyone who wants the old single-arg
   ergonomics can register a `:cmd/run-fn` on the root via a custom
   extension."
  [& args]
  ;; Quiet stdout BEFORE any extension load triggers Telemere registration
  ;; spam — the user only sees logs when they pass --debug / --verbose / -v
  ;; (or set VIS_DEBUG=1).
  (configure-logging! args)
  (discover-all!)
  (pre-redirect-stderr! args)
  (let [root      (root-command)
        full-args (cons "vis" args)]
    (cond
      (empty? args)
      (println (commandline/render-tree root))

      ;; `vis help` is a universal synonym for `vis --help`. Without
      ;; this branch the dispatcher would treat `help` as an unknown
      ;; command, print the tree, AND tag it with "Unknown command:
      ;; help" + exit 1 -- which surprised everyone who tried it.
      (= ["help"] (vec args))
      (println (commandline/render-tree root))

      (unknown-command? root args)
      (do (println (commandline/render-tree root))
        (println)
        (println (str "Unknown command: " (str/join " " args)))
        (System/exit 1))

      :else
      ;; `dispatch!` returns `{:status :ok|:help|:error|:no-match …}`.
      ;; `:error` covers spec-validation failures (missing required
      ;; args, unknown flags). Without an explicit `System/exit 1` here
      ;; the process exited 0 even though the user-visible output was
      ;; an error message + help text -- so any shell pipeline like
      ;; `vis foo --bogus && echo ok` printed `ok`. Map `:error` to
      ;; exit code 2 (POSIX convention for usage errors); `:no-match`
      ;; can't actually fire here because `unknown-command?` above
      ;; already short-circuited that case.
      (let [{:keys [status]} (commandline/dispatch! root full-args)]
        (case status
          :error (System/exit 2)
          nil)))))
