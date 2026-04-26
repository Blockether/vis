(ns com.blockether.vis.commandline
  "Reusable command-line primitives.

   Models a CLI as a TREE of `command` maps. Each command:

     {:cmd/name        \"tui\"                 ;; required
      :cmd/doc         \"Run the TUI.\"        ;; required
      :cmd/usage       \"vis channels tui …\"  ;; optional
      :cmd/args        [{…arg-spec…}]          ;; optional
      :cmd/run-fn      (fn [parsed-args raw-residual] …) ;; optional
      :cmd/subcommands [child …]               ;; optional, vector OR (fn [] vector)
      :cmd/owns-tty?   true                    ;; optional metadata}

   Parent commands need no `:cmd/run-fn`; children are matched by
   `:cmd/name` against the next CLI token. `:cmd/subcommands` may be
   a vector or a 0-arg fn returning a vector \u2014 the latter lets
   callers compute children from a live registry without rebuilding
   the parent each tick.

   ── How the rest of the codebase uses this ───────────────────────

   `vis-core/channels.cli` assembles a root command tree mixing
   built-in subcommands (`run`, `auth`, `doctor`, \u2026) with two
   delegating parents:

     - `vis extensions <cmd>` \u2192 children come from extension `:ext/cli`
     - `vis channels <name>`  \u2192 children come from the channel registry

   Both parents pass a 0-arg fn for `:cmd/subcommands` so help and
   dispatch always see the latest registrations. The dispatcher and
   the help renderer share the same tree, which is the whole point
   of pulling this into its own package."
  (:require [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [taoensso.telemere :as tel]))

;; =============================================================================
;; Spec
;; =============================================================================

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))

(s/def :cmd/name  non-blank-string?)
(s/def :cmd/doc   non-blank-string?)
(s/def :cmd/usage non-blank-string?)
(s/def :cmd/run-fn ifn?)
(s/def :cmd/owns-tty? boolean?)

;; Where in the command tree this command mounts. Vector of parent
;; command-names from the root, EXCLUDING the root itself and the
;; command's own `:cmd/name`. Examples:
;;   []                  — top-level (`vis <name>`)
;;   ["extensions"]      — nested under `vis extensions`
;;   ["channels"]        — nested under `vis channels`
;;   ["foo" "bar"]       — nested as `vis foo bar <name>`
;; Used by the CLI dispatcher's auto-mount via `registered-under`.
(s/def :cmd/parent (s/coll-of string? :kind vector?))

;; arg spec: {:name "model" :kind :flag|:positional :type :string|:int|:boolean
;;            :required true :doc "..."}
(s/def :cmd.arg/name non-blank-string?)
(s/def :cmd.arg/kind #{:flag :positional})
(s/def :cmd.arg/type #{:string :int :boolean :file})
(s/def :cmd.arg/required boolean?)
(s/def :cmd.arg/doc string?)

(s/def ::arg
  (s/keys :req-un [:cmd.arg/name :cmd.arg/kind]
    :opt-un [:cmd.arg/type :cmd.arg/required :cmd.arg/doc]))

(s/def :cmd/args (s/coll-of ::arg :kind vector?))

;; subcommands: vector OR 0-arg ifn returning vector
(s/def :cmd/subcommands
  (s/or :static (s/coll-of map? :kind vector?)
    :dynamic ifn?))

;; Optional vector of single-line example invocations shown in the
;; EXAMPLES help section, e.g.
;;   ["vis run \"What is 2+2?\""
;;    "vis run --json --model gpt-4o \"Explain auth flow\""]
(s/def :cmd/examples (s/coll-of string? :kind vector?))

(s/def ::command
  (s/keys :req [:cmd/name :cmd/doc]
    :opt [:cmd/usage :cmd/args :cmd/run-fn :cmd/subcommands
          :cmd/owns-tty? :cmd/examples :cmd/parent]))

(defn command
  "Build and validate a command map. Children are NOT validated
   recursively; they're checked the first time `dispatch!` or
   `render-help` walks into them, which keeps dynamic subcommands
   from forcing their fn at build time."
  [spec]
  (when-not (s/valid? ::command spec)
    (throw (ex-info (str "Invalid command '" (:cmd/name spec) "':\n"
                      (with-out-str (s/explain ::command spec)))
             {:type    :commandline/invalid-spec
              :name    (:cmd/name spec)
              :explain (s/explain-data ::command spec)})))
  spec)

(defn resolve-subcommands
  "Return the static vector of subcommands, calling the dynamic fn
   when needed. Returns `[]` when the command has no children."
  [cmd]
  (let [s (:cmd/subcommands cmd)]
    (cond
      (nil? s)        []
      (vector? s)     s
      (sequential? s) (vec s)
      (ifn? s)        (vec (s))
      :else           (throw (ex-info ":cmd/subcommands must be a vector or 0-arg fn"
                               {:got (type s) :command (:cmd/name cmd)})))))

;; =============================================================================
;; Lookup + dispatch
;; =============================================================================

(defn- by-name [children nm]
  (some (fn [c] (when (= (:cmd/name c) nm) c)) children))

(defn find-leaf
  "Walk the tree from `root` consuming tokens until either:
     - a child matches and has no further subcommands     - or no child matches the next token

   Returns `{:command resolved-cmd :path [name…] :residual [token…]}`,
   or nil when even the root's name doesn't match args[0]. The
   residual is everything LEFT after the resolved command name."
  [root args]
  (let [args (vec args)]
    (when (and (seq args) (= (first args) (:cmd/name root)))
      (loop [cur  root
             path [(:cmd/name root)]
             rest (subvec args 1)]
        (let [children (resolve-subcommands cur)
              nxt      (first rest)
              child    (when nxt (by-name children nxt))]
          (if child
            (recur child (conj path nxt) (subvec rest 1))
            {:command cur :path path :residual rest}))))))

(defn find-named
  "Like `find-leaf`, but matches against the bare arg vector ignoring
   the root command's name (the way `cli/-main` typically gets called).
   Useful when the root is implicit and you just want the resolved
   subcommand for the given args."
  [root args]
  (find-leaf root (cons (:cmd/name root) args)))

;; =============================================================================
;; Argument parsing
;; =============================================================================

(defn- flag-arg? [s] (str/starts-with? (str s) "--"))

(defn- coerce [value type]
  (case (or type :string)
    :string  value
    :int     (or (parse-long (str value))
               (throw (ex-info (str "Expected integer, got: " value)
                        {:value value})))
    :boolean (contains? #{"true" "1" "yes"} (str/lower-case (str value)))
    :file    value
    value))

(defn parse-args
  "Parse `raw-args` against `arg-specs`. Returns a map of
   `{arg-name value}`. Positional specs are matched in order; flag
   specs by `--name`; boolean flags need no value. Unknown flags
   are silently dropped so commands can layer their own loose flags."
  [arg-specs raw-args]
  (let [positional (vec (filter #(= :positional (:kind %)) arg-specs))
        flags      (into {} (map (fn [a] [(str "--" (:name a)) a]))
                     (filter #(= :flag (:kind %)) arg-specs))]
    (loop [args    (seq raw-args)
           pos-idx 0
           result  {}]
      (if-not args
        result
        (let [arg  (first args)
              more (next args)]
          (if (flag-arg? arg)
            (if-let [spec (get flags arg)]
              (if (= :boolean (:type spec))
                (recur more pos-idx (assoc result (:name spec) true))
                (recur (next more) pos-idx
                  (assoc result (:name spec)
                    (coerce (first more) (:type spec)))))
              (recur more pos-idx result))
            (if (< pos-idx (count positional))
              (let [spec (nth positional pos-idx)]
                (recur more (inc pos-idx)
                  (assoc result (:name spec) (coerce arg (:type spec)))))
              (recur more pos-idx result))))))))

(defn validate-args
  "Validate parsed args against spec. Returns nil on success, or an
   error string describing the missing required arguments."
  [arg-specs parsed]
  (let [required (filter :required arg-specs)
        missing  (remove #(contains? parsed (:name %)) required)]
    (when (seq missing)
      (str "Missing required argument(s): "
        (str/join ", " (map :name missing))))))

;; =============================================================================
;; Help rendering
;;
;; Two public entry points:
;;
;;   (render-command cmd path)  → detailed help for one command,
;;                                 always used by the dispatcher when
;;                                 the user asks `--help` or hits a
;;                                 parent that has no `:cmd/run-fn`.
;;
;;   (render-tree root)         → top-level overview shown when the
;;                                 binary is invoked with no args.
;;
;; Both build the same kind of sectioned, column-aligned output and
;; share the same color/section helpers. Color is auto-disabled when
;; stdout is not a TTY (tests, pipes), or when `NO_COLOR` is set, or
;; when `TERM=dumb` — callers can also force it via
;; `(binding [*color-enabled?* false] …)`.
;; =============================================================================

(defn pad-right [s w]
  (let [s (str s)]
    (if (>= (count s) w) s (str s (apply str (repeat (- w (count s)) \space))))))

(defn pad-left [s w]
  (let [s (str s)]
    (if (>= (count s) w) s (str (apply str (repeat (- w (count s)) \space)) s))))

;; ---- Color ------------------------------------------------------------------

(def ^:dynamic *color-enabled?*
  "Color output toggle. Auto-detects: TTY attached + no `NO_COLOR` env
   + `TERM` not `dumb`. Override with `binding`. Tests run with
   `System/console` returning nil, so colors are off by default and
   substring assertions on `render-*` output stay stable."
  (boolean (and (System/console)
             (str/blank? (System/getenv "NO_COLOR"))
             (not= "dumb" (System/getenv "TERM")))))

(defn- ansi [code s]
  (if *color-enabled?*
    (str "\u001b[" code "m" s "\u001b[0m")
    (str s)))

(defn- bold    [s] (ansi "1" s))
;; Bright-black (`90`) instead of dim (`2`) — the standard ANSI "dim"
;; renders almost invisibly on most terminal themes (especially dark
;; ones with low-contrast palettes). Bright-black is the universal
;; "muted but visible" choice used by gh, kubectl, cargo, etc.
(defn- dim     [s] (ansi "90" s))
(defn- cyan    [s] (ansi "36" s))
(defn- yellow  [s] (ansi "33" s))
(defn- magenta [s] (ansi "35" s))

(defn- section [title]
  (bold (cyan title)))

;; ---- Width helpers ----------------------------------------------------------
;;
;; `count` over a string with ANSI escapes counts the escape bytes
;; too, breaking column alignment. Strip them BEFORE measuring so
;; padding stays correct under either color mode.

(def ^:private ANSI_RE #"\u001b\[[0-9;]*m")
(defn- visible-len ^long [s] (count (str/replace (str s) ANSI_RE "")))

(defn- pad-visible-right [s w]
  (let [pad (max 0 (- w (visible-len s)))]
    (str s (apply str (repeat pad \space)))))

;; ---- Arg formatting ---------------------------------------------------------

(defn- flag-token
  "Stringified left-hand side of a flag entry, e.g. `--model MODEL`
   for `:type :string`, `--verbose` for `:type :boolean`."
  [{:keys [name type]}]
  (let [tag (case (or type :string)
              :boolean nil
              :int     "N"
              :file    "PATH"
              :string  (str/upper-case name)
              (str/upper-case (clojure.core/name (or type :string))))]
    (cond-> (str "--" name)
      tag (str " " tag))))

(defn- positional-token
  [{:keys [name required]}]
  (if required (str "<" name ">") (str "[" name "]")))

(defn- format-positional-args [pos col-width]
  (mapv (fn [{:keys [doc] :as p}]
          (str "  " (pad-visible-right (yellow (positional-token p)) col-width)
            (or doc "")))
    pos))

(defn- format-flag-args [flags col-width]
  (mapv (fn [{:keys [doc required] :as f}]
          (let [token (yellow (flag-token f))
                doc   (cond-> (or doc "")
                        required (str " " (dim "(required)")))]
            (str "  " (pad-visible-right token col-width) doc)))
    flags))

(defn- format-subcommand-lines [children col-width]
  (mapv (fn [c]
          (str "  " (pad-visible-right (magenta (:cmd/name c)) col-width)
            (:cmd/doc c)))
    children))

;; ---- Usage line + multi-paragraph doc ---------------------------------------

(defn- default-usage-line [path pos flags children]
  (let [parts (cond-> [(str/join " " path)]
                (seq pos)      (into (mapv positional-token pos))
                (seq flags)    (conj "[FLAGS]")
                (seq children) (conj "<subcommand>"))]
    (str/join " " parts)))

(defn- usage-line [cmd path pos flags children]
  (or (:cmd/usage cmd)
    (default-usage-line path pos flags children)))

(defn- doc-block
  "Indent each line of `:cmd/doc` two spaces so it lines up with the
   section bodies. The first line is treated as a one-liner; everything
   after it is a free-form description."
  [doc]
  (when-not (str/blank? doc)
    (->> (str/split-lines doc)
      (map #(str "  " %))
      (str/join "\n"))))

;; ---- Public renderers -------------------------------------------------------

(defn- col-width [items min-w]
  (max min-w (+ 2 (reduce max 0 (map visible-len items)))))

(defn render-command
  "Render multi-section help for a single command:
     USAGE / DESCRIPTION / SUBCOMMANDS / ARGUMENTS / FLAGS / EXAMPLES.

   Empty sections are omitted. `path` is the command-name chain
   leading up to and including this command — used for the USAGE line
   when `:cmd/usage` isn't set."
  [cmd path]
  (let [args     (or (:cmd/args cmd) [])
        pos      (filter #(= :positional (:kind %)) args)
        flags    (filter #(= :flag (:kind %))       args)
        children (resolve-subcommands cmd)
        examples (or (:cmd/examples cmd) [])
        sub-w    (col-width (map :cmd/name children) 16)
        pos-w    (col-width (map positional-token pos) 16)
        flag-w   (col-width (map flag-token flags) 20)]
    (str/join "\n"
      (remove nil?
        [(section "USAGE")
         (str "  " (bold (usage-line cmd path pos flags children)))

         (when-let [d (doc-block (:cmd/doc cmd))]
           (str "\n" (section "DESCRIPTION") "\n" d))

         (when (seq children)
           (str "\n" (section "SUBCOMMANDS") "\n"
             (str/join "\n" (format-subcommand-lines children sub-w))))

         (when (seq pos)
           (str "\n" (section "ARGUMENTS") "\n"
             (str/join "\n" (format-positional-args pos pos-w))))

         (when (seq flags)
           (str "\n" (section "FLAGS") "\n"
             (str/join "\n" (format-flag-args flags flag-w))))

         (when (seq examples)
           (str "\n" (section "EXAMPLES") "\n"
             (str/join "\n" (map (fn [ex] (str "  " (dim "$") " " ex))
                              examples))))

         (when (seq children)
           (str "\n" (dim (str "Run \"" (str/join " " path)
                            " <subcommand> --help\" for more details."))))]))))

(defn render-tree
  "Top-level overview rendered when the binary is invoked with no
   arguments (or via `vis help`). Shows the root doc, then a single
   COMMANDS block listing every immediate subcommand."
  [root]
  (let [children (resolve-subcommands root)
        col-w    (col-width (map :cmd/name children) 16)
        ;; Root doc may carry both a one-liner and an extended
        ;; paragraph; render verbatim with a 2-space indent so it
        ;; lines up with the COMMANDS body.
        doc      (doc-block (:cmd/doc root))]
    (str/join "\n"
      (remove nil?
        [(when doc doc)
         (when doc "")
         (section "COMMANDS")
         (str/join "\n" (format-subcommand-lines children col-w))
         ""
         (dim (str "Run \"" (:cmd/name root)
                " <command> --help\" for more information about a command."))]))))

;; =============================================================================
;; Top-level dispatch
;; =============================================================================

(defn dispatch!
  "Resolve the command for `args` against `root`, parse the residual
   tokens against the resolved command's `:cmd/args` spec, and call
   its `:cmd/run-fn` with `[parsed-args residual]`.

   When the command lacks `:cmd/run-fn` and has subcommands, prints
   help for that level via `render-command`. When `--help`/`-h` is
   in the residual, also prints help.

   Returns:
     {:status :ok       :command cmd :result <whatever run-fn returned>}
     {:status :help     :command cmd :help-text <string>}
     {:status :no-match :args args}"
  ([root args]
   (dispatch! root args {:print-fn println}))
  ([root args {:keys [print-fn]}]
   (if-let [{:keys [command path residual]} (find-leaf root args)]
     (let [help? (some #{"--help" "-h"} residual)]
       (cond
         (or help? (and (nil? (:cmd/run-fn command))
                     (seq (resolve-subcommands command))))
         (let [help (render-command command path)]
           (when print-fn (print-fn help))
           {:status :help :command command :help-text help})

         (nil? (:cmd/run-fn command))
         {:status :help :command command
          :help-text (render-command command path)}

         :else
         (let [parsed (parse-args (:cmd/args command) residual)
               err    (validate-args (:cmd/args command) parsed)]
           (if err
             (let [help (str err "\n\n" (render-command command path))]
               (when print-fn (print-fn help))
               {:status :error :command command :error err :help-text help})
             {:status :ok :command command
              :result ((:cmd/run-fn command) parsed residual)}))))
     {:status :no-match :args args})))

;; =============================================================================
;; Global registry — extensions, channels, and any other plug-in jar
;; can self-register CLI commands without touching the dispatcher.
;;
;; Register at namespace-load time:
;;
;;     (ns my.ext.git
;;       (:require [com.blockether.vis.commandline :as cmd]))
;;
;;     (cmd/register-global!
;;       {:cmd/name   "git-status"
;;        :cmd/parent ["extensions"]         ;; mounts under `vis extensions git-status`
;;        :cmd/doc    "Show git working tree status."
;;        :cmd/run-fn (fn [_parsed _residual] (println (status)))})
;;
;; Auto-discovery: ship `META-INF/vis/commandline.edn` in the jar's
;; resources/ listing every namespace that calls `register-global!`:
;;
;;     [my.ext.git my.ext.docker]
;;
;; The CLI dispatcher calls `discover-commands!` once at boot, then
;; uses `registered-under` to compose subcommand vectors at every
;; level of the tree dynamically.
;; =============================================================================

(defonce ^:private global-registry
  ;; Vector preserves registration order, which then becomes the
  ;; default ordering in help output. De-duplication is by
  ;; [parent vector + command name].
  (atom []))

(defn- registry-key [c]
  [(or (:cmd/parent c) []) (:cmd/name c)])

(defn register-global!
  "Register a command in the global registry. Idempotent on
   `[:cmd/parent :cmd/name]` — re-registering replaces the prior
   entry, useful for REPL-driven development. Returns the validated
   command map."
  [spec]
  (let [c   (command spec)
        k   (registry-key c)
        cur @global-registry]
    (reset! global-registry
      (let [stripped (vec (remove #(= k (registry-key %)) cur))]
        (conj stripped c)))
    (tel/log! {:level :info :id ::register-global
               :data  {:name (:cmd/name c)
                       :parent (:cmd/parent c)}
               :msg   (str "Command '" (str/join " " (conj (or (:cmd/parent c) []) (:cmd/name c)))
                        "' registered")})
    c))

(defn deregister-global!
  "Remove a registered command. `parent` defaults to `[]` (top-level)."
  ([nm] (deregister-global! [] nm))
  ([parent nm]
   (swap! global-registry
     (fn [cur] (vec (remove #(= [parent nm] (registry-key %)) cur))))
   nil))

(defn registered-commands
  "Return all registered commands as a vector, in registration order."
  []
  @global-registry)

(defn registered-under
  "Return the vector of registered commands whose `:cmd/parent` equals
   `parent-path` (a vector of names). Use this from a parent command's
   `:cmd/subcommands` slot \u2014 typically as a 0-arg fn so newly
   registered children appear immediately:

       {:cmd/name \"ext\"
        :cmd/doc  \"Run an extension command.\"
        :cmd/subcommands #(cmd/registered-under [\"ext\"])}"
  [parent-path]
  (let [k (vec parent-path)]
    (vec (filter #(= k (or (:cmd/parent %) []))
           @global-registry))))

;; ----------------------------------------------------------------------------
;; Auto-discovery
;; ----------------------------------------------------------------------------

(def ^:private COMMANDS_RESOURCE "META-INF/vis/commandline.edn")

(defn discover-commands!
  "Scan the classpath for every `META-INF/vis/commandline.edn`
   resource. Each file is an EDN vector of namespace symbols whose
   load triggers `register-global!`. Returns the count of commands
   added by this call (for diagnostics).

   Idempotent through the underlying `require` cache \u2014 calling
   it twice does no extra work."
  []
  (let [urls   (try
                 (enumeration-seq
                   (.getResources
                     (.getContextClassLoader (Thread/currentThread))
                     COMMANDS_RESOURCE))
                 (catch Exception _ nil))
        before (count @global-registry)]
    (doseq [^java.net.URL url urls]
      (try
        (let [ns-syms (edn/read-string (slurp url))]
          (when (sequential? ns-syms)
            (doseq [ns-sym ns-syms]
              (when (symbol? ns-sym)
                (try (require ns-sym)
                  (tel/log! {:level :info :id ::discover
                             :data  {:command-ns ns-sym :source (str url)}
                             :msg   (str "Auto-discovered commandline ns '"
                                      ns-sym "' from " url)})
                  (catch Throwable t
                    (tel/log! {:level :error :id ::discover-failed
                               :data  {:command-ns ns-sym :source (str url)
                                       :class (.getName (class t))
                                       :message (ex-message t)}
                               :msg   (str "Failed to load commandline ns '"
                                        ns-sym "': " (ex-message t))})))))))
        (catch Throwable t
          (tel/log! {:level :error :id ::discover-parse-failed
                     :data  {:source (str url) :message (ex-message t)}
                     :msg   (str "Failed to parse " url ": " (ex-message t))}))))
    (- (count @global-registry) before)))
