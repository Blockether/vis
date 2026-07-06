(ns com.blockether.vis.internal.commandline
  "CLI command parsing, lookup, help rendering, dispatch.

   The command spec, builder, and global registry now live in
   `com.blockether.vis.internal.registry` alongside the channel and provider
   registries. This namespace provides the operations OVER those
   command maps:

     Lookup
       find-leaf       walk the tree consuming tokens until a match
       find-named      same, but ignoring the root command's name

     Argument parsing
       parse-args      parse residual tokens against `:cmd/args`
       validate-args   nil on success, error string on missing required

     Help rendering
       render-command  detailed help for one command
       render-tree     top-level overview shown on `vis` with no args
       *color-enabled?*  dynamic toggle (TTY auto-detect)
       pad-right       width-padding helper
       pad-left        width-padding helper

     Dispatch
       dispatch!       resolve, parse, validate, invoke `:cmd/run-fn`

   The registry surface (`command`, `register-cmd!`, `deregister-cmd!`,
   `registered-commands`, `registered-under`, `resolve-subcommands`)
   is reachable through `com.blockether.vis.internal.registry`."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.registry :as registry]))

;; =============================================================================
;; Lookup
;; =============================================================================

(defn- by-name
  [children nm]
  (some (fn [c]
          (when (= (:cmd/name c) nm) c))
        children))

(defn find-leaf
  "Walk the tree from `root` consuming tokens until either:
     - a child matches and has no further subcommands
     - or no child matches the next token

   Returns `{:command resolved-cmd :path [name...] :residual [token...]}`,
   or nil when even the root's name doesn't match args[0]. The
   residual is everything LEFT after the resolved command name."
  [root args]
  (let [args (vec args)]
    (when (and (seq args) (= (first args) (:cmd/name root)))
      (loop [cur root
             path [(:cmd/name root)]
             rest (subvec args 1)]

        (let [children (registry/resolve-subcommands cur)
              nxt (first rest)
              child (when nxt (by-name children nxt))]

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

(defn- coerce
  [value type]
  (case (or type :string)
    :string
    value

    :int
    (or (parse-long (str value))
        (throw (ex-info (str "Expected integer, got: " value) {:value value})))

    :boolean
    (contains? #{"true" "1" "yes"} (str/lower-case (str value)))

    :file
    value

    value))

(defn parse-args
  "Parse `raw-args` against `arg-specs`. Returns a map of
   `{arg-name value}`. Positional specs are matched in order; flag
   specs by `--name`; boolean flags need no value. Unknown flags
   are silently dropped so commands can layer their own loose flags."
  [arg-specs raw-args]
  (let [positional
        (vec (filter #(= :positional (:kind %)) arg-specs))

        flags
        (into {}
              (map (fn [a]
                     [(str "--" (:name a)) a]))
              (filter #(= :flag (:kind %)) arg-specs))]

    (loop [args
           (seq raw-args)

           pos-idx
           0

           result
           {}]

      (if-not args
        result
        (let [arg
              (first args)

              more
              (next args)]

          (if (flag-arg? arg)
            (if-let [spec (get flags arg)]
              (if (= :boolean (:type spec))
                (recur more pos-idx (assoc result (:name spec) true))
                (recur (next more)
                       pos-idx
                       (assoc result (:name spec) (coerce (first more) (:type spec)))))
              (recur more pos-idx result))
            (if (< pos-idx (count positional))
              (let [spec (nth positional pos-idx)]
                (recur more (inc pos-idx) (assoc result (:name spec) (coerce arg (:type spec)))))
              (recur more pos-idx result))))))))

(defn validate-args
  "Validate parsed args against spec. Returns nil on success, or an
   error string describing the missing required arguments."
  [arg-specs parsed]
  (let [required
        (filter :required arg-specs)

        missing
        (remove #(contains? parsed (:name %)) required)]

    (when (seq missing)
      (str "Missing required argument(s): " (str/join ", " (map :name missing))))))

(def ^:private universal-help-flags #{"--help" "-h"})

(defn unknown-flags
  "Return a vector of `--flag` tokens in `raw-args` that are NOT declared
   in `arg-specs`. Walks the args the same way `parse-args` does so that
   a string-typed flag's VALUE (e.g. `bar` in `--out bar`) is never
   misclassified as an unknown flag. Boolean flags don't consume their
   next token. Universal `--help` / `-h` are always considered known.

   Used by `dispatch!` to refuse unknown flags and surface the list of
   accepted flags via `render-command`. Pure; no side effects.

   Unknown flags are reported only by their leading token; we don't
   know whether the user intended them to take a value, so the walker
   conservatively advances by one token after each unknown."
  [arg-specs raw-args]
  (let [flags (into {}
                    (map (fn [a]
                           [(str "--" (:name a)) a]))
                    (filter #(= :flag (:kind %)) arg-specs))]
    (loop [args (seq raw-args)
           unknown []]

      (if-not args
        unknown
        (let [a (first args)
              more (next args)]

          (cond (not (flag-arg? a)) (recur more unknown)
                (contains? universal-help-flags a) (recur more unknown)
                (contains? flags a)
                (let [spec (get flags a)]
                  (if (= :boolean (:type spec)) (recur more unknown) (recur (next more) unknown)))
                :else (recur more (conj unknown a))))))))

(defn- format-unknown-flags-error
  "Render the user-facing error string for one or more unknown flags.
   Lists the accepted flag tokens (`--name TYPE` for typed flags,
   `--name` for booleans) so the user can copy-paste the right one."
  [arg-specs unknown]
  (let [decl
        (filter #(= :flag (:kind %)) arg-specs)

        names
        (mapv #(str "--" (:name %)) decl)

        head
        (if (= 1 (count unknown))
          (str "Unknown flag: " (first unknown))
          (str "Unknown flag(s): " (str/join ", " unknown)))]

    (str head
         (when (seq names) (str "\n\nAccepted flags: " (str/join ", " names)))
         "\n\nAlso accepted: --help, -h.")))

;; =============================================================================
;; Help rendering
;;
;; Two public entry points:
;;
;;   (render-command cmd path)  -> detailed help for one command,
;;                                 always used by the dispatcher when
;;                                 the user asks `--help` or hits a
;;                                 parent that has no `:cmd/run-fn`.
;;
;;   (render-tree root)         -> top-level overview shown when the
;;                                 binary is invoked with no args.
;;
;; Both build the same kind of sectioned, column-aligned output and
;; share the same color/section helpers. Color is auto-disabled when
;; stdout is not a TTY (tests, pipes), or when `NO_COLOR` is set, or
;; when `TERM=dumb` - callers can also force it via
;; `(binding [*color-enabled?* false] ...)`.
;; =============================================================================

(defn pad-right
  [s w]
  (let [s (str s)]
    (if (>= (count s) w) s (str s (apply str (repeat (- w (count s)) \space))))))

(defn pad-left
  [s w]
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

(defn- ansi [code s] (if *color-enabled?* (str "\u001b[" code "m" s "\u001b[0m") (str s)))

(defn- bold [s] (ansi "1" s))
;; Bright-black (`90`) instead of dim (`2`) - the standard ANSI "dim"
;; renders almost invisibly on most terminal themes (especially dark
;; ones with low-contrast palettes). Bright-black is the universal
;; "muted but visible" choice used by gh, kubectl, cargo, etc.
(defn- dim [s] (ansi "90" s))
(defn- cyan [s] (ansi "36" s))
(defn- yellow [s] (ansi "33" s))
(defn- magenta [s] (ansi "35" s))

(defn- section [title] (bold (cyan title)))

;; ---- Width helpers ----------------------------------------------------------

(def ^:private ANSI_RE #"\u001b\[[0-9;]*m")
(defn- visible-len ^long [s] (count (str/replace (str s) ANSI_RE "")))

(defn- pad-visible-right
  [s w]
  (let [pad (max 0 (- w (visible-len s)))]
    (str s (apply str (repeat pad \space)))))

;; ---- Arg formatting ---------------------------------------------------------

(defn- flag-token
  "Stringified left-hand side of a flag entry, e.g. `--model MODEL`
   for `:type :string`, `--verbose` for `:type :boolean`."
  [{:keys [name type]}]
  (let [tag (case (or type :string)
              :boolean
              nil

              :int
              "N"

              :file
              "PATH"

              :string
              (str/upper-case name)

              (str/upper-case (clojure.core/name (or type :string))))]
    (cond-> (str "--" name)
      tag
      (str " " tag))))

(defn- positional-token
  [{:keys [name required]}]
  (if required (str "<" name ">") (str "[" name "]")))

(defn- format-positional-args
  [pos col-width]
  (mapv (fn [{:keys [doc] :as p}]
          (str "  " (pad-visible-right (yellow (positional-token p)) col-width) (or doc "")))
        pos))

(defn- format-flag-args
  [flags col-width]
  (mapv (fn [{:keys [doc required] :as f}]
          (let [token
                (yellow (flag-token f))

                doc
                (cond-> (or doc "")
                  required
                  (str " " (dim "(required)")))]

            (str "  " (pad-visible-right token col-width) doc)))
        flags))

(defn- format-subcommand-lines
  "Render each subcommand as `name + doc`. Order is preserved
   (registration order). When the caller wants the host-canonical
   commands rendered separately from extension contributions, it
   filters `children` itself and calls this twice; this fn no longer
   mixes groups under a single header."
  [children col-width]
  (let [fmt (fn [c]
              (str "  " (pad-visible-right (magenta (:cmd/name c)) col-width) (:cmd/doc c)))]
    (mapv fmt children)))

(defn- split-children
  "Split children into host-owned canonical entries (`:cmd/internal?`
   true) and extension-contributed entries. Order is preserved inside
   each group."
  [children]
  {:internal (filterv (comp boolean :cmd/internal?) children)
   :contributed (filterv (complement (comp boolean :cmd/internal?)) children)})

(defn- render-extra-section
  "Render a single `:cmd/extra-sections` entry. Each entry is a map
   `{:title string :body string-or-fn}`; a fn body is invoked with no
   args so the caller can defer expensive lookups (registered
   extensions, runtime tables, ...) until help is actually requested.
   Blank body collapses the whole section."
  [{:keys [title body]}]
  (let [text (cond (fn? body) (try (body) (catch Throwable _ ""))
                   (string? body) body
                   :else (str body))]
    (when-not (str/blank? text) (str "\n" (section title) "\n" text))))

(defn- collect-extra-sections
  "Resolve `:cmd/extra-sections` into a vector of rendered section
   strings. `:cmd/extra-sections` itself may be a fn returning a
   sequence of entry maps, or the entry sequence directly."
  [cmd]
  (let [raw
        (:cmd/extra-sections cmd)

        entries
        (cond (nil? raw) []
              (fn? raw) (try (raw) (catch Throwable _ []))
              :else raw)]

    (->> entries
         (keep render-extra-section)
         vec)))

;; ---- Usage line + multi-paragraph doc ---------------------------------------

(defn- default-usage-line
  [path pos flags children]
  (let [parts (cond-> [(str/join " " path)]
                (seq pos)
                (into (mapv positional-token pos))

                (seq flags)
                (conj "[FLAGS]")

                (seq children)
                (conj "<subcommand>"))]
    (str/join " " parts)))

(defn- usage-line
  [cmd path pos flags children]
  (or (:cmd/usage cmd) (default-usage-line path pos flags children)))

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

(defn- col-width [items min-w] (max min-w (+ 2 (reduce max 0 (map visible-len items)))))

(defn render-command
  "Render multi-section help for a single command:
     USAGE / DESCRIPTION / SUBCOMMANDS / ARGUMENTS / FLAGS / EXAMPLES.

   Empty sections are omitted. `path` is the command-name chain
   leading up to and including this command - used for the USAGE line
   when `:cmd/usage` isn't set."
  [cmd path]
  (let [args
        (or (:cmd/args cmd) [])

        pos
        (filter #(= :positional (:kind %)) args)

        flags
        (filter #(= :flag (:kind %)) args)

        children
        (registry/resolve-subcommands cmd)

        examples
        (or (:cmd/examples cmd) [])

        sub-w
        (col-width (map :cmd/name children) 16)

        pos-w
        (col-width (map positional-token pos) 16)

        flag-w
        (col-width (map flag-token flags) 20)]

    (str/join
      "\n"
      (remove nil?
        [(section "USAGE") (str "  " (bold (usage-line cmd path pos flags children)))
         (when-let [d (doc-block (:cmd/doc cmd))]
           (str "\n" (section "DESCRIPTION") "\n" d))
         (let [{:keys [internal contributed]} (split-children children)]
           (cond (and (seq internal) (seq contributed))
                 (str "\n" (section "COMMANDS")
                      "\n" (str/join "\n" (format-subcommand-lines internal sub-w))
                      "\n\n" (section "EXTENSION COMMANDS")
                      "\n" (str/join "\n" (format-subcommand-lines contributed sub-w)))
                 (seq children) (str "\n" (section "SUBCOMMANDS")
                                     "\n" (str/join "\n"
                                                    (format-subcommand-lines children sub-w)))))
         (when-let [extras (seq (collect-extra-sections cmd))]
           (str/join "" extras))
         (when (seq pos)
           (str "\n" (section "ARGUMENTS") "\n" (str/join "\n" (format-positional-args pos pos-w))))
         (when (seq flags)
           (str "\n" (section "FLAGS") "\n" (str/join "\n" (format-flag-args flags flag-w))))
         (when (seq examples)
           (str "\n" (section "EXAMPLES")
                "\n" (str/join "\n"
                               (map (fn [ex]
                                      (str "  " (dim "$") " " ex))
                                    examples))))
         (when (seq children)
           (str "\n"
                (dim (str "Run \""
                          (str/join " " path)
                          " <subcommand> --help\" for more details."))))]))))

(defn render-tree
  "Top-level overview rendered when the binary is invoked with no
   arguments (or via `vis help`). Shows the root doc, then a single
   COMMANDS block listing every immediate subcommand."
  [root]
  (let [children
        (registry/resolve-subcommands root)

        col-w
        (col-width (map :cmd/name children) 16)

        ;; Root doc may carry both a one-liner and an extended
        ;; paragraph; render verbatim with a 2-space indent so it
        ;; lines up with the COMMANDS body.
        doc
        (doc-block (:cmd/doc root))]

    (str/join "\n"
              (remove nil?
                [(when doc doc) (when doc "") (section "COMMANDS")
                 (str/join "\n" (format-subcommand-lines children col-w)) ""
                 (dim (str "Run \""
                           (:cmd/name root)
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
  ([root args] (dispatch! root args {:print-fn println}))
  ([root args {:keys [print-fn]}]
   (if-let [{:keys [command path residual]} (find-leaf root args)]
     (let [help? (some #{"--help" "-h"} residual)
           children (registry/resolve-subcommands command)
           unresolved-before-help (take-while #(not (#{"--help" "-h"} %)) residual)]

       (cond (and help? (seq children) (seq unresolved-before-help))
             (let [err (str "Unknown command: " (str/join " " (concat path unresolved-before-help)))
                   help (str err "\n\n" (render-command command path))]

               (when print-fn (print-fn help))
               {:status :error :command command :error err :help-text help})
             (or help? (and (nil? (:cmd/run-fn command)) (seq children)))
             (let [help (render-command command path)]
               (when print-fn (print-fn help))
               {:status :help :command command :help-text help})
             (nil? (:cmd/run-fn command))
             {:status :help :command command :help-text (render-command command path)}
             :else
             ;; Strict-flag check fires only when the command actually
             ;; declares flags. Commands without `:cmd/args` (or with only
             ;; positionals) keep the loose, layer-your-own-flags posture
             ;; - `vis providers status <provider>` and similar bespoke
             ;; handlers stay working without forcing every command to
             ;; declare its full surface up front.
             (let [arg-specs (:cmd/args command)
                   has-flags? (some #(= :flag (:kind %)) arg-specs)
                   unknown (when has-flags? (unknown-flags arg-specs residual))]

               (cond (seq unknown) (let [err (format-unknown-flags-error arg-specs unknown)
                                         help (str err "\n\n" (render-command command path))]

                                     (when print-fn (print-fn help))
                                     {:status :error :command command :error err :help-text help})
                     :else (let [parsed (parse-args arg-specs residual)
                                 err (validate-args arg-specs parsed)]

                             (if err
                               (let [help (str err "\n\n" (render-command command path))]
                                 (when print-fn (print-fn help))
                                 {:status :error :command command :error err :help-text help})
                               {:status :ok
                                :command command
                                :result ((:cmd/run-fn command) parsed residual)}))))))
     {:status :no-match :args args})))
