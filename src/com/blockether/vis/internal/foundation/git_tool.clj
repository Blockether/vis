(ns com.blockether.vis.internal.foundation.git-tool
  "The single `git` tool — a thin, honest proxy to the host `git` binary.

   ONE built-in Python function, `git`, runs a SERIAL batch of `git <args…>` commands
   in the active workspace root and returns a TOTAL, string-keyed result the model reads
   directly: `{\"commands\" [{\"cmd\", \"args\", \"stdout\", \"stderr\", \"exit\", \"duration_ms\",
   \"timed_out\", \"timeout_secs\"} …]}`. Every key is present for every command
   (`stderr` \"\" when empty, `exit` None only when that command timed out), so no
   field ever KeyErrors. A non-zero exit is DATA, not a tool failure; later commands
   still run, so a batch reports partial outcomes exactly like a terminal script.

   This REPLACES the old JGit-backed `git_*` surface (foundation-git): no
   embedded git implementation, no SSH/BouncyCastle stack — the only git is
   the one already on the user's PATH, so behaviour matches their shell
   exactly. Read-only workspace facts (branch/dirty/ahead-behind for the
   footer, env block, file picker) still flow through
   `com.blockether.vis.internal.git`; this namespace is purely the model-
   facing command tool.

   Built-in (bare `git` in the sandbox, next to `cat`/`rg`), active when
   the workspace sits inside or contains a repository."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.git :as git]
            [com.blockether.vis.internal.strutil :as strutil]
            [com.blockether.vis.internal.workspace :as workspace])
  (:import (java.io File)))

(def ^:private default-timeout-secs
  "Sync ceiling for a git op — generous enough for network ops (fetch/push/
   clone) yet bounded so a hung remote can't wedge the turn."
  120)

(defn- now-ms ^long [] (System/currentTimeMillis))

(defn- tokenize
  "Quote-aware whitespace split of a bare command string, so a human can type
   `git(\"commit -m 'wip'\")` in the sandbox and still get the message as one
   token. The model passes a LIST (the native-tool schema is an array), which
   skips this path entirely — each element is a literal arg, spaces and all."
  [^String s]
  (loop
    [chars
     (seq s)

     cur
     (StringBuilder.)

     quote
     nil

     acc
     []]

    (if-let [c (first chars)]
      (cond quote (if (= c quote)
                    (recur (rest chars) cur nil acc)
                    (recur (rest chars) (.append cur c) quote acc))
            (or (= c \") (= c \')) (recur (rest chars) cur c acc)
            (Character/isWhitespace ^char c)
            (if (pos? (.length cur))
              (recur (rest chars) (StringBuilder.) nil (conj acc (str cur)))
              (recur (rest chars) cur nil acc))
            :else (recur (rest chars) (.append cur c) quote acc))
      (if (pos? (.length cur)) (conj acc (str cur)) acc))))

(defn- normalize-args
  "Coerce the tool's single `args` value into a vector of literal git tokens.
   A sequential is taken element-by-element (each a literal arg); a string is
   quote-aware tokenized; a lone scalar becomes a one-element vector."
  [args]
  (cond (sequential? args) (into [] (comp (map str) (remove str/blank?)) args)
        (string? args) (tokenize args)
        (nil? args) []
        :else [(str args)]))

(defn- verbose-add-tokens
  "`git add` is silent by design, so a bare `add` gives no feedback on WHAT it
   staged — the op-card / GIT band paints an empty `$ add`. When the tokens are
   an `add` with no reporting flag already present (`-v`/`--verbose`, or the
   self-reporting `-n`/`--dry-run`), append `--verbose` so git itself lists each
   staged path (`add 'file'` / `remove 'file'`) on stdout. Only the SUBPROCESS
   runs verbose; the echoed `cmd`/`args` stay the caller's original tokens,
   since `--verbose` only adds reporting, not a different index mutation."
  [tokens]
  (if (and (= "add" (first tokens)) (not (some #{"-v" "--verbose" "-n" "--dry-run"} tokens)))
    (conj (vec tokens) "--verbose")
    (vec tokens)))

(defn- git-command-result
  "Run one literal Git argv in `dir` and return its total, string-keyed result.
   A non-zero exit is deliberately a result, so callers can continue a batch and
   inspect every command's stdout/stderr independently."
  [^File dir args]
  (let [tokens (normalize-args args)]
    (when (empty? tokens)
      (throw
        (ex-info
          "Every git command needs at least one argument, e.g. [\"status\"] or [\"commit\", \"-m\", \"msg\"]."
          {:type ::no-args})))
    (let
      [t0 (now-ms)
       {:keys [exit out err timed-out? duration-ms]}
       (git/run-command dir (verbose-add-tokens tokens) {:timeout-secs default-timeout-secs})
       t1 (now-ms)]

      {"cmd" (str "git " (str/join " " tokens))
       "args" (vec tokens)
       "stdout" (or out "")
       "stderr" (or err "")
       "exit" exit
       "duration_ms" (or duration-ms (- t1 t0))
       "timed_out" (boolean timed-out?)
       "timeout_secs" default-timeout-secs})))

(defn- git-impl
  ([env commands] (git-impl env commands nil))
  ([_env commands _opts]
   (let [commands (vec commands)]
     (when (empty? commands)
       (throw (ex-info "git needs at least one command." {:type ::no-commands})))
     ;; Deliberately serial: a later command may depend on an earlier mutation
     ;; (`add` → `commit` → `push`). A failed command remains data and never
     ;; prevents the remaining commands from producing their own total results.
     (let
       [dir ^File (.getCanonicalFile (workspace/cwd))
        t0 (now-ms)
        results (mapv #(git-command-result dir %) commands)
        t1 (now-ms)]

       (extension/success {:result {"commands" results}
                           :op :git
                           :metadata {:command-count (count results)
                                      :started-at-ms t0
                                      :finished-at-ms t1
                                      :duration-ms (- t1 t0)}})))))

;; =============================================================================
;; Render — the op-card for a `git` call: `<args>` headline (with an
;; exit/timeout note) + fenced stdout / stderr. The GIT badge already names
;; the command, so the headline shows only the ARGS — no redundant `$ git`.
;; A `commit -m <msg>` is special-cased: the message is lifted OUT of the
;; headline (which stays `commit -m`) and rendered as a markdown blockquote at
;; the top of the body, so the real message reads as a quoted block instead of
;; a crammed argument. Like the shell renderer, git writes normal output to
;; stderr on success (progress, hints), so the
;; `stderr:` label rides along only when the command actually FAILED.
;; =============================================================================

(defn- fence
  "Wrap `s` in a code fence, or nil when blank."
  ([s] (fence s nil))
  ([s lang] (when (seq (str s)) (strutil/fenced s lang))))

(defn- section
  "One labeled git detail section, matching REPL/shell expanded cards."
  ([label s] (section label s nil))
  ([label s lang]
   (when-let [f (fence s lang)]
     (str "**" label "**\n" f))))

(defn- prose-section
  "One labeled prose detail section. Used for commit messages so blockquotes stay
  readable instead of becoming code."
  [label s]
  (when (seq (str s)) (str "**" label "**\n" s)))

(defn- kv-lines
  "Render non-nil `[label value]` pairs as `label: value` lines."
  [pairs]
  (not-empty (str/join "\n"
                       (for
                         [[k v]
                          pairs

                          :when (some? v)]

                         (str k ": " v)))))

(defn- commit-message
  "The commit MESSAGE this git call authored, or nil for a non-commit. Joins
   every `-m`/`--message` value (git treats repeated `-m` as separate
   paragraphs) plus the inline `--message=…` form, so `commit -m subject -m body`
   reads back as the real multi-paragraph message."
  [args]
  (when (= "commit" (first args))
    (not-empty (str/join "\n\n"
                         (loop
                           [xs
                            (rest args)

                            acc
                            []]

                           (if-let [a (first xs)]
                             (cond (#{"-m" "--message"} a) (recur (drop 2 xs)
                                                                  (cond-> acc
                                                                    (some? (second xs))
                                                                    (conj (second xs))))
                                   (str/starts-with? (str a) "--message=")
                                   (recur (rest xs) (conj acc (subs a (count "--message="))))
                                   :else (recur (rest xs) acc))
                             acc))))))

(defn- strip-commit-message
  "The commit arg vector with the `-m`/`--message` VALUES removed (the flag
   itself stays) so the headline reads `commit -m` instead of cramming the whole
   message onto one line — the message renders as its own quoted block below."
  [args]
  (loop
    [xs
     args

     out
     []]

    (if-let [a (first xs)]
      (cond (#{"-m" "--message"} a) (recur (drop 2 xs) (conj out a))
            (str/starts-with? (str a) "--message=") (recur (rest xs) (conj out "--message"))
            :else (recur (rest xs) (conj out a)))
      out)))

(defn- quote-block
  "Render `s` as a markdown blockquote (each line prefixed `> `, blank lines a
   bare `>`) so the channel paints it as one solid `│ ` bar — the commit message
   reads as a quoted block instead of a crammed argument."
  [s]
  (->> (str/split-lines (str/trim (str s)))
       (map (fn [l]
              (if (str/blank? l) ">" (str "> " l))))
       (str/join "\n")))

(defn- clip-subject
  "Clamp a commit SUBJECT to `max-len` chars for the one-line headline, adding
   a single-glyph ellipsis, so a really long subject (or a run-on first
   paragraph) can't blow out the collapsed card. The FULL message still renders
   untruncated as the blockquote body below, so nothing is lost — only the
   headline preview is bounded."
  ([s] (clip-subject s 72))
  ([s ^long max-len]
   (let [s (str/trim (str s))]
     (if (> (count s) max-len) (str (str/trimr (subs s 0 (max 0 (dec max-len)))) "\u2026") s))))

(defn- render-git-result
  [r]
  (let
    [args
     (get r "args")

     exit
     (get r "exit")

     failed?
     (or (get r "timed_out") (and exit (not (zero? (long exit)))))

     note
     (cond (get r "timed_out") " (timed out)"
           (and exit (not (zero? (long exit)))) (str " (exit " exit ")")
           :else "")

     msg
     (commit-message args)

     ;; A commit lifts its SUBJECT (first message line) onto the headline
     ;; after an em-dash — `commit — <subject>` — so the collapsed card
     ;; shows WHAT was committed while the full message still renders as the
     ;; blockquote body below. The now-redundant `-m` flags are dropped (the
     ;; subject already says it's a message commit); any OTHER flag
     ;; (`--amend`, `-a`, …) survives. Dropped on failure so the `(exit N)`
     ;; note stays the headline's focus.
     subject
     (some-> msg
             str/split-lines
             first
             str/trim
             not-empty)

     show?
     (and subject (not failed?))

     base
     (cond->> (if msg (strip-commit-message args) args)
       show?
       (remove #{"-m" "--message"}))

     head
     (cond-> (str/join " " base)
       show?
       (str " \u2014 " (clip-subject subject)))

     status
     (kv-lines [["status"
                 (cond (get r "timed_out") "timed out"
                       failed? "failure"
                       :else "success")] ["exit" exit]
                ["duration"
                 (some-> (get r "duration_ms")
                         vis/format-duration)]
                ;; The timeout budget is TOTAL in the result but only worth a row
                ;; when it was actually hit.
                ["timeout" (when (get r "timed_out") (str (get r "timeout_secs") "s"))]])

     body
     (->> [(section "COMMAND" (str "git " (str/join " " args)) "bash") (section "STATUS" status)
           (when msg (prose-section "MESSAGE" (quote-block msg)))
           (section "STDOUT" (get r "stdout")) (section "STDERR" (get r "stderr"))]
          (remove nil?)
          (str/join "\n\n"))]

    {:summary (str "⎇ " head note) :body (when (seq body) body)}))

(defn- render-git-batch-result
  "Render one expandable result card with each command's own stdout/stderr intact."
  [r]
  (let [commands (vec (get r "commands"))
        rendered (mapv render-git-result commands)
        failures (count (filter #(or (get % "timed_out")
                                     (let [exit (get % "exit")]
                                       (and exit (not (zero? (long exit))))))
                                commands))
        successes (- (count commands) failures)
        outcome (str successes " succeeded, " failures " failed")
        body (->> rendered
                  (map-indexed (fn [^long idx {:keys [summary body]}]
                                 (str "### " (inc idx) ". " summary
                                      (when (seq body) (str "\n\n" body)))))
                  (str/join "\n\n────────────\n\n"))]
    {:summary (str "⎇ " (count commands) " git commands — " outcome)
     :body (when (seq body) body)}))

;; =============================================================================
;; Symbol + extension. Built-in ⇒ binds BARE as `git` in the sandbox ns.
;; =============================================================================

(defn- inject-env [env f args] {:env env :fn f :args (into [env] args)})

(def
  ^{:doc
    "await git([[\"status\", \"--short\"], [\"diff\", \"--stat\"]])
await git([[\"add\", \"-A\"], [\"commit\", \"-m\", \"wip: message with spaces\"]])

Run SERIAL host-Git commands in the workspace root. `commands` is a non-empty LIST
of non-empty LISTS of literal tokens; each inner element is one git argument, safe
for commit messages and paths with spaces. Commands run in request order, so later
mutations see earlier ones. Every command returns exactly `{\"cmd\", \"args\", \"stdout\",
\"stderr\", \"exit\", \"duration_ms\", \"timed_out\", \"timeout_secs\"}` under
`{\"commands\" [...]}`. ALL stdout and stderr stay with the command that emitted them.

Gotcha: a non-zero `exit` is DATA to read, not a tool failure; remaining commands run."
    :arglists '([commands])}
  git
  git-impl)

(def git-symbol
  (vis/symbol
    #'git
    {:symbol 'git
     :native-tool? true
     :result
     "Object with exactly `commands`, whose entries each have `cmd`, `args`, `stdout`, `stderr`, `exit`, `timed_out`, `timeout_secs`, and `duration_ms`."
     :name "git"
     :description
     (str
       "Run SERIAL host-Git commands only when `session[\"workspace\"]` lacks needed VCS facts or to act. "
       "Each keeps stdout/stderr; non-zero exits are data and later commands run.")
     :call {:pos ["commands"]}
     :render render-git-batch-result
     :color-role :tool-color/shell
     :before-fn inject-env
     :tag :mutation
     :schema {:type "object"
              :properties
              {"commands"
               {:type "array"
                :minItems 1
                :description
                "Git commands in serial request order; each a list of literal argv tokens with `git` omitted, e.g. `[[\"status\", \"--short\"]]`."
                :items {:type "array" :minItems 1 :items {:type "string"}}}}
              :required ["commands"]
              :additionalProperties false}}))

(def git-symbols [git-symbol])

(def vis-extension
  (vis/extension
    {:ext/name "foundation-git"
     :ext/description
     "Single built-in `git` tool: runs SERIAL batches of host-Git argv in the workspace root and returns per-command exit/stdout/stderr. Replaces the JGit-backed git_ surface — the only git is the one on the user's PATH. Activates when the workspace sits inside or contains a repository."
     :ext/version "0.2.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/activation-fn (fn [_env]
                          (let [root (git/cwd-file)]
                            (or (git/in-repository? root)
                                (seq (:repositories (vis/repository-inventory root))))))
     :ext/engine {:ext.engine/builtin? true :ext.engine/symbols git-symbols}
     :ext/kind "foundation"}))

(vis/register-extension! vis-extension)
