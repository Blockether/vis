(ns com.blockether.vis.internal.foundation.git-tool
  "The single `git` tool — a thin, honest proxy to the host `git` binary.

   ONE built-in Python function, `git`, runs `git <args…>` in the active
   workspace root and returns a lean, string-keyed result the model reads
   directly: `{\"cmd\", \"args\", \"stdout\", \"duration_ms\"}` plus `\"exit\"`
   (when the process finished), `\"stderr\"` (when non-empty), and
   `\"timed_out\"` (when it blew the timeout). A non-zero exit is DATA, not a
   tool failure — the model reads it like it would in a terminal.

   This REPLACES the old JGit-backed `git_*` surface (foundation-git): no
   embedded git implementation, no SSH/BouncyCastle stack — the only git is
   the one already on the user's PATH, so behaviour matches their shell
   exactly. Read-only workspace facts (branch/dirty/ahead-behind for the
   footer, env block, file picker) still flow through
   `com.blockether.vis.internal.git`; this namespace is purely the model-
   facing command tool.

   Built-in (bare `git` in the sandbox, next to `cat`/`rg`), gated to
   activate only when the workspace sits inside a repository."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.git :as git]
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
  (loop [chars
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

(defn- git-impl
  ([env args] (git-impl env args nil))
  ([_env args _opts]
   (let [tokens (normalize-args args)]
     (when (empty? tokens)
       (throw
         (ex-info
           "git needs at least one argument, e.g. git([\"status\"]) or git([\"commit\", \"-m\", \"msg\"])."
           {:type ::no-args})))
     (let [dir ^File (.getCanonicalFile (workspace/cwd))
           t0 (now-ms)
           {:keys [exit out err timed-out? duration-ms]}
           (git/run-git dir (verbose-add-tokens tokens) {:timeout-secs default-timeout-secs})
           t1 (now-ms)]

       (extension/success
         {:result (cond-> {"cmd" (str "git " (str/join " " tokens))
                           "args" (vec tokens)
                           "stdout" (or out "")
                           "duration_ms" (or duration-ms (- t1 t0))}
                    (some? exit)
                    (assoc "exit" exit)

                    timed-out?
                    (assoc "timed_out"
                      true "timeout_secs"
                      default-timeout-secs)

                    (not (str/blank? err))
                    (assoc "stderr" err))
          :op :git
          :metadata {:command (str "git " (str/join " " tokens))
                     :exit exit
                     :timed-out? (boolean timed-out?)
                     :started-at-ms t0
                     :finished-at-ms t1
                     :duration-ms (or duration-ms (- t1 t0))}})))))

;; =============================================================================
;; Render — the op-card for a `git` call: `<args>` headline (with an
;; exit/timeout note) + fenced stdout / stderr. The GIT badge already names
;; the command, so the headline shows only the ARGS — no redundant `$ git`.
;; A `commit -m <msg>` is special-cased: the message is lifted OUT of the
;; headline (which stays `commit -m`) and rendered as a markdown blockquote at
;; the top of the body, so the real message reads as a quoted block instead of
;; a crammed argument. Like shell_run's renderer, git writes normal output to
;; stderr on success (progress, hints), so the
;; `stderr:` label rides along only when the command actually FAILED.
;; =============================================================================

(defn- fence
  "Wrap `s` in a code fence, or nil when blank."
  ([s] (fence s nil))
  ([s lang] (when (seq (str s)) (str "```" (or lang "") "\n" s "\n```"))))

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
                       (for [[k v]
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
                         (loop [xs
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
  (loop [xs
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
  (let [args
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
                   ["timeout"
                    (when-let [s (get r "timeout_secs")]
                      (str s "s"))]])

        body
        (->> [(section "COMMAND" (str "git " (str/join " " args)) "bash") (section "STATUS" status)
              (when msg (prose-section "MESSAGE" (quote-block msg)))
              (section "STDOUT" (get r "stdout")) (section "STDERR" (get r "stderr"))]
             (remove nil?)
             (str/join "\n\n"))]

    {:summary (str "⎇ " head note) :body (when (seq body) body)}))

;; =============================================================================
;; Symbol + extension. Built-in ⇒ binds BARE as `git` in the sandbox ns.
;; =============================================================================

(defn- inject-env [env f args] {:env env :fn f :args (into [env] args)})

(def
  ^{:doc
    "await git([\"status\", \"--short\"])
await git([\"commit\", \"-m\", \"wip: message with spaces\"])

Run the host `git` binary in the workspace root with the given args and return
its result. `args` is a LIST of literal tokens (each element is one git argument
— safe for commit messages / paths with spaces); a bare string is quote-aware
split for convenience.
Returns {\"cmd\", \"args\", \"stdout\", \"duration_ms\"} plus, only when meaningful (use .get): \"exit\", \"stderr\", \"timed_out\".
Gotcha: a non-zero \"exit\" is DATA to read (like in a terminal), not a tool failure."
    :arglists '([args])}
  git
  git-impl)

(def git-symbol
  (vis/symbol
    #'git
    {:symbol 'git
     :native-tool? true
     :name "git"
     :call {:pos ["args"]}
     :render render-git-result
     :color-role :tool-color/shell
     :before-fn inject-env
     :tag :mutation
     :schema
     {:type "object"
      :properties
      {"args"
       {:type "array"
        :items {:type "string"}
        :description
        "git arguments as a list of literal tokens, e.g. [\"status\", \"--short\"] or [\"commit\", \"-m\", \"a message\"]."}}
      :required ["args"]}}))

(def git-symbols [git-symbol])

(def ^:private prompt-text
  (str "git tool — ONE function, a direct proxy to the host `git` binary:\n"
       "  git([\"status\", \"--short\"])   git([\"log\", \"--oneline\", \"-5\"])\n"
       "  git([\"add\", \"-A\"])           git([\"commit\", \"-m\", \"message with spaces\"])\n"
       "Pass args as a LIST of literal tokens (each element is one argument — the\n"
       "safe way for commit messages / paths with spaces); a bare string is\n"
       "quote-aware split for convenience. It runs in the workspace root and\n"
       "returns {\"cmd\", \"stdout\", \"duration_ms\"} plus \"exit\"/\"stderr\"/\"timed_out\"\n"
       "when meaningful. A non-zero \"exit\" is DATA to read, not a tool error.\n"
       "Read-only VCS facts (branch/dirty/ahead-behind) already ride in\n"
       "session[\"workspace\"] — read there before shelling out to probe."))

(def vis-extension
  (vis/extension
    {:ext/name "foundation-git"
     :ext/description
     "Single built-in `git` tool: runs the host git binary in the workspace root with the given args (git([\"status\"]) / git([\"commit\", \"-m\", \"msg\"])) and returns exit/stdout/stderr. Replaces the JGit-backed git_ surface — the only git is the one on the user's PATH. Activates only when the workspace sits inside a repository."
     :ext/version "0.2.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/activation-fn (fn [_env]
                          (git/in-repository? (git/cwd-file)))
     :ext/engine {:ext.engine/builtin? true :ext.engine/symbols git-symbols}
     :ext/prompt-fn (fn [_env]
                      prompt-text)
     :ext/kind "foundation"}))

(vis/register-extension! vis-extension)
