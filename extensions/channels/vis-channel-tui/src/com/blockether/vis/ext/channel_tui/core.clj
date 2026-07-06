(ns com.blockether.vis.ext.channel-tui.core
  "Lightweight TUI channel registration.

   Keep this namespace tiny: manifest discovery loads it on every Vis startup.
   The full Lanterna screen implementation is resolved only when the TUI
   channel actually runs.

   Fast-fail validation:
     `--session-id ID` is parsed, looked up, and (on miss) reported with
     a friendly message + exit code 2 BEFORE the heavy
     `com.blockether.vis.ext.channel-tui.screen` namespace (Lanterna +
     ~14 sibling tui namespaces) is required. Catching the miss early on
     the lightweight path keeps `vis channels tui --session-id <bad>`
     from paying full TUI class-loading cost.

     Correct hits still go through the normal screen channel-main, so all
     existing screen behavior - argument parsing, redirects, lifecycle -
     stays intact. Runtime semantics live below the TUI layer
     and are unaffected: this only changes WHEN screen.clj is required."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.builtin-hooks :as builtin-hooks]))

(def tui-usage "vis channels tui [--session-id ID | --resume | --continue]")

(defn render-for-tui
  "TUI's :channel/messages-renderer-fn.

   STRICT input contract: canonical answer-IR (`[:ir & nodes]` as
   produced by `vis.internal.render/->ast`). Anything else is a
   programmer bug at the call site — we throw with the actual type so
   the offender shows up in stderr immediately. The IR boundary is
   upstream (loop / persistence / chat helpers); this renderer never
   soft-coerces strings, Hiccup, EDN, or anything else.

   Returns plain markdown text the TUI transcript layouter consumes.
   A follow-up commit replaces the markdown round-trip with the
   styled-line walker (`channel-tui.render-ir/ir->lines`) without
   changing this function's contract."
  ([ir] (render-for-tui ir nil))
  ([ir opts]
   (when-not (and (vector? ir) (= :ir (first ir)))
     (throw
       (ex-info
         "render-for-tui requires canonical [:ir ...] input; build IR upstream, do not pass raw text or Hiccup here"
         {:got-type (some-> ir
                            class
                            .getName)
          :got-preview (let [s (pr-str ir)]
                         (subs s 0 (min 200 (count s))))})))
   (vis/render ir :markdown opts)))

(defn- parse-session-id-flag
  "Walk `args` and return the value of the first `--session-id` flag, or
   nil. Lightweight enough to avoid loading the screen ns. Mirrors
   `screen/parse-args` for the `--session-id` case only; the screen
   parser still owns full validation (unknown flags, missing values, etc.)
   when we hand control off to it."
  [args]
  (loop [args (seq args)]
    (when args
      (cond (= "--session-id" (first args))
            (let [v (second args)]
              (when (and (string? v) (not (str/starts-with? v "--"))) v))
            :else (recur (next args))))))

(defn- resolve-session-id
  "Resolve a user-supplied id (full UUID or unambiguous prefix) against
   ALL channels. Returns the canonical id string on hit, nil on miss.
   Kept here so the lookup runs without loading the screen ns, but it still
   goes through the public gateway facade."
  [session-id]
  (let [cid (some-> session-id
                    str
                    str/trim)]
    (when (seq cid)
      (or (when-let [session (try (vis/gateway-soul cid) (catch Throwable _ nil))]
            (:id session))
          (let [matches (->> (try (vis/gateway-list-sessions :all) (catch Throwable _ []))
                             (map :id)
                             (filter #(str/starts-with? (str %) cid))
                             vec)]
            (when (= 1 (count matches)) (str (first matches))))))))

(defn- format-session-not-found
  "Same wording as `screen/format-session-not-found` (intentional -
   the user message must not regress)."
  [cid]
  (let [available
        (try (vec (take 10 (vis/gateway-list-sessions :all))) (catch Throwable _ []))

        line
        (fn [c]
          (let [id-str
                (str (:id c))

                id8
                (if (>= (count id-str) 8) (subs id-str 0 8) id-str)

                title
                (let [t (:title c)]
                  (when-not (str/blank? t) t))]

            (str "  " id8 "  " (or title "(untitled)"))))]

    (str "Session not found: "
         cid
         (if (seq available)
           (str "\n\nAvailable sessions (most recent first):\n"
                (str/join "\n" (map line available))
                "\n\nUse the 8-char prefix or full UUID with --session-id.")
           "\n\nNo sessions exist yet - run `vis channels tui` without --session-id first."))))

(defn- require-screen-channel-main
  "Resolve the heavyweight screen channel entry point. Pulled out so the fast
   path never references the screen var until validation succeeds."
  []
  (or (requiring-resolve 'com.blockether.vis.ext.channel-tui.screen/channel-main)
      (throw (ex-info "TUI screen channel entry point did not resolve"
                      {:type :channel-tui/missing-screen-main}))))

(defn- exit-not-found!
  "Print the friendly miss message to the original stdout and exit 2.
   Pulled out as a private fn so tests can `with-redefs` it instead of
   tearing down the JVM. The exit code mirrors `screen/channel-main`'s
   `:vis/user-error` branch."
  [cid]
  (let [^java.io.PrintStream out vis/original-stdout]
    (.println out (str "vis: " (format-session-not-found cid)))
    (.flush out))
  (try (vis/shutdown!) (catch Throwable _ nil))
  (System/exit 2))

(defn- pre-validate-session-id!
  "If `args` carries `--session-id ID`, run a lightweight validation
pass: bring up `vis/init!` (DB connection only - Lanterna stays
unloaded), look the id up through the public gateway facade, and on miss
print the friendly not-found message to the original stdout and exit 2.

Returns:
  - `:miss` when `--session-id` was supplied but did not resolve
    (caller MUST NOT continue into the screen channel-main).
  - `nil` otherwise (no `--session-id`, or it resolved cleanly).

Tests can stub `vis/init!` / `vis/gateway-soul` /
`vis/gateway-list-sessions` / `vis/shutdown!` and the private
`exit-not-found!` to assert that the screen ns is NOT required on the
miss path. Production `exit-not-found!` calls `System/exit`, so the
`:miss` return is only observable in tests that have stubbed it out."
  [args]
  (when-let [cid (parse-session-id-flag args)]
    (vis/init!)
    (if (resolve-session-id cid) nil (do (exit-not-found! cid) :miss))))

(defn channel-main
  "Lazy channel entry point. Loading the Lanterna screen stack is deferred
   until the TUI channel is invoked AND any supplied `--session-id`
   has been validated, so command discovery/help and `--session-id`
   misses do not pay full TUI class-loading cost."
  [args]
  (when-not (= :miss (pre-validate-session-id! args)) ((require-screen-channel-main) args)))

(def tui-extension
  (vis/extension {:ext/name "channel-tui"
                  :ext/description "Lanterna-based terminal UI channel."
                  :ext/version "0.3.0"
                  :ext/author "Blockether"
                  :ext/owner "vis"
                  :ext/license "Apache-2.0"
                  :ext/channels [{:channel/id :tui
                                  :channel/cmd "tui"
                                  :channel/doc "Interactive terminal UI."
                                  :channel/usage tui-usage
                                  :channel/owns-tty? true
                                  :channel/main-fn #'channel-main
                                  :channel/messages-renderer-fn #'render-for-tui}]
                  :ext/channel-contributions builtin-hooks/channel-contributions}))

(vis/register-extension! tui-extension)
