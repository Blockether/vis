(ns com.blockether.vis.internal.doctor
  "Doctor protocol: aggregates `:ext/doctor-fn` from every
   registered extension into a single cross-cutting diagnostic
   surface. `vis doctor` invokes [[run-checks]] then
   [[format-output]] + [[exit-code]].

   Plan §1 Q19 + §10:
     - One fn per extension. The fn returns a seq of message maps;
       the extension self-stamps `:check-id` on each message when it
       wants the formatter's per-section prefix.
     - Output ordering: extensions in registration order; messages
       in fn-return order. Levels NOT re-sorted within a section -
       cause-and-effect narrative preserved.
     - Activation contract: the fn runs for EVERY registered
       extension regardless of `:ext/activation-fn`. Doctor fns must
       defensively handle missing env keys.
     - Exit codes: 0 if only :info or empty; 1 if any :warn (no
       :error); 2 if any :error.
     - TTY-detected ANSI colors. UTF-8 icons by default."
  (:require [clojure.string :as string]
            [com.blockether.vis.internal.extension :as extension]
            [taoensso.telemere :as tel]))

;; ---------------------------------------------------------------------------
;; Run.
;; ---------------------------------------------------------------------------

(defn- format-bytes
  [^long n]
  (cond (< n 1024) (str n " B")
        (< n (* 1024 1024))
        (String/format java.util.Locale/US "%.1f KB" (object-array [(/ (double n) 1024.0)]))
        (< n (* 1024 1024 1024)) (String/format java.util.Locale/US
                                                "%.1f MB"
                                                (object-array [(/ (double n) (* 1024.0 1024.0))]))
        :else (String/format java.util.Locale/US
                             "%.1f GB"
                             (object-array [(/ (double n) (* 1024.0 1024.0 1024.0))]))))

(defn- host-system-messages
  [environment]
  (let
    [rt
     (Runtime/getRuntime)

     used
     (- (.totalMemory rt) (.freeMemory rt))

     max-mem
     (.maxMemory rt)

     db-path
     (or (some-> environment
                 :db-info
                 :path)
         "(no DB)")]

    (mapv #(assoc %
             :ext "vis"
             :check-id ::system)
          [{:level :info
            :message (str "OS: " (System/getProperty "os.name")
                          " " (System/getProperty "os.arch")
                          " " (System/getProperty "os.version"))}
           {:level :info
            :message (str "Java: "
                          (System/getProperty "java.version")
                          " ("
                          (System/getProperty "java.vendor")
                          ")")} {:level :info :message (str "Clojure: " (clojure-version))}
           {:level :info :message (str "Memory: " (format-bytes used) " / " (format-bytes max-mem))}
           {:level :info :message (str "DB path: " db-path)}])))

(defn- coerce-message
  "Best-effort sanity check on a doctor fn's returned message. Forces
   `:level` into the allowed set; missing/blank `:message` becomes a
   placeholder so the output never has empty lines."
  [m]
  (let
    [level
     (let [l (:level m)]
       (if (#{:info :warn :error} l) l :error))

     message
     (or (:message m) "(no message)")]

    (-> m
        (assoc :level level
               :message message))))

(defn- run-one-extension
  "Run ONE extension's `:ext/doctor-fn`. Each emitted message
   gets `:ext` (the namespace) auto-injected; `:check-id` is left to
   the extension to stamp. Throwables become a single :error message
   describing the throw."
  [ext-ns doctor-fn environment]
  (try (let
         [returned
          (doctor-fn environment)

          msgs
          (cond (nil? returned) []
                (map? returned) [returned]
                (sequential? returned) (vec returned)
                :else [{:level :error
                        :message (str ":ext/doctor-fn returned non-message value: "
                                      (pr-str returned))}])]

         (mapv (fn [m]
                 (-> (coerce-message m)
                     (assoc :ext ext-ns)))
               msgs))
       (catch Throwable t
         (tel/log! {:level :error :id ::check-threw :data {:ext ext-ns :error (ex-message t)}})
         [{:level :error
           :message (str ":ext/doctor-fn threw: " (or (ex-message t) (str t)))
           :ext ext-ns}])))

(defn run-checks
  "Walk every registered extension, invoke its `:ext/doctor-fn`,
   return a vec of message maps with `:ext` auto-injected. The
   extension's fn is responsible for stamping `:check-id` on each
   message when it wants per-section grouping in the formatter.

   Plan §10: extensions in registration order; messages in fn-return
   order. Activation-fn ignored: every registered extension's fn runs."
  [environment]
  (vec (concat (host-system-messages environment)
               (mapcat (fn [ext]
                         (when-let [doctor-fn (:ext/doctor-fn ext)]
                           (run-one-extension (:ext/name ext) doctor-fn environment)))
                       (extension/registered-extensions)))))

(defn exit-code
  "Compute the doctor exit code from a vec of messages. 0 / 1 / 2 by
   max level. Plan Q19/d2."
  [messages]
  (cond (some #(= :error (:level %)) messages) 2
        (some #(= :warn (:level %)) messages) 1
        :else 0))

;; ---------------------------------------------------------------------------
;; Format.
;; ---------------------------------------------------------------------------

(def ^:private ICONS "Plan §10: UTF-8 by default." {:info "ℹ" :warn "⚠" :error "✗"})

(def ^:private ANSI
  {:reset "\u001b[0m" :dim "\u001b[2m" :yellow "\u001b[33m" :red "\u001b[31m" :bold "\u001b[1m"})

(defn- tty?
  "Best-effort TTY detection on stdout. `(System/console)` returns
   nil when stdout is piped/redirected; that's our signal to skip
   ANSI."
  []
  (some? (System/console)))

(defn- color-for
  [level use-ansi?]
  (if-not use-ansi?
    ""
    (case level
      :warn
      (:yellow ANSI)

      :error
      (:red ANSI)

      "")))

(defn- message-label
  [{:keys [check-id]}]
  (or (some-> check-id
              name)
      "?"))

(defn- format-message
  "Render one diagnostic message line:
     `<icon> <label>: <message>`
   where label is the message's `:check-id` name. Followed
   (optionally) by an indented `-> <remediation>`. `use-ansi?` controls
   whether to wrap level-colored bits."
  [{:keys [level message remediation] :as m} use-ansi?]
  (let
    [icon
     (or (ICONS level) "•")

     color
     (color-for level use-ansi?)

     reset
     (if use-ansi? (:reset ANSI) "")

     dim
     (if use-ansi? (:dim ANSI) "")

     head
     (str "  " color icon reset " " (message-label m) ": " message)

     tail
     (when (and remediation (not (string/blank? remediation)))
       (str "\n      " dim "-> " remediation reset))]

    (str head tail)))

(defn- format-extension-section
  [ext-name messages use-ansi?]
  (let
    [bold
     (if use-ansi? (:bold ANSI) "")

     rst
     (if use-ansi? (:reset ANSI) "")

     ext-str
     (str ext-name)

     head
     (str "  " bold ext-str rst)

     rule
     (str "  " (apply str (repeat (count ext-str) "─")))

     body
     (string/join "\n" (mapv #(format-message % use-ansi?) messages))]

    (string/join "\n" [head rule body])))

(defn format-output
  "Build the full TTY output from a vec of messages. Empty result
   prints a placeholder. Caller routes it to stdout. ANSI is
   auto-detected; pass `:use-ansi?` to override."
  ([messages] (format-output messages {:use-ansi? (tty?)}))
  ([messages {:keys [use-ansi?]}]
   (let
     [bold
      (if use-ansi? (:bold ANSI) "")

      rst
      (if use-ansi? (:reset ANSI) "")]

     (cond (empty? messages) "vis doctor\n\nNo diagnostic checks registered."
           :else
           (let
             [grouped
              (group-by :ext messages)

              ext-order
              (vec (distinct (mapv :ext messages)))

              sections
              (mapv #(format-extension-section % (get grouped %) use-ansi?) ext-order)

              totals
              (frequencies (mapv :level messages))

              summary
              (str "Summary: "
                   (or (:error totals) 0)
                   " errors, "
                   (or (:warn totals) 0)
                   " warnings, "
                   (or (:info totals) 0)
                   " info")]

             (str bold "vis doctor" rst "\n\n" (string/join "\n\n" sections) "\n\n" summary))))))

;; ---------------------------------------------------------------------------
;; Startup hint - one-line warning printed before non-doctor commands
;; when issues exist. Plan §10 last subsection.
;; ---------------------------------------------------------------------------

(defn startup-hint-line
  "Return a single-line string like `⚠ vis: 2 issues detected - run
   \\`bin/vis doctor\\` for details.` when warn/error count > 0;
   nil otherwise. Caller decides whether to print (skipped when the
   command being dispatched IS `vis doctor`)."
  ([] (startup-hint-line {}))
  ([environment]
   (let
     [msgs
      (try (run-checks environment)
           (catch Throwable t
             (tel/log! {:level :error :id ::startup-hint-failed :data {:error (ex-message t)}})
             []))

      issues
      (count (filter #(#{:warn :error} (:level %)) msgs))]

     (when (pos? issues)
       (str "⚠ vis: "
            issues
            " issue"
            (when (> issues 1) "s")
            " detected - run `bin/vis doctor` for details.")))))
