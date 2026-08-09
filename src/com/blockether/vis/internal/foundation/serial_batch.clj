(ns com.blockether.vis.internal.foundation.serial-batch
  "ONE definition of \"a strictly ordered batch of commands\", shared by the two
   tools that run one: `shell` and `git`.

   Both tools had grown their own copy of the same four ideas — the `commands`
   input property, the ordered-collection check, the serial reduce that keeps a
   failed command's slot, and the expandable result card that numbers each
   command — with the copies drifting apart in name (`cmd` vs `commands`), in
   shape and in wording. They are defined here exactly once:

   - [[commands-property]] — the `commands` JSON-Schema property; the ITEM shape
     stays with the tool (a shell line is a string, a git command is argv).
   - [[ordered]] — the caller's value as a vector: a bare string is coerced into
     the batch of one it means, while a non-collection, an empty batch and
     (deliberately) a SET are refused — an unordered collection cannot
     describe a strictly ordered batch.
   - [[run-serial]] — run in input order, every result at its input position.
   - [[budget]] — the ONE wait budget an ordered batch shares, so a command is
     given what is LEFT of it and never a fresh full wait of its own.
   - [[result]] — the `{\"commands\" [...]}` key both tools answer with: `git`
     returns it alone, `shell` merges it onto its own total result shape.
   - [[card]] — one expandable op-card, `### n. <summary>` per command.

   The wire key is `commands` for BOTH tools, so a batch reads the same whether
   it ran through bash or through git."
  (:require [clojure.string :as str]))

(def commands-key
  "The ONE wire key for an ordered batch — input property AND result key."
  "commands")

(defn commands-property
  "The shared `commands` array property for a tool's JSON Schema. `items` is the
   tool's own item shape and `description` its own prose; everything structural
   (array, non-empty, ordered) is fixed here."
  [{:keys [items description]}]
  {:type "array" :minItems 1 :items items :description description})

(defn ordered
  "`commands` as a vector, in input order. A batch is an ARRAY — one command is a
   batch of ONE (`[\"ls\"]`) — but a bare string is COERCED into exactly that
   batch of one rather than failing the call: the shape written, read back and
   rendered stays the array, while the obvious one-command spelling never costs a
   turn. Throws for `tool` only when there is no ordered batch to be had: an
   unordered collection (a set or a map has no input order and must never be
   silently sequenced), a value that is neither, or an empty batch."
  [tool commands]
  (let
    [v (cond
         ;; The one coercion: a lone command line IS a batch of one.
         (string? commands) [commands]
         (or (sequential? commands) (instance? java.util.List commands)) (vec commands)
         :else (throw (ex-info (str
                                 tool
                                 " commands must be an ORDERED array (a bare string is taken as the"
                                 " batch of one); a set or a map has no input order.")
                               {:type ::bad-commands :tool tool})))]
    (when (empty? v)
      (throw (ex-info (str tool " needs at least one command.") {:type ::no-commands :tool tool})))
    v))

(defn run-serial
  "Run `commands` strictly in input order — `(run-one command)` finishes before
   the next begins — and return the results in that same order.

   With `on-error`, a thrown command becomes `(on-error command throwable)` at
   its own input position, so one launch failure neither erases the completed
   entries nor shifts the later ones. Without it the throw propagates and the
   whole call fails. An InterruptedException is turn CANCELLATION, never a
   per-command failure: it always propagates so the tool call stops promptly."
  ([commands run-one] (run-serial commands run-one nil))
  ([commands run-one on-error]
   (reduce (fn [results command]
             (conj results
                   (if on-error
                     (try (run-one command)
                          (catch InterruptedException e (throw e))
                          (catch Exception e (on-error command e)))
                     (run-one command))))
           []
           commands)))

(defn budget
  "The batch's ONE wait budget, as a no-argument function returning the whole
   seconds LEFT of it — 0 once it is spent.

   A wait is what the CALLER waits for the CALL, so it belongs to the whole
   ordered batch and never to each command in turn: three commands under a 5s
   wait have to answer in 5 seconds, not in 15, and ten git commands under a 120s
   default cannot cost twenty minutes. Each command is handed what is LEFT, and a
   batch that spends its budget leaves the rest of the commands unstarted rather
   than borrowing a fresh full wait for every one of them.

   Rounded UP, so a command still inside the budget always gets at least the one
   whole second a process timeout can express."
  [^long total-secs]
  (let [end (+ (System/currentTimeMillis) (* 1000 total-secs))]
    (fn []
      (max 0 (long (Math/ceil (/ (double (- end (System/currentTimeMillis))) 1000.0)))))))

(defn result
  "The batch's own key: `{\"commands\" [per-command result …]}`, in input order.
   `git` returns exactly this; `shell` MERGES it onto its one total result shape.
   NEITHER tool has a lone-command shape: one command is a batch
   of one, so `commands` is where a command's own output always is."
  [results]
  {commands-key results})

(defn batch?
  "Did `r` actually run commands — does it carry entries? Emptiness, not absence,
   is the test: both tools carry `commands` on EVERY result, so a stage that ran
   no command of its own (a shell lifecycle op) leaves it empty instead of
   answering with a second envelope."
  [r]
  (boolean (seq (get r commands-key))))

(defn failed?
  "Did one command entry fail — a timeout, or a non-zero exit?"
  [entry]
  (boolean (or (get entry "timed_out")
               (let [exit (get entry "exit")]
                 (and exit (not (zero? (long exit))))))))

(defn tally
  "Default headline tail: `S succeeded, F failed`."
  [results]
  (let [failures (count (filter failed? results))]
    (str (- (count results) failures) " succeeded, " failures " failed")))

(defn card
  "One expandable op-card for a serial batch: each command rendered by
   `render-one` into `{:summary :body}`, numbered and divided, under a headline
   of `icon`, the command count, `noun`, and the outcome from `tally-fn`
   (default [[tally]])."
  [{:keys [icon noun results render-one tally-fn]}]
  (let
    [results
     (vec results)

     body
     (->> results
          (map render-one)
          (map-indexed (fn [^long idx {:keys [summary body]}]
                         (str "### " (inc idx) ". " summary (when (seq body) (str "\n\n" body)))))
          (str/join "\n\n────────────\n\n"))]

    {:summary (str icon " " (count results) " " noun " commands — " ((or tally-fn tally) results))
     :body (when (seq body) body)}))
