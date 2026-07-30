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
   - [[ordered]] — the caller's value as a vector, refusing a non-collection, an
     empty batch and (deliberately) a SET: an unordered collection cannot
     describe a strictly ordered batch.
   - [[run-serial]] — run in input order, every result at its input position.
   - [[result]] — the `{\"commands\" [...]}` envelope both tools return.
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
  "`commands` as a vector, in input order. Throws for `tool` when the value is
   not an ordered collection (a set or map has no input order and must never be
   silently sequenced) or when the batch is empty."
  [tool commands]
  (when-not (or (sequential? commands) (instance? java.util.List commands))
    (throw (ex-info (str tool
                         " commands must be an ORDERED list; a set or a map has no input order.")
                    {:type ::bad-commands :tool tool})))
  (let [v (vec commands)]
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

(defn result
  "The batch envelope both tools return: `{\"commands\" [per-command result …]}`."
  [results]
  {commands-key results})

(defn batch?
  "Is `r` a batch result rather than a single-command one?"
  [r]
  (contains? r commands-key))

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
