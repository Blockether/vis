(ns com.blockether.vis.loop.conversations.shared
  "Reusable helpers for RLM conversation lifecycle."
  (:require [clojure.string :as str]
            [com.blockether.vis.languages.commons.edit :as edit]
            [com.blockether.vis.languages.commons.grep :as grep]
            [com.blockether.vis.languages.commons.list :as list]
            [com.blockether.vis.languages.commons.read :as read]
            [com.blockether.vis.languages.commons.write :as write]
            [com.blockether.vis.core :as core]))

;; conv-id (string) -> {:env env :lock Object}
(defonce cache (atom {}))

(def base-tools
  "File-system tools registered on every conversation env (all channels).
   Shell is intentionally NOT here — Telegram and any other untrusted caller
   would otherwise have arbitrary code execution on the host. If a caller
   truly needs shell, wire a scoped tool into the agent-def for that run."
  [read/tool-def
   write/tool-def
   edit/tool-def
   list/tool-def
   grep/tool-def])

(defn register-base-tools!
  [env]
  (reduce (fn [e {:keys [sym fn] :as t}]
            (core/register-env-fn! e sym fn (dissoc t :sym :fn)))
    env
    base-tools))

(defn cache-env!
  [id env]
  (swap! cache assoc id {:env env :lock (Object.)})
  env)

(defn error->user-message
  "Map an exception from `conversations/send!` to a human-readable string.
   Infrastructure errors get friendly phrasing; everything else passes through
   as-is. Used by every adapter — only the final presentation differs."
  [^Throwable e]
  (let [ex-type (:type (ex-data e))]
    (case ex-type
      :svar.llm/all-providers-exhausted
      "LLM provider is currently unavailable. Please try again in a few minutes."

      :svar.llm/circuit-open
      "LLM provider circuit breaker is open — too many recent failures. Please wait a moment."

      :svar.llm/provider-exhausted
      "LLM provider exhausted all retry attempts. The service may be down."

       ;; default
       (str "Error: " (ex-message e)))))

(defn streamed-code->expr
  "Normalize one streamed `:code` item into a raw expr string.
   Streaming chunks can carry plain strings OR maps like
   `{:expr \"(foo ...)\" :time-ms ...}`. Returns nil for unsupported shapes."
  [entry]
  (cond
    (string? entry) entry
    (map? entry)    (let [expr (:expr entry)]
                      (when (string? expr) expr))
    :else           nil))

(defn chunk->code-vec
  "Extract normalized code expressions from a streaming chunk.
   Returns [] when chunk has no `:code` payload."
  [{:keys [code]}]
  (if (sequential? code)
    (vec (keep streamed-code->expr code))
    []))

(defn merge-stream-iteration
  "Merge one streaming chunk into an iteration timeline vec.

   `iters` is a vector of iteration entries:
   [{:iteration int :thinking str-or-nil :code [str] :final? bool} ...]

   Merge rules are adapter-agnostic and intentionally defensive:
   - preserve prior `:thinking` when incoming chunk has nil
   - preserve prior `:code` when incoming chunk has no code payload
   - mark `:final?` when either chunk `:final` OR chunk `:done?` OR prior final

   This keeps all adapters aligned when terminal `done?` chunks arrive with
   sparse payloads."
  [iters {:keys [iteration thinking final done?] :as chunk}]
  (let [new-code (chunk->code-vec chunk)
        existing (get iters iteration)
        merged   {:iteration iteration
                  :thinking  (if (and (nil? thinking) existing)
                               (:thinking existing)
                               thinking)
                  :code      (if (and (empty? new-code) existing)
                               (:code existing)
                               new-code)
                  :final?    (or (boolean final)
                               (:final? existing)
                               (boolean done?))}]
    (cond
      (< iteration (count iters)) (assoc iters iteration merged)
      (= iteration (count iters)) (conj iters merged)
      :else                       (conj iters merged))))

(defn make-on-chunk-projector
  "Create a reusable streaming projector callback.

   Returns a function `(fn [chunk] timeline)` that:
   1) merges each incoming chunk with `merge-stream-iteration`
   2) keeps timeline state internally in an atom
   3) returns the updated timeline vector
   4) optionally calls `:on-update` with `(on-update timeline chunk)`

   Options:
   - :initial   initial timeline vector (defaults to [])
   - :on-update side-effect callback invoked after each merge

   This gives adapters a zero-duplication path for chunk merge semantics.
   They only decide how to project/render the returned timeline." 
  ([]
   (make-on-chunk-projector {}))
  ([{:keys [initial on-update]}]
   (let [timeline* (atom (vec (or initial [])))]
     (fn [chunk]
       (let [timeline (swap! timeline* merge-stream-iteration chunk)]
         (when on-update
           (on-update timeline chunk))
         timeline)))))

(defn first-symbol
  "Extract the leading symbol name from a Clojure expression string.
   Returns nil when no obvious symbol is present."
  [code]
  (when code
    (let [t  (str/trim code)
          b  (if (str/starts-with? t "(") (subs t 1) t)
          nm (first (str/split b #"[\s\)\(\"']" 2))]
      (when (and nm (not (str/blank? nm))) nm))))
