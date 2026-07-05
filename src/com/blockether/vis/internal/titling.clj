(ns com.blockether.vis.internal.titling
  "Session-title subsystem, lifted out of the loop namespace: the three listener
   registries (per-session value, global, and the pending/spinner channel), the
   single `set-title-with-broadcast!` mutation point, and the async auto-title
   side-channel (an off-surface `ask!` that names a session on its first
   real turn). A LEAF — depends only on persistance + svar + runtime-settings,
   never back on the loop."
  (:require
   [clojure.string :as str]
   [com.blockether.svar.core :as svar]
   [com.blockether.vis.internal.persistance :as persistance]
   [com.blockether.vis.internal.runtime-settings :as rt]
   [com.blockether.vis.internal.strutil :refer [truncate]]
   [taoensso.telemere :as tel]))

(defonce ^:private title-listeners
  ;; {session-id-uuid #{listener-fn ...}}
  (atom {}))

(defn add-title-listener!
  "Register `listener-fn` for `session-id`. The fn is invoked with
   the new title (a string) every time the title changes. Multiple
   listeners are supported; they fire in unspecified order.

   Returns the listener fn so callers can pass it to
   `remove-title-listener!` later."
  [session-id listener-fn]
  (let [cid (persistance/->uuid session-id)]
    (swap! title-listeners update cid (fnil conj #{}) listener-fn))
  listener-fn)

(defn remove-title-listener!
  "Deregister a previously added listener. Idempotent."
  [session-id listener-fn]
  (let [cid (persistance/->uuid session-id)]
    (swap! title-listeners update cid
      (fn [existing] (disj (or existing #{}) listener-fn))))
  nil)

(defonce ^:private global-title-listeners
  ;; #{listener-fn ...} - fns of [session-id-uuid title], fired on EVERY
  ;; session's title change (the per-session listeners above stay scoped).
  (atom #{}))

(defn add-global-title-listener!
  "Register `listener-fn` to observe title changes across ALL sessions.
   The fn is invoked with the session id (a UUID) and the new title
   every time ANY session's title changes - host rename and auto-title
   generation alike, since they all funnel through `set-title-with-broadcast!`.

   Returns the listener fn so callers can pass it to
   `remove-global-title-listener!` later."
  [listener-fn]
  (swap! global-title-listeners conj listener-fn)
  listener-fn)

(defn remove-global-title-listener!
  "Deregister a previously added global title listener. Idempotent."
  [listener-fn]
  (swap! global-title-listeners disj listener-fn)
  nil)

(defn- broadcast-title-change!
  "Fire every registered listener for `session-id` with `title`, then
   every GLOBAL listener with `(session-id title)`. Listeners that throw
   are swallowed and logged - a misbehaving channel must NOT block the
   iteration loop."
  [session-id title]
  (let [cid (persistance/->uuid session-id)]
    (doseq [f (get @title-listeners cid)]
      (try (f title)
        (catch Throwable t
          (tel/log! {:level :warn :id ::title-listener-failed
                     :data {:session-id cid
                            :error (ex-message t)}
                     :msg (str "Title listener threw: " (ex-message t))}))))
    (doseq [f @global-title-listeners]
      (try (f cid title)
        (catch Throwable t
          (tel/log! {:level :warn :id ::global-title-listener-failed
                     :data {:session-id cid
                            :error (ex-message t)}
                     :msg (str "Global title listener threw: " (ex-message t))}))))))

;; ── Title-PENDING listeners ────────────────────────────────────────────────
;; A separate channel from the title VALUE listeners above: this one fires
;; `true` when host auto-title generation STARTS and `false` when it ends,
;; so a channel can show a "generating title…" spinner. Kept distinct so the
;; pending signal never gets confused with a real title string.

(defonce ^:private title-pending-listeners
  ;; {session-id-uuid #{listener-fn ...}}
  (atom {}))

(defn add-title-pending-listener!
  "Register `listener-fn` for `session-id`; invoked with a boolean
   (true when title generation starts, false when it ends). Returns the
   listener fn for later `remove-title-pending-listener!`."
  [session-id listener-fn]
  (let [cid (persistance/->uuid session-id)]
    (swap! title-pending-listeners update cid (fnil conj #{}) listener-fn))
  listener-fn)

(defn remove-title-pending-listener!
  "Deregister a previously added pending listener. Idempotent."
  [session-id listener-fn]
  (let [cid (persistance/->uuid session-id)]
    (swap! title-pending-listeners update cid
      (fn [existing] (disj (or existing #{}) listener-fn))))
  nil)

(defn- broadcast-title-pending!
  "Fire every pending listener for `session-id` with `pending?`.
   Listeners that throw are swallowed and logged."
  [session-id pending?]
  (let [cid (persistance/->uuid session-id)]
    (doseq [f (get @title-pending-listeners cid)]
      (try (f (boolean pending?))
        (catch Throwable t
          (tel/log! {:level :warn :id ::title-pending-listener-failed
                     :data {:session-id cid
                            :error (ex-message t)}
                     :msg (str "Title-pending listener threw: " (ex-message t))}))))))

(defn set-title-with-broadcast!
  "Single mutation point for session titles.

   1. Writes the title to the persisted `session_state` row.
   2. Updates the env's in-memory `:session-title-atom` so the next iteration's
      `:session-title-atom` mirror sees the new value AND so a
      read from the Python sandbox returns the fresh string immediately,
      without a DB round-trip.
   3. Broadcasts to every registered listener.

   `session-title-atom` may be nil (host-driven path with no live env)."
  [db-info session-id session-title-atom title]
  (let [t (str title)]
    (persistance/db-update-session-title! db-info session-id t)
    (when session-title-atom (reset! session-title-atom t))
    (broadcast-title-change! session-id t)
    nil))

(def ^:private AUTO_TITLE_MAX_CHARS 80)
(def ^:private AUTO_TITLE_TTFT_MS 15000)
(def ^:private AUTO_TITLE_IDLE_MS 10000)
(def ^:private AUTO_TITLE_SEMANTIC_MS 30000)

(def ^:private AUTO_TITLE_HARD_DEADLINE_MS
  "Absolute wall-clock cap on the whole auto-title provider attempt. svar's
   soft ttft/idle/semantic timeouts have been observed NOT to fire on some
   transport hangs (a socket that dies before headers, or trickles below the
   idle threshold) — a title future then parked for HOURS, well past its turn,
   which left the fallback (sequenced AFTER the call) unreachable and the tab
   untitled. This hard cap guarantees the call returns so the deterministic
   fallback that was already written up front stays put."
  45000)

(def ^:private auto-title-placeholder-labels
  #{"untitled" "untitled session"})

(defn- auto-title-placeholder?
  [s]
  (contains? auto-title-placeholder-labels
    (str/lower-case (str/trim (str s)))))

(defn- usable-existing-title
  [s]
  (let [t (some-> s str str/trim not-empty)]
    (when-not (auto-title-placeholder? t)
      t)))

(defn- strip-code-fence
  "Drop a surrounding Markdown code fence so a model that answers with
   ```text\n<title>\n``` doesn't leak the fence info-string (`text`) as the
   title. Returns the inner body when fenced, else the input unchanged."
  [s]
  (let [t (str/trim (or s ""))]
    (if (str/starts-with? t "```")
      (-> t
        (str/replace #"(?s)\A```[^\n]*\n?" "")
        (str/replace #"\n?```\s*\z" "")
        str/trim)
      t)))

(defn- sanitize-auto-title
  [s]
  (let [line (->> (-> (or s "") str strip-code-fence str/split-lines)
               (map str/trim)
               (remove str/blank?)
               ;; skip any stray fence markers left mid-string
               (remove #(re-matches #"`{3,}.*" %))
               first
               (#(or % ""))
               (#(-> %
                   (str/replace #"(?i)^\s*(title|new title)\s*[:\-–—]\s*" "")
                   (str/replace #"^[\s\"'`*_#>\-–—]+" "")
                   (str/replace #"[\s\"'`*_#>\-–—.]+$" "")
                   str/trim)))
        clipped (truncate line AUTO_TITLE_MAX_CHARS)]
    (when-not (or (str/blank? clipped)
                (auto-title-placeholder? clipped))
      clipped)))

(def ^:private uuid-text-pattern
  #"(?i)\b[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\b")

(defn- fallback-auto-title
  "Cheap deterministic title when the auto-title LLM route fails. Keeps TUI
   sessions from staying `Untitled` just because the cheapest routed model was
   unavailable/unsupported."
  [user-request]
  (let [words (->> (str/replace (str user-request) uuid-text-pattern " ")
                (re-seq #"[\p{L}\p{N}][\p{L}\p{N}'-]*")
                (take 7)
                (str/join " "))]
    (sanitize-auto-title words)))

(def ^:private auto-title-spec
  "Structured-output spec for the title side-channel. Using `ask!` + spec (SAP
   JSON parsing) instead of `ask-code!` fence extraction removes a whole class
   of bugs: a model that wraps its answer in a text code fence no longer leaks
   the fence info-string as the title — SAP reads the JSON title field
   regardless of surrounding markdown."
  (svar/spec
    (svar/field svar/NAME :title
      svar/TYPE svar/TYPE_STRING
      svar/CARDINALITY svar/CARDINALITY_ONE
      svar/DESCRIPTION "Short session title: 3-7 words, a stable noun phrase, no surrounding quotes or markdown.")))

(defn- auto-title-prompt
  [previous-title user-request]
  [{:role "system"
    :content (str "You generate short chat/session titles. 3-7 words, a stable noun phrase. "
               "Use the user's latest request and the previous title. If the previous title "
               "still fits, keep it unchanged; otherwise update it to reflect the new focus.")}
   {:role "user"
    :content (str "Previous title: " (or (not-empty previous-title) "<none>")
               "\nLatest user request:\n" user-request)}])

(def ^:private AUTO_TITLE_PROVIDER_ORDER
  "Preferred provider order for the auto-title side-channel. These are all
   flat-fee coding-plan subscriptions, so per-token `:cost` is the WRONG lens
   (it would dodge the metered-but-actually-free plans toward $0 Copilot).
   Instead we pin each plan in this deliberate order and pick its SMALLEST
   model (`{:provider p :optimize [:cost :speed]}` selects the cheapest +
   fastest model WITHIN the pinned provider: glm on zai, gpt-5.3-codex on
   codex, haiku on anthropic, a mini on copilot). Any configured provider not
   listed here is appended afterwards so the chain still covers the whole
   fleet. First provider that returns a usable title wins; on failure
   (model unavailable / endpoint rejects) we fall through to the next."
  [:zai-coding-plan
   :openai-codex
   :anthropic-coding-plan
   :github-copilot-individual
   :github-copilot-business
   :github-copilot-enterprise])

(defn- model-auto-title!
  "Generate a session title off the model's visible surface. A single `ask!`
   declares the preferred plan order via `:prefer-providers`; svar walks it
   natively (cheapest+fastest model per plan, falling through on
   model-unsupported / transient failure) so there is no host-side provider
   loop. The whole call is wrapped in a HARD wall-clock deadline
   (`AUTO_TITLE_HARD_DEADLINE_MS`) because svar's soft timeouts have been seen
   to miss some transport hangs. Returns nil (loudly — at `:warn`, so failures
   are visible, not a silent `:debug`) when the chain fails or the deadline
   trips; the caller then keeps the deterministic fallback."
  [{:keys [router]} previous-title user-request]
  (let [fut     (future
                  (try
                    {:ok (svar/ask! router
                           (rt/with-default-ask-code-idle-timeout
                             {:messages            (auto-title-prompt previous-title user-request)
                              :spec                auto-title-spec
                              :reasoning           :off
                              :routing             {:prefer-providers AUTO_TITLE_PROVIDER_ORDER
                                                    :optimize         [:cost :speed]}
                              :ttft-timeout-ms     AUTO_TITLE_TTFT_MS
                              :idle-timeout-ms     AUTO_TITLE_IDLE_MS
                              :semantic-timeout-ms AUTO_TITLE_SEMANTIC_MS}))}
                    (catch Throwable t {:error t})))
        outcome (deref fut AUTO_TITLE_HARD_DEADLINE_MS ::deadline)]
    (cond
      (= ::deadline outcome)
      (do (future-cancel fut)
          (tel/log! {:level :warn
                     :id ::auto-title-deadline
                     :data {:deadline-ms AUTO_TITLE_HARD_DEADLINE_MS
                            :providers   (vec AUTO_TITLE_PROVIDER_ORDER)}}
            "Auto-title provider call exceeded hard deadline; keeping deterministic fallback")
          nil)

      (:error outcome)
      (do (tel/log! {:level :warn
                     :id ::auto-title-call-failed
                     :data {:error     (ex-message (:error outcome))
                            :providers (vec AUTO_TITLE_PROVIDER_ORDER)}}
            "Auto-title provider chain failed; keeping deterministic fallback")
          nil)

      :else
      (sanitize-auto-title (some-> outcome :ok :result :title)))))

(defonce ^:private provisional-title-sessions
  ;; #{session-id-uuid ...} — sessions whose CURRENT persisted title is a
  ;; deterministic FALLBACK (first-words of the request), not an LLM title.
  ;; These stay eligible for ONE more LLM upgrade on a later turn (once a
  ;; degraded/rate-limited provider chain recovers), unlike a real LLM title
  ;; which is generated once and then frozen. In-process only: a restart
  ;; re-freezes a provisional title (its provenance is not persisted — that
  ;; would need a `session_state` schema column).
  (atom #{}))

(defn- provisional-title?
  [session-id]
  (contains? @provisional-title-sessions (persistance/->uuid session-id)))

(defn- mark-provisional-title!
  [session-id provisional?]
  (let [cid (persistance/->uuid session-id)]
    (swap! provisional-title-sessions (if provisional? conj disj) cid))
  nil)

(defn- title-needs-generation?
  "True when the session has NO usable title, OR its current title is a
   PROVISIONAL deterministic fallback (eligible for one more LLM upgrade). A
   real LLM title is frozen and re-titling is skipped."
  [session-id session-title-atom]
  (or (not (usable-existing-title (some-> session-title-atom deref)))
    (provisional-title? session-id)))

(defn maybe-auto-title!
  "Title the session asynchronously on a normal LLM turn. Two-phase so a slow
   or hung provider can never leave the tab untitled:

   1. FALLBACK-FIRST — write a deterministic first-words title up front (it
      cannot hang), marked PROVISIONAL, so the tab is titled instantly.
   2. LLM UPGRADE — a hard-deadline-bounded `ask!` (`model-auto-title!`) that
      OVERWRITES the fallback with a real title and freezes it. On failure the
      provisional fallback stays and a LATER turn retries the upgrade.

   A real (non-provisional) LLM title is generated once and never regenerated.
   Titles are host-owned; there is no model-facing override. Returns a future
   or nil; callers intentionally do not wait."
  [{:keys [db-info session-id session-title-atom] :as env} user-request]
  (when (and db-info session-id (:router env)
          (title-needs-generation? session-id session-title-atom))
    (future
      ;; Announce "generating title" so a channel (TUI) can spinner the
      ;; tab; always clear it in the finally.
      (broadcast-title-pending! session-id true)
      (try
        ;; 1. Fallback-first: never leave the tab untitled while the LLM runs.
        (when-not (usable-existing-title (some-> session-title-atom deref))
          (when-let [fallback (fallback-auto-title user-request)]
            (set-title-with-broadcast! db-info session-id session-title-atom fallback)
            (mark-provisional-title! session-id true)))
        ;; 2. LLM upgrade: overwrite + freeze on success; keep fallback on fail.
        (when-let [title (model-auto-title! env nil user-request)]
          (set-title-with-broadcast! db-info session-id session-title-atom title)
          (mark-provisional-title! session-id false))
        (catch Throwable t
          (tel/log! {:level :warn
                     :id ::auto-title-update-failed
                     :data {:session-id session-id
                            :error (ex-message t)}}
            "Auto-title update failed; keeping existing title"))
        (finally
          (broadcast-title-pending! session-id false))))))
