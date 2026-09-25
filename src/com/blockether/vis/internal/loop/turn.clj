(ns com.blockether.vis.internal.loop.turn
  "One turn from user input to its persisted outcome.

   Prepares the turn context and routing, dispatches slash, bang and normal
   turns, and finalizes the turn result. `turn!` is the entry point."
  (:require [clojure.string :as str]
            [com.blockether.anomaly.core :as anomaly]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.contract.content :as content-contract]
            [com.blockether.vis.internal.attachment.audio-transcribe :as audio-transcribe]
            [com.blockether.vis.internal.attachment.core :as attachments]
            [com.blockether.vis.internal.attachment.storage :as attachment-storage]
            [com.blockether.vis.internal.channel.slash :as slash]
            [com.blockether.vis.internal.config.runtime-settings :as rt]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.context.engine :as ctx-engine]
            [com.blockether.vis.internal.context.loop :as ctx-loop]
            [com.blockether.vis.internal.context.prompt-templates :as prompt-templates]
            [com.blockether.vis.internal.council.core :as council]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.shell :as shell]
            [com.blockether.vis.internal.loop.compaction :as compaction]
            [com.blockether.vis.internal.loop.iteration :as iteration]
            [com.blockether.vis.internal.loop.router :as loop-router]
            [com.blockether.vis.internal.loop.transcript :as transcript]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.provider.service :as providers]
            [com.blockether.vis.internal.python.env :as env]
            [com.blockether.vis.internal.session.agents :as agents]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [com.blockether.vis.internal.session.goals :as goals]
            [com.blockether.vis.internal.session.model :as session-model]
            [com.blockether.vis.internal.session.titling :as titling]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel]))

(defn- slash-ctx-for-env
  "Build the slash dispatch ctx from a turn env. Pure data; carries
   the channel/session/workspace coordinates the slash handlers read."
  [env user-request]
  (let [db-info
        (:db-info env)

        state-id
        (or (:session/state-id env)
            (when db-info
              (some-> (:session-id env)
                      (persistance/db-latest-session-state-id db-info))))]

    (cond-> {:channel/id (or (:channel env) :tui)
             :session/id (:session-id env)
             :db-info db-info
             :command/raw user-request}
      state-id
      (assoc :session/state-id state-id)

      (:session-title-atom env)
      (assoc :session-title-atom (:session-title-atom env))

      (:workspace/id env)
      (assoc :workspace/id (:workspace/id env))

      (:workspace-atom env)
      (assoc :workspace-atom (:workspace-atom env)))))

(defn- slash-body->markdown
  "Project a slash body to Markdown without constructing renderer IR."
  [body]
  (cond (nil? body) nil
        (string? body) body
        (and (vector? body) (every? content-contract/block-valid? body)) (content/text-projection
                                                                           body)
        :else (pr-str body)))

(defn- slash-result->answer-markdown
  "Build the prose Markdown carried by a slash result's canonical prose block."
  [{:keys [result error reason]}]
  (cond result (let [title
                     (or (:slash/title result) "Slash handled")

                     body
                     (some-> (:slash/body result)
                             slash-body->markdown
                             str/trim
                             not-empty)]

                 (cond-> (str "**" title "**")
                   body
                   (str "\n\n" body)))
        error (str "**Slash failed**\n\n" error)
        reason (str "**Slash unavailable**\n\n" reason)
        :else "**Slash handled**"))

(defn- cost-with-route
  "Attach the selected root route to a turn cost map once, for both durable history
   and the public terminal result. Numeric cost remains the sum of actual serving routes."
  [cost model provider]
  (let [cost (or cost {})]
    (cond-> cost
      (and model (not (get cost "model")))
      (assoc "model" (str model))

      (and provider (not (get cost "provider")))
      (assoc "provider" (if (keyword? provider) (name provider) (str provider))))))

(defn- turn-store-opts
  [env user-request loop-opts]
  (cond-> {:parent-session-id (:session-id env)
           :user-request user-request
           :request-kind (or (:request-kind loop-opts) :user)
           :council-entry-id (:council-entry-id loop-opts)
           :status :running}
    (some? (:session-turn-id loop-opts))
    (assoc :session-turn-id (:session-turn-id loop-opts))))

(defn- run-slash-turn!
  "Persist a slash-only turn: one `session_turn_soul` + state + ONE
   synthetic `session_turn_iteration` whose forms vec carries the slash
   envelope at `:tag :user-slash`. The turn is marked :success without
   any LLM round-trip. Returns the same shape `iteration-loop` would
   have produced (so callers don't special-case slash turns).

   The synthetic iteration row is persisted for audit/history."
  [env user-request slash-result loop-opts]
  (let [db-info
        (:db-info env)

        ;; A slash turn never enters `iteration-loop` (the only path that streams
        ;; live `:progress` activity), so the zero-iterations live bubble otherwise
        ;; claims Vis is "calling the provider". Emit ONE `:slash` phase chunk BEFORE
        ;; the (possibly slow) local dispatch so the tracker renders
        ;; `Vis is running: /<name>` instead — a PURE command never touches a provider.
        on-chunk
        (or (:on-chunk loop-opts) (get-in loop-opts [:hooks :on-chunk]))

        slash-label
        (or (re-find #"^/\S+" (str/trim (str user-request))) (str/trim (str user-request)))

        _
        (when (fn? on-chunk)
          (try (on-chunk {:phase :slash :iteration 1 :slash slash-label})
               (catch Throwable t
                 (tel/log! {:level :warn
                            :id ::slash-progress-emit-failed
                            :data {:slash slash-label :error (ex-message t)}}))))

        turn-id
        (persistance/db-store-session-turn! db-info (turn-store-opts env user-request loop-opts))

        turn-pos
        (or (transcript/session-turn-position env turn-id) 1)]

    ;; Stamp turn-state so synthesize-scope returns the canonical
    ;; `t<N>/i1/f1` scope for any CTX mutations the slash emits.
    (ctx-loop/set-turn-state! env
                              :iteration-id nil
                              :session-turn-id turn-id
                              :user-request user-request
                              :turn-position turn-pos
                              :iteration 1
                              :form-idx 0)
    (let [scope
          (str "t" turn-pos "/i1/f1")

          envelope
          {:scope scope :tag :user-slash :src user-request}

          answer-md
          (slash-result->answer-markdown slash-result)

          ;; Persist the canonical cursor state just like a normal turn: gc-pass,
          ;; strip cursor metadata, then drop ephemerals before Nippy-encoding.
          ctx-snapshot
          (when-let [ca (:ctx-atom env)]
            (let [clean (compaction/durable-context-snapshot env @ca)]
              (reset! ca clean)
              clean))]

      (try (persistance/db-store-iteration! db-info
                                            {:session-turn-id turn-id
                                             :code user-request
                                             :forms [envelope]
                                             :duration-ms 0
                                             :llm-full-duration-ms 0
                                             :thinking ""
                                             :answer answer-md
                                             :llm-messages []
                                             :llm-returned-empty-code? false})
           (catch Throwable t
             (tel/log!
               {:level :warn :id ::slash-iter-persist-failed :data {:error (ex-message t)}})))
      (transcript/persist-turn-outcome! db-info
                                        turn-id
                                        {:content [(content/prose answer-md)]
                                         :iteration-count 1
                                         :duration-ms 0
                                         :status :success
                                         :prior-outcome :complete
                                         :ctx ctx-snapshot}
                                        (get-in loop-opts [:hooks :claim-terminal!]))
      {:session-turn-id turn-id
       :answer answer-md
       :iteration-count 1
       :duration-ms 0
       :status :success
       :slash slash-result
       :prior-outcome :complete})))

(defn- store-transcripts!
  "Fill in the words for every recording this turn was written down WITHOUT.

   Staging a recording only STARTS its transcript, so the attachment rows land with
   an empty `transcription` column; the prompt assembler waits for the words it
   quotes, but those words would die with the pass that made them and the stored row
   would stay empty for good. This joins that same content-keyed work OFF the turn
   thread and writes each transcript onto the row it belongs to.

   The index in `attachments` IS the stored `position`: the offload that produced the
   rows is a 1:1 `mapv`, and the insert numbers positions from the same vector - so
   this must be handed the attachments as the turn saw them, never the offloaded copy
   (offload drops `:base64`, which is half of a recording's identity).

   Best-effort: a transcript that never arrives simply leaves the column as it is."
  [db-info session-turn-id attachments]
  (when (and db-info
             session-turn-id
             (some #(attachments/audio-media-type? (:media-type %)) attachments))
    (future
      (try (doseq [[position before after]
                   (map vector
                        (range)
                        attachments
                        (audio-transcribe/transcribe-attachments attachments))

                   :when (str/blank? (str (:transcription before)))
                   :let [words
                         (not-empty (str (:transcription after)))]
                   :when words]

             (persistance/db-set-turn-attachment-transcription! db-info
                                                                session-turn-id
                                                                position
                                                                words
                                                                (:transcription-segments after)))
           (catch Throwable t
             (tel/log! {:level :warn
                        :id ::transcript-store-failed
                        :data {:error (ex-message t)}
                        :msg
                        "transcribed an attached recording but could not store its words"}))))))

(defn- run-normal-turn!
  "LLM round-trip path: store turn, run iteration-loop, persist
   the end-of-turn CTX snapshot, update the turn row with answer +
   tokens. `:hooks :prepare-result`, when supplied, transforms the result before
   terminal ownership and persistence, so the gateway's timeout diagnosis is durable.
   Called by `run-turn!` for a normal user message."
  [env user-request loop-opts]
  (let [;; Persist EVERY image the user attached to this turn as durable
        ;; `session_turn_attachment` BLOB bytes: INLINE uploads (web/API base64,
        ;; carried on `:user/attachments`) AND terminal-drop images (paths pasted
        ;; into the message, sniffed + loaded here via the same magic-byte scan
        ;; the assemble seam uses). Storing the bytes - not just the on-disk path
        ;; - lets resume + history re-render survive the source file moving or
        ;; being deleted. Best-effort: a scan failure never blocks the turn.
        disk-attachments
        (try (:attached (attachments/collect-user-images user-request
                                                         {:workspace-root (:workspace/root env)}))
             (catch Throwable t
               (tel/log!
                 {:level :warn :id ::turn-image-persist-scan-failed :data {:error (ex-message t)}})
               nil))

        ;; The transcript of a voice memo is persisted WITH the recording (the same
        ;; content-keyed pass the prompt reads), so the player in every surface can
        ;; open its transcript and a resumed session still has the words. It is only
        ;; REQUESTED here: the turn is being written down, not answered, so it stores
        ;; what a composer already made and starts what nobody asked for yet. The
        ;; waiting, if any is left to do, happens once — where the prompt is built.
        turn-attachments
        (audio-transcribe/request-attachments! (into (vec (:user/attachments env))
                                                     disk-attachments))

        session-turn-id
        (persistance/db-store-session-turn!
          (:db-info env)
          (cond-> (turn-store-opts env user-request loop-opts)
            (seq turn-attachments)
            (assoc :attachments (attachment-storage/offload-attachments turn-attachments))))

        _
        (store-transcripts! (:db-info env) session-turn-id turn-attachments)

        turn-position
        (transcript/session-turn-position env session-turn-id)

        _
        (ctx-loop/set-turn-state! env
                                  :session-turn-id session-turn-id
                                  :user-request user-request
                                  :turn-position (or turn-position 1)
                                  :iteration nil
                                  :form-idx nil
                                  :iteration-id nil)

        _
        (titling/maybe-auto-title! env user-request)

        goal-at-turn-start
        (let [sid
              (str (:session-id env))

              council-wake?
              (get-in (council/runtime (:db-info env) sid) [sid :wake?])]

          (if (or council-wake?
                  (some-> (:cancel-atom loop-opts)
                          deref)
                  (cancellation/cancelled? (:cancel-token loop-opts)))
            (goals/check-goal env)
            (goals/resume-for-user-turn! env)))

        result
        (try (let [raw
                   (iteration/iteration-loop env
                                             user-request
                                             (assoc loop-opts :session-turn-id session-turn-id))

                   result
                   (if-let [prepare-result (get-in loop-opts [:hooks :prepare-result])]
                     (let [prepared (prepare-result raw)]
                       (assoc prepared :status-id (loop-router/status->id (:status prepared))))
                     raw)]

               (goals/finish-turn! env
                                   goal-at-turn-start
                                   (:status result)
                                   (some-> (transcript/turn-error-data (:answer result))
                                           (get "message")))
               result)
             (catch Throwable t
               (goals/finish-turn! env goal-at-turn-start :error (ex-message t))
               (throw t)))

        ;; Deferred auto-title: only a successful foreground turn earns a cosmetic
        ;; provider call. Cancellation and failure must stay terminal without
        ;; starting new provider work. The call remains deferred so it cannot take
        ;; a rate-limited gateway's slot away from the user's request
        ;; (Blockether/vis#71). A no-op unless `titling.mode` is `llm`.
        _
        (when (and (= :success (:status result)) (nil? (goals/halt-result env goal-at-turn-start)))
          (titling/after-turn-auto-title! env user-request))

        ;; `prior_outcome` is a CHECKed column (`complete`/`cancelled`/`error`). A goal
        ;; halt or an empty-reply give-up ends the loop with `:status :success`, which
        ;; is not an outcome value: stored verbatim it failed every persist stage and
        ;; left the turn row `:running` forever (Blockether/vis#211).
        prior-outcome
        (some-> (:status result)
                {:cancelled :cancelled :error :error :success :complete})

        ;; Snapshot the CTX as it stands at end-of-turn. Run gc-pass first
        ;; so terminal-status entries past their TTL drop out of the live
        ;; tree before persistence; historical snapshots in earlier
        ;; session_turn_state rows still carry them (the archive store + the
        ;; persisted forms rows). The renderer stamps the cursor in fresh each
        ;; call; we drop the cursor before persisting because the next-turn
        ;; loader will derive a new cursor from the loop counters (cursor
        ;; is iter-local, not turn-local). Persisted Nippy-encoded to
        ;; session_turn_state.ctx in the same transaction that flips the
        ;; turn status, so live CTX = ctx on the latest turn-state for the
        ;; latest turn-soul of the session_state.
        ctx-snapshot
        (when-let [ca (:ctx-atom env)]
          (let [clean (compaction/durable-context-snapshot env @ca)]
            (reset! ca clean)
            clean))

        turn-content
        ;; A failed turn's fallback may be a raw provider value instead of answer
        ;; content. Persist the diagnostic reconstructed from its trace rather than
        ;; letting answer validation hide the failure or leave history blank.
        (transcript/failed-turn-content (:answer result) (:trace result))

        turn-error
        (or (transcript/turn-error-data (:answer result)) (transcript/turn-error-data turn-content))

        root-route
        (loop-router/resolve-effective-model (:router env))

        turn-cost
        (cost-with-route (:cost result) (:name root-route) (:provider root-route))

        persisted?
        (transcript/persist-turn-outcome! (:db-info env)
                                          session-turn-id
                                          (cond-> {:content turn-content
                                                   :iteration-count (:iteration-count result)
                                                   :duration-ms (:duration-ms result)
                                                   :status (or (:status result) :success)
                                                   :tokens (:tokens result)
                                                   :cost turn-cost
                                                   :prior-outcome prior-outcome
                                                   :ctx ctx-snapshot}
                                            turn-error
                                            (assoc :error turn-error))
                                          (get-in loop-opts [:hooks :claim-terminal!]))

        prompt-cache-completion
        (:prompt-cache-completion result)

        _prompt-cache-complete
        (when (and persisted? prompt-cache-completion)
          (transcript/persist-prompt-cache-state! env
                                                  (:provider prompt-cache-completion)
                                                  (:model prompt-cache-completion)
                                                  prompt-cache-completion))]

    (-> result
        (dissoc :prompt-cache-completion)
        (assoc :session-turn-id session-turn-id
               :prior-outcome prior-outcome))))

(defn- health-gated-router
  "ONE health gate for every routing entry point: demote unreachable LOCAL
   providers to the router's
   end (`providers/demote-unreachable-providers` — never throws) and
   log the demotion once. Returns `{:router r :demoted [ids]}`."
  [router where]
  (let [{:keys [demoted] :as gated} (providers/demote-unreachable-providers router)]
    (when (seq demoted)
      (tel/log! {:level :warn
                 :id ::unreachable-providers-demoted
                 :data {:demoted demoted :where where}
                 :msg "router health gate: unreachable local providers demoted to last resort"}))
    gated))

(defn- parse-bang
  "Parse a `!`/`!&` shell-sugar user message into `{:kind :run|:bg :cmd :id?}`,
   or nil when `text` is NOT a bang. `!<cmd>` invokes the shell tool's synchronous
   run op; `!&<cmd>` invokes its background op under an auto-generated resource id.
   A blank command (a bare `!`) is ordinary
   prose, so it returns nil and the message runs as a normal turn."
  [text]
  (when (string? text)
    (let [t (str/triml text)]
      (cond (str/starts-with? t "!&") (let [cmd (str/trim (subs t 2))]
                                        (when (seq cmd)
                                          {:kind :bg
                                           :cmd cmd
                                           :id (str "background-"
                                                    (subs (str (java.util.UUID/randomUUID)) 0 8))}))
            (str/starts-with? t "!") (let [cmd (str/trim (subs t 1))]
                                       (when (seq cmd) {:kind :run :cmd cmd}))))))

(defn- bang-stdout
  "The one stdout value for a `!`/`!&` form: command bytes followed by its factual
   completion line. There is no parallel headline field."
  [kind id result]
  (when (map? result)
    (let [out
          (when (some? (get result "out")) (str (get result "out")))

          status
          (cond (= kind :bg) (if (get result "already_running")
                               (str "Background shell " id " was already running")
                               (str "Started background shell " id))
                (get result "timed_out") (str "Timed out"
                                              (when-let [seconds (get result "timeout_secs")]
                                                (str " after " seconds "s")))
                (some? (get result "exit")) (str "exit " (get result "exit"))
                :else (util/non-blank (get result "status")))]

      (cond (and (some? out) status)
            (str out (when (and (seq out) (not (str/ends-with? out "\n"))) "\n") status)
            (some? out) out
            status status))))

(defn- run-bang-turn!
  "LLM-free `!`/`!&` shell-sugar turn: run the shell tool directly, then persist
   and stream one ordinary form. Its canonical output is `:stdout`; the shell
   result map and any rendered card are not copied beside it. The empty answer
   keeps the form itself as the one visible result."
  [env user-request {:keys [kind cmd id]} loop-opts]
  (let [db-info
        (:db-info env)

        turn-id
        (persistance/db-store-session-turn! db-info (turn-store-opts env user-request loop-opts))

        turn-pos
        (or (transcript/session-turn-position env turn-id) 1)

        _
        (ctx-loop/set-turn-state! env
                                  :iteration-id nil
                                  :session-turn-id turn-id
                                  :user-request user-request
                                  :turn-position turn-pos
                                  :iteration 1
                                  :form-idx 0)

        enabled?
        (toggles/enabled? "shell")

        tool-name
        "shell"

        block-code
        (if (= kind :bg)
          (str "await shell({\"command\": " (pr-str cmd) ", \"wait\": 0, \"id\": " (pr-str id) "})")
          (str "await shell({\"command\": " (pr-str cmd) "})"))

        t0
        (util/now-ms)

        ;; Run the core shell implementation without introducing a compile-time cycle.
        ;; The `shell` toggle gate is applied HERE because a direct var call bypasses
        ;; the symbol's activation predicate.
        on-chunk
        (or (:on-chunk loop-opts) (get-in loop-opts [:hooks :on-chunk]))

        ;; This local turn never enters `iteration-loop`, so publish the same form
        ;; lifecycle normal execution does. The shell phase names the live wait;
        ;; the form start gives the eventual stdout a stable owner.
        _
        (when (and enabled? (fn? on-chunk))
          (try (on-chunk {:phase (if (= kind :bg) :shell-bg :shell-run) :iteration 1 :cmd cmd})
               (on-chunk {:phase :form-start
                          :iteration 1
                          :position 0
                          :count 1
                          :code block-code
                          :display-code cmd
                          :display-language "bash"
                          :tag :user-shell})
               (catch Throwable t
                 (tel/log! {:level :warn
                            :id ::bang-progress-emit-failed
                            :data {:cmd cmd :error (ex-message t)}}))))

        envelope
        (when enabled?
          (try (let [shell-fn (if (= kind :bg)
                                shell/shell
                                ;; A `!cmd` bang PRINTS the command's output, so it is the one
                                ;; caller that genuinely blocks. The tool no longer takes a wait
                                ;; knob — waiting is a handle method — so the bang path calls the
                                ;; INTERNAL blocking runner directly instead of a request flag.
                                shell/run-blocking)]
                 ;; Calling the shell var directly skips the symbol-call seam, so the
                 ;; workspace view stays unbound and `resolve-dir` falls back to the
                 ;; PROCESS cwd — a bang inside a draft would then run on trunk.
                 (extension/with-context
                   {:env env}
                   (if (= kind :bg) (shell-fn env cmd {"id" id}) (shell-fn env cmd {}))))
               (catch Throwable t
                 (tel/log!
                   {:level :warn :id ::bang-run-threw :data {:cmd cmd :error (ex-message t)}})
                 {:error {:message (or (ex-message t) (str t))}})))

        t1
        (util/now-ms)

        result-map
        (:result envelope)

        err
        (:error envelope)

        stdout
        (bang-stdout kind id result-map)

        ;; Disabled commands have no executed form to explain the refusal. Every
        ;; executed outcome is already visible on its form, including errors.
        answer-md
        (if-not enabled?
          (str "**Shell layer is OFF.** Only you can enable it: settings dialog"
               " → 'Shell commands'. Then `"
               cmd
               "` will run.")
          "")

        block
        (cond-> {:code block-code
                 :svar/tool-call-id (str "bang-" (subs (str (java.util.UUID/randomUUID)) 0 8))
                 :vis/tool-name tool-name
                 :op tool-name
                 :envelope {:started-at-ms t0 :finished-at-ms t1}}
          (some? stdout)
          (assoc :stdout stdout)

          (some? err)
          (assoc :error err))

        ;; A bang is an ordinary visible form. The authored source display names
        ;; the shell language without storing a rendered copy of its output.
        forms
        (mapv #(assoc %
                 :tag :user-shell
                 :display-code cmd
                 :display-language "bash")
              (ctx-engine/blocks->forms [block] {:turn turn-pos :iter 1} nil))

        _output
        (when (and enabled? (fn? on-chunk))
          (try (on-chunk (cond-> {:phase :form-result
                                  :iteration 1
                                  :position 0
                                  :count 1
                                  :code block-code
                                  :display-code cmd
                                  :display-language "bash"
                                  :tag :user-shell
                                  :error err
                                  :envelope (:envelope block)}
                           (some? stdout)
                           (assoc :stdout stdout)))
               (catch Throwable t
                 (tel/log! {:level :warn
                            :id ::bang-output-emit-failed
                            :data {:cmd cmd :error (ex-message t)}}))))

        ;; Snapshot CTX like run-slash-turn! / run-normal-turn! so resume is stable.
        ctx-snapshot
        (when-let [ca (:ctx-atom env)]
          (let [clean (compaction/durable-context-snapshot env @ca)]
            (reset! ca clean)
            clean))]

    (try (persistance/db-store-iteration! db-info
                                          {:session-turn-id turn-id
                                           :code user-request
                                           :forms forms
                                           :duration-ms (- t1 t0)
                                           :llm-full-duration-ms 0
                                           :thinking ""
                                           :answer answer-md
                                           :llm-messages []
                                           :llm-returned-empty-code? false})
         (catch Throwable t
           (tel/log! {:level :warn :id ::bang-iter-persist-failed :data {:error (ex-message t)}})))
    (transcript/persist-turn-outcome! db-info
                                      turn-id
                                      {:content (cond-> []
                                                  (seq answer-md)
                                                  (conj (content/prose answer-md)))
                                       :iteration-count 1
                                       :duration-ms (- t1 t0)
                                       :status :success
                                       :prior-outcome :complete
                                       :ctx ctx-snapshot}
                                      (get-in loop-opts [:hooks :claim-terminal!]))
    {:session-turn-id turn-id
     :answer answer-md
     :iteration-count 1
     :duration-ms (- t1 t0)
     :status :success
     :prior-outcome :complete}))

(defn run-turn!
  "Store turn -> iteration-loop -> update turn -> return result.

   Derives `:prior-outcome` (one of `:complete`, `:cancelled`, `:error`)
   from the loop result and
   persists it on the `session_turn_state` row. The next turn's
   `<system_state>` digest reads it.

   BEFORE the LLM round-trip, every turn is passed through
   `slash/dispatch`. When the user-message resolves
   to a registered slash, the turn is fully handled by a synthetic
   iteration (`tag :user-slash`) and the LLM is never called. The
   transcript still shows the user message + the slash envelope.

   A slash NO extension claims (`:reason :unknown`) gets one more
   chance as a PROMPT TEMPLATE (`.vis/prompts/*.md`, `~/.vis/prompts`,
   provider-contributed templates like `/<name>`): when a
   template matches, the expanded text runs as a NORMAL LLM turn.
   Registered slashes always win over templates."
  [env user-request loop-opts]
  (when-not (map? env) (throw (ex-info "run-turn! requires an env map" {:got (type env)})))
  (when (clojure.string/blank? user-request)
    (throw (ex-info "run-turn! requires a non-blank user request" {:got user-request})))
  (let [;; Re-resolve the active workspace from the session's CURRENT pin so a
        ;; model-managed workspace transition takes effect in the same turn. The cached
        ;; env was built at session start; without this the agent keeps editing the old root.
        env
        (or (when-let [db (:db-info env)]
              (when-let [sid (or (:session/state-id env)
                                 (some->> (:session-id env)
                                          (persistance/db-latest-session-state-id db)))]
                (when-let [ws (persistance/db-workspace-for-session db sid)]
                  ;; Keep the sandbox confinement's live pointer in step —
                  ;; sandbox-roots-fn derefs this on every real-fs access.
                  (some-> (:workspace-atom env)
                          (reset! ws))
                  (assoc env
                    :workspace ws
                    :workspace/id (:id ws)
                    :workspace/root (:root ws)))))
            env)

        ;; Turn-start health gate: probe LOCAL providers (Ollama/LM Studio)
        ;; and sink unreachable ones to the END of this turn's router, so a
        ;; dead local endpoint can't catch the turn or an svar fallback.
        ;; The demotion is per-turn (the env binding is local, so a provider
        ;; that comes back reappears next turn) and raises an engine warning
        ;; so the user knows. Remote providers are not network-checked here.
        env
        (let [{:keys [router demoted]}
              (health-gated-router (:router env) :turn)

              env'
              (assoc env :router router)]

          (when (seq demoted)
            (when-let [ca (:ctx-atom env)]
              (swap! ca update
                "engine_warnings"
                (fnil conj [])
                {:code :provider-unreachable
                 :anchor ["session_routing"]
                 :message (str "Local provider(s) "
                               (str/join ", " (map name demoted))
                               " unreachable — demoted to last-resort for this turn.")})))
          env')

        slash-result
        (extension/with-context
          {:env env}
          (try (slash/dispatch env (slash-ctx-for-env env user-request) user-request)
               (catch Throwable t
                 (tel/log! {:level :warn
                            :id ::slash-dispatch-threw
                            :data {:user-request user-request :error (ex-message t)}})
                 {:handled? false})))]

    (if-let [bang (parse-bang user-request)]
      (run-bang-turn! env user-request bang loop-opts)
      (if (:handled? slash-result)
        (if-let [expansion (when (= :unknown (:reason slash-result))
                             (extension/with-context {:env env}
                                                     (try (prompt-templates/expand env user-request)
                                                          (catch Throwable t
                                                            (tel/log! {:level :warn
                                                                       :id ::template-expand-threw
                                                                       :data
                                                                       {:user-request user-request
                                                                        :error (ex-message t)}})
                                                            nil))))]
          (let [turn-env (if-let [root (:project-root expansion)]
                           (assoc env
                             :workspace/root root
                             :workspace (assoc (:workspace env) :root root))
                           env)]
            (extension/with-context {:env turn-env}
                                    (run-normal-turn! turn-env (:text expansion) loop-opts)))
          (if (and (= ["goal"] (:path slash-result))
                   (= :ok (get-in slash-result [:result :slash/status]))
                   (true? (get-in slash-result [:result :slash/data :goal-run?])))
            (run-normal-turn! env user-request loop-opts)
            (run-slash-turn! env user-request slash-result loop-opts)))
        (run-normal-turn! env user-request loop-opts)))))

(defn custom-bindings
  "Current custom sandbox bindings {sym -> value}."
  [env]
  (some-> (:state-atom env)
          deref
          :custom-bindings))

;; Prepare turn context

(defn- forced-routing-for-pref
  "svar routing that FORCES a per-session provider+model preference.

   Why this exists: `router-for-model` reorders the router's `:providers`
   VECTOR, but svar's default `:strategy :root` selection sorts candidates by
   each provider's `:priority` field (NOT vector order — see
   `svar…router/candidate-sort-key`). So a config where anthropic is
   `:priority 0` and zai is `:priority 1` ALWAYS routes to anthropic's root
   (opus) no matter how Vis reorders the vector — the per-session pick was
   silently ignored. The fix is to hand svar the EXACT model (force-model) /
   provider (force-provider), which it honors regardless of priority.

   Returns routing additions, validated against `router` so a stale pref
   degrades instead of throwing (resolve-routing throws on an unknown
   provider):
     - provider+model both present & valid -> {:provider <kw> :model <str>}
     - a provider NAMED but absent from `router` -> {} (see below)
     - model alone, owned by some provider -> {:model <str>}
       (force-model restricts candidates to providers that expose it)
     - otherwise -> {} (no override; default `:strategy :root` runs)

   A pin that NAMED a provider binds THAT provider or nothing. Model NAMES are not
   unique across vendors — GitHub Copilot serves `gpt-5.6-*` too — so falling through
   to the model-only branch handed a session pinned to OpenAI Codex over to Copilot,
   which took the conversation and 400d on a model that endpoint never had.

   `provider` accepts a string id (`\"zai-coding-plan\"`, as stored in the DB
   pref) or keyword; `model` is the model name string."
  [router provider model]
  (let [model
        (some-> model
                str
                str/trim
                not-empty)

        prov-kw
        (some-> provider
                name
                keyword)

        prov
        (when prov-kw (first (filter #(= (:id %) prov-kw) (:providers router))))

        owns?
        (fn [p]
          (and model (some #(= (:name %) model) (:models p))))]

    (cond (and model prov (owns? prov)) {:provider prov-kw :model model}
          ;; Named a provider this router cannot serve: force NOTHING.
          ;; `prepare-turn-context` turns that into a user-fixable failure rather
          ;; than a silent hop to another vendor sharing the model name.
          prov-kw {}
          (and model (some owns? (:providers router))) {:model model}
          :else {})))

(defn- router-with-pinned-model
  "Teach `router` about a session-pinned model its CONFIG does not list.

   Provider `:models` in config is a curated subset; the gateway deliberately
   accepts any model the provider's LIVE catalog exposes, and the model picker
   offers exactly those. `forced-routing-for-pref` however validates the pin
   against `:models`, so a live-catalog pick degraded to `{}` and the turn silently
   ran the DEFAULT model — the pick looked applied in the UI and never bound. svar
   `resolve-routing` throws on a model it does not know, so the pin has to be
   MATERIALISED instead: synthesize a minimal `{:name model}` entry on the pinned
   provider (the same shape catalog hydration produces) and let provider-level
   settings inherit as usual.

   Returns `router` unchanged when there is no pin, the provider is unknown, or it
   already lists the model."
  [router provider model]
  (let [model
        (some-> model
                str
                str/trim
                not-empty)

        pid
        (some-> provider
                name
                keyword)

        ps
        (vec (:providers router))

        idx
        (when (and model pid)
          (first (keep-indexed (fn [i p]
                                 (when (= (:id p) pid) i))
                               ps)))

        p
        (when idx (nth ps idx))]

    (if (and p (not (some #(= (:name %) model) (:models p))))
      (assoc router :providers (assoc ps idx (update p :models (fnil conj []) {:name model})))
      router)))

(defn- router-for-pinned-provider
  "Hoist `provider-id`'s entry to the router HEAD and renumber the fleet.

   `router-for-model` alone cannot do this: when two providers expose the SAME
   model name they tie on rank and the stable sort keeps config order. A session
    pinned to `github-copilot/gpt-5.4` therefore CALLED copilot (the
   forced `:routing` binds that) while `resolve-effective-model` read the head —
   openai-codex — so the turn card, the cost row and every provider-error card
   named (and PRICED) the wrong provider. Hoisting the pinned provider makes
   display/cost attribution agree with the call, and puts the pinned provider
   first in the fallback order — which needs the `:priority` renumbering too,
   since svar drops `:force-provider` on an auth fallback and re-sorts by
   priority alone."
  [router provider-id]
  (let [pid
        (some-> provider-id
                name
                keyword)

        ps
        (:providers router)]

    (if-let [p (and pid (first (filter #(= (:id %) pid) ps)))]
      (assoc router
        :providers (providers/reprioritize-providers (into [p] (remove #(= (:id %) pid)) ps)))
      router)))

(defn- prepare-turn-context
  "Validates inputs, resolves sandbox bindings, sets up atoms.
   Returns a map of all computed context needed for subsequent phases."
  [env messages opts]
  (let [{:keys [spec provider model max-context-tokens system-prompt debug? hooks cancel-token
                eval-timeout-ms reasoning-default reasoning-effort routing extra-body
                session-turn-id request-kind council-entry-id]
         :or {debug? false}}
        opts]
    (when-not (:db-info env)
      (anomaly/incorrect! "Invalid RLM environment" {:type :vis/invalid-env}))
    (when-not (and (vector? messages) (seq messages))
      (anomaly/incorrect!
        "messages must be a non-empty vector of message maps, e.g. [(svar/user \"...\")]"
        {:type :vis/invalid-messages :got (type messages)}))
    (when (and (some? eval-timeout-ms) (not (integer? eval-timeout-ms)))
      (anomaly/incorrect!
        ":eval-timeout-ms must be an integer (milliseconds)"
        {:type :vis/invalid-eval-timeout :got eval-timeout-ms :got-type (type eval-timeout-ms)}))
    (let [;; Per-session route preference is composer state. When the caller passes
          ;; no explicit provider OR model, use the provider+model pair the gateway
          ;; froze when this turn was submitted. A direct engine caller can still
          ;; provide its own pair (or a model-only override) in `opts`.
          session-pref (when (and (nil? provider) (nil? model) (:session-id env))
                         (session-model/model-of (:db-info env) (:session-id env)))
          ;; ONE canonical spelling of the pin from here on. `forced-routing-for-pref`
          ;; and `router-with-pinned-model` trim; `router-for-model` does NOT — so a
          ;; pref carrying stray whitespace (a hand-edited DB row, a client that pads
          ;; the field) BOUND the right model while the display/cost root fell back to
          ;; the pinned provider's first model: the turn card named a model the turn
          ;; never ran.
          model (some-> (or model (:model session-pref))
                        str
                        str/trim
                        not-empty)
          ;; A provider belongs only to the model it was supplied with. Never combine
          ;; a persisted provider with an explicit caller model: that creates a
          ;; synthetic pair and can silently degrade to config order. An explicitly
          ;; supplied provider + model is already one immutable turn snapshot.
          ;; Keywords survive as their bare name (`:lmstudio` -> "lmstudio"), never
          ;; as `":lmstudio"`.
          pref-provider (let [p (or provider (:provider session-pref))]
                          (some-> (if (keyword? p) (name p) p)
                                  str
                                  str/trim
                                  not-empty))
          ;; The pin the session actually BINDS (provider+model, validated against
          ;; the router). Computed once: it drives BOTH the display/cost root
          ;; (env-router below) and svar's forced `:routing`, so the two can never
          ;; name different providers again.
          ;; A pick may name a model only the provider's LIVE catalog lists (the
          ;; picker offers those); materialise it on the pinned provider or the pin
          ;; validates away and the turn silently runs the default model.
          pref-router
          (agents/restrict-router env (router-with-pinned-model (:router env) pref-provider model))
          pref-forced (forced-routing-for-pref pref-router pref-provider model)
          ;; The pin names a provider this router cannot serve — its build failed
          ;; (absent or expired credential) or it left the fleet. FAIL the turn:
          ;; `router-for-model` below would otherwise hoist whatever OTHER provider
          ;; lists the same model NAME and hand the conversation to a vendor the
          ;; user never picked.
          _ (when (and model pref-provider (nil? (:provider pref-forced)))
              (throw (ex-info (str "Session is pinned to "
                                   pref-provider
                                   "/"
                                   model
                                   ", but "
                                   pref-provider
                                   " is not available on this router — its credential could not be"
                                   " resolved. Re-authenticate it (`vis-agent providers auth "
                                   pref-provider
                                   "`) or pick another model.")
                              {:type :vis/pinned-provider-unavailable
                               :vis/user-error true
                               :provider pref-provider
                               :model model})))
          ;; Cancellation TOKEN carries the cooperative flag AND the
          ;; on-cancel! callback registry that hard-cancels Python /
          ;; provider futures. Callers create one via
          ;; `cancellation/cancellation-token` and pass it as
          ;; `:cancel-token`. The derived atom is the lower-level
          ;; primitive every poll site checks.
          cancel-token (or cancel-token (cancellation/cancellation-token))
          cancel-atom (cancellation/cancellation-atom cancel-token)
          ;; INLINE image uploads (web/API base64, no durable disk path):
          ;; validate here (magic-byte sniff + size/count caps) so BOTH the
          ;; assemble seam and turn persistence see the canonical
          ;; `{:attached :skipped}` shape.
          prepared-attachments (attachments/prepare-inline-attachments (:user/attachments opts))
          ;; `user-request` = ONLY the current turn's user message.
          ;; Prior dialog transcript is dropped here — one ask, one value.
          ;; Durable context flows through ctx and persisted iterations, not
          ;; by joining every message's content into one growing blob.
          extract-text (fn [c]
                         (cond (string? c) c
                               (sequential? c)
                               (str/join " " (keep #(when (= "text" (:type %)) (:text %)) c))
                               :else nil))
          ;; Locate the LAST user message once. It is the only human text
          ;; sent into this turn. Prior dialog transcript is intentionally
          ;; NOT replayed to the model; durable context flows through
          ;; persisted iterations, defs, SYSTEM vars, and DB-backed tools.
          last-user-idx (->> (map-indexed vector messages)
                             reverse
                             (some (fn [[i m]]
                                     (when (contains? #{"user" :user} (:role m)) i))))
          last-user-message (when last-user-idx (nth messages last-user-idx))
          user-request (or (some-> last-user-message
                                   :content
                                   extract-text)
                           ;; Fallback: no :user role found (malformed caller) -
                           ;; use the last message's text. Better than an empty user request.
                           (some-> messages
                                   last
                                   :content
                                   extract-text)
                           "")
          ;; A `:model` preference HOISTS that model to the router root for
          ;; DISPLAY + COST: `resolve-effective-model` reads the vector head, so
          ;; root-model/root-provider (and the persisted cost label) reflect the
          ;; pick. Blank/unknown names degrade to the config order.
          env-router (cond-> pref-router
                       (and model (not (str/blank? (str model))))
                       (loop-router/router-for-model model)

                       ;; …and a pinned PROVIDER hoists that provider, so a model
                       ;; name two providers share attributes to the one being called.
                       (:provider pref-forced)
                       (router-for-pinned-provider (:provider pref-forced)))
          root-resolved-model (when env-router (loop-router/resolve-effective-model env-router))
          root-model (or (:name root-resolved-model) model)
          root-provider (:provider root-resolved-model)
          root-provider-map (some #(when (= root-provider (:id %)) %) (:providers env-router))
          reasoning-effort-resolution (when (some? reasoning-effort)
                                        (svar/resolve-reasoning-effort
                                          (or (:api-style root-resolved-model)
                                              (:api-style root-provider-map))
                                          root-resolved-model
                                          reasoning-effort))
          _ (when (and (some? reasoning-effort) (nil? (:effective reasoning-effort-resolution)))
              (throw (ex-info (str "Reasoning effort " (pr-str reasoning-effort)
                                   " is unsupported for " (some-> root-provider
                                                                  name)
                                   "/" root-model
                                   "; accepted values: "
                                   (if (seq (:supported reasoning-effort-resolution))
                                     (str/join ", " (:supported reasoning-effort-resolution))
                                     "none"))
                              {:type :vis/unsupported-reasoning-effort
                               :vis/user-error true
                               :requested reasoning-effort
                               :provider root-provider
                               :model root-model
                               :supported (:supported reasoning-effort-resolution)
                               :resolution reasoning-effort-resolution})))
          ;; The hoisted router makes the selected model the FIRST attempt. Hybrid
          ;; fallback then wraps through the remaining fleet — including a configured
          ;; primary that preceded a manual pick — while an explicit caller policy wins.
          routing (let [merged (merge pref-forced (or routing {}))]
                    (cond-> merged
                      (and (loop-router/provider-fallback-allowed?)
                           (not (contains? merged :on-transient-error)))
                      (assoc :on-transient-error :hybrid)

                      (and root-provider
                           root-model
                           (not (contains? merged :provider))
                           (not (contains? merged :model)))
                      (merge (forced-routing-for-pref (:router env) root-provider root-model))))
          db-info (:db-info env)
          custom-bindings (custom-bindings env)
          ;; Forces the sandbox only when there is something to bind: a turn with
          ;; no custom bindings must not start an interpreter to install nothing.
          _ (doseq [[sym val] (or custom-bindings {})]
              (when val (env/set-python-binding! (env/python-context env) sym val)))
          ;; Workspace pin lives on the env itself (set in create-environment).
          ;; Opts may carry namespaced `:workspace/*` overrides for unusual
          ;; per-turn cases; the bare `:workspace` key is not accepted
          ;; (only :workspace/* namespaced keys flow through).
          ;; turn-state-atom already lives on env (one atom for all
          ;; per-turn cursor + id fields); no re-assoc needed.
          workspace-overrides (select-keys opts
                                           [:workspace/root :workspace/id :workspace/sandbox?
                                            :vcs/kind :vcs/ref :vcs/mainline])
          ;; Reseat :router to the preference-hoisted one — run-iteration-phase
          ;; routes off THIS environment's router, not the ctx :router below.
          environment (cond-> (assoc env
                                :router env-router
                                :user/attachments (:attached prepared-attachments)
                                :user/skipped-attachments (:skipped prepared-attachments))
                        (seq workspace-overrides)
                        (merge workspace-overrides)

                        ;; Refresh the routing digest HEAD
                        ;; (:model/:provider) to the per-turn pick so
                        ;; `context["routing"]` + the TUI footer reflect the
                        ;; session's chosen provider/model. The digest
                        ;; is built ONCE at env creation from the GLOBAL
                        ;; router head (the config default), so without
                        ;; this every turn's `:session/routing` showed
                        ;; the default provider (e.g. zai) even after
                        ;; the user switched models — the forced pref
                        ;; bound the actual call but never the displayed
                        ;; routing.
                        (and (seq (:routing env)) (or root-model root-provider))
                        (update :routing
                                (fn [r]
                                  (cond-> r
                                    root-model
                                    (assoc "model" (str root-model))

                                    root-provider
                                    (assoc "provider" (name root-provider))))))
          environment-id (:environment-id env)]

      {:cancel-token cancel-token
       :cancel-atom cancel-atom
       :user-request user-request
       :router env-router
       :root-resolved-model root-resolved-model
       :root-model root-model
       :root-provider root-provider
       :db-info db-info
       :environment environment
       :environment-id environment-id
       ;; The turn's canonical id, when the CALLER already froze one. The gateway mints
       ;; it in `submit-turn!` BEFORE the first event goes out, so every `turn.*` event,
       ;; every cancel and the forced-terminal backstop already name it. Dropping it here
       ;; made the durable row a SECOND id: the turn existed twice (gateway record +
       ;; persisted twin), and the trace behind the id the channels hold came back empty.
       :session-turn-id session-turn-id
       ;; Keep explicit request provenance through preparation and execution;
       ;; the stored Council entry owns the visible request, not the wake instruction.
       :request-kind request-kind
       :council-entry-id council-entry-id
       :spec spec
       :max-context-tokens max-context-tokens
       :system-prompt system-prompt
       :debug? debug?
       :hooks hooks
       :eval-timeout-ms eval-timeout-ms
       :reasoning-default reasoning-default
       :reasoning-effort (:effective reasoning-effort-resolution)
       :reasoning-effort-resolution reasoning-effort-resolution
       :routing routing
       :extra-body extra-body
       :turn-features (get opts :turn/features)
       :workspace-overrides workspace-overrides
       :messages messages})))

(defn- run-iteration-phase
  "Runs the main iteration loop via run-turn!.
   Returns iteration-result, session-turn-id, cost atoms, and merge-cost! fn."
  [{:keys [environment user-request spec max-context-tokens system-prompt hooks cancel-atom
           cancel-token reasoning-default reasoning-effort routing extra-body turn-features
           workspace-overrides session-turn-id request-kind council-entry-id]}]
  (let [iteration-result
        (run-turn! environment
                   user-request
                   (cond-> {:output-spec spec
                            :max-context-tokens max-context-tokens
                            :system-prompt system-prompt
                            :reasoning-default reasoning-default
                            :reasoning-effort reasoning-effort
                            :hooks hooks
                            :cancel-atom cancel-atom
                            :cancel-token cancel-token
                            :request-kind request-kind
                            :council-entry-id council-entry-id}
                     session-turn-id
                     (assoc :session-turn-id session-turn-id)

                     routing
                     (assoc :routing routing)

                     extra-body
                     (assoc :extra-body extra-body)

                     turn-features
                     (assoc :turn-features turn-features)

                     (seq workspace-overrides)
                     (assoc :workspace-overrides workspace-overrides)))

        session-turn-id
        (:session-turn-id iteration-result)

        {iteration-tokens :tokens iteration-cost :cost}
        iteration-result

        total-tokens-atom
        (atom (or iteration-tokens {}))

        total-cost-atom
        (atom (or iteration-cost {}))

        merge-cost!
        (fn [extra-tokens extra-cost]
          (when extra-tokens
            (swap! total-tokens-atom (fn [acc]
                                       (merge-with +
                                                   acc
                                                   (select-keys extra-tokens
                                                                ["input" "output" "reasoning"
                                                                 "cached" "total"])))))
          (when extra-cost
            (swap! total-cost-atom (fn [acc]
                                     (loop-router/merge-cost-maps acc extra-cost)))))]

    {:iteration-result iteration-result
     :session-turn-id session-turn-id
     :total-tokens-atom total-tokens-atom
     :total-cost-atom total-cost-atom
     :merge-cost! merge-cost!}))

(defn- finalize-turn-result
  "Build the public terminal result after `run-turn!` performed the turn's one
   durable outcome write.

   `:provider` and `:model` are attached to the returned cost map so the web
   footer can render `provider/model / N iteration / duration / tokens / $total`."
  [{:keys [root-model root-provider reasoning-effort]}
   {:keys [start-time iteration-count status status-id trace locals answer confidence reasoning
           utilization total-tokens-atom total-cost-atom]}]
  (let [duration-ms
        (/ (- (System/nanoTime) (long start-time)) 1e6)

        eval-evidence
        (transcript/turn-eval-evidence reasoning-effort trace)

        cost-with-model
        (cost-with-route @total-cost-atom root-model root-provider)]

    (if status
      (do (iteration/log-stage!
            :turn/complete
            0
            {:duration-ms duration-ms :iteration-count iteration-count :status status})
          (let [fallback-answer (:result answer answer)]
            (cond-> {:answer fallback-answer
                     :status status
                     :status-id status-id
                     :trace trace
                     :iteration-count iteration-count
                     :duration-ms duration-ms
                     :tokens @total-tokens-atom
                     :cost cost-with-model}
              eval-evidence
              (assoc :eval eval-evidence)

              (some? locals)
              (assoc :locals locals))))
      (do (iteration/log-stage! :turn/complete
                                0
                                {:duration-ms duration-ms
                                 :iteration-count iteration-count
                                 :cost (str (get cost-with-model "total_cost"))})
          (cond-> {:answer answer
                   :trace trace
                   :iteration-count iteration-count
                   :duration-ms duration-ms
                   :tokens @total-tokens-atom
                   :cost cost-with-model
                   :utilization utilization}
            eval-evidence
            (assoc :eval eval-evidence)

            (some? confidence)
            (assoc :confidence confidence)

            (some? reasoning)
            (assoc :reasoning reasoning))))))

;; Public entry point

(defn turn!
  "Runs one session turn on an RLM environment using iterative LLM code evaluation.

    Params:
    `environment` - RLM environment from create-environment.
    `messages` - Vector of message maps. Always a vector, e.g.:
                 [(svar/user <prompt-text>)]
                 [(svar/user <prompt-text> (svar/image <b64> <mime-type>))]
   `opts` - Map, optional:
     - :spec - Output spec for structured answers.
     - :model - Override config's default model.
      - :max-context-tokens - Token budget for context.
      - :debug? - Enable verbose debug logging (default: false). Logs iteration details,
        code evaluation, LLM responses at :debug level with :rlm-phase context.
      - :reasoning-default - Optional base reasoning effort for reasoning-capable models.
        Accepts :low/:medium/:high or low/medium/high strings. Adaptive escalation still applies.
      - :reasoning-effort - Exact provider-native effort string, `high` or `max`.
        Catalog-gated and threaded unchanged through every iteration.
      - :extra-body - Optional provider-specific request-body params merged into the
        upstream LLM call after auto max_tokens + reasoning translation.
      - :request-kind - Request origin, :user (default) or :council.
      - :council-entry-id - Required immutable Council entry id for :council requests.

    Returns:
   Map with:
      - :trace - Vector of iteration trace entries, each containing:
          {:iteration N
           :response <llm-response-text>
           :blocks [{:id 0 :code <code-str> :stdout <printed-text> :error nil
                     :envelope {:started-at-ms 10 :finished-at-ms 15 ...}}
                       ...]}
     - :iteration-count - Number of iterations used.
     - :duration-ms - Turn duration in milliseconds.
     - :tokens - Token usage map {\"input\" N \"output\" N \"total\" N} (canonical string keys).
     - :cost - Cost map {\"input_cost\" N \"output_cost\" N \"total_cost\" N} (canonical string keys).
     - :confidence - Confidence level (:high/:medium/:low) from final iteration.
      - :reasoning - String summary of how the answer was derived (from LLM's FINAL call).
      - :status - Only present on failure (`:error` or `:cancelled`)."
  ([environment messages] (turn! environment messages {}))
  ([environment messages opts]
   (let [ctx
         (prepare-turn-context environment messages opts)

         {:keys [eval-timeout-ms debug? user-request root-model db-info environment-id]}
         ctx]

     (binding [rt/*rlm-context*
               {:rlm-environment-id environment-id
                :rlm-type :main
                :rlm-debug? debug?
                :rlm-phase :turn
                :db-info db-info
                :session-soul-id (:session-id environment)}

               rt/*eval-timeout-ms*
               (rt/clamp-eval-timeout-ms (or eval-timeout-ms rt/*eval-timeout-ms*))]

       (tel/with-ctx+
         {:db-info db-info :session-soul-id (:session-id environment)}
         (iteration/log-stage!
           :turn/open
           0
           {:model root-model
            :reasoning? (boolean (:reasoning? (first (mapcat :models
                                                             (:providers (:router environment))))))
            :user-request user-request})
         (let [start-time
               (System/nanoTime)

               phase2
               (run-iteration-phase ctx)

               {:keys [iteration-result session-turn-id total-tokens-atom total-cost-atom]}
               phase2

               {iteration-answer :answer
                trace :trace
                iteration-count :iteration-count
                status :status
                status-id :status-id
                locals :locals
                confidence :confidence
                reasoning :reasoning}
               iteration-result

               result
               (if status
                 (finalize-turn-result ctx
                                       {:session-turn-id session-turn-id
                                        :start-time start-time
                                        :iteration-count iteration-count
                                        :status status
                                        :status-id status-id
                                        :trace trace
                                        :locals locals
                                        :answer iteration-answer
                                        :total-tokens-atom total-tokens-atom
                                        :total-cost-atom total-cost-atom})
                 (finalize-turn-result ctx
                                       {:session-turn-id session-turn-id
                                        :start-time start-time
                                        :iteration-count iteration-count
                                        :trace trace
                                        :answer iteration-answer
                                        :confidence confidence
                                        :reasoning reasoning
                                        :utilization (:utilization iteration-result)
                                        :total-tokens-atom total-tokens-atom
                                        :total-cost-atom total-cost-atom}))]

           result))))))
