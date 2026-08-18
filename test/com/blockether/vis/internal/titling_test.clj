(ns com.blockether.vis.internal.titling-test
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.titling :as titling]
            [lazytest.core :refer [defdescribe it expect]]))

;; The interesting fns are private; reach them the way loop-test does.
(defn- with-titling-cfg
  "Run `f` with the `titling:` config block pinned, so a test never depends on
   the operator's own `~/.vis` config."
  [cfg f]
  (with-redefs-fn {#'titling/titling-config (constantly cfg)} f))

(defn- maybe-auto-title!
  "One full titling round the way the loop drives it: the LOCAL turn-start pass
   first (all `titling/maybe-auto-title!` does now), then the deferred LLM
   upgrade. Waits for the returned future so the redefs stay in force for the
   whole pass."
  [env user-request]
  (with-titling-cfg {}
                    (fn []
                      (titling/maybe-auto-title! env user-request)
                      (let [f (titling/after-turn-auto-title! env user-request)]
                        (when f @f)
                        f))))

(def ^:private provisional-title? (deref #'titling/provisional-title?))

(defn- env*
  [sid title-atom]
  {:db-info :db
   :session-id sid
   :session-title-atom title-atom
   :router {:providers [{:id :zai-coding-plan :models [{:name "glm-5-turbo"}]}]}})

(defn- fresh-sid [] (str (java.util.UUID/randomUUID)))

(defdescribe
  auto-title-two-phase-test
  (it "writes the deterministic fallback FIRST, then upgrades to the LLM title"
      (let [sid
            (fresh-sid)

            title*
            (atom "")

            writes
            (atom [])]

        (with-redefs [titling/set-title-with-broadcast!
                      (fn [_ _ a t]
                        (swap! writes conj t)
                        (reset! a t))

                      svar/ask!
                      (fn [_ _]
                        {:result {:title "REPL Architecture Deep Dive"}})]

          @(maybe-auto-title! (env* sid title*) "I want to discuss the current approach to REPLs")
          ;; fallback landed BEFORE the model title — the tab is never untitled
          (expect (= ["I want to discuss the current approach to REPLs"
                      "REPL Architecture Deep Dive"]
                     @writes))
          (expect (= "REPL Architecture Deep Dive" @title*))
          ;; a real LLM title is NOT provisional
          (expect (false? (provisional-title? sid))))))
  (it "REGRESSION: a provisional fallback is upgraded on a LATER turn once the chain recovers"
      ;; This is the 4f0f6ac1/b7f27b7b bug: turn 1's LLM title failed under a
      ;; degraded/rate-limited provider chain, wrote the crude fallback, and the
      ;; old guard then froze it forever. Now the fallback stays PROVISIONAL and a
      ;; later turn re-attempts the upgrade.
      (let [sid
            (fresh-sid)

            title*
            (atom "")]

        ;; turn 1 — provider chain fails → provisional fallback
        (with-redefs [titling/set-title-with-broadcast!
                      (fn [_ _ a t]
                        (reset! a t))

                      svar/ask!
                      (fn [_ _]
                        (throw (ex-info "429 rate limited" {})))]

          @(maybe-auto-title! (env* sid title*) "let us go over the ownership model now")
          (expect (= "let us go over the ownership model now" @title*))
          (expect (true? (provisional-title? sid))))
        ;; turn 2 — providers recovered → the guard ALLOWS a retry and upgrades
        (with-redefs [titling/set-title-with-broadcast!
                      (fn [_ _ a t]
                        (reset! a t))

                      svar/ask!
                      (fn [_ _]
                        {:result {:title "Python REPL Ownership"}})]

          (let [f (maybe-auto-title! (env* sid title*) "and now the ownership model")]
            (expect (some? f)) ; retry was NOT skipped
            @f
            (expect (= "Python REPL Ownership" @title*))
            (expect (false? (provisional-title? sid)))))))
  (it "a real LLM title is FROZEN: a later turn does not re-title (and never calls the provider)"
      (let [sid
            (fresh-sid)

            title*
            (atom "")]

        (with-redefs [titling/set-title-with-broadcast!
                      (fn [_ _ a t]
                        (reset! a t))

                      svar/ask!
                      (fn [_ _]
                        {:result {:title "Security Audit Setup"}})]

          @(maybe-auto-title! (env* sid title*) "please review the security audit setup for clj")
          (expect (= "Security Audit Setup" @title*)))
        (with-redefs [titling/set-title-with-broadcast!
                      (fn [_ _ _ _]
                        (throw (ex-info "must not re-title" {})))

                      svar/ask!
                      (fn [_ _]
                        (throw (ex-info "must not re-title" {})))]

          (expect (nil? (maybe-auto-title! (env* sid title*)
                                           "a totally different follow-up request")))
          (expect (= "Security Audit Setup" @title*)))))
  (it "a HUNG provider call trips the hard deadline and keeps the provisional fallback"
      (let [sid
            (fresh-sid)

            title*
            (atom "")

            blocker
            (promise)]

        (try (with-redefs [titling/set-title-with-broadcast!
                           (fn [_ _ a t]
                             (reset! a t))

                           titling/AUTO_TITLE_HARD_DEADLINE_MS
                           100

                           svar/ask!
                           (fn [_ _]
                             @blocker)]

               ; never returns
               @(maybe-auto-title! (env* sid title*) "hang test request words here for title")
               (expect (= "hang test request words here for title" @title*))
               (expect (true? (provisional-title? sid))))
             (finally (deliver blocker nil)))))
  ;; Regression, issue #b712ee2e: a message whose first lines are a pasted
  ;; image's `````vis-image` fence was titled after the clipboard TEMP FILE —
  ;; `/var/folders/.../T/clipboard-2026-08-07-130827-BCB` — instead of the words
  ;; the human typed under the picture.
  (it
    "REGRESSION: a pasted-image attachment fence never names the session"
    (let
      [sid
       (fresh-sid)

       title*
       (atom "")

       request
       (str
         "````vis-image\n" "[Image #1: clipboard-2026-08-07-130827-BCBBE597.png 1720×578, 87KB]\n"
         "/var/folders/67/5js7xvyn2t14v8zq9rrzb8m40000gn/T/clipboard-2026-08-07-130827-BCBBE597.png\n"
         "image/png\n1720x578\n87KB\n"
         "````\n" "Streaming was killed between the chunks. Please analyze it.")]

      (with-redefs [titling/set-title-with-broadcast!
                    (fn [_ _ a t]
                      (reset! a t))

                    svar/ask!
                    (fn [_ _]
                      (throw (ex-info "429 rate limited" {})))]

        @(maybe-auto-title! (env* sid title*) request)
        (expect (= "Streaming was killed between the chunks" @title*)))))
  (it
    "REGRESSION: a leading /new-session slash command is stripped so the title reflects the real prompt, not the command word"
    ;; The `/new-session <task>` composer action leaked its command word into
    ;; the titling request, so the tab was named after "session" instead of the
    ;; task. Both the LLM prompt and the deterministic fallback must see the
    ;; STRIPPED request.
    (let [sid
          (fresh-sid)

          title*
          (atom "")

          seen
          (atom nil)]

      (with-redefs [titling/set-title-with-broadcast!
                    (fn [_ _ a t]
                      (reset! a t))

                    svar/ask!
                    (fn [_ opts]
                      (reset! seen opts)
                      {:result {:title "JSON Parser Build"}})]

        @(maybe-auto-title! (env* sid title*) "/new-session build a json parser")
        (let [user-msg (-> @seen
                           :messages
                           second
                           :content)]
          (expect (str/includes? user-msg "build a json parser"))
          (expect (not (str/includes? user-msg "/new-session"))))
        (expect (= "JSON Parser Build" @title*))))
    ;; provider chain down → fallback titles off the STRIPPED request too
    (let [sid
          (fresh-sid)

          title*
          (atom "")]

      (with-redefs [titling/set-title-with-broadcast!
                    (fn [_ _ a t]
                      (reset! a t))

                    svar/ask!
                    (fn [_ _]
                      (throw (ex-info "429 rate limited" {})))]

        @(maybe-auto-title! (env* sid title*) "/new-session build a json parser")
        (expect (= "build a json parser" @title*))))))

(defdescribe
  titling-config-test
  (it "the LLM call ALWAYS lands past the foreground turn (Blockether/vis#71)"
      ;; The title `ask!` used to race the user's own request for the same
      ;; rate-limited gateway slot. Now the turn-start pass is LOCAL ONLY and
      ;; there is no config that puts it back in front of the user's request.
      (let [sid
            (fresh-sid)

            title*
            (atom "")

            asks
            (atom 0)

            request
            "please explain the deferred titling design"]

        (with-redefs [titling/set-title-with-broadcast!
                      (fn [_ _ a t]
                        (reset! a t))

                      svar/ask!
                      (fn [_ _]
                        (swap! asks inc)
                        {:result {:title "Deferred LLM Title"}})]

          (with-titling-cfg {}
                            (fn []
                              (expect (nil? (titling/maybe-auto-title! (env* sid title*) request)))
                              (expect (= request @title*))
                              (expect (zero? @asks)) ; no provider slot spent during the turn
                              (expect (true? (provisional-title? sid)))
                              ;; …and the upgrade happens once the turn is done
                              (let [f (titling/after-turn-auto-title! (env* sid title*) request)]
                                (expect (some? f))
                                @f
                                (expect (= 1 @asks))
                                (expect (= "Deferred LLM Title" @title*))
                                (expect (false? (provisional-title? sid)))))))))
  (it "mode first_sentence titles from the request itself and NEVER calls a provider"
      (let [sid
            (fresh-sid)

            title*
            (atom "")]

        (with-redefs [titling/set-title-with-broadcast!
                      (fn [_ _ a t]
                        (reset! a t))

                      svar/ask!
                      (fn [_ _]
                        (throw (ex-info "must not call a provider" {})))]

          (with-titling-cfg {"mode" "first_sentence"}
                            (fn []
                              (titling/maybe-auto-title! (env* sid title*)
                                                         "Fix the parser. Then ship the release.")
                              (expect (= "Fix the parser" @title*))
                              ;; nothing is deferred either — the mode is local
                              (expect (nil? (titling/after-turn-auto-title!
                                              (env* sid title*)
                                              "Fix the parser. Then ship the release."))))))))
  (it "mode disabled writes no title at all"
      (let [sid
            (fresh-sid)

            title*
            (atom "")]

        (with-redefs [titling/set-title-with-broadcast!
                      (fn [_ _ _ _]
                        (throw (ex-info "must not title" {})))

                      svar/ask!
                      (fn [_ _]
                        (throw (ex-info "must not call a provider" {})))]

          (with-titling-cfg {"mode" "disabled"}
                            (fn []
                              (expect (nil? (titling/maybe-auto-title! (env* sid title*)
                                                                       "name this session please")))
                              (expect (= "" @title*))
                              (expect (nil? (titling/after-turn-auto-title!
                                              (env* sid title*)
                                              "name this session please"))))))))
  (it "an explicit provider/model PINS the title call instead of walking the fleet"
      (let [sid
            (fresh-sid)

            title*
            (atom "")

            seen
            (atom nil)]

        (with-redefs [titling/set-title-with-broadcast!
                      (fn [_ _ a t]
                        (reset! a t))

                      svar/ask!
                      (fn [_ opts]
                        (reset! seen opts)
                        {:result {:title "Pinned Title Route"}})]

          (with-titling-cfg
            {"mode" "llm" "provider" "rbi_genai" "model" "gpt-5.4-mini"}
            (fn []
              @(titling/after-turn-auto-title! (env* sid title*)
                                               "pin the titling route to one endpoint")
              (expect (= {:provider :rbi_genai :model "gpt-5.4-mini"} (:routing @seen)))
              (expect (= "Pinned Title Route" @title*))))))))

;; Auto-titling is COSMETIC, but it is an `ask!` like any other: with a Copilot
;; plan in AUTO_TITLE_PROVIDER_ORDER and no `X-Initiator`, svar inferred `user`
;; from the fresh system+user title prompt and GitHub billed one FULL premium
;; interaction per session title.
(defdescribe auto-title-agent-initiator-test
             (it "marks the auto-title call as agent initiated so Copilot never bills it as premium"
                 (let [sid
                       (fresh-sid)

                       title*
                       (atom "")

                       seen
                       (atom nil)]

                   (with-redefs [titling/set-title-with-broadcast!
                                 (fn [_ _ a t]
                                   (reset! a t))

                                 svar/ask!
                                 (fn [_ opts]
                                   (reset! seen opts)
                                   {:result {:title "Fleet Scope Chips"}})]

                     @(maybe-auto-title! (env* sid title*) "please redesign the fleet scope chips")
                     (expect (= "agent" (get-in @seen [:llm-headers "X-Initiator"])))
                     (expect (= "Fleet Scope Chips" @title*))))))
