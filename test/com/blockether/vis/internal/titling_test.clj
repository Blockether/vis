(ns com.blockether.vis.internal.titling-test
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.titling :as titling]
            [lazytest.core :refer [defdescribe it expect]]))

;; The interesting fns are private; reach them the way loop-test does.
(def ^:private maybe-auto-title! (deref #'titling/maybe-auto-title!))

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
      (let
        [sid
         (fresh-sid)

         title*
         (atom "")

         writes
         (atom [])]

        (with-redefs
          [titling/set-title-with-broadcast!
           (fn [_ _ a t]
             (swap! writes conj t)
             (reset! a t))

           svar/ask!
           (fn [_ _]
             {:result {:title "REPL Architecture Deep Dive"}})]

          @(maybe-auto-title! (env* sid title*) "I want to discuss the current approach to REPLs")
          ;; fallback landed BEFORE the model title — the tab is never untitled
          (expect (= ["I want to discuss the current approach" "REPL Architecture Deep Dive"]
                     @writes))
          (expect (= "REPL Architecture Deep Dive" @title*))
          ;; a real LLM title is NOT provisional
          (expect (false? (provisional-title? sid))))))
  (it "REGRESSION: a provisional fallback is upgraded on a LATER turn once the chain recovers"
      ;; This is the 4f0f6ac1/b7f27b7b bug: turn 1's LLM title failed under a
      ;; degraded/rate-limited provider chain, wrote the crude fallback, and the
      ;; old guard then froze it forever. Now the fallback stays PROVISIONAL and a
      ;; later turn re-attempts the upgrade.
      (let
        [sid
         (fresh-sid)

         title*
         (atom "")]

        ;; turn 1 — provider chain fails → provisional fallback
        (with-redefs
          [titling/set-title-with-broadcast!
           (fn [_ _ a t]
             (reset! a t))

           svar/ask!
           (fn [_ _]
             (throw (ex-info "429 rate limited" {})))]

          @(maybe-auto-title! (env* sid title*) "let us go over the ownership model now")
          (expect (= "let us go over the ownership model" @title*))
          (expect (true? (provisional-title? sid))))
        ;; turn 2 — providers recovered → the guard ALLOWS a retry and upgrades
        (with-redefs
          [titling/set-title-with-broadcast!
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
      (let
        [sid
         (fresh-sid)

         title*
         (atom "")]

        (with-redefs
          [titling/set-title-with-broadcast!
           (fn [_ _ a t]
             (reset! a t))

           svar/ask!
           (fn [_ _]
             {:result {:title "Security Audit Setup"}})]

          @(maybe-auto-title! (env* sid title*) "please review the security audit setup for clj")
          (expect (= "Security Audit Setup" @title*)))
        (with-redefs
          [titling/set-title-with-broadcast!
           (fn [_ _ _ _]
             (throw (ex-info "must not re-title" {})))

           svar/ask!
           (fn [_ _]
             (throw (ex-info "must not re-title" {})))]

          (expect (nil? (maybe-auto-title! (env* sid title*)
                                           "a totally different follow-up request")))
          (expect (= "Security Audit Setup" @title*)))))
  (it "a HUNG provider call trips the hard deadline and keeps the provisional fallback"
      (let
        [sid
         (fresh-sid)

         title*
         (atom "")

         blocker
         (promise)]

        (try (with-redefs
               [titling/set-title-with-broadcast!
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
  (it
    "REGRESSION: a leading /new-session slash command is stripped so the title reflects the real prompt, not the command word"
    ;; The `/new-session <task>` composer action leaked its command word into
    ;; the titling request, so the tab was named after "session" instead of the
    ;; task. Both the LLM prompt and the deterministic fallback must see the
    ;; STRIPPED request.
    (let
      [sid
       (fresh-sid)

       title*
       (atom "")

       seen
       (atom nil)]

      (with-redefs
        [titling/set-title-with-broadcast!
         (fn [_ _ a t]
           (reset! a t))

         svar/ask!
         (fn [_ opts]
           (reset! seen opts)
           {:result {:title "JSON Parser Build"}})]

        @(maybe-auto-title! (env* sid title*) "/new-session build a json parser")
        (let
          [user-msg (-> @seen
                        :messages
                        second
                        :content)]
          (expect (str/includes? user-msg "build a json parser"))
          (expect (not (str/includes? user-msg "/new-session"))))
        (expect (= "JSON Parser Build" @title*))))
    ;; provider chain down → fallback titles off the STRIPPED request too
    (let
      [sid
       (fresh-sid)

       title*
       (atom "")]

      (with-redefs
        [titling/set-title-with-broadcast!
         (fn [_ _ a t]
           (reset! a t))

         svar/ask!
         (fn [_ _]
           (throw (ex-info "429 rate limited" {})))]

        @(maybe-auto-title! (env* sid title*) "/new-session build a json parser")
        (expect (= "build a json parser" @title*))))))
