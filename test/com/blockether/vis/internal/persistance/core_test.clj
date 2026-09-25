(ns com.blockether.vis.internal.persistance.core-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.persistance.sqlite.core :as sqlite]
            [lazytest.core :refer [defdescribe expect it]]))

;; Regression (session 4b6897d4): a runtime error quoted the entire document
;; that broke it, so the turn's terminal write bound a value past
;; SQLITE_MAX_LENGTH, threw `[SQLITE_TOOBIG]`, and the turn stayed `running`
;; for good -- no status, no error, no iteration count -- inside a session that
;; had already finished it.
(defdescribe
  bounded-turn-diagnostics-test
  "`db-update-session-turn!` writes the row that says HOW a turn ended, and its
   DIAGNOSTIC fields are bounded here in the facade so every backend gets the
   same guarantee. The answer's own content is DATA and is persisted verbatim."
  (it "truncates one oversized diagnostic string and names what it cut"
      (let [huge
            (apply str (repeat (+ persistance/max-persisted-error-chars 500) "x"))

            bounded
            (persistance/bounded-error-text huge)]

        ;; The marker counts itself in: bounding NEVER grows a string.
        (expect (<= (count bounded) persistance/max-persisted-error-chars))
        (expect (str/starts-with? bounded (subs huge 0 1000)))
        (expect (str/ends-with? bounded " chars truncated>"))))
  (it "leaves a diagnostic already within the cap byte-identical"
      (expect (= "boom" (persistance/bounded-error-text "boom")))
      (expect (= "" (persistance/bounded-error-text ""))))
  (it "reaches every string at any depth of a structured error"
      (let [huge
            (apply str (repeat (inc persistance/max-persisted-error-chars) "y"))

            bounded
            (persistance/bound-error-data
              {"type" "error" "code" "python_runtime" "detail" {"message" huge "frames" [huge]}})]

        (expect (= "error" (get bounded "type")))
        (expect (= "python_runtime" (get bounded "code")))
        (expect (<= (count (get-in bounded ["detail" "message"]))
                    persistance/max-persisted-error-chars))
        (expect (str/ends-with? (get-in bounded ["detail" "message"]) " chars truncated>"))
        (expect (<= (count (first (get-in bounded ["detail" "frames"])))
                    persistance/max-persisted-error-chars)))
      (expect (nil? (persistance/bound-error-data nil))))
  (it "bounds the error and the ERROR content blocks before the backend sees them, and nothing else"
      (let [huge
            (apply str (repeat (+ persistance/max-persisted-error-chars 7) "z"))

            seen
            (atom nil)]

        (with-redefs-fn {#'persistance/backend-op (fn [_]
                                                    (fn [_ _ opts]
                                                      (reset! seen opts)))}
          #(persistance/db-update-session-turn! {}
                                                "turn-1"
                                                {:status :error
                                                 :iteration-count 33
                                                 :error {"type" "error" "message" huge}
                                                 :content [{"type" "error" "message" huge}
                                                           {"type" "text" "text" huge}]}))
        (expect (= :error (:status @seen)))
        (expect (= 33 (:iteration-count @seen)))
        (expect (<= (count (get (:error @seen) "message")) persistance/max-persisted-error-chars))
        (expect (<= (count (get (first (:content @seen)) "message"))
                    persistance/max-persisted-error-chars))
        ;; An answer is DATA: the facade never truncates it.
        (expect (= huge (get (second (:content @seen)) "text"))))))

(defdescribe
  iteration-precondition-test
  "`db-store-iteration!` refuses opts no backend can store before the backend
   sees them."
  (it "refuses opts without a turn, and opts that are not a map"
      (let [calls
            (atom 0)

            refusal
            (fn [opts]
              (try (with-redefs-fn {#'persistance/backend-op (fn [_]
                                                               (fn [& _]
                                                                 (swap! calls inc)))}
                     #(persistance/db-store-iteration! {} opts))
                   nil
                   (catch clojure.lang.ExceptionInfo e (ex-message e))))]

        (expect (= "db-store-iteration! requires :session-turn-id" (refusal {:iteration 1})))
        (expect (= "db-store-iteration! opts must be a map" (refusal [:session-turn-id "t"])))
        (expect (zero? @calls)))))

(defdescribe
  store-backend-test
  "SQLite is the one backend: every store value, and nil for no store, forwards
   to it, and it implements every `Store` op or does not compile."
  (it "implements every Store op"
      (expect (= (set (keys (:sigs persistance/Store)))
                 (set (keys (:implementation sqlite/backend)))))
      (expect (every? fn? (vals (:implementation sqlite/backend)))))
  (it "opens and disposes an in-memory store"
      (let [store (persistance/db-create-connection! :memory)]
        (try (expect (= :memory (:mode store)))
             (expect (nil? (persistance/db-get-session store (random-uuid))))
             (finally (persistance/db-dispose-connection! store)))))
  (it "forwards an absent store to the backend"
      (expect (nil? (persistance/db-create-connection! nil)))
      (expect (nil? (persistance/db-get-session nil (random-uuid)))))
  (it "refuses an unknown backend"
      (let [ex (try (persistance/db-create-connection! {:backend :postgres :path "x"})
                    nil
                    (catch clojure.lang.ExceptionInfo e e))]
        (expect (some? ex))
        (expect (str/includes? (ex-message ex) "Unknown persistence backend"))
        (expect (= [:sqlite] (:known (ex-data ex)))))))

(defdescribe backend-load-test
             (it "names the backend namespace and keeps the cause when loading fails"
                 (let [failure (try (#'persistance/load-backend
                                     'no.such.persistence.backend/backend)
                                    nil
                                    (catch clojure.lang.ExceptionInfo e e))]
                   (expect (= {:ns 'no.such.persistence.backend} (ex-data failure)))
                   (expect (some? (ex-cause failure)))
                   (expect (str/includes? (ex-message failure)
                                          "no.such.persistence.backend failed to load")))))
