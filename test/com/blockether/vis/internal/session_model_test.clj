(ns com.blockether.vis.internal.session-model-test
  "The session's model pick and the listeners that mirror it onto every attached surface."
  (:require [com.blockether.vis.internal.persistance :as persistance]
            [com.blockether.vis.internal.session-model :as smodel]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- with-quiet-store
  "Run `f` with the debounced DB write stubbed out and the pending queue emptied after,
   so a unit test can drive `set-model!` without a database or a late flush."
  [f]
  (with-redefs [persistance/db-set-session-model-pref! (fn [& _]
                                                         nil)]
    (try (f) (finally (reset! @#'smodel/pending {})))))

(defn- capturing
  "Register a listener that records `[sid pick]`, run `f`, then deregister it and answer
   what the listener saw."
  [f]
  (let [seen
        (atom [])

        listener
        (fn [sid pick]
          (swap! seen conj [sid pick]))]

    (try (smodel/add-model-listener! listener)
         (with-quiet-store f)
         @seen
         (finally (smodel/remove-model-listener! listener)))))

;; Regression, issue #154: a pick moved by anything but the web picker changed the DB and
;; nothing else — the TUI footer chip and the companion header kept naming the provider
;; whose credentials had just been rejected, because the broadcast lived in the gateway
;; facade instead of in the store every writer goes through.
(defdescribe
  model-listener-test
  "`set-model!` is the one door onto the pick, so it is where the notification belongs."
  (it "hands every registered listener the pick that was just set"
      (expect (= [["sess-1" {:provider "openai" :model "gpt-5" :reason nil}]]
                 (capturing #(smodel/set-model! :db "sess-1" "openai" "gpt-5")))))
  (it "carries the REASON a non-human writer moved the pick"
      ;; Without it a surface can show the new model but not why it changed by itself.
      (expect (= [["sess-1" {:provider "openai" :model "gpt-5.4" :reason :authentication-fallback}]]
                 (capturing
                   #(smodel/set-model! :db "sess-1" "openai" "gpt-5.4" :authentication-fallback)))))
  (it "broadcasts a CLEARED pick as blanks rather than staying silent"
      ;; Silence would freeze the last pick on every chip that mirrors this store.
      (expect (= [["sess-1" {:provider nil :model nil :reason nil}]]
                 (capturing #(smodel/set-model! :db "sess-1" nil "   ")))))
  (it "says nothing when there is no session or no store to write to"
      (expect (= [] (capturing #(smodel/set-model! nil "sess-1" "openai" "gpt-5"))))
      (expect (= [] (capturing #(smodel/set-model! :db nil "openai" "gpt-5")))))
  (it "keeps notifying the others when one listener throws"
      ;; A channel that blows up must not reject the model change for everybody else.
      (let [boom (fn [& _]
                   (throw (ex-info "listener down" {})))]
        (try (smodel/add-model-listener! boom)
             (expect (= [["sess-1" {:provider "openai" :model "gpt-5" :reason nil}]]
                        (capturing #(smodel/set-model! :db "sess-1" "openai" "gpt-5"))))
             (finally (smodel/remove-model-listener! boom)))))
  (it "stops calling a listener that was removed"
      (let [seen
            (atom [])

            listener
            (fn [& args]
              (swap! seen conj (vec args)))]

        (smodel/add-model-listener! listener)
        (smodel/remove-model-listener! listener)
        (with-quiet-store #(smodel/set-model! :db "sess-1" "openai" "gpt-5"))
        (expect (= [] @seen)))))
