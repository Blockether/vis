(ns com.blockether.vis.loop.runtime.tool-rescue-test
  "Tests for the `:rescue-fn` hook on tool-defs.

   Contract:
   - When the tool's :fn throws, the dispatcher invokes
     `(apply rescue-fn err args)` (args = the exact arg vector passed to
     the tool AFTER :validate-input).
   - Rescue-fn may return a value → used as the tool result (and still
     runs through :validate-output).
   - Rescue-fn may throw → exception propagates as a normal :tool-exception.
   - Rescue-fn may return nil → used as the result (NOT fallthrough).
   - Tools without :rescue-fn behave exactly as before."
  (:require
    [lazytest.core :refer [defdescribe describe expect it]]
    [com.blockether.vis.loop.runtime.core :as rt]
    [com.blockether.vis.loop.tool :as sci-tool]))

(defn- make-env
  "Minimal env stub for execute-tool. Only the fields execute-tool touches
   are needed here — no DB / router / SCI ctx required for this contract."
  []
  {:tool-registry-atom (atom {})
   :state-atom         (atom {})})

(defn- register!
  "Make a canonical tool-def, register it into env's registry, return env."
  [env sym f tool-def]
  (let [canonical (sci-tool/make-tool-def sym f tool-def)]
    (rt/register-tool-def! (:tool-registry-atom env) sym canonical)
    env))

(defn- invoke
  "Call a registered tool by :sym, returning the raw execute-tool outcome
   `{:result <v> :error nil}` or `{:result nil :error {...}}`."
  [env sym args]
  (let [tool-def (get @(:tool-registry-atom env) sym)]
    (rt/execute-tool env sym (:fn tool-def) (vec args)
      {:tool-def tool-def
       :tool-registry-atom (:tool-registry-atom env)
       :query-ctx {}})))

(defdescribe rescue-fn-happy-path
  (describe "tool without :rescue-fn"
    (it "surfaces exceptions as :tool-exception outcomes"
      (let [env (-> (make-env)
                  (register! 'boom
                    (fn [_] (throw (ex-info "kaboom" {:kind :test})))
                    {:doc "throws" :examples ["(boom 1)"]}))
            out (invoke env 'boom [1])]
        (expect (nil? (:result out)))
        (expect (= :tool-exception (:type (:error out))))
        (expect (= "kaboom" (:message (:error out))))))))

(defdescribe rescue-fn-returns-value
  (describe "tool whose :rescue-fn returns a recovery value"
    (it "uses the rescue value as the tool result"
      (let [env (-> (make-env)
                  (register! 'divy
                    (fn [a b] (/ a b))
                    {:doc "divide"
                     :examples ["(divy 6 2)"]
                     :rescue-fn (fn [_err a b]
                                  ;; Map a divide-by-zero into an
                                  ;; informational return instead of an
                                  ;; error surfaced to the LLM.
                                  (str "cannot divide " a " by " b))}))
            happy (invoke env 'divy [10 2])
            rescued (invoke env 'divy [10 0])]
        (expect (= 5 (:result happy)))
        (expect (nil? (:error happy)))
        (expect (= "cannot divide 10 by 0" (:result rescued)))
        (expect (nil? (:error rescued)))))))

(defdescribe rescue-fn-rethrows
  (describe "tool whose :rescue-fn re-throws"
    (it "re-thrown error becomes a :tool-exception with the rescuer's message"
      (let [env (-> (make-env)
                  (register! 'strict
                    (fn [_] (throw (ex-info "raw" {:orig true})))
                    {:doc "always throws"
                     :examples ["(strict :x)"]
                     :rescue-fn (fn [err & _args]
                                  ;; Replace cryptic error with a
                                  ;; teaching one. Still throws — the
                                  ;; LLM should see the new message.
                                  (throw (ex-info (str "strict: " (ex-message err))
                                           {:hint "supply a non-nil value"})))}))
            out (invoke env 'strict [nil])]
        (expect (nil? (:result out)))
        (expect (= :tool-exception (:type (:error out))))
        (expect (= "strict: raw" (:message (:error out))))
        (expect (= "supply a non-nil value" (:hint (:data (:error out)))))))))

(defdescribe rescue-fn-sees-original-args
  (describe ":rescue-fn argument wiring"
    (it "receives the exception followed by the exact args the tool was called with"
      (let [capture (atom nil)
            env (-> (make-env)
                  (register! 'multi
                    (fn [_a _b _c] (throw (ex-info "oops" {})))
                    {:doc "three args"
                     :examples ["(multi 1 2 3)"]
                     :rescue-fn (fn [err a b c]
                                  (reset! capture {:err-msg (ex-message err)
                                                   :args [a b c]})
                                  :rescued)}))
            out (invoke env 'multi [:one 2 "three"])]
        (expect (= :rescued (:result out)))
        (expect (= {:err-msg "oops" :args [:one 2 "three"]} @capture))))))

(defdescribe rescue-fn-respects-validate-output
  (describe "validate-output still runs on the rescued value"
    (it "validate-output can coerce the rescued result"
      (let [env (-> (make-env)
                  (register! 'wrap
                    (fn [_] (throw (ex-info "x" {})))
                    {:doc "throws"
                     :examples ["(wrap :a)"]
                     :rescue-fn (fn [_err _arg] "raw-string")
                     :validate-output (fn [inv]
                                        ;; Coerce any string result into
                                        ;; a map the caller can destructure.
                                        (if (string? (:result inv))
                                          {:result {:wrapped (:result inv)}}
                                          inv))}))
            out (invoke env 'wrap [:a])]
        (expect (= {:wrapped "raw-string"} (:result out)))
        (expect (nil? (:error out)))))))

(defdescribe rescue-fn-validation
  (describe "assert-fn-tool-def! rejects non-fn :rescue-fn"
    (it "throws when :rescue-fn is not a function"
      (let [thrown (try
                     (sci-tool/make-tool-def
                       'bad
                       (fn [] 1)
                       {:doc "bad"
                        :examples ["(bad)"]
                        :rescue-fn "not-a-fn"})
                     (catch Exception e e))]
        (expect (instance? Exception thrown))
        (expect (= :rlm/invalid-tool-def (:type (ex-data thrown))))
        (expect (= :rescue-fn (:field (ex-data thrown))))))))
