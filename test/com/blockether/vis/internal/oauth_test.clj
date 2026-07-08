(ns com.blockether.vis.internal.oauth-test
  (:require [com.blockether.vis.internal.oauth :as oauth]
            [lazytest.core :refer [defdescribe expect it throws?]]))

(defdescribe
  oauth-facade-test
  (it "fresh-within? is true only for a recent numeric stamp"
      (expect (oauth/fresh-within? (System/currentTimeMillis)))
      (expect (not (oauth/fresh-within? (- (System/currentTimeMillis) 999999))))
      (expect (not (oauth/fresh-within? nil)))
      (expect (not (oauth/fresh-within? "nope"))))
  (it "single-flight! reuses when reuse returns non-nil, else refreshes"
      (let [ran
            (atom 0)

            refresh!
            (fn []
              (swap! ran inc)
              {:token "fresh"})]

        (expect (= {:token "cached"}
                   (oauth/single-flight! (oauth/new-lock)
                                         (fn []
                                           {:token "cached"})
                                         refresh!)))
        (expect (zero? @ran))
        (expect (= {:token "fresh"}
                   (oauth/single-flight! (oauth/new-lock)
                                         (fn []
                                           nil)
                                         refresh!)))
        (expect (= 1 @ran))))
  (it "make-file-refresher collapses a concurrent burst into ONE exchange"
      (let [exchanges
            (atom 0)

            saved
            (atom nil)

            r
            (oauth/make-file-refresher
              {:load (fn []
                       (or @saved {:refresh-token "R"}))
               :saved-at :saved-at-ms
               :refresh-token :refresh-token
               :exchange! (fn [_]
                            (swap! exchanges inc)
                            (Thread/sleep 40)
                            {:access-token "T"})
               :persist! (fn [c]
                           (reset! saved (assoc c :saved-at-ms (System/currentTimeMillis))))
               :->token (fn [c]
                          {:token (:access-token c)})
               :no-token! #(throw (ex-info "no token" {}))})

            results
            (mapv deref
                  (mapv (fn [_]
                          (future (r)))
                        (range 40)))]

        (expect (= 1 @exchanges))
        (expect (every? #(= {:token "T"} %) results))))
  (it
    "make-file-refresher never REUSES the just-rejected token, but still reuses a different one"
    (let [exchanges
          (atom 0)

          ;; on-file token is locally FRESH (saved just now) but DEAD
          ;; server-side — exactly the timing bug: plain freshness reuse
          ;; would hand it straight back.
          saved
          (atom {:access-token "DEAD" :refresh-token "R" :saved-at-ms (System/currentTimeMillis)})

          r
          (oauth/make-file-refresher
            {:load (fn []
                     @saved)
             :saved-at :saved-at-ms
             :refresh-token :refresh-token
             :exchange! (fn [_]
                          (swap! exchanges inc)
                          {:access-token "FRESH" :refresh-token "R2"})
             :persist! (fn [c]
                         (reset! saved (assoc c :saved-at-ms (System/currentTimeMillis))))
             :->token (fn [c]
                        {:token (:access-token c)})
             :no-token! #(throw (ex-info "no token" {}))})]

      ;; 0-arity: no rejected token -> plain freshness reuse hands back DEAD.
      (expect (= {:token "DEAD"} (r)))
      (expect (zero? @exchanges))
      ;; rejecting DEAD forces a real exchange instead of reusing it.
      (expect (= {:token "FRESH"} (r "DEAD")))
      (expect (= 1 @exchanges))
      ;; the file now holds FRESH (differs from the rejected DEAD), so a
      ;; concurrent 401 on DEAD reuses FRESH — storm still collapses, no
      ;; second exchange.
      (expect (= {:token "FRESH"} (r "DEAD")))
      (expect (= 1 @exchanges))))
  (it
    "make-file-refresher PROPAGATES an exchange failure on 401 recovery — never swallows it nor hands back the dead token"
    (let [saved
          (atom {:access-token "DEAD" :refresh-token "R" :saved-at-ms (System/currentTimeMillis)})

          exchanges
          (atom 0)

          r
          (oauth/make-file-refresher
            {:load (fn []
                     @saved)
             :saved-at :saved-at-ms
             :refresh-token :refresh-token
             ;; dead/rotated refresh_token -> the server rejects the exchange
             ;; (HTTP 400 invalid_grant). The refresh genuinely cannot recover.
             :exchange! (fn [_]
                          (swap! exchanges inc)
                          (throw (ex-info "invalid_grant" {:status 400})))
             :persist! (fn [c]
                         (reset! saved (assoc c :saved-at-ms (System/currentTimeMillis))))
             :->token (fn [c]
                        {:token (:access-token c)})
             :no-token! #(throw (ex-info "no token" {}))})]

      ;; On 401 recovery we reject DEAD, so reuse can't short-circuit: the real
      ;; exchange runs, fails, and the failure MUST surface (so the turn loop's
      ;; try-refresh returns false and the original 401 is thrown) — it must not
      ;; be swallowed into a fake success handing DEAD straight back.
      (expect (throws? clojure.lang.ExceptionInfo #(r "DEAD")))
      (expect (= 1 @exchanges))))
  (it
    "two refreshers (different providers) refresh IN PARALLEL — no shared lock"
    (let [mk
          (fn [c]
            (oauth/make-file-refresher {:load (fn []
                                                {:refresh-token "R" :saved-at-ms 0})
                                        :saved-at :saved-at-ms
                                        :refresh-token :refresh-token
                                        :exchange! (fn [_]
                                                     (swap! c inc)
                                                     (Thread/sleep 250)
                                                     {:access-token "T" :saved-at-ms 0})
                                        :persist! identity
                                        :->token (fn [a]
                                                   {:token (:access-token a)})
                                        :no-token! #(throw (ex-info "no" {}))}))

          a
          (mk (atom 0))

          b
          (mk (atom 0))

          t0
          (System/currentTimeMillis)

          fa
          (future (a))

          fb
          (future (b))]

      @fa
      @fb
      ;; serialized by a shared lock would be ~500ms; independent locks ~250ms
      (expect (< (- (System/currentTimeMillis) t0) 480))))
  (it "make-file-refresher throws via :no-token! when no refresh token"
      (let [r (oauth/make-file-refresher {:load (fn []
                                                  {})
                                          :saved-at :saved-at-ms
                                          :refresh-token :refresh-token
                                          :exchange! (fn [_]
                                                       {:access-token "T"})
                                          :persist! identity
                                          :->token (fn [c]
                                                     {:token (:access-token c)})
                                          :no-token! #(throw (ex-info "no refresh token"
                                                                      {:type :test/no-token}))})]
        (expect (throws? clojure.lang.ExceptionInfo r)))))
