(ns com.blockether.vis.ext.channel-tui.provider-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.ext.channel-tui.provider :as provider]
            [com.blockether.vis.ext.provider-openai-codex :as codex]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provider-dialog-namespace-test
  (it "loads the provider dialog namespace"
    (expect (some? (find-ns 'com.blockether.vis.ext.channel-tui.provider)))))

(defdescribe swap-items-test
  (it "swaps two positions without touching the others"
    (let [swap-items @#'provider/swap-items]
      (expect (= [:a :c :b :d]
                (swap-items [:a :b :c :d] 1 2))))))

(defdescribe move-model-to-front-test
  (it "moves the selected model to the first slot"
    (let [move-model-to-front @#'provider/move-model-to-front]
      (expect (= [{:name "beta"} {:name "alpha"} {:name "gamma"}]
                (move-model-to-front [{:name "alpha"} {:name "beta"} {:name "gamma"}] 1))))))

(defdescribe codex-oauth-ready-test
  (it "returns true immediately when Codex credentials already exist"
    (let [login-called? (atom false)]
      (with-redefs [vis/provider-by-id (constantly {:provider/detect-fn (constantly {:account-id "acct_123"})})
                    codex/login!       (fn [& _]
                                         (reset! login-called? true)
                                         :ok)
                    dlg/confirm-dialog! (fn [& _] nil)]
        (expect (= true (@#'provider/codex-oauth-ready! nil)))
        (expect (= false @login-called?)))))

  (it "runs the shared Codex login flow from the TUI"
    (let [seen (atom nil)]
      (with-redefs [vis/provider-by-id (constantly {:provider/detect-fn (constantly nil)})
                    codex/login!       (fn [printer-fn opts]
                                         (reset! seen {:printer-fn printer-fn :opts opts})
                                         :ok)
                    dlg/confirm-dialog! (fn [& _] nil)
                    dlg/text-input-dialog! (fn [& _] "http://localhost:1455/auth/callback?code=abc&state=s")]
        (expect (= true (@#'provider/codex-oauth-ready! nil)))
        (expect (= "vis-tui" (get-in @seen [:opts :originator])))
        (expect (ifn? (get-in @seen [:opts :manual-code-fn])))
        (expect (= "http://localhost:1455/auth/callback?code=abc&state=s"
                  ((get-in @seen [:opts :manual-code-fn]) nil))))))

  (it "returns false when the shared Codex login flow fails"
    (with-redefs [vis/provider-by-id (constantly {:provider/detect-fn (constantly nil)})
                  codex/login!       (fn [& _]
                                       (throw (ex-info "boom" {})))
                  dlg/confirm-dialog! (fn [& _] nil)]
      (expect (= false (@#'provider/codex-oauth-ready! nil))))))
