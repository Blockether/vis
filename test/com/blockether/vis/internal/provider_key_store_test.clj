(ns com.blockether.vis.internal.provider-key-store-test
  (:require [com.blockether.vis.internal.provider-key-store :as store]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private book
  {:vendor "Acme"
   :file "acme-auth-test.json"
   :key-hint "<your-acme-api-key>"
   :error-type :vis/acme-not-authenticated
   :auth-notes ["  The key is plan-scoped." ""]
   :plans {:coding {:provider-id :acme-coding-plan
                    :label "Acme (Coding Plan)"
                    :base-url "https://coding.acme.test/v1"
                    :default-models ["acme-coder"]
                    :env-keys ["ACME_CODING_API_KEY"]}
           :token {:provider-id :acme-token-plan
                   :label "Acme (Token Plan)"
                   :base-url "https://token.acme.test/v1"
                   :default-models ["acme-token"]
                   :env-keys ["ACME_TOKEN_API_KEY"]}}})

(defdescribe
  lookup-order-test
  (it "prefers the configured key, then the env var, then the file"
      (with-redefs-fn {#'store/load-auth (constantly {:coding {:api-key "file"}})}
        (fn []
          (with-redefs-fn {#'store/configured-key (constantly "config")
                           #'store/env-key (constantly "env")}
            (fn []
              (expect (= {:api-key "config" :source :config} (store/detect-key book :coding)))))
          (with-redefs-fn {#'store/configured-key (constantly nil)
                           #'store/env-key (constantly "env")}
            (fn []
              (expect (= {:api-key "env" :source :env-var} (store/detect-key book :coding)))))
          (with-redefs-fn {#'store/configured-key (constantly nil) #'store/env-key (constantly nil)}
            (fn []
              (expect (= {:api-key "file" :source :auth-file} (store/detect-key book :coding)))
              ;; A key persisted for one plan is rejected by the other plan's
              ;; endpoint, so it must never answer for the sibling.
              (expect (nil? (store/detect-key book :token))))))))
  (it "fails with the plan's own auth command and env var when nothing has a key"
      (with-redefs-fn {#'store/detect-key (constantly nil)}
        (fn []
          (let [thrown (try (store/token-envelope book :coding)
                            nil
                            (catch clojure.lang.ExceptionInfo e e))]
            (expect (= :vis/acme-not-authenticated (:type (ex-data thrown))))
            (expect (= :acme-coding-plan (:provider-id (ex-data thrown))))
            (expect (re-find #"ACME_CODING_API_KEY" (ex-message thrown)))))))
  (it "answers the plan's own endpoint in the token envelope"
      (with-redefs-fn {#'store/detect-key (constantly {:api-key "k" :source :auth-file})}
        (fn []
          (expect (= {:token "k" :api-url "https://token.acme.test/v1"}
                     (store/token-envelope book :token)))))))

(defdescribe status-test
             (it "previews a key instead of reporting it"
                 (with-redefs-fn {#'store/detect-key (constantly {:api-key "sk-acme-0123456789"
                                                                  :source :env-var})}
                   (fn []
                     (let [report (store/status-report book :coding)]
                       (expect (true? (:is-authenticated report)))
                       (expect (= :env-var (:source report)))
                       (expect (= "sk-acme-..." (:api-key-preview report)))))))
             (it "says NOT authenticated without inventing a preview"
                 (with-redefs-fn {#'store/detect-key (constantly nil)}
                   (fn []
                     (let [report (store/status-report book :token)]
                       (expect (false? (:is-authenticated report)))
                       (expect (not (contains? report :api-key-preview))))))))

(defdescribe persistence-test
             (it "keeps one plan's key when the sibling logs out, and deletes an emptied file"
                 (let [f
                       (java.io.File/createTempFile "vis-key-store" ".json")

                       path
                       (.getAbsolutePath f)]

                   (.delete f)
                   (with-redefs-fn {#'store/auth-file (constantly path)}
                     (fn []
                       (try (store/update-plan! book :coding {:api-key "coding-key"})
                            (store/update-plan! book :token {:api-key "token-key"})
                            (store/logout-plan! book :coding)
                            (expect (nil? (:coding (store/load-auth book))))
                            (expect (= "token-key" (:api-key (:token (store/load-auth book)))))
                            (store/logout-plan! book :token)
                            ;; An empty file must not linger: its mere existence reads as
                            ;; "authenticated" to a detect-fn.
                            (expect (not (.exists (java.io.File. path))))
                            (finally (.delete (java.io.File. path)))))))))

(defdescribe auth-prompt-test
             (it "prints the book's own notes, env var and endpoint"
                 (let [lines (store/auth-instruction-lines book :coding)]
                   (expect (some #(= "  Acme (Coding Plan) requires a static API key." %) lines))
                   (expect (some #(= "  The key is plan-scoped." %) lines))
                   (expect (some #(= "         export ACME_CODING_API_KEY=<your-acme-api-key>" %)
                                 lines))
                   (expect (some #(= "  Endpoint: https://coding.acme.test/v1" %) lines)))))
