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

(def ^:private flat-book
  "A vendor with ONE credential and therefore no plan vocabulary: the key lives
   at the file ROOT, which is where its users' keys already are."
  {:vendor "Solo"
   :file "solo-auth-test.json"
   :file-shape :flat
   :key-hint "<your-solo-api-key>"
   :error-type :vis/solo-not-authenticated
   :plans {:solo {:provider-id :solo
                  :label "Solo"
                  :base-url "https://solo.test/v1"
                  :default-models ["solo-1"]
                  :env-keys ["SOLO_API_KEY"]}}})

(defdescribe
  persistence-test
  ;; ONE `it`: both shapes share the `auth-file` redef, and `with-redefs` mutates
  ;; a global Var, so splitting them would let the two cases race over it.
  (it "keeps sibling plans apart in a by-plan file and a lone key at a flat file's root"
      (let [dir (java.io.File.
                  (str (System/getProperty "java.io.tmpdir") "/vis-key-store-" (System/nanoTime)))]
        (.mkdirs dir)
        (with-redefs-fn {#'store/auth-file (fn [b]
                                             (str dir "/" (:file b)))}
          (fn []
            (try (store/update-plan! book :coding {:api-key "coding-key"})
                 (store/update-plan! book :token {:api-key "token-key"})
                 (store/logout-plan! book :coding)
                 (expect (nil? (:coding (store/load-auth book))))
                 (expect (= "token-key" (:api-key (:token (store/load-auth book)))))
                 (store/logout-plan! book :token)
                 ;; An empty file must not linger: its mere existence reads as
                 ;; "authenticated" to a detect-fn.
                 (expect (not (.exists (java.io.File. ^String (store/auth-file book)))))
                 ;; The flat book's plan tag never reaches disk, so a key an
                 ;; earlier build wrote at the root keeps resolving.
                 (store/update-plan! flat-book :solo {:api-key "solo-key" :saved-at 1})
                 (expect (= "solo-key" (:api-key (store/load-auth flat-book))))
                 (expect (nil? (:solo (store/load-auth flat-book))))
                 (expect (= "solo-key" (#'store/file-key flat-book :solo)))
                 (store/logout-plan! flat-book :solo)
                 (expect (not (.exists (java.io.File. ^String (store/auth-file flat-book)))))
                 (finally (run! #(.delete ^java.io.File %) (.listFiles dir)) (.delete dir))))))))

(defdescribe auth-prompt-test
             (it "prints the book's own notes, env var and endpoint"
                 (let [lines (store/auth-instruction-lines book :coding)]
                   (expect (some #(= "  Acme (Coding Plan) requires a static API key." %) lines))
                   (expect (some #(= "  The key is plan-scoped." %) lines))
                   (expect (some #(= "         export ACME_CODING_API_KEY=<your-acme-api-key>" %)
                                 lines))
                   (expect (some #(= "  Endpoint: https://coding.acme.test/v1" %) lines)))))

(defdescribe
  single-credential-message-test
  ;; Nothing is redefined here: neither book's env var, config entry or file
  ;; exists, so the real lookup runs and really fails.
  (it "names no plan in the message a single-credential user sees"
      (let [thrown
            (try (store/token-envelope flat-book :solo) nil (catch clojure.lang.ExceptionInfo e e))]
        (expect (= :vis/solo-not-authenticated (:type (ex-data thrown))))
        (expect (re-find #"No Solo API key\. Run" (ex-message thrown)))
        (expect (nil? (re-find #"for plan" (ex-message thrown))))))
  (it "still says WHICH plan when the user has two of them"
      (let [thrown
            (try (store/token-envelope book :coding) nil (catch clojure.lang.ExceptionInfo e e))]
        (expect (re-find #"No Acme API key for plan :coding\." (ex-message thrown))))))
