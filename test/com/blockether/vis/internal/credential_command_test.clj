(ns com.blockether.vis.internal.credential-command-test
  "Contract tests for command-backed provider credentials (`api_key_command`).

   The helper is a real executable script in a temp dir, so every assertion here
   exercises the actual `ProcessBuilder` path: no shell, bounded, single-flight,
   cached, and never persisted or logged."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.config-spec :as config-spec]
            [com.blockether.vis.internal.credential-command :as cred]
            [com.blockether.vis.internal.providers :as providers]
            [lazytest.core :refer [defdescribe expect it]]
            [yamlstar.core :as yamlstar]))

(def ^:private secret "s3cr3t-token-value")

(def ^:private tmp-dir (str (System/getProperty "java.io.tmpdir") "/vis-credential-command-test"))

(defn- script!
  "Write an executable `sh` script and return its absolute path."
  [name body]
  (.mkdirs (io/file tmp-dir))
  (let [f (io/file tmp-dir name)]
    (spit f (str "#!/bin/sh\n" body "\n"))
    (.setExecutable f true)
    (.getAbsolutePath f)))

(def ^:private counter-file (str tmp-dir "/exec-count.txt"))

(defn- exec-count
  []
  (let [f (io/file counter-file)]
    (if (.exists f) (count (remove str/blank? (str/split-lines (slurp f)))) 0)))

(defn- reset-count! [] (.mkdirs (io/file tmp-dir)) (spit counter-file ""))

(defn- ok-script [] (script! "ok.sh" (str "echo x >> " counter-file "\necho '" secret "'")))

(defn- fresh!
  "Cold cache + zeroed invocation counter before every scenario."
  []
  (cred/reset-cache!)
  (reset-count!))

(defdescribe argv-normalization-test
             (it "accepts a structured argv and a bare program, and never shell-splits"
                 (expect (= ["helper" "--env" "sbox"] (cred/argv ["helper" "--env" "sbox"])))
                 ;; A bare string is ONE argument: `foo --env a` is a program NAMED that,
                 ;; not a command line. Word-splitting here would be command injection.
                 (expect (= ["foo --env a"] (cred/argv "foo --env a"))))
             (it "rejects absent and malformed values"
                 (expect (nil? (cred/argv nil)))
                 (expect (nil? (cred/argv [])))
                 (expect (nil? (cred/argv "")))
                 (expect (nil? (cred/argv ["  "])))
                 (expect (nil? (cred/argv ["ok" 7])))
                 (expect (nil? (cred/argv {:cmd "helper"})))))

(defdescribe
  resolve-test
  (it "returns the helper's trimmed stdout as the token"
      (fresh!)
      (expect (= {:token secret} (cred/resolve! :p (ok-script))))
      (expect (= 1 (exec-count))))
  (it "returns nil when no command is configured" (fresh!) (expect (nil? (cred/resolve! :p nil))))
  (it "reports a malformed value instead of running anything"
      (fresh!)
      (expect (:error (cred/resolve! :p [])))
      (expect (zero? (exec-count))))
  (it "caches: a second resolve does not fork the helper again"
      (fresh!)
      (let [av (ok-script)]
        (expect (= {:token secret} (cred/resolve! :p av)))
        (expect (= {:token secret} (cred/resolve! :p av)))
        (expect (= 1 (exec-count)))))
  (it "is single-flight: 12 concurrent cold callers fork the helper ONCE"
      (fresh!)
      (let
        [av
         (ok-script)

         results
         (doall (pmap (fn [_]
                        (cred/resolve! :race av))
                      (range 12)))]

        (expect (every? #(= {:token secret} %) results))
        (expect (= 1 (exec-count)))))
  (it "invalidate! forces the next resolve to re-run the helper (401 refresh)"
      (fresh!)
      (let [av (ok-script)]
        (cred/resolve! :p av)
        (cred/invalidate! :p)
        (expect (= {:token secret} (cred/resolve! :p av)))
        (expect (= 2 (exec-count)))))
  (it "a changed argv invalidates by identity"
      (fresh!)
      (let [av (ok-script)]
        (cred/resolve! :p [av])
        (cred/resolve! :p [av "--env" "other"])
        (expect (= 2 (exec-count))))))

(defdescribe
  resolve-failure-test
  (it "a missing executable is an error naming the program, not a throw"
      (fresh!)
      (let [{:keys [error token]} (cred/resolve! :missing ["vis-definitely-not-installed-xyz"])]
        (expect (nil? token))
        (expect (str/includes? error "vis-definitely-not-installed-xyz"))))
  (it "a non-zero exit reports the code and the helper's stderr, never stdout"
      (fresh!)
      (let
        [av
         (script! "boom.sh" (str "echo '" secret "'\necho 'not logged in' >&2\nexit 3"))

         {:keys [error token]}
         (cred/resolve! :boom av)]

        (expect (nil? token))
        (expect (str/includes? error "3"))
        (expect (str/includes? error "not logged in"))
        ;; The credential travels on stdout and must never reach a diagnostic.
        (expect (not (str/includes? error secret)))))
  (it "blank stdout is a failure, not an empty credential"
      (fresh!)
      (let
        [av
         (script! "silent.sh" "exit 0")

         {:keys [error token]}
         (cred/resolve! :silent av)]

        (expect (nil? token))
        (expect (str/includes? error "no credential"))))
  (it "a failure is remembered briefly so a broken helper is not re-forked"
      (fresh!)
      (let [av (script! "fail.sh" (str "echo x >> " counter-file "\nexit 1"))]
        (cred/resolve! :fail av)
        (cred/resolve! :fail av)
        (expect (= 1 (exec-count))))))

(defdescribe peek-token-test
             (it "never forks: unknown is nil, and a resolved token is readable"
                 (fresh!)
                 (let [av (ok-script)]
                   (expect (nil? (cred/peek-token :peek av)))
                   (expect (zero? (exec-count)))
                   (cred/resolve! :peek av)
                   (expect (= {:token secret} (cred/peek-token :peek av)))
                   (expect (= 1 (exec-count))))))

(defdescribe
  credential-gap-test
  (it "a failing helper reads as a provider credential gap, like an unset ${NAME}"
      (fresh!)
      (let
        [provider
         {:id :gap :api-key-command ["vis-definitely-not-installed-xyz"] :models [{:name "m1"}]}

         {:keys [reason env-vars]}
         (config/provider-credential-gap provider)]

        (expect (string? reason))
        (expect (nil? env-vars))
        (expect (str/includes? reason "gap"))))
  (it "a working helper has no gap and leaks no token into the reason"
      (fresh!)
      (let [provider {:id :fine :api-key-command (ok-script) :models [{:name "m1"}]}]
        (expect (nil? (config/provider-credential-gap provider)))
        (expect (nil? (config/provider-credential-error provider)))))
  (it "the cached read paints without forking a subprocess"
      (fresh!)
      (let
        [provider {:id :cachedgap
                   :api-key-command ["vis-definitely-not-installed-xyz"]
                   :models [{:name "m1"}]}]
        (expect (nil? (config/provider-credential-gap-cached provider)))
        (config/provider-credential-gap provider)
        (expect (:reason (config/provider-credential-gap-cached provider)))))
  (it "invalidate-credential-command! is the config-level refresh seam"
      (fresh!)
      (let
        [av
         (ok-script)

         provider
         {:id :seam :api-key-command av :models [{:name "m1"}]}]

        (config/provider-credential-gap provider)
        (config/invalidate-credential-command! :seam)
        (expect (nil? (cred/peek-token :seam av))))))

(defdescribe
  config-shape-test
  (it "the YAML contract accepts argv and bare-program spellings"
      (let
        [cfg (yamlstar/load (str "providers:\n"
                                 "  - id: sso_gateway\n" "    compatibility: openai-responses\n"
                                 "    base_url: https://gateway.example.com/v1\n"
                                 "    api_key_command: [token-helper, --env, sbox]\n"
                                 "    models:\n" "      - name: m1\n"))]
        (expect (config-spec/valid? cfg))
        (expect (config-spec/valid?
                  (assoc-in cfg ["providers" 0 "api_key_command"] "token-helper")))))
  (it "rejects an empty, blank or non-string command"
      (let
        [cfg (yamlstar/load (str "providers:\n" "  - id: sso_gateway\n"
                                 "    base_url: https://gateway.example.com/v1\n"
                                 "    api_key_command: [token-helper]\n"
                                 "    models:\n" "      - name: m1\n"))]
        (expect (config-spec/valid? cfg))
        (expect (not (config-spec/valid? (assoc-in cfg ["providers" 0 "api_key_command"] []))))
        (expect (not (config-spec/valid? (assoc-in cfg ["providers" 0 "api_key_command"] ""))))
        (expect (not (config-spec/valid? (assoc-in cfg ["providers" 0 "api_key_command"] [7]))))
        ;; snake_case is the only accepted spelling.
        (expect (not (config-spec/valid? (-> cfg
                                             (update-in ["providers" 0] dissoc "api_key_command")
                                             (assoc-in ["providers" 0 "api-key-command"]
                                                       ["h"])))))))
  (it "runtime-config carries the argv through as :api-key-command"
      (let
        [cfg (yamlstar/load (str "providers:\n" "  - id: sso_gateway\n"
                                 "    base_url: https://gateway.example.com/v1\n"
                                 "    api_key_command: [token-helper, --env, sbox]\n"
                                 "    models:\n" "      - name: m1\n"))]
        (expect (= ["token-helper" "--env" "sbox"]
                   (-> cfg
                       config/runtime-config
                       :providers
                       first
                       :api-key-command))))))

(defdescribe never-persisted-test
             (it "the resolved token never reaches the durable provider shape"
                 (fresh!)
                 (let
                   [av
                    (ok-script)

                    provider
                    {:id :persist :api-key-command av :models [{:name "m1"}]}]

                   (expect (= {:token secret} (cred/resolve! :persist av)))
                   (let [persisted (providers/persisted-provider-config provider)]
                     (expect (= av (:api-key-command persisted)))
                     (expect (nil? (:api-key persisted)))
                     (expect (not (str/includes? (pr-str persisted) secret)))))))
