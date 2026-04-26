(ns com.blockether.vis.provider
  "LLM provider registry — same plug-in pattern as channels, extensions,
   commandline, and persistance backends.

   A provider is an authentication/credential strategy for talking to
   an LLM API: GitHub Copilot device flow, Anthropic API key, Ollama
   local, etc. Each provider package self-registers a descriptor map
   at namespace load time; vis-core's `vis auth` CLI and the runtime
   token-resolution path look providers up through this registry by id.

   ── Why a registry ────────────────────────────────────────────────

   Before this split, vis-core hardcoded `vis auth github-copilot`
   and used `requiring-resolve` against the github-copilot namespace.
   That made the provider non-removable and forbade third-party
   provider packages. With this registry:

     - vis-core never references a concrete provider namespace
     - `vis auth` lists every registered provider dynamically
     - dropping a provider jar onto the classpath adds it to the CLI
       AND to the runtime token-resolution path automatically

   ── Registration ──────────────────────────────────────────────────

     (ns com.acme.providers.anthropic
       (:require [com.blockether.vis.provider :as p]))

     (p/register-global!
       {:provider/id           :anthropic
        :provider/label        \"Anthropic\"
        :provider/status-fn    #'status
        :provider/logout-fn    #'logout!
        :provider/detect-fn    #'detect-token
        :provider/auth-fn      #'interactive-auth!
        :provider/get-token-fn #'get-token!})

   Auto-discovery: ship `META-INF/vis/providers.edn` in the jar's
   resources/ listing every namespace that calls `register-global!`.
   The CLI dispatcher's plug-in loader picks it up at boot."
  (:require [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [taoensso.telemere :as tel]))

;; =============================================================================
;; Spec
;; =============================================================================

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))

(s/def :provider/id    keyword?)
(s/def :provider/label non-blank-string?)

;; All four runtime fns are optional individually so a minimal provider
;; (e.g. one that reads a static API key from env) doesn't need to
;; ship a no-op stub for every slot. Whoever calls them handles the
;; absent case (`(when-let [f (:provider/status-fn p)] (f))`).
(s/def :provider/status-fn    ifn?)  ;; () -> {:authenticated? bool …}
(s/def :provider/logout-fn    ifn?)  ;; () -> nil  (clear creds)
(s/def :provider/detect-fn    ifn?)  ;; () -> token-or-nil  (non-interactive)
(s/def :provider/auth-fn      ifn?)  ;; (printer-fn) -> nil (interactive)
(s/def :provider/get-token-fn ifn?)  ;; () -> token-string  (resolve usable token)

(s/def ::provider
  (s/keys :req [:provider/id :provider/label]
    :opt [:provider/status-fn :provider/logout-fn :provider/detect-fn
          :provider/auth-fn :provider/get-token-fn]))

(defn provider
  "Build and validate a provider descriptor."
  [spec]
  (when-not (s/valid? ::provider spec)
    (throw (ex-info (str "Invalid provider '" (:provider/id spec) "':\n"
                      (with-out-str (s/explain ::provider spec)))
             {:type    :provider/invalid-spec
              :id      (:provider/id spec)
              :explain (s/explain-data ::provider spec)})))
  spec)

;; =============================================================================
;; Global registry
;; =============================================================================

(defonce ^:private global-registry
  ;; {:provider/id → provider-map}
  (atom {}))

(defn register-global!
  "Register a provider in the global registry. Idempotent on
   `:provider/id` — re-registering replaces the previous descriptor.
   Returns the validated provider."
  [spec]
  (let [p (provider spec)]
    (swap! global-registry assoc (:provider/id p) p)
    (tel/log! {:level :info :id ::register-global
               :data  {:provider (:provider/id p)
                       :label    (:provider/label p)}
               :msg   (str "Provider '" (:provider/id p)
                        "' (" (:provider/label p) ") registered")})
    p))

(defn deregister-global! [id]
  (swap! global-registry dissoc id) nil)

(defn registered-providers
  "All globally registered providers as a vector."
  []
  (vec (vals @global-registry)))

(defn by-id
  "Lookup a provider by `:provider/id`. Returns nil when absent."
  [id] (get @global-registry id))

;; =============================================================================
;; Auto-discovery
;; =============================================================================

(def ^:private PROVIDERS_RESOURCE "META-INF/vis/providers.edn")

(defn discover-providers!
  "Scan the classpath for every `META-INF/vis/providers.edn` resource.
   Each file is an EDN vector of namespace symbols whose load triggers
   `register-global!`. Returns the count of providers added by this
   call (for diagnostics). Idempotent through `require`'s cache."
  []
  (let [urls   (try
                 (enumeration-seq
                   (.getResources
                     (.getContextClassLoader (Thread/currentThread))
                     PROVIDERS_RESOURCE))
                 (catch Exception _ nil))
        before (set (keys @global-registry))]
    (doseq [^java.net.URL url urls]
      (try
        (let [ns-syms (edn/read-string (slurp url))]
          (when (sequential? ns-syms)
            (doseq [ns-sym ns-syms]
              (when (symbol? ns-sym)
                (try (require ns-sym)
                  (tel/log! {:level :info :id ::discover
                             :data  {:provider-ns ns-sym :source (str url)}
                             :msg   (str "Auto-discovered provider ns '"
                                      ns-sym "' from " url)})
                  (catch Throwable t
                    (tel/log! {:level :error :id ::discover-failed
                               :data  {:provider-ns ns-sym :source (str url)
                                       :class (.getName (class t))
                                       :message (ex-message t)}
                               :msg   (str "Failed to load provider ns '"
                                        ns-sym "': " (ex-message t))})))))))
        (catch Throwable t
          (tel/log! {:level :error :id ::discover-parse-failed
                     :data  {:source (str url) :message (ex-message t)}
                     :msg   (str "Failed to parse " url ": " (ex-message t))}))))
    (- (count @global-registry) (count before))))
