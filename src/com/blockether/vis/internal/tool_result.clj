(ns com.blockether.vis.internal.tool-result
  "Structured return contract for tool-like SCI symbols.

   The contract lives in DATA, not metadata:

     {:ok?          boolean
      :result       any
      :result-shape map
      :provenance   {:op :started-at-ms :finished-at-ms :duration-ms
                     ;; optional richer context:
                     :tool {:sym :alias? :call}
                     :extension {:namespace :registry-id? :kind? :version?
                                 :author? :owner? :license? :doc?}
                     :source {:paths :mtime-max :hash-sha256}}
      :markdown     string
      :error        nil | {:type :message :trace}}

   Metadata may carry `:vis/presentation` rendering hints, but the
   schema above is the canonical persisted/runtime surface.

   Error traces are normalized and lightly sanitized so callers get a
   semi-explanatory stack without the full SCI trampoline noise.
   Frame `:origin` is one of:

     :user-code  sandbox/user frame (`user`, `sandbox`, `iteration`)
     :tool       extension/tool implementation
     :runtime    vis host runtime internals
     :library    third-party / JDK / everything else"
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]))

(def ^:private max-trace-frames 12)

(defn- now-ms [] (System/currentTimeMillis))

(s/def ::ok? boolean?)
(s/def ::result any?)
(s/def ::result-shape map?)
(s/def ::markdown (s/and string? #(not (str/blank? %))))

(s/def ::op keyword?)
(s/def ::started-at-ms integer?)
(s/def ::finished-at-ms integer?)
(s/def ::duration-ms nat-int?)

(s/def ::sym symbol?)
(s/def ::alias symbol?)
(s/def ::call (s/and string? #(not (str/blank? %))))
(s/def ::tool (s/keys :req-un [::sym ::call]
                :opt-un [::alias]))

(s/def ::namespace symbol?)
(s/def ::registry-id symbol?)
(s/def ::kind (s/and string? #(not (str/blank? %))))
(s/def ::doc (s/and string? #(not (str/blank? %))))
(s/def ::version (s/and string? #(not (str/blank? %))))
(s/def ::author (s/and string? #(not (str/blank? %))))
(s/def ::owner (s/and string? #(not (str/blank? %))))
(s/def ::license (s/and string? #(not (str/blank? %))))
(s/def ::extension (s/keys :req-un [::namespace]
                     :opt-un [::registry-id ::kind ::doc ::version
                              ::author ::owner ::license]))

(s/def ::paths (s/coll-of string? :kind vector?))
(s/def ::mtime-max integer?)
(s/def ::hash-sha256 (s/nilable (s/and string? #(= 64 (count %)))))
(s/def ::source (s/keys :req-un [::paths ::mtime-max ::hash-sha256]))

(s/def ::provenance
  (s/keys :req-un [::op ::started-at-ms ::finished-at-ms ::duration-ms]
    :opt-un [::tool ::extension ::source]))

(s/def ::type (s/and string? #(not (str/blank? %))))
(s/def ::message string?)
(s/def ::class string?)
(s/def ::method string?)
(s/def ::file string?)
(s/def ::line pos-int?)
(s/def ::origin #{:user-code :tool :runtime :library})
(s/def ::trace-frame (s/keys :req-un [::class ::method ::file ::origin]
                       :opt-un [::line]))
(s/def ::trace (s/coll-of ::trace-frame :kind vector?))
(s/def ::error-map (s/keys :req-un [::type ::message ::trace]))
(s/def ::error (s/nilable ::error-map))

(s/def ::tool-result-base
  (s/keys :req-un [::ok? ::result ::result-shape ::provenance ::markdown ::error]))

(s/def ::tool-result
  (s/and
    ::tool-result-base
    (fn [{:keys [ok? error]}]
      (if ok?
        (nil? error)
        (some? error)))))

(defn tool-result?
  [x]
  (s/valid? ::tool-result x))

(defn assert-tool-result!
  [x]
  (when-not (tool-result? x)
    (throw (ex-info "Invalid tool result"
             {:type :vis/invalid-tool-result
              :value x
              :explain (s/explain-data ::tool-result x)})))
  x)

(defn with-presentation
  "Attach runtime-only presentation hints. The CONTRACT lives in the
   value; metadata is only for render policy."
  [x presentation]
  (with-meta x (assoc (meta x) :vis/presentation presentation)))

(defn presentation
  [x]
  (:vis/presentation (meta x)))

(defn normalize-provenance
  "Fill the common provenance clock keys when absent.

   Required by the tool-result contract:
     :op
     :started-at-ms
     :finished-at-ms
     :duration-ms

   Callers may pass richer maps (tool / extension / source metadata);
   this helper only normalizes the shared timing surface."
  [provenance]
  (let [provenance (or provenance {})
        t          (now-ms)
        started    (long (or (:started-at-ms provenance) t))
        finished   (long (or (:finished-at-ms provenance) t))
        duration   (long (or (:duration-ms provenance)
                           (max 0 (- finished started))))]
    (assoc provenance
      :started-at-ms started
      :finished-at-ms finished
      :duration-ms duration)))

(defn merge-provenance
  "Merge `extra` into an already-valid tool-result envelope, re-check
   the contract, and preserve metadata. Used by the extension wrapper
   to stamp extension/source provenance onto tool-like returns."
  [tool-result extra]
  (let [meta*  (meta tool-result)
        merged (-> tool-result
                 (update :provenance #(merge (or % {}) extra))
                 assert-tool-result!)]
    (with-meta merged meta*)))

(defn- simple-shape
  [v depth]
  (cond
    (neg? depth) {:type :unknown}
    (nil? v) {:type :nil}
    (string? v) {:type :string :chars (count v) :lines (count (str/split-lines v))}
    (boolean? v) {:type :boolean}
    (integer? v) {:type :int}
    (float? v) {:type :float}
    (keyword? v) {:type :keyword}
    (symbol? v) {:type :symbol}
    (map? v) {:type :map
              :count (count v)
              :keys (->> (keys v) (take 12) vec)
              :shape (into {}
                       (map (fn [[k vv]] [k (simple-shape vv (dec depth))])
                         (take 8 v)))}
    (vector? v) {:type :vector
                 :count (count v)
                 :items (when-let [x (first v)] (simple-shape x (dec depth)))}
    (set? v) {:type :set
              :count (count v)
              :items (when-let [x (first (seq v))] (simple-shape x (dec depth)))}
    (sequential? v) {:type :seq
                     :count (count (take 128 v))
                     :items (when-let [x (first v)] (simple-shape x (dec depth)))}
    :else {:type :object
           :class (.getName (class v))}))

(defn result-shape
  [v]
  (simple-shape v 2))

(defn- frame-origin
  [^StackTraceElement frame]
  (let [class-name (.getClassName frame)
        file-name  (.getFileName frame)
        method     (.getMethodName frame)]
    (cond
      (or (= class-name "user")
        (= class-name "sandbox")
        (= file-name "iteration")
        (= method "anonymous-fn"))
      :user-code

      (str/starts-with? class-name "com.blockether.vis.ext.")
      :tool

      (str/starts-with? class-name "com.blockether.vis.internal.")
      :runtime

      :else
      :library)))

(defn- noisy-frame?
  [^StackTraceElement frame]
  (let [class-name (.getClassName frame)]
    (or (str/starts-with? class-name "sci.impl.")
      (str/starts-with? class-name "sci.ctx_store")
      (str/starts-with? class-name "clojure.lang.AFn")
      (str/starts-with? class-name "clojure.lang.RestFn")
      (str/starts-with? class-name "clojure.lang.MultiFn")
      (str/starts-with? class-name "clojure.lang.Var")
      (str/starts-with? class-name "java.lang.reflect.")
      (str/starts-with? class-name "jdk.internal.reflect."))))

(defn normalize-trace
  [^Throwable t]
  (->> (.getStackTrace t)
    (remove noisy-frame?)
    (map (fn [^StackTraceElement frame]
           (cond-> {:class  (.getClassName frame)
                    :method (.getMethodName frame)
                    :file   (or (.getFileName frame) "unknown")
                    :origin (frame-origin frame)}
             (pos? (.getLineNumber frame))
             (assoc :line (.getLineNumber frame)))))
    (take max-trace-frames)
    vec))

(defn normalize-error
  [^Throwable t]
  {:type    (.getName (class t))
   :message (or (ex-message t) "")
   :trace   (normalize-trace t)})

(defn success
  [{:keys [result provenance markdown] :as m}]
  (-> {:ok?          true
       :result       result
       :result-shape (or (:result-shape m) (result-shape result))
       :provenance   (normalize-provenance provenance)
       :markdown     markdown
       :error        nil}
    assert-tool-result!))

(defn failure
  [{:keys [result provenance markdown error throwable] :as m}]
  (let [err (or error
              (when throwable (normalize-error throwable)))]
    (-> {:ok?          false
         :result       result
         :result-shape (or (:result-shape m) (result-shape result))
         :provenance   (normalize-provenance provenance)
         :markdown     markdown
         :error        err}
      assert-tool-result!)))
