(ns com.blockether.vis.loop.redact
  "Regex-based secret redaction for logs and tool output.

   Applies pattern matching to mask API keys, tokens, and credentials
   before they reach log files. Short tokens (< 18 chars) are fully
   masked. Longer tokens preserve first 6 and last 4 chars for debugging.

   Inspired by Hermes Agent's redact.py."
  (:require [clojure.string :as str])
  (:import [java.io FilterOutputStream OutputStream PrintStream]
           [java.util.regex Pattern]))

;;; ── Masking ────────────────────────────────────────────────────────────────

(defn- mask-token
  "Mask a token string. Short (< 18 chars) -> '***'. Long -> 'prefix...tail'."
  [^String token]
  (if (< (.length token) 18)
    "***"
    (str (subs token 0 6) "..." (subs token (- (count token) 4)))))

;;; ── Known prefix patterns ──────────────────────────────────────────────────

(def ^:private prefix-patterns
  ["sk-[A-Za-z0-9_-]{10,}"            ;; OpenAI / OpenRouter / Anthropic (sk-ant-*)
   "ghp_[A-Za-z0-9]{10,}"             ;; GitHub PAT (classic)
   "github_pat_[A-Za-z0-9_]{10,}"     ;; GitHub PAT (fine-grained)
   "xox[baprs]-[A-Za-z0-9-]{10,}"     ;; Slack tokens
   "AIza[A-Za-z0-9_-]{30,}"           ;; Google API keys
   "pplx-[A-Za-z0-9]{10,}"            ;; Perplexity
   "fal_[A-Za-z0-9_-]{10,}"           ;; Fal.ai
   "fc-[A-Za-z0-9]{10,}"              ;; Firecrawl
   "bb_live_[A-Za-z0-9_-]{10,}"       ;; BrowserBase
   "gAAAA[A-Za-z0-9_=-]{20,}"         ;; Codex encrypted tokens
   "AKIA[A-Z0-9]{16}"                 ;; AWS Access Key ID
   "sk_live_[A-Za-z0-9]{10,}"         ;; Stripe secret (live)
   "sk_test_[A-Za-z0-9]{10,}"         ;; Stripe secret (test)
   "rk_live_[A-Za-z0-9]{10,}"         ;; Stripe restricted
   "SG\\.[A-Za-z0-9_-]{10,}"          ;; SendGrid
   "hf_[A-Za-z0-9]{10,}"              ;; HuggingFace
   "r8_[A-Za-z0-9]{10,}"              ;; Replicate
   "npm_[A-Za-z0-9]{10,}"             ;; npm
   "pypi-[A-Za-z0-9_-]{10,}"          ;; PyPI
   "dop_v1_[A-Za-z0-9]{10,}"          ;; DigitalOcean PAT
   "am_[A-Za-z0-9_-]{10,}"])          ;; AgentMail

(def ^:private prefix-re
  (Pattern/compile
    (str "(?<![A-Za-z0-9_-])("
      (str/join "|" prefix-patterns)
      ")(?![A-Za-z0-9_-])")))

;;; ── Contextual patterns ────────────────────────────────────────────────────

;; ENV assignment: API_KEY=value or API_KEY="value"
(def ^:private env-assign-re
  (Pattern/compile
    "([A-Z_]*(?:API_?KEY|TOKEN|SECRET|PASSWORD|PASSWD|CREDENTIAL|AUTH)[A-Z_]*)\\s*=\\s*(['\"]?)(\\S+)\\2"
    Pattern/CASE_INSENSITIVE))

;; JSON field: "apiKey": "value"
(def ^:private json-field-re
  (Pattern/compile
    "(\"(?:api_?[Kk]ey|token|secret|password|access_token|refresh_token|auth_token|bearer|secret_value|key_material)\")\\s*:\\s*\"([^\"]+)\""
    Pattern/CASE_INSENSITIVE))

;; Authorization header
(def ^:private auth-header-re
  (Pattern/compile "(Authorization:\\s*Bearer\\s+)(\\S+)" Pattern/CASE_INSENSITIVE))

;; Private key block
(def ^:private private-key-re
  (Pattern/compile "-----BEGIN[A-Z ]*PRIVATE KEY-----[\\s\\S]*?-----END[A-Z ]*PRIVATE KEY-----"))

;; DB connection string: postgres://user:PASSWORD@host
(def ^:private db-connstr-re
  (Pattern/compile
    "((?:postgres(?:ql)?|mysql|mongodb(?:\\+srv)?|redis|amqp)://[^:]+:)([^@]+)(@)"
    Pattern/CASE_INSENSITIVE))

;;; ── Main redaction fn ──────────────────────────────────────────────────────

(defn redact
  "Apply all redaction patterns to a string. Non-matching text passes through unchanged."
  [^String text]
  (when text
    (if (zero? (.length text))
      text
      (-> text
        ;; Known prefixes (sk-, ghp_, AIza, etc.)
        (as-> t (.replaceAll (.matcher prefix-re t)
                  (fn [mr] (mask-token (.group ^java.util.regex.MatchResult mr 1)))))
        ;; ENV assignments
        (as-> t (.replaceAll (.matcher env-assign-re t)
                  (fn [mr]
                    (let [^java.util.regex.MatchResult mr mr]
                      (str (.group mr 1) "=" (.group mr 2) (mask-token (.group mr 3)) (.group mr 2))))))
        ;; JSON fields
        (as-> t (.replaceAll (.matcher json-field-re t)
                  (fn [mr]
                    (let [^java.util.regex.MatchResult mr mr]
                      (str (.group mr 1) ": \"" (mask-token (.group mr 2)) "\"")))))
        ;; Auth headers
        (as-> t (.replaceAll (.matcher auth-header-re t)
                  (fn [mr]
                    (let [^java.util.regex.MatchResult mr mr]
                      (str (.group mr 1) (mask-token (.group mr 2)))))))
        ;; Private key blocks
        (as-> t (.replaceAll (.matcher private-key-re t) "[REDACTED PRIVATE KEY]"))
        ;; DB connection strings
        (as-> t (.replaceAll (.matcher db-connstr-re t)
                  (fn [mr]
                    (let [^java.util.regex.MatchResult mr mr]
                      (str (.group mr 1) "***" (.group mr 3))))))))))

;;; ── Redacting PrintStream ──────────────────────────────────────────────────

(defn redacting-print-stream
  "Wrap an OutputStream in a PrintStream that redacts secrets line-by-line."
  [^OutputStream out]
  (let [buf (StringBuilder.)
        flush-buf! (fn []
                     (when (pos? (.length buf))
                       (let [line (redact (.toString buf))]
                         (.write out (.getBytes line "UTF-8"))
                         (.flush out))
                       (.setLength buf 0)))
        filtered (proxy [FilterOutputStream] [out]
                   (write
                     ([b]
                      (if (instance? Integer b)
                        (do (.append buf (char (int b)))
                          (when (= (int b) (int \newline))
                            (flush-buf!)))
                        (let [^bytes bs b]
                          (.append buf (String. bs "UTF-8"))
                          (when (>= (.indexOf (.toString buf) "\n") 0)
                            (flush-buf!)))))
                     ([b off len]
                      (.append buf (String. ^bytes b (int off) (int len) "UTF-8"))
                      (when (>= (.indexOf (.toString buf) "\n") 0)
                        (flush-buf!))))
                   (flush []
                     (flush-buf!)
                     (.flush out)))]
    (PrintStream. filtered true "UTF-8")))
