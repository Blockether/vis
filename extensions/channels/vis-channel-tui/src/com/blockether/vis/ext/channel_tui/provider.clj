(ns com.blockether.vis.ext.channel-tui.provider
  "The TUI's provider surface: the model picker, and every provider verb offered
   as a magit transient INSIDE the frame it was fired from (Settings › Providers).
   There is no second provider manager — one surface, one set of verbs.
   Config I/O and data helpers live in tui/config.clj.

   The channel-neutral brain — status probing, limits, live model
   catalogs, presets, persistence shapes — lives in
   `com.blockether.vis.internal.providers` (exposed through `vis.core`)
   and can be SHARED across channels. This namespace owns only the
   lanterna interaction layer.

   ALL provider OAuth is driven ENTIRELY through the gateway —
   Anthropic + Codex over browser/PKCE, GitHub Copilot over device code —
   via `/v1/providers/:id/auth/{start,complete,poll,cancel}`, and every question
   those flows ask (the device code, the pasted redirect URL, the API-key field)
   is a BAND in the caller's own frame. Teardown is ONE verb — `DELETE
   /v1/providers/:id` — because the daemon runs the extension's own logout there
   and drops the config entry in the same step, exactly as the companion's Remove
   does.
   The TUI therefore needs NO provider extension on its own classpath, holds
   no credential secret at any moment, and behaves identically when attached
   to a gateway on another machine."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.internal.external-opener :as opener])
  (:import [com.googlecode.lanterna.screen TerminalScreen]))

(set! *unchecked-math* :warn-on-boxed)

;;; ── Model list (core service + the TUI's 'Show all' affordance) ────────────

(defn- same-id?
  [a b]
  (= (some-> a
             name)
     (some-> b
             name)))

(defn- move-matches-first
  "Stable partition used only for presentation; non-matches keep their order."
  [pred xs]
  (let [{matches true others false} (group-by (comp boolean pred) xs)]
    (into (vec matches) others)))


(defn- build-model-list
  "Build the model selection list from the gateway's LIVE catalog.
   Preferred/default models are presented first. Appends the 'Show all models...'
   toggle when dated variants were hidden."
  [provider preferred-models show-all?]
  (let
    [{:keys [models hidden-count]}
     (vis/gateway-provider-model-options (:id provider) show-all?)

     preferred
     (into #{} (map #(if (map? %) (vis/model-name %) (str %))) preferred-models)

     items
     (->> models
          (mapv (fn [id]
                  {:label id :id id}))
          (move-matches-first #(contains? preferred (:id %))))]

    (if (and (not show-all?) (pos? (long hidden-count)))
      (conj items {:label "Show all models..." :id :show-all})
      items)))



(def ^:private default-model-configs vis/provider-default-model-configs)

(def ^:private provider-config-with-models vis/provider-config-with-models)

;;; ── Provider OAuth — every flow runs through the gateway ─────────────

(def ^:private github-copilot-account-types
  {:github-copilot-individual :individual
   :github-copilot-business :business
   :github-copilot-enterprise :enterprise})

(defn- github-copilot-provider? [provider-id] (contains? github-copilot-account-types provider-id))

(defn- gateway-authenticated?
  "Ask the DAEMON whether `provider-id` already holds usable credentials.

   The TUI must not read the auth file itself: the gateway may be on another
   machine, and it is the one process that owns credential resolution."
  [provider-id]
  (try (boolean (get (vis/gateway-provider-status provider-id) "is_authenticated"))
       (catch Exception _ false)))


(def ^:private device-wait-timeout-ms (* 6 60 1000))

(def ^:private device-auth-cancelled ::device-auth-cancelled)

(defn- cancel-device-poll!
  [result]
  (when (instance? java.util.concurrent.Future result)
    (.cancel ^java.util.concurrent.Future result true)))

(defn- wait-for-device-auth!
  "HOLD the caller's band while the daemon polls for the device verdict. Esc gives
   up, and so does the wall clock; either way the poll is cancelled and the caller
   reads it as a cancel."
  [q label result]
  (let
    [started-at-ms
     (System/currentTimeMillis)

     deadline-ms
     (+ started-at-ms (long device-wait-timeout-ms))

     finished?
     ((:wait! q)
       (str label " — waiting for authorization")
       (fn []
         (str "Finish the login in the browser · "
              (quot (- (System/currentTimeMillis) started-at-ms) 1000)
              "s"))
       (fn []
         (or (realized? result) (>= (System/currentTimeMillis) deadline-ms))))]

    (if (and finished? (realized? result))
      @result
      (do (cancel-device-poll! result)
          (when finished?
            ((:note! q) label "Timed out waiting for authorization — start the sign-in again."))
          device-auth-cancelled))))

(defn device-auth-transient-spec
  "PURE: the band a DEVICE-code sign-in paints. The two things the user must SEE —
   the code to type and the URL to type it into — are the group headings over the
   ONE key each, and `w` then holds the band while the daemon polls. `status` is
   what the last keystroke left behind (`Copied the code.`), so the band reports
   itself instead of stacking a toast over the code the user is still reading."
  [verification-uri user-code status]
  {:groups [{:title (str "Code  " user-code)
             :items [{:key "c" :type :action :id :copy :label "Copy the code"}]}
            {:title (str verification-uri)
             :items [{:key "o" :type :action :id :open :label "Open the URL in a browser"}]}
            (cond->
              {:items
               [{:key "w" :type :action :id :wait :label "Wait — I authorized in the browser"}]}
              (not (str/blank? (str status)))
              (assoc :title (str status)))]})

(defn- device-auth-band!
  "Show the device code IN THE CALLER'S BAND and let the terminal act on it.
   Returns true when the user says they authorized, nil on Esc."
  [q label verification-uri user-code]
  (loop [status nil]
    (case
      (:action ((:transient! q)
                 (assoc (device-auth-transient-spec verification-uri user-code status)
                   :title label)))
      :copy
      (do (input/clipboard-copy! user-code) (recur "Copied the device code to the clipboard."))

      :open
      (do (opener/open! verification-uri) (recur "Opened the browser."))

      :wait
      true

      nil)))

(defn- gateway-device-login!
  "Run one DEVICE-code OAuth flow for `provider-id` THROUGH THE GATEWAY, entirely
   inside the CALLER'S band.

   `POST auth/start` mints the flow daemon-side and returns only what the user
   must SEE (verification URI + user code); the device code, the token exchange
   and the credential file all stay in the daemon. This leg shows the code and
   asks `auth/poll` for the verdict — exactly what the phone app does, so a TUI
   attached to a REMOTE gateway signs in on the right machine.

   Returns true on success, nil on cancel or failure (the band already said so)."
  ([q provider-id label] (gateway-device-login! q provider-id label false))
  ([q provider-id label force?]
   (if (and (not force?) (gateway-authenticated? provider-id))
     true
     (try
       (let
         [flow
          (vis/gateway-provider-auth-start! provider-id)

          flow-id
          (get flow "flow_id")

          uri
          (or (get flow "verification_uri") (get flow "url"))

          user-code
          (get flow "user_code")

          interval-ms
          (max 1000 (long (or (get flow "interval_ms") 5000)))]

         (cond (not (and flow-id uri user-code))
               (do ((:note! q) label "No device code came back from vis.") nil)
               (not (device-auth-band! q label uri user-code))
               (do (vis/gateway-provider-auth-cancel! provider-id flow-id) nil)
               :else (let
                       [poll
                        (vis/worker-future
                          "vis-tui-device-auth-poll"
                          #(loop []

                             (let [verdict (vis/gateway-provider-auth-poll! provider-id flow-id)]
                               (if (= "pending" (get verdict "status"))
                                 (do (Thread/sleep interval-ms) (recur))
                                 verdict))))

                        verdict
                        (wait-for-device-auth! q label poll)]

                       (cond (= device-auth-cancelled verdict)
                             (do (vis/gateway-provider-auth-cancel! provider-id flow-id) nil)
                             ;; Success is silent: an "Authenticated!" toast over the
                             ;; band that just closed is the noise the user vetoed.
                             (= "ok" (get verdict "status")) true
                             :else (do ((:note! q)
                                         label
                                         (str "Auth failed: "
                                              (or (get verdict "message") "authorization failed")))
                                       nil)))))
       (catch Exception e
         ((:note! q)
           label
           (str "Auth failed: " (ex-message e)
                " — fallback: vis-agent providers auth " (name provider-id)))
         nil)))))

(defn- gateway-pkce-login!
  "Run one browser (PKCE) OAuth flow for `provider-id` THROUGH THE GATEWAY, in the
   CALLER'S band.

   `POST auth/start` mints the flow daemon-side and returns only the
   authorization URL plus an opaque flow id — the PKCE verifier never reaches
   this process. The user finishes in a browser, pastes the final redirect URL
   back, and `POST auth/complete` exchanges and persists the credentials in the
   daemon. So a TUI attached to a REMOTE gateway signs in exactly like the phone
   app does, and no channel needs the provider extension on its own classpath.

   Returns true on success, nil on cancel or failure (the band already said so)."
  [q provider-id label]
  (try (let
         [flow
          (vis/gateway-provider-auth-start! provider-id)

          flow-id
          (get flow "flow_id")

          url
          (get flow "url")]

         (if-not (and flow-id url)
           (do ((:note! q) label "No authorization URL came back from vis.") nil)
           (do (opener/open! url)
               (let
                 [pasted
                  ((:read! q) (str label " — paste the final browser URL:") {:placeholder url})

                  input
                  (some-> pasted
                          str/trim)]

                 (if (str/blank? input)
                   (do (vis/gateway-provider-auth-cancel! provider-id flow-id) nil)
                   (do (vis/gateway-provider-auth-complete! provider-id flow-id input)
                       ;; Success is silent: parity with the copilot flow.
                       true))))))
       (catch Exception e
         ((:note! q)
           label
           (str "Auth failed: " (ex-message e)
                " — fallback: vis-agent providers auth " (name provider-id)))
         nil)))

(defn- codex-oauth-ready!
  "Run OpenAI Codex browser OAuth from the TUI when needed.

   The GATEWAY owns the flow end to end (see `gateway-pkce-login!`); the TUI
   only opens the browser and collects the pasted redirect URL. With `force?`,
   start a fresh OAuth flow even when credentials already exist."
  ([q] (codex-oauth-ready! q false))
  ([q force?]
   (if (and (not force?) (gateway-authenticated? :openai-codex))
     true
     (when ((:confirm! q)
             "Start the ChatGPT/Codex browser sign-in?"
             {:cost "Vis opens a browser; the final redirect URL is pasted back here."
              :yes-label "Yes, open the browser"
              :no-label "Not now"})
       (boolean (gateway-pkce-login! q :openai-codex "OpenAI Codex"))))))

(defn- anthropic-oauth-ready!
  "Run Anthropic Claude subscription browser OAuth from the TUI when needed.

   Gateway-driven, exactly like Codex — see `gateway-pkce-login!`."
  ([q] (anthropic-oauth-ready! q false))
  ([q force?]
   (if (and (not force?) (gateway-authenticated? :anthropic-coding-plan))
     true
     (when ((:confirm! q)
             "Start the Anthropic Claude subscription sign-in?"
             {:cost "Vis opens a browser; the final redirect URL is pasted back here."
              :yes-label "Yes, open the browser"
              :no-label "Not now"})
       (boolean (gateway-pkce-login! q :anthropic-coding-plan "Anthropic"))))))



;;; ── Reuse dialog infrastructure from dialogs.clj ───────────────────────────
;; dlg/dlg/draw-dialog-chrome!, dlg/dlg/dialog-layout, dlg/dlg/draw-hint-bar!,
;; dlg/dlg/ellipsize, p/clamp, dlg/visible-window-start, dlg/clear-screen!








(defn- tagged?
  "True when `provider` carries the tag in `selection` — either role's
   provider/model pair. An untagged fleet has no `:provider-id`, and must never
   match a provider."
  [provider selection]
  (boolean (and (some? (:provider-id selection))
                (same-id? (:id provider) (:provider-id selection)))))




;; Channel-neutral status / limits / persistence shapes — the core
;; provider service (channel-neutral). Aliased privately so
;; the dialog code below reads unchanged.
(def ^:private local-no-auth-provider-ids vis/provider-local-no-auth-ids)

(defn- save-provider-config!
  "Persist the current provider set THROUGH THE CORE fleet write, then return the
   reloaded domain config. Credentials are the DAEMON's (`auth/complete` writes
   them), so every row is merged ONTO its persisted entry: fields the TUI does not
   carry — notably `:api-key` — survive the write.

   The write itself belongs to `vis/save-config-providers!`, not to a raw config
   assoc here: the core write is what re-points the default root at a fleet that
   just gained its first provider, and what rebuilds the shared router — a fleet
   the TUI wrote behind its back stayed untagged and unrouted until something else
   happened to save."
  [items]
  (let [persisted (into {} (map (juxt :id identity)) (vis/configured-providers))]
    (vis/save-config-providers! (mapv #(merge (get persisted (:id %)) %) items))
    (vis/load-config)))



(def gateway-probe-timeout-ms
  "Wall for ONE gateway provider diagnostics call, in milliseconds.

   A provider's `detect_fn`/`status_fn` is extension code and may block for as
   long as it likes; the terminal must not. Every probe runs on a worker thread
   and is abandoned here, so a wedged extension costs ONE card its verdict
   instead of freezing the key/paint loop until the gateway client's own
   request budget expires."
  6000)

(defn- probe-within
  "Run `f` on a worker future and give up after `gateway-probe-timeout-ms`,
   answering `(on-timeout message)` rather than blocking the caller further."
  [label f on-timeout]
  (let
    [fut
     (vis/worker-future label f)

     value
     (deref fut gateway-probe-timeout-ms ::timed-out)]

    (if (identical? ::timed-out value)
      (do (.cancel ^java.util.concurrent.Future fut true)
          (on-timeout (str "timed out after " gateway-probe-timeout-ms "ms")))
      value)))

(defn- gateway-provider-status-safe
  [provider]
  (probe-within "vis-tui-provider-status-probe"
                (fn []
                  (try (vis/gateway-provider-status (:id provider))
                       (catch Throwable e
                         {"is_authenticated" false "error" (or (ex-message e) (str e))})))
                (fn [message]
                  {"is_authenticated" false "error" message})))

(defn- gateway-provider-limits-safe
  [provider]
  (probe-within "vis-tui-provider-limits-probe"
                (fn []
                  (try (vis/gateway-provider-limits (:id provider))
                       (catch Throwable e
                         {:provider-id (:id provider)
                          :status :error
                          :static {}
                          :dynamic {:limits []}
                          :error {:message (or (ex-message e) (str e))}})))
                (fn [message]
                  {:provider-id (:id provider)
                   :status :error
                   :static {}
                   :dynamic {:limits []}
                   :error {:message message}})))





(defn- provider-authenticated?
  ([provider] (boolean (get (gateway-provider-status-safe provider) "is_authenticated")))
  ([_provider status] (boolean (get status "is_authenticated"))))

(defn show-provider-status!
  "Status + limits as the RICH canonical markdown form, painted through the IR
   walker — the same report the web renders as markdown. The fallback arity
   fetches diagnostics through the gateway, never through local provider OAuth."
  ([^TerminalScreen screen provider]
   (show-provider-status! screen
                          provider
                          (gateway-provider-status-safe provider)
                          (gateway-provider-limits-safe provider)))
  ([^TerminalScreen screen provider status limits]
   (dlg/markdown-viewer-dialog! screen
                                (str (vis/display-label (:id provider)) " Status & Limits")
                                (vis/provider-status-md provider status limits))))

(defn- provider-supports-auth?
  [provider]
  (not (contains? local-no-auth-provider-ids (:id provider))))


(defn provider-action-items
  "Actions for one provider row.

   `is-fallback` (the row already carries the FALLBACK tag) is what adds
   `:clear-fallback`; `is-default` (the row holds the PRIMARY tag) is what
   REMOVES `:fallback`. The daemon refuses a fallback naming the primary's own
   provider, so offering that action on the primary's card is a guaranteed
   rejection dialog — the web settings panel disables the same button for the
   same reason. Pass both values to the painter and the key handler or the two
   menus disagree."
  ([provider] (provider-action-items provider (gateway-provider-status-safe provider)))
  ([provider status] (provider-action-items provider status false))
  ([provider status is-fallback] (provider-action-items provider status is-fallback false))
  ([provider status is-fallback is-default]
   (let
     [registered
      (vis/provider-by-id (:id provider))

      is-authenticated
      (provider-authenticated? provider status)

      auth-label
      (if is-authenticated "Re-authenticate" "Authenticate")]

     (->
       (cond-> [{:id :default :label "Set as Default..." :key \d}]
         (not is-default)
         (conj {:id :fallback :label "Set as Fallback..." :key \f})

         is-fallback
         (conj {:id :clear-fallback :label "Clear Fallback" :key \c})

         (provider-supports-auth? provider)
         (conj {:id :authenticate :label auth-label :key \a :force? is-authenticated})

         (or (:provider/status-fn registered) (:provider/detect-fn registered) (:api-key provider))
         (conj {:id :status :label "Show Status + Limits" :key \s}))
       ;; Removal is the ONE teardown a provider has, the same verb the companion
       ;; offers: the daemon runs the extension's own logout AND drops the config
       ;; entry, so nothing survives to resurrect the row as an authenticated
       ;; preset. A separate "log out" only ever left that ghost behind — and a
       ;; fleet you can only ADD to is how a provider nobody wants stays on the
       ;; screen forever.
       (conj {:id :remove :label "Remove Provider" :key \x})))))

(def ^:private routing-action-ids
  "The actions that re-point the ROUTER. Magit groups a popup's commands by what
   they touch, and these are the ones that change where requests go."
  #{:default :fallback :clear-fallback})

(defn provider-transient-spec
  "PURE: the magit transient spec for ONE provider row's `actions` (whatever
   `provider-action-items` offered). Every action keeps the SINGLE key it already
   advertised, so the popup is driven by direct keystrokes — `d`, `f`, `a` — with
   no cursor at all, and the commands are grouped the way magit groups a popup:
   routing first, then the account verbs. A group with no surviving action is
   dropped, so a provider that cannot authenticate never shows an empty heading."
  [actions]
  (let
    [group (fn [title pred]
             (when-let
               [items (seq (into []
                                 (comp (filter pred)
                                       (map (fn [action]
                                              {:key (str (:key action))
                                               :type :action
                                               :id (:id action)
                                               :label (:label action)})))
                                 actions))]
               {:title title :items (vec items)}))]
    {:groups (into []
                   (remove nil?)
                   [(group "Routing" #(contains? routing-action-ids (:id %)))
                    (group "Account" #(not (contains? routing-action-ids (:id %))))])}))

(def ^:private model-transient-keys
  "Single-key bindings the model transient hands out, in order. Magit's own
   paging keys (`n` / `p`) are held back so a model can never shadow them."
  "abcdefghijklmoqrstuvwxyz")

(defn model-transient-page-size
  "PURE: how many models one transient page holds inside `rows` usable body rows.
   Reserves the popup's own chrome — the leading blank, the `Models` header and
   the `Commands` group carrying paging plus `Show every model` — and never asks
   for more single-key bindings than exist."
  [rows]
  (p/clamp (- (long rows) 7) 1 (count model-transient-keys)))

(defn- paged-key-groups
  "PURE: one PAGE of `items` (`{:id :label}`) as a magit group whose every row is
   bound to a single letter, plus the `Commands` group that pages it.

   Every picker in this dialog family — models, presets — is the same shape: too
   many choices for one screen, each chosen with ONE keystroke, `n` / `p` between
   pages. `:commands` are the caller's own extra commands, appended after paging."
  [{:keys [title items page page-size commands]}]
  (let
    [page-size
     (long
       (p/clamp (long (or page-size (count model-transient-keys))) 1 (count model-transient-keys)))

     items
     (vec items)

     pages
     (max 1 (quot (+ (count items) (dec page-size)) page-size))

     page
     (long (p/clamp (long page) 0 (dec pages)))

     from
     (min (count items) (* page page-size))

     window
     (subvec items from (min (count items) (+ from page-size)))

     commands
     (-> (if (> pages 1)
             [{:key "n" :type :action :id ::next-page :label "Next page"}
              {:key "p" :type :action :id ::prev-page :label "Previous page"}]
             [])
         (into commands))]

    {:groups (cond->
               [{:title (if (> pages 1) (str title "  " (inc page) "/" pages) title)
                 :items (into []
                              (map-indexed (fn [i item]
                                             (assoc item
                                               :key (str (nth model-transient-keys i))
                                               :type :action)))
                              window)}]
               (seq commands)
               (conj {:title "Commands" :items commands}))}))

(defn model-transient-spec
  "PURE: the magit transient spec for the model picker. `entries` are
   `build-model-list` rows, and each real model becomes a COMMAND bound to one
   letter — a model is chosen with a single keystroke exactly like `d` sets the
   default, never with a cursor. Models past one page are reached with magit's
   `n` / `p`, and the `:show-all` sentinel becomes `*`. `marks` is
   `{:default id :fallback id}` for the models this provider already holds, so
   the tagged pair stays findable without reading the cards."
  ([entries page marks] (model-transient-spec entries page marks (count model-transient-keys)))
  ([entries page marks page-size]
   (let
     [models
      (into [] (remove #(= :show-all (:id %))) entries)

      show-all?
      (boolean (some #(= :show-all (:id %)) entries))

      labelled
      (mapv (fn [model]
              (let
                [id
                 (:id model)

                 role
                 (cond (and (some? (:default marks)) (= id (:default marks))) "  (default)"
                       (and (some? (:fallback marks)) (= id (:fallback marks))) "  (fallback)"
                       :else "")]

                {:id id :label (str (:label model) role)}))
            models)]

     (paged-key-groups {:title "Models"
                        :items labelled
                        :page page
                        :page-size page-size
                        :commands
                        (when show-all?
                          [{:key "*" :type :action :id :show-all :label "Show every model"}])}))))



(defn- run-paged-transient!
  "Run a `paged-key-groups` band until one of its items is chosen, paging in
   place on `n` / `p`. `spec-of` builds the spec for a page number. Returns the
   chosen id, or nil when the user backed out with Esc.

   Pages differ in height, and a shorter page must leave nothing of the taller
   one above it — and nothing of the HOST's own rows blanked either. The region's
   `:restore!` snapshot (taken once, before the flow's first band painted) puts
   the rows between the two band tops back the way the host drew them. Wiping
   down from `:min-row` instead took the host's content with it: setting a
   default or fallback model from Settings blanked the settings pane behind the
   popup, frame and all."
  [^TerminalScreen screen g geom title spec-of]
  (let [geom (dlg/host-band-region screen geom)]
    (loop [page 0]
      (let [picked (:action (dlg/embed-transient! screen g geom title (spec-of page)))]
        (cond (nil? picked) nil
              (= picked ::next-page) (recur (inc page))
              (= picked ::prev-page) (recur (dec page))
              :else picked)))))

(defn- run-model-transient!
  "Magit transient model picker for `provider`: one keystroke per model, `n` / `p`
   to page a long catalog, `*` to expand the models the gateway hid. Returns the
   chosen model id, or nil when the user backed out with Esc."
  [^TerminalScreen screen g geom provider entries preferred marks page-size]
  (let
    [geom
     (dlg/host-band-region screen geom)

     title
     (str (or (:label provider) (vis/display-label (:id provider))) " — models")]

    (loop [entries entries]
      (let
        [picked (run-paged-transient! screen
                                      g
                                      geom
                                      title
                                      #(model-transient-spec entries % marks page-size))]
        (if (= picked :show-all) (recur (build-model-list provider preferred true)) picked)))))

(defn api-key-hint
  "PURE: the ONE line of a provider's own auth guidance that answers `where do I
   get the key?` — the first line naming a URL, else the first line that says
   anything at all. The rest of that guidance is written for the CLI (`export …`,
   `vis-agent providers auth …`) and has no business in a band the user reached by
   another route entirely."
  [instructions]
  (let [lines (into [] (comp (map #(str/trim (str %))) (remove str/blank?)) instructions)]
    (or (first (filter #(str/includes? % "http") lines)) (first lines))))

(defn api-key-transient-spec
  "PURE: the magit transient an API-key sign-in runs. `k` reads the key INLINE on
   the band's own hint row (echoed as `*`, and the armed value renders as dots, so
   the credential never lands on screen), `a` submits it to the gateway, Esc
   cancels. No cursor to move, no full-screen prompt.

   `hint` is `api-key-hint` of the provider's own guidance — where to create the
   key — and it becomes the heading over the field, which is the only part of that
   guidance a band has room for and the only part that answers anything."
  ([] (api-key-transient-spec nil))
  ([hint]
   {:title "Sign in"
    :groups [{:title (if (str/blank? (str hint)) "Credential" (str hint))
              :items [{:key "k"
                       :type :option
                       :id :api-key
                       :label "API key"
                       :prompt "API key:"
                       :mask \*
                       :secret? true}]}
             {:title "Authenticate"
              :items [{:key "a" :type :action :id :submit :label "Sign in with this key"}]}]}))

(defn- read-api-key!
  "Run `api-key-transient-spec` IN THE CALLER'S BAND until a non-blank key is
   submitted (`k`, then `a`) or the user backs out with Esc. Submitting nothing
   simply re-arms the band. Returns the key, or nil on cancel."
  [q hint]
  (loop []

    (let [{:keys [action options]} ((:transient! q) (api-key-transient-spec hint))]
      (when (= :submit action)
        (or (some-> (:api-key options)
                    str
                    str/trim
                    not-empty)
            (recur))))))

(defn- gateway-api-key-login!
  "Authenticate a plain API-key provider THROUGH THE GATEWAY, in the CALLER'S band.

   `POST auth/start` mints an `api-key` flow daemon-side and returns the
   provider's OWN guidance lines (`:provider/auth-prompt-fn`), so the TUI needs
   neither the provider extension nor its env-var knowledge on this classpath.
   The user types the key here and `POST auth/complete` persists it in the
   DAEMON's config — the TUI never runs a provider's `:provider/auth-fn` and
   never writes a credential itself, exactly like the OAuth flows.

   Returns the provider row WITHOUT the key on success (the daemon holds the
   credential), nil on cancel or failure (the band already said so)."
  [q provider]
  (let
    [pid
     (:id provider)

     label
     (str (vis/display-label pid) " Authentication")]

    (try
      (let
        [flow
         (vis/gateway-provider-auth-start! pid)

         flow-id
         (get flow "flow_id")

         hint
         (api-key-hint (get flow "instructions"))]

        (if-not flow-id
          (do
            ((:note! q) label (str (vis/display-label pid) " cannot be authenticated through vis."))
            nil)
          (let
            [raw
             (read-api-key! q hint)

             api-key
             (some-> raw
                     str/trim
                     not-empty)]

            (if (nil? api-key)
              (do (vis/gateway-provider-auth-cancel! pid flow-id) nil)
              (do (vis/gateway-provider-auth-submit-key! pid flow-id api-key)
                  ;; The DAEMON persisted it. Hand the row back WITHOUT the key:
                  ;; a TUI attached to a REMOTE gateway must never write provider
                  ;; credentials into the config of the machine it happens to run
                  ;; on, and every later fleet write merges onto the persisted
                  ;; entry anyway.
                  (dissoc provider :api-key))))))
      (catch Exception e
        ((:note! q) label (str "Authentication failed: " (or (ex-message e) (str e))))
        nil))))

(defn authenticate-provider!
  "The ONE auth entry point for every channel action (a provider's transient,
   add-provider). EVERY kind goes through the gateway: device for GitHub Copilot,
   PKCE for Codex/Anthropic, `api-key` for everything else — and every question
   each of them asks is a BAND in the caller's own frame, never a window over the
   list it was fired from. No provider credential is ever exchanged or written in
   the TUI process."
  ([^TerminalScreen screen g region provider]
   (authenticate-provider! screen g region provider false))
  ([^TerminalScreen screen g region provider force?]
   (let
     [q
      (dlg/band-questions screen g (dlg/host-band-region screen region))

      pid
      (:id provider)

      label
      (vis/display-label pid)]

     (cond (vis/provider-command-minted? provider)
           ;; The credential is MINTED BY THE MACHINE: `api_key_command` runs per
           ;; request. Prompting for a key here would let a typed value silently
           ;; outrank the helper, so explain and refuse instead.
           (do ((:note! q)
                 (str label " Authentication")
                 (str label
                      " mints its own credential — its api_key_command helper runs for every"
                      " request, so change that instead of typing a key."))
               nil)
           (github-copilot-provider? pid) (when (gateway-device-login! q pid label force?) provider)
           (= :openai-codex pid) (when (codex-oauth-ready! q force?) provider)
           (= :anthropic-coding-plan pid) (when (anthropic-oauth-ready! q force?) provider)
           (= :ollama pid) nil
           (= :lmstudio pid) nil
           :else (gateway-api-key-login! q provider)))))

(defn- perform-remove!
  "Fleet removal for `provider` THROUGH THE GATEWAY. No dialogs — the caller owns
   confirmation and feedback.

   The daemon owns BOTH halves of a provider: its config entry and its credential.
   Dropping the entry locally while the token stays on disk is why a removed
   OAuth provider used to come back on the next open — it re-surfaced as an
   authenticated preset. Returns nil on success, or a human-readable failure."
  [provider]
  (try (vis/gateway-provider-remove! (:id provider))
       nil
       (catch Throwable t (or (not-empty (str (ex-message t))) (str t)))))

(defn remove-provider!
  "Confirm IN THE CALLER'S BAND, then drop `provider` from the fleet through the
   gateway — config entry and credential both.

   This is the ONE teardown a provider has, the verb the companion offers under
   the same name: the daemon runs the extension's own logout before it forgets the
   entry, so nothing survives to resurrect the row as an authenticated preset. The
   question, what saying yes COSTS and any refusal all paint INSIDE the frame the
   transient was fired from — a verb reached from a band must never answer with a
   window stacked on top of it.

   Returns true when the removal ran."
  [^TerminalScreen screen g region provider]
  (let
    [label
     (vis/display-label (:id provider))

     {:keys [confirm! note!]}
     (dlg/band-questions screen g region)]

    (when (confirm! (str "Remove " label "?")
                    {:cost (str "Signs out of " label
                                " on the gateway machine and drops its entry there"
                                " — every device paired with it loses the provider.")
                     :yes-label "Yes, remove"
                     :no-label "Keep it"})
      (if-let [err (perform-remove! provider)]
        (do (note! (str label " — remove failed") err) false)
        true))))

(defn- choose-router-model!
  "Pick the model that completes a routing tag for `provider` and hand the pair to
   the daemon: `:primary` re-points the default root, `:fallback` the second one.

   The model list is its OWN transient — a magit popup picks a command, and the
   command that needs an argument opens the popup that supplies it. A daemon
   refusal (a fallback naming the primary's own provider is a 400) explains
   itself and returns `::rejected` instead of killing the caller's loop; nil
   means the user backed out."
  [^TerminalScreen screen g region provider role default-selection fallback-selection]
  (let
    [current
     (if (= role :fallback) fallback-selection default-selection)

     preferred
     (when (same-id? (:id provider) (:provider-id current)) [(:model current)])

     rows
     (max 1 (- (long (:hint-row region)) (long (:min-row region)) 1))

     choice
     (run-model-transient!
       screen
       g
       region
       provider
       (build-model-list provider preferred false)
       preferred
       {:default (when (same-id? (:id provider) (:provider-id default-selection))
                   (:model default-selection))
        :fallback (when (tagged? provider fallback-selection) (:model fallback-selection))}
       (model-transient-page-size rows))]

    (when choice
      (try (if (= role :fallback)
             (vis/gateway-set-router-fallback! (:id provider) choice)
             (vis/gateway-set-router-default! (:id provider) choice))
           (catch Exception e
             ((:note! (dlg/band-questions screen g (dlg/host-band-region screen region)))
               (if (= role :fallback) "Fallback rejected" "Default rejected")
               (or (ex-message e) (str e)))
             ::rejected)))))

(defn provider-transient!
  "Run ONE provider's magit transient inside the CALLER's frame — the same
   commands a provider row offers on Enter, reachable straight from a
   Settings row so a provider needs no manager of its own.

   The fleet, the default tag and the fallback tag are re-read from the LIVE
   config on every call, so the popup reasons about what the daemon persisted and
   not about a snapshot the caller carried in. Returns true when something
   changed and the caller should reload its inventory."
  [^TerminalScreen screen g region provider-id]
  (let
    [region
     (dlg/host-band-region screen region)

     config
     (or (vis/load-config) {:providers []})

     base
     (vec (or (:providers config) []))

     configured-ids
     (into #{} (map :id) base)

     fleet
     (into base
           (remove #(contains? configured-ids (:id %)))
           (try (vis/authenticated-preset-providers) (catch Throwable _ nil)))

     provider
     (first (filter #(same-id? (:id %) provider-id) fleet))

     default-selection
     (vis/resolve-default-selection config fleet)

     fallback-selection
     (when-let [pid (:fallback-provider config)]
       {:provider-id (keyword (name pid)) :model (:fallback-model config)})]

    (when provider
      (let
        [actions
         (provider-action-items provider
                                (gateway-provider-status-safe provider)
                                (tagged? provider fallback-selection)
                                (tagged? provider default-selection))

         picked
         (:action (dlg/embed-transient! screen
                                        g
                                        region
                                        (str (vis/display-label (:id provider)) " — actions")
                                        (provider-transient-spec actions)))

         action
         (some #(when (= picked (:id %)) %) actions)]

        (case (:id action)
          (:default :fallback)
          (let
            [selection (choose-router-model! screen
                                             g
                                             region
                                             provider
                                             (if (= (:id action) :fallback) :fallback :primary)
                                             default-selection
                                             fallback-selection)]
            (boolean (and (some? selection) (not= selection ::rejected))))

          :clear-fallback
          (try (vis/gateway-set-router-fallback!)
               true
               (catch Exception e
                 ((:note! (dlg/band-questions screen g (dlg/host-band-region screen region)))
                   "Clear fallback failed"
                   (or (ex-message e) (str e)))
                 false))

          :authenticate
          (boolean (authenticate-provider! screen g region provider (:force? action)))

          :status
          (do (show-provider-status! screen provider) false)

          :remove
          (boolean (remove-provider! screen g region provider))

          false)))))

;;; ── Add a provider — a BAND in the caller's frame, never a stacked wizard ──

(defn preset-transient-spec
  "PURE: the band that picks WHICH provider to add — one keystroke per preset,
   `n` / `p` between pages. The same shape as the model picker, because it is the
   same question asked about a different list."
  ([presets page] (preset-transient-spec presets page (count model-transient-keys)))
  ([presets page page-size]
   (paged-key-groups {:title "Providers"
                      :items (mapv (fn [preset]
                                     {:id (:id preset)
                                      :label (or (:label preset) (str (:id preset)))})
                                   presets)
                      :page page
                      :page-size page-size})))

(defn local-setup-transient-spec
  "PURE: the band that confirms a LOCAL provider's endpoint before it is added.
   `u` types the base URL on the hint row and `a` adds the provider with whatever
   the line says — LM Studio and Ollama run wherever the user hosts them."
  [label base-url]
  {:groups [{:title (str label)
             :items [{:key "u" :type :option :id :base-url :label "Base URL" :prompt "Base URL:"}]}
            {:title "Commands"
             :items [{:key "a"
                      :type :action
                      :id :add
                      :label (str "Add with "
                                  (or (not-empty (str base-url)) "the preset URL"))}]}]})

(defn- band-page-size
  "How many keyed rows one band may show inside `region` — the rows between the
   first row it may touch and the host's hint bar."
  [region]
  (model-transient-page-size (max 1
                                  (- (long (:hint-row region)) (long (or (:min-row region) 0)) 1))))

(defn- read-local-base-url!
  "Run the endpoint band. Returns the URL to use, or nil when the user backed out."
  [^TerminalScreen screen g region preset]
  (let
    [{:keys [action options]}
     (dlg/embed-transient! screen
                           g
                           region
                           (str (:label preset) " — setup")
                           (assoc (local-setup-transient-spec (:label preset) (:base-url preset))
                             :read-option (dlg/region-option-reader screen g region)))]
    (when (= :add action)
      (or (some-> (:base-url options)
                  str
                  str/trim
                  (str/replace #"/+$" "")
                  not-empty)
          (:base-url preset)))))

(defn- add-preset-provider!
  "Finish adding `preset`: endpoint (local providers), sign-in, model — each step
   a band in the SAME frame. Persists the provider and returns its config, or nil
   when the user backed out anywhere along the way."
  [^TerminalScreen screen g region preset]
  (let
    [pid
     (:id preset)

     local?
     (contains? local-no-auth-provider-ids pid)

     oauth?
     (or (github-copilot-provider? pid) (= :openai-codex pid) (= :anthropic-coding-plan pid))

     base-url
     (if local? (read-local-base-url! screen g region preset) (:base-url preset))

     preset
     (assoc preset :base-url base-url)

     ;; Sign-in is a band here too: the device code, the pasted redirect URL and
     ;; the API-key field all paint in THIS frame, so the model band that follows
     ;; lands on the settings list the user never lost sight of.
     auth-ok?
     (and (or (not local?) (some? base-url))
          (or local?
              (some? (:api-key preset))
              (some? (authenticate-provider! screen g region preset))))

     config
     (when auth-ok?
       (if-let [oauth-models (when oauth? (not-empty (default-model-configs preset)))]
         (provider-config-with-models preset oauth-models)
         (let [preferred (vis/provider-default-model-names preset)]
           (when-let
             [model (run-model-transient! screen
                                          g
                                          region
                                          preset
                                          (build-model-list preset preferred false)
                                          preferred
                                          {}
                                          (band-page-size region))]
             (cond-> (provider-config-with-models preset [{:name model}])
               (and (string? (:api-key preset)) (not oauth?))
               (assoc :api-key (:api-key preset)))))))]

    (when config (save-provider-config! (conj (vec (vis/configured-providers)) config)) config)))

(defn add-provider-transient!
  "Add a provider as a BAND inside the caller's own frame: one keystroke picks the
   preset, a second band confirms a local endpoint, sign-in runs, a third band
   picks the model. No wizard, no stacked popups — the list behind it stays on
   screen the whole way, which is the whole point of a transient.

   Persists the provider and returns its config, or nil when nothing was added."
  ([^TerminalScreen screen g region]
   (add-provider-transient! screen g region (into #{} (map :id) (vis/configured-providers))))
  ([^TerminalScreen screen g region existing-ids]
   (let
     [existing
      (set existing-ids)

      available
      (vec (remove #(contains? existing (:id %)) (vis/provider-presets)))

      region
      (dlg/host-band-region screen region)]

     (if (empty? available)
       (do (dlg/embed-transient! screen
                                 g
                                 region
                                 "Add provider"
                                 {:groups [{:title "Nothing to add"
                                            :items [{:key "q"
                                                     :type :action
                                                     :id :done
                                                     :label
                                                     "Every provider is already configured"}]}]})
           nil)
       (when-let
         [pid (run-paged-transient! screen
                                    g
                                    region
                                    "Add provider"
                                    #(preset-transient-spec available % (band-page-size region)))]
         (add-preset-provider! screen g region (first (filter #(= pid (:id %)) available))))))))

