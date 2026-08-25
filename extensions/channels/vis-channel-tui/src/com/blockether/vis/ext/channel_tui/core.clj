(ns com.blockether.vis.ext.channel-tui.core
  "Lightweight TUI channel registration.

   Keep this namespace tiny: the distribution manifest initializes it on every Vis startup.
   The full Lanterna screen implementation is resolved only when the TUI
   channel actually runs.

   Startup boundary:
     This namespace never contacts the gateway. It resolves the full Lanterna
     screen only when the TUI channel runs, then hands every argument through.
     Session lookup belongs to the screen's post-first-frame worker so even a
     cold gateway cannot leave the terminal blank."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.builtin-hooks :as builtin-hooks]))

(def tui-usage
  "vis-agent [--gateway HOST[:PORT] --gateway-token TOKEN] channels tui [--session-id ID | --resume | --continue]")

(defn render-for-tui
  "Project canonical typed content blocks to Markdown for the TUI."
  ([blocks] (render-for-tui blocks nil))
  ([blocks _opts]
   (when-not (vector? blocks)
     (throw (ex-info "render-for-tui requires canonical content blocks"
                     {:got-type (some-> blocks
                                        class
                                        .getName)})))
   (->> blocks
        (keep
          (fn [block]
            (case (get block "type")
              "prose"
              (get block "markdown")

              "code"
              (str "```" (or (get block "language") "") "\n" (get block "text" "") "\n```")

              "reasoning"
              (get block "text")

              ;; Mirrors `chat/content->markdown`: the machine code on its own
              ;; line, the message (a whole provider card for a provider failure)
              ;; as the next paragraph. A NOTICE is prose for a human and prints
              ;; its message alone - lumping it in with the error branch made
              ;; this projection shout `**turn_cancelled**` at someone who had
              ;; just pressed Esc, the very thing `chat/content->markdown`
              ;; stopped doing.
              "error"
              (let [message
                    (get block "message")

                    code
                    (get block "code")]

                (if (and (seq code) (seq message)) (str "**" code "**\n\n" message) message))

              "notice"
              (get block "message")

              "tool"
              (some-> (get block "output")
                      str)

              nil)))
        (str/join "\n\n"))))

(defn- require-screen-channel-main
  "Resolve the heavyweight screen channel entry point without doing gateway work.
   Arguments, including `--session-id`, stay untouched so the screen can paint
   before its deferred startup worker resolves the requested session."
  []
  (or (requiring-resolve 'com.blockether.vis.ext.channel-tui.screen/channel-main)
      (throw (ex-info "TUI screen channel entry point did not resolve"
                      {:type :channel-tui/missing-screen-main}))))

(defn channel-main
  "Lazy channel entry point. Loading the Lanterna screen stack is deferred until
   the TUI channel is invoked; all gateway work begins after its first paint."
  [args]
  ((require-screen-channel-main) args))

(def tui-extension
  (vis/extension {:ext/name "channel-tui"
                  :ext/description "Lanterna-based terminal UI channel."
                  :ext/version "0.3.0"
                  :ext/author "Blockether"
                  :ext/owner "vis"
                  :ext/license "Apache-2.0"
                  :ext/channels [{:channel/id :tui
                                  :channel/cmd "tui"
                                  :channel/doc "Interactive terminal UI."
                                  :channel/usage tui-usage
                                  :channel/owns-tty? true
                                  :channel/main-fn #'channel-main
                                  :channel/messages-renderer-fn #'render-for-tui}]
                  :ext/channel-contributions builtin-hooks/channel-contributions}))

(defn register! [] (vis/register-extension! tui-extension))
