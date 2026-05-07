(ns com.blockether.vis.ext.channel-tui.core
  "Lightweight TUI channel registration.

   Keep this namespace tiny: manifest discovery loads it on every Vis startup.
   The full Lanterna screen implementation is resolved only when the TUI
   channel actually runs."
  (:require [com.blockether.vis.core :as vis]))

(def tui-usage
  "vis channels tui [--conversation-id ID | --resume]")

(defn channel-main
  "Lazy channel entry point. Loading the Lanterna screen stack is deferred
   until the TUI channel is invoked, so command discovery/help does not pay
   full TUI class-loading cost."
  [args]
  ((or (requiring-resolve 'com.blockether.vis.ext.channel-tui.screen/channel-main)
     (throw (ex-info "TUI screen channel entry point did not resolve"
              {:type :channel-tui/missing-screen-main})))
   args))

(def tui-extension
  (vis/extension
    {:ext/namespace 'com.blockether.vis.ext.channel-tui.core
     :ext/doc       "Lanterna-based terminal UI channel."
     :ext/version   "0.3.0"
     :ext/author    "Blockether"
     :ext/owner     "vis"
     :ext/license   "Apache-2.0"
     :ext/channels  [{:channel/id        :tui
                      :channel/cmd       "tui"
                      :channel/doc       "Interactive terminal UI."
                      :channel/usage     tui-usage
                      :channel/owns-tty? true
                      :channel/main-fn   #'channel-main}]}))

(vis/register-extension! tui-extension)
