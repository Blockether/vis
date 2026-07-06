(ns com.blockether.vis.ext.channel-web.tool-color-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-web.core :as core]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private tool-color-var @#'core/tool-color-var)

(defdescribe tool-color-role-coverage-test
             (it "the web badge colour map covers every canonical vis/tool-color-roles role"
                 ;; Guard against drift: the role list lives once in vis core; if a new role is
                 ;; added there, this fails until the web map handles it (mirror of the TUI test).
                 (doseq [role vis/tool-color-roles]
                   (expect (some? (get tool-color-var role))
                           (str role " has no web badge colour — add it to core/tool-color-var")))))
