(ns com.blockether.vis.extension
  "The extension-authoring API: build an extension map, declare its sandbox
   symbols and values, register its feature toggles and register the extension.

   It depends only on the extension and toggle registries, so a namespace that
   ships a built-in extension requires it instead of `com.blockether.vis.core`,
   which loads the whole engine. The facade re-exports every name here, so code
   written against `vis/extension` or `vis/register-extension!` keeps working.

       (require '[com.blockether.vis.extension :as ext])

       (def my-extension
         (ext/extension {:ext/name \"my-tools\" ... :ext/symbols [(ext/symbol #'my-tool {...})]}))

       (defn register! [] (ext/register-extension! my-extension))"
  (:refer-clojure :exclude [symbol])
  (:require [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.import :refer [import-vars]]))

(defmacro extension
  "Build extension spec and stamp caller namespace for reload/source tracking."
  [spec]
  `(extension/extension (assoc ~spec :ext/source-nses ['~(ns-name *ns*)])))

(import-vars [symbol extension/symbol]
             [value extension/value]
             [render-prompt extension/render-prompt]
             [register-extension! extension/register-extension!]
             [register-toggle! toggles/register-toggle!])
