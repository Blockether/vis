(ns com.blockether.vis.test-builtins
  "Lazytest hook of the `:test` alias that registers the built-in extensions."
  (:require [lazytest.hooks :as hooks]))

(hooks/defhook
  builtins
  "Register the built-in extensions first, as the engine does at startup. A test
   that triggers the first registration inside a temporary registry loses it for
   the whole process, because the manifest registers each entry only once."
  (pre-test-run [_config m]
                ((requiring-resolve 'com.blockether.vis.internal.extension.manifest/initialize!))
                m))
