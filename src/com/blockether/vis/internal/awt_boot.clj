(ns com.blockether.vis.internal.awt-boot
  "One RUNTIME bootstrap for every Java2D user in vis (attachment conversion,
   the PIL shim, the matplotlib shim).

   A native image inherits nothing from build time: a top-level
   `(System/setProperty \"java.awt.headless\" ...)` runs while the image is being
   BUILT and is gone by the time the binary starts, so the first `createGraphics`
   in a shipped `vis` dies with `NoClassDefFoundError java/awt/event/InputEvent`
   (the platform toolkit -- LWCToolkit on macOS -- loads because nothing said
   headless). The repair therefore has to happen at RUNTIME, exactly once, before
   the first Graphics2D call -- which is what `ensure!` is for."
  (:import (java.awt GraphicsEnvironment)
           (java.io File)))

(defonce ^:private awt-ready!
  ;; GraalVM native-image: AWT works, but only once two JVM assumptions are
  ;; repaired at runtime.
  ;;
  ;;   1. There is no windowing session. Touching a Graphics2D without
  ;;      `java.awt.headless` loads the platform toolkit (LWCToolkit on macOS)
  ;;      and dies with NoClassDefFoundError java/awt/event/InputEvent.
  ;;   2. There is no `java.home`. `sun.awt.FontConfiguration` throws
  ;;      `java.lang.Error: java.home property not set` before it looks at
  ;;      anything else, so the FIRST glyph a renderer draws kills it. A
  ;;      directory that need not exist satisfies the check, and
  ;;      `sun.awt.fontconfig` then short-circuits the search for
  ;;      lib/fontconfig.bfc, which a native binary does not ship. The one-line
  ;;      config only has to parse: real families come from the platform font
  ;;      manager (342 of them on macOS), which is why the rendered bytes are
  ;;      identical to a JVM run.
  ;;
  ;; `java.home` is restored immediately -- the font manager reads it exactly
  ;; once, during this forced init, and nothing else in vis should see a fake
  ;; JDK home. On a real JVM this is a no-op beyond the headless hint.
  (delay
    (try (when (nil? (System/getProperty "java.awt.headless"))
           (System/setProperty "java.awt.headless" "true"))
         ;; No Dock icon / menu bar takeover on macOS.
         (when (nil? (System/getProperty "apple.awt.UIElement"))
           (System/setProperty "apple.awt.UIElement" "true"))
         (when (nil? (System/getProperty "java.home"))
           (let [cfg (doto (File/createTempFile "vis-fontconfig" ".properties") .deleteOnExit)]
             (spit cfg "version=1\nsequence.allfonts=alphabetic\nalphabetic.font=Helvetica\n")
             (when (nil? (System/getProperty "sun.awt.fontconfig"))
               (System/setProperty "sun.awt.fontconfig" (.getAbsolutePath cfg)))
             (System/setProperty "java.home"
                                 (str (System/getProperty "java.io.tmpdir") "/vis-awt-home"))
             (try (.getAvailableFontFamilyNames (GraphicsEnvironment/getLocalGraphicsEnvironment))
                  (finally (System/clearProperty "java.home")))))
         true
         (catch Throwable _ false))))

(defn ensure!
  "Force the headless/font bootstrap once. Returns true when AWT is usable.
   Call it before the FIRST Java2D operation on any code path a native binary
   can reach; it is idempotent and cheap after the first call."
  []
  @awt-ready!)
