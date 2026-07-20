(ns com.blockether.vis.internal.foundation.environment.agents
  "Thin re-export shim — project-guidance discovery moved to
   `com.blockether.vis.internal.agents`. AGENTS.md / CLAUDE.md
   handling is core functionality (drives system prompt + ctx
   digest), so it now lives outside the foundation extension.

   This shim keeps the legacy `main-agent-instructions` tool +
   any downstream `(:project ctx) :guidance` consumers compiling
   while they migrate to the internal namespace."
  (:require [com.blockether.vis.internal.agents :as internal]))

(defn scan-in [root] (internal/scan-in root))

(defn scan [] (internal/scan))

(defn current [] (internal/current))

(defn reload! [] (internal/reload!))

(defn instructions [] (internal/instructions))

(defn read-warnings [] (internal/read-warnings))
