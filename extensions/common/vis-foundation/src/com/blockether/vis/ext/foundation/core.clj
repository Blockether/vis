(ns com.blockether.vis.ext.foundation.core
  "vis-foundation aggregator — single classpath entry that registers
   four cooperating extensions:

     foundation/   programmatic introspection (turn / conversation /
                   var-history / find-attempts / diagnose / failures /
                   extensions catalog) — see `foundation.introspection`
     vis/          file I/O (cat / ls / rg / edit / write)
                   — see `foundation.editing.tools`
     fs/           path math (cwd / exists? / glob / parent /
                   components / file-name / extension / expand-home /
                   list-dir / relativize) — see `foundation.editing.fs`
     environment/  cwd / git / language scan / monorepo shape
                   — see `foundation.environment.core`

   Each sub-namespace self-registers its extension via
   `sdk/register-extension!` at namespace load. This aggregator's only
   job is to require all four so a single
   `META-INF/vis-extension/vis.edn` manifest entry pulls every
   extension onto the classpath."
  (:require
   [com.blockether.vis.ext.foundation.introspection]
   [com.blockether.vis.ext.foundation.editing.core]
   [com.blockether.vis.ext.foundation.environment.core]))
