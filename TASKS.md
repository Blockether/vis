# OPEN

1. A-T1 TODO Fix Proof Rendering
2. T13 TODO Pimp the providers, TUI, alt keys, navigations etc, details, etc, keyboard shortcuts etc... - Alex
3. T14 TODO Add gates, harden the provenance, THINK how to enforce the SCHEMA - Karol
4. T16b TODO Add `/` for commands, decide whether `/` should be exposed as an extension binding

# DONE

1. T1 DONE Fix Codex Provider - Alex - reauthenticate etc
2. T2 DONE Fix Codex limit-fn - Alex
3. T3 DONE Add bash in foundation - Karol
4. T4 DONE Fix rendering of the markdown, so the answer is not the two times, or `" "` empty blocks - Karol
5. T5 DONE Harden the SPECs of the provenance and tools, enforce them. Use consistently symbols instead of TOOLS naming - Karol
6. T6 DONE Add the NREPL + development tools + introspection - Karol
7. T7 DONE Remove the Copy buttons under the messages totally - Alex
8. T8 DONE Investigate why in terminal app the scrollbar has two boxes instead of one...
9. T9 DONE Fix pasting, and reading from pastes
10. T10 DONE Add Clojure code highlighting
11. T11 DONE Add support for cycling of the thinking, verbosity, model and provider, easier!!!
12. T12 DONE Fix the diagnostics / timeline - Karol, single FUNCTION that returns data
13. T15 DONE Investigate of why details are not working in the answer, why not collapsible + the rendering issues...
14. T16a DONE Fix `@` for selecting the file

# ARCHITECTURAL TASK ANALYSIS

Composed from the user-pasted architectural draft and the live task tracker above.

Pareto score = Impact(1-10) x Ease(1-10). Higher = do first.

## A-T1 Fix Proof Rendering

### Problem Statement

Proofs render anemically. Provenance timeline and proof trail do not show enough useful information in the TUI.

### Rationale

Provenance is the core trust mechanism. If proofs look like noise, the agent cannot verify its own work and the user cannot audit what happened.

### Code Locations

- `src/com/blockether/vis/internal/provenance_lifecycle.clj` - lifecycle state machine

### Source Excerpt Preserved

```clojure
(answer
    (v/join
      (v/h1 "Vis Architectural Task Analysis - 22 Items")
      (v/p
        "Pareto score = Impact(1-10) × Ease(1-10). Higher = do first. Based on probing: 82 ext .clj, 28 src .clj, 32 test .clj, V")
      (v/hr)
      ;; TASK 1
      (v/h2 "T1 - Fix Proof Rendering")
      (v/h3 "Problem Statement")
      (v/p
        "Proofs render anemically - provenance timeline and proof trail don't show enough useful information in the TUI. The `(v/")
      (v/h3 "Rationale")
      (v/p
        "Provenance is the core trust mechanism. If proofs look like noise, the agent can't verify its own work and the user can't")
      (v/h3 "Code Locations")
      (v/ul
        ["src/com/blockether/vis/internal/provenance_lifecycle.clj - lifecycle state machine"])
```

## Merge Notes

- `A-T1` is tracked in `OPEN` because the architectural draft marks it as active work.
- The rest of the architectural 22-item list was not present in the pasted excerpt, so only the visible item is merged here.
- Existing tracker items remain segregated into `OPEN` and `DONE` above.
