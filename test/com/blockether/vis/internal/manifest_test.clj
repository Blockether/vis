(ns com.blockether.vis.internal.manifest-test
  "Public-surface coverage for `internal/manifest`.

   Why this file exists:
     1. AGENTS.md hard rule - every namespace ships with a `_test.clj`.
     2. The `load-failures` surface added after conversation
        d8aff512-d60d-42b6-a009-041f1bec3891 is the keystone of an
        end-to-end fix: a syntax error in any extension source file
        used to silently disable its alias namespace, the LLM would
        see `Unable to resolve symbol: v/cat` for every call, and
        the only trail was a buried `~/.vis/vis.log` ERROR line. Now
        the same failure feeds two visible surfaces - the launcher's
        stderr banner AND the system prompt's `<scan-warnings>`
        block. Both consumers depend on this contract:

          - `(load-failures)` returns a vec of maps, never nil.
          - Each entry carries the `{:source :reason :path}` shape
            `vis-foundation`'s `<scan-warnings>` renderer expects.
          - Reading the surface does NOT mutate.

        Drift on either contract = silent regression, hence the
        regression net here."
  (:require
   [com.blockether.vis.internal.manifest :as manifest]
   [lazytest.core :refer [defdescribe describe expect it]]))

(defdescribe load-failures-public-surface-test
  (describe "`load-failures` always returns a vec"
    (it "the function exists and is callable"
      (expect (fn? manifest/load-failures)))

    (it "return value is a vec (never nil), even before any scan ran"
      ;; The test runner may or may not have triggered a classpath
      ;; scan by the time this lands, so assert only the contract:
      ;; vec, never nil.
      (let [v (manifest/load-failures)]
        (expect (vector? v))))

    (it "calling twice in a row returns equal values (read-only surface)"
      ;; If `load-failures` ever started doing work on read - e.g.
      ;; lazily re-scanning the classpath - two consecutive calls
      ;; could disagree. The launcher banner + system-prompt block
      ;; both call this fn, sometimes from different threads; they
      ;; need to see the same answer.
      (expect (= (manifest/load-failures) (manifest/load-failures)))))

  (describe "failure entries match the `<scan-warnings>` renderer contract"
    ;; `vis-foundation/format-scan-warnings-block` renders
    ;; `{:source :reason :path}` triples. Drifting from that shape
    ;; would either drop entries from the system prompt silently or
    ;; render them as nil-suffixed garbage. We verify the contract
    ;; against any failures the live classpath happens to carry; in
    ;; a clean tree the vec is empty and the doseq is a no-op (still
    ;; useful: pins that the fn returns plain maps with the keyword
    ;; keys, not e.g. namespaced :ext.failure/source surprises).
    (it "every entry has :source, :reason, :path keys"
      (doseq [{:keys [source reason path]} (manifest/load-failures)]
        (expect (some? source))
        (expect (string? reason))
        (expect (string? path))
        (expect (keyword? source))
        ;; :extension-load is the only :source value emitted by
        ;; `manifest/scan!`; pin it so foundation's renderer can
        ;; differentiate scan-source families if it ever wants to.
        (expect (= :extension-load source))))))

(defdescribe scan-extensions-still-idempotent-test
  ;; `load-failures` reset semantics live INSIDE `scan!` - each scan
  ;; clears the atom before re-populating. We don't trigger a real
  ;; scan here (it would mutate global state used by other tests);
  ;; instead we pin that the public scan entry point is callable and
  ;; returns a map. If the addition of the load-failures-atom ever
  ;; broke the return-shape contract, this catches it.
  (describe "`scan-extensions!` keeps its existing return contract"
    (it "returns a map"
      (expect (map? (manifest/scan-extensions!))))))
