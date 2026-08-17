# PLAN — Let an extension stream a live view the human watches and interrupts

*Progress belongs on the human's screen and only the verdict belongs in the model's context — and the extension already knows both.*

## Context

**State before.**

- The engine has exactly ONE typed interaction: `human-input/request!` publishes
  `:human-input/request` on every channel the request names, BLOCKS the calling thread, and
  returns when a surface answers, the deadline fires, or nothing was mounted
  (`src/com/blockether/vis/internal/human_input.clj:1152-1266`). Its vocabulary is a FORM — 8
  answerable field types, 2 decorations, 1 layout group
  (`src/com/blockether/vis/internal/human_input/spec.clj:31-87`). Every node is static: a request
  is painted once and never changes until `settle!` closes it
  (`internal/human_input.clj:1079-1099`).
- The only other way an extension may reach a mounted surface mid-run is the channel bus:
  `:input/replace`, `:input/append`, `:input/insert`, `:status/set`, `:status/clear`, `:notify`
  (`extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/screen.clj:509-556`).
  `:status/set` is ONE line of text with a TTL — no history, no structure, no interrupt — and the
  gateway bridge translates only `:human-input/request` and `:human-input/close` into session
  events (`src/com/blockether/vis/internal/gateway/human_input.clj:77-88`), so a status line never
  reaches the companion app at all. An extension currently has NO way to tell the phone that
  anything is happening.
- Because the surfaces cannot be told, the MODEL is made to watch. The shell verb hands the model a
  live handle and the guidance tells it to loop: `sh.logs(-50)`, `sh.wait(30)`
  (`src/com/blockether/vis/internal/foundation/shell.clj:2710-2729,3115-3145`;
  `resources/vis-docs/context-and-prompts.md:139`). `Shell.wait` is a bounded poll executed in the
  host (`packages/vis-agent/src/vis/__init__.py:409-413`), so every additional LOOK at a running
  process is a full model round trip — provider latency plus billed tokens — to learn what the host
  already holds: the process status, its exit code, its accumulated log. The extension knows when
  the work is done; the model is paid to ask.
- What HITL already built is the asset this plan spends. Engine parser 1382 lines
  (`internal/human_input.clj`), closed vocabulary 629 lines (`internal/human_input/spec.clj`),
  builder surface `src/com/blockether/vis/human_input.clj` (181 lines), TUI dialog 55,525 bytes
  (`extensions/channels/vis-channel-tui/.../channel_tui/human_input.clj`) behind a 94,702-byte
  test, gateway bridge 4,198 bytes, REST answers
  (`internal/gateway/server.clj:2847-2915`, routes at `:3748-3750`), companion reducer and dialog
  (`apps/vis-companion/src/lib/human-input.ts` 18,362 bytes,
  `apps/vis-companion/src/components/HumanInputPrompt.tsx` 24,645 bytes), and a drift test that
  reads the TypeScript from Clojure
  (`extensions/channels/vis-channel-tui/test/.../human_input_cross_channel_test.clj`).
- The terminal can already scroll the way this needs and nothing extension-owned uses it: SGR mouse
  decode including the wheel (`.../channel_tui/input.clj:309-341`), wheel-momentum scrolling with a
  directional lock (`.../channel_tui/scroll.clj:219-298`), a scrollbar (`.../channel_tui/scrollbar.clj`).
- The journal can already carry it: `state/append-event!` canonicalizes to snake_case, stamps a
  monotonic `seq`, fans out to SSE and keeps a replay ring, and `:store? false` fans out live while
  staying OUT of the ring (it still burns a `seq`, still reaches SSE and still reaches the bus) —
  documented, implemented, and with no current caller
  (`src/com/blockether/vis/internal/gateway/state.clj:348-364,429-437`).
- The repo already draws the line this plan depends on: attachments carry a closed `audiences`
  vocabulary that decides what the human is SHOWN versus what the model is TOLD
  (`src/com/blockether/vis/internal/attachments.clj:670-679`), and storage offload dispatches on
  media type, not on pictures (`src/com/blockether/vis/internal/attachment_storage.clj:166-198`).
  A background shell already keeps a ring buffer of its output
  (`internal/foundation/shell.clj:116`), which an EXTENSION can drain into a view it owns.

**Root problem.** The engine owns a PAUSE primitive and no PROGRESS primitive. Work that runs and
finishes on its own — a build, a device log tail, a deploy, a fleet scan — has only two honest
expressions today: block the human with a form, or say nothing. So observation is routed through
the most expensive component in the system. The model polls what the extension already knows, the
human who would happily watch sees a spinner, and the log the human wanted is either invisible or
dumped into the transcript, where it is re-sent to the provider on every later request.

**What we solve.** A second interaction kind beside the form, in the SAME namespaces, on the SAME
channels, through the SAME gateway bridge: a LIVE VIEW an extension opens, patches while it works,
and closes. Its nodes are a closed set — a status line, a progress bar, a strip of label/value
counters, an ordered checklist of steps, an append-only log buffer, a table whose rows are upserted
and removed, and links the human can open. It paints in the TUI and in the companion app from
one declared vocabulary, scrolls with the mouse wheel in both, is interrupted with Escape (the
extension's call then returns `interrupted` and decides what to do), and settles into an artifact
the human can reopen afterwards. Patches are HUMAN-facing: they never enter model context. The
model receives exactly one thing — the settled result the extension returns.

**What we do not solve.** No answers inside a live view: a question stays `ask`, because mixing the
two puts validation, secrets and focus back into a pane that must never own the keyboard. No
arbitrary markup, HTML or markdown nodes — a closed vocabulary is the only way the terminal and the
phone can both be honest about a node. No second event bus, no schema library beyond
`clojure.spec.alpha`, and no persistence beyond the session journal plus the one artifact. And
nothing in the HOST opens a view for anybody: `shell`, the tools and the runtime keep exactly the
behavior they have today. A live view exists where EXTENSION code opened one — visibility is
written, never something that happens to a process.

**Alternatives considered.**

- *Give `:status/set` structure and a history.* Lost: it is a TTL toast with no bridge to the app,
  no interrupt and no artifact; teaching it those three things is this plan with a worse name, and
  it would still be a second vocabulary next to human input.
- *A cheaper polling tool for the model ("wait until X").* Lost: still one model round trip per
  look, still nothing on the human's screen. It optimizes the symptom and leaves the human blind.
- *A standalone `live` namespace with its own events, its own routes and its own app screen.* Lost:
  it duplicates the channel bridge, the parked-session bookkeeping, the push tap, the REST answer
  routes and the TUI dialog host, and hands the repo a second cross-language vocabulary to keep
  from drifting — precisely the debt `human_input_cross_channel_test.clj` exists to prevent.
- *Stream progress into the transcript as markdown.* Lost: the transcript IS model context, so a
  10,000-line build log becomes a permanent per-request tax — the cost this plan removes.
- *Write a file and attach it when the work ends.* Lost: no live feedback and no interrupt, the two
  things actually asked for.
- *Make it a background job the agent later joins.* Lost: a job that outlives the turn needs its own
  lifecycle, ownership and cancellation story; parking the extension thread reuses
  `rt/park-blocking-wall` (`internal/human_input.clj:1245-1255`), which already survives the Python
  eval watchdog and the native-tool wall.
- *Have the host's background `shell` open a view by itself — automatically when a channel is
  mounted, or on `is_live: true`.* Lost on review: a live view belongs to the code that knows what
  the work MEANS — a status, a score, a step list — and the host only has bytes. An extension that
  spawns a shell drains its ring (`internal/foundation/shell.clj:116`) into a view it owns and
  labels; the model's own `sh.logs()` / `sh.wait()` guidance stays exactly as it is today.

## Phase 1 — Declare the live view and give the engine the primitive

**Rationale.** Without it there is no vocabulary at all: every surface would invent its own node
names, and the first Python extension to try would freeze a thread nobody can release. This phase
lands a usable engine primitive on its own — with no surface mounted, a live view answers
`undeliverable` at once, exactly as `request!` does today.

**Data.** The live vocabulary is added to `internal/human_input/spec.clj` — the one file that owns
human-input keys, so the snake_case wire spellings keep being DERIVED by `wire-keys`
(`internal/human_input.clj:216-222`) instead of written twice.

```clojure
;; Wire type name -> internal LIVE node type. CLOSED, like `field-types`: an unknown
;; name is refused BY NAME, never keyword-minted.
(def live-node-types
  {"status"   :status                ; one line, REPLACED: what is happening right now
   "progress" :progress              ; a fraction, or indeterminate
   "stat"     :stat                  ; label -> value counters upserted by id: the score
   "steps"    :steps                 ; an ORDERED keyed checklist, each item carrying its own tone
   "log"      :log                   ; append-only ring of lines — the scrollback
   "table"    :table                 ; rows upserted and removed by row id
   "link"     :link})                ; labeled pointers the human OPENS

;; The three things a surface knows how to open. CLOSED.
(def link-targets {"attachment" :attachment "path" :path "url" :url})

;; REFUSED here so review does not reopen them: `image`/`chart` (bytes on the event bus and a
;; renderer neither surface has — a picture is an attachment a `link` points at), `markdown`/`html`
;; (a closed vocabulary is the only reason the terminal and the phone can agree), `button`/`field`
;; (a view that ASKS is `ask`; blocking belongs to the form), `spinner` (`progress` with a nil
;; value already means indeterminate), `tree` (no caller).

;; What one patch operation DOES to one node. CLOSED.
(def live-ops
  {"set"    :set                     ; replace a node's own state (status text, progress value …)
   "append" :append                  ; add lines to a log; upsert rows, steps, stats and links by id
   "remove" :remove                  ; drop keyed items by id
   "clear"  :clear})                 ; empty a log, table, step list, stat strip or link list

(def live-tones {"idle" :idle "running" :running "ok" :ok "warn" :warn "error" :error})

;; Why a view ended. CLOSED, and the only vocabulary an extension branches on.
(def live-reasons #{"completed" "interrupted" "timeout" "undeliverable" "failed"})

(def log-defaults   {:max-lines 2000 :max-lines-cap 20000 :max-patch-lines 500})
(def table-defaults {:max-rows 500 :max-rows-cap 5000 :max-patch-rows 200})
(def stat-defaults  {:max-stats 32})           ; a strip, not a spreadsheet
(def step-defaults  {:max-steps 200})          ; a checklist, not a second log
(def link-defaults  {:max-links 32})
(def progress-defaults {:value nil})           ; nil is INDETERMINATE, not zero

;; One dispatch key, two multi-specs: a form node and a live node can never be
;; mistaken for each other, because neither multimethod has a method for the other's type.
(s/def ::type (set (concat (vals field-types) (vals live-node-types))))
(s/def ::tone (set (vals live-tones)))
(s/def ::line string?)                          ; a blank line is a line
(s/def ::lines (s/coll-of ::line :kind vector? :max-count (:max-patch-lines log-defaults)))
(s/def ::max-lines (s/int-in 1 (inc (:max-lines-cap log-defaults))))
(s/def ::value (s/nilable (s/and number? #(<= 0 % 1))))
(s/def ::done nat-int?)
(s/def ::total pos-int?)
(s/def ::cells (s/coll-of string? :kind vector?))
(s/def ::table-column (s/and #(closed? column-keys %) (s/keys :req-un [::id ::label] :opt-un [::align])))
(s/def ::columns (s/and (s/coll-of ::table-column :kind vector?) non-empty? distinct-ids?))
(s/def ::row (s/and #(closed? row-keys %) (s/keys :req-un [::id ::cells])))
(s/def ::rows (s/coll-of ::row :kind vector? :max-count (:max-patch-rows table-defaults)))
(s/def ::item-ids (s/coll-of ::id :kind vector?))   ; rows, steps, stats or links
(s/def ::max-rows (s/int-in 1 (inc (:max-rows-cap table-defaults))))

(s/def ::detail (s/nilable string?))            ; a dimmed second line under a status or a step
(s/def ::value-text string?)                    ; a stat's value AS SHOWN ("12 failed", "3.4 MB/s")
(s/def ::target string?)                        ; attachment id, workspace path, or absolute url
(s/def ::target-kind (set (vals link-targets)))
(s/def ::stat  (s/and #(closed? stat-keys %) (s/keys :req-un [::id ::label ::value-text] :opt-un [::tone])))
(s/def ::stats (s/coll-of ::stat :kind vector? :max-count (:max-stats stat-defaults)))
(s/def ::step  (s/and #(closed? step-keys %) (s/keys :req-un [::id ::label ::tone] :opt-un [::detail ::value])))
(s/def ::steps (s/coll-of ::step :kind vector? :max-count (:max-steps step-defaults)))
(s/def ::link  (s/and #(closed? link-keys %) (s/keys :req-un [::id ::label ::target-kind ::target] :opt-un [::tone])))
(s/def ::links (s/coll-of ::link :kind vector? :max-count (:max-links link-defaults)))

(defmulti live-node-form :type)
(defmethod live-node-form :status   [_] (s/keys :req-un [::id ::type ::text ::tone] :opt-un [::label ::detail]))
(defmethod live-node-form :progress [_] (s/keys :req-un [::id ::type ::value] :opt-un [::label ::done ::total]))
(defmethod live-node-form :stat     [_] (s/keys :req-un [::id ::type ::stats] :opt-un [::label]))
(defmethod live-node-form :steps    [_] (s/keys :req-un [::id ::type ::steps] :opt-un [::label]))
(defmethod live-node-form :log      [_] (s/keys :req-un [::id ::type ::lines ::max-lines] :opt-un [::label]))
(defmethod live-node-form :table    [_] (s/keys :req-un [::id ::type ::columns ::rows ::max-rows] :opt-un [::label]))
(defmethod live-node-form :link     [_] (s/keys :req-un [::id ::type ::links] :opt-un [::label]))
(s/def ::live-node (s/and (s/multi-spec live-node-form :type) live-node-closed?))
(s/def ::nodes (s/and (s/coll-of ::live-node :kind vector?) non-empty? distinct-ids?))

;; The view itself. `:id`, `:seq` and `:created-at` are ENGINE stamps
;; (`request-stamp-keys`), never written in a spec.
(s/def ::live-view
  (s/and #(closed? live-view-keys %)
         (s/keys :req-un [::id ::title ::session-id ::channel-ids ::nodes
                          ::is-cancellable ::timeout-ms ::seq ::created-at]
                 :opt-un [::description ::source])))

;; One patch. `:seq` is monotonic PER VIEW, so a surface that sees a gap re-reads the
;; snapshot instead of painting a torn view.
(defmulti live-op-form :op)
(defmethod live-op-form :set    [_] (s/keys :req-un [::op ::node] :opt-un [::text ::detail ::tone ::label ::value ::done ::total ::stats ::steps ::links]))
(defmethod live-op-form :append [_] (s/keys :req-un [::op ::node] :opt-un [::lines ::rows ::stats ::steps ::links]))
(defmethod live-op-form :remove [_] (s/keys :req-un [::op ::node ::item-ids]))
(defmethod live-op-form :clear  [_] (s/keys :req-un [::op ::node]))
(s/def ::live-op (s/multi-spec live-op-form :op))
(s/def ::ops (s/and (s/coll-of ::live-op :kind vector?) non-empty?))
(s/def ::live-patch (s/and #(closed? live-patch-keys %) (s/keys :req-un [::view-id ::seq ::ops])))

;; What the blocked extension receives, and what the close event carries.
(s/def ::is-completed boolean?)
(s/def ::live-result
  (s/and #(closed? live-result-keys %)
         (s/keys :req-un [::view-id ::is-completed ::reason]
                 :opt-un [::summary ::artifact-id ::error])))
```

Booleans are `is_<foo>` on the wire and `:is-<foo>` in the engine, per
`src/com/blockether/vis/internal/gateway/wire.clj`; the snake_case spellings come from `wire-keys`,
so no key is spelled a second time anywhere.

**Acceptance criteria.**

- `src/com/blockether/vis/internal/human_input/spec.clj` — the block above, plus
  `contract-vocabulary` (`:122-137`) exporting the live tables so the Python contract document reads
  one source.
- `src/com/blockether/vis/internal/human_input.clj` — `normalize-live-view`, `normalize-patch`,
  `apply-patch` (the materializer: log ring by `:max-lines`, upsert-by-id for table rows, steps,
  stats and links, capped by `:max-rows` / `:max-steps` / `:max-stats` / `:max-links`), `live!`
  (opens, publishes `:human-input/live-open`, parks with `rt/park-blocking-wall`, always closes),
  `patch!`, `close!`, `interrupt!`.
- Same file — the `pending` registry entry gains `:kind` (`:form` | `:live`) so `cancel-all!`
  (`:1145-1150`), the turn-interrupt path (`:1251-1255`) and the undeliverable path (`:1220-1235`)
  release both kinds, and `checked-answer` routes to `::answer` or `::live-result` by kind.
- `src/com/blockether/vis/human_input.clj` — builders `status`, `progress`, `stat`, `steps`, `log`,
  `table`, `table-column`, `link`, and `live!` taking the view spec plus a function that receives
  the handle, so the view closes on a throw as well as on a return.
- Test `test/com/blockether/vis/internal/human_input_test.clj` — patch materialization including
  ring eviction and row removal, refusal of an unknown node type / op / key naming the key,
  `undeliverable` with no channel mounted, `interrupt!` releasing the parked caller, and a form and
  a live view coexisting in the registry.
- Same file — a bad live spec is refused WHERE IT WAS DECLARED: `live!` normalizes before it mounts
  anything, exactly as `request!` does (`:1200`), and throws the engine's one-line reason. There is no
  answer-instead-of-throw seam left to teach (`check`/`check-json` went with `vis.check`), so the live
  view inherits one rule and cannot grow a second opinion. Test in the same file: an unknown node type
  throws with the key named, and nothing was published.

**Unknowns.** Should `:timeout-ms` default to `no-timeout-ms` for a live view (a build takes as long
as it takes) while a form keeps its five minutes? The plan assumes yes, stated in the docstring.

## Phase 2 — Paint it in the TUI, scroll it with the wheel, interrupt it with Escape

**Rationale.** Until a surface paints it the primitive can only answer `undeliverable`. The terminal
is where the operator already watches a run, and every piece it needs — mouse decode, momentum
scroll, scrollbar, the dialog host — is already built.

**Data.** None. The pane's own state (scroll offset, follow-tail flag, focus) never leaves the TUI
process.

**Acceptance criteria.**

- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/live_view.clj` (new) —
  one painter per node type (the expensive one is already drawn: `table.clj`, `boxed_table.clj`,
  `scrollbar.clj`), the scrollback buffer over `scroll.clj`, follow-tail that releases when the human
  scrolls up and re-arms at the bottom, a header line carrying title plus elapsed time, and click
  regions on `link` items.
- `.../channel_tui/screen.clj:509-556` — three more ops (`:human-input/live-open`,
  `:human-input/live-patch`, `:human-input/live-close`) dispatching into state, built exactly like
  the `:human-input/request` case at `:548`.
- `.../channel_tui/state.clj` — `:live-view-open`, `:live-view-patch`, `:live-view-close`; the pane
  does NOT take the keyboard (the composer keeps focus, unlike the form dialog at `screen.clj:545`),
  the wheel over the pane scrolls it, and Escape interrupts the newest open live view BEFORE it
  interrupts the turn, with the footer saying which one it will hit.
- `.../channel_tui/footer.clj` — a live view contributes its status/progress to the footer segment
  while it is open, so a scrolled-away view is still legible.
- Test `extensions/channels/vis-channel-tui/test/.../channel_tui/live_view_test.clj` — open, patch,
  close, wheel scroll, follow-tail release/re-arm, Escape precedence over turn interrupt; plus the
  screenshot gate described by `doc("tui-rendering")`.

**Unknowns.** Where does the pane live when several views are open at once — stacked above the
composer, or one pane with a switcher? The plan assumes stacked, newest last, capped at three
visible with the rest collapsed to one line each.

## Phase 3 — Give Python the same primitive through one host op

**Rationale.** Extensions are Python first; a Clojure-only primitive would not touch a single one of
the 16 shipped extensions, and the model's polling loop lives in the Python sandbox.

**Data.** One new host op and its op grammar, mirroring how `shell` is already declared —
`packages/vis-contract/resources/vis-contract/python-host.edn`, `:contract/version` 2 -> 3:

```clojure
{:op/name "live"
 :op/global "__vis_host_live__"
 :op/arity 1
 :op/summary "Open, patch, read or close one live view."
 :op/outside :outside/local}      ; outside Vis: render the patches on stderr

:contract/live
{:live/default-op "open"
 :live/spawn-ops ["open"]
 :live/handle-ops ["patch" "state" "close"]}
```

The envelope it carries is the Phase 1 shape as JSON:
`(s/def ::live-envelope (s/and #(closed? live-envelope-keys %) (s/keys :req-un [::op] :opt-un [::view-id ::view ::patch])))`.

**Acceptance criteria.**

- `packages/vis-agent/src/vis/__init__.py` — `vis.live(title, nodes, **options)` as a context
  manager answering a `LiveView` handle with `status()`, `progress()`, `stat()`, `step()`, `log()`,
  `row()`, `link()`, `remove()`, `clear()`, plus node builders `vis.status`, `vis.progress`,
  `vis.stat`, `vis.steps`, `vis.log`, `vis.table`, `vis.table_column`, `vis.link`. The handle carries
  `is_interrupted` and `reason`; a push after an interrupt raises `vis.Interrupted`, so an unattended
  loop stops by itself while a compute loop can poll the flag.
- `resources/vis-python/extension_bootstrap.py` — `live=__vis_host_live__` in `_host`.
- `src/com/blockether/vis/internal/python_extensions.clj:490` — `host-member-names` gains `live`.
- `src/com/blockether/vis/internal/human_input.clj` — `live-json!`, the strings-only seam beside
  `request-json!` (`:1312-1345`).
- Test `test/com/blockether/vis/contract/python_host_test.clj` (drift: document, bootstrap, package
  and engine dispatch agree) and `test/com/blockether/vis/internal/extension_check_test.clj` (a
  static check of an extension that opens a live view).
- `packages/vis-agent/src/vis/_outside.py` `_check_request` (`:708-724`) accepts the live shape on the
  same path, so `vis.live` refuses outside a Vis host exactly as it refuses inside one, and
  `resources/vis-python/extension_check.py` `_ASKS` gains `live`: the checker judges a declared view by
  CALLING it on the inert host (`internal/extension_check.clj` `judge-request`), which is the only
  pre-flight an unattended extension gets.
- `resources/vis-docs/extending.md` — the live view named in the `extension check` list (`:887-888`),
  which promises ONE judge, so a primitive missing from it reads as a second one; plus the worked
  example that carries the shell case at the EXTENSION layer where it belongs — spawn a background
  shell, drain its ring (`internal/foundation/shell.clj:116`) into a `log` node the extension labels,
  return one verdict to the model.

**Unknowns.** Outside a Vis host, is stderr rendering enough, or should `:outside/local` write the
same JSON the engine would publish so a harness can assert on it? The plan assumes stderr, one line
per patch.

## Phase 4 — Bridge it to the companion app

**Rationale.** Half the reason to stream at all is the phone: the operator who walked away wants to
see the build move and stop it from the couch. The gateway already turns HITL channel events into
session events; without this phase the app stays blind exactly as it is today.

**Data.** None. The three session events carry the Phase 1 shapes verbatim through
`wire/canonical`; no key crosses that Phase 1 did not declare.

**Acceptance criteria.**

- `src/com/blockether/vis/internal/gateway/human_input.clj:77-88` — three more ops.
  `human_input.live.open` and `human_input.live.close` are STORED, so a client that joins late
  replays them. `human_input.live.patch` is published `:store? false` (`gateway/state.clj:365`), and
  that flag is narrower than it sounds: the event is still stamped with the session's next `seq`,
  still fanned out to SSE, still published on the cross-process bus — the ONLY thing skipped is
  `(update :events trim-ring)`, the bounded in-memory replay ring (`:429-437`). Three consequences,
  and this phase is designed around them. (1) A 2000-patch build cannot evict `turn.started`, the
  form requests and the turn's real history out of that bounded ring. (2) A cursor replay or a
  `/poll` pull — both read the ring (`state.clj:357-361`) — never re-delivers a patch, so nothing
  redraws yesterday's progress. (3) On the bus a transient event is handed to the writer WITHOUT
  waiting and may be dropped when the queue is saturated (`gateway/bus.clj:590-594`), and travels
  marked `"_store" false` (`:483`) so a mirroring process keeps it out of its ring too. The price:
  a reconnecting client sees a `seq` jump and must NOT read it as loss — it resyncs from the snapshot
  route below, which is therefore a correctness requirement of this phase, not an optimization.
  Patches are coalesced per view on a fixed tick (superseded `set` ops dropped, `append` lines
  merged) before they are published.
- `src/com/blockether/vis/internal/gateway/server.clj` — `GET /v1/sessions/:sid/human-input/live`
  answering the materialized snapshots (the resync path for a client that joined mid-flight or lost
  SSE) and `POST /v1/sessions/:sid/human-input/live/:view-id/actions/interrupt`, registered beside
  the routes at `:3748-3750`; the session list's `awaiting` (`:1702`) gains the working state so the
  sessions screen can mark a run as busy rather than parked.
- `apps/vis-companion/src/lib/live-view.ts` — the pure reduction of the three events into view
  state, a `seq` gap read as RESYNC rather than loss (the patch rule above), and the mirrors of the
  closed tables (`LIVE_NODE_TYPES`, `LIVE_OPS`, `LIVE_TONES`, `LIVE_REASONS`, `LINK_TARGETS`), the
  same way `lib/human-input.ts:22-40` mirrors the form vocabulary.
- `apps/vis-companion/src/components/LiveView.tsx` + `dev/liveViewVariants.tsx` +
  `lib/live-view.fixture.json` (one node of every kind, `request->view` verbatim) and rendering in
  `screens/SessionScreen.tsx` where `HumanInputPrompt` renders; scroll follows the tail and releases
  on touch, per `doc("companion-ui")`.
- `apps/vis-companion/src/components/ui.tsx` — the two controls the vocabulary needs and the app
  does not have: a progress bar and a table row. The closed set already covers the rest — `stat` on
  `Pill` (`:749`), `steps` and `link` on `ListRow` (`:463`), a collapsed log on `Disclosure`
  (`:603`), tone on `Banner` (`:1392`), pending on `Spinner` (`:1750`) — so the phone costs two new
  controls, not seven painters, and they are added THERE per `doc("companion-ui")`, never inline.
- `apps/vis-companion/src/lib/gateway.ts` — subscribe the three events, resync from the snapshot
  route on reconnect (`:3182` is where the same argument is already made for `human_input.request`).
- Test: `extensions/channels/vis-channel-tui/test/.../human_input_cross_channel_test.clj` extended to
  read the new TypeScript tables and fail on drift; `LiveView.test.tsx` rendering the fixture;
  `test/com/blockether/vis/internal/gateway/human_input_test.clj` for store/no-store and coalescing.

**Unknowns.** Does a live view deserve a push notification? The plan says no by default — it is not
a question — with one exception under discussion: a view that closes `failed` while the app is
backgrounded.

## Phase 5 — Settle a finished view into an artifact the human can reopen

**Rationale.** Without this the buffer dies with the pane: the human who looked away loses the log
they were watching, and the only way to keep it would be to dump it into the transcript, which is
what this plan exists to avoid.

**Data.** The stored artifact, declared in `internal/human_input/spec.clj` beside the rest:

```clojure
(def live-artifact-media-type "application/vnd.vis.live+json")
(s/def ::live-artifact
  (s/and #(closed? live-artifact-keys %)
         (s/keys :req-un [::id ::view-id ::session-id ::title ::media-type ::audience
                          ::ended-at ::reason ::view]
                 :opt-un [::storage-uri ::size])))
```

`::audience` is the existing closed vocabulary (`internal/attachments.clj:670-679`) and a live
artifact is human-only: the model is told it exists and gets the summary, never the bytes.

**Acceptance criteria.**

- `src/com/blockether/vis/internal/human_input.clj` — on close, materialize the final view, encode it
  through `persistance/->json`, and route it via
  `internal/attachment_storage/offload-attachment` (`:166-198`); the close event and the extension's
  `::live-result` carry the resulting `artifact-id`.
- `extensions/channels/vis-channel-tui/src/.../channel_tui/live_view.clj` — a closed view collapses
  to one clickable line (title, reason, line count, elapsed) registered in `click_regions.clj`, which
  reopens the full scrollback read-only.
- `apps/vis-companion/src/lib/artifacts.ts` — classify the media type so a finished view appears in
  `ArtifactsSheet`; `components/LiveArtifact.tsx` renders it with the Phase 4 node painters in
  read-only mode.
- Test: `test/com/blockether/vis/internal/human_input_test.clj` (artifact written once, on every
  reason including `interrupted`), `apps/vis-companion/src/lib/artifacts.test.ts` (classification),
  and a TUI test that reopens a closed view after the pane was dismissed.

**Unknowns.** Cap on a stored view: the plan assumes the materialized ring (`:max-lines`) is what is
stored, so a 2000-line default artifact is bounded — but a build that wants its whole log needs
either a higher `:max-lines` or a second, file-backed sink. Decide when the first extension asks.

## State of the plan

**REQUIRES WORK** — written, not yet accepted. Nothing is implemented.

Done:

- Nothing in this plan. Its predecessor (make every capability an extension declared by one
  cross-language contract) is parked at commit `6ac932db4` and is recoverable from there; its open
  decisions — the TypeScript binding and the publishing identity — are untouched by this work and
  outlive it.

TODO, in order:

1. Phase 1 — live vocabulary in `spec.clj`, `live!`/`patch!`/`close!`/`interrupt!` in the engine,
   `:kind` in the pending registry.
2. Phase 2 — TUI pane, wheel scroll, Escape precedence, screenshot gate.
3. Phase 3 — `live` host op (contract version 3), `vis.live` context manager, checker parity.
4. Phase 4 — gateway bridge (open/close stored, patches `:store? false` and coalesced, snapshot
   resync, interrupt route), companion reducer, component and drift test.
5. Phase 5 — artifact on close, reopen in both surfaces.
