# PLAN — Let an extension stream a live view the human watches and interrupts

*Progress belongs on the human's screen; the model reads the finished picture ONCE, as data — and the extension already knows both.*

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
- The event rail can CARRY a live view, but nothing on it is STORAGE — every sink is bounded, and
  this plan is designed around the exact bounds. `state/append-event!` canonicalizes to snake_case,
  stamps a monotonic `seq`, fans out to SSE and keeps a replay ring
  (`src/com/blockether/vis/internal/gateway/state.clj:348-364,429-437`) — 2000 events for the WHOLE
  session, and that ring is explicitly not the record: "older events stay durable in the session
  transcript; the ring only backs short SSE cursor reconnects" (`:37-40`). The cross-process journal
  is smaller still: `<sid>.ndjson` is TRUNCATED at every `turn.started` and force-truncated past
  16 MB mid-turn (`gateway/bus.clj:53,488-499`). And a durable publish PARKS the producing thread
  until the journal writer acknowledges it, up to 5 s (`bus.clj:567-589`), so patch RATE is a
  latency decision and not only a volume one.
- The offload rail that turns bytes into an artifact wants the whole payload in memory as base64
  (`internal/attachment_storage.clj:174-198`), so a long log cannot become an artifact by being
  encoded at the end — it has to have been written while it ran. The file backend that resolves such
  an artifact back already exists (`:261-275`, scheme `file`).
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
the human can reopen afterwards. Patches are HUMAN-facing: not one of them enters model context. The
model receives exactly ONE thing, and it receives the WHOLE view: when a view ends the engine hands
back the verdict carrying its materialized state AS DATA — every node with its id, its tone and its
numbers — budgeted, never prose. The stream stays out of the prompt; the picture the human watched is
what the model reads, and `->markdown` renders that same picture into the document a human reopens.

Two rules the rest of the plan is built on, both settled on review:

- **Every node has an id, and the id is the address.** A view holds as many logs, tables and
  progress bars as the extension declares — `failures` and `passed` are two tables, patched
  independently — and a view may GAIN or DROP a node while it runs, so a scan that discovers a
  second device does not have to have declared it up front. There is no implicit "the" table.
- **Everything is stored, and nothing is ever silently evicted.** Every accepted patch is appended
  to the view's own append-only file BEFORE it is published, so the record is complete even if the
  process dies mid-run. What a surface paints is a WINDOW over that record — a rendering decision,
  never a data one. Where a collection must stay in memory to remain addressable (rows, steps,
  stats, links) the bound is a REFUSAL carrying its reason, never a trim behind the caller's back.
- **The model reads DATA; markdown is the human's document.** One materializer, four readers: the
  terminal pane, the companion screen, `picture` for the model and `->markdown` for the page a human
  reopens. The model never sees a patch and never polls; it reads the finished view as VALUES — ids,
  tones, numbers — with the verdict first, so it acts on the picture instead of recovering it from
  prose. A budget may truncate what the MODEL holds — always saying how much it left behind — and
  never what the record holds.

**What we do not solve.** No answers inside a live view: a question stays `ask`, because mixing the
two puts validation, secrets and focus back into a pane that must never own the keyboard. No
arbitrary markup, HTML or markdown NODES — a closed vocabulary is the only way the terminal and the
phone can both be honest about a node, and the document a human reopens is a RENDERING of that
vocabulary produced by the engine, never a node an extension writes. No second event bus, no schema
library beyond
`clojure.spec.alpha`, and no database — the only new storage is ONE append-only file per view, which
IS the artifact once the view closes. And
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
- *Keep only a window of the stream and let the rest fall off the end (this plan's first draft: a
  2000-line ring inside the log node).* Lost on review: an eviction the human cannot see is data
  loss with a progress bar in front of it, and it makes the artifact a lie — the log they watched is
  not the log they can reopen. Storage is a file; the window is paint.
- *Let the session event ring BE the store — publish every patch and read it back on reconnect.*
  Lost: that ring holds 2000 events for the whole session and is documented as a reconnect cache,
  not the record (`gateway/state.clj:37-40`); the journal it feeds is truncated every turn and again
  past 16 MB (`gateway/bus.clj:53,488-499`); and a durable publish parks the producing thread until
  the writer acks (`bus.clj:567-589`). A stream stored there evicts the session's own history AND
  pays disk latency per patch, which is why the sink is a file the view owns.
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
   "table"    :table                 ; rows upserted and removed by row id, in a DECLARED order
   "link"     :link})                ; labeled pointers the human OPENS

;; The three things a surface knows how to open. CLOSED.
(def link-targets {"attachment" :attachment "path" :path "url" :url})

;; REFUSED here so review does not reopen them: `image`/`chart` (bytes on the event bus and a
;; renderer neither surface has — a picture is an attachment a `link` points at), `markdown`/`html`
;; (a closed vocabulary is the only reason the terminal and the phone can agree), `button`/`field`
;; (a view that ASKS is `ask`; blocking belongs to the form), `spinner` (`progress` with a nil
;; value already means indeterminate), `tree` (no caller).

;; What one patch operation DOES. The first four address ONE node BY ID; the last two change the
;; view's SHAPE while it runs. CLOSED.
(def live-ops
  {"set"         :set            ; replace a node's own state (status text, progress value …)
   "append"      :append         ; add lines to a log; upsert rows, steps, stats and links by id
   "remove"      :remove         ; drop keyed ITEMS by id
   "clear"       :clear          ; empty a log, table, step list, stat strip or link list
   "add-node"    :add-node       ; add a WHOLE node mid-run (a second table, a per-device log)
   "remove-node" :remove-node})  ; drop a whole node, its items with it

(def live-tones {"idle" :idle "running" :running "ok" :ok "warn" :warn "error" :error})

;; Why a view ended. CLOSED, and the only vocabulary an extension branches on.
(def live-reasons #{"completed" "interrupted" "timeout" "undeliverable" "failed"})

;; NOTHING here evicts. A `log` is UNBOUNDED: every line goes to the view's sink file and
;; `:window-lines` is only how much of it a surface holds hot. The keyed collections must stay in
;; memory to remain addressable, so they are bounded by REFUSAL — a patch that would exceed the
;; bound is refused with the bound, the node id and `log` named as the home for unbounded volume.
(def log-defaults   {:window-lines 2000 :window-lines-cap 100000 :max-patch-lines 500})
(def table-defaults {:max-rows 5000 :max-patch-rows 200})
(def stat-defaults  {:max-stats 32})           ; a strip, not a spreadsheet
(def step-defaults  {:max-steps 200})          ; a checklist, not a second log
(def link-defaults  {:max-links 32})
(def view-defaults  {:max-nodes 32})           ; 200 devices are 200 ROWS, not 200 panes
(def progress-defaults {:value nil})           ; nil is INDETERMINATE, not zero

;; One dispatch key, two multi-specs: a form node and a live node can never be
;; mistaken for each other, because neither multimethod has a method for the other's type.
(s/def ::type (set (concat (vals field-types) (vals live-node-types))))
(s/def ::tone (set (vals live-tones)))
(s/def ::line string?)                          ; a blank line is a line
(s/def ::lines (s/coll-of ::line :kind vector? :max-count (:max-patch-lines log-defaults)))
(s/def ::window-lines (s/int-in 1 (inc (:window-lines-cap log-defaults))))  ; PAINT window; the sink keeps every line
(s/def ::value (s/nilable (s/and number? #(<= 0 % 1))))
(s/def ::done nat-int?)
(s/def ::total pos-int?)
(s/def ::cells (s/coll-of string? :kind vector?))
(s/def ::table-column (s/and #(closed? column-keys %) (s/keys :req-un [::id ::label] :opt-un [::align])))
(s/def ::columns (s/and (s/coll-of ::table-column :kind vector?) non-empty? distinct-ids?))
(s/def ::row (s/and #(closed? row-keys %) (s/keys :req-un [::id ::cells] :opt-un [::tone])))
(s/def ::rows (s/coll-of ::row :kind vector? :max-count (:max-patch-rows table-defaults)))
;; A table is a KEYED collection, so its paint order has to be DECLARED or the terminal and the
;; phone are free to disagree. `:insertion` (default) keeps first-seen order and an upsert NEVER
;; moves a row — a row that changes stays where the eye left it. `:newest-first` is insertion
;; reversed (a live feed). `{:by "col" :dir :asc|:desc}` sorts by one DECLARED column id, using
;; DataTable's existing rule (numeric when every non-blank cell parses, else case-insensitive,
;; blanks last, ties broken by insertion order so the order is TOTAL and reproducible).
(s/def ::order (s/or :implicit #{:insertion :newest-first}
                     :sorted (s/and #(closed? order-keys %) (s/keys :req-un [::by] :opt-un [::dir]))))
(s/def ::item-ids (s/coll-of ::id :kind vector?))   ; rows, steps, stats or links
(s/def ::max-rows (s/int-in 1 (inc (:max-rows table-defaults))))    ; the REFUSAL bound, not a ring

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
(defmethod live-node-form :log      [_] (s/keys :req-un [::id ::type ::lines ::window-lines] :opt-un [::label]))
(defmethod live-node-form :table    [_] (s/keys :req-un [::id ::type ::columns ::rows ::max-rows ::order] :opt-un [::label]))
(defmethod live-node-form :link     [_] (s/keys :req-un [::id ::type ::links] :opt-un [::label]))
(s/def ::live-node (s/and (s/multi-spec live-node-form :type) live-node-closed?))
;; `::id` is the ADDRESS: chosen by the extension, unique inside the view, and named by every
;; patch. Two tables are two ids, not two views.
(s/def ::nodes (s/and (s/coll-of ::live-node :kind vector? :max-count (:max-nodes view-defaults))
                      non-empty? distinct-ids?))

;; The view itself. `:id`, `:seq` and `:created-at` are ENGINE stamps
;; (`request-stamp-keys`), never written in a spec.
(s/def ::live-view
  (s/and #(closed? live-view-keys %)
         (s/keys :req-un [::id ::title ::session-id ::channel-ids ::nodes
                          ::timeout-ms ::seq ::created-at]
                 :opt-un [::description ::source])))

;; One patch. `:seq` is monotonic PER VIEW, so a surface that sees a gap re-reads the
;; snapshot instead of painting a torn view.
(s/def ::node-spec ::live-node)                 ; the node `add-node` introduces
(s/def ::after (s/nilable ::id))                ; place it after this node; nil means last
(defmulti live-op-form :op)
(defmethod live-op-form :set    [_] (s/keys :req-un [::op ::node-id] :opt-un [::text ::detail ::tone ::label ::value ::done ::total ::stats ::steps ::links]))
(defmethod live-op-form :append [_] (s/keys :req-un [::op ::node-id] :opt-un [::lines ::rows ::stats ::steps ::links]))
(defmethod live-op-form :remove [_] (s/keys :req-un [::op ::node-id ::item-ids]))
(defmethod live-op-form :clear  [_] (s/keys :req-un [::op ::node-id]))
(defmethod live-op-form :add-node    [_] (s/keys :req-un [::op ::node-spec] :opt-un [::after]))
(defmethod live-op-form :remove-node [_] (s/keys :req-un [::op ::node-id]))
;; The address is `:node-id`, not `:node`: `::node` already names a whole node of
;; the FORM tree, and a spec keyword is shared by every map that spells the key.
(s/def ::live-op (s/multi-spec live-op-form :op))
(s/def ::ops (s/and (s/coll-of ::live-op :kind vector?) non-empty?))
(s/def ::live-patch (s/and #(closed? live-patch-keys %) (s/keys :req-un [::view-id ::seq ::ops])))

;; What the blocked extension receives, and what the close event carries.
(s/def ::is-completed boolean?)
(s/def ::is-from-human boolean?)   ; a PERSON stopped it, engine-stamped
(s/def ::live-result
  (s/and #(closed? live-result-keys %)
         (s/keys :req-un [::view-id ::is-completed ::reason ::is-from-human]
                 :opt-un [::note ::summary ::artifact-id ::error])))
```

Booleans are `is_<foo>` on the wire and `:is-<foo>` in the engine, per
`src/com/blockether/vis/internal/gateway/wire.clj`; the snake_case spellings come from `wire-keys`,
so no key is spelled a second time anywhere.

**Acceptance criteria.**

- `src/com/blockether/vis/internal/human_input/spec.clj` — the block above, plus
  `contract-vocabulary` (`:122-137`) exporting the live tables so the Python contract document reads
  one source.
- `src/com/blockether/vis/internal/human_input.clj` — `normalize-live-view` and `normalize-patch`
  (either spelling of every key, closed-table lookups instead of `keyword`-minting, blanks and nils
  dropped BEFORE the materializer, which refuses them), then the lifecycle: `open-live!` (publishes
  `:human-input/live-open`, returns the materialized view), `patch-live!`, `close-live!`,
  `interrupt-live!`, and `with-live!`, which closes what it opened on a throw as well as on a return.
  NOTHING PARKS: a form parks its caller because it must be answered, while a view is the work
  reporting on itself — so the open returns at once and the verdict is the close. Every op names its
  node, so a view with four tables needs no ordering rule and no "current" node.
- Same file — a view is DECLARED without a session and MOUNTED into one: `:session-id` is optional in
  `::live-view` and refused at `open-live!`, exactly as a request's is, so the same builder call works
  in an extension that has not been handed a session yet.
- `src/com/blockether/vis/internal/human_input/live.clj` (new) — the MATERIALIZER and the MODEL's
  renderer, pure and total, kept out of the lifecycle file because three surfaces read it and none of
  them may own a private copy of what the view IS. `apply-patch` is ALL OR NOTHING — a patch whose
  second op refuses leaves the view it was handed, so no surface ever paints half a patch — and it
  refuses a `:seq` that does not ADVANCE, which is what lets a surface treat a gap as *re-read the
  snapshot* instead of as loss.
- Same file — a keyed collection stays the DECLARED vector and an upsert indexes it ONCE per patch:
  an id already present is REPLACED IN PLACE (a counter ticking in row 3 does not throw the table at
  the human), an unseen id is appended to the order, `remove` drops it. One shape for declaration,
  snapshot, patch and paint, so no surface converts between two of them and the row identity a scroll
  anchor is pinned to survives every re-sort. `:order` is applied at PAINT time by `ordered-rows`,
  never by re-sorting the record, and a `{:by "col"}` naming a column the table does not declare is
  refused AT DECLARATION with the known column ids named.
- Same file — removing an absent item, clearing an empty collection and dropping a node that is
  already gone are NO-OPS: a patch states the wanted state, and teardown is idempotent. A WRITE to a
  node that is not there REFUSES, naming the ids the view does have — a patch that lands nowhere is a
  bug, not a state.
- Same file — `set` refuses a key the node's type has not got and names what it does set; `append`
  refuses lines into a table and rows into a log, naming both; a keyed collection pushed past its
  bound refuses with the bound, the node id and `log` named as the home for unbounded volume; and one
  over-large patch refuses with `split it`, because a per-patch cap is a REFUSAL about latency (the
  journal writer parks the producer) and not a second bound on the node.
- Same file — a `:log` keeps a hot WINDOW of `:window-lines` plus an engine-stamped `:total-lines`
  counting the RECORD, so `… N earlier lines` is counted and never guessed, and `clear` empties the
  window while the record keeps every line.
- Same file — `->markdown`, the human's DOCUMENT, rendered ONCE from the same materialized state the
  two human surfaces painted: the verdict FIRST, then every node in declaration order. `status` is a
  bold line over an italic detail, `progress` a percent and a count (`_working_` when indeterminate),
  `stat` one `label value` strip, `steps` a `- [tone] label` checklist, `log` a fence widened past
  whatever backticks the output carries, `table` a GFM pipe table honouring `:align` with cells
  escaped so a pipe or a newline cannot invent a row, `link` a markdown link, a backticked path or a
  named attachment. Colour is the one thing that cannot cross: a `[tone]` token stands where a surface
  paints red, and a table whose rows carry tones grows ONE leading `!` column — the only place the
  render is not a transliteration.
- Same file — the model's BUDGET, and only the model's, shared by `picture` and `->markdown` so the
  data and the document leave the same thing behind: a log answers its TAIL (120 lines) and a table
  its HEAD (50 rows), each saying how many it left behind and that the record still holds them. A
  caller may widen either per call. Truncation is a RENDER decision; nothing here touches the record,
  and neither budget applies to the human's surfaces, which scroll the whole thing.
- Same file — `parse-markdown`, the INVERSE: markdown back to a view, so the human's document is
  two-way rather than exhaust. The law is exact — a picture that elided nothing renders back byte for
  byte, so `(->markdown view {:result result})` IS the markdown it was parsed from — which is what
  makes a view authorable as markdown and a rendered view re-readable. Making the inverse exist paid
  for itself in the render: a description now hugs its title and the verdict stands in a `>` block
  (nothing else paints one, so a first status node can no longer be mistaken for the verdict), an
  error is marked `· error:` so a reader tells it from the summary, a table paints its HEADER even
  with no rows (the columns are the declaration, and `_no rows yet_` goes under them), and a cell is
  trimmed because a pipe table has no way to paint its own padding. Ids are never painted, so each is
  derived from the label the eye reads — deterministically, so a patch written against a parsed view
  still lands on the row it names. What a budget left behind is REPORTED as `:elided`, never guessed.
- Same file — the `pending` registry entry gains `:kind` (`:form` | `:live`) so `cancel-all!`
  (`:1145-1150`), the turn-interrupt path (`:1251-1255`) and the undeliverable path (`:1220-1235`)
  release both kinds, and `checked-answer` routes to `::answer` or `::live-result` by kind.
- `src/com/blockether/vis/internal/human_input/live_sink.clj` (new) — the store of record, because
  no existing sink keeps a stream (ring 2000 events, journal truncated per turn and past 16 MB).
  One append-only NDJSON file per view at `~/.vis/gateway/views/<session-id>/<view-id>.ndjson`
  (`live/` is TAKEN — the bus keeps a turn-liveness marker per session there and LISTS that
  directory, so a view's stream gets a directory of its own rather than a subdirectory somebody
  else's listing has to learn to skip):
  header line = the opened view, one line per ACCEPTED patch, trailer line = the reason it ended.
  Appended BEFORE the patch is published, so a crashed run keeps everything the engine accepted, and
  opened in append mode so a resumed process never overwrites. Reads are a line range
  (`read-range` from/count), which is what lets a surface pull the scrollback it paints and lets the
  artifact be the file itself instead of a re-encoded copy.
- `src/com/blockether/vis/human_input.clj` — builders `status`, `progress`, `stat`, `steps`, `log`,
  `table`, `link` — each taking its ID FIRST — the two POSITIONAL item builders `table-column` /
  `table-row`, the shape ops `add-node` / `remove-node`, and `view`, which mirrors `form`. Each
  validates through the engine's public `normalize-live-node` / `normalize-live-op` /
  `normalize-live-view` seam and DROPS the normalized form, so a mistake is dated to the builder call
  instead of to the human's screen. The four STATE ops (`set`, `append`, `clear`, `remove`) get NO
  builder on purpose: each is a two-key map whose `:op` the engine already refuses by name, and an
  invented `set-node` here would mint a second vocabulary beside the closed table the wire, the phone
  and the terminal all read.
- `src/com/blockether/vis/core.clj` — the runner an extension actually calls, beside
  `request-human-input!`: `with-live-view!` first, then `open-live-view!`, `patch-live-view!`,
  `close-live-view!`, `interrupt-live-view!` and `live-view`.
- Test `test/com/blockether/vis/internal/human_input/live_test.clj` (new) — a view whose patches each
  touch only their own node, `add-node` / `remove-node` mid-run including a WRITE to a node that was
  dropped, every bound refusing with the node and the bound named, the log window kept while
  `:total-lines` counts the record, and the model's markdown asserted as one GOLDEN document (verdict
  first, the `!` tone column, the widened fence, both budgets, the escaped cell).
- Same file — `picture`, the model's surface as DATA: the ids the view DECLARED (a node the model read
  is a node it can patch), tones as keywords, the mount left behind, the same budget the document
  renders with, `:elided` counting what stayed in the record, and a table's declared order applied
  ONCE, so mounting the picture again cannot sort it twice.
- Same file — the round trip, as laws rather than examples: every node type through
  `->markdown` → `parse-markdown` → `->markdown` unchanged (including each empty state, the fence
  inside a log, a pipe and a newline inside a cell, and two nodes sharing a label), every parsed node
  accepted by `spec/live-node-error`, a truncated log repainting its own note while a truncated table
  says what it left behind, a HAND-WRITTEN picture patched by the addresses its labels earned, and a
  refusal per malformed shape naming the line to fix.
- Same file — a table driven through an INTERLEAVED script (add a, add b, add c, update b, remove a,
  re-add a) asserted row by row under each `:order`: insertion proves the updated row did not move and
  the re-added row went to the END (it is a new arrival, not a resurrection), `:newest-first` proves
  the mirror image, `{:by …}` proves ties keep insertion order and blanks stay last, so the same
  script paints identically on every surface. Removing an absent id and clearing an empty table are
  asserted as no-ops that still advance `seq`, and `{:by "nope"}` is refused at declaration naming the
  columns the table does declare.
- Test `test/com/blockether/vis/internal/human_input_test.clj` — the lifecycle half: a view mounted,
   patched BY NODE ID and closed into a verdict carrying the picture as DATA — with the document
   rendered from that same picture on demand; the record
  round-tripped (open + patch + close read back, then reopened and APPENDED to, never truncated); a
  view that names no session refused at the MOUNT; a patch for a view that is not open refused by
  name; close idempotent (a second close is nil, not a second verdict); `interrupt-live!` ending
  `interrupted` and still carrying the picture; `with-live!` closing what a body opened when the body
  THROWS, the record's trailer reading `failed` with the message; and a form and a live view
  coexisting in one registry, where `submit!` refuses a view because a view never asked a question.
- Same file — a view nobody watches still RUNS and still ends in the verdict the model reads: no
  channel mounted is a WARN in the log, never the refusal a form gets, because the whole product of a
  view is the picture at the end.
- Test `test/com/blockether/vis/human_input_test.clj` — the builder half, the same two promises the
  form builders make: every live builder returns the plain map an extension could have typed by hand,
  and every refusal (a tone outside the table, an order no surface paints, an unknown key, a node with
  no id, two nodes sharing one, a view with no nodes) is dated to the builder call with the engine's
  own one-line reason. A view with no session is NOT refused there — the mount owns that.

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
- Same file — a view is a STACK of labelled nodes in declaration order on ONE scroll surface, so
  three tables and two logs read as sections rather than as competing panes. A node paints a WINDOW,
  never its record: a table shows a window of its declared order plus a `+N more` line that expands,
  and a log paints its TAIL — the newest lines are why anybody is watching — and says how many
  earlier lines the view's own record still keeps. `add-node` / `remove-node` reflow without moving
  what the human is
  reading — the scroll anchor is the node id under the viewport, never a line offset.
- Same file — a table under mutation repaints WITHOUT MOVING THE EYE, which is the whole difficulty:
  the anchor inside a table is the ROW ID at the top of the viewport, so rows arriving above it (or
  a row above it being removed) change the scrollbar, not the reading position; only a viewport
  pinned at the end follows new rows, exactly like the log's follow-tail. Column widths are measured
  ONCE per repaint from the painted window and only ever GROW while the view is open, so a wider
  value in row 900 does not shuffle every earlier column; `columns` is fixed at declaration, so a
  table never changes shape under the human. A row upserted or added is emphasised for one repaint
  interval (`theme` tone, the same vocabulary as `steps`), which is what makes a changing table
  readable instead of a flicker — and an optional row `:tone` keeps a failed row red for good.
  A removed row is dropped immediately, never blanked in place: a gap that stays is a lie about the
  state, and the sink is where the history lives.
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
- Same file — the table-under-mutation case gets its own assertions: with the viewport parked mid
  table, rows inserted and removed ABOVE the anchor leave the top visible row id unchanged, a
  viewport pinned at the end follows new rows, a wider cell grows a column and never shrinks it
  while open, and the screenshot gate pins a before/after pair across one interleaved script.

**Unknowns.** None left. Several open views stack as ONE band: the newest is painted in full and
every older one collapses to a single line carrying its title and where it got to, and the band
takes at most half the rows between the transcript and the prompt.

## Phase 3 — Give Python the same primitive through one host op

**Rationale.** Extensions are Python first; a Clojure-only primitive would not touch a single one of
the 16 shipped extensions, and the model's polling loop lives in the Python sandbox.

**Data.** One new host op and its op grammar, mirroring how `shell` is already declared —
`packages/vis-contract/resources/vis-contract/python-host.edn`, `:contract/version` 3 -> 4:

```clojure
{:op/name "live"
 :op/global "__vis_host_live__"
 :op/arity 1
 :op/summary "Open, patch, read or close one live view."
 :op/outside :outside/local}      ; outside Vis: render the patches on stderr

:contract/live
{:live/default-op "open"
 :live/spawn-ops ["open"]
 :live/handle-ops ["patch" "state" "close"]
 :live/flush-ms 100}
```

The envelope it carries is the Phase 1 shape as JSON:
`(s/def ::live-envelope (s/and #(closed? live-envelope-keys %) (s/keys :req-un [::op] :opt-un [::view-id ::view ::patch])))`.

**Acceptance criteria.**

- `packages/vis-agent/src/vis/__init__.py` — `vis.live(title, nodes, **options)` as a context
  manager answering a `LiveView` handle. Nodes are addressed BY ID, because a view with two tables
  has no "the" table: `view["failures"]` (also `view.node("failures")`) answers a typed node handle —
  `Table.upsert(row_id, cells)` / `.remove(ids)` / `.clear()`, `Log.write(*lines)`,
  `Progress.set(value, done=, total=)`, `Status.set(text, tone=, detail=)`,
  `Steps.set(step_id, tone=, label=, detail=, value=)`, `Stat.set(stat_id, value_text, tone=)`,
  `Link.add(link_id, label, target)`.
- Same file — `Table.upsert` is one verb for both "new row" and "row changed", because the caller
  loop that writes a live table does not know which it is: `upsert("dev-7", [...])` inserts the
  first time and replaces in place after, so a scan loop is a `for` over devices rather than a diff
  the extension has to keep. `vis.table(id, columns=[…], order=…)` declares the order once
  (`"insertion"` default, `"newest-first"`, or `{"by": "duration", "dir": "desc"}`); the batching
  tick coalesces repeated upserts of the SAME row id into the last one, so a per-row progress
  counter costs one wire row per tick instead of one per tick per write.
- Same file — the view-level shortcuts (`view.write(...)`, `view.row(...)`, `view.status(...)`,
  `view.progress(...)`, `view.stat(...)`, `view.step(...)`, `view.link(...)`) stay for the common
  single-node case and resolve to that node ONLY when the view holds exactly one node of the type;
  otherwise they raise naming the candidate ids, so an ambiguous view fails at the call instead of
  quietly patching the wrong table. `view.add(vis.table("passed", columns=[…]))` and
  `view.drop("device-7")` reshape a running view; node builders `vis.status`, `vis.progress`,
  `vis.stat`, `vis.steps`, `vis.output`, `vis.table`, `vis.table_column`, `vis.table_row` and
  `vis.link` all take the id first. The log builder is `vis.output` because `vis.log` is already the
  engine log line — the same collision `vis.slider` answers by building a `range` field. The handle
  carries `is_interrupted` and `reason`; a push after an interrupt raises `vis.Interrupted`, so an
  unattended loop stops by itself while a compute loop can poll the flag.
- Same file — a push per line would be a host round trip per line, so the handle BATCHES: ops buffer
  and flush on a tick (`:live/flush-ms`, default 100), at `:max-patch-lines`, and on close. That is
  a correctness property rather than a nicety, because the engine's durable publish parks its
  producing thread until the journal writer acknowledges the event (`gateway/bus.clj:567-589`).
- `resources/vis-python/extension_bootstrap.py` — `live=__vis_host_live__` in `_host`.
- `src/com/blockether/vis/internal/python_extensions.clj:459` — the binder binds `__vis_host_live__`;
  `host-member-names` reads the contract document, so it gains `live` with the op itself.
- `src/com/blockether/vis/internal/human_input.clj` — `live-json!`, the strings-only seam beside
  `request-json!` (`:1312-1345`).
- Test `test/com/blockether/vis/contract/python_host_test.clj` (drift: document, bootstrap, package
  and engine dispatch agree) and `test/com/blockether/vis/internal/extension_check_test.clj` (a
  static check of an extension that opens a live view).
- `packages/vis-agent/src/vis/_outside.py` — `live(envelope_json)` in `_IMPLEMENTATIONS`, judged by
  `_check_view` on the same path `_check_request` judges a dialog, so `vis.live` refuses outside a
  Vis host exactly as it refuses inside one; and `resources/vis-python/extension_check.py` gains
  `_OPENS ("live",)` beside `_ASKS ("ask",)`: the checker judges a declared view by CALLING it on the
  inert host (`internal/extension_check.clj` `judge-view`, the second judge beside `judge-request`),
  which is the only pre-flight an unattended extension gets.
- `resources/vis-docs/extending.md` — a live view named in the `extension check` list, which promises
  ONE judge, so a primitive missing from it reads as a second one; plus the section "Showing the
  human live work" carrying the shell case at the EXTENSION layer where it belongs — spawn a
  background shell, drain its ring (`internal/foundation/shell.clj:116`) into a `log` node the
  extension labels, return one verdict to the model.

**Unknowns.** Answered: stderr alone is not enough. `:outside/local` prints one line per patch AND
materializes the view, so a harness can assert on `state()` — the same upserts, the same bounds — and
a view that has already ended answers its verdict instead of vanishing.

## Phase 4 — Bridge it to the companion app

**Rationale.** Half the reason to stream at all is the phone: the operator who walked away wants to
see the build move and stop it from the couch. The gateway already turns HITL channel events into
session events; without this phase the app stays blind exactly as it is today.

**Data.** None. The three session events carry the Phase 1 shapes verbatim through
`wire/canonical`; no key crosses that Phase 1 did not declare.

**Acceptance criteria.**

- `src/com/blockether/vis/internal/gateway/human_input.clj:77-88` — three more ops, and all three
  are STORED exactly like every other session event (`:store?` left defaulted; the transient flag
  keeps having no caller in this repo). `human_input.live.open` and `human_input.live.close` are the
  lifecycle a late-joining client replays; `human_input.live.patch` carries the op vector the engine
  already accepted and already appended to the sink. Storing them is what makes a TUI mirroring the
  session from ANOTHER process paint the same view, and ring eviction is not data loss here for one
  reason worth stating in the code: that ring is a 2000-event reconnect cache for the whole session
  and explicitly not the record (`gateway/state.clj:37-40`) — the record is the view's sink file
  plus the session transcript.
- Same file — patches are COALESCED per view on a fixed tick before publishing (superseded `set` ops
  dropped, `append` lines merged, `add-node`/`remove-node` never dropped). Not an optimization: a
  durable publish parks the producing thread until the journal writer acks, up to 5 s
  (`gateway/bus.clj:567-589`), and the cross-process journal is force-truncated past 16 MB mid-turn
  (`:53,488-499`). The tick bounds thread cost and file churn while losing nothing, because the sink
  already holds every accepted patch and the snapshot route below serves the repair.
- `src/com/blockether/vis/internal/gateway/server.clj` — `GET /v1/sessions/:sid/human-input/live`
  answering the materialized snapshots (the resync path for a client that joined mid-flight or lost
  SSE), `GET …/live/:view-id/log/:node-id?from=<line>&limit=<n>` reading the sink range so a phone can
  scroll back through a log whose patches it never received, and
  `POST /v1/sessions/:sid/human-input/live/:view-id/actions/interrupt`, registered beside the form
  routes. The view id is parsed as a UUID before anything reaches the record directory; an unknown view
  is 404. There is NO 409: a view asks nothing, so no view may refuse the stop, and the body may carry
  `{"note": "…"}` — the comment the person typed with it.
- DROPPED: the session list's `awaiting` (`:1702`) does NOT gain a working state. `awaiting` means
  "this run needs YOU"; a live view needs nobody, and the row's own `live` flag already says a turn is
  moving. A working state would cost a new cross-process marker scan for a distinction the row makes.
- `apps/vis-companion/src/lib/live-view.ts` — the pure reduction of the three events into view state
  KEYED BY NODE ID (`add-node` and `remove-node` included), a `seq` gap read as RESYNC rather than
  loss — a reconnect, a dropped frame and an evicted ring entry look identical from the client, and
  the snapshot plus the log-range route are the repair — and the mirrors of the closed tables
  (`LIVE_NODE_TYPES`, `LIVE_OPS`, `LIVE_TONES`, `LIVE_REASONS`, `LIVE_LINK_TARGETS`, `LIVE_ALIGNS`,
  `LIVE_ORDERS`, `LIVE_SORT_DIRS`) and the two bounds a client must not exceed on its own
  (`LIVE_LOG_WINDOW`, `LIVE_TABLE_MAX_ROWS`), the same way
  `lib/human-input.ts:22-40` mirrors the form vocabulary.
- `apps/vis-companion/src/components/LiveView.tsx` + `lib/live-view.fixture.json` (one node of every
  kind, generated as the engine's own projection of a declared view) rendering in
  `screens/SessionScreen.tsx` at the END of the transcript, where the newest thing in the session
  belongs — a view blocks nothing, so it is a section and never a scrim. Nodes are labelled sections in
  declaration order, the transcript's own scroller keeps following the tail, and a log pages older
  lines through the range route, per `doc("companion-ui")`. No `dev/liveViewVariants.tsx`: this app has
  no dev route that would render a catalogue, so the variants live in `LiveView.test.tsx`, where they
  are asserted instead of eyeballed.
- `apps/vis-companion/src/components/DataTable.tsx` — NOT reused, and this plan was wrong to promise
  it. It takes `body: string` — an artifact's table BLOCK parsed through `parseTableBlock`/`parseCsv`
  (`:246,251-252`) — and keys its rows by their INDEX (`:255`), which is exactly the identity a live
  table must not lose. Feeding it live rows would mean serializing them back into text and throwing
  away the row ids, tones and declared alignment. The live table is its own `<table>`, rows keyed by
  ROW ID so React moves a row instead of rebuilding it. The human's header sort goes with it: a live
  table is written to while it is read, a column the operator sorted by would re-shuffle rows under the
  thumb on every patch, and the declared order is the one the terminal paints too.
- `apps/vis-companion/src/components/ui.tsx` — the ONE control the vocabulary needs and the app does
  not have: `Meter`, a segmented progress bar (twenty cells, filled against the declared fraction).
  Segmented because no inline style may exist in this app and Tailwind cannot compile a runtime width —
  and because the terminal paints the same segmented bar, so the two surfaces agree by construction. An
  indeterminate progress gets NO bar, only its own words. The rest of the closed set covers the live
  vocabulary WITHOUT stretching it: `Spinner` for a view still running, `Banner` for a refusal,
  `LoadMore` for the log's earlier lines, `Button` for interrupt. `Pill` (`:750`) and `ListRow`
  (`:464`) are NOT reused for `stat`, `steps` or `link` rows for one reason: both are `<button>`, and a
  live row is not pressable — painting it as a control would promise a press that does nothing.
- `apps/vis-companion/src/lib/gateway.ts` — `liveViews`, `liveViewLog` and `interruptLiveView` beside
  the form's own three, and the component resyncs from the snapshot route on reconnect and on a `seq`
  gap (`:3182` is where the same argument is already made for `human_input.request`).
- Test: `extensions/channels/vis-channel-tui/test/.../human_input_cross_channel_test.clj` extended with
  a deftest that reads the new TypeScript tables and the three event names back out of
  `lib/live-view.ts` and fails on drift against `hi-spec` and `gateway/human_input.clj`;
  `apps/vis-companion/src/lib/live-view.test.ts` for the reducer laws (upsert in place, the log window
  sliding while `total_lines` keeps growing, `clear` emptying the window and not the record, a replayed
  frame dropped, an unknown op still ADVANCING `seq` so the next frame is not read as a gap, a gap
  marking the view stale); `LiveView.test.tsx` rendering the fixture and proving the screen by numbers
  off the DOM (node order, row count, right-aligned header, `role="status"` and no dialog,
  `aria-valuenow` 67 with its `2/3`, an indeterminate progress with no bar, the engine's own empty
  sentence, the log's hole line after paging, interrupt present only when the view allows it);
  `test/com/blockether/vis/internal/gateway/human_input_test.clj` for storage, coalescing (one frame
  carrying `first_seq`..`seq`), the log-range route, the 409 and the fixture being the engine's own
  projection.

**Unknowns.** Answered: no push notification. A live view is not a question, and the phone already
shows it the moment the app is open; a notification for a view that closes `failed` while the app is
backgrounded stays a separate decision, not a silent default.

## Phase 5 — Settle a finished view into an artifact the human can reopen

**Rationale.** By this phase the record already exists — the sink has been written since `open` —
so nothing here copies a buffer anywhere. What is missing is REACHABILITY: after the pane is
dismissed the human has no way back to the log they were watching, and the only alternative would be
dumping it into the transcript, which is the cost this plan exists to remove.

**Data.** The stored artifact, declared in `internal/human_input/spec.clj` beside the rest:

```clojure
(def live-artifact-media-type "application/vnd.vis.live+json")
(s/def ::live-artifact
  (s/and #(closed? live-artifact-keys %)
         (s/keys :req-un [::id ::view-id ::session-id ::title ::media-type ::audience
                          ::ended-at ::reason ::view ::storage-uri ::size ::line-count]
                 :opt-un [::base64])))   ; inlined only under the small-view threshold
```

`::view` is the FINAL materialized state (the summary a surface opens instantly); the bytes are the
sink file the run already wrote, addressed by `::storage-uri`. `::audience` is the existing closed
vocabulary (`internal/attachments.clj:670-679`) and a live artifact is human-only: the model is told
it exists and gets the summary, never the bytes. The artifact stores no markdown copy: `::view` is the
final materialized state, and `live/->markdown` re-renders exactly what the model was given from it.

**Acceptance criteria.**

- `src/com/blockether/vis/internal/human_input.clj` — on close the artifact IS the sink file that has
  been growing since `open`: write the trailer line, register the attachment with
  `:storage-uri "vis-live://<session-id>/<view-id>"` — a scheme `live_sink.clj` OWNS and registers a
  read-only rail for, because `attachment_storage.clj`'s `file-backend` is explicitly not auto-registered
  and an address no rail resolves is a 404 on exactly the long runs this artifact exists for —
  `:size`, `:line-count` and the materialized final view as its summary. No base64 round trip:
  `offload-attachment` wants the whole payload in memory as base64 (`:174-198`), and a build log is
  precisely the thing that must never be held that way. A view under the inline threshold is ALSO
  inlined, so a small one survives a session sync. The close event and the extension's
  `::live-result` carry the resulting `artifact-id`.
- `extensions/channels/vis-channel-tui/src/.../channel_tui/live_view.clj` — a closed view collapses
  to one clickable line (title, reason, line count, elapsed) registered in `click_regions.clj`, which
  reopens the full scrollback read-only.
- `apps/vis-companion/src/lib/artifacts.ts` — classify the media type so a finished view appears in
  `ArtifactsSheet`; `components/LiveArtifact.tsx` renders the summary with the Phase 4 node painters
  in read-only mode and pages the log through the same range route, so a 100 000-line run opens on one
  screenful. The record itself arrives as ONE artifact through the shared attachment cache (the byte
  endpoint serves no ranges): what the two-ended fold saves is parsing it, not downloading it.
- Test: `test/com/blockether/vis/internal/human_input_test.clj` (artifact registered once on every
  reason including `interrupted` and `failed`, the sink file REFERENCED rather than re-encoded, and
  the line count matching what was written), `apps/vis-companion/src/lib/artifacts.test.ts`
  (classification), and a TUI test that reopens a closed view after the pane was dismissed.

**Unknowns.** Nothing is left open about WHAT is kept: the sink keeps every accepted patch and the
artifact is that file — settled on review, not deferred to the first extension. The open question is
lifetime: a sink outlives its session unless something deletes it, and `bus/forget!`
(`gateway/bus.clj:636-640`) is the existing precedent for dropping per-session files on close. The
plan assumes a sink is kept with its session and removed with it.

## Phase 6 — `gh`: the first live view a person actually watches

**Rationale.** Everything above is mechanism. A CI run is the archetype the mechanism was built for:
it takes fifteen minutes, the extension can see exactly when it ends, the human wants to WATCH it, and
the model needs one paragraph at the end. Today that run is observed the expensive way — `sh.logs()`
in a loop, one provider round trip per look — or not at all. It is also the honest end-to-end proof:
the payload comes from a system this repo does not control, so a view that survives it survives real
data. The mapping below was run against a real 18-job run of this repository (`gh run view --json`)
through `live/apply-patch` and `live/->markdown` before it was written down.

**Data.** The declared view — seven nodes, one per thing a person asks about a run:

```
status   "run"      "12 of 18 jobs finished, 1 failed"   detail: "workflow CI on main"
progress "progress" done = finished jobs, total = all jobs
stat     "score"    passed · failed · skipped · queued
table    "jobs"     Job | Status | Took   rows keyed by the job's databaseId, :insertion order
steps    "failing"  the running (else first failed) job's steps, one tone each
log      "output"   that job's log, appended per poll
link     "links"    the run URL, plus one per failed job
```

One mapping, used by every node: `status`/`conclusion` -> tone — `queued`/`in_progress` is `running`,
`success` is `ok`, `skipped` and `neutral` are `idle`, anything else is `error`. Rows are upserted by
job id every poll, so a job that changes state keeps its slot and the eye keeps its place.

**Acceptance criteria.**

- `.vis/extensions/gh.py` — one file, `vis.extension(name="gh")`, tools `gh_watch_run` (a run id, or
  the newest run of the current branch) and `gh_watch_checks` (the same view over `gh pr checks`).
  Opens the view, polls `gh run view --json jobs,status,conclusion,…` on a 5 s tick backing off to
  15 s past five minutes, patches ONLY what changed, and returns when the run ends.
- Same file — every GitHub call is the `gh` CLI through the sandbox shell verb. No hand-built HTTPS,
  no token read, copied or printed: authentication is the operator's own `gh` session, and a missing
  or unauthenticated `gh` REFUSES with one line before any view is opened — nothing to watch beats an
  empty pane.
- Same file — the tool returns the view's PICTURE, whatever ended it. On `interrupted` the model
  still receives the state the human was looking at when they pressed Escape, plus the reason, and
  decides what to do next; on `completed` it receives the finished run. The log node is the failing
  job's tail, so the model's copy is bounded by the Phase 1 budget and the whole log stays in the
  view's record.
- `resources/vis-docs/extending.md` — this replaces the background-shell example as the worked
  example for `vis.live`: it is shorter, it is real, and it shows the two things an author gets wrong
  (one node per question rather than one log for everything, and upsert-by-id rather than re-appending
  the whole table).
- Test — a recorded `gh run view` payload (one real run, trimmed) as a fixture: the mapper builds the
  declared nodes and the patches, the engine accepts them, and the model's picture is asserted
  against a golden document. No network in the test; the mapping is pure and the polling is not what
  is being proven.

**Unknowns.** Whether the failing job's log should stream continuously or only once a job fails — the
plan assumes only the running-or-failing job, appended per poll, because eighteen job logs at once is
a download rather than a view. And whether `gh_watch_checks` is the same mapper over a different
payload (assumed yes) or its own view.

## State of the plan

**DONE** — Phases 1-6 are landed and green; the plan is finished.

Done:

- Phase 1, the vocabulary — `internal/human_input/spec.clj` carries the closed live tables
  (`live-node-types`, `live-ops`, `live-tones`, `live-orders`, `live-reasons`, `link-targets`,
  `item-bounds`, `live-op-key-sets`), the specs (`::live-node`, `::live-view`, `::live-op`,
  `::live-patch`, `::live-result`) and the four explainers, exported through `contract-vocabulary`
  into `vis_contract/contract.json`. Commit `a566ad102`.
- Phase 1, the materializer and the model's renderer — `internal/human_input/live.clj` with
  `materialize`, `apply-patch`, `ordered-rows` and `->markdown`, pinned by
  `test/com/blockether/vis/internal/human_input/live_test.clj` (the interleaved table script under
  every declared order, every refusal, the budgets, and one golden markdown document).
- Phase 1, the document both ways — `parse-markdown` in the same file, with the render changes
  that made the inverse total (blockquoted verdict, marked error, a table's header always painted,
  trimmed cells). 37 tests in `live_test.clj`, 162 green across human-input.
- Phase 1 COMPLETE — the lifecycle, the record and the builders. `normalize-live-view` /
  `normalize-live-node` / `normalize-live-op` / `normalize-patch` and `open-live!` / `patch-live!` /
  `close-live!` / `interrupt-live!` / `with-live!` in `internal/human_input.clj`; the append-only
  record in `internal/human_input/live_sink.clj` under `~/.vis/gateway/views/<session-id>/`; `:kind`
  in the pending registry, where `submit!` refuses a view and a cancel closes one; the live builders
  and `view` in `com.blockether.vis.human-input`; the runner exports in `com.blockether.vis.core`.
  205 green across the three human-input test files. A view is declared without a session and refused
  at the MOUNT if it still names none — a builder is callable before a session exists.
- Phase 1, the model's surface is DATA — `live/picture` hands the verdict the finished view as values
  (the ids the view declared, tones as keywords, numbers as numbers) under the SAME budget
  `->markdown` renders with, `:elided` counts what stayed in the record, and a table's declared order
  is applied once so mounting the picture again cannot sort it twice. `::live-result` now REQUIRES
  `:view` and refuses `:markdown`: markdown is the HUMAN's document — what an artifact stores, what a
  transcript embeds, what a hand-written picture is authored as — never the model's contract. 314
  green across the human-input, gateway, contract, python-extensions and core test files.
- Phase 2 COMPLETE — the terminal paints it.
  `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/live_view.clj` plans a
  view as ONE scroll surface of labelled nodes in declaration order, windows every node to twelve
  items behind a `+ N more` line that expands on a CLICK (the pane never takes the keyboard), anchors
  the eye on `[node-id item-id]` so rows arriving above it move the scrollbar and not the reading
  position, grows column widths and never shrinks them while a view is open, and emphasises exactly
  what the last patch upserted. `state.clj` keeps the panes per tab, `screen.clj` dispatches the three
  events, scrolls the band under the wheel and gives Escape to the newest cancellable view before the
  turn, `footer.clj` keeps a scrolled-away view legible. Its ink is DATA and not prose: a counter
  wears its own tone, and a progress that declared `:done` of `:total` paints the bar that fraction
  earned — `live/fraction` and `live/percent` are the ONE definition the document and the pane share.
  10 tests in `live_view_test.clj` including the screenshot gate, 586 green across the eight
  neighbouring TUI suites.
- Phase 3 COMPLETE — Python gets the same primitive through one host op. `live` is declared in
  `packages/vis-contract/resources/vis-contract/python-host.edn` (`:contract/version` 4,
  `:live/flush-ms 100`), rendered into `vis_contract/contract.json`, bound in
  `internal/python_extensions.clj` and answered by `live-json!` / `live-dispatch` in
  `internal/human_input.clj`. `vis.live(...)` is a context manager whose handle addresses nodes BY ID
  (`view["failures"].upsert("web-1", […])`), folds a burst into one patch per 100 ms window on the
  LEADING edge, flushes before every read and every close, raises `vis.Interrupted` once the human
  stopped watching, and answers the verdict as DATA. The log builder is `vis.output` because
  `vis.log` is the engine log line. Outside a Vis host the view still runs — a stderr transcript AND
  a truthfully materialized state, so an extension is written once and behaves the same in both
  places. `extension_check` grew the SECOND judge (`judge-view` beside `judge-request`), so a
  declared view is refused before a human ever sees it. 59 Python tests across `test_outside.py` and
  the contract package, 27 Clojure tests across `python_host_test.clj` and `extension_check_test.clj`;
  `resources/vis-docs/extending.md` teaches it with the worked background-shell drain.
- Phase 4 COMPLETE — the phone watches it too. `internal/gateway/human_input.clj` stores all three live
  events like every other session event and coalesces a burst into ONE frame per 100 ms tick carrying
  `first_seq`..`seq`, so a client can tell a merge from a gap; `internal/gateway/server.clj` answers the
  snapshot, the sink's log range and the interrupt (404 for an unknown view; there is no 409 — no view
  may refuse to be stopped). `apps/vis-companion/src/lib/live-view.ts` is the engine's materializer written once more
  in TypeScript — ops applied BY NODE ID, a replayed frame ignored, a view it cannot paint a single node
  of DROPPED rather than shown as an empty box, and a `seq` gap marking the view stale so the section
  re-reads the snapshot instead of painting a picture it already knows is wrong. `LiveView.tsx` renders
  it at the END of the transcript, where the newest thing in the session belongs, and `Meter` is the one
  control the app was missing — the closed set covered the rest, and `Pill`/`ListRow` were refused for
  live rows because both are `<button>` and a row that is not pressable must not look like one.
  `human_input_cross_channel_test.clj` fails a Clojure test the day the TypeScript vocabulary drifts
  from `hi-spec`. 1491 app tests over 134 files with `npm run lint` and `npm run build` clean, 386 green
  across the engine, gateway and TUI suites.
- Phase 5 COMPLETE — a finished view SETTLES into an artifact instead of vanishing. `close-live!`
  builds the artifact BEFORE it retires the view, so what it addresses is the record the run itself
  wrote: `hi-spec/live-artifact-media-type` (`application/vnd.vis.live+json`), `:storage-uri`
  pointing at the sink's NDJSON, `:size` / `:line-count` from one streamed pass
  (`live-sink/stats`), the materialized `:view` as the summary a surface opens instantly, and bytes
  ONLY under `hi-spec/live-artifact-inline-bytes` (256 KiB) — holding a build log in memory as
  base64 is the cost this design removes. The verdict gains `:artifact-id`, that same verdict seals
  the record as its trailer, and the close event carries the result, so every surface settles from
  ONE frame. The TUI collapses a settled pane to one line — tone glyph, title, reason, the record's
  line count, elapsed FROZEN at the end — that clicks back open, keeps only the newest settled pane
  beside the open ones, and stops offering to interrupt what has already ended. The app sorts the
  row by MEDIA TYPE into a new `live` artifact kind with its own filter and plate (`RUN`, never
  `LIVE`: a finished run is not a live one), and `LiveArtifact.tsx` folds the record back into a
  picture — the TRAILER's own view wins over any replay of it, a record past 1 MB is read at its
  two ENDS and never in the middle, and the log is still paged from
  `…/human-input/live/:view-id/log/:node-id`, which reads the FILE and therefore answers for a view
  that has already closed. 151 green in `human_input_test.clj`, 15 in the TUI's `live_view_test.clj`
  (with a PNG gate on the settled pane), 1526 app tests over 135 files, `npm run lint` and
  `npm run build` clean.
- The stop became a CONVERSATION, and `:is-cancellable` left the live vocabulary entirely. A question
  may be mandatory; a view asks nothing, so nothing may refuse to stop one. Escape in the terminal and
  Interrupt on the phone both ARM the stop and open one line for a comment — Escape or Enter ends the
  view with it, Backspace on an empty line (Keep watching on the phone) returns to watching — and the verdict the model reads carries the
  finished picture plus `is_from_human` and the human's `note` (trimmed, cut at `hi-spec/note-chars`
  500, never refused for length). `close-live!` takes the human as a THIRD argument the extension
  cannot forge: `:is-from-human` and `:note` are engine stamps, and the spec refuses a note nobody
  left, a human stop that does not read `interrupted`, and any verdict that will not say who ended it.
- Five questions about the SURFACE, answered in the surface rather than argued about. (1) A table now
  FILLS the band: `filled-widths` hands the slack back to the WIDEST column after `fitted-widths` has
  taken it away, so the rule spans the pane instead of hugging the text. (2) Every human-facing string
  paints INLINE markdown — code spans, bold, italic, links — on both surfaces (`md-runs`/`md-lines`
  over the TUI's own markdown walker; `InlineMarkdown`, extracted from `ChatContent.tsx`, for the app),
  while a log line stays VERBATIM because it is machine output. A markdown NODE stays refused: the
  view's document is two-way, one markdown form per node type, so a fence IS a log and a pipe table IS
  a table — a free-markdown node could not be read back as the type it was declared. (3) Escape STOPS.
  It arms the stop and Escape again sends it; Backspace on an empty line is the way back. Enter still
  sends, but the key a person reaches for to abandon something must be the key that abandons it. (4)
  The armed note row is FENCED on both sides — `paint!` reserves two rows and rules above it as well as
  below, so the line a human types into is not floating in the band. (5) A node can stand BESIDE the one
  before it, and a status node's prose WRAPS and JUSTIFIES to its column, so a paragraph can stand to the
  right of the table it explains. That shipped first as a node key `:is-aside`; the session below replaced
  it with the form's own layout group, which is what a reader already knows.
- The live view lays out with the FORM's own aligners, and a table is a BOX. `:is-aside` is GONE from the
  vocabulary — a view's nodes may hold `{:type :group :id … :direction :row|:column :fields [...]}`, reusing
  `hi-spec/group-type` and `group-directions` verbatim, and the TUI's side-by-side machinery moved into
  `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/columns.clj` so the form
  painter and the live painter share ONE vocabulary instead of two copies of it. Layout is DECLARED and
  never patched (no op key names `:direction` or `:fields`); ids are unique across the TREE; `add-node
  :after` lands in the sibling's group, `remove-node` on a group takes its children, and `picture` /
  `->markdown` FLATTEN groups so the model reads content and the two-way document still round-trips.
  `vis.row` / `vis.column` take a leading id string and now serve a question and a view alike. A table
  paints its own rails — `┌┬┐ │ ├┼┤ └┴┘` with a rule between every pair of rows in the terminal,
  `border-collapse` with a border on every cell in the app, the empty sentence INSIDE the box. An IMAGE
  node stays refused: a picture is an attachment a `link` points at.
- Nothing in this feature is deprecated on the way out: a var, type or constant with no reader is
  DELETED. This pass took the four TUI pane fns and the two gateway live constants nobody calls
  outside their file down to private, deleted the app's `LiveVerdict`, `LIVE_REASONS` and `LiveReason`
  (the phone never paints why a view ended — the transcript carries the verdict) and un-exported the
  two wire parsers behind `liveViewFromWire`. The one dead thing kept is a CALL that was missing:
  `gateway/server.clj` `stop!` now calls `gw-human-input/uninstall!`, so a gateway going away
  publishes the patches it buffered instead of swallowing them.
- Phase 6 COMPLETE — the first live view a person actually watches, and the end-to-end proof of the chain.
  `.vis/extensions/gh.py` watches a GitHub Actions run through the `gh` CLI: seven declared nodes (`run` status,
  `progress`, the four-counter `score`, a `jobs` table upserted by `databaseId`, the failing job's `failing` steps,
  the `output` log and the `links`), a 5s tick that backs off to 15s after five minutes, and a 90-minute cap. Only
  what MOVED since the last poll crosses the wire, focus follows the job that failed (its checklist cleared before
  the new job's steps land), and `gh_watch_checks` maps a pull request's checks onto the same seven answers. The
  Unknown is settled by the CLI itself: `gh` refuses a job log while the run is in progress, so the log pane holds
  one line until the end and then shows a window cut at the last `##[error]`. Proof is two REAL polls of one real
  run — `.vis/extensions/test_gh.py` (13 tests) pins the mapper and the envelopes the extension SAYS, and
  `test/com/blockether/vis/internal/human_input/gh_live_test.clj` (4 tests) replays those very envelopes through
  `live-dispatch` and `live-json!` and asserts the picture and the markdown the model reads, so a drift between the
  Python surface and the engine is red in Clojure. `resources/vis-docs/extending.md` now teaches this extension.
- Its predecessor plan (make every capability an extension declared by one cross-language contract) is
  parked at commit `6ac932db4` and is recoverable from there; its open decisions — the TypeScript
  binding and the publishing identity — are untouched by this work and outlive it.

TODO, in order: nothing remains.
