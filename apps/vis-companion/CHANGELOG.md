# Vis Companion — release notes

What each TestFlight build changed. Edit before uploading; the release script never rewrites an existing entry.

## 0.1.37 (4154) — 2026-08-14
<!-- commit: a80a5a898559b37fdd0925aa4b202dfb0ef9bdc5 -->

- Every row wears its own verbs: Star, Rename and Delete stand in a session row, Make primary, Rename and Forget in a machine row — always visible, never a swipe or a hidden menu
- The machines band is one line, and pairing is a labelled "Add a machine" button instead of a bare plus
- Notifications answer one question — connected or not — with a single switch, instead of listing every push token the machine holds
- The composer refits its height when its own box narrows, not only when you type
- A turn settles only itself, keeps the answer it just painted, and refetches a short transcript
- A staged photo is no longer rewritten into storage on every keystroke
- A copied session id is marked vis_session_id#<uuid>
- Search answers freshest first, which is the order the session list is already in
- The italic font axis is imported, so italics actually slant
- Every theme ships as static CSS, with no /v1/theme round trip to the gateway

## 0.1.35 (4124) — 2026-08-14
<!-- commit: 52f098458a174d9adffa3ed35f99f2aac1224061 -->

- Keep a reaped child's pid from signalling the child that took its slot
- A search settles on the word typing rested on, and reports it on its own line
- One microphone in the Companion composer: tap acts, hold switches the mode
- Confine the extension child-pid handoff to the spawning thread
- Keep only the sentences the summarizer finished
- Give an extension's Popen the child's real OS pid
- Pin the Vis JVM heap ceiling at an explicit 5 GiB
- Add half-duplex voice conversations
- Preserve streamed answers during transcript handover
- Fix Android Bluetooth voice capture
- A helper never gets a tool's name: refuse it, and never restore one
- A restored toolbox is the whole toolbox, not the parseable half
- A user turn is no paint island either
- A helper outlives the process: defs() lists them, a restart re-creates them
- A machine known to be dark is asked no further search questions
- Anchor ruff's per-file-ignores at the canonical path
- A dead gateway is an absence, not an answer of "no matches"
- A helper you defined stays readable, so you refine it instead of re-pasting it
- Search answers in a quarter of the time, and says where it is while it runs
- Write a program, not a transcript: teach the core prompt the shape of a good block

## 0.1.35 (4090) — 2026-08-13
<!-- commit: 4b67f7bafd8181c495f9397bf5eabfe01242d310 -->

- Companion: the notch strip stands ON the dialog band, not inside it
- Plan: let a session speak to the other sessions in its tree
- Name tests by path, and let each pack say pass and fail itself
- Give a switched-off toggle a frame it can be found by
- Name the session surface after the verb, not the storage table
- Give an edit a coordinate again: cat mints it, patch spends it
- Say what pairing is doing, and bound how long it can take
- Delete a session without re-downloading the fleet
- Honour every TLS option the sandbox shims accept, or refuse it out loud
- Count the tests that THREW, and read the cli tally as counts
- Never lose the TestFlight notes to one bad minute from Apple
- Collapse run_tests faults into ONE typed failures list
- Show every repository in the magit buffer, not the first twelve
- Plan: bring back cat and patch, and make grep speak anchored text
- Companion: record the 0.1.35 (4075) TestFlight build

## 0.1.35 (4075) — 2026-08-13
<!-- commit: 303d39e0132803b54e94db9293b6828bfbde6901 -->

- Retire the automatic outbox capture; `attach` is how an artifact is kept
- Hand the finished answer the trace the live bubble already painted
- Fix #141: an unverified HTTPS request was impossible from the sandbox, and the escape hatch was deleted
- Hand-written config keeps the provider and model pair; the committed file still cannot
- Make the provider/model pair a remembered selection, not configuration
- Fix #140: a committed vis.yml forced one developer's provider on every clone
- Companion: the result band gets its name back, in caps and bold
- Companion: a machine retry answers in five seconds, in red, then goes quiet
- Name every collapsible band in caps and bold
- Fix Android gateway and optional push support
- Update Bridge extension for 0.3
- Migrate verification governance to Bridge 0.3
- Keep a focused field's accent ring inside the frame's rails
- Fix #139: concurrent shells shared one auto-derived handle id
- Sandbox: pin that a cancel reaches a host park of any length
- Sandbox: cancel a parked block through the guest safepoint
- Companion: a turn that is running always says it is running
- Release: macOS arm64 builds on GitHub's hosted runner, never a laptop
- Native: lanterna's TTY control decides in the BINARY, not the builder
- Native: an extension declares what the IMAGE needs, not only discovery

## 0.1.35 (4047) — 2026-08-12
<!-- commit: 3605627d4c480ff9816ae24f17724b8ca6e107a1 -->

- Companion: record the 0.1.35 (4044) TestFlight build
- Build the macOS asset on a cloud Mac, never on a laptop
- One header per repository in the magit buffer

## 0.1.35 (4044) — 2026-08-12
<!-- commit: c83786fd6d61b4a9810ec08a04b090fd12901637 -->

- List vis.yml's declared repositories in the magit buffer
- One header per repository in the magit buffer, duplicates collapsed

## 0.1.35 (4042) — 2026-08-12
<!-- commit: 59354fd64c245c588a43a67274e99ef54de33e54 -->

- Native: every built-in extension ns reaches the image, or the binary dies
- Wait for the pty child, and let the jail greet the Linux runner
- The last red tests — the relay's name, a reloaded card, pasta's log
- Native: no FFM TTY downcalls on Linux — the binaries ship again
- Green the suite — the relay's name, a reloaded hook, a stack-safe scan
- Name svar's unroutable failure instead of the generic provider card
- Cancellation: a best-effort catch no longer eats the interrupt
- Prompt: run_tests reloads what it RUNS, never its dependencies
- Shell: a cancel during usage sampling is no longer swallowed
- Companion: a starred row keeps its page, so the star lands where the thumb is
- Companion: a star you can see on the row and read on the strip
- Companion: drop the artifacts gallery's dead tool_name read
- Pytest shim: capfd captures the real descriptor (#138)
- Companion: a machine's dot reports a probe, not the memory of one
- Shell: one stream, no dead stderr field (#137)
- Companion: let the pen write on a picture instead of selecting it
- Companion: give every band's way out the band's own cell
- Show every nested repository in the magit status buffer
- Price the fold trigger in the core prompt's budget line
- Name session_fold in the rule that orders the fold

## 0.1.35 (4019) — 2026-08-12
<!-- commit: 624ea44af361a3cb5d3d340b8a6aaec99ca1a521 -->

- Companion: one meaning, one mark — the plus is a session and nothing else
- Pin svar 0.7.115: a reasoning level never means "do not think"
- Pin svar 0.7.114: ask each model for the depth its catalog sells
- Companion: spread a fleet's hues around the wheel so no two machines wear one red
- Send Copilot Claude the depth the caller asked for
- Account for fast mode pricing
- Drop the op-name badge from every result card
- Stop a sandbox FD leak from wedging the whole process
- Companion: keep a project's pager on one line at every page
- Add fast mode switch
- Companion: record the 0.1.35 (4008) TestFlight build

## 0.1.35 (4008) — 2026-08-12
<!-- commit: ba43d093d0bce4dc4173f04bb651d77cea9477a6 -->

- Companion: hang a project's pager on its own shelf, and open each group on a trough
- Companion: centre a menu item's content in the thumb target it fills
- Companion: an All view that gives every machine its own hue, rail and section

## 0.1.35 (4003) — 2026-08-12
<!-- commit: 4a1a594e9f988b7d96d89c5a440b610cea2aa9da -->

- Companion: the cog opens the machines, and pairing happens there
- Companion: put a project header's path under its name
- Companion: record the 0.1.35 (4000) TestFlight build

## 0.1.35 (4000) — 2026-08-12
<!-- commit: 9523cf14b093199e1f2f2ead6dca13767978fa13 -->

- Make sandbox functions and tools introspectable
- Companion: drop the machine row's kebab and always show the switch
- Drop TOP TOOLS and TOP ERRORS from the session card
- Drop the machine band; its verbs stand on the row above the list
- Honour the file redirect an extension's subprocess asked for
- Settings rows carry one line, not a paragraph
- Extension env= never widens a confined child environment
- Jail.md replaces sandbox.md, document the declared extension env
- Drain the pipes an extension's guest asked for
- Document only call shapes the runtime accepts
- Add jail.environment: declared (default) or inherit
- Contain extension subprocess output instead of inheriting fd 1/2
- Make grep and struct_nodes take one canonical options map
- Load the workspace .env by default, jail or no jail
- CHANGELOG: run_tests(python) names its faults (issue #136)
- Companion: the one close button is a square, the same size on every band
- One word for confinement: jail
- Companion: one CloseButton — every close glyph in the app is the same 32px column
- Delete jail.env: `environment:` is the one place a variable is named
- Companion: draw one close glyph at one size, so the queued tray's close is the black one every other close is

## 0.1.35 (3971) — 2026-08-12
<!-- commit: eef1f83d0f5af4c3cfa30158ccbde81d9a5326ea -->

- Trim the Companion image viewer to what is on screen
- Shorten inline session delete confirm to Yes, delete / No, keep
- Tone the inline session-delete confirm: err surface, edge and ink
- Companion: confirm a session delete in the row, not in a dialog
- Remove live resources from the model-facing session context
- Stop naming the deleted cat and patch tools in the docstrings
- Drop finished background shells from the model-facing ctx
- Split the shim block into a pushed line and a pulled page
- Drop the provisioning-profile env overrides from the iOS release
- Fetch the App Store profile at release time instead of storing it as a secret
- Pull area doctrine out of the pushed prompt and into skills
- Record the 0.1.35 (3959) companion build in the changelog

## 0.1.35 (3959) — 2026-08-11
<!-- commit: 99ccc2b5af2816ee6f6389a8f042355310a42b90 -->

- Rewrite the model-facing prompt for the one-call surface, and measure it

## 0.1.35 (3958) — 2026-08-11
<!-- commit: 0ebaf1a2829b3f9b0f9ddfb52306f7994195a8dc -->

- Wait for the folder listing before asserting the reuse footer
- Remove cat/patch and retire the lineno:hash anchor
- Paint a result from its own data, not from a per-tool renderer
- Keep a skill live by the iteration that printed its body
- Retire the native-result store and the fold's recovery half
- Search every document with apropos, retrieve one whole with doc
- Ls: decide index-vs-list from the FILE, not the rendered address
- Title a result card from the value's own op, not from a tool table
- Trace the provider stream only when asked
- List a directory outside the workspace instead of indexing it
- Move the HITL band scrollbar into the dialog gutter lane
- Stand the band's rails on the prompt's own rule
- One tool on the wire: delete the native-tool test surface
- Inset the HITL band's body and stand its caps off the fields
- Hand a failed prompt back to the composer so Enter retries it
- Make the human-input band the same box the C-x transient is
- A held turn shows Paused, not a phantom "calling the provider" spinner
- Surface the real provider failure, not the canonical-content validation
- One diagnostic log per process: ~/.vis/logs/vis-<pid>.log
- Advertise only python_execution: the wire carries one tool

## 0.1.34 (3854) — 2026-08-09
<!-- commit: 4f9983d232c5914655f847615b1587018880dff3 -->

- PLAN: name Phase 5 by its commit
- Make every run a handle: a timeout is a wait that expired
- Stop forcing deferred work at namespace load, which native-image runs on the builder
- PLAN: name Phase 4 by its commit
- Give a background shell a log FILE and a byte OFFSET cursor
- Page a project from the list on screen, not the gateway's own window
- Delete the project-wide rename
- Ask the :fs/access gate from struct_rename too
- Record what a host map actually looks like in the sandbox
- Release v0.1.34
- Refuse toolchain output at the incidental capture tap
- Stop capturing the temp file nobody named
- PLAN: record Phase 3 as done
- Replace the `ls` native tool with a sandbox `ls()` helper
- Invent the provider the native suite talks to
- Companion: measure the "Latest" offer instead of remembering it
- Prove the native binary from its own suite, not from a docker build
- Companion: let an artifact tile show its own note, and keep its controls legible
- Companion: one Settings dialog, this device beside the machines
- Record Phase 2 in the plan

## 0.1.33 (3824) — 2026-08-09
<!-- commit: 5c0bcda60cac8388413d4fc55d088c0c5df9d621 -->

- Let an attached page run its own script, never the app's origin
- Rename the sandbox attachment surface to plain verbs
- Make every way out say what it closes
- Give the companion's paint back to the components that own it
- Leave the image viewer through the app's one close
- Give every close mark the page's own ink
- Zoom a picture by the distance scrolled, and take Safari own pinch
- Release v0.1.33
- Put the search field and every transcript card header on one height
- Take the machine strip's side edges from the page
- Page grep results with offset and next_offset
- Let a session row fill its swipe track
- Anchor the companion search field to the bar's trailing edge
- Default grep to 50 elements, filename fallback included
- Frame a sent picture like a produced one and gallery several
- Stretch a row-ending icon button at mouse density too
- Put search back on the app bar with its own magnifying glass
- Give search its own band on a phone and land Clear on the field's edge
- Render MetaButton's children so the composer strip has words again
- Name a button by its rank and give the composer one strip

## 0.1.32 (3799) — 2026-08-07
<!-- commit: 686ea282730f4d3a2868b30e0e03ec0727e28c26 -->

- Start a stroke from beside the picture, not only on its edge
- Release the companion app locally when this machine can sign
- Enforce :ext/protected-paths in the Python sandbox filesystem
- Say the create inside its button and shrink the row question
- Show a document artifact once, as a card that opens over everything
- Paint the star action yellow and keep the starred row in view
- Let the app stop the turn it started again
- Never link a TestFlight build to an internal beta group
- Give a note ten annotation threads and a comment on the whole document
- Push from workflows through one shared git-push action
- Mark annotations in theme colours and annotate plain text too
- Make the machine switcher square, unread a highlight, and hide it for a solo machine
- Draw a comment ordinal as a plain coloured number
- Accept any spelling of a path in vis_attach
- Clear the NEW badge on the row you just read
- Make the fleet switcher one segmented track
- Put the session star immediately right of the title
- Number and colour markdown comments, and underline the passage each is about
- Clear the machine card below the fleet strip
- Pin the companion's artifact-revision save URL to its route

## 0.1.21 (2871) — 2026-08-01
<!-- commit: 1db0d4f7d66aecc93ba26cae8751d39f925181c5 -->

- Reduce attachment shim tool docs
- Reduce sandbox discovery tool docs
- Reduce sandbox helper tool docs
- Use FFF for directory listings
- Reduce Bridge tool surfaces
- Reduce introspection tool surfaces
- Reduce skill tool surface
- Reduce Git tool surface
- Reduce shell tool surface
- Reduce language facade tool surfaces
- Reduce node and filesystem tool surfaces
- Reduce mutation tool surfaces
- Reduce read tool surfaces
- Reduce struct index tool surface
- Reduce session fold tool surface
- Compact engine native contracts
- Compact research search contract
- Compact MCP contracts
- Compact repl lifecycle contract
- Modernize empty session state

## 0.1.21 (2861) — 2026-08-01
<!-- commit: 52953cd362b54144b18dde9388055588cc2abe7a -->

- Unify releases and harden live companion behavior
- Enforce the locked GraalVM pin across build workflows

## 0.1.20 (2856) — 2026-08-01
<!-- commit: 66b0c31d8faee8c348f30a412c520b826579c506 -->

- Ship viewport speedups and accumulated runtime work
- Release notes for 0.1.19 (2854)

## 0.1.19 (2854) — 2026-08-01
<!-- commit: da7516494469a10de4d4becab81f330722c792a1 -->

- Expandable session stats and drafts grouped under their project

## 0.1.19 (2853) — 2026-08-01
<!-- commit: 91170014c043b6acf3a1821f6a4a91924bc5f030 -->

- Add image viewer and smooth native viewport
- Release notes for 0.1.18 (2851)

## 0.1.18 (2851) — 2026-08-01
<!-- commit: d5f4f08cf0cb184cdc2df14da7642baa8ca4b896 -->

- Faster keyboard show/hide and orientation change: the app shell no longer sits on its own composited layer, so raising the keyboard and rotating re-run layout only instead of re-rasterizing the whole screen every frame (fixes a regression from 0.1.17)

## 0.1.17 (2849) — 2026-08-01
<!-- commit: e1e9c7743a054f472fd4d6e35e7eabe7d4dc0cca -->

- Smoother keyboard show/hide and rotation: the app shell now tracks the visual viewport through CSS custom properties instead of React state, so the screen no longer re-renders on every keyboard/rotation frame
- Pasted blocks in your messages are now full-width with wider spacing

## 0.1.16 (2846) — 2026-08-01
<!-- commit: 68352e07b671b3aad7b3b1e53087c8699423ab03 -->

- Show recently-active sessions in collapsed projects
- Release notes for 0.1.15 (2844)

## 0.1.15 (2844) — 2026-07-31
<!-- commit: a70d925169253f6c0758254f726a880ff0685297 -->

- Isolate shell re-renders from keyboard and rotation frames
- Collapsible projects with per-project paging and richer settings
- Release notes for 0.1.15 (2841)

## 0.1.15 (2841) — 2026-07-31
<!-- commit: d15a8b24b9b1cf6a97b25f0e2a3cc03954df4028 -->

- Match the composer strip type ladder and shrink the rule
- Release notes for 0.1.15 (2839)

## 0.1.15 (2839) — 2026-07-31
<!-- commit: 25fa99d8ee45873af76c2d03b0cdce1a46520331 -->

- Glyph-free composer strip and animated reasoning swap
- Release notes for 0.1.15 (2837)

## 0.1.15 (2837) — 2026-07-31
<!-- commit: 2add31771ef95afb1f471452210627b31cb7f95d -->

- Preserve pinned provider selection
- Revert(companion): restore the composer strip glyphs
- Revert(companion): bring the glyphs back
- Handle whitespace split across styled runs
- Release notes for 0.1.15 (2832)

## 0.1.15 (2832) — 2026-07-31
<!-- commit: 012b26d82f5a9fd6a817d70fa924f01251887f19 -->

- Glyph-free thinking band and model manager
- Release notes for 0.1.15 (2830)

## 0.1.15 (2830) — 2026-07-31
<!-- commit: dbab3492cd03e2c55a1be61d42a37a88eef3c069 -->

- Footer reasoning chip, landscape safe areas
- Stop stamping --- before / +++ after on every diff
- Restore the native builder args that last built green
- Give the native builder a 22g heap on the swapfile-backed runner
- Switch the preselected ParallelGC off before enabling G1
- Overcommit the native builder heap with G1 instead of starving it
- Release notes for 0.1.15 (2823)

## 0.1.15 (2823) — 2026-07-31
<!-- commit: 2626ea8d7835f485bea60d9a79e0babc2e1c0501 -->

- Measure the guest-interrupt CPU delta, not JVM-wide CPU
- Keep the native-image heap inside runner RAM and give it a longer clock
- Give the native-image builder enough heap, and let a dispatch rebuild a tag
- Refresh dependency inventory
- TestFlight notes for 0.1.14 (2817)

## 0.1.14 (2817) — 2026-07-30
<!-- commit: edcac200a917a05e9f724ecc8a3706b122d60a37 -->

- Newline-faithful structural edits and comment docs across 28 languages
- Record the 0.1.14 (2815) release notes

## 0.1.14 (2815) — 2026-07-30
<!-- commit: 060fe81dfe16d410e4ef6efd2b03cd7d8bc70572 -->

- Cover turn attachments with tests and note 0.1.14 in the changelog

## 0.1.14 (2814) — 2026-07-30
<!-- commit: e3b729f13299640d797fef528bf655c7fc1f6f56 -->

- Serve a turn's inline attachments and hide the footer mid-turn
- Name every working directory `cwd` across the tool surface
- Cache the live turn bubble so re-entry paints it instantly
- Release notes for 0.1.14 (2808)

## 0.1.14 (2808) — 2026-07-30
<!-- commit: f567648ee4a861d6110c4475d7b75e95da3482da -->

- Let the companion app change the reasoning mode
- Adopt already-running turns in the companion session screen
- Add PRIVACY.md for the companion app (Play store policy URL)

## 0.1.14 (2805) — 2026-07-30
<!-- commit: 5bb959dd751a3ade42121036f3221ca427647e4a -->

- Guarantee turn terminals and bound Python GC

## 0.1.14 (2804) — 2026-07-30
<!-- commit: 88bbea7eb3eee254690946ef364d5f0a31b67e16 -->

- Allow block-local shadowing of bound tool names in vis Python
- Release notes for 0.1.14 (2802)

## 0.1.14 (2802) — 2026-07-30
<!-- commit: 7e3b8a2c2788faca62f436ad6fda377531a79824 -->

- Unify tool input carriers and refresh companion diff view
- Fix gateway, Python, Git, and TUI regressions (#61, #73, #74, #75)

## 0.1.14 (2800) — 2026-07-30
<!-- commit: c9b84ab7ed0ca15062d65d199aa3d951eb2b6886 -->

- Fix failed turn error cards after watchdog recovery
- Guard orphan retirement against registered gateways
- Retire orphaned loopback gateways before restart
- Route extension subprocess APIs through jailed shell
- Improve dotenv environment handling
- Activate Git tool for nested repositories
- Gate commits through verification hooks
- Prefer JSON in Bridge extension docs

## 0.1.14 (2792) — 2026-07-30
<!-- commit: dfece59420181d2b9112e8a56514468a25a2dbc6 -->

- Enforce GraalVM pin consistency
- Update GraalVM and extension runtime
- Smooth terminal result scrolling
- Improve extension configuration and tools
- Refresh shell bindings after settings changes
- Respect disabled shell toggle in sub-agents

## 0.1.14 (2786) — 2026-07-30
<!-- commit: 66da8bf722c3080cec05a5fa84dcbcbff60e833f -->

- Restore transcript layout stabilization
- Limit NTR browsing to latest turn

## 0.1.14 (2784) — 2026-07-30
<!-- commit: bf89880867cd250b2947ade75598de10288edad6 -->

- Simplify companion reconnect and transcript behavior
- Improve compaction guidance and retry diagnostics
- Install the pinned GraalVM CE automatically when it is missing

## 0.1.14 (2781) — 2026-07-29
<!-- commit: 636ea5af07bb6fcea247870d9771f7d56efee6a7 -->

- Stabilize transcript rotation

## 0.1.14 (2780) — 2026-07-29
<!-- commit: df93fefef6194b1309ca45cfc2e4370fde1fca65 -->

- Say what to do, and advertise only the 5 newest ntr entries
- Blame the gateway, not a Vis schema, for injected tool fields
- The LLM title always runs after the turn, on its own route
- The deferred title upgrade is after-turn-auto-title! (#71)
- Configurable session titling, deferred past the foreground turn (#71)
- Widen the observation-batch concurrency margin for loaded runners
- De-flake the live-progress layout budget on shared runners
- Regenerate audit/README.md for svar 0.7.88
- Describe vis as a coding agent, not a "Recursive Language Model"
- Per-provider `is_stateless` for gateways that reject replayed item ids
- Regenerate the dependency inventory (ruff 0.3.2, svar 0.7.86)
- Collapse recorded non-image attachments into one disclosure row
- Read packaging metadata with Python's own parsers, add `python.source_paths`
