# Vis Companion — release notes

What each TestFlight build changed. Edit before uploading; the release script never rewrites an existing entry.

## 0.1.44 (5564) — 2026-09-08
<!-- commit: a549750cb51be2d7d427a2bf77d8b1bb7d424138 -->

- Add grep max_count and streamline results and themes
- Retry unreachable machines without expanding
- Ignore foreground focus wake signals
- Reduce the activity operation count size
- Prevent removal of extension-managed providers
- Center the assistant fork action
- Move provider refresh into row actions
- Record companion build 5528

## 0.1.44 (5528) — 2026-09-08
<!-- commit: 39cba5db14213b15d20173d43ac0f3e158d68067 -->

- Add confirmed account limit resets
- Remove obsolete subscription and compatibility paths
- Align code copy icon with activity chevron
- Keep code clear behind copy icons
- Use icon-only copy controls
- Record companion build 5520

## 0.1.44 (5520) — 2026-09-08
<!-- commit: bb9bad25ee6d49f5e52f5bc2983c859e23796558 -->

- V0.1.44
- Restore trace insets and nest results under code
- Order execution bands and remove left rails
- Refresh response controls after reconnect
- Correct OAuth flow types
- Checkpoint UI and live-view changes
- Correct send and keyboard activation timing
- Open MCP sign-in tab inside the tap
- Keep refreshing MCP rows until the reconnect lands
- Open MCP sign-in directly from the row
- Align execution rails and collapse activity by default
- Record companion build 5487

## 0.1.43 (5487) — 2026-09-08
<!-- commit: 599dfd9acb5ae2c7ff2524fa45d28bbad3b5e8dd -->

- Allow the MCP rows' own swipe verbs in settings
- Simplify guides and improve mobile tables
- Unify client groups and align execution text
- Match MCP servers panel to provider rows
- Clear completed project removal state
- Record companion build 5468

## 0.1.43 (5468) — 2026-09-07
<!-- commit: 063f8998138a1888c28a31a347ddb1fda02334ce -->

- Join Activity with execution sections
- Keep only macOS on self-hosted runners
- Register OAuth callbacks in existing projects
- Record companion build 5463


## 0.1.43 (5463) — 2026-09-07
<!-- commit: 548183a79fd7a98368a3085c93df1f1705d04c35 -->

- Align Thinking and Code sections
- Use paired transport without VPN prompts
- Allow confirmed VPN gateway connections
- Record companion build 5457

## 0.1.43 (5457) — 2026-09-07
<!-- commit: b716429ab7a6d126d09e94b61ad89ad204bd089a -->

- Install Linux AppImage desktop helpers
- Collect normalized Linux package names
- Receive OAuth callbacks on native clients
- Target Linux and universal macOS
- V0.1.43
- Repair release validation fixtures
- Unify activity, SDK and authentication flows
- Align session controls and reserve hover actions
- Turn CODE red on failure instead of naming the error
- Collapse every step by default and provide a chevron to expand it
- Align verbosity with TUI model capabilities
- Omit absent chooser group headers
- Fold CODE and RESULT like THINKING in both clients
- Record companion build 5421


## 0.1.42 (5421) — 2026-09-07
<!-- commit: b474a223c91064abf40823ad86f04c34e374831f -->

- Expand Application by clicking anywhere in its header
- Estimate linked repository guidance in metrics
- Add rich symbol content and refine execution UI
- Pad queued turn rows
- Shrink queued remove mark
- Compact queued remove control
- Reduce queued row padding
- Compact queued turn tray
- Unify Python clients and interaction contracts
- Drop duplicate limits from session health
- Record companion build 5397

## 0.1.42 (5397) — 2026-09-06
<!-- commit: 48d1b443c46e526638964b35aec1a6a9ca91d211 -->

- Keep outline icons unfilled
- Record companion build 5395

## 0.1.42 (5395) — 2026-09-06
<!-- commit: e4baada06375f657cac8e08b8075f72c60802ba2 -->

- Show persisted session health in app metrics
- Reopen only the transcript displayed when the app terminated
- V0.1.42
- Consolidate Python workers in the runtime
- Preserve touch-opened dialogs
- Group internal namespaces by domain
- Match the diagnostics header height
- Compact the diagnostics panel
- Shrink queued message remove buttons
- Fill starred session marks
- Record companion build 5358

## 0.1.41 (5358) — 2026-09-04
<!-- commit: 6d94cf3156300f57a750c3ac179d4d38fa56159b -->

- Make drawing rail collapsible
- Refine mobile drawing rail
- Move drawing tools into side rail
- Use icons and frame zoom controls
- Move drawing check into pencil slot
- Use JetBrains Mono throughout
- Record companion build 5347

## 0.1.41 (5347) — 2026-09-04
<!-- commit: 285fe174b5114cc11e124f6067196029bc33b84d -->

- Let the sessions list scroll under the home indicator
- Present phone pairing methods as alternatives, not sequential steps
- Pair through one field on a three-step page
- Pin parked sessions within their project and retain the list header
- Enforce snake_case diagnostic codes
- Provide Pake desktop installers and hide Scan QR on desktop
- Remove the row's Fork action; retain forking from a turn
- Support forking from a session row or a specific turn
- Reduce the fork menu to two choices
- Limit the fit sheet height to the space below the notch
- Move the list toggle off the app bar, read the Mac host natively
- Align and collapse the desktop sidebar
- Split the desktop layout into a session sidebar and transcript
- Use circular icon buttons for desktop row actions, outside the row columns
- Use the prose font for session titles
- Align retired feature checks
- Remove obsolete terminal artifact bridge
- Remove aggregate and trim source commentary
- Expand diagnostics by clicking anywhere in its header
- Record companion build 5306

## 0.1.41 (5306) — 2026-09-03
<!-- commit: 22cf694c3c0c39111ac1e42a37b3f3910ab76f8a -->

- Stop blurring the composer as the app backgrounds
- Provide a standalone app with built-in speech
- Set diagnostics facts at the dialog detail size
- Collapse the diagnostics panel under its header
- Present diagnostics as individual values instead of paragraphs
- Add project swipe deletion
- Collapse the Application column with its header chevron
- Remove divider before project plus
- Compact project creation and deletion
- Target copied session title events
- Move pull-to-search prompt into app bar
- Move favorites to row lead
- Restore queued action gutter
- Preserve partial output after cancellation
- Record release entry for build 5275

## 0.1.41 (5275) — 2026-09-03
<!-- commit: b15d795974db4d0d3eb6c6a382c7697249616d59 -->

- Record release entries for builds 5209-5232
- Revert unrelated working-tree changes included in an earlier commit
- Make the python child a worker, one per key
- Keep stop control circular
- Balance project header controls
- Make queued and send controls circular
- Align thinking with user prose
- Strengthen TokyoNight companion contrast
- Add all TokyoNight styles

## 0.1.41 (5232) — 2026-09-02
<!-- commit: c6d3317b25cb8a9eb36b6f2a8439c7ae41e81ab1 -->

- Add a bottom border to the session list
- Reserve project header controls
- Stabilize audio attachment transcription
- Simplify settings add actions
- Recover paused queues after request failures
- Remove gaps between project sections

## 0.1.41 (5222) — 2026-09-02
<!-- commit: 10538feb57ba876e43e4499047b13d9f9d4da842 -->

- Give each project a section and remove the needs-you group
- Retain the local clock when turn.started arrives
- Keep the running placeholder out of the render window
- Keep the halted step's struck-through ring stable
- Collapse long turn traces instead of rendering them in full
- Show step outcome in the ring and the called operation in the row
- Center the timeline through every step marker
- Draw the turn timeline behind step markers
- Use the transcript background behind trace markers
- Align the user bubble with its turn timeline

## 0.1.41 (5209) — 2026-09-01
<!-- commit: 8de68974ed6b7c5e9fb8bc610176929d64d0f854 -->

- Use metadata text sizing for error traces, not chip styling
- Measure muted text contrast against its actual background
- Show receipt elapsed time once and preserve the original program
- Make the queued tray a scrollable labelled region
- Label receipts with call names, not state names
- Show every patch and refusal line whole
- Use the same show-more divider in both clients
- Indicate step state with marker color
- Stop a story fixture doubling a diff sign
- One diff per changed file, paths as ~/
- Group each block's file changes under one row
- Accept exported gzip diagnostics
- Distinguish bundled pages from TUI exports
- Make os.link reach its destination and report it
- Remove redundant failure labels
- Use circular session create spinner
- Keep session create button compact
- Preserve provider failure facts
- Avoid touch rename selection handles
- Align the Activity expand control with file paths

## 0.1.41 (5118) — 2026-08-31
<!-- commit: 0005909e61bdfd8d8fa006eda5f3a6667c235f4a -->

- Keep the retired bridge's name out of the gallery config
- Embed responsive terminal in Companion
- Simplify dialog header actions
- Align dialog header with its body
- Render all terminal output through Lanterna grids
- Edit only the session title
- Rename sessions inline
- Strengthen session list boundaries
- Extract session header
- Unify composer suggestions
- Fold completion text into stdout
- Extract composer feature components
- Extract the shared machine navigation component
- Remove iteration form results
- Strengthen feature component boundaries
- Make prompt reuse context-safe
- Extract queued turn tray
- Extract session project groups
- Extract screen feature boundaries
- Canonicalize block output facts

## 0.1.41 (5066) — 2026-08-29
<!-- commit: 876103f5b201162aa8cfe0922e7b3fb4f9a46e4c -->

- Associate Activity with the form that produced it
- Show both protocol versions at the top of the mismatch screen
- Remove the machine navigation sidebar and round icon-only controls

## 0.1.41 (5061) — 2026-08-29
<!-- commit: a45da30c17e23de30ca9f157c85c627d26caa8de -->

- Update project panel backgrounds
- Hide Latest when the reader taps the composer
- Use the filled close button in Blockether Dark
- Match dock container corner radii to the composer
- Fill the Blockether Light close circle
- Reuse downloaded artifact bytes
- Make touch controls act on release
- Drop the light/dark column from the theme list
- Remove the navigation container when only one machine is present
- Round dialog headers
- Restore spacing around the project header

## 0.1.41 (5049) — 2026-08-29
<!-- commit: 1abdb07bf86bd9744ad9f4b15520bfb77813b554 -->

- Correlate requests across app wake
- Round the machine switcher corners
- Give each desktop project a separate panel
- Show image actions as icons
- Add exportable app diagnostics
- Use the removed desktop sidebar's width for the session list
- Align the desktop session list to one grid
- Open sessions at latest turn
- Count every LLM call in session usage
- Remove empty response placeholder
- Remove mismatch recovery buttons
- Record build 5033

## 0.1.41 (5033) — 2026-08-29
<!-- commit: d667f47b63696b1c0f268289a2a73ac2ff9ebb38 -->

- Drop the private voice-settings deep link
- Rename focus state to selection
- Unify operator actions
- Hide false unreachable panel on refusal

## 0.1.41 (5029) — 2026-08-29
<!-- commit: 8ea250645d5ea5d75eb14df14651ccd4d895d7d3 -->

- Unify input and live view lifecycle
- Preload unread session views
- Add machine navigation beside the desktop session list
- Remove UIKit's form accessory bar from the composer
- Reduce the session heading size on pointer devices
- Distinguish sent turns from stalled turns
- Reject abandoned fetches explicitly
- Update tests for justified prose rendering
- Adjust leading elements in app bars
- Retire streams before backgrounding

## 0.1.41 (5009) — 2026-08-28
<!-- commit: 2f2f49f9b7fcc43da24f3e20e7af7370a3eaac9d -->

- Round and rescale the Latest pill
- Ignore hardware keyboard shortcut frames
- Display the session header in one row
- Expose all premium and enhanced iOS voices
- Restore justified transcript prose
- Decode downloaded video frames
- Keep the PDF header stationary during page changes

## 0.1.41 (5002) — 2026-08-28
<!-- commit: 6fe72373c32189db83b45427cf8ac19321745295 -->

- Use a stop icon instead of a colored box
- Round interactive controls
- Preserve unread state across restarts
- Move PDF page and drawing controls into the file header
- Keep completion taps from moving transcript
- Show skills in the empty slash palette
- Draw every control mark with Lucide, not a character
- Arrange live view content vertically
- Show run activity instead of job count
- Send the message with an icon, not a character
- Draw every mark with Lucide
- Cancel abandoned transcript loads
- Honor primary machine in session list
- Open iOS voice download settings
- Restyle parked share notice
- Run an attached page's script without asking
- Reserve no space for an off-screen keyboard
- Apply the page's top inset once

## 0.1.41 (4973) — 2026-08-27
<!-- commit: f462587b680da4bf4dc85fc06471e98b07394b46 -->

- Do not predict an on-screen keyboard when a hardware keyboard is connected
- Support Enter to send across keyboard types
- Position row actions outside the row's data columns
- Disable horizontal scrolling for pointer-driven row actions
- Fill the desktop viewport
- Separate Activity from produced artifacts
- Isolate Activity from turn rendering
- Clear stale Latest after resume
- Remove duplicate system voice
- Prefer installed premium Apple voices
- Retry transient Play failures
- Attach shared HTML files
- Keep Live View pinned through large updates
- Withhold an attached page's script until asked
- Fill incomplete system voice choices
- Remove retired browser bridge
- Recover streams after network handoff
- Upload companion attachments as binary
- Keep activity and media responsive
- Align cache metrics with the stat columns

## 0.1.41 (4962) — 2026-08-27
<!-- commit: 667851f54f5f42d184f28f1850c6e480f5b15429 -->

- Retry transient Play failures
- Attach shared HTML files
- Keep Live View pinned through large updates

## 0.1.41 (4959) — 2026-08-27
<!-- commit: 66a3845446410adf3a208ae1d3e682fa9257ad99 -->

- Withhold an attached page's script until asked
- Fill incomplete system voice choices
- Remove retired browser bridge
- Recover streams after network handoff
- Upload companion attachments as binary
- Keep activity and media responsive
- Align cache metrics with the stat columns
- Retry artifact downloads before persistence completes
- Separate prompt cache metrics
- Restore Latest after native momentum
- Distinguish cache share from reuse coverage
- Report session loading progress
- Make artifact viewers fill the viewport
- Share artifacts through system sheet
- Keep terminal handover silent
- Smooth submitted prompt reveal
- Present Python cancellations as interruptions
- Simplify the API-key sign-in card and increase password-dot spacing

## 0.1.41 (4938) — 2026-08-27
<!-- commit: 84eb4977304a8754281c4a12c94efe26c58c1523 -->

- Keep automatic address reachable
- Refine voice preview cells
- Unify Python execution receipts
- Align activity receipts with TUI
- Record TestFlight build 4906
- Drop DOM focus when the app leaves the foreground

## 0.1.41 (4906) — 2026-08-26
<!-- commit: 23004b18d4e24221350a5e41628ec86229bd85ae -->

- Clarify sharing and skip empty image saves
- Record TestFlight builds 4834-4861
- Unify execution traces
- Transcribe a staged recording before the turn
- Keep voice mic over artifact reader
- Preserve voice playback across transcript handoff
- Add recommended native iOS voices
- Remove the touch timestamp from session data
- Replace per-session SSE routes with the shared session feed
- Update the list from the shared session stream instead of fetching it again
- Keep the newest turn visible for readers already at the end
- Read twenty session rows per window
- Align CI with active machine scope
- Separate voice controls and groups

## 0.1.41 (4861) — 2026-08-25
<!-- commit: 184d0988c67e4e5da9dd0bce354aad54dc4a13f5 -->

- Close mobile machine navigation panels
- Make voice previews stoppable
- Avoid overlapping voice previews during switches
- Hide disclosure for empty projects

## 0.1.41 (4855) — 2026-08-25
<!-- commit: 557cafb08d3d0665753595edd229845f9fea1f90 -->

- Restore keyboard after backgrounding
- Make live activity compact and current

## 0.1.41 (4852) — 2026-08-25
<!-- commit: 976eb53adeab60c69838d4c37c44ff6aa609224f -->

- Stabilize compatibility listener
- Keep home project path visible
- Respect small upward scroll gestures
- Reveal empty projects in session list
- Put voice sample icons before names
- Prevent large live updates from changing the scroll position
- Confirm project deletion in its row
- Show a refused protocol instead of failing calls
- Compress responses and reduce live-close payloads
- Remove requests for the deleted diff metric
- Use panel text colors for nesting indicators

## 0.1.41 (4837) — 2026-08-25
<!-- commit: 44f44b81d529ca39cc632c039a5f839e5a12c89c -->

- Keep page labels with their rows
- Stabilize cancelled turn handover

## 0.1.41 (4834) — 2026-08-24
<!-- commit: 3c9e55fbb1db6e924399542f3c006e08704e5d71 -->

- Rebuild current companion release

## 0.1.41 (4833) — 2026-08-24
<!-- commit: 045d45b3d35d211d81c54eec36b86cdc2dc5a375 -->

- Separate engine settings and favorite status

## 0.1.41 (4829) — 2026-08-24
<!-- commit: c88c11bc5cb0dca9aeff859556626caad9ddcd5d -->

- Restore hierarchy among settings choices
- Align nested settings to one left edge
- Preview a voice before selecting it
- Prefetch active session transcripts

## 0.1.41 (4819) — 2026-08-24
<!-- commit: 604e93e80253c05239011648e87a027624cbd56d -->

- Skip the loading overlay for cached sessions
- Remove remaining human draft remnants
- Cache machine capabilities
- Remove human draft controls
- Keep CSV artifacts as tables
- Open artifact links from answers
- Make CSV artifacts touch-friendly
- Keep machine settings collapsed
- Nest voices under their speech engine
- Summarize actionable project states
- Announce transcript synchronization
- Preview tabular artifacts
- Make use project add the folder
- Open CSV artifacts as tables
- Remove duplicate projects button
- Always select the first machine
- Keep Activity with its persisted turn
- Position Activity after Python results
- Reduce visual detail in Activity receipts
- Keep one machine active

## 0.1.41 (4807) — 2026-08-24
<!-- commit: eafc9355ac21a68f8480b3a89d6270086124aa0a -->

- Keep machine settings collapsed
- Nest voices under their speech engine
- Summarize actionable project states
- Announce transcript synchronization
- Preview tabular artifacts
- Make use project add the folder
- Open CSV artifacts as tables
- Remove duplicate projects button
- Always select the first machine
- Keep Activity with its persisted turn
- Position Activity after Python results
- Reduce visual detail in Activity receipts
- Keep one machine active
- Keep live activities in stable positions
- Stabilize machine colors across reorderings
- Align settled reasoning across clients
- Remove synthetic all machine group
- Keep transcript prose naturally spaced
- Lower the priority of unreachable machines
- Avoid duplicate persisted Activity

## 0.1.41 (4804) — 2026-08-24
<!-- commit: 741fdde771ff3ff90f9fe61d2bd3bef3d90c509c -->

- Summarize actionable project states
- Announce transcript synchronization
- Preview tabular artifacts
- Make use project add the folder
- Open CSV artifacts as tables
- Remove duplicate projects button
- Always select the first machine
- Keep Activity with its persisted turn
- Position Activity after Python results
- Reduce visual detail in Activity receipts
- Keep one machine active
- Keep live activities in stable positions
- Stabilize machine colors across reorderings
- Align settled reasoning across clients
- Remove synthetic all machine group
- Keep transcript prose naturally spaced
- Lower the priority of unreachable machines
- Avoid duplicate persisted Activity
- Repair CSV table interactions
- Position live views by iteration

## 0.1.41 (4783) — 2026-08-24
<!-- commit: f576692a03dbc67e2f1d89d3e7aba0a20ae27f2c -->

- Stabilize machine colors across reorderings
- Align settled reasoning across clients
- Remove synthetic all machine group
- Keep transcript prose naturally spaced
- Lower the priority of unreachable machines
- Avoid duplicate persisted Activity
- Repair CSV table interactions
- Position live views by iteration
- Move project pagination into its header
- Delete the skill corpus, keep the contracts in AGENTS.md
- Move project panel actions into its header
- Use Activity status icons
- Preserve terminal Activity handoff
- Render structured Activity diffs
- Build a chronological Activity list
- Build compact Activity receipt
- Associate Activity with its form
- Include semantic Activity projections
- Finish Activity presentation
- Show bounded tool activity receipts

## 0.1.41 (4777) — 2026-08-24
<!-- commit: d82d3d0c8cf34c0077b1305dbeb7ba36dd05e31c -->

- Remove synthetic all machine group
- Keep transcript prose naturally spaced
- Lower the priority of unreachable machines
- Avoid duplicate persisted Activity
- Repair CSV table interactions
- Position live views by iteration
- Move project pagination into its header
- Delete the skill corpus, keep the contracts in AGENTS.md
- Move project panel actions into its header
- Use Activity status icons
- Preserve terminal Activity handoff
- Render structured Activity diffs
- Build a chronological Activity list
- Build compact Activity receipt
- Associate Activity with its form
- Include semantic Activity projections
- Finish Activity presentation
- Show bounded tool activity receipts
- Remove the machine address from project headers
- Stop retrying rejected voice audio

## 0.1.41 (4729) — 2026-08-23
<!-- commit: 97e0ab26f8f54af36d10fbb2e1d39338046e03f5 -->

- Manage every speech engine model
- Restore project and document additions
- Separate device voices from TTS engines
- Stop loading every machine's sessions into the list
- Paginate projects on the gateway
- Send locally available transcript text to the gateway
- Prevent rewritten log views from repeatedly recording themselves
- Size project pages for the device
- Cap premium device voices at three
- Label gateway engines and curate voices
- Unify speech engine selection
- Distinguish provider authentication states
- Avoid visual disruption during provider refreshes
- Move session usage out of composer
- Keep delete confirmation at row height
- Preserve transcript width when rendering is skipped
- Place the transcript caption outside its frame
- Include waveform plots in the single-icon-module rule
- Give the transcript a waveform header
- Restore provider disclosures

## 0.1.41 (4727) — 2026-08-22
<!-- commit: a4ea80786403562cc81e021c9521e5ed786ef716 -->

- Separate device voices from TTS engines
- Stop loading every machine's sessions into the list
- Paginate projects on the gateway
- Send locally available transcript text to the gateway
- Prevent rewritten log views from repeatedly recording themselves
- Size project pages for the device
- Cap premium device voices at three
- Label gateway engines and curate voices
- Unify speech engine selection
- Distinguish provider authentication states
- Avoid visual disruption during provider refreshes
- Move session usage out of composer
- Keep delete confirmation at row height
- Preserve transcript width when rendering is skipped
- Place the transcript caption outside its frame
- Include waveform plots in the single-icon-module rule
- Give the transcript a waveform header
- Restore provider disclosures
- Inline provider limits
- Display the spoken reply in one section

## 0.1.41 (4724) — 2026-08-22
<!-- commit: 276aa958000fa66eb650c25f686862319c2b0211 -->

- Paginate projects on the gateway
- Send locally available transcript text to the gateway
- Prevent rewritten log views from repeatedly recording themselves
- Size project pages for the device
- Cap premium device voices at three
- Label gateway engines and curate voices
- Unify speech engine selection
- Distinguish provider authentication states
- Avoid visual disruption during provider refreshes
- Move session usage out of composer
- Keep delete confirmation at row height
- Preserve transcript width when rendering is skipped
- Place the transcript caption outside its frame
- Include waveform plots in the single-icon-module rule
- Give the transcript a waveform header
- Restore provider disclosures
- Inline provider limits
- Display the spoken reply in one section
- Disclose provider limits
- Add seek controls for spoken replies

## 0.1.41 (4655) — 2026-08-21
<!-- commit: f5747d0a913eb2327fe5f15d769100db7a8a01c2 -->

- Align menu header assertions
- Clarify project management
- Reveal interrupted live artifacts
- Shrink usage metric values
- Reduce compact label size
- Keep iOS machine polling current
- Nest matrix jobs
- Send committed iOS autocorrection
- Fit and retain PDF annotations
- Validate PDF media type exactly
- V0.1.41
- Tighten composer footer spacing
- Clarify response footer controls
- Keep keyboard open after send
- Retain finished job details
- Disable justification in fenced code
- Disable justification in Markdown code spans
- Resume Android beta publishing
- Rename quick reasoning effort to low
- Restore the verbosity composer control

## 0.1.40 (4618) — 2026-08-20
<!-- commit: 02602989fb71b323fdec8a263673da962d3c9d77 -->

- Record parinferish 0.1.1 in audit
- Sync selected GitHub job
- Record TestFlight build 4615

## 0.1.40 (4615) — 2026-08-20
<!-- commit: e2cd4f701d5aca9ef808feb2929eb5847fced1be -->

- Use parinferish closing-delimiter relocation
- Use Windows trust stores through WSL
- Discover system certificate stores
- Keep finished live runs in stable positions
- Add clj-parinferish workspace root
- Isolate live views from test runs
- Keep short responses beside composer
- Show GitHub run start time
- Load session totals with list
- Harden GitHub watches and name NDJSON
- Keep composer pinned during keyboard dismissal
- Collapse repeated live-run snapshots
- Stop watching superseded CI runs
- Preserve interrupted live records
- Show live CI job activity
- Use GitHub-hosted macOS runners
- Refresh dependency audit
- Bump svar to 0.7.125
- Minimize active live views
- Expand and focus live CI jobs

## 0.1.40 (4546) — 2026-08-19
<!-- commit: ac9e2bde25e37e0a1bbb61e2c8ffb9f44bc86201 -->

- Render recording transcripts as quotations, not code
- Render memo transcripts as quotations instead of decorated blocks
- Dispose of environments replaced by cache inserts
- Normalize near-matching tool keywords instead of rejecting calls
- Wrap recording transcripts at word boundaries
- Open a recording's transcription below its player
- Collapse recording transcripts under their associated turn
- Include attached recording transcriptions in the model's manifest
- Allow issued credentials to specify their API format
- Split long documentation paragraphs into lists, tables and steps
- Include live-view row types in the SDK wheel
- Use one validated API-format vocabulary with normalized inputs
- Identify the live view shown in the running row
- Standardize tool documentation pages and validate them against handlers
- Update GitHub watches continuously and retain one final view
- Keep project-scoped blocks out of the machine store
- Use the extension to identify Android octet-stream selections
- Isolate dropped recording paths in the same way as image paths
- Detect all supported recorder formats, not only m4a
- Validate documentation against one page structure

## 0.1.40 (4508) — 2026-08-19
<!-- commit: 057697b52fb6875584d6a565351226d15335c360 -->

- Fetch notification state once per machine
- Accept pathlib.Path wherever shims accept paths
- Fetch machine notification state before opening its settings row
- Dispose three more single-test sandboxes
- Watch GitHub runs until completion
- Format every namespace with the canonical formatter
- Handle binding forms inside #() without formatter failures
- Retire inactive client leases
- Identify a source build by its commit alone
- Exclude watched-run time from execution deadlines
- Show the gateway's current build and available replacement
- Attribute live-view wait time to the watching block
- Report failed REPL starts consistently across languages
- Refuse a status this build cannot read
- Identify a dev build by its commit, not by "dev"
- Replace an idle daemon when a new build is available
- One REPL lifecycle contract for every language
- Stop managed daemons when no clients remain
- Read pack test counts by their contract names
- Refuse a bare string where repl options belong

## 0.1.40 (4488) — 2026-08-18
<!-- commit: 3bf0134120b93be902f54e59d2b0331d1106a619 -->

- Interpret repl_stop's first argument as its ID regardless of later arguments
- Point whole-store writers at update-machine-config!
- Dispose the sandboxes this suite builds for itself
- Apply the stored palette on the first frame
- Close the Engine with the sandbox it belongs to
- Cover removed_providers in the exhaustive config fixture
- Make deleting a provider actually delete it
- Restrict each test sandbox to that test's lifetime
- Say why a provider cannot be deleted instead of silently refusing
- Document the four REPL lifecycle operations
- Describe ls's tree string where the sandbox reads it
- Count folds from turn context records
- Let /projects/overview and /projects/:pid coexist
- Render project counts from the gateway overview

## 0.1.40 (4474) — 2026-08-18
<!-- commit: e192628bdc20a226e146900263e14e0629646e0f -->

- Render ls as one compact tree string
- Inset panel actions rather than extending them to the panel edges
- Format _outside.py the way ruff 0.16.3 does
- Match the notification action to the panel action container
- Show newly created sessions without requiring a filter tap
- Correct the remote-target claims to what the code does

## 0.1.40 (4468) — 2026-08-18
<!-- commit: f6c4265518a97932a824d004263011eafbf794e6 -->

- Fork a session or selected turn from its row actions
- Store settings without merging configuration levels
- Verify the first live-view operation after machine startup
- Watch a GitHub Actions run in one live view

## 0.1.40 (4464) — 2026-08-18
<!-- commit: c2e5d4122f2f264579e471f2429ace609687a4d7 -->

- Open the project inventory rather than the browser from the projects icon
- Preserve printed output in failed-block responses
- Name the remote gateway flags in help, not only in the docs
- Avoid boxing the log-page window and remove unused test bindings
- Repair the PIL envelope arity, and stop repeating the scope
- Declare guest exports in shims and manage their lifetime automatically
- Restrict guest host objects to the session that opened them
- Document duplicate registration in the shared sandbox
- Remove child-environment code used only by sub_loop
- One GraalPy Engine per session, not one per process
- Remove agent() and all sub_loop callers
- Record the TestFlight build testers now have

## 0.1.40 (4452) — 2026-08-18
<!-- commit: f03e0ddb8f5d41288d0730dc9d4c17e83e55c843 -->

- Make sink-only functions private
- Serve completed view records and persist them on stop
- Save finished live views as artifacts
- Stabilize the live materializer used by the TUI channel
- Serve on Jetty 12 core, drop the ee9 servlet layer
- Name the commands a remote target does not redirect
- Control a remote gateway from the command line
- Test that gateway shutdown publishes pending patches
- Remove unused live-view data and flush patches on stop
- Arrange live views with the same groups as forms
- Show the pull-to-search activation state
- Load the session list behind an open transcript
- Keep turns before and after the visible turn laid out
- Show the entire aside section and test is_aside outside Vis
- Fill the section, render inline Markdown and support aside nodes
- Allow users to stop any live view with a note
- Watch and stop a live view from the phone
- Open live views from a Python extension
- Render each live view in one scrollable section
- Return live-view contents to the model as structured data, not Markdown

## 0.1.40 (4423) — 2026-08-17
<!-- commit: 52ce0b0877ce8ac03d0d0ce577c51dc1fef65704 -->

- Keep project-list height stable across pages
- Refuse a missing path as missing, and select .cljc tests
- Select clojure tests by namespace, not just paths
- Activate the composer on touch rather than waiting for click
- Keep a safety refusal inside the provider that refused
- Reposition images when live updates shift the transcript
- Let users disable fallback and report the serving provider
- Bound post-block defs snapshots while holding the GIL
- Make vis.state a whole mapping
- Switch the session's provider after persistent authentication failure
- Keep slash commands out of the input history ring
- Read current documentation in the sandbox
- Resolve tools passed to to_thread within their gather slots
- Drop the structural editing scenarios
- Drop the last structural editing references
- Preserve the default-root marker across machine updates
- Drop the structural editing tools
- Standardize deps.edn formatting
- Cover asyncio queues, locks, futures and timeouts

## 0.1.38 (4319) — 2026-08-16
<!-- commit: 8088f819e680b5526f62f9616b44f629a28029c9 -->

- Brand the launch screen and name the Android channel
- Reopen a session where the reader stopped reading
- Read a paste mask's alpha band, not its blue channel
- Generate Android launcher and notification icons from the iOS source
- Fetch one session-list page when the list is unchanged
- Tag Android alerts so the badge can clear
- Allow attachment references in the block that created them
- Set the app badge to the pending notification count
- Put an opened note's Save button in its header
- Indicate operation state with a status dot
- Describe the consequences in destructive-action confirmation
- Replace the speech toggle with voice conversation controls
- Keep a project folded once you have folded it
- Remove voice-section borders and the Off route
- Plan making every capability an extension of one contract
- Make the Python host an object, not a dict
- Remove the MCP header and move its action to the last list row
- Give the host contract its own package
- Color the Providers action amber and unpin the blocked banner

## 0.1.38 (4280) — 2026-08-16
<!-- commit: 936e3be55930ab3eec07f34b255f2b9d6397a0b5 -->

- Disclose each machine's settings under its own row
- Keep a reader's place when the keyboard comes and goes
- Update the table-card test for the relocated display
- Drain exited-child output before reporting completion
- Reclaim the SQLite freelist once a fortnight
- Render only visible session turns
- Drop a retired column SQLite was refusing to drop
- Raise the wait budget and cap to thirty minutes
- Stop persisting values that readers can derive
- Distribute the extension API as the vis-agent package
- Remove the fully completed TODO list
- Complete item 1 and record the unscheduled remainder
- Make the redaction test independent of the caller's environment
- Record the TODO state after the doc and ranker work
- Reformat every Clojure and Python source in place
- Remove timing sensitivity from two tests on loaded runners
- Provide missing espeak and pty-log test preconditions
- Document test-result keys
- Fix the test suite on both runners

## 0.1.38 (4179) — 2026-08-14
<!-- commit: 1c55f10b2d9fca5397741b8ac0c8ac1097f97b97 -->

- Repair an edit's delimiters from the file, never from the fragment
- Select a machine address within its settings row
- Update marker tables after removing the duration marker
- Show tool-call duration in the TUI, as in Companion
- Handle failures in unattended warm-up loads
- Keep the direct push in the magit push transient (issue #144)
- Companion: move model-picker actions into its header and size the sheet to its content
- Fix #145: handle answers promoted from rows without content in Companion
- Fix #145: render completed answers once in TUI and Companion
- Every editor write is atomic, and a refused struct_patch batch is rolled back
- Resolve tool calls before helper return so callers receive values rather than thunks
- The gateway keeps "no AI provider" typed, so the TUI opens the dialog
- Release v0.1.38
- Close row action drawers immediately instead of animating them
- Pin svar 0.7.117 to support GLM-5.3 routing and GLM reasoning levels
- Restore a session's helpers through the rewrite that defined them
- List addresses with actions in each row's swipe drawer
- Unregister this device when forgetting a machine
- Record Companion TestFlight build 4159 in the app changelog

## 0.1.37 (4159) — 2026-08-14
<!-- commit: 09ad53d9c488577f2f1e512550b11108c4a63cdb -->

- Restore right-side swipe actions: Star, Rename and Delete for sessions; Make primary, Rename and Forget for machines. Remove the adjacent menu icon.
- Opening a row's swipe actions closes the previous row; its closing event no longer closes the new row.
- Show a starred session's star beside its title
- Make notifications machine-specific and ensure Disconnect completes

## 0.1.37 (4154) — 2026-08-14
<!-- commit: a80a5a898559b37fdd0925aa4b202dfb0ef9bdc5 -->

- Show Star, Rename and Delete on session rows and Make primary, Rename and Forget on machine rows without swipes or hidden menus
- Use one line for the Machines header and label pairing Add a machine instead of using a plus icon
- Replace the push-token list with one notification connection switch
- Recalculate composer height when its width changes, not only on typing
- Complete only the matching turn, preserve its rendered answer and refetch a short transcript
- A staged photo is no longer rewritten into storage on every keystroke
- A copied session id is marked vis_session_id#<uuid>
- Return newest search results first, matching the session list
- Import the font's italic axis to render italics correctly
- Bundle every theme as static CSS, removing the /v1/theme gateway request

## 0.1.35 (4124) — 2026-08-14
<!-- commit: 52f098458a174d9adffa3ed35f99f2aac1224061 -->

- Avoid signaling a reused PID after its original child has been reaped
- Search for the final typed query and display that query separately
- One microphone in the Companion composer: tap acts, hold switches the mode
- Confine the extension child-pid handoff to the spawning thread
- Keep only the sentences the summarizer finished
- Give an extension's Popen the child's real OS pid
- Set the Vis JVM maximum heap to 5 GiB
- Add half-duplex voice conversations
- Preserve streamed answers during transcript handover
- Fix Android Bluetooth voice capture
- Reject helper names that conflict with tools, including during restoration
- Restore all helpers rather than only those that parse successfully
- Remove isolated rendering from user turns
- Persist helper definitions across process restarts and list them with defs()
- Skip search requests to known-unreachable machines
- Anchor ruff's per-file-ignores at the canonical path
- Report unreachable gateways separately from searches with no matches
- Keep helper definitions readable so they can be edited without repetition
- Reduce search duration to one quarter and show progress during execution
- Add program structure guidance to the core prompt

## 0.1.35 (4090) — 2026-08-13
<!-- commit: 4b67f7bafd8181c495f9397bf5eabfe01242d310 -->

- Companion: position the notch strip above the dialog header
- Plan communication between sessions in the same tree
- Select tests by path and use each language pack's test counts
- Give disabled toggles a visible border
- Name session APIs by operation rather than storage table
- Restore cat-generated addresses for patch edits
- Show pairing progress and enforce a timeout
- Delete sessions without reloading all machines
- Honor accepted TLS options or explicitly reject unsupported values
- Count test exceptions and parse CLI test counts
- Preserve TestFlight notes when Apple requests fail temporarily
- Collapse run_tests faults into ONE typed failures list
- Show every repository in the magit buffer, not the first twelve
- Plan restoration of cat and patch with anchored grep output
- Companion: record the 0.1.35 (4075) TestFlight build

## 0.1.35 (4075) — 2026-08-13
<!-- commit: 303d39e0132803b54e94db9293b6828bfbde6901 -->

- Retire the automatic outbox capture; `attach` is how an artifact is kept
- Preserve the live execution trace in the finished answer
- Fix #141: enforce verified HTTPS and remove the redundant bypass option
- Hand-written config keeps the provider and model pair; the committed file still cannot
- Make the provider/model pair a remembered selection, not configuration
- Fix #140: a committed vis.yml forced one developer's provider on every clone
- Companion: restore the bold uppercase RESULT header
- Companion: limit machine retries to five seconds and show failures in red without repeated attempts
- Use bold uppercase labels for collapsible sections
- Fix Android gateway and optional push support
- Update Bridge extension for 0.3
- Migrate verification governance to Bridge 0.3
- Keep focused-field accent rings within their borders
- Fix #139: concurrent shells shared one auto-derived handle id
- Sandbox: test cancellation during host waits of any duration
- Sandbox: cancel a parked block through the guest safepoint
- Companion: show running state throughout turn execution
- Release: macOS arm64 builds on GitHub's hosted runner, never a laptop
- Native: determine Lanterna TTY control at binary runtime, not build time
- Native: include extension native-image requirements as well as discovery metadata

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

- Native: include every built-in extension namespace and reject incomplete binaries
- Wait for the pty child and test the jail on Linux runners
- Fix remaining tests for relay naming, card reloads and pasta logging
- Native: remove Linux FFM TTY downcalls to restore binary releases
- Fix tests for relay naming, hook reloads and stack-safe scanning
- Name svar's unroutable failure instead of the generic provider card
- Cancellation: preserve interrupts through best-effort exception handling
- Prompt: run_tests reloads what it RUNS, never its dependencies
- Shell: preserve cancellation during usage sampling
- Companion: keep starred rows on the current page to preserve their position
- Companion: show star status on session rows and in the header
- Companion: drop the artifacts gallery's dead tool_name read
- Pytest shim: capfd captures the real descriptor (#138)
- Companion: show current probe results in machine status indicators
- Shell: one stream, no dead stderr field (#137)
- Companion: support drawing on images without selecting them
- Companion: give every section a dedicated close-control cell
- Show every nested repository in the magit status buffer
- Show the fold threshold in the core prompt's budget line
- Name session_fold in the rule that orders the fold

## 0.1.35 (4019) — 2026-08-12
<!-- commit: 624ea44af361a3cb5d3d340b8a6aaec99ca1a521 -->

- Companion: reserve the plus icon for session creation
- Pin svar 0.7.115 to distinguish reasoning levels from disabling reasoning
- Pin svar 0.7.114 to use model-supported reasoning levels
- Companion: distribute machine colors across the hue range to distinguish them
- Send the requested reasoning level to Copilot Claude
- Account for fast mode pricing
- Drop the op-name badge from every result card
- Fix a sandbox file-descriptor leak that blocked the process
- Companion: keep a project's pager on one line at every page
- Add fast mode switch
- Companion: record the 0.1.35 (4008) TestFlight build

## 0.1.35 (4008) — 2026-08-12
<!-- commit: ba43d093d0bce4dc4173f04bb651d77cea9477a6 -->

- Companion: give project pagination a separate row and group headers a distinct background
- Companion: center menu content within its touch target
- Companion: give each machine a distinct color, navigation item and section in the All view

## 0.1.35 (4003) — 2026-08-12
<!-- commit: 4a1a594e9f988b7d96d89c5a440b610cea2aa9da -->

- Companion: open machine management and pairing from Settings
- Companion: put a project header's path under its name
- Companion: record the 0.1.35 (4000) TestFlight build

## 0.1.35 (4000) — 2026-08-12
<!-- commit: 9523cf14b093199e1f2f2ead6dca13767978fa13 -->

- Make sandbox functions and tools introspectable
- Companion: remove the machine row overflow menu and always show its switch
- Drop TOP TOOLS and TOP ERRORS from the session card
- Remove the machine header and place its actions in the row above the list
- Honour the file redirect an extension's subprocess asked for
- Limit settings descriptions to one line
- Extension env= never widens a confined child environment
- Jail.md replaces sandbox.md, document the declared extension env
- Drain the pipes an extension's guest asked for
- Document only call shapes the runtime accepts
- Add jail.environment: declared (default) or inherit
- Contain extension subprocess output instead of inheriting fd 1/2
- Make grep and struct_nodes take one canonical options map
- Load the workspace .env by default, jail or no jail
- CHANGELOG: run_tests(python) names its faults (issue #136)
- Companion: use square close buttons of equal size in every section
- One word for confinement: jail
- Companion: one CloseButton — every close glyph in the app is the same 32px column
- Delete jail.env: `environment:` is the one place a variable is named
- Companion: use the same black close icon and size in the queued tray and other controls

## 0.1.35 (3971) — 2026-08-12
<!-- commit: eef1f83d0f5af4c3cfa30158ccbde81d9a5326ea -->

- Trim the Companion image viewer to what is on screen
- Shorten inline session delete confirm to Yes, delete / No, keep
- Use error background, border and text colors for inline session-delete confirmation
- Companion: confirm a session delete in the row, not in a dialog
- Remove live resources from the model-facing session context
- Stop naming the deleted cat and patch tools in the docstrings
- Drop finished background shells from the model-facing ctx
- Split shim documentation into a prompt summary and an on-demand page
- Drop the provisioning-profile env overrides from the iOS release
- Fetch the App Store profile at release time instead of storing it as a secret
- Move area-specific guidance from the system prompt into skills
- Record the 0.1.35 (3959) companion build in the changelog

## 0.1.35 (3959) — 2026-08-11
<!-- commit: 99ccc2b5af2816ee6f6389a8f042355310a42b90 -->

- Rewrite and measure the model-facing prompt for the single-tool API

## 0.1.35 (3958) — 2026-08-11
<!-- commit: 0ebaf1a2829b3f9b0f9ddfb52306f7994195a8dc -->

- Wait for the folder listing before asserting the reuse footer
- Remove cat/patch and retire the lineno:hash anchor
- Render results from their data rather than per-tool renderers
- Retain a skill's instructions through the iteration that printed them
- Retire the native-result store and the fold's recovery half
- Search every document with apropos, retrieve one whole with doc
- Ls: decide index-vs-list from the FILE, not the rendered address
- Title result cards from the value's operation field, not a tool lookup table
- Trace the provider stream only when asked
- List a directory outside the workspace instead of indexing it
- Move the human-input scrollbar into the dialog margin
- Align section borders with the prompt divider
- One tool on the wire: delete the native-tool test surface
- Inset human-input content and separate header labels from fields
- Restore failed prompts to the composer for retry with Enter
- Match human-input panel geometry to the C-x transient
- Show Paused for held turns instead of an incorrect provider-call spinner
- Surface the real provider failure, not the canonical-content validation
- One diagnostic log per process: ~/.vis/logs/vis-<pid>.log
- Advertise only python_execution: the wire carries one tool

## 0.1.34 (3854) — 2026-08-09
<!-- commit: 4f9983d232c5914655f847615b1587018880dff3 -->

- PLAN: name Phase 5 by its commit
- Make every run a handle: a timeout is a wait that expired
- Stop forcing deferred work at namespace load, which native-image runs on the builder
- PLAN: name Phase 4 by its commit
- Give background shells log files and byte-offset cursors
- Paginate the displayed project list rather than the gateway's result window
- Delete the project-wide rename
- Ask the :fs/access gate from struct_rename too
- Record what a host map actually looks like in the sandbox
- Release v0.1.34
- Exclude toolchain output from incidental attachment capture
- Stop capturing the temp file nobody named
- PLAN: record Phase 3 as done
- Replace the `ls` native tool with a sandbox `ls()` helper
- Use a deterministic provider fixture in native tests
- Companion: calculate whether Latest is needed from the current scroll position
- Verify native binaries with native tests, not only Docker builds
- Companion: show artifact notes on their tiles and keep controls legible
- Companion: one Settings dialog, this device beside the machines
- Record Phase 2 in the plan

## 0.1.33 (3824) — 2026-08-09
<!-- commit: 5c0bcda60cac8388413d4fc55d088c0c5df9d621 -->

- Run attached-page scripts in isolation from the app origin
- Use direct operation names for sandbox attachment APIs
- Label close controls with the element they close
- Move Companion rendering styles into the relevant components
- Use the shared close control in the image viewer
- Use page text colors for close icons
- Scale image zoom by scroll distance and support Safari pinch gestures
- Release v0.1.33
- Put the search field and every transcript card header on one height
- Align machine-strip side borders with the page
- Page grep results with offset and next_offset
- Let a session row fill its swipe track
- Align Companion search with the app bar's trailing edge
- Default grep to 50 elements, filename fallback included
- Use the artifact image frame for sent images and a gallery for multiple images
- Stretch row-end icon buttons at pointer density as well as touch density
- Put search back on the app bar with its own magnifying glass
- Give mobile search a separate row and place Clear at the field edge
- Render MetaButton children so composer labels remain visible
- Name button variants by hierarchy and use one composer control row

## 0.1.32 (3799) — 2026-08-07
<!-- commit: 686ea282730f4d3a2868b30e0e03ec0727e28c26 -->

- Start a stroke from beside the picture, not only on its edge
- Release the companion app locally when this machine can sign
- Enforce :ext/protected-paths in the Python sandbox filesystem
- Put creation labels inside their buttons and shorten row confirmation prompts
- Show document artifacts once as cards that open an overlay viewer
- Color the Star action yellow and keep starred rows visible
- Let the app stop the turn it started again
- Exclude TestFlight builds from internal beta groups
- Give a note ten annotation threads and a comment on the whole document
- Push from workflows through one shared git-push action
- Mark annotations in theme colours and annotate plain text too
- Use a square machine switcher, highlight unread state and hide it for a single machine
- Draw a comment ordinal as a plain coloured number
- Accept any spelling of a path in vis_attach
- Clear the NEW badge on the row you just read
- Use one segmented machine switcher
- Put the session star immediately right of the title
- Number and colour markdown comments, and underline the passage each is about
- Remove the machine card below the machine switcher
- Test the Companion artifact-revision save URL against its route

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

- Release viewport performance improvements and accumulated runtime changes
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

- Match composer font sizes and reduce divider thickness
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
- Remove redundant --- before / +++ after labels from diffs
- Restore the last successful native-builder arguments
- Give the native builder a 22g heap on the swapfile-backed runner
- Switch the preselected ParallelGC off before enabling G1
- Use G1 and an overcommitted heap for the native builder
- Release notes for 0.1.15 (2823)

## 0.1.15 (2823) — 2026-07-31
<!-- commit: 2626ea8d7835f485bea60d9a79e0babc2e1c0501 -->

- Measure the guest-interrupt CPU delta, not JVM-wide CPU
- Limit native-image heap to runner RAM and extend the build timeout
- Increase native-image build heap and support tag rebuilds through workflow dispatch
- Refresh dependency inventory
- TestFlight notes for 0.1.14 (2817)

## 0.1.14 (2817) — 2026-07-30
<!-- commit: edcac200a917a05e9f724ecc8a3706b122d60a37 -->

- Preserve newlines in structural edits and support comment documentation across 28 languages
- Record the 0.1.14 (2815) release notes

## 0.1.14 (2815) — 2026-07-30
<!-- commit: 060fe81dfe16d410e4ef6efd2b03cd7d8bc70572 -->

- Cover turn attachments with tests and note 0.1.14 in the changelog

## 0.1.14 (2814) — 2026-07-30
<!-- commit: e3b729f13299640d797fef528bf655c7fc1f6f56 -->

- Serve a turn's inline attachments and hide the footer mid-turn
- Name every working directory `cwd` across the tool surface
- Cache live turn content for immediate rendering on session re-entry
- Release notes for 0.1.14 (2808)

## 0.1.14 (2808) — 2026-07-30
<!-- commit: f567648ee4a861d6110c4475d7b75e95da3482da -->

- Let the companion app change the reasoning mode
- Adopt already-running turns in the companion session screen
- Add PRIVACY.md for the companion app (Play store policy URL)

## 0.1.14 (2805) — 2026-07-30
<!-- commit: 5bb959dd751a3ade42121036f3221ca427647e4a -->

- Ensure turns emit terminal events and bound Python GC

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

- Clarify instructions and advertise only the five newest NTR entries
- Attribute injected tool fields to the gateway rather than a Vis schema
- Generate LLM titles after turns through a separate route
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
