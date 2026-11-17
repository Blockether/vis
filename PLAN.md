# Hands-free voice implementation

> Keep capture live during speech. Keep task authority in the existing agent.

## Context

The approved scope is local implementation and testing of the full-duplex voice plan.
The source baseline is `a96abba81be1cb50ca4ae87bc76d6453711765db`.
All changes stay in the `vis/hands-free-voice` draft. Do not commit, approve, push or deploy this work.
The qualification profile is English on Apple M1 Max, with 64 GiB memory and one voice session.
Local speech does not make a hosted text agent local.

The gateway queues input while a turn runs. Voice follow-ups use this existing queue.
Barge-in stops output, not agent work. Existing approvals remain authoritative.
Media uses binary WebSocket through the existing Jetty adapter.
Do not use batch WAV uploads as a substitute for incremental inference.
Do not add silent device or cloud fallback.

## 0. Verify the baseline

- Rationale: Source support does not prove runtime quality.
- Data: Dependency preparation passed with Java 25 and Node 25 on the declared hardware.
- Data: All four ASR files match their pinned hashes. The installed files total 2,742,379,518 bytes.
- Data: All seven existing Pocket files match the pinned archive. Its size is 96,353,153 bytes.
- Data: The Pocket voice archive passed its checksum check. Its size is 2,109,943 bytes.
- Acceptance criteria: Verify model assets, licenses, admission behavior and WebSocket transport.
- Data: Pinned ASR weights are MIT. Pocket weights are CC-BY-4.0 with additional gated use terms.
- Data: Bundled clips are CC-BY-4.0 or CC0. Runtime libraries use Apache-2.0 or MIT.
- Unknowns: Historical Pocket gate terms and deployment-specific voice rights are not independently verified.
- State: Asset and license evidence is recorded. This is not legal clearance. Native fixtures pass, but physical audio remains unverified.

## 1. Define the shared contract and coordinator

- Rationale: Ownership and limits must precede device work.
- Data: The canonical voice schema defines frames, events, limits, controls and orthogonal states.
- Data: Scoped, expiring tickets authorize media. The ordinary HTTP routes retain their normal authentication.
- Data: The coordinator rejects stale media and limits input, output, pending work and retained state.
- Data: Confirmed admission receipts persist in session storage. Recovery never restores audio or replays a request.
- Acceptance criteria: Reject stale media, bound resources and admit each final utterance once.
- Unknowns: A crash can leave the last admission uncertain. Missing receipts do not prove that work failed.
- State: Implemented. Coordinator, runtime, real transport and receipt recovery tests pass on the JVM.

## 2. Add live client audio

- Rationale: Local interruption must not wait for the gateway.
- Data: Companion uses continuous AudioWorklet capture and streamed PCM playback.
- Data: The browser must report enabled echo cancellation before capture connects. This check does not establish acoustic quality.
- Data: TUI capture and playback use separate bounded workers. An explicit headset assertion is required.
- Data: Both clients stop local output before a gateway round trip. Muting releases microphone capture.
- Acceptance criteria: Capture during playback, retain mute, reject stale audio and clean up devices.
- Unknowns: Speaker echo, route changes and device behavior require physical tests.
- State: Desktop behavior has deterministic tests. Phase 13 adds native mobile testing behind explicit consent.

## 3. Add incremental engines

- Rationale: Stream transport cannot remove batch inference latency.
- Data: VibeVoice ASR retains incremental state. Pocket emits PCM through its native callback.
- Data: ASR needs an initial 3.467-second window and then advances in 2.933-second steps.
- Data: Native abort requests do not wait on inference. Acknowledgement follows actual worker exit.
- Data: Model preparation runs before microphone readiness. Strict local speech has no fallback.
- Acceptance criteria: Emit useful partials and playable frames before full completion, with bounded resource use.
- Data: Explicit model preparation now installs the missing native runtime. Passive readiness checks do not download it.
- Data: The native media failure exposed missing reflective WebSocket callbacks. The pinned listener now has metadata and a regression test.
- Data: The rebuilt image passes incremental ASR, native TTS callbacks and real media transport.
- Data: Abort qualification uses a 30-second resource-release deadline for each engine. Tests measure return, not only the abort request.
- Unknowns: Pocket cannot preempt its initial latent loop. The test deadline is not a hard operating guarantee.
- State: JVM and native inference pass. The final ASR and TTS deadline checks pass.

## 4. Connect automatic turns

- Rationale: Normal conversation needs no utterance buttons.
- Data: Endpointing finalizes speech and admits it through the ordinary task queue.
- Data: Partial transcripts cannot execute. Follow-ups do not implicitly steer existing work.
- Data: Stopping speech, ending voice mode and cancelling an owned task remain separate actions.
- Data: Ambiguous spoken cancellation does nothing. Cancellation never implies that completed actions were undone.
- Data: Both clients retain uncertainty when recovery lacks an expected receipt. Neither client replays speech automatically.
- Acceptance criteria: Ten automatic turns, safe interruption, no duplicate admission and no partial execution.
- Unknowns: Endpoint thresholds and recognition of user intent require acoustic qualification.
- State: Deterministic admission, ownership, control and ten-turn tests pass.

## 5. Commit early speech segments

- Rationale: Final-only output is an intermediate result.
- Data: Explicit, closed speech projections become immutable segments before task completion.
- Data: Rewrites and retries invalidate old speech. Reasoning, tool output and private forms are not speech sources.
- Data: Source ownership and output epochs prevent old task results from speaking over a newer utterance.
- Acceptance criteria: Speak eligible committed content before task completion without exposing reasoning or tools.
- Unknowns: Actual time to useful audio depends on the selected agent and physical playback.
- State: Parser, prompt, projection and native fixture checks pass. Physical playback and real-agent qualification remain open.

## 6. Verify clients and failure behavior

- Rationale: Unit tests do not prove acoustic quality.
- Data: Affected engine tests pass in a clean JVM: 1,033 cases.
- Data: Initial Companion coverage passed 3,922 cases, with one skip. Later focused recovery and audio checks pass.
- Data: Current TUI checks pass 547 cases. Its separate native build and no-device startup checks pass.
- Data: Engine and TUI builds use pinned GraalVM CE 25.3.4.1. Building an image does not prove runtime inference.
- Data: The first synthetic corpus completed 200 utterances with no pipeline failures or empty transcripts.
- Data: Streaming and batch transcripts match in all 200 cases. Reference text matches in 186 cases.
- Data: The first run did not retain the 14 mismatches. The instrumented repeat matched 184 references and all 200 batch transcripts.
- Data: The repeat had 16 mismatches: eight controls, six uncertainty phrases, one ordinary phrase and one short phrase.
- Data: All eight control mismatches changed or dropped “End voice mode”. None of the 40 negation trials differed.
- Data: These synthetic trials do not prove reliable voice control or general intent safety. No transcript was executed.
- Data: The JVM soak completed 3,600.395 seconds with 1,880 full cycles and 376 interruptions. No cycle failed.
- Data: RSS fell from 6,007,635,968 to 4,344,414,208 bytes. Threads stayed at nine and descriptors stabilized at 156.
- Data: The JVM samples show no cumulative growth.
- Data: The native gateway soak completed its 3,600-second deadline. Its test took 3,616.39 seconds and passed 120 cycles.
- Data: All 120 admissions and durable receipts matched. The client reported 10,822 playback credits and 24 acknowledged interruptions.
- Data: Warm end-to-first-PCM p50/p95 were 1,417.31/1,613.77 ms across 119 cycles. The cold cycle took 2,289.63 ms.
- Data: After first PCM, interrupt request-to-acknowledgement p50/p95 were 59.71/61.25 ms across 24 cycles.
- Data: These wire observations use synthetic input and a local stub. They exclude physical playback, endpoint and hosted-agent costs.
- Data: After warmup, RSS stayed within 6,859,600–6,883,776 KiB. Threads stayed within 125–128, and descriptors within 48–50.
- Data: Final growth from warmup was -22,352 KiB RSS, one thread and zero descriptors. These periodic samples do not prove heap retention.
- Data: All 120 final transcripts matched the direct JVM reference. The 8,080 ms clip fed in 8,080–8,095 ms without skips.
- Data: Smaller Pocket chunks changed PCM samples and gave inconsistent timing gains. Production keeps the existing chunk size.
- Data: Pocket generates all latents for a text chunk before its first audio callback. Cancellation cannot interrupt that initial loop.
- Data: That run used fresh leases at least 30 seconds apart. It did not prove endurance with one continuous voice lease.
- Data: The continuous-lease preflight passed 154.293 seconds, six utterances and one acknowledged interruption.
- Data: The continuous run held one session, lease and socket for 3,622.824 seconds across 141 utterances.
- Data: All 141 admissions, durable receipts and reference transcripts matched. IDs and output epochs stayed consecutive.
- Data: That run reported 11,652 playback credits and 28 acknowledged interruptions. Its test log reports zero failures in 3,641.007 seconds.
- Data: Warm end-to-first-PCM p50/p95 were 1,482.71/1,702.12 ms across 140 utterances. Cold latency was 2,084.45 ms.
- Data: Interrupt acknowledgement p50/p95 were 55.11/60.29 ms. These measurements still exclude physical playback, endpoint and hosted-agent costs.
- Data: After warmup, RSS stayed within 6,849,376–6,872,752 KiB. Threads stayed within 125–127, and descriptors within 49–53.
- Data: Across 60 resource samples, post-warmup growth was -21,696 KiB RSS, zero threads and minus one descriptor.
- Data: The samples show no cumulative growth. They do not measure heap retention or every transient peak.
- Data: All 141 clips used unchanged PCM. The 8,080 ms clip fed in 8,080.094–8,089.934 ms without skips.
- Data: The first six warmed, pre-PCM Pocket trials tested three seeds at each of two input lengths, without playback.
- Data: The 25-character text released native work in 327.53–465.56 ms. At the 600-character engine limit, it took 2,582.44–2,848.77 ms.
- Data: All six trials returned the TTS permit and delivered no PCM. The abort call itself took 0.004–0.005 ms.
- Data: Six repeated trials also passed the deadline assertions. Short inputs released in 417.39–488.33 ms, and long inputs in 3,102.08–3,698.57 ms.
- Data: Every repeated trial delivered zero PCM and returned the permit. Native ASR also acknowledged termination within its test deadline.
- Acceptance criteria: Meet the original corpus, latency and resource targets. Qualify each supported device.
- Unknowns: Physical interruption, double-talk, Bluetooth, output quality and playback latency remain unmeasured. Native TTS cancellation remains cooperative.
- State: Automated fixtures and relevant formatting checks pass. Physical qualification remains open. Do not claim the latency targets or general availability.

## 7. Document opt-in operation

- Rationale: Experimental support must not imply a qualified release.
- Data: The global feature is off by default. Its switch is in Voice settings. Existing dictation, manual replay and text remain available.
- Data: Session guides explain controls, ordinary approvals, privacy, receipt recovery and rollback.
- Data: SDK lifecycle calls use HTTP. The SDK rejects media calls before network access because it has no WebSocket client.
- Acceptance criteria: State privacy limits, supported profiles and all unverified claims.
- Unknowns: Final native and acoustic results determine support claims. License restrictions still apply.
- State: Implemented. Documentation and SDK checks pass. This record states the remaining qualification limits.

## 8. Optional native speech-to-speech comparison

- Rationale: A native model may improve conversation quality.
- Data: Optional local benchmark, outside the critical path.
- Acceptance criteria: Keep the cascade unless the alternative proves reliable delegation and better results.
- Unknowns: Model cost, licensing and tool reliability.
- State: Deferred. This experiment does not block the cascade implementation.

## 9. Improve conversational replies

- Rationale: Real voice requests returned prose without speech. Short TUI noise also interrupted output before an utterance qualified.
- Data: Live instructions now require spoken replies, including greetings, questions, final outcomes and corrections.
- Data: A missing projection produces one fixed notice. Arbitrary prose, code and reasoning remain outside speech.
- Data: The coordinator applies existing ownership, mute, epoch and duplicate checks to the notice.
- Data: TUI interruption now requires 120 milliseconds of continuous detected speech. Companion already applies this threshold.
- Data: Regression tests reproduced missing speech and premature interruption before the fixes.
- Data: A mocked provider boundary receives voice instructions on both enabled turns, but not on adjacent disabled turns.
- Acceptance criteria: Avoid silent completion and noise-only interruption without reading private content or changing task authority.
- Unknowns: Real model compliance, physical conversation quality and complete response latency remain unverified.
- State: Local fixes and focused checks pass. This does not qualify fluid conversation.
- Remaining: Define safe correction delivery while work runs. Follow-ups still use the ordinary queue and do not implicitly cancel tasks.
- Remaining: Improve acoustic detection and native synthesis cancellation before claiming conversational latency targets.

## 10. Recover from speech input overload

- Rationale: A slow ASR operation can fill the shared queue and prevent an utterance from ending.
- Data: Audio remains limited to two seconds. Six reserved command slots preserve input order without increasing the audio budget.
- Data: Overflow records queue depth, buffered duration, active operation and operation time. It stores no audio or transcript.
- Data: Overload mutes capture and rejects unfinished utterances explicitly. Existing admissions and task authority remain unchanged.
- Data: Rejection receipts survive restart. Both clients require explicit unmute and never replay input automatically.
- Acceptance criteria: Bound input, preserve lifecycle commands, reject stale callbacks and recover without duplicate admission.
- Data: Tests reproduce blocked inference and finalization, then verify reserved capacity and explicit recovery.
- Unknowns: The original native stall duration was not recorded. These changes do not accelerate ASR or establish acoustic latency.
- State: Implemented locally. Native deployment and physical qualification remain pending.

## 11. Profile paced speech inference

- Rationale: Separate encoder, token decoding, finalization and input backlog before tuning inference.
- Data: The opt-in `native-paced-profile` test uses seed 42 and one 4.24-second synthetic fixture.
- Data: It schedules 20-millisecond frames, warms ASR once, then measures three trials alone and three with continuous Pocket synthesis.
- Data: Enable JVM property `vis.test.paced-speech-profile=true` and select `com.blockether.vis.internal.speech.live-native-test/native-paced-profile`.
- Data: The benchmark uses installed assets, Java 25.0.3, ten logical processors and default runtime thread counts.
- Data: In the final run, maximum feed duration was 971–982 ms alone and 3,034–3,515 ms with TTS.
- Data: Encoder calls took 453–460 ms alone and 1,455–2,049 ms with TTS. Token decoding also slowed.
- Data: Finalization took 634–642 ms alone and 2,056–2,167 ms with TTS.
- Data: Scheduled audio end to final transcript took 838–840 ms alone and 4,423–4,813 ms with TTS.
- Data: Maximum modeled queue wait was 957–965 ms alone and 3,016–3,498 ms with TTS.
- Data: Each concurrent trial completed five or six TTS requests before ASR finished. All six transcripts matched the warmup.
- Acceptance criteria: Report separate stages and queue timing without devices, paid calls or production inference changes.
- Unknowns: The unbounded arrival model is not the gateway queue. Continuous synthesis is a stress workload, not typical conversation.
- Unknowns: These results do not identify the original overflow cause or distinguish CPU scheduling from memory bandwidth contention.
- State: The benchmark passed twice locally. Encoder inference is the largest measured stage in this fixture.
- Data: The opt-in `native-thread-profile` test compares default, two-thread and four-thread ASR with ten-thread and two-thread TTS.
- Data: Enable JVM property `vis.test.thread-speech-profile=true` and select `com.blockether.vis.internal.speech.live-native-test/native-thread-profile`.
- Data: Each setting reloads models and warms ASR once. The test repeats default settings last to check timing drift.
- Data: All 42 measured transcripts matched. Each concurrent trial completed five to seven TTS requests before ASR finished.
- Data: With default ASR and two-thread TTS, concurrent audio-end-to-transcript time was 913–975 ms. Maximum modeled queue wait was 971–1,085 ms.
- Data: With four-thread ASR and two-thread TTS, these ranges were 1,060–1,090 ms and 1,063–1,089 ms.
- Data: Initial defaults produced 4,216–5,430 ms and 2,794–3,628 ms. Repeated defaults produced 4,550–4,668 ms and 3,091–3,382 ms.
- Data: Limiting ASR alone did not remove concurrent stalls. Two-thread ASR also slowed isolated recognition.
- State: Default ASR with two-thread TTS is the best tested candidate. Production settings remain unchanged.
- Unknowns: The comparison uses one short fixture on one machine. It does not measure TTS first-audio latency or output consistency.
- Data: `pocket-thread-paired-qualification` compares 25-, 246- and 600-character fixtures with seeds 42, 43 and 44.
- Data: Enable JVM property `vis.test.pocket-thread-profile=true` and select `com.blockether.vis.internal.speech.live-native-test/pocket-thread-paired-qualification`.
- Data: The test reloads and warms TTS for each setting: default, two threads, then default again.
- Data: All 27 generations finished with finite PCM. All nine default repeats were bit-identical to their references.
- Data: Two-thread output preserved every sample count and callback size. The largest absolute sample difference was 2.99e-7, with RMSE below 2.31e-8.
- Data: Two-thread first-audio times were 296–319 ms for short text, 663–681 ms for multiple sentences and 613–680 ms at the limit.
- Data: Initial default first-audio times were 357–366 ms, 916–1,120 ms and 955–1,268 ms, respectively.
- Data: At the character limit, two-thread generation took 6.09–6.72 seconds. Initial defaults took 9.21–10.75 seconds.
- State: Two threads improved first-audio and total generation time for every tested pair, including comparison with repeated defaults.
- Unknowns: Tiny numerical differences are not bit equality. Listening quality, other hardware and concurrent long-form speech remain unverified.
- State: Pocket now uses at most two threads, bounded by available processors. Piper and ASR settings remain unchanged.
- Data: A regression test failed before the change and passed after it. It checks one, two and ten processors and unchanged Piper settings.
- Data: The production-path benchmark passed all six measured trials with matching transcripts.
- Data: With concurrent TTS, scheduled audio-end-to-transcript time was 927–954 ms. Maximum modeled queue wait was 984–1,046 ms.
- Data: The comparison benchmarks retain explicit overrides for the old thread counts. Production-path reports include the actual Pocket thread count.
- State: Both native binaries are rebuilt and pass their affected synthetic checks. Physical audio and other hardware remain unverified.

## 12. Typed conversation and screen-only content

- Rationale: Hands-free use still needs visible diagrams, code and documents. Speech and displayed conversation must share one source.
- Data: A conversational reply omitted its speech fences. The gateway played the missing-speech notice instead of the reply.
- Data: The provider interface exposes one Python tool. Typed presentation records preserve that interface across providers.
- Acceptance criteria: Display and speak each validated conversation record once. Keep visual records on screen only.
- Acceptance criteria: Require a final conversational outcome or explicit user-requested silence. Progress alone does not satisfy this requirement.
- Acceptance criteria: Repair malformed final presentation once without tools. Preserve completed work if presentation still fails.
- Acceptance criteria: Preserve immutable commits, interruption, mute, epoch ownership and rich visual rendering.
- Acceptance criteria: Verify the affected engine, gateway, contract, SDK, Companion and TUI paths before human testing.
- Unknowns: Automated checks do not establish microphone behavior, acoustic echo handling or subjective voice quality.
- Data: Conversation, visual and silence records persist through live events and history reload. Provider JSON stays outside the displayed transcript.
- Data: The terminal filter initially dropped typed records and triggered a false missing-speech notice. Its regression failed before the fix.
- Data: The fixed JVM regression and rebuilt native fixture pass. The fixture verifies one spoken answer and the saved Mermaid block.
- State: Ready for human testing. Changes remain local and uncommitted, with no paid calls, device access or live service restart.

## Verification record

- No microphone, speaker, paid model call or live service restart has occurred.
- Real transport tests use owned loopback fixtures. Native speech tests use synthetic audio and a local stub provider.
- The first full JVM run had 12 failures among 7,321 cases. Four in-scope failures are fixed.
- The eight remaining failures came from the draft location and user settings. Their 15 affected cases pass with an isolated test home.
- The latest embedded SDK, utility, key-policy and runtime checks pass: 397 cases.
- The latest full JVM run completed 7,331 cases in 415.1 seconds. It reported 14 failures under a private home.
- All 14 failing cases pass within 108 targeted cases with the normal home. Private-home speech failures lacked the existing JNI cache.
- These mixed-profile checks are not a single clean full-suite pass.
- Companion capture ignores stale processor errors after mute, replacement and close. All 64 affected cases, typecheck, lint and formatting checks pass.
- The native fixture shares model and JNI caches with its private home. The rebuilt image passes incremental ASR, TTS and transport checks.
- Narrow metadata covers Ring callbacks and Jetty sink constructors. The fixture now handles transcript and completion events.
- An unintended wider native run stopped at `native-python-hang-evidence-test`. It has no final summary or pass claim. Its owned processes stopped.
- The monitoring worker was interrupted during the continuous run. Its saved log reports zero failures, but the monitor did not record an exit code.
- All matching fixture processes stopped. The cancelled worker was not restarted, and the continuous run was not repeated.
- The final native speech checks passed six cases. Lint reported no errors or warnings. Reflection checking reported only the existing emitter boxing warning.
- The Vis formatter was unavailable during the earlier native checks. The later typed-presentation checks use the available formatter successfully.
- The TUI native image includes `libjsound.dylib`. Its startup checks do not verify device capture or playback.
- The first corpus measured warm, buffered stage timings, not complete conversational latency.
- First-PCM TTS latency was 517.50 ms at p50 and 886.86 ms at p95.
- Final streaming ASR latency was 875.00 ms at p50 and 1,256.08 ms at p95.
- In the repeat, first-PCM TTS latency was 447.13 ms at p50 and 696.32 ms at p95.
- Repeat final streaming ASR latency was 867.65 ms at p50 and 978.51 ms at p95.
- These stages exclude physical capture, endpoint delay, the real agent and audible playback. Compiler and test contention can affect timings.
- The targets remain p95 at most 200 ms to stop old audio and two seconds to useful audio after speech ends.
- Do not add stage percentiles and call the result an end-to-end percentile.
- The Vis formatter passes for the typed-presentation Clojure changes and the native fixture.
- Remaining: Qualify physical audio, controls and actual agent latency on the supported desktop profiles.
- When a required check needs human access, keep this work in the draft. Do not commit or push it.
- Typed-presentation suites pass: 114 contract cases, 14 engine cases, 337 gateway cases and 520 persistence/reload cases. These suites overlap.
- Companion focused suites, typecheck, compiler lint and the production build pass. Existing build size warnings remain.
- TUI passes 444 affected cases and 10 rebuilt-native checks. SDK passes 88 affected cases.
- Documentation checks pass in 130 cases. Relevant formatting, lint and reflection checks pass.
- The rebuilt gateway passes the typed native voice fixture. It preserves conversation and visual records, with one matching speech segment.
- Cold synthetic finalization-to-first-PCM latency was 2,187.97 ms. This single trial does not establish the latency targets.

## 13. Mobile voice and echo cancellation

- Rationale: Mobile voice needs platform echo cancellation before speaker playback can share the microphone.
- Data: The Companion selects a native PCM bridge on iOS and Android. Browser capture still requires reported echo cancellation.
- Data: iOS uses system voice processing. Android 12 or newer requires an enabled acoustic echo canceller on its communication input.
- Data: Native capture and playback use the canonical 24 kHz format, bounded queues and session-owned generations.
- Data: Playback receipts come from rendered progress, not enqueue completion. Microphone transitions invalidate old output before native reconfiguration.
- Data: The mobile test checkbox resets after each session. Gateway capabilities list mobile test clients without setting `mobile_qualified` to true.
- Acceptance criteria: Add iOS and Android PCM capture and playback with platform echo cancellation enabled and checked.
- Acceptance criteria: Keep the existing bounded media protocol, ownership, interruption, mute and approval rules.
- Acceptance criteria: Require explicit mobile test opt-in. Stop on backgrounding, route loss or unsupported echo cancellation.
- Acceptance criteria: Test in owned simulators where possible. Gate physical audio and speakerphone qualification on human checks.
- Data: The user reports that echo cancellation worked in separate testing. This confirms that tested setup, not every mobile device.
- Unknowns: Cross-device echo rejection, speakerphone routing, Bluetooth, simultaneous speech and device latency remain unverified.
- State: Implemented and verified within the permitted test scope. Mobile use remains behind explicit human testing consent. All work stays local.

Verification at this phase:

- Companion: 89 affected unit and Storybook tests pass. Type checking, React Compiler lint and the production build pass.
- Gateway and documentation: 100 tests pass. Clojure formatting, lint and reflection checks pass.
- iOS preparation: All 71 script tests pass after the user accepted the SDK terms. The 15 affected cases pass after final formatting.
- Swift: All nine state tests pass. Simulator and unsigned device builds pass with Xcode 27.1.
- Four iOS 26.5 simulator tests pass with microphone access denied. These tests never start an audio engine.
- Tests cover plugin registration, rejected capture and playback, repeated close, and actual WebView calls.
- Swift formatting and lint pass for the bridge and smoke tests. LiveVoice compiles without warnings.
- Unrelated NativeSpeech, dependency and generated build warnings remain.
- Both iOS apps contain all 39 production bundle files with matching bytes. The owned iOS simulator was stopped and removed.
- The test target setup is repeatable and preserves the normal app scheme. Use `apps/vis-companion/scripts/ios-live-voice-smoke.rb` after CocoaPods and iOS preparation.
- Android: Seven native state tests and nine preparation tests pass. Debug and instrumentation APK builds pass.
- Android lint reports zero errors and 17 existing scaffold or dependency warnings. None concern LiveVoice.
- Four Android 36 emulator tests pass, including permission cancellation, stale callbacks, document navigation and actual WebView reload.
- The final APK contains every production bundle file with matching bytes. Its SHA256 is `f90454456750b6b62d027ae75f2bbb2209f413c604d3cb9ab8e8373450974eb1`.
- The owned emulator is stopped. Android 8–11 cannot start live voice because idle route monitoring requires Android 12.
- The Android emulator tests acquire no microphone or speaker. They do not establish echo performance.
- The emulator uses an isolated SDK and AVD under generated `apps/vis-companion/android/`. Existing SDK licenses were reused without new agreements.
- Physical devices, host audio, paid model calls, installed services and remote publication were not used.

Mobile verification gate:

1. On each physical device, start at low volume. Android still requires the mobile testing checkbox; iOS uses the microphone hold.
2. Check that speaker playback does not produce a second request through the microphone.
3. Speak during playback and verify interruption, transcription and the next answer.
4. Mute and unmute during speech. Check microphone indicators, new playback and rejection of old audio.
5. Check speaker, headset and Bluetooth routes separately. Check backgrounding, screen lock, reload and permission cancellation.
6. Do not claim mobile acoustic qualification until these checks pass. Simulator results cannot establish it.

The TUI remains headset-only. Its JavaSound path does not provide echo cancellation.

## 14. Speech timing and active task follow-ups

- Rationale: Spoken follow-ups should steer active voice work without losing the next reply or cancelling the task.
- Data: The shared endpoint now waits 1,100 ms. Speech qualification stays at 120 ms.
- Data: A new follow-up marks only its own queue row for the next step of an active, owned voice task.
- Data: Ordinary queues and cancel holds remain unchanged. Tasks started outside voice mode do not gain automatic voice steering.
- Data: The bridge now observes delivered queue rows and transfers voice ownership to the running task.
- Data: The next reply uses the current output epoch. Old audio, earlier iterations and duplicate sources remain suppressed.
- Data: Consumed queue rows become terminal voice tasks. An explicit cancellation then targets the running task, not a consumed row.
- Acceptance criteria: Test the 1,100 ms endpoint in the shared contract and both clients.
- Acceptance criteria: Speak a steered answer once without replaying interrupted progress. Keep task cancellation separate.
- Acceptance criteria: Preserve queue order, held requests, session ownership, output limits and local-only work.
- Unknowns: The historical truncated progress and missing final reply have no complete playback trace. Their exact cause remains unconfirmed.
- Unknowns: Contraction pronunciation needs a controlled comparison with straight and curly apostrophes using the same voice and sentence.
- State: The endpoint and steering regressions pass. No build, deployment or service restart activated these changes.

Verification at this phase:

- Reproduced the TUI failure with its old 700 ms fixture. All 25 TUI voice tests pass after the fixture and boundary checks changed.
- Reproduced missing speech ownership and absent steering with two failing gateway tests. Both pass after the fix.
- All 81 affected gateway, contract, queue and documentation tests pass. Additional guards cover stale input, closed sessions and unrelated tasks.
- All nine Companion protocol tests pass. Type checking and React Compiler lint pass.
- Clojure formatting, lint and reflection checks pass for the changed files.
- No Prettier configuration or dependency exists in these projects. Keep the existing JSON and TypeScript style.
- The contraction investigation passed 52 existing speech tests. The speech entry points preserve apostrophes and trim only outer whitespace.
- Straight and curly apostrophes use different Pocket tokenizer paths. This does not prove missing text or a pronunciation defect.
- No pronunciation rewrite was applied. No physical audio, paid model calls or installed services were used for these checks.

## 15. Voice controls and reported speaker echo

- Rationale: Replace the older voice conversation entry with the microphone hold gesture.
- Acceptance criteria: Keep tap dictation. Show voice controls only after an explicit start request.
- Acceptance criteria: Remove the iOS testing checkbox and request admission count. Keep errors and uncertain request warnings.
- Data: iOS enables voice processing and routes playback through the same engine as capture.
- Data: The user reported that speaker playback became microphone input. The current energy detector can accept residual echo.
- Unknowns: No captured audio or device trace establishes the acoustic cause. Speaker echo and simultaneous speech still need physical checks.
- Data: Five voice session tests failed because their silence fixtures still used 700 ms.
- Data: Fixtures now use the shared endpoint duration. All 73 session, protocol, browser, native adapter and worklet tests pass.
- Data: The microphone hold starts hands-free voice. Tap still dictates. The panel stays hidden until requested.
- Data: iOS no longer requires the testing checkbox. Android keeps its separate verification step.
- Data: Request counts and pilot labels are removed. Retry, connection cancellation and separate audio and task controls remain.
- Data: All 26 affected UI unit tests and eight Storybook tests pass. Type checking, React Compiler lint and the production build pass.
- State: Interface changes are verified locally. No acoustic fix is claimed. No device was deployed or service restarted.

## 16. Residual speaker echo

- Rationale: Speaker playback must not start another request through the phone microphone.
- Data: The phone trace showed a stale 44.1 kHz stereo connection during the Bluetooth switch to 16 kHz mono.
- Data: iOS connects mixer output with the capture format before startup. Bounded retries refresh this connection and replace the capture tap.
- Data: iOS now checks both processing flags, bypass state and matching IO formats after startup and route retries.
- Data: Startup format mismatches wait without sending PCM. Invalid processing or mismatches after startup fail closed.
- Data: Startup still waits for its first valid PCM frame. Capture retries preserve output receipt ownership.
- Data: Microphone holds wait for dictation cleanup before native capture starts. Stale startup requests cannot reclaim the microphone.
- Data: The Companion compares microphone input with an 80 ms output reference before speech detection.
- Data: Reference storage has sample and frame limits. Only playback receipts confirm a reference.
- Data: Output gaps and epochs remain separate. Only a confirmed echo tail remains available after playback stops.
- Data: Browser references include scheduled starts and device latency. These estimates never confirm playback or grant credit.
- Data: Clear matches become silence only when unexplained input stays below one fifth of the speech threshold.
- Data: A confirmed match follows capture progress. Fractional alignment keeps sample clock drift from selecting older audio.
- Data: Tests preserve quiet overlapping speech, periodic input, capture restarts and output gaps. Interruption never becomes task cancellation.
- Acceptance criteria: Reproduce feedback with injected PCM. Preserve quiet overlapping speech, bounded transport and stale audio rejection.
- Acceptance criteria: Use only confirmed playback. Preserve input while receipts are delayed. Reject references across gaps or epochs.
- Verification: The earlier 189 affected unit tests pass. The updated native path passes 13 Swift tests and the iOS preparation test.
- Verification: Type checking and React Compiler lint passed before the native update. No TypeScript files changed in that update.
- Verification: Five local Pocket samples each pass 18 echo and speech cases. No sound was played.
- Verification: Swift format lint, the production build and the unsigned iOS device build pass. Signing settings remain unchanged.
- Unknowns: Physical speaker echo, simultaneous speech, Bluetooth and sustained phone processing cost still need human checks.
- State: The local candidate passes injected tests. The phone acoustic result remains unverified. No deployment, commit or push occurred.

## 17. Spoken reply recovery

- Rationale: An admitted voice request must receive its conversational answer, not only a presentation failure notice.
- Data: The real phone captured PCM and admitted its transcript after the native format fix.
- Data: The gateway recorded 101760 fallback samples as played. The requested spoken answer was unavailable.
- Data: Repair requests used keyword `:low`, but Svar requires a supported string for an exact effort.
- Data: Repair now prefers the string `"low"`. It removes the task's exact effort and uses the model's supported options.
- Acceptance criteria: Repair malformed final output without repeating tools, admitting another request or publishing raw records.
- Verification: All 76 affected presentation, gateway, coordinator and transport tests pass. Formatting, lint and reflection checks pass.
- Verification: The native gateway build passes. Both isolated voice tests pass with local speech models and a stub language model.
- Verification: Malformed replies repair once and produce answer PCM. The test preserves one admission and disables tools for repair.
- Unknowns: The running gateway must load this fix before the next phone test. Acoustic echo and simultaneous speech remain unverified.
- State: The local gateway candidate is ready. No service restart, deployment, paid call, physical playback, commit or push occurred.

## 18. Playback during capture startup

- Rationale: Capture retries must not report queued audio as played or remove its echo reference before playback starts.
- Data: Eight failing adapter cases reproduced premature playback during startup. Two failing echo cases reproduced lost references after startup delays.
- Data: The adapter holds output until capture startup succeeds. Cancellation discards held output without playback credit.
- Data: Microphone frames still pass during startup. Muted capture does not delay playback.
- Data: Unplayed references remain bounded by the existing sample and frame limits. Only playback receipts make them eligible.
- Acceptance criteria: Preserve capture generations, output epochs, queue limits and overlapping human speech. Never confirm playback from elapsed time.
- Verification: All 156 affected unit tests and 18 local speech cases pass. The ten new regression cases pass.
- Verification: Type checking, React Compiler lint and the production build pass. No TypeScript formatter is configured.
- Verification: The unsigned iOS device build passes. All 39 embedded assets match the production bundle; signing settings are unchanged.
- Unknowns: The phone must load the updated app and gateway. Speaker echo, simultaneous speech and sustained processing remain unverified.
- State: Changes remain local. Locked-screen behavior is unchanged. No service restart, deployment, physical playback, commit or push occurred.

## 19. Voice behind a locked screen

- Rationale: A hands-free session must continue when the phone locks. Before this phase, a lock ended the session.
- Data: Two deliberate stops caused this. The iOS bridge failed on `willResignActive`. The web adapter failed when the page became hidden.
- Data: The Android bridge failed on activity pause and stop. Android also stops the microphone of a background process without a foreground service.
- Data: This phase replaces the phase 13 rule "Stop on backgrounding". Route loss, interruptions and unsupported echo cancellation still stop the session.
- Data: The gateway changes the capture generation only on mute, unmute, overload and close. An unmuted session keeps one capture graph through a lock.
- Data: iOS keeps the audio session active while the app is not in the foreground. iOS can refuse a new activation in the background.
- Data: iOS starts a playback-only graph when the app leaves the foreground without running audio. A muted session thus continues behind the lock.
- Data: Foreground transitions on iOS are unchanged. Mute in the foreground still releases the audio session.
- Data: The voice session logic and the media socket run in the web view. From iOS 17, the bridge sets `inactiveSchedulingPolicy` to `none` at load.
- Data: Android starts `LiveVoiceService`, a foreground service with the types `mediaPlayback` and `microphone`, a wake lock and an ongoing notice.
- Data: Android adds the microphone type only after the permission is granted. The service stops when the session ends.
- Data: A new session still needs the foreground on both platforms. Capture can restart behind the lock only in a live session.
- Data: The web adapter no longer reads page visibility. A resumed iOS web view can continue to report a hidden document.
- Data: New iOS projects get a microphone purpose text that names hands-free voice. An existing generated project keeps its text.
- Decision: The session also continues when the user opens another app. The system shows its microphone indicator.
- Decision: There is no lock-screen control. To mute or end voice, the user opens the app. Android shows an ongoing notice that opens the app.
- Decision: Approvals stay on the screen. The gateway rules for spoken progress and approvals are unchanged.
- Acceptance criteria: A lock and an unlock do not end a live session, with the microphone on or muted.
- Acceptance criteria: Calls, alarms, route loss and a media service reset still end the session with a message.
- Verification: The new adapter cases failed before the change. All 3,531 Companion unit cases pass, with 19 skipped. Type checking and compiler lint pass.
- Verification: All 14 Swift state cases pass. The iOS bridge type-checks for a device against the built Capacitor framework. Swift format lint passes.
- Verification: All 26 affected script cases pass. The Android debug sources compile, all seven state cases pass and lint reports zero errors.
- Unknowns: No phone ran this phase. The web view schedule behind a lock, the audio session behind a lock and the Android service are unverified on devices.
- Unknowns: A hidden web view can delay timers. A long silent lock can thus lose the 15 second lease. This is unverified.
- Unknowns: Capture restart behind the lock after an overload is unverified. A locked phone uses its speaker, where echo remains unresolved.
- State: Changes remain local in the draft. No iOS preparation, service restart, deployment, device use, commit or push occurred.

## Human testing handoff

Use the rebuilt draft gateway at `target/vis` and TUI at `apps/vis-tui/target/vis-tui`.
The Companion production bundle is at `apps/vis-companion/dist`.
The Android debug APK is at `apps/vis-companion/android/app/build/outputs/apk/debug/app-debug.apk`.
The iOS simulator app is at `apps/vis-companion/build/ios-voice-verification/Build/Products/Debug-iphonesimulator/App.app`.
The unsigned iOS device build is at `apps/vis-companion/build/ios-voice-device/Build/Products/Debug-iphoneos/App.app`.
The gateway binary includes phase 17. The current web bundle and unsigned iOS device build include phases 15, 16 and 18.
No installed service was replaced or restarted.

1. Enable hands-free voice in Voice settings. Hold the microphone to start. On Android, complete the separate consent step above.
2. Ask for a short answer. Check that the same words appear once and play once.
3. Ask for a Mermaid diagram and code. Check that visuals render and are not narrated.
4. Ask explicitly for a screen-only answer. Check that the requested silence remains silent.
5. Interrupt playback. Check that old audio stops without cancelling agent work.
6. Check mute and unmute. Check that muted capture stops and stale audio does not resume.
7. Reload the session. Check that conversation and visuals remain, without raw provider records or duplicate speech.
8. Try a task with tools. Check spoken progress, the final outcome and existing approval prompts.
9. Measure response latency and listening quality with the actual provider and devices.
10. Check headset routing, speaker echo, double-talk, background noise and device changes separately.
11. Use the phone speaker. Let one answer play without speaking. Check that playback creates no user request.
12. Speak a quiet request during playback. Check that speech stops the old audio without cancelling work.
13. Before the lock checks, run `node scripts/ios-prepare.mjs` in `apps/vis-companion` and build again. The installed build has no `LiveVoicePlugin` registration.
14. Start voice, then lock the phone. Speak a request. Check that the answer plays behind the lock.
15. Keep the phone locked and silent for ten minutes. Speak again. Check that the session continues.
16. Mute, lock the phone for two minutes, then unlock. Check that the session is live and that unmute works.
17. Unlock during playback. Check that the controls and the transcript are current.
18. Take a call or let an alarm sound behind the lock. Check that voice ends with a message.
19. Open another app during voice. Check the microphone indicator. Return to Vis and end voice.
20. On Android, repeat checks 14 to 19. Check that the ongoing notice appears and disappears with the session.

The latest phone test captured speaker playback as input. Speaker echo remains unresolved; broader device coverage and pronunciation remain unverified.
This handoff is not a release or a claim that the latency targets pass.
