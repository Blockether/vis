# vis — React Native companion

An Expo (React Native) companion app for a running vis gateway, in the
blockether.com look: warm paper, amber gold, ink, square corners, mono
accents. TypeScript 7, strict.

## Feature map

| Area | What it does | Backed by |
| --- | --- | --- |
| Header | `vis▌` wordmark, live `● connected` chip, model chip (⚡), settings (⚙), sessions (▤) | `/v1/sessions` |
| Sessions drawer | slide-in panel: status dot, title, channel · age · id, **rename** (✎), **delete** (🗑 with confirm), `+ new session` | `GET/POST/PATCH/DELETE /v1/sessions[/:sid]` |
| Transcript | white cards with colored left rule (amber YOU / green VIS), mono role + HH:MM, markdown-lite: fenced code in ink terminal boxes, headings, bullets, inline `code` / **bold**; a running turn **streams live tool-call cards** (colored op-card badge + summary/stdout, running spinner, duration) and streaming prose/thinking as they happen | `GET /v1/sessions/:sid/turns` (1.8s poll, pull-to-refresh) + `GET /v1/sessions/:sid/events` (native SSE via `react-native-sse` EventSource, live overlay, auto-reconnect w/ Last-Event-ID) |
| Composer | multiline input, amber ↑ send; while a turn runs the send button becomes a red ■ **cancel** | `POST …/turns`, `POST …/turns/:tid/cancel` |
| Voice | 🎤 records 16-bit PCM WAV @16kHz mono (expo-audio), pulsing red dot + timer + ✕/✓ strip, transcribes through the gateway's local Parakeet model; drives the model download with progress if it isn't installed yet. iOS-first (Android's MediaRecorder can't emit WAV) | `POST /ui/session/:sid/voice`, `GET/POST …/voice/model` (web channel) |
| Model picker | provider chips + model input, applies to the session | `GET /v1/models`, `GET/PATCH /v1/sessions/:sid/model` |
| Dialogs | amber title bars (`--dialog-title-bg #f0ad00`), red for errors, square corners everywhere | blockether.com tokens |
| Icons | Feather via `@expo/vector-icons` — the same set as the web channel's `icons.svg` | |

## Run

```sh
cd app
npm install --legacy-peer-deps
EXPO_PUBLIC_VIS_GATEWAY_URL=http://127.0.0.1:7890 npm run start
```

Press `i` for the iOS simulator. Configure gateway URL + bearer token
in-app under ⚙. `npm run typecheck` runs TypeScript 7 strict
(`exactOptionalPropertyTypes`, `noUncheckedIndexedAccess`). `npm test` runs
the Jest (jest-expo) unit suite over the pure logic: the SSE→card reducer
(`LiveTurns`), the streaming markdown fence splitter (`Markdown`), the gateway
client's error unwrap + SSE dispatch (`VisClient`), and the drawer time/id
helpers (`theme`).

Voice requires the `vis-channel-web` + `vis-foundation-voice` extensions
on the gateway (the app degrades gracefully with a note when absent).

## Layout

```
App.tsx                 orchestration: header, transcript, composer, dialogs
src/theme.ts            blockether.com design tokens + time helpers
src/VisClient.tsx       typed gateway client (/v1 + voice endpoints)
src/ui.tsx              IconBtn, DialogModal, ActionBtn
src/Markdown.tsx        markdown-lite transcript renderer
src/SessionsDrawer.tsx  slide-in sessions panel
src/VoiceButton.tsx     mic + capture strip + Parakeet upload
```
