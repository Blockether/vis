# Vis - Development Guide

## Clojure CLI

### JS Interop

Always use `^js` type annotations when interoperating with JavaScript:

```clojure
;; Good - specify JS types
(defn handler [^js key]
  ...)

;; Good - convert Clojure data to JS objects
(react/createElement Component #js {:props value})

;; Bad - will cause type inference issues
(defn handler [key]
  ...)
```

### Running

```bash
clojure -M:run
```

### Project Structure

- `src/com/blockether/vis/cli.clj` - CLI entrypoint (subcommands: chat, run, help)
- `src/com/blockether/vis/agent.clj` - Agent orchestration over svar RLM (Sandcastle-inspired)
- `src/com/blockether/vis/config.clj` - Config I/O, provider presets, svar-native helpers
- `src/com/blockether/vis/logging.clj` - TTY logging setup, stream redirection
- `src/com/blockether/vis/redact.clj` - Secret redaction for logs and tool output
- `src/com/blockether/vis/trace.clj` - ANSI trace renderer for CLI + structured sections for TUI
- `src/com/blockether/vis/languages/commons/read.clj` - Base READ tool (offset/limit)
- `src/com/blockether/vis/languages/commons/write.clj` - Base WRITE tool (full overwrite)
- `src/com/blockether/vis/languages/commons/edit.clj` - Base EDIT tool (old_string → new_string)
- `src/com/blockether/vis/tui/state.clj` - App state (re-frame pattern)
- `src/com/blockether/vis/tui/screen.clj` - TUI main loop and rendering
- `src/com/blockether/vis/tui/render.clj` - Rendering components (boxes, bubbles, text wrapping)
- `src/com/blockether/vis/tui/chat.clj` - LLM integration via SVAR RLM
- `src/com/blockether/vis/tui/dialogs.clj` - Interactive dialog screens
- `src/com/blockether/vis/tui/input.clj` - Keyboard input handling
- `src/com/blockether/vis/tui/provider.clj` - Provider setup wizard (TUI dialogs)
- `src/com/blockether/vis/tui/theme.clj` - Colors and visual constants

### Agent Module (agent.clj)

Sandcastle-inspired agent orchestration. Define agents as data, register tools, run one-shot queries with JSON/EDN output.

**Programmatic usage:**
```clojure
(require '[com.blockether.vis.agent :as ag])

;; Define an agent with tools
(def reviewer
  (ag/agent {:name "reviewer"
             :system-prompt "You are a senior Clojure engineer. Review code for bugs, performance, and style."
             :tools [(ag/tool 'read-file slurp
                       {:doc "Read file contents"
                        :params [{:name "path" :type :string :required true}]
                        :returns {:type :string :description "File contents"}})]
             :max-iterations 30}))

;; Run a query
(ag/run! reviewer "Review src/auth.clj for security issues")
;; => {:answer "..." :iterations 5 :duration-ms 2340 :tokens {...} :cost {...}}

;; JSON output
(ag/result->json result)
```

**CLI usage:**
```bash
vis run "What is 2+2?"                          # plain text output
vis run --json "Explain the auth flow"          # JSON output
vis run --model gpt-4o "Summarize X"            # override model
vis run --system-prompt "You are a code reviewer" "Review auth.clj"
```

### State Management (MANDATORY)

All application state lives in `tui/state.clj` using a re-frame dispatch pattern.

**Rules:**
- ALL app state must go through `state/app-db` — never create standalone atoms for app state
- State changes MUST use `(state/dispatch [:event-name args...])` — never `swap!`/`reset!` on app-db directly
- Event handlers registered with `reg-event-db` must be **pure functions**: `(fn [db event-vec] new-db)`
- Side effects (RLM calls, disk I/O, futures) go through `reg-event-fx` + `reg-fx`
- Local ephemeral state in dialogs (selection index, cursor position) may use local atoms — these are component-scoped and transient

```clojure
;; Good — dispatch an event
(state/dispatch [:send-message text])

;; Good — pure event handler
(reg-event-db :update-input
  (fn [db [_ new-input]]
    (assoc db :input new-input)))

;; Good — side effects via reg-event-fx
(reg-event-fx :send-message
  (fn [db [_ text]]
    {:db (-> db (update :messages conj msg) (assoc :loading? true))
     :fx [[:rlm-query (:conv db) text]]}))

;; Bad — direct atom mutation
(swap! some-atom assoc :key val)
```

**App-db shape:**
```clojure
{:config     nil              ;; provider config map: {:providers [{:id kw :label str :api-base str :api-key? str :model str} ...]}
 :conv       nil              ;; {:env ...} RLM environment
 :messages   []               ;; [{:role :user|:assistant :text str :timestamp #inst}]
 :msg-scroll nil              ;; row offset, nil = auto-bottom
 :input      {:lines [] :crow 0 :ccol 0}
 :loading?   false            ;; true while RLM is working
 :dialog-open? false}         ;; dialog singleton guard
```
