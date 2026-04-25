# Directory Structure

```
src/com/blockether/vis/
├── core.clj                          public API facade
│                                     (create-environment, query!,
│                                      dispose-environment!)
├── config.clj                        config I/O, provider presets
│
├── loop/
│   ├── core.clj                      environment lifecycle + iteration loop
│   │                                 (create-environment, dispose-environment!,
│   │                                  register-extension!, iteration-loop,
│   │                                  run-iteration, execute-code)
│   ├── mustache.clj                  Mustache template rendering for :answer
│   └── runtime/
│       ├── prompt.clj                system prompt builder + nudge composers
│       │                             (budget-warning, var-index-overflow,
│       │                              repetition-warning,
│       │                              collect-extension-nudges)
│       ├── shared.clj                realize-value, truncate, shape, format helpers
│       └── conversation/
│           ├── core.clj              conversation lifecycle
│           │                         (create!, send!, close!, delete!,
│           │                          by-channel, cache, make-on-chunk-projector,
│           │                          error->user-message)
│           └── environment/
│               ├── core.clj          SCI sandbox primitives
│               │                     (create-sci-context, build-var-index,
│               │                      bind-and-bump!, var-history)
│               ├── extension.clj     extension spec + hook execution
│               └── query/
│                   ├── core.clj      query engine
│                   │                 (query!, prepare-query-context,
│                   │                  run-iteration-phase, router lifecycle)
│                   └── iteration/
│                       └── core.clj  single iteration engine
│                                     (run-iteration, execute-code,
│                                      format-expression-results,
│                                      build-iteration-context)
│
├── persistance/
│   ├── core.clj                      public persistence API (delegates to sqlite)
│   ├── spec.clj                      iteration specs, constants, dynamic vars
│   └── sqlite/
│       └── core.clj                  SQLite implementation (HoneySQL)
│
└── channels/
    ├── web/                          Web adapter (Jetty + routes + SSE)
    │   ├── app.clj                   process boot (Jetty start/stop)
    │   ├── routes.clj                HTTP routes (no DB/env poking)
    │   ├── conversations.clj         conversation list/page/title/cache
    │   ├── executor.clj              async turn execution + live progress
    │   └── presentation*.clj         pure rendering
    ├── tui/                          TUI adapter (Lanterna)
    │   ├── state.clj                 app state (re-frame pattern)
    │   ├── screen.clj                main loop + rendering
    │   ├── chat.clj                  LLM integration
    │   ├── input.clj                 keyboard handling
    │   └── theme.clj                 colors + visual constants
    ├── telegram/                     Telegram adapter
    │   └── bot.clj                   bot polling + message handling
    └── cli.clj                       CLI adapter (vis run/chat/web/doctor)
```
