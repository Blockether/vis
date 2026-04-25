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
    ├── core.clj                      cross-channel provider mgmt,
    │                                 streaming, extension CLI
    ├── tui/                          TUI adapter (Lanterna)
    │   ├── state.clj                 app state (re-frame pattern)
    │   ├── screen.clj                main loop + rendering
    │   ├── chat.clj                  LLM integration
    │   ├── input.clj                 keyboard handling
    │   ├── render.clj                render helpers
    │   ├── dialogs.clj               dialog components
    │   ├── primitives.clj            TUI primitive widgets
    │   ├── provider.clj              provider selection UI
    │   └── theme.clj                 colors + visual constants
    ├── telegram/                     Telegram adapter
    │   ├── bot.clj                   bot polling + message handling
    │   └── api.clj                   Telegram HTTP API client
    └── cli.clj                       CLI adapter (vis run/chat/web/doctor)
    └── cli/
        └── agent.clj                 one-shot CLI agent helper
```
