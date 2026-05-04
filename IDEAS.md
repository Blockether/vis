# Ideas

1. Support extensions written in shell scripts, not only Clojure.
2. Automatically copy the current selection.
3. Notify when an agent response ends.
   - Reference: https://github.com/mitsuhiko/agent-stuff/blob/main/extensions/notify.ts
4. Save every agent edit to a git worktree/checkpoint so every state is a tree that can become a new branch. Support time travel for conversations, files, or both.
   - Reference: https://github.com/prateekmedia/pi-hooks/tree/main/checkpoint
5. When a prompt is submitted and then cancelled with Escape, restore the prompt to the input instead of rendering "cancelled by user", so it can be edited and resent.
6. Introduce a single CRaC bootstrap component that runs before TUI/extensions, including in development.
7. Add a --standalone flag for the TUI that starts Lanterna with the Swing backend for standalone app-like UX, font sizing, and custom fonts.
8. Add conversation switching/tabs in the TUI so users can switch between conversations.
