import { useState } from "react";

import { keepKeyboard } from "../lib/keyboard";
import { pasteSummary, type ComposerPaste } from "../lib/paste";
import { useSafeBottomStyle } from "../lib/viewport";
import { Button, DialogHeader } from "./ui";

export function PasteEditor({
  paste,
  onDismiss,
  onSave,
}: {
  paste: ComposerPaste;
  onDismiss: () => void;
  onSave: (content: string) => void;
}) {
  // The draft belongs to the dialog. Its caller owns only the selected domain
  // object and the persistence boundary, so typing does not rerender the screen.
  const [draft, setDraft] = useState(paste.content);
  // `--safe-bottom` rides in on this element instead of the document root: it
  // changes with every keyboard movement, and a root-scoped custom property is
  // a whole-document style recalculation (see `useSafeBottomStyle`).
  const safeBottomStyle = useSafeBottomStyle();
  const save = () => onSave(draft);

  // This overlay lives inside SessionScreen's positioned root, so it follows
  // the app shell without creating its own fixed WebKit layer.
  return (
    <div
      className="absolute inset-0 z-50 flex h-full items-stretch justify-center bg-ink/85 p-0 pl-[env(safe-area-inset-left)] pr-[env(safe-area-inset-right)] backdrop-blur-[2px] transition-opacity duration-200 starting:opacity-0 motion-reduce:transition-none sm:items-center sm:p-5"
      onMouseDown={(event) => {
        if (event.target === event.currentTarget) onDismiss();
      }}
    >
      <section
        className="flex h-full w-full max-w-3xl flex-col overflow-hidden border-dialog-edge bg-panel shadow-none transition-[opacity,transform,translate,scale,rotate] duration-200 starting:translate-y-6 starting:opacity-0 motion-reduce:transition-none sm:h-[70%] sm:max-h-[calc(100%-2rem)] sm:border sm:shadow-[8px_8px_0_var(--dialog-shadow)] sm:starting:translate-y-2"
        role="dialog"
        aria-modal="true"
        aria-labelledby="paste-editor-title"
        onKeyDown={(event) => {
          if (event.key === "Escape") {
            event.stopPropagation();
            onDismiss();
          } else if (
            event.key === "Enter" &&
            (event.metaKey || event.ctrlKey)
          ) {
            event.preventDefault();
            save();
          }
        }}
      >
        <DialogHeader
          isUnderNotch
          titleId="paste-editor-title"
          title={`Pasted #${paste.id}`}
          subtitle={pasteSummary(paste.id, draft)}
          closeLabel="Close paste editor"
          onClose={onDismiss}
        />

        <textarea
          // eslint-disable-next-line jsx-a11y/no-autofocus
          autoFocus
          value={draft}
          onChange={(event) => setDraft(event.target.value)}
          spellCheck={false}
          autoCapitalize="off"
          autoCorrect="off"
          className="min-h-0 flex-1 resize-none touch-pan-y overflow-y-auto overscroll-contain border-t border-dialog-edge bg-input p-3 font-mono text-body text-dialog-foreground outline-none sm:p-4"
          aria-label={`Content of pasted block ${paste.id}`}
        />

        <footer
          style={safeBottomStyle}
          className="flex shrink-0 items-center justify-end gap-2 border-t border-dialog-edge bg-panel-2 px-3 py-2 pb-[max(0.5rem,var(--safe-bottom,env(safe-area-inset-bottom)))] font-mono text-meta text-dialog-hint sm:px-4"
        >
          <span className="mr-auto hidden truncate sm:block">
            Esc cancels · ⌘↵ saves
          </span>
          <Button
            variant="secondary"
            onMouseDown={keepKeyboard}
            onClick={onDismiss}
          >
            Cancel
          </Button>
          <Button onMouseDown={keepKeyboard} onClick={save}>
            Save
          </Button>
        </footer>
      </section>
    </div>
  );
}
