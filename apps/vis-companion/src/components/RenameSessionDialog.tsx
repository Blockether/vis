import { useRef, useState } from "react";

import type { Session } from "../lib/types";
import { shortId } from "./SessionList";
import { Banner, Button, DialogFrame, Input, Modal } from "./ui";

/** Owns the unsaved name and request lifecycle for renaming one session. */
export function RenameSessionDialog({
  session,
  onDismiss,
  onRename,
}: {
  session: Pick<Session, "id" | "title">;
  onDismiss: () => void;
  onRename: (title: string) => Promise<void>;
}) {
  const [draft, setDraft] = useState(session.title?.trim() ?? "");
  const [isSaving, setIsSaving] = useState(false);
  const [error, setError] = useState("");
  const active = useRef(true);

  const dismiss = () => {
    active.current = false;
    onDismiss();
  };
  const save = async () => {
    const title = draft.trim();
    if (!title) {
      setError("A session name cannot be empty.");
      return;
    }
    setIsSaving(true);
    setError("");
    try {
      await onRename(title);
      if (active.current) dismiss();
    } catch (cause) {
      if (active.current) {
        setError(
          cause instanceof Error
            ? cause.message
            : "Could not rename this session.",
        );
      }
    } finally {
      if (active.current) setIsSaving(false);
    }
  };

  return (
    <Modal size="fit" onDismiss={dismiss}>
      <DialogFrame title="Rename session" onClose={dismiss}>
        <div className="space-y-3 p-4">
          <p className="truncate font-mono text-meta text-dialog-hint">
            {session.title?.trim() || "Untitled session"} ·{" "}
            {shortId(session.id)}
          </p>
          <label className="block">
            <span className="mb-1 block font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
              Session name
            </span>
            <Input
              autoFocus
              value={draft}
              maxLength={200}
              placeholder="Session name"
              onChange={(event) => setDraft(event.target.value)}
              onKeyDown={(event) => {
                if (event.key === "Enter") void save();
              }}
            />
          </label>
          {error && <Banner kind="err">{error}</Banner>}
          <div className="flex justify-end gap-2">
            <Button variant="secondary" onClick={dismiss}>
              Cancel
            </Button>
            <Button
              variant="primary"
              disabled={isSaving}
              onClick={() => void save()}
            >
              {isSaving ? "Saving..." : "Save"}
            </Button>
          </div>
        </div>
      </DialogFrame>
    </Modal>
  );
}
