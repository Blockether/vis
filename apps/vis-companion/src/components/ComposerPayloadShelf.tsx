import type { PendingAttachment } from "../lib/attachments";
import { isAudioMediaType, isVideoMediaType } from "../lib/attachments";
import { keepKeyboard } from "../lib/keyboard";
import type { ComposerPaste } from "../lib/paste";
import { MicIcon } from "./icons";
import { ExpandableImage } from "./ImageViewer";
import { CloseButton, TextButton } from "./ui";

export type ComposerPayloadCommands = {
  editPaste: (id: number) => void;
  removePaste: (id: number) => void;
  editAttachment: (id: string, edited: Blob) => void | Promise<void>;
  removeAttachment: (id: string) => void;
};

/** The staged content that will accompany the composer's text. */
export function ComposerPayloadShelf({
  pastes,
  attachments,
  commands,
}: {
  pastes: readonly ComposerPaste[];
  attachments: readonly PendingAttachment[];
  commands: ComposerPayloadCommands;
}) {
  return (
    <>
      {pastes.length > 0 && (
        <div className="flex gap-1 overflow-x-auto overscroll-x-contain border-b border-dialog-edge px-1.5 py-1 [scrollbar-width:thin]">
          {pastes.map((paste) => (
            <span
              key={paste.id}
              className="inline-flex min-h-7 shrink-0 items-center overflow-hidden rounded-chip border border-code-edge bg-code font-mono text-chip"
            >
              <TextButton
                isToken
                className="max-w-56 shrink"
                onMouseDown={keepKeyboard}
                onClick={() => commands.editPaste(paste.id)}
                aria-label={`Edit pasted block ${paste.id}`}
                title="Edit this paste"
              >
                {paste.token}
              </TextButton>
              <CloseButton
                label={`Remove pasted block ${paste.id}`}
                onMouseDown={keepKeyboard}
                onClick={() => commands.removePaste(paste.id)}
              />
            </span>
          ))}
        </div>
      )}

      {attachments.length > 0 && (
        <div className="flex gap-1.5 overflow-x-auto overscroll-x-contain border-b border-dialog-edge px-1.5 py-1.5 [scrollbar-width:thin]">
          {attachments.map((attachment) => (
            <div
              key={attachment.id}
              className="group relative flex min-w-0 max-w-40 shrink-0 items-center gap-1.5 overflow-hidden rounded-chip border border-dialog-edge bg-panel pr-8 transition-[opacity,transform,translate,scale,rotate] duration-150 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none"
            >
              {isVideoMediaType(attachment.media_type) ? (
                <video
                  src={attachment.previewUrl}
                  className="size-8 shrink-0 object-cover"
                  muted
                  playsInline
                  preload="auto"
                />
              ) : isAudioMediaType(attachment.media_type) ? (
                <span className="flex min-w-0 flex-1 items-center gap-1.5 py-1.5 pl-1.5">
                  <MicIcon className="size-4 shrink-0" />
                  <span className="truncate font-mono text-chip text-dialog-hint">
                    {attachment.filename}
                  </span>
                </span>
              ) : (
                <ExpandableImage
                  src={attachment.previewUrl}
                  alt=""
                  loading="eager"
                  className="size-8 shrink-0 object-cover"
                  frameClassName="min-w-0 flex-1"
                  onApply={(edited) =>
                    commands.editAttachment(attachment.id, edited)
                  }
                >
                  <span className="truncate font-mono text-chip text-dialog-hint">
                    {attachment.filename}
                  </span>
                </ExpandableImage>
              )}
              <CloseButton
                label={`Remove ${attachment.filename}`}
                className="absolute inset-y-0 right-0 my-auto"
                onMouseDown={keepKeyboard}
                onClick={() => commands.removeAttachment(attachment.id)}
              />
            </div>
          ))}
        </div>
      )}
    </>
  );
}
