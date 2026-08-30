import { Capacitor } from "@capacitor/core";
import { useRef, useState, type ChangeEvent } from "react";

import { keepKeyboard } from "../lib/keyboard";
import { CameraIcon, ClipIcon, ImageIcon, PlusIcon } from "./icons";
import { MenuItem } from "./Menu";
import { ComposerButton } from "./ui";

export type ComposerAttachmentSource = "camera" | "media" | "files";

export type ComposerAttachmentCommands = {
  addBrowserFiles: (files: File[]) => void | Promise<void>;
  pickNative: (source: ComposerAttachmentSource) => void | Promise<void>;
};

type Props = {
  accept: string;
  disabled: boolean;
  commands: ComposerAttachmentCommands;
  isNative?: boolean;
};

/**
 * Owns the composer's one attachment door across browser and native pickers.
 * Native keeps camera, gallery and Files separate because no one OS sheet
 * reaches all three; the browser's file input already spans those sources.
 */
export function ComposerAttachmentPicker({
  accept,
  disabled,
  commands,
  isNative = Capacitor.isNativePlatform(),
}: Props) {
  const inputRef = useRef<HTMLInputElement>(null);
  const [menuOpen, setMenuOpen] = useState(false);
  const label = isNative
    ? "Attach a photo, clip, recording or file"
    : "Choose photos, clips, recordings or files";

  function pickNative(source: ComposerAttachmentSource) {
    setMenuOpen(false);
    void commands.pickNative(source);
  }

  function receiveBrowserFiles(event: ChangeEvent<HTMLInputElement>) {
    const input = event.currentTarget;
    const files = Array.from(input.files ?? []);
    input.value = "";
    if (files.length) void commands.addBrowserFiles(files);
  }

  return (
    <>
      <input
        ref={inputRef}
        type="file"
        accept={accept}
        aria-label="Choose attachment files"
        multiple
        className="hidden"
        onChange={receiveBrowserFiles}
      />

      <div
        className="relative shrink-0"
        onKeyDown={(event) => {
          if (event.key === "Escape" && menuOpen) {
            event.stopPropagation();
            setMenuOpen(false);
          }
        }}
      >
        {isNative && menuOpen && (
          <>
            <div
              role="presentation"
              className="fixed inset-0 z-20"
              onMouseDown={keepKeyboard}
              onClick={() => setMenuOpen(false)}
            />
            <div
              role="dialog"
              aria-label="Attach"
              onMouseDown={keepKeyboard}
              className="absolute bottom-full left-0 z-30 mb-1.5 w-max min-w-40 overflow-hidden rounded-panel border border-dialog-edge bg-panel shadow-[6px_6px_0_var(--dialog-shadow)] transition-[opacity,transform,translate,scale,rotate] duration-150 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none"
            >
              <MenuItem
                title="Take a photo"
                icon={<CameraIcon />}
                onSelect={() => pickNative("camera")}
              />
              <MenuItem
                title="Photos or videos"
                icon={<ImageIcon />}
                onSelect={() => pickNative("media")}
              />
              <MenuItem
                title="Files"
                icon={<ClipIcon />}
                onSelect={() => pickNative("files")}
              />
            </div>
          </>
        )}

        <ComposerButton
          onMouseDown={keepKeyboard}
          onClick={() => {
            if (isNative) {
              setMenuOpen((open) => !open);
            } else {
              inputRef.current?.click();
            }
          }}
          disabled={disabled}
          aria-haspopup={isNative ? "menu" : undefined}
          aria-expanded={isNative ? menuOpen : undefined}
          label={label}
          title={label}
        >
          <PlusIcon
            className={`size-3.5 transition-transform duration-150 motion-reduce:transition-none ${menuOpen ? "rotate-45" : ""}`}
          />
        </ComposerButton>
      </div>
    </>
  );
}
