import { describe, expect, it } from "vitest";

import sessionScreenSource from "./SessionScreen.tsx?raw";

describe("session feature boundaries", () => {
  it("leaves queued-turn interaction outside the screen orchestrator", () => {
    const leaks = [
      ["queued edit state", /\[editingQueued,\s*setEditingQueued\]/],
      ["queued row rendering", /queued\.map\(\(item/],
      ["queued update request", /\.updateQueuedTurn\(/],
      ["queued delete request", /\.deleteQueuedTurn\(/],
      ["queue resume request", /\.resumeQueue\(/],
    ]
      .filter(([, pattern]) => (pattern as RegExp).test(sessionScreenSource))
      .map(([name]) => name);

    expect(leaks).toEqual([]);
  });

  it("lets the paste editor own its unsaved draft and dialog interaction", () => {
    const leaks = [
      ["local paste editor", /function PasteEditor\(/],
      ["screen-owned paste draft", /\[editingPaste,[\s\S]*?draft:\s*string/],
      ["paste draft callback plumbing", /onDraftChange=/],
    ]
      .filter(([, pattern]) => (pattern as RegExp).test(sessionScreenSource))
      .map(([name]) => name);

    expect(leaks).toEqual([]);
  });

  it("lets the attachment picker own its platform menu and browser input", () => {
    const leaks = [
      ["attachment menu state", /\[attachMenuOpen,\s*setAttachMenuOpen\]/],
      ["browser file input", /fileInputRef/],
      ["attachment menu rendering", /aria-label="Attach"/],
    ]
      .filter(([, pattern]) => (pattern as RegExp).test(sessionScreenSource))
      .map(([name]) => name);

    expect(leaks).toEqual([]);
    expect(sessionScreenSource).toContain("<ComposerAttachmentPicker");
    expect(sessionScreenSource).toContain("commands={attachmentCommands}");
  });

  it("lets the composer payload shelf own staged content rendering", () => {
    const leaks = [
      ["pasted block shelf", /activePastes\.map\(\(paste/],
      ["attachment shelf", /attachments\.map\(\(attachment/],
      ["staged media rendering", /isVideoMediaType\(attachment\.media_type\)/],
    ]
      .filter(([, pattern]) => (pattern as RegExp).test(sessionScreenSource))
      .map(([name]) => name);

    expect(leaks).toEqual([]);
    expect(sessionScreenSource).toContain("<ComposerPayloadShelf");
    expect(sessionScreenSource).toContain("commands={payloadCommands}");
  });

  it("lets response controls own their compact option vocabulary", () => {
    const leaks = [
      ["response meta controls", /<MetaButton/],
      ["reasoning option paint", /<ReasoningIcon/],
      ["verbosity option paint", /<VerbosityIcon/],
      ["fast option paint", /<FastIcon/],
    ]
      .filter(([, pattern]) => (pattern as RegExp).test(sessionScreenSource))
      .map(([name]) => name);

    expect(leaks).toEqual([]);
    expect(sessionScreenSource).toContain("<ComposerResponseControls");
    expect(sessionScreenSource).toContain("controls={responseControls}");
  });
});
