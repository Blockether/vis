// @vitest-environment jsdom
import { fireEvent, render, waitFor } from "@testing-library/react";
import { afterEach, expect, it, vi } from "vitest";
import { UserMessage } from "./ChatContent";
import { shareArtifact } from "../lib/artifact-share";

vi.mock("../lib/artifact-share", () => ({
  artifactShareVerb: () => "Save",
  shareArtifact: vi.fn(async () => "Artifact saved."),
}));
const log = { filename: "vis-diagnostics.jsonl.gz", media_type: "application/gzip", base64: "H4sIAAAAAAAC/w3KMQ6AIAxG4Z1j/LMMrtyGQNVGLMQ2LoS72/V7b+JmqUj4WGMeI1bOp3Q1LooNWi56MtK+wgR9JOarWn6Nqufm1FxYjo4VfvjpmRxNAAAA", size: 90 };
afterEach(() => { vi.unstubAllGlobals(); vi.clearAllMocks(); });

it("hands original log bytes to the existing save/share boundary", async () => {
  const blob = new Blob([Uint8Array.from(atob(log.base64), (c) => c.charCodeAt(0))], { type: log.media_type });
  vi.stubGlobal("fetch", vi.fn(async () => ({ blob: async () => blob })));
  const view = render(<UserMessage attachments={[log]}>{""}</UserMessage>);
  fireEvent.click(view.getByRole("button", { name: `Save ${log.filename}` }));
  await waitFor(() => expect(shareArtifact).toHaveBeenCalledWith(blob, log.filename, log.media_type));
  expect(view.getByRole("status")).toHaveTextContent("Artifact saved.");
});

it("keeps a failed save retryable and missing bytes visible", async () => {
  vi.stubGlobal("fetch", vi.fn(async () => { throw new Error("Read failed"); }));
  const view = render(<UserMessage attachments={[log]}>{""}</UserMessage>);
  fireEvent.click(view.getByRole("button", { name: `Save ${log.filename}` }));
  expect(await view.findByRole("status")).toHaveTextContent("Could not share file. Try again.");
  expect(view.getByRole("button", { name: `Save ${log.filename}` })).toBeEnabled();
  view.rerender(<UserMessage attachments={[{ ...log, base64: "" }]}>{""}</UserMessage>);
  expect(view.getByRole("button", { name: `Save ${log.filename}` })).toBeDisabled();
  expect(view.getByText(log.filename)).toBeVisible();
});
