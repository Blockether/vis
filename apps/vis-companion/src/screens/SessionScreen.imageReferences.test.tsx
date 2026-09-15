// @vitest-environment jsdom
import { act, fireEvent, screen, waitFor } from "@testing-library/react";
import { beforeEach, expect, it, vi } from "vitest";
import * as attachments from "../lib/attachments";
import {
  clearDraftMessage,
  draftMessageKey,
  peekDraftMessage,
} from "../lib/draft-messages";
import { renderSessionScreen, subscriptionHub } from "./session-screen-harness";
import type { SseEvent } from "../lib/types";

const photo = (id = "image-reference") => ({
  id,
  filename: `${id}.png`,
  media_type: "image/png",
  base64: "data:image/png;base64,AAAA",
  previewUrl: "data:image/png;base64,AAAA",
  size: 3,
});
let count = 0;
beforeEach(() => {
  clearDraftMessage(draftMessageKey("http://gateway.example.com", "s1"));
  clearDraftMessage(
    draftMessageKey("http://gateway.example.com", "other-session"),
  );
  count = 0;
  vi.spyOn(attachments, "attachmentsFromFiles").mockImplementation(
    async () => ({
      attachments: [photo(`photo-${++count}`)],
      rejected: [],
    }),
  );
});
const editor = () =>
  screen.getByLabelText("Message Vis") as HTMLTextAreaElement;
async function pasteImage() {
  fireEvent.paste(editor(), {
    clipboardData: {
      files: [new File(["image"], "photo.png", { type: "image/png" })],
      items: [],
      getData: () => "",
    },
  });
  await waitFor(() =>
    expect(
      screen.getByRole("button", { name: `Remove photo-${count}.png` }),
    ).toBeInTheDocument(),
  );
}

it("inserts an owned image reference at the caret and removes its shelf item with the last token", async () => {
  renderSessionScreen();
  fireEvent.input(editor(), { target: { value: "Compare  please" } });
  editor().setSelectionRange(8, 8);
  await pasteImage();
  expect(editor().value).toBe("Compare [IMAGE #1] please");
  expect(screen.getByText("[IMAGE #1]")).toBeInTheDocument();
  fireEvent.input(editor(), { target: { value: "Compare  please" } });
  expect(
    screen.queryByRole("button", { name: "Remove photo-1.png" }),
  ).not.toBeInTheDocument();
});

it("keeps duplicate tokens backed until the final one is deleted, then restarts numbering", async () => {
  renderSessionScreen();
  await pasteImage();
  fireEvent.input(editor(), { target: { value: "[IMAGE #1] and [IMAGE #1]" } });
  editor().setSelectionRange(10, 10);
  fireEvent.keyDown(editor(), { key: "Backspace" });
  expect(editor().value).toBe(" and [IMAGE #1]");
  expect(
    screen.getByRole("button", { name: "Remove photo-1.png" }),
  ).toBeInTheDocument();
  fireEvent.input(editor(), { target: { value: " and " } });
  await pasteImage();
  expect(editor().value).toBe(" and [IMAGE #1]");
});

it("restarts after removing every image from the shelf, including a reopened draft", async () => {
  const view = renderSessionScreen();
  fireEvent.input(editor(), { target: { value: "Compare " } });
  await pasteImage();
  await pasteImage();
  fireEvent.click(screen.getByRole("button", { name: "Remove photo-1.png" }));
  expect(editor().value).toBe("Compare  [IMAGE #2]");
  fireEvent.click(screen.getByRole("button", { name: "Remove photo-2.png" }));
  expect(editor().value).toBe("Compare  ");
  expect(
    peekDraftMessage(draftMessageKey("http://gateway.example.com", "s1"))
      .imageCounter,
  ).toBe(0);
  view.rerenderSession("other-session");
  view.rerenderSession("s1");
  await pasteImage();
  expect(editor().value).toBe("Compare  [IMAGE #1]");
});

it.each(["Backspace", "Delete"])(
  "restarts after removing the final image with %s",
  async (key) => {
    renderSessionScreen();
    await pasteImage();
    const caret = key === "Backspace" ? editor().value.length : 0;
    editor().setSelectionRange(caret, caret);
    fireEvent.keyDown(editor(), { key });
    expect(editor().value).toBe("");
    await pasteImage();
    expect(editor().value).toBe("[IMAGE #1]");
  },
);

it("removes all exact shelf references without normalizing authored spaces", async () => {
  renderSessionScreen();
  await pasteImage();
  fireEvent.input(editor(), {
    target: { value: "  [IMAGE #1]  x [IMAGE #10] [IMAGE #1]  " },
  });
  fireEvent.click(screen.getByRole("button", { name: "Remove photo-1.png" }));
  expect(editor().value).toBe("    x [IMAGE #10]   ");
  await pasteImage();
  expect(editor().value).toBe("    x [IMAGE #10]   [IMAGE #11]");
});

it("never binds an authored literal number and sends the original filename with the reference", async () => {
  const submitTurn = vi.fn(async () => ({
    turn_id: "image-send",
    status: "running",
  }));
  renderSessionScreen({ client: { submitTurn } });
  fireEvent.input(editor(), { target: { value: "Literal [IMAGE #4] and" } });
  await pasteImage();
  fireEvent.click(screen.getByRole("button", { name: "Send message" }));
  expect(submitTurn).toHaveBeenCalledWith(
    "s1",
    "Literal [IMAGE #4] and [IMAGE #5]",
    expect.objectContaining({
      attachments: [
        expect.objectContaining({
          filename: "photo-1.png",
          reference: "[IMAGE #5]",
          base64: photo().base64,
        }),
      ],
    }),
  );
});

it("ignores an image whose intake finishes after a session switch", async () => {
  let finish!: (value: attachments.PickAttachmentResult) => void;
  vi.mocked(attachments.attachmentsFromFiles).mockReturnValue(
    new Promise((resolve) => {
      finish = resolve;
    }),
  );
  const view = renderSessionScreen();
  fireEvent.paste(editor(), {
    clipboardData: {
      files: [new File(["image"], "photo.png", { type: "image/png" })],
      items: [],
      getData: () => "",
    },
  });
  view.rerenderSession("other-session");
  await act(async () => finish({ attachments: [photo()], rejected: [] }));
  expect(editor().value).toBe("");
  expect(
    screen.queryByRole("button", { name: "Remove image-reference.png" }),
  ).not.toBeInTheDocument();
});

it("persists the highest image number after removing a payload and reopens with labels", async () => {
  const view = renderSessionScreen();
  await pasteImage();
  await pasteImage();
  fireEvent.click(screen.getByRole("button", { name: "Remove photo-2.png" }));
  expect(
    peekDraftMessage(draftMessageKey("http://gateway.example.com", "s1"))
      .imageCounter,
  ).toBe(2);
  view.rerenderSession("other-session");
  view.rerenderSession("s1");
  await pasteImage();
  expect(editor().value).toContain("[IMAGE #1]");
  expect(editor().value).toContain("[IMAGE #3]");
});

it("restores the same reference after a failed send and retries with the same bytes", async () => {
  const submitTurn = vi
    .fn()
    .mockRejectedValueOnce(new Error("Unavailable"))
    .mockResolvedValue({ turn_id: "retry", status: "running" });
  renderSessionScreen({ client: { submitTurn } });
  await pasteImage();
  fireEvent.click(screen.getByRole("button", { name: "Send message" }));
  await waitFor(() => expect(editor().value).toBe("[IMAGE #1]"));
  expect(
    screen.getByRole("button", { name: "Remove photo-1.png" }),
  ).toBeInTheDocument();
  fireEvent.click(screen.getByRole("button", { name: "Send message" }));
  await waitFor(() => expect(submitTurn).toHaveBeenCalledTimes(2));
  expect(submitTurn.mock.calls[1][2].attachments).toEqual(
    submitTurn.mock.calls[0][2].attachments,
  );
});

it("does not send image bytes after a native deletion that has not emitted input yet", async () => {
  const submitTurn = vi
    .fn()
    .mockResolvedValue({ turn_id: "native-delete", status: "running" });
  renderSessionScreen({ client: { submitTurn } });
  await pasteImage();
  editor().value = "Only words";
  fireEvent.click(screen.getByRole("button", { name: "Send message" }));
  expect(submitTurn.mock.calls[0][2].attachments).toEqual([]);
});

it("uses the same reference path for browser file selection and leaves recordings unnumbered", async () => {
  vi.mocked(attachments.attachmentsFromFiles).mockResolvedValueOnce({
    attachments: [
      photo("chosen"),
      { ...photo("recording"), filename: "memo.wav", media_type: "audio/wav" },
    ],
    rejected: [],
  });
  renderSessionScreen();
  fireEvent.change(screen.getByLabelText("Choose attachment files"), {
    target: {
      files: [new File(["image"], "chosen.png", { type: "image/png" })],
    },
  });
  await waitFor(() => expect(editor().value).toBe("[IMAGE #1]"));
  expect(
    screen.getByRole("button", { name: "Remove memo.wav" }),
  ).toBeInTheDocument();
  fireEvent.input(editor(), { target: { value: "" } });
  expect(
    screen.getByRole("button", { name: "Remove memo.wav" }),
  ).toBeInTheDocument();
  await pasteImage();
  expect(editor().value).toBe("[IMAGE #1]");
  expect(
    screen.getByRole("button", { name: "Remove memo.wav" }),
  ).toBeInTheDocument();
});

it("restores queued image references with their bytes after cancellation", async () => {
  const events = subscriptionHub();
  const submitTurn = vi
    .fn()
    .mockResolvedValue({ turn_id: "queued-image", status: "queued" });
  renderSessionScreen({ client: { submitTurn }, subscriptions: events });
  await pasteImage();
  fireEvent.click(screen.getByRole("button", { name: "Send message" }));
  await waitFor(() => expect(submitTurn).toHaveBeenCalledOnce());
  await act(async () => {
    events.emit({
      type: "turn.queued.deleted",
      seq: 100,
      turn_id: "queued-image",
      reason: "cancelled",
      request: "[IMAGE #1]",
    } as unknown as SseEvent);
  });
  await waitFor(() => expect(editor().value).toBe("[IMAGE #1]"));
  expect(
    screen.getByRole("button", { name: "Remove photo-1.png" }),
  ).toBeInTheDocument();
});

it("recovers a failed image send without discarding a newer composed message", async () => {
  let fail!: (reason: Error) => void;
  const submitTurn = vi.fn().mockReturnValue(
    new Promise((_resolve, reject) => {
      fail = reject;
    }),
  );
  renderSessionScreen({ client: { submitTurn } });
  await pasteImage();
  fireEvent.click(screen.getByRole("button", { name: "Send message" }));
  fireEvent.input(editor(), { target: { value: "Newer work " } });
  await pasteImage();
  await act(async () => fail(new Error("Unavailable")));
  expect(editor().value).toBe("[IMAGE #1]\n\nNewer work [IMAGE #2]");
  expect(
    screen.getByRole("button", { name: "Remove photo-1.png" }),
  ).toBeInTheDocument();
  expect(
    screen.getByRole("button", { name: "Remove photo-2.png" }),
  ).toBeInTheDocument();
});

it.each(["running", "queued"])(
  "restarts image numbering for a fresh composer after a %s submission",
  async (status) => {
    const submitTurn = vi
      .fn()
      .mockResolvedValue({ turn_id: "accepted", status });
    renderSessionScreen({ client: { submitTurn } });
    await pasteImage();
    fireEvent.click(screen.getByRole("button", { name: "Send message" }));
    await act(async () => {});
    expect(editor().value).toBe("");
    await pasteImage();
    expect(editor().value).toBe("[IMAGE #1]");
  },
);

it("does not reset numbering after success when newer authored work already exists", async () => {
  let accept!: (value: { turn_id: string; status: string }) => void;
  const submitTurn = vi.fn().mockReturnValue(
    new Promise((resolve) => {
      accept = resolve;
    }),
  );
  renderSessionScreen({ client: { submitTurn } });
  await pasteImage();
  fireEvent.click(screen.getByRole("button", { name: "Send message" }));
  await pasteImage();
  await act(async () => accept({ turn_id: "accepted", status: "running" }));
  await pasteImage();
  expect(editor().value).toBe("[IMAGE #2] [IMAGE #3]");
});

it("rebases a cancelled queued image instead of binding an authored literal", async () => {
  const events = subscriptionHub();
  const submitTurn = vi
    .fn()
    .mockResolvedValue({ turn_id: "queued-image", status: "queued" });
  renderSessionScreen({ client: { submitTurn }, subscriptions: events });
  await pasteImage();
  fireEvent.click(screen.getByRole("button", { name: "Send message" }));
  await act(async () => {});
  fireEvent.input(editor(), { target: { value: "Literal [IMAGE #1]" } });
  await act(async () => {
    events.emit({
      type: "turn.queued.deleted",
      seq: 100,
      turn_id: "queued-image",
      reason: "cancelled",
      request: "[IMAGE #1]",
    } as unknown as SseEvent);
  });
  await waitFor(() =>
    expect(editor().value).toBe("Literal [IMAGE #1]\n\n[IMAGE #2]"),
  );
  fireEvent.click(screen.getByRole("button", { name: "Remove photo-1.png" }));
  expect(editor().value).toBe("Literal [IMAGE #1]\n\n");
});

it("keeps a newer image number when restoring a queued image from an earlier message", async () => {
  const events = subscriptionHub();
  const submitTurn = vi
    .fn()
    .mockResolvedValue({ turn_id: "queued-image", status: "queued" });
  renderSessionScreen({ client: { submitTurn }, subscriptions: events });
  await pasteImage();
  fireEvent.click(screen.getByRole("button", { name: "Send message" }));
  await act(async () => {});
  await pasteImage();
  await act(async () => {
    events.emit({
      type: "turn.queued.deleted",
      seq: 100,
      turn_id: "queued-image",
      reason: "cancelled",
      request: "[IMAGE #1]",
    } as unknown as SseEvent);
  });
  await waitFor(() => expect(editor().value).toBe("[IMAGE #1]\n\n[IMAGE #2]"));
  fireEvent.click(screen.getByRole("button", { name: "Remove photo-1.png" }));
  expect(editor().value).toBe("[IMAGE #1]\n\n");
  expect(
    screen.getByRole("button", { name: "Remove photo-2.png" }),
  ).toBeInTheDocument();
});

it("rebases a failed send around a newer unowned literal", async () => {
  let fail!: (reason: Error) => void;
  const submitTurn = vi.fn().mockReturnValue(
    new Promise((_resolve, reject) => {
      fail = reject;
    }),
  );
  renderSessionScreen({ client: { submitTurn } });
  await pasteImage();
  fireEvent.click(screen.getByRole("button", { name: "Send message" }));
  fireEvent.input(editor(), { target: { value: "Literal [IMAGE #1]" } });
  await act(async () => fail(new Error("Unavailable")));
  expect(editor().value).toBe("[IMAGE #2]\n\nLiteral [IMAGE #1]");
  fireEvent.click(screen.getByRole("button", { name: "Remove photo-1.png" }));
  expect(editor().value).toBe("\n\nLiteral [IMAGE #1]");
});
