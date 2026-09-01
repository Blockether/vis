// @vitest-environment jsdom
import {
  cleanup,
  fireEvent,
  render,
  screen,
  waitFor,
} from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";

import type { GatewayClient } from "../lib/gateway";
import type { QueuedTurn } from "../lib/types";
import { QueuedTurnsTray } from "./QueuedTurnsTray";

afterEach(cleanup);

const queued: QueuedTurn[] = [
  {
    turnId: "turn-2",
    request: "Inspect the release manifest",
    preview: "Inspect the release manifest",
    attachments: [
      { filename: "manifest.png", mediaType: "image/png", sizeLabel: "24 KB" },
    ],
  },
];

function gateway(methods: Partial<GatewayClient> = {}): GatewayClient {
  return {
    updateQueuedTurn: vi.fn().mockResolvedValue(undefined),
    deleteQueuedTurn: vi.fn().mockResolvedValue(undefined),
    resumeQueue: vi.fn().mockResolvedValue(undefined),
    ...methods,
  } as unknown as GatewayClient;
}

describe("queued turns tray", () => {
  it("edits through the gateway without rewriting its row optimistically", async () => {
    const client = gateway();
    render(
      <QueuedTurnsTray
        client={client}
        sid="session-1"
        queued={queued}
        paused={null}
        onError={() => {}}
      />,
    );

    fireEvent.click(screen.getByTitle("Tap to edit"));
    const input = screen.getByLabelText("Edit queued message 1");
    fireEvent.change(input, {
      target: { value: "Inspect the signed release manifest" },
    });
    fireEvent.keyDown(input, { key: "Enter" });

    await waitFor(() =>
      expect(client.updateQueuedTurn).toHaveBeenCalledWith(
        "session-1",
        "turn-2",
        "Inspect the signed release manifest",
      ),
    );
    expect(screen.getByText("Inspect the release manifest")).toBeTruthy();
    expect(screen.getByText("manifest.png")).toBeTruthy();
  });

  it("owns removal and paused-queue recovery, including failures", async () => {
    const failure = new Error("queue changed first");
    const client = gateway({
      deleteQueuedTurn: vi.fn().mockRejectedValue(failure),
    });
    const onError = vi.fn();
    render(
      <QueuedTurnsTray
        client={client}
        sid="session-1"
        queued={queued}
        paused={{ held: 2, reason: "turn_failed" }}
        onError={onError}
      />,
    );

    expect(screen.getByText("2 held · turn failed")).toBeTruthy();
    fireEvent.click(screen.getByRole("button", { name: "Continue queue" }));
    expect(client.resumeQueue).toHaveBeenCalledWith("session-1");

    fireEvent.click(
      screen.getByRole("button", { name: "Remove queued message 1" }),
    );
    expect(client.deleteQueuedTurn).toHaveBeenCalledWith("session-1", "turn-2");
    expect(screen.getByText("Inspect the release manifest")).toBeTruthy();
    await waitFor(() =>
      expect(onError).toHaveBeenCalledWith("queue changed first"),
    );
  });
  it("keeps a long queue in a named keyboard-scrollable region", () => {
    render(
      <QueuedTurnsTray
        client={gateway()}
        sid="session-1"
        queued={Array.from({ length: 12 }, (_, index) => ({
          ...queued[0],
          turnId: `turn-${index}`,
          attachments: [],
        }))}
        paused={null}
        onError={() => {}}
      />,
    );

    const queue = screen.getByRole("region", { name: "Queued messages" });
    expect(queue.tabIndex).toBe(0);
    expect(screen.getAllByRole("listitem")).toHaveLength(12);
  });
});
