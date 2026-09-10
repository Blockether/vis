// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { fireEvent, screen, waitFor } from "@testing-library/react";
import { renderSessionScreen, sessionFixture } from "./session-screen-harness";

describe("gateway-owned coding-agent name", () => {
  it("uses the session API name for historical answers and a newly submitted turn", async () => {
    renderSessionScreen({
      session: sessionFixture({ agent_name: "Ada" }),
      client: {
        transcript: () => Promise.resolve([{
          turn_id: "old", request: "old request", status: "completed",
          content: [{ id: "a", type: "prose", markdown: "Old answer" }],
        }]),
        submitTurn: () => new Promise(() => {}),
      },
      subscriptions: {
        subscribeConnection: (on: (connected: boolean) => void) => { on(true); return () => {}; },
      },
    });
    expect(await screen.findByText("Old answer")).toBeInTheDocument();
    expect(screen.getByText("Ada", { exact: true })).toBeInTheDocument();
    const box = await screen.findByLabelText("Message Ada");
    fireEvent.change(box, { target: { value: "new request" } });
    fireEvent.click(screen.getByRole("button", { name: "Send message" }));
    await waitFor(() => expect(document.querySelector('[data-live="true"]')).toHaveTextContent("Ada sent your message"));
    expect(screen.queryByText("Vis", { exact: true })).not.toBeInTheDocument();
  });
});
