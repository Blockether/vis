// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { act, fireEvent, screen, waitFor } from "@testing-library/react";
import { renderSessionScreen, sessionFixture, subscriptionHub } from "./session-screen-harness";

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
  it("updates an open transcript from another client and refreshes after reconnect", async () => {
    const hub = subscriptionHub();
    renderSessionScreen({
      session: sessionFixture({ agent_name: "Ada" }),
      subscriptions: hub,
      client: { noteSessionGoal: () => null, transcript: async () => [{
        turn_id: "old", request: "question", status: "completed",
        content: [{ id: "a", type: "prose", markdown: "Existing answer" }],
      }] },
    });
    await screen.findByText("Existing answer");
    act(() => hub.emit({ type: "session.agent_name_updated", seq: 1, agent_name: "Grace" }));
    await waitFor(() => expect(screen.getByLabelText("Message Grace")).toBeInTheDocument());
    expect(screen.getByText("Grace", { exact: true })).toBeInTheDocument();
    expect(screen.queryByText("Ada", { exact: true })).toBeNull();
    act(() => hub.emit({ type: "subscription.ready", is_live: false, agent_name: "助手" }));
    await waitFor(() => expect(screen.getByLabelText("Message 助手")).toBeInTheDocument());
    expect(screen.getByText("助手", { exact: true })).toBeInTheDocument();
  });
});
