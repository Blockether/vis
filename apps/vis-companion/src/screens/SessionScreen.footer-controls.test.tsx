// @vitest-environment jsdom
import { describe, expect, it, vi } from "vitest";
import { act, screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";

import { renderSessionScreen, subscriptionHub } from "./session-screen-harness";

const toggle = (id: string, label: string, value: string, choices: string[]) => ({
  id,
  label,
  type: "enum",
  value,
  choices,
});

describe("composer response controls", () => {
  it("keeps the footer compact while preserving the native safe area", () => {
    const { container } = renderSessionScreen();
    expect(container.querySelector("section > footer")).toHaveClass(
      "pb-[calc(0.375rem+var(--safe-bottom,env(safe-area-inset-bottom)))]",
    );
  });

  // Regression, user report: cumulative token and price totals were repeated in the
  // session composer footer even though the session-list disclosure owns those details.
  it("leaves cumulative usage out of the composer footer", () => {
    const turn = {
      turn_id: "turn-with-usage",
      request: "Count this",
      status: "completed",
      created_at: Date.now(),
      content: [],
      tokens: { input: 1_200, output: 34 },
      total_cost: 0.25,
    };
    const { container } = renderSessionScreen({
      client: {
        cachedTranscript: () => [turn],
        transcript: () => Promise.resolve([turn]),
      },
    });
    const footer = container.querySelector("section > footer");
    expect(footer).not.toHaveTextContent("1.2k→34");
    expect(footer).not.toHaveTextContent("~$0.2500");
  });
  it("orders reasoning, verbosity, then fast mode and cycles verbosity", async () => {
    const user = userEvent.setup();
    const reasoning = toggle("reasoning_level", "Reasoning effort", "balanced", [
      "low",
      "balanced",
      "deep",
    ]);
    const verbosity = toggle("verbosity", "Verbosity", "low", ["low", "medium", "high"]);
    const fast = { id: "codex_fast_mode", label: "Fast mode", type: "boolean", enabled: false };
    const setSetting = vi.fn((id: string) =>
      Promise.resolve(id === "verbosity" ? { ...verbosity, value: "medium" } : fast),
    );

    renderSessionScreen({
      client: {
        cachedDefaultModel: () => ({ provider: "openai-codex", model: "gpt-5.6" }),
        defaultModel: () => Promise.resolve({ provider: "openai-codex", model: "gpt-5.6" }),
        cachedSetting: (id: string) =>
          id === "reasoning_level" ? reasoning : id === "verbosity" ? verbosity : fast,
        setting: (id: string) =>
          Promise.resolve(id === "reasoning_level" ? reasoning : id === "verbosity" ? verbosity : fast),
        setSetting,
      },
    });

    const fastButton = await screen.findByRole("button", { name: /fast mode — off/i });
    const verbosityButton = await screen.findByRole("button", {
      name: /verbosity — low, tap for the next level/i,
    });
    const reasoningButton = screen.getByRole("button", { name: /reasoning effort — balanced/i });
    // Each chip LEADS WITH A MARK: `◇`, `≡` and `»` used to stand in for
    // one, in the body face, beside real icons.
    expect(reasoningButton).toHaveTextContent("balanced");
    expect(reasoningButton.querySelector("svg")).not.toBeNull();
    expect(verbosityButton).toHaveTextContent("low");
    expect(verbosityButton.querySelector("svg")).not.toBeNull();
    expect(fastButton).toHaveTextContent("standard");
    expect(fastButton.querySelector("svg")).not.toBeNull();
    expect(reasoningButton.compareDocumentPosition(verbosityButton) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
    expect(verbosityButton.compareDocumentPosition(fastButton) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
    await user.click(verbosityButton);

    expect(setSetting).toHaveBeenCalledWith("verbosity", "cycle");
    await waitFor(() =>
      expect(screen.getByRole("button", { name: /verbosity — medium/i })).toBeInTheDocument(),
    );
  });
  // Regression, reported session b30f87ac-f20e-4d7f-9fd2-416788d10527:
  // Fast mode was encoded as an OpenAI-only request field before routing finished.
  it("submits Fast mode as a provider-neutral turn feature", async () => {
    const user = userEvent.setup();
    const fast = { id: "codex_fast_mode", label: "Fast mode", type: "boolean", enabled: true };
    const submitTurn = vi.fn(
      (
        _sid: string,
        _request: string,
        _options?: {
          extraBody?: Record<string, unknown>;
          turnFeatures?: Record<string, boolean>;
        },
      ) => Promise.resolve({ turn_id: "turn-1", status: "running" }),
    );
    const codex = { provider: "openai-codex", model: "gpt-5.6" };

    renderSessionScreen({
      client: {
        cachedSessionModel: () => codex,
        sessionModel: () => Promise.resolve(codex),
        cachedSetting: (id: string) => (id === "codex_fast_mode" ? fast : null),
        setting: (id: string) => Promise.resolve(id === "codex_fast_mode" ? fast : null),
        submitTurn,
      },
    });

    await user.type(screen.getByRole("textbox", { name: "Message Vis" }), "hello");
    await user.click(screen.getByRole("button", { name: "Send message" }));

    await waitFor(() => expect(submitTurn).toHaveBeenCalled());
    const options = submitTurn.mock.calls[0]?.[2];
    expect(options?.extraBody).toBeUndefined();
    expect(options?.turnFeatures).toEqual({ codex_fast_mode: true });
  });

  it("does not let an initial model read overwrite a newer gateway model event", async () => {
    let resolveSessionModel!: (pref: { provider: string; model: string }) => void;
    const sessionModel = vi.fn(
      () =>
        new Promise<{ provider: string; model: string }>((resolve) => {
          resolveSessionModel = resolve;
        }),
    );
    const hub = subscriptionHub();
    const noteSessionModel = vi.fn((_sid: string, pref: unknown) => pref);
    const fast = { id: "codex_fast_mode", label: "Fast mode", type: "boolean", enabled: true };
    const codex = { provider: "openai-codex", model: "gpt-5.6" };

    renderSessionScreen({
      client: {
        cachedSessionModel: () => codex,
        sessionModel,
        noteSessionModel,
        cachedSetting: (id: string) => (id === "codex_fast_mode" ? fast : null),
        setting: (id: string) => Promise.resolve(id === "codex_fast_mode" ? fast : null),
      },
      subscriptions: hub,
    });

    expect(await screen.findByRole("button", { name: /fast mode — on/i })).toBeInTheDocument();
    hub.emit({
      type: "session.model_updated",
      provider: "anthropic-coding-plan",
      model: "claude-opus-5",
    } as never);
    await waitFor(() =>
      expect(screen.queryByRole("button", { name: /fast mode/i })).not.toBeInTheDocument(),
    );

    await act(async () => {
      resolveSessionModel(codex);
      await Promise.resolve();
    });
    expect(screen.queryByRole("button", { name: /fast mode/i })).not.toBeInTheDocument();
  });
});
