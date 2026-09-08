// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from "vitest";
import { act, screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";

import { GatewayClient } from "../lib/gateway";
import type { RouterProvider } from "../lib/types";
import { renderSessionScreen, subscriptionHub } from "./session-screen-harness";

function routerProvider(id: string, overrides: Partial<RouterProvider> = {}): RouterProvider {
  return {
    id,
    is_managed: false,
    label: id,
    models: ["gpt-6-astra"],
    is_default: true,
    default_model: "gpt-6-astra",
    is_fallback: false,
    fallback_model: null,
    model_details: [{
      name: "gpt-6-astra",
      is_reasoning_effort_configurable: true,
      verbosity_style: "openai-text",
    }],
    ...overrides,
  };
}

function turnSubmitter() {
  let turn = 0;
  return vi.fn(
    (_sid: string, _request: string, _options?: { extraBody?: Record<string, unknown> }) =>
      Promise.resolve({ turn_id: `turn-${++turn}`, status: "running" }),
  );
}

const toggle = (id: string, label: string, value: string, choices: string[]) => ({
  id,
  label,
  type: "enum",
  value,
  choices,
});

afterEach(() => vi.unstubAllGlobals());
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
    const fleet = [routerProvider("openai-codex")];
    renderSessionScreen({
      client: {
        cachedDefaultModel: () => ({ provider: "openai-codex", model: "gpt-6-astra" }),
        defaultModel: () => Promise.resolve({ provider: "openai-codex", model: "gpt-6-astra" }),
        cachedRouter: () => fleet,
        router: () => Promise.resolve(fleet),
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

  // Regression: the chip was provider-gated and its value never reached submitTurn.
  it.each(["openai-codex", "github-copilot", "custom-responses"])(
    "sends the chosen Astra verbosity from %s on immediate and queued turns",
    async (provider) => {
      const user = userEvent.setup();
      const pref = { provider, model: "gpt-6-astra" };
      let verbosity = toggle("verbosity", "Verbosity", "low", ["low", "medium", "high"]);
      const submitTurn = turnSubmitter();
      const fleet = [routerProvider(provider)];
      renderSessionScreen({
        client: {
          cachedSessionModel: () => pref,
          sessionModel: () => Promise.resolve(pref),
          cachedRouter: () => fleet,
          router: () => Promise.resolve(fleet),
          cachedSetting: (id: string) => id === "verbosity" ? verbosity : null,
          setting: (id: string) => Promise.resolve(id === "verbosity" ? verbosity : null),
          setSetting: vi.fn(async () => {
            const choices = verbosity.choices;
            verbosity = {
              ...verbosity,
              value: choices[(choices.indexOf(verbosity.value) + 1) % choices.length]!,
            };
            return verbosity;
          }),
          submitTurn,
        },
      });
      for (const [index, level] of ["low", "medium", "high", "low"].entries()) {
        const chip = await screen.findByRole("button", {
          name: `Verbosity — ${level}, tap for the next level`,
        });
        await user.type(screen.getByRole("textbox", { name: "Message Vis" }), `answer ${index}`);
        await user.click(screen.getByRole("button", { name: index ? "Queue message" : "Send message" }));
        await waitFor(() => expect(submitTurn).toHaveBeenCalledTimes(index + 1));
        expect(submitTurn.mock.calls[index]?.[2]?.extraBody).toEqual({ text: { verbosity: level } });
        await user.click(chip);
      }
    },
  );

  it.each([null, undefined])(
    "hides verbosity and omits its request field when wire capability is %s, even on Codex",
    async (style) => {
      const user = userEvent.setup();
      const pref = { provider: "openai-codex", model: "gpt-6-astra" };
      const verbosity = toggle("verbosity", "Verbosity", "high", ["low", "medium", "high"]);
      const fleet = [routerProvider(pref.provider, {
        model_details: style === undefined ? undefined : [{
          name: pref.model,
          is_reasoning_effort_configurable: true,
          verbosity_style: style,
        }],
      })];
      const submitTurn = turnSubmitter();
      renderSessionScreen({
        client: {
          cachedSessionModel: () => pref,
          sessionModel: () => Promise.resolve(pref),
          cachedRouter: () => fleet,
          router: () => Promise.resolve(fleet),
          cachedSetting: (id: string) => id === "verbosity" ? verbosity : null,
          setting: (id: string) => Promise.resolve(id === "verbosity" ? verbosity : null),
          submitTurn,
        },
      });
      expect(screen.queryByRole("button", { name: /verbosity/i })).not.toBeInTheDocument();
      await user.type(screen.getByRole("textbox", { name: "Message Vis" }), "hello");
      await user.click(screen.getByRole("button", { name: "Send message" }));
      await waitFor(() => expect(submitTurn).toHaveBeenCalledOnce());
      expect(submitTurn.mock.calls[0]?.[2]?.extraBody).toBeUndefined();
      expect(screen.queryByRole("button", { name: /verbosity/i })).not.toBeInTheDocument();
    },
  );

  it("uses the session model, not the global default, and follows model changes", async () => {
    const user = userEvent.setup();
    const hub = subscriptionHub();
    const verbosity = toggle("verbosity", "Verbosity", "high", ["low", "medium", "high"]);
    const defaultPref = { provider: "openai-codex", model: "gpt-6-astra" };
    const sessionPref = { provider: "github-copilot", model: "claude-opus-5" };
    const fleet = [routerProvider(defaultPref.provider), routerProvider(sessionPref.provider, {
      is_default: false,
      models: ["gpt-6-astra", "claude-opus-5"],
      model_details: [
        { name: "gpt-6-astra", is_reasoning_effort_configurable: true, verbosity_style: "openai-text" },
        { name: "claude-opus-5", is_reasoning_effort_configurable: true, verbosity_style: null },
      ],
    })];
    const submitTurn = turnSubmitter();
    renderSessionScreen({
      client: {
        cachedSessionModel: () => sessionPref,
        sessionModel: () => Promise.resolve(sessionPref),
        noteSessionModel: (_sid: string, pref: unknown) => pref,
        cachedDefaultModel: () => defaultPref,
        defaultModel: () => Promise.resolve(defaultPref),
        cachedRouter: () => fleet,
        router: () => Promise.resolve(fleet),
        cachedSetting: (id: string) => id === "verbosity" ? verbosity : null,
        setting: (id: string) => Promise.resolve(id === "verbosity" ? verbosity : null),
        submitTurn,
      },
      subscriptions: hub,
    });
    for (const [index, model] of ["claude-opus-5", "gpt-6-astra", "claude-opus-5"].entries()) {
      if (index) {
        act(() => hub.emit({ type: "session.model_updated", provider: sessionPref.provider, model } as never));
      }
      const expected = model === "gpt-6-astra" ? { text: { verbosity: "high" } } : undefined;
      await waitFor(() =>
        expect(Boolean(screen.queryByRole("button", { name: /verbosity — high/i }))).toBe(Boolean(expected)),
      );
      await user.type(screen.getByRole("textbox", { name: "Message Vis" }), `answer ${index}`);
      await user.click(screen.getByRole("button", { name: index ? "Queue message" : "Send message" }));
      await waitFor(() => expect(submitTurn).toHaveBeenCalledTimes(index + 1));
      expect(submitTurn.mock.calls[index]?.[2]?.extraBody).toEqual(expected);
    }
  });

  // Regression, session 75371ea8-06af-4853-9d13-1056672df5db: a failed
  // initial read left Astra's verbosity absent after the connection recovered.
  it.each([
    ["/v1/settings/verbosity", "reconnect"],
    ["/v1/router", "reconnect"],
    ["/v1/settings/verbosity", "wake"],
    ["/v1/router", "wake"],
    ["/v1/settings/verbosity", "session switch"],
    ["/v1/router", "session switch"],
  ])("recovers %s on %s through the real gateway client", async (failedPath, recovery) => {
    const user = userEvent.setup();
    const pref = { provider: "github-copilot-enterprise", model: "gpt-6-astra" };
    const fleet = [routerProvider(pref.provider)];
    let verbosity = toggle("verbosity", "Verbosity", "low", ["low", "medium", "high"]);
    let offline = true;
    const fetcher = vi.fn(async (url: string, init?: RequestInit) => {
      const path = new URL(url).pathname;
      if (offline && path.endsWith(failedPath)) throw new TypeError("Network unavailable");
      if (path.endsWith("/v1/router")) return Response.json({ providers: fleet });
      if (path.endsWith("/v1/settings") && init?.method === "POST") {
        expect(JSON.parse(String(init.body))).toMatchObject({ id: "verbosity", action: "cycle" });
        verbosity = { ...verbosity, value: "medium" };
      }
      return Response.json(path.endsWith("verbosity") || init?.method === "POST" ? verbosity : null);
    });
    vi.stubGlobal("fetch", fetcher);
    const gateway = new GatewayClient({ url: `http://gateway.example.com/${recovery}${failedPath}` });
    const submitTurn = turnSubmitter();
    let connectionState = false;
    const connectionListeners = new Set<(connected: boolean) => void>();
    const view = renderSessionScreen({
      client: {
        cachedSessionModel: () => pref,
        sessionModel: () => Promise.resolve(pref),
        cachedRouter: gateway.cachedRouter.bind(gateway),
        router: gateway.router.bind(gateway),
        defaultModel: gateway.defaultModel.bind(gateway),
        cachedSetting: gateway.cachedSetting.bind(gateway),
        setting: gateway.setting.bind(gateway),
        setSetting: gateway.setSetting.bind(gateway),
        submitTurn,
      },
      subscriptions: {
        subscribeConnection: (listener: (connected: boolean) => void) => {
          connectionListeners.add(listener);
          listener(connectionState);
          return () => connectionListeners.delete(listener);
        },
      },
    });
    await act(async () => {});
    expect(fetcher.mock.calls.some(([url]) => url.endsWith(failedPath))).toBe(true);
    expect(screen.queryByRole("button", { name: /verbosity/i })).not.toBeInTheDocument();
    offline = false;
    act(() => {
      if (recovery === "reconnect") {
        connectionState = true;
        for (const listener of [...connectionListeners]) listener(true);
      } else if (recovery === "session switch") {
        view.rerenderSession("s2");
      } else {
        window.dispatchEvent(new Event("online"));
      }
    });
    const chip = await screen.findByRole("button", { name: /verbosity — low/i });
    await user.click(chip);
    await screen.findByRole("button", { name: /verbosity — medium/i });
    // A later failed refresh must retain the recovered, chosen value.
    gateway.invalidateRouter();
    offline = true;
    const attempts = fetcher.mock.calls.filter(([url]) => url.endsWith(failedPath)).length;
    act(() => window.dispatchEvent(new Event("online")));
    await waitFor(() => expect(
      fetcher.mock.calls.filter(([url]) => url.endsWith(failedPath)).length,
    ).toBeGreaterThan(attempts));
    expect(screen.getByRole("button", { name: /verbosity — medium/i })).toBeInTheDocument();
    await user.type(screen.getByRole("textbox", { name: "Message Vis" }), "hello");
    await user.click(screen.getByRole("button", { name: "Send message" }));
    await waitFor(() => expect(submitTurn).toHaveBeenCalledOnce());
    expect(submitTurn.mock.calls[0]?.[2]?.extraBody).toEqual({ text: { verbosity: "medium" } });
  });

  it("enables verbosity when an uncached fleet arrives for the default model", async () => {
    let resolveRouter!: (fleet: RouterProvider[]) => void;
    const router = new Promise<RouterProvider[]>((resolve) => { resolveRouter = resolve; });
    const pref = { provider: "github-copilot", model: "gpt-6-astra" };
    const verbosity = toggle("verbosity", "Verbosity", "high", ["low", "medium", "high"]);
    renderSessionScreen({
      client: {
        cachedDefaultModel: () => pref,
        defaultModel: () => Promise.resolve(pref),
        router: () => router,
        cachedSetting: (id: string) => id === "verbosity" ? verbosity : null,
        setting: (id: string) => Promise.resolve(id === "verbosity" ? verbosity : null),
      },
    });
    expect(screen.queryByRole("button", { name: /verbosity/i })).not.toBeInTheDocument();
    await act(async () => { resolveRouter([routerProvider(pref.provider)]); });
    expect(await screen.findByRole("button", { name: /verbosity — high/i })).toBeInTheDocument();
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
