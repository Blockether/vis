// @vitest-environment jsdom
import { act, screen, waitFor } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";

import type * as GatewayModule from "./lib/gateway";

type IncompatibleListener = Parameters<
  typeof GatewayModule.onGatewayIncompatible
>[0];

const gatewayEvent = vi.hoisted(() => ({
  listener: null as IncompatibleListener | null,
  registrations: 0,
}));

vi.mock("./lib/gateway", async (importOriginal) => {
  const actual = await importOriginal<typeof GatewayModule>();
  return {
    ...actual,
    onGatewayIncompatible(listener: IncompatibleListener) {
      gatewayEvent.listener = listener;
      gatewayEvent.registrations += 1;
      return () => {
        if (gatewayEvent.listener === listener) gatewayEvent.listener = null;
      };
    },
  };
});

import { renderApp } from "./app-harness";
import { APP_MIN_GATEWAY_PROTOCOL, APP_PROTOCOL } from "./lib/compat";

let restore = () => {};
afterEach(() => {
  restore();
  restore = () => {};
  gatewayEvent.listener = null;
  gatewayEvent.registrations = 0;
});

describe("the running app compatibility listener", () => {
  it("keeps a settled refusal without reopening negotiation", async () => {
    const view = renderApp({
      machines: [
        {
          routes: {
            "/healthz": {
              status: "ok",
              protocol: {
                protocol: APP_PROTOCOL,
                min_client: APP_PROTOCOL + 1,
                min_gateway: APP_MIN_GATEWAY_PROTOCOL,
                version: "0.0.0-test",
              },
            },
          },
        },
      ],
    });
    restore = () => {
      view.unmount();
      view.restore();
    };

    await waitFor(() =>
      expect(screen.getByText("Update this app")).toBeTruthy(),
    );
    expect(gatewayEvent.registrations).toBe(1);
    expect(gatewayEvent.listener).not.toBeNull();

    const healthReads = () =>
      view.requests.filter((href) => new URL(href).pathname === "/healthz")
        .length;
    const before = healthReads();

    await act(async () => {
      gatewayEvent.listener?.(undefined as never);
      await Promise.resolve();
    });

    expect(healthReads()).toBe(before);
  });
});
