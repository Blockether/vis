// @vitest-environment jsdom
import { screen, waitFor, within } from "@testing-library/react";
import { afterEach, describe, expect, it } from "vitest";

import { renderApp } from "./app-harness";
import { listSession } from "./screens/sessions-screen-harness";

let restore = () => {};
afterEach(() => restore());

// Regression, user report: making the second paired machine primary left the first
// paired machine first and selected in the sessions list.
describe("the primary machine owns the first sessions scope", () => {
  it("puts the primary machine first and selects its sessions", async () => {
    const view = renderApp({
      machines: [
        { label: "alpha", sessions: [listSession({ id: "a1", title: "Alpha session" })] },
        { label: "beta", sessions: [listSession({ id: "b1", title: "Beta session" })] },
      ],
      primary: 1,
    });
    restore = view.restore;

    const strip = within(await screen.findByLabelText("Machines"));
    await waitFor(() =>
      expect(
        strip
          .getAllByRole("button")
          .map((button) => button.getAttribute("aria-label") ?? button.textContent),
      ).toEqual(["beta", "alpha"]),
    );

    expect(strip.getByRole("button", { name: /^beta/ }).getAttribute("aria-pressed")).toBe("true");
    expect(await screen.findByText("Beta session")).toBeTruthy();
    expect(screen.queryByText("Alpha session")).toBeNull();
  });
});
