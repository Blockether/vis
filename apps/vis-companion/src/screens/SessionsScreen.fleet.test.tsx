// @vitest-environment jsdom
import { screen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { afterEach, describe, expect, it } from "vitest";

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

let restore = () => {};
afterEach(() => restore());

const fleet = () => [
  {
    label: "alpha",
    sessions: [listSession({ id: "a1", title: "First", workspace: { root: "/w/one" } })],
  },
  {
    label: "beta",
    sessions: [listSession({ id: "b1", title: "Second", workspace: { root: "/w/two" } })],
  },
];

const section = (machine: string) => screen.getByLabelText(`${machine} projects`);

/** The palette name a machine is wearing, off whatever class carries the hue. */
const hue = (element: Element | null | undefined, property: "border" | "bg") =>
  element?.className.match(new RegExp(`${property}-machine-([a-z]+)`))?.[1] ?? null;

const railHue = (machine: string) =>
  hue(section(machine).querySelector("[class*='border-machine-']"), "border");

const named = (pattern: RegExp) =>
  screen.queryAllByRole("button").filter((button) => {
    const name = button.getAttribute("aria-label") ?? button.textContent ?? "";
    return pattern.test(name);
  });

// Regression, user report (paraphrased: "all the machine rails have the same colour, I
// only ever see one"): a machine's hue, rail and section were per machine, but the scope
// was always exactly ONE machine — so a fleet of six painted a single colour and the only
// place several rails could coexist was unreachable. `All` is that view: one named
// section per machine, each under its own rail.
describe("the All view is a fleet of separate machines", () => {
  it("stacks one section per machine, each under its own rail", async () => {
    const view = renderSessionsScreen({ machines: fleet() });
    restore = view.restore;
    await screen.findByText("First");

    expect(section("alpha").contains(screen.getByText("First"))).toBe(true);
    expect(section("beta").contains(screen.getByText("Second"))).toBe(true);
    // Two rails, two hues: where one computer ends is a colour change.
    expect(railHue("alpha")).toBeTruthy();
    expect(railHue("beta")).toBeTruthy();
    expect(railHue("alpha")).not.toBe(railHue("beta"));
  });

  it("wears the same hue on the tab, the band and the rail of one machine", async () => {
    const view = renderSessionsScreen({ machines: fleet() });
    restore = view.restore;
    await screen.findByText("First");

    const tab = within(screen.getByLabelText("Machines")).getByRole("button", {
      name: /^beta/,
    });
    const band = within(section("beta")).getByText("beta").closest("header")!;
    expect(hue(tab.querySelector("[class*='bg-machine-']"), "bg")).toBe(railHue("beta"));
    expect(hue(band.querySelector("[class*='bg-machine-']"), "bg")).toBe(railHue("beta"));
  });

  // The band the fleet view needs is the one the SCOPED view was reported for: with the
  // strip directly above naming the machine, a band repeating it is a second header one
  // hairline from the project's. It exists where the name is not otherwise on screen.
  it("names a machine on its section only while the fleet is on screen", async () => {
    const view = renderSessionsScreen({ machines: fleet() });
    restore = view.restore;
    await screen.findByText("First");
    expect(within(section("alpha")).getByText("alpha")).toBeTruthy();

    await userEvent.click(
      within(screen.getByLabelText("Machines")).getByRole("button", { name: /^alpha/ }),
    );
    await waitFor(() => expect(screen.queryByText("Second")).toBeNull());
    expect(within(section("alpha")).queryByText("alpha")).toBeNull();
  });

  // A verb that had to ask which of two computers it meant would be the chooser the
  // switch exists to abolish, so in the fleet view it stands on each machine's own band.
  it("gives every machine its own New project verb, and one when scoped", async () => {
    const view = renderSessionsScreen({ machines: fleet() });
    restore = view.restore;
    await screen.findByText("First");

    expect(named(/^New project on/).map((button) => button.getAttribute("aria-label"))).toEqual([
      "New project on alpha",
      "New project on beta",
    ]);
    expect(section("beta").contains(named(/^New project on beta$/)[0]!)).toBe(true);

    await userEvent.click(
      within(screen.getByLabelText("Machines")).getByRole("button", { name: /^beta/ }),
    );
    await waitFor(() => expect(named(/^New project on/)).toHaveLength(1));
    // Scoped, the verb is back above the card, outside the list.
    expect(section("beta").contains(named(/^New project on beta$/)[0]!)).toBe(false);
  });

  // A search is a FLEET question: "across 1 of 2 machines" is the only proof the query
  // left this gateway, and a machine with no hit says so instead of "No sessions yet".
  it("reports which machines the query reached", async () => {
    const view = renderSessionsScreen({ machines: fleet() });
    restore = view.restore;
    await screen.findByText("First");

    view.setQuery("Second");
    expect(await screen.findByText("across 1 of 2 machines")).toBeTruthy();
    expect(screen.getByText("1 match")).toBeTruthy();
    expect(within(section("alpha")).getByText("No matches on this machine.")).toBeTruthy();
    expect(document.body.textContent).not.toContain("No sessions yet");
  });

  // One dead machine is a degraded SECTION, not an error page — the rest of the fleet is
  // still a working list, which is the whole reason to pair more than one.
  it("degrades one dead machine inside a working fleet", async () => {
    const view = renderSessionsScreen({
      machines: [fleet()[0], { label: "beta", down: true }],
    });
    restore = view.restore;
    await screen.findByText("First");

    const beta = within(section("beta"));
    // Its band still NAMES it — a rail with no name is a colour nobody can resolve —
    // and counts nothing, because a machine that is down has no count.
    expect(beta.getByText("beta")).toBeTruthy();
    expect(section("beta").textContent).not.toContain("0 sessions");
    expect(await beta.findByText("beta is not answering.")).toBeTruthy();
    expect(beta.getByRole("button", { name: /Retry/ })).toBeTruthy();
    // Nothing offers to create on a machine that cannot answer.
    expect(named(/^New project on beta$/)).toHaveLength(0);
    expect(screen.getByText("First")).toBeTruthy();
  });
});
