// @vitest-environment jsdom
import { act, fireEvent, screen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";
import type { MachineFixture } from "./sessions-screen-harness";

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

  // A control that had to ask which of two computers it meant would be the chooser the
  // switch exists to abolish, so in the fleet view it stands on each machine's own band.
  it("gives every machine its own projects control, and one when scoped", async () => {
    const view = renderSessionsScreen({ machines: fleet() });
    restore = view.restore;
    await screen.findByText("First");

    expect(named(/^Projects on/).map((button) => button.getAttribute("aria-label"))).toEqual([
      "Projects on alpha",
      "Projects on beta",
    ]);
    expect(section("beta").contains(named(/^Projects on beta$/)[0]!)).toBe(true);

    await userEvent.click(
      within(screen.getByLabelText("Machines")).getByRole("button", { name: /^beta/ }),
    );
    await waitFor(() => expect(named(/^Projects on/)).toHaveLength(1));
    // Scoped, the control is back above the card, outside the list.
    expect(section("beta").contains(named(/^Projects on beta$/)[0]!)).toBe(false);
  });

  // Regression, user report ("the plus is here and here"): the machine's control is a
  // folder mark now, and a machine with nothing under its band has no project row on
  // screen to say what that folder opens.
  it("spells the machine's control out where its section is empty", async () => {
    const view = renderSessionsScreen({
      machines: [fleet()[0], { label: "beta", sessions: [] }],
    });
    restore = view.restore;
    await screen.findByText("First");

    const beta = within(section("beta"));
    expect(beta.getByText("No sessions on this machine yet.")).toBeTruthy();
    // Two controls for this machine: the band's mark, and the word beside the empty
    // body — same name, same sheet, and the word is the one carrying a word.
    const both = beta.getAllByRole("button", { name: "Projects on beta" });
    expect(both).toHaveLength(2);
    expect(both.map((button) => button.textContent)).toEqual(["", "Projects"]);
    // A machine that IS answering with sessions keeps the mark alone.
    expect(
      within(section("alpha")).getAllByRole("button", { name: "Projects on alpha" }),
    ).toHaveLength(1);
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

  // Regression, user report ("offline stuff should just not be accessible... and maybe
  // more visually shown that they are disabled instead of showing offline"): a machine
  // that was not answering took a named section in the middle of the fleet whose whole
  // content was its own failure, and a live tab wearing the word "offline" that scoped
  // the screen to a machine with nothing to show.
  it("keeps a machine that is not answering out of the All view", async () => {
    const view = renderSessionsScreen({
      machines: [fleet()[0], { label: "beta", down: true }],
    });
    restore = view.restore;
    await screen.findByText("First");
    const strip = within(screen.getByLabelText("Machines"));
    await waitFor(() => expect(strip.getByRole("button", { name: /^Reconnect to beta/ })).toBeTruthy());

    // No section, no band, no Retry inside the list: the fleet on screen is the fleet
    // that answered, and the machine that did not is the strip's business.
    expect(screen.queryByLabelText("beta projects")).toBeNull();
    expect(document.body.textContent).not.toContain("beta is not answering.");
    expect(named(/Retry/)).toHaveLength(0);
    expect(screen.getByText("First")).toBeTruthy();

    // Its tile stays — a machine you paired never vanishes from the row that lists your
    // machines — drained, unpressed-looking, and no longer a state to be in.
    const tile = strip.getByRole("button", { name: /^Reconnect to beta/ });
    expect(tile.getAttribute("aria-pressed")).toBeNull();
    expect(tile.getAttribute("title")).toContain("beta is not answering");
    expect(tile.textContent).toBe("beta");
    // Hollow hue: the machine keeps its colour, with nothing behind it.
    const mark = tile.querySelector("[class*='machine-']")!;
    expect(mark.className).toMatch(/border border-machine-/);
    expect(mark.className).not.toMatch(/bg-machine-/);
  });

  // The one thing a dead machine can still do is come back, so that is what its tile
  // does. The press answers where the finger is, and a machine that answers walks back
  // into the fleet view with its own section and rail.
  it("makes the drained tile the retry, and says what the retry did", async () => {
    const view = renderSessionsScreen({
      machines: [fleet()[0], { label: "beta", down: true, heals: true, sessions: [
        listSession({ id: "b1", title: "Second", workspace: { root: "/w/two" } }),
      ] }],
    });
    restore = view.restore;
    await screen.findByText("First");
    const strip = within(screen.getByLabelText("Machines"));
    const tile = await strip.findByRole("button", { name: /^Reconnect to beta/ });

    await userEvent.click(tile);
    // Woken, it is a machine again: its own section, its own rail, its own tab.
    expect(await screen.findByText("Second")).toBeTruthy();
    expect(railHue("beta")).toBeTruthy();
    expect(railHue("beta")).not.toBe(railHue("alpha"));
    await waitFor(() =>
      expect(strip.getByRole("button", { name: /^beta/ }).getAttribute("aria-pressed")).toBe(
        "false",
      ),
    );
    expect(document.body.textContent).not.toContain("Unable to connect");
  });

  it("says so in the tile when the retry comes back dead", async () => {
    const view = renderSessionsScreen({
      machines: [fleet()[0], { label: "beta", down: true }],
    });
    restore = view.restore;
    await screen.findByText("First");
    const strip = within(screen.getByLabelText("Machines"));

    await userEvent.click(await strip.findByRole("button", { name: /^Reconnect to beta/ }));
    await waitFor(() =>
      expect(strip.getByRole("button", { name: /^Reconnect to beta/ }).textContent).toContain(
        "Unable to connect",
      ),
    );
    // Still not a door: the failure did not put a section back in the list.
    expect(screen.queryByLabelText("beta projects")).toBeNull();
  });

  // Dropping the dead machines from `All` must never empty the screen: a TOTAL
  // blackout is not a quiet list, it is an unreachable one, and it is handed to the
  // shell's own offline gate rather than painted as a fleet of nothing.
  it("hands a whole dark fleet to the offline gate instead of an empty list", async () => {
    const unreachable: (string | null)[] = [];
    const view = renderSessionsScreen({
      machines: [
        { label: "alpha", down: true },
        { label: "beta", down: true },
      ],
      onUnreachable: (message) => unreachable.push(message),
    });
    restore = view.restore;
    await waitFor(() => expect(unreachable.filter(Boolean)).not.toHaveLength(0));
    expect(screen.queryByLabelText("Machines")).toBeNull();
    expect(screen.queryByLabelText("alpha projects")).toBeNull();
  });
});

// Regression, user report (paraphrased: "this reconnecting should have a 5 second
// timeout, and the error should then be RED, say something like 'Unable to connect',
// and be shown for at most 3 seconds"): the press inherited the transport's own 30s
// budget — and a list read can page, so a machine that was blackholed rather than
// refused wore `reconnecting...` for longer still — then printed "no answer" in the
// strip's own hint ink and kept it there for as long as the screen stayed open.
describe("a retry answers on its own clock", () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });
  afterEach(() => {
    vi.useRealTimers();
  });

  /** Let every press, probe and repaint that fits inside `ms` happen. */
  const settle = async (ms = 0) => {
    await act(async () => {
      await vi.advanceTimersByTimeAsync(ms);
    });
  };

  /** Mount a fleet whose second machine is down and press that machine's tile. */
  const pressRetry = async (beta: MachineFixture) => {
    const view = renderSessionsScreen({ machines: [fleet()[0], beta] });
    restore = view.restore;
    await settle(50);
    const strip = within(screen.getByLabelText("Machines"));
    const tile = () => strip.getByRole("button", { name: /^Reconnect to beta/ });
    await act(async () => {
      fireEvent.click(tile());
    });
    return tile;
  };

  /** What the pressed tile is saying, beside the machine's own name. */
  const note = (tile: HTMLElement) => tile.textContent?.replace(/^beta/, "") ?? "";

  it("gives up on a machine that never answers, five seconds in", async () => {
    const tile = await pressRetry({ label: "beta", down: true, hangs: true });
    expect(note(tile())).toBe("reconnecting...");

    // Four seconds of silence is still a retry in flight, not a verdict.
    await settle(4_000);
    expect(note(tile())).toBe("reconnecting...");

    await settle(1_500);
    expect(note(tile())).toBe("Unable to connect");
    // Red: a failure in the strip's own hint ink reads as more chrome.
    expect(tile().querySelector(".text-err")).toBeTruthy();
  });

  it("takes the failure back off the tile three seconds later", async () => {
    const tile = await pressRetry({ label: "beta", down: true });
    await settle(50);
    expect(note(tile())).toBe("Unable to connect");

    await settle(2_000);
    expect(note(tile())).toBe("Unable to connect");

    await settle(1_500);
    expect(note(tile())).toBe("");
  });
});
