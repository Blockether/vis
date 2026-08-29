// @vitest-environment jsdom
import { act, fireEvent, screen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";
import type { MachineFixture } from "./sessions-screen-harness";
import { machineOutage, rememberMachineOutage } from "../lib/fleet-outage";

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

/** The hue the LIST wears for a machine: none — the mark on the switch carries it. */
const listHue = (machine: string) =>
  hue(section(machine).querySelector("[class*='border-machine-']"), "border");

const named = (pattern: RegExp) =>
  screen.queryAllByRole("button").filter((button) => {
    const name = button.getAttribute("aria-label") ?? button.textContent ?? "";
    return pattern.test(name);
  });

// Exactly one machine owns the list at a time; the switch behaves like a radio group.
describe("the machine scope always has one active machine", () => {
  // Regression, user report: the screen started unscoped and pressing the active machine
  // again returned to a hidden fleet view.
  it("selects the first machine on the initial paint and cannot turn it off", async () => {
    const view = renderSessionsScreen({ machines: fleet() });
    restore = view.restore;

    // Selection is part of the first render, not a later loading-state transition.
    const strip = within(screen.getByLabelText("Machines"));
    const alpha = strip.getByRole("button", { name: /^alpha/ });
    const beta = strip.getByRole("button", { name: /^beta/ });
    expect(alpha.getAttribute("aria-pressed")).toBe("true");
    expect(beta.getAttribute("aria-pressed")).toBe("false");

    await screen.findByText("First");
    expect(screen.queryByText("Second")).toBeNull();

    await userEvent.click(alpha);
    expect(alpha.getAttribute("aria-pressed")).toBe("true");
    expect(screen.getByText("First")).toBeTruthy();
    expect(screen.queryByText("Second")).toBeNull();

    await userEvent.click(beta);
    expect(await screen.findByText("Second")).toBeTruthy();
    expect(screen.queryByText("First")).toBeNull();
    expect(beta.getAttribute("aria-pressed")).toBe("true");
  });

  it("keeps every tab's distinct colour when switching machines", async () => {
    const view = renderSessionsScreen({ machines: fleet() });
    restore = view.restore;
    await screen.findByText("First");

    const strip = within(screen.getByLabelText("Machines"));
    const alpha = strip.getByRole("button", { name: /^alpha/ });
    const beta = strip.getByRole("button", { name: /^beta/ });
    const alphaHue = hue(alpha.querySelector("[class*='bg-machine-']"), "bg");
    const betaHue = hue(beta.querySelector("[class*='bg-machine-']"), "bg");
    expect(alphaHue).toBeTruthy();
    expect(betaHue).toBeTruthy();
    expect(alphaHue).not.toBe(betaHue);
    // Regression, user report (paraphrased: bin that rail on the left): the list used to
    // echo the tab's hue as a 2px frame down everything that machine owned.
    expect(listHue("alpha")).toBeNull();

    await userEvent.click(beta);
    await screen.findByText("Second");
    const chosen = within(screen.getByLabelText("Machines")).getByRole("button", {
      name: /^beta/,
    });
    expect(hue(chosen.querySelector("[class*='bg-machine-']"), "bg")).toBe(betaHue);
    expect(listHue("beta")).toBeNull();
  });

  it("puts machines that are not answering after every active machine", async () => {
    const view = renderSessionsScreen({
      machines: [
        { label: "sleeping-one", down: true },
        fleet()[0],
        { label: "sleeping-two", down: true },
        fleet()[1],
      ],
    });
    restore = view.restore;
    await screen.findByText("First");

    const labels = within(screen.getByLabelText("Machines"))
      .getAllByRole("button")
      .map((button) => button.getAttribute("aria-label") ?? button.textContent);
    expect(labels).toEqual([
      "alpha",
      "beta",
      "Reconnect to sleeping-one",
      "Reconnect to sleeping-two",
    ]);
  });

  it("keeps an unreachable machine as a retry without making it active", async () => {
    const view = renderSessionsScreen({ machines: [fleet()[0], { label: "beta", down: true }] });
    restore = view.restore;
    await screen.findByText("First");

    const strip = within(screen.getByLabelText("Machines"));
    const tile = await strip.findByRole("button", { name: /^Reconnect to beta/ });
    expect(tile.getAttribute("aria-pressed")).toBeNull();
    expect(screen.queryByLabelText("beta projects")).toBeNull();
    expect(strip.getByRole("button", { name: /^alpha/ }).getAttribute("aria-pressed")).toBe("true");
  });

  it("hands a whole dark fleet to the offline gate", async () => {
    const unreachable: (string | null)[] = [];
    const view = renderSessionsScreen({
      machines: [{ label: "alpha", down: true }, { label: "beta", down: true }],
      onUnreachable: (message) => unreachable.push(message),
    });
    restore = view.restore;
    await waitFor(() => expect(unreachable.filter(Boolean)).not.toHaveLength(0));
    expect(screen.queryByLabelText("Machines")).toBeNull();
  });
});

// Regression, user report (paraphrased: "I have one machine picked, and coming out of a
// session the list sometimes flashes the all-machines version"): the read that runs the
// moment the list is back on the glass can land in a radio handover, and ONE failed read
// declared the machine dark — which drained it out of `All` and dropped the reader's
// scope back to the whole fleet (`resolveScope`), then took it back a poll later.
describe("a machine that misses one read is not an outage", () => {
  const betaReads = (view: ReturnType<typeof renderSessionsScreen>) => {
    const origin = new URL(view.conns[1].url).origin;
    // A project's page is a read of ITS own (`GatewayClient.listProjectPage`); a
  // FLEET read is the one asked without a `root=`.
    return view.requests.filter(
      (request) =>
        request.machine === origin &&
        request.path.startsWith("/v1/sessions?") &&
        !request.path.includes("root="),
    ).length;
  };

  /** Park the list behind a transcript and come back to it, as leaving a session does. */
  const leaveSession = (view: ReturnType<typeof renderSessionsScreen>) => {
    view.setVisible(false);
    view.setVisible(true);
  };

  const scopeToBeta = async () => {
    await userEvent.click(
      within(screen.getByLabelText("Machines")).getByRole("button", { name: /^beta/ }),
    );
    await waitFor(() => expect(screen.queryByText("First")).toBeNull());
  };

  it("keeps the reader on their machine when one read drops", async () => {
    const view = renderSessionsScreen({
      machines: [fleet()[0], { ...fleet()[1], drops: [2] }],
    });
    restore = view.restore;
    await screen.findByText("First");
    await scopeToBeta();

    leaveSession(view);
    await waitFor(() => expect(betaReads(view)).toBe(2));
    await act(async () => {});

    // The screen the reader left is the screen they came back to.
    expect(screen.queryByText("First")).toBeNull();
    expect(screen.getByText("Second")).toBeTruthy();
    expect(named(/^Reconnect to beta/)).toHaveLength(0);
  });

  // The other half of the same rule: a machine that really is gone still goes, so the
  // reader is never parked on a dead machine's list.
  it("drains the machine once a second read confirms it", async () => {
    const view = renderSessionsScreen({
      machines: [fleet()[0], { ...fleet()[1], drops: [2, 3] }],
    });
    restore = view.restore;
    await screen.findByText("First");
    await scopeToBeta();

    leaveSession(view);
    await waitFor(() => expect(betaReads(view)).toBe(2));
    leaveSession(view);
    await waitFor(() => expect(betaReads(view)).toBe(3));

    expect(await screen.findByText("First")).toBeTruthy();
    await waitFor(() => expect(named(/^Reconnect to beta/)).toHaveLength(1));
    expect(screen.queryByText("Second")).toBeNull();
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

// Regression, user report (paraphrased: "even when a machine is not active you try it on
// `All` every single time, and that is what makes the list flash — reconnect to a machine
// that failed in the background, and do not put it back on `All` while you are trying"):
// the fleet was rebuilt from nothing on every mount, and opening a session unmounts this
// screen, so a laptop this device had already found asleep came back as a machine nobody
// had tried yet. It took a band, a rail and a section in the middle of the working fleet
// until the probe behind it spent the transport's whole budget confirming what was
// already known — and then the section vanished under the reader. Worse, that probe rode
// the fleet's own poll: the tick queued behind it was dropped as stale, so one dead
// gateway halved the refresh of every machine that was answering.
describe("a machine known to be dark reconnects in the background", () => {
  const alphaOrigin = (conns: { url: string }[]) => new URL(conns[0].url).origin;

  it("never takes a place in All while it is being tried again", async () => {
    const beta: MachineFixture = { label: "beta", down: true, hangs: true };
    const view = renderSessionsScreen({ machines: [fleet()[0], beta] });
    restore = view.restore;
    await screen.findByText("First");
    await waitFor(() => expect(screen.queryByLabelText("beta projects")).toBeNull());
    view.unmount();

    // The relaunch: same machines, same addresses, and beta is now the closed laptop that
    // takes the socket without ever answering it.
    const again = renderSessionsScreen({ machines: [fleet()[0], beta], at: view.conns });
    restore = () => {
      again.restore();
      view.restore();
    };
    // The FIRST frame already knows: no band, no rail, no section standing in for a probe.
    expect(screen.queryByLabelText("beta projects")).toBeNull();
    await screen.findByText("First");
    expect(screen.queryByLabelText("beta projects")).toBeNull();
    // Still being asked, though — quietly. Dropping it from the view is not giving up on it.
    const betaOrigin = new URL(view.conns[1].url).origin;
    expect(again.requests.some((request) => request.machine === betaOrigin)).toBe(true);
  });

  // Regression, user report ("gateways that are not active should not show up in All —
  // it should only appear once the gateway answers, not appear at once and then get
  // detached"): a machine this device had ANSWERED FOR before painted its cached rows on
  // the first frame of the next mount, so a laptop that had since gone to sleep took a
  // named section in the middle of the fleet and lost it when its probe ran out.
  it("waits for the machine to speak before giving it a section", async () => {
    const beta: MachineFixture = {
      label: "beta",
      sessions: [listSession({ id: "b1", title: "Second", workspace: { root: "/w/two" } })],
    };
    const view = renderSessionsScreen({ machines: [fleet()[0], beta] });
    restore = view.restore;
    // It answered here, so this device now holds beta's list.
    await userEvent.click(within(screen.getByLabelText("Machines")).getByRole("button", { name: /^beta/ }));
    await screen.findByText("Second");
    view.unmount();

    // The relaunch: the same beta, now asleep and never answering.
    const again = renderSessionsScreen({
      machines: [fleet()[0], { ...beta, down: true, hangs: true }],
      at: view.conns,
    });
    restore = () => {
      again.restore();
      view.restore();
    };
    // Cached rows are what to paint WHEN it answers, never a section on their own.
    expect(screen.queryByLabelText("beta projects")).toBeNull();
    expect(document.body.textContent).not.toContain("Second");
    await screen.findByText("First");
    expect(screen.queryByLabelText("beta projects")).toBeNull();
    expect(document.body.textContent).not.toContain("Second");
  });

  describe("beside the fleet's own poll", () => {
    beforeEach(() => vi.useFakeTimers());
    afterEach(() => vi.useRealTimers());

    /** Let every poll, probe and repaint that fits inside `ms` happen. */
    const settle = async (ms = 0) => {
      await act(async () => {
        await vi.advanceTimersByTimeAsync(ms);
      });
    };

    const listReads = (view: { requests: { machine: string; path: string }[]; conns: { url: string }[] }) =>
      view.requests.filter(
        (request) =>
          request.machine === alphaOrigin(view.conns) &&
          request.path.startsWith("/v1/sessions?") &&
          !request.path.includes("root="),
      ).length;

    it("lets the machines that answer keep their ten seconds", async () => {
      const view = renderSessionsScreen({
        machines: [fleet()[0], { label: "beta", down: true, hangs: true }],
      });
      restore = view.restore;
      await settle(50);
      expect(screen.getByText("First")).toBeTruthy();

      // One poll, then the next: neither is spent waiting on the machine that is not there.
      const cold = listReads(view);
      await settle(10_000);
      const first = listReads(view);
      expect(first).toBeGreaterThan(cold);
      await settle(10_000);
      expect(listReads(view)).toBeGreaterThan(first);
    });

    it("walks the machine back in when it finally answers", async () => {
      const view = renderSessionsScreen({
        machines: [
          fleet()[0],
          {
            label: "beta",
            down: true,
            heals: true,
            sessions: [listSession({ id: "b1", title: "Second", workspace: { root: "/w/two" } })],
          },
        ],
      });
      restore = view.restore;
      await settle(50);
      expect(screen.queryByLabelText("beta projects")).toBeNull();

      // Nobody pressed anything: the poll's silent probe restores the destination, but
      // alpha remains active until the reader explicitly switches machines.
      await settle(10_000);
      await settle(50);
      const strip = within(screen.getByLabelText("Machines"));
      expect(strip.getByRole("button", { name: /^beta/ }).getAttribute("aria-pressed")).toBe("false");
      expect(strip.getByRole("button", { name: /^alpha/ }).getAttribute("aria-pressed")).toBe("true");
      expect(screen.queryByText("Second")).toBeNull();
    });
  });
});

// Regression, user report (paraphrased: "the gateway machine has been off for hours, and the
// sessions list still shows it as active and pressable — it goes dark for a second, then it is
// active again; you are not saving it anywhere"): the dark verdict lived in a module-level Map
// that dies with the JavaScript context, and the OS kills this webview whenever the app goes
// to the background. So every launch met a machine this device had already found dark as one
// nobody had ever tried — a raised, pressable tile — until that launch's own socket ran out
// the transport's deadline all over again.
describe("what this device found dark outlives the app", () => {
  const strip = () => within(screen.getByLabelText("Machines"));

  it("writes the verdict down when the machine goes dark", async () => {
    const view = renderSessionsScreen({ machines: [fleet()[0], { label: "beta", down: true }] });
    restore = view.restore;
    await screen.findByText("First");
    await waitFor(() =>
      expect(strip().getByRole("button", { name: /^Reconnect to beta/ })).toBeTruthy(),
    );

    expect(machineOutage(view.conns[1].url)).toBeTruthy();
    // The machine that answered is not remembered as anything.
    expect(machineOutage(view.conns[0].url)).toBeNull();
  });

  it("starts that machine drained in the FIRST frame of the next launch", async () => {
    const conns = [
      { url: "http://relaunch-alpha.example.com", token: "t", label: "alpha" },
      { url: "http://relaunch-beta.example.com", token: "t", label: "beta" },
    ];
    // What the run before the kill wrote down.
    rememberMachineOutage(conns[1].url, "Failed to fetch");

    const view = renderSessionsScreen({
      machines: [fleet()[0], { label: "beta", down: true, hangs: true }],
      at: conns,
    });
    restore = view.restore;

    // Nothing has been probed yet in this app: the tile is the retry, not a place to go.
    expect(strip().getByRole("button", { name: /^Reconnect to beta/ })).toBeTruthy();
    expect(strip().queryByRole("button", { name: /^beta$/ })).toBeNull();
    expect(screen.queryByLabelText("beta projects")).toBeNull();

    await screen.findByText("First");
    expect(strip().getByRole("button", { name: /^Reconnect to beta/ })).toBeTruthy();
  });

  // A memory is not a blackout: the shell's offline screen belongs to a fleet that has RUN OUT
  // of machines this run, or a solo user whose laptop was woken an hour ago would open the app
  // onto the pairing page every time.
  it("never hands the shell its offline screen on a saved verdict alone", async () => {
    const conns = [{ url: "http://relaunch-solo.example.com", token: "t", label: "tower" }];
    rememberMachineOutage(conns[0].url, "Failed to fetch");
    const told: string[] = [];

    const view = renderSessionsScreen({
      machines: [{ label: "tower", sessions: [listSession({ id: "t1", title: "Only" })] }],
      at: conns,
      onUnreachable: (message) => {
        if (message) told.push(message);
      },
    });
    restore = view.restore;

    // It is up, and it says so on this run's first read.
    await screen.findByText("Only");
    expect(told).toEqual([]);
  });
});
