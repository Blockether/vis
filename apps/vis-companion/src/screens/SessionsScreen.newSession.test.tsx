// @vitest-environment jsdom
import { screen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { afterEach, describe, expect, it } from "vitest";

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

let restore = () => {};
afterEach(() => restore());

const named = (pattern: RegExp) =>
  screen.queryAllByRole("button").filter((button) => {
    const name = button.getAttribute("aria-label") ?? button.textContent ?? "";
    return pattern.test(name);
  });

const alpha = () => [
  {
    label: "alpha",
    sessions: [listSession({ id: "a1", title: "First" })],
  },
];

// Regression, user report (new sessions belong to a project, not a machine): the create
// button used to live on the fleet and machine headers, where it had no project owner,
// and it sent the home-shortened display path back as the workspace root — on a gateway
// that resolved `~` relatively, creating in `~/vis` produced the impossible `~/vis/~/vis`.
describe('where "New session" lives', () => {
  it("stands once on the project header, naming its machine and its project", async () => {
    const view = renderSessionsScreen({ machines: alpha() });
    restore = view.restore;
    await screen.findByText("First");

    const create = named(/^New session on/);
    expect(create).toHaveLength(1);
    expect(create[0]!.getAttribute("aria-label")).toBe("New session on alpha");
    // Several machines are on screen at once, so the row says which one AND which project.
    expect(create[0]!.getAttribute("title")).toBe("New session on alpha, in project");
    // The word repeated once per project; the mark says the same thing in 37px, and the
    // name it gave up is on the label and the tooltip above.
    expect(create[0]!.textContent).toBe("");
    expect(create[0]!.querySelector("svg")).toBeTruthy();
    // It sits inside the project header's trailing cluster, never on the machine band.
    const header = within(screen.getByLabelText("project sessions"));
    expect(header.getByRole("button", { name: "New session on alpha" })).toBe(create[0]);
  });

  // Regression, user report (paraphrased: creating a new session from the app took
  // several seconds): the screen used to await its own full fleet re-read — one
  // request per 100-row window, serially, per machine — before opening the session
  // it had just created, so the wait the user paid for was a list they were leaving.
  it("opens the created session without waiting for the list to reload", async () => {
    const opened: string[] = [];
    const view = renderSessionsScreen({
      machines: alpha(),
      onOpen: (_conn, sid) => opened.push(sid),
    });
    restore = view.restore;
    await screen.findByText("First");

    // The post-create fleet re-read never lands while this test watches.
    view.holdList();
    view.requests.length = 0;
    await userEvent.click(screen.getByRole("button", { name: "New session on alpha" }));

    await waitFor(() => expect(opened).toHaveLength(1));
    expect(opened[0]).toMatch(/^created-/);
    // ...and it was opened as a FRESH session, with the list read still in flight.
    view.releaseList();
  });

  it("sends the canonical workspace root, never the home-shortened display path", async () => {
    const view = renderSessionsScreen({ machines: alpha() });
    restore = view.restore;
    await screen.findByText("First");
    // The header SHOWS the short path...
    expect(screen.getByTitle("/Users/dev/project").textContent).toBe("~/project");

    view.requests.length = 0;
    await userEvent.click(screen.getByRole("button", { name: "New session on alpha" }));

    // ...and SENDS the real one.
    const create = view.requests.find(
      (request) => request.method === "POST" && request.path === "/v1/sessions",
    );
    expect(create?.body).toEqual({ channel: "web", root: "/Users/dev/project" });
  });

  // Regression, user report (paraphrased: take the `⋯` off the right of the machine row):
  // the list's last overflow menu stood beside the switcher and held two rows — `Manage
  // projects`, which is the sheet the machine's own control opens anyway, and `Machine settings`,
  // which the Machines tab and the app bar's cog already open. A menu whose every answer
  // was one tap away without it.
  it("leaves the list no overflow menu at all", async () => {
    const view = renderSessionsScreen({ machines: alpha() });
    restore = view.restore;
    await screen.findByText("First");

    expect(named(/^Actions for/)).toHaveLength(0);
    expect(named(/^Remove /)).toHaveLength(0);
    expect(screen.queryByRole("menu")).toBeNull();

    // The one row it held that this screen owns is the sheet the amber mark opens.
    await userEvent.click(screen.getByRole("button", { name: "Projects on alpha" }));
    const sheet = within(await screen.findByRole("dialog"));
    expect(sheet.getByText(/New project/)).toBeTruthy();
  });

  // Regression, user report (paraphrased: put `+` and the gear on the band — that is add
  // project and the machine settings; and later, make them real buttons like New session;
  // and last, that a plus on the machine band and a plus on every project header meant two
  // different creations): the band's control is the app's button wearing the FOLDER it
  // opens, which is this machine's project inventory and never was a create.
  it("marks the machine's projects with the folder it opens, in a real button", async () => {
    const view = renderSessionsScreen({ machines: alpha() });
    restore = view.restore;
    await screen.findByText("First");

    const add = screen.getByRole("button", { name: "Projects on alpha" });
    expect(add.textContent).toBe("");
    expect(add.querySelector("svg")).toBeTruthy();
    // The amber fill `New session` wears, so the two controls are one species.
    expect(add.className).toContain("bg-accent");
    // ...and the plus is left to mean exactly one thing on this screen: a session.
    expect(add.innerHTML).not.toBe(
      screen.getAllByRole("button", { name: /^New session on/ })[0]!.innerHTML,
    );
    // ...and it opens the SAME portal the menu row opens, aimed at this machine.
    await userEvent.click(add);
    expect(await screen.findByRole("dialog")).toBeTruthy();
  });

  // Regression, user report (the machine band struck out on a screenshot, with the create
  // verb moved up to the row that holds the machine chips): a band inside the card named
  // the machine the chips had just named, printed "2 projects · 1080 sessions" that every
  // project header below already counts, and rented a whole row of a phone's glass for
  // the two controls that now stand on the chip row.
  it("keeps the machine's verbs above the card and prints no second band", async () => {
    const view = renderSessionsScreen({ machines: alpha() });
    restore = view.restore;
    await screen.findByText("First");

    // The counts the band carried are gone from the screen.
    expect(screen.queryByText(/\d+ projects?\s*·/)).toBeNull();
    // The machine is named by its tab and the rail, never by a band of its own.
    expect(screen.queryByRole("button", { name: "Rename alpha" })).toBeNull();

    // The control stands OUTSIDE the list card, on the row above it, after the switch.
    const create = screen.getByRole("button", { name: "Projects on alpha" });
    const strip = screen.getByLabelText("Machines");
    const list = screen.getByLabelText("alpha projects");
    expect(create.closest("section")).toBe(screen.getByLabelText("Sessions"));
    expect(list.contains(create)).toBe(false);
    expect(list.contains(strip)).toBe(false);
    expect(strip.compareDocumentPosition(create) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
  });
});

// Regression, user report ("there is no much difference visually between the machine and
// the project"): the list carried TWO header bands one hairline apart. A machine is not a
// header any more — it is a tag and the spine down everything it owns — and the project
// header stays the only header BAND in the list.
describe("machine, project and session are three different shapes", () => {
  it("folds a project from the naming half of its own band", async () => {
    const view = renderSessionsScreen({ machines: alpha() });
    restore = view.restore;
    await screen.findByText("First");

    await userEvent.click(screen.getByRole("button", { name: "Collapse project" }));
    expect(screen.queryByText("First")).toBeNull();
    // The verb stays outside the fold.
    expect(screen.getByRole("button", { name: "New session on alpha" })).toBeTruthy();

    await userEvent.click(screen.getByRole("button", { name: "Expand project" }));
    expect(screen.getByText("First")).toBeTruthy();
  });

  // Regression, user report ("the individual session is bigger then project"): a session
  // row stood 48px against a 36px project band — the child taller than the thing that
  // contains it. The leaf is the SHORTEST of the three levels, and a pointer tightens it.
  it("keeps the session row shorter than the bands that contain it", async () => {
    const view = renderSessionsScreen({ machines: alpha() });
    restore = view.restore;
    const row = (await screen.findByText("First")).closest("button")!;
    expect(row.className).toContain("min-h-12");
    expect(row.className).toContain("mouse:min-h-8");
    expect(row.className).not.toContain("min-h-14");
  });

  // NOTHING ANSWERING IS NOT AN EMPTY LIST, and this screen never paints one: a total
  // blackout is handed to the shell's own offline gate, which is the screen that can
  // actually say which gateway failed and why. Never "No sessions yet" over a fleet
  // that was never read.
  it("hands a fleet with nothing answering to the offline gate", async () => {
    const unreachable: (string | null)[] = [];
    const view = renderSessionsScreen({
      machines: [{ label: "alpha", down: true }],
      onUnreachable: (message) => unreachable.push(message),
    });
    restore = view.restore;

    await waitFor(() => expect(unreachable.filter(Boolean)).not.toHaveLength(0));
    expect(screen.queryByText(/No sessions yet/)).toBeNull();
    expect(named(/^New session on/)).toHaveLength(0);
  });

  // Regression, user report ("offline stuff should just not be accessible"): a machine
  // that was not answering was still a tab that scoped the whole screen to it.
  it("refuses to scope a fleet to a machine that is not answering", async () => {
    const view = renderSessionsScreen({
      machines: [
        { label: "alpha", sessions: [listSession({ id: "a1", title: "First" })] },
        { label: "beta", down: true },
      ],
    });
    restore = view.restore;
    await screen.findByText("First");
    const strip = within(screen.getByLabelText("Machines"));

    await userEvent.click(await strip.findByRole("button", { name: /^Reconnect to beta/ }));
    // Pressing it retried beta; it never became the scope, and `All` stayed on.
    await waitFor(() =>
      expect(strip.getByRole("button", { name: "All" }).getAttribute("aria-pressed")).toBe("true"),
    );
    expect(screen.getByText("First")).toBeTruthy();
  });
});

// Regression, user report (paraphrased: the machine tab must be rendered on the left
// whether we have one machine or more): the strip appeared only above two machines, so a
// solo user's list said nowhere which computer it was on, and pairing a second machine
// rearranged the screen. Pairing itself still lives in Preferences, not on this strip.
describe("the machine strip", () => {
  const fleet = [
    { label: "alpha", sessions: [listSession({ id: "a1", title: "First" })] },
    { label: "beta", sessions: [listSession({ id: "b1", title: "Second" })] },
  ];

  it("stands for a fleet of one too: one tab, already pressed, and no All", async () => {
    const view = renderSessionsScreen({ machines: alpha() });
    restore = view.restore;
    await screen.findByText("First");

    const strip = within(screen.getByLabelText("Machines"));
    const tabs = strip.getAllByRole("button");
    // "Every machine" and "this machine" would be the same list under two names.
    expect(tabs.map((tab) => tab.textContent)).toEqual(["alpha"]);
    expect(tabs[0]!.getAttribute("aria-pressed")).toBe("true");
  });

  // Regression, user report (paraphrased: every machine should have its own hue and its
  // own rail, but the screen only ever showed one of them): the scope was always exactly
  // one machine, so a fleet of six painted one colour and the fleet view was unreachable.
  it("leads with All above a fleet, and starts there", async () => {
    const view = renderSessionsScreen({ machines: fleet });
    restore = view.restore;
    await screen.findByText("First");

    const strip = within(screen.getByLabelText("Machines"));
    expect(strip.getAllByRole("button").map((tab) => tab.textContent)).toEqual([
      "All",
      "alpha",
      "beta",
    ]);
    // No pairing verb: this row answers "which machine", All included, and nothing else.
    expect(strip.queryByRole("button", { name: /Add machine/i })).toBeNull();
    expect(strip.getByRole("button", { name: "All" }).getAttribute("aria-pressed")).toBe(
      "true",
    );
    // Both machines are on screen at once, which is the whole point of All.
    expect(screen.getByText("Second")).toBeTruthy();
  });

  it("scopes to one machine, and All takes the fleet back", async () => {
    const view = renderSessionsScreen({ machines: fleet });
    restore = view.restore;
    await screen.findByText("First");
    const strip = within(screen.getByLabelText("Machines"));

    await userEvent.click(strip.getByRole("button", { name: /^beta/ }));
    expect(await screen.findByText("Second")).toBeTruthy();
    expect(screen.queryByText("First")).toBeNull();
    expect(screen.getByRole("button", { name: "New session on beta" })).toBeTruthy();

    await userEvent.click(strip.getByRole("button", { name: "All" }));
    expect(await screen.findByText("First")).toBeTruthy();
    expect(screen.getByText("Second")).toBeTruthy();
  });
});
