// @vitest-environment jsdom
import { screen, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { afterEach, describe, expect, it, vi } from "vitest";

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
    expect(create[0]!.textContent).toBe("New session");
    // It sits inside the project header's trailing cluster, never on the machine band.
    const header = within(screen.getByLabelText("project sessions"));
    expect(header.getByRole("button", { name: "New session on alpha" })).toBe(create[0]);
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

  // Regression, user report ("when I open the ⋯ the view is not coherent between the one ⋯
  // on the machine and another ⋯ on the project level"), and the follow-up ("these remove
  // sessions do we really need to have it in the ⋯?"): two hand-built panels of different
  // widths, and the project's held exactly ONE destructive row — a menu of one, repeated on
  // every project header, standing permanently beside the verb that creates.
  it("leaves the list exactly one overflow menu, and it belongs to the machine", async () => {
    const onMachineSettings = vi.fn();
    const view = renderSessionsScreen({ machines: alpha(), onMachineSettings });
    restore = view.restore;
    await screen.findByText("First");

    const kebabs = named(/^Actions for/);
    expect(kebabs.map((button) => button.getAttribute("aria-label"))).toEqual([
      "Actions for alpha",
    ]);
    expect(named(/^Remove /)).toHaveLength(0);

    await userEvent.click(kebabs[0]!);
    const menu = within(screen.getByRole("menu"));
    expect(menu.getByRole("menuitem", { name: /Manage projects/ })).toBeTruthy();
    await userEvent.click(menu.getByRole("menuitem", { name: /Machine settings/ }));
    expect(onMachineSettings).toHaveBeenCalledWith(view.conns[0]);
  });

  // Regression, user report (paraphrased: put `+` and the gear on the band — that is add
  // project and the machine settings; and later, make them real buttons like New session):
  // the band's verbs were bordered glyphs, then frameless words, beside an amber slab.
  it("spells the machine's ADD verb as a word in a real button", async () => {
    const view = renderSessionsScreen({ machines: alpha() });
    restore = view.restore;
    await screen.findByText("First");

    const add = screen.getByRole("button", { name: "Add a project on alpha" });
    expect(add.textContent).toBe("Add project");
    // The amber fill `New session` wears, so the two verbs are one species.
    expect(add.className).toContain("bg-accent");
    // ...and it opens the SAME portal the menu row opens, aimed at this machine.
    await userEvent.click(add);
    expect(await screen.findByRole("dialog")).toBeTruthy();
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

  // One dead machine is a degraded section, not an error page. Scoped to that machine
  // its failure IS the screen: say it is not answering, show the message and a Retry, and
  // disable the create buttons instead of rendering "No sessions yet".
  it("keeps a dead machine's Retry where its sessions would have been", async () => {
    const view = renderSessionsScreen({
      machines: [
        { label: "alpha", sessions: [listSession({ id: "a1", title: "First" })] },
        { label: "beta", down: true },
      ],
    });
    restore = view.restore;
    await screen.findByText("First");

    await userEvent.click(
      within(screen.getByLabelText("Machines")).getByRole("button", { name: /^beta/ }),
    );
    const retry = await screen.findByRole("button", { name: /Retry/ });
    expect(retry).toBeTruthy();
    expect(screen.getAllByText(/not answering/i).length).toBeGreaterThan(0);
    expect(screen.queryByText(/No sessions yet/)).toBeNull();
    // Nothing offers to create on a machine that cannot answer.
    expect(named(/^New session on/)).toHaveLength(0);
  });
});

// Regression, user report (paraphrased: unify the view whether the fleet holds one machine
// or many): the strip appeared only above two machines and carried a pairing chip a solo
// user saw instead of chips. A choice of one is not a choice, so with one gateway paired
// the whole band goes — and pairing lives in Preferences, not on this strip.
describe("the machine strip", () => {
  const fleet = [
    { label: "alpha", sessions: [listSession({ id: "a1", title: "First" })] },
    { label: "beta", sessions: [listSession({ id: "b1", title: "Second" })] },
  ];

  it("costs a solo user nothing: no strip, no chips, no machine landmark", async () => {
    const view = renderSessionsScreen({ machines: alpha() });
    restore = view.restore;
    await screen.findByText("First");
    expect(screen.queryByLabelText("Machines")).toBeNull();
  });

  it("scopes to exactly one machine — this one or that one, never All", async () => {
    const view = renderSessionsScreen({ machines: fleet });
    restore = view.restore;
    await screen.findByText("First");

    const strip = within(screen.getByLabelText("Machines"));
    expect(strip.getAllByRole("button").map((tab) => tab.textContent)).toEqual([
      "alpha",
      "beta",
    ]);
    // No fleet-wide chip, and no pairing verb: this row answers "which machine" only.
    expect(strip.queryByRole("button", { name: /All/ })).toBeNull();
    expect(strip.queryByRole("button", { name: /machine/i })).toBeNull();

    await userEvent.click(strip.getByRole("button", { name: /^beta/ }));
    expect(await screen.findByText("Second")).toBeTruthy();
    expect(screen.queryByText("First")).toBeNull();
    expect(screen.getByRole("button", { name: "New session on beta" })).toBeTruthy();
  });
});
