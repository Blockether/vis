// @vitest-environment jsdom
import type { ReactElement } from "react";
import { describe, expect, it, vi } from "vitest";
import { render, screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { AnchoredPanel, MenuItem } from "./Menu";
import { Button, IconButton } from "./ui";
import {
  ManageProjectsSheet,
  startingDir,
  type ManagedProject,
} from "./ManageProjectsSheet";
import type { GatewayClient } from "../lib/gateway";
import type { BrowseEntry, BrowseListing } from "../lib/types";

const HOME = "/Users/me";
const CODE = `${HOME}/code`;
const VIS = `${CODE}/vis`;
const SPEL = `${CODE}/spel`;

function entry(
  name: string,
  at: string,
  extra: Partial<BrowseEntry> = {},
): BrowseEntry {
  return {
    name,
    path: `${at}/${name}`,
    entry_count: 4,
    is_repo: false,
    ...extra,
  };
}

const TREE: Record<string, BrowseEntry[]> = {
  [HOME]: [entry("code", HOME)],
  [CODE]: [
    entry("vis", CODE, { is_repo: true, branch: "main" }),
    entry("spel", CODE, { is_repo: true, branch: "main" }),
    entry("tools", CODE, { is_repo: true, branch: "trunk" }),
    entry("notes", CODE, { entry_count: 1 }),
  ],
  [VIS]: [entry("src", VIS)],
};

/** The gateway's own answer for a path, `~` included, so the sheet can browse. */
function listing(asked: string): BrowseListing {
  const trimmed = asked.replace(/\/+$/, "") || "/";
  const path = trimmed.startsWith("~") ? `${HOME}${trimmed.slice(1)}` : trimmed;
  const cut = path.lastIndexOf("/");
  return {
    path,
    parent: cut > 0 ? path.slice(0, cut) : path === "/" ? null : "/",
    home: HOME,
    is_truncated: false,
    entries: TREE[path] ?? [],
  };
}

const PROJECTS: ManagedProject[] = [
  { name: "vis", root: VIS, projectId: "p-vis", count: 3, live: 1 },
  { name: "spel", root: SPEL, projectId: "p-spel", count: 1, live: 0 },
];

function machine() {
  return {
    browse: vi.fn(async (path?: string) => listing(path ?? "~")),
    createDirectory: vi.fn(async (at: string, name: string) => ({
      path: `${at}/${name}`,
    })),
  };
}

type SheetProps = Parameters<typeof ManageProjectsSheet>[0];

function sheet(props: Partial<SheetProps> = {}) {
  const client = machine();
  const onCancel = vi.fn();
  const onChoose = vi.fn();
  const onRemove = vi.fn();
  const view = render(
    <ManageProjectsSheet
      label="tower"
      at={null}
      client={client as unknown as GatewayClient}
      startAt={VIS}
      knownRoots={new Set([VIS, SPEL])}
      projects={PROJECTS}
      onCancel={onCancel}
      onChoose={onChoose}
      onRemove={onRemove}
      {...props}
    />,
  );
  const panel = () =>
    screen.getByRole("dialog", { name: "Manage projects on tower" });
  return { view, client, onCancel, onChoose, onRemove, panel };
}

const classesOf = (element: Element) => new Set(element.classList);

/** Every utility a node wears, order-free — a class string may hold blanks. */
const paint = (element: Element) => [...element.classList].sort();

/**
 * What a shipped control PAINTS, read off a rendering of that control on its own.
 * A screen reuses `ui.tsx`/`Menu.tsx` when its own node wears exactly this skin —
 * a class string copied into a screen drifts the first time the owner changes.
 */
function skinOf(element: ReactElement, select: string): string[] {
  const view = render(element);
  // Its OWN container first: a reference control is rendered while the sheet is
  // already on screen, and `document.body` holds both.
  const node =
    view.container.querySelector(select) ?? document.querySelector(select);
  if (!node) throw new Error(`nothing matched ${select}`);
  const classes = paint(node);
  view.unmount();
  return classes;
}

// Regression, user report ("the manage projects looks absolutely awful on the desktop
// and iphone too — buttons are not canonicalized, components are not reused"). The
// sheet painted every box it stood in by hand, each a near-copy of something `Menu`
// already shipped, and each drifted: its own panel (the one surface in the app with no
// way out but the scrim, whose `Use project` landed 30px below a 1440x900 window), its
// own heading band, its own 44px row that never shrank under a mouse, its own badge,
// its own borderless pencil, and `quiet` for the secondary verb where every other
// dialog footer uses `secondary`.
describe("ManageProjectsSheet paints no box of its own", () => {
  it("is the app’s one anchored panel, not a dialog of its own", async () => {
    const { panel } = sheet();
    await screen.findByRole("menuitem", { name: /vis/ });

    // Byte for byte the draft picker's panel: the sheet adds no box, no height cap of
    // its own (`max-h-[80vh]` at the call site stopped it 169px short of the glass)
    // and no second way of arriving.
    expect(paint(panel())).toEqual(
      skinOf(
          <AnchoredPanel
            size="browse"
            role="dialog"
            label="reference"
            at={null}
            onDismiss={() => {}}
          >
            <p>x</p>
          </AnchoredPanel>,
        '[aria-label="reference"]',
      ),
    );
  });

  it("names itself with the menu's own band and its one way out", async () => {
    const { onCancel } = sheet();
    expect(await screen.findByText("Projects · tower")).toBeInTheDocument();

    await userEvent.click(
      screen.getByRole("button", { name: "Close projects on tower" }),
    );
    expect(onCancel).toHaveBeenCalledTimes(1);
  });

  // Regression, user report: adding a project offered no way out of its own — the band
  // was a BACK arrow into the project inventory, so the only exit from "New project"
  // was a screen the human never asked for. Adding closes, like every other panel: its
  // band carries the close, and the scrim dismisses it.
  it("closes out of adding instead of retreating into the inventory", async () => {
    const { onCancel } = sheet({ isAdding: true });
    expect(await screen.findByText("New project · tower")).toBeInTheDocument();
    expect(screen.queryByRole("button", { name: /back/i })).toBeNull();

    await userEvent.click(
      screen.getByRole("button", { name: "Close new project on tower" }),
    );
    expect(onCancel).toHaveBeenCalledTimes(1);
  });

  it("lists folders with the shipped menu row and its badge", async () => {
    sheet({ isAdding: true });
    const row = await screen.findByRole("menuitem", { name: /tools/ });

    expect(paint(row)).toEqual(
      skinOf(
          <MenuItem title="reference" hint="4 entries" onSelect={() => {}} />,
        '[role="menuitem"]',
      ),
    );
    // The badge is the row's own word, not a chip this file invented: the folder Vis
    // already runs sessions in says so, a bare checkout only says `git`.
    expect(
      screen.getByRole("menuitem", { name: /spel/ }).textContent,
    ).toContain("project");
    expect(row.textContent).toContain("git");
    expect(
      screen.getByRole("menuitem", { name: /notes/ }).textContent,
    ).not.toContain("git");
  });

  it("uses the shipped icon button for the pencil, ink at rest", async () => {
    sheet({ isAdding: true });
    const pencil = await screen.findByRole("button", { name: "Type a path" });

    expect(paint(pencil)).toEqual(
      skinOf(
          <IconButton variant="quiet" label="reference" onClick={() => {}}>
            <span />
          </IconButton>,
        '[aria-label="reference"]',
      ),
    );
  });

  it("commits with the footer every other dialog in the app commits with", async () => {
    sheet({ isAdding: true });
    const use = await screen.findByRole("button", { name: "Use project" });
    const make = screen.getByRole("button", { name: "New folder" });

    expect(paint(make)).toEqual(
      skinOf(
          <Button variant="secondary" onClick={() => {}}>
            reference
          </Button>,
        "button",
      ),
    );
    expect(paint(use)).toEqual(
      skinOf(<Button onClick={() => {}}>reference</Button>, "button"),
    );

    // Both verbs at the TRAILING edge, primary last — thrown to opposite ends of a
    // 384px panel they read as two unrelated screens.
    const row = use.parentElement;
    expect(make.parentElement).toBe(row);
    expect(classesOf(row!)).toContain("justify-end");
    expect(classesOf(row!)).not.toContain("justify-between");
    const order = [...row!.children];
    expect(order.indexOf(use)).toBeGreaterThan(order.indexOf(make));
  });

  it("does not caption the footer with the path the crumbs already say", async () => {
    sheet({ isAdding: true });
    const use = await screen.findByRole("button", { name: "Use project" });

    // The footer repeated the destination as a third spelling of it — the crumb bar
    // above already names the folder, and the button says what will happen to it.
    expect(use.parentElement?.textContent).toBe("New folderUse project");
  });

  it("docks the commit footer instead of scrolling it away", async () => {
    sheet({ isAdding: true });
    const row = await screen.findByRole("menuitem", { name: /tools/ });
    const use = screen.getByRole("button", { name: "Use project" });

    // The list is the only part of this sheet that scrolls; the two verbs stay put.
    const scroller = row.closest(".overflow-y-auto");
    expect(scroller).not.toBeNull();
    expect(scroller!.contains(use)).toBe(false);
    expect(classesOf(use.closest(".shrink-0")!)).toContain("border-t");
  });

  it("keeps a crumb a real target rather than 14px of bare text", async () => {
    const { client } = sheet({ isAdding: true });
    const home = await screen.findByRole("button", { name: "~" });

    expect(home.tagName).toBe("BUTTON");
    expect(classesOf(home)).toContain("min-h-11");
    // The crumb you are standing on is not a way to anywhere.
    expect(screen.getByRole("button", { name: "code" })).toBeDisabled();
    expect(screen.getByRole("button", { name: "code" })).toHaveAttribute(
      "aria-current",
      "location",
    );

    await userEvent.click(home);
    await waitFor(() =>
      expect(client.browse).toHaveBeenCalledWith(HOME, expect.anything()),
    );
    expect(await screen.findByRole("menuitem", { name: /code/ })).toBeInTheDocument();
  });

  it("takes the rows out of play with inert, never with aria-hidden alone", async () => {
    sheet({ isAdding: true });
    const rows = (await screen.findByRole("menuitem", { name: /tools/ }))
      .parentElement;

    expect(rows).not.toHaveAttribute("inert");
    await userEvent.click(screen.getByRole("button", { name: "New folder" }));

    // A container merely hidden from the a11y tree still hands its buttons to Tab.
    expect(rows).toHaveAttribute("inert");
    expect(rows).not.toHaveAttribute("aria-hidden");
  });
});

// Regression, user report ("let it start ../ from the current project"). Browsing
// opened INSIDE the machine's current project, so adding the next checkout beside it
// began with a tap on the parent crumb, and the first list you saw was that project's
// own `src/`.
describe("browsing opens one level above the current project", () => {
  it("lists the project’s siblings, not its contents", () => {
    expect(startingDir("/Users/me/code/vis")).toBe("/Users/me/code");
    expect(startingDir("/Users/me/code/vis/")).toBe("/Users/me/code");
  });

  it("stays put where there is no `..`", () => {
    expect(startingDir(null)).toBe(null);
    expect(startingDir("/")).toBe("/");
    expect(startingDir("vis")).toBe("vis");
  });

  it("is what the sheet opens on", async () => {
    const { client } = sheet({ isAdding: true });
    await screen.findByRole("menuitem", { name: /tools/ });

    expect(client.browse.mock.calls[0][0]).toBe(CODE);
    expect(screen.queryByRole("menuitem", { name: /src/ })).toBeNull();
  });

  // ...and the project you came from is named in that listing, so a folder one level
  // up is still recognisable as where you already are.
  it("badges the current project in both lists", async () => {
    sheet();
    expect(
      (await screen.findByRole("menuitem", { name: /vis/ })).textContent,
    ).toContain("current");

    await userEvent.click(screen.getByRole("button", { name: "New project…" }));
    expect(
      (await screen.findByRole("menuitem", { name: /vis\// })).textContent,
    ).toContain("current");
  });
});

// Regression, user report ("Use project + New folder should be disabled and say it's
// already a project"). Aiming at a folder this machine ALREADY runs sessions in left
// both verbs live: "Use project" re-added an existing root and said nothing.
describe("a folder that is already a project offers no verb", () => {
  it("takes both footer buttons down and says why", async () => {
    sheet({ isAdding: true, knownRoots: new Set([CODE]) });
    // The aim is the folder being LISTED, so the verbs only answer once it lands.
    await screen.findByRole("menuitem", { name: /vis/ });
    const use = screen.getByRole("button", { name: "Use project" });

    expect(use).toBeDisabled();
    expect(screen.getByRole("button", { name: "New folder" })).toBeDisabled();
    expect(screen.getByText("It’s already a project")).toBeInTheDocument();
  });

  it("leaves the verbs live for a folder the machine does not run yet", async () => {
    const { onChoose } = sheet({ isAdding: true });
    // The aim is the folder being LISTED, so the verbs wake with the listing.
    await screen.findByRole("menuitem", { name: /vis/ });
    const use = screen.getByRole("button", { name: "Use project" });

    expect(use).toBeEnabled();
    expect(screen.queryByText("It’s already a project")).toBeNull();
    await userEvent.click(use);
    expect(onChoose).toHaveBeenCalledWith(CODE);
  });
});

// Regression, user report ("clicking the edit button in choosing the project flickers
// and reflows"): the pencil handed the input `~/vis`, whose DIR is the parent — so the
// toggle re-listed one level up, filtered it to `vis*`, and the rows jumped; and even
// the same folder under its other spelling triggered a fresh fetch.
describe("taking the pencil does not move the list", () => {
  it("hands over a path that names the folder itself, and re-lists nothing", async () => {
    const { client } = sheet({ isAdding: true });
    await screen.findByRole("menuitem", { name: /tools/ });
    const reads = client.browse.mock.calls.length;

    await userEvent.click(screen.getByRole("button", { name: "Type a path" }));
    expect(screen.getByLabelText("Path on this machine")).toHaveValue("~/code/");

    // Past the typing debounce: the folder on screen is the folder asked for, so the
    // gateway is not asked for it again and no row moves.
    await new Promise((resolve) => setTimeout(resolve, 250));
    expect(client.browse.mock.calls.length).toBe(reads);
    expect(
      screen.getByRole("menuitem", { name: /tools/ }),
    ).toBeInTheDocument();
  });

  it("narrows the same listing as the leaf is typed", async () => {
    const { client } = sheet({ isAdding: true });
    await screen.findByRole("menuitem", { name: /tools/ });
    await userEvent.click(screen.getByRole("button", { name: "Type a path" }));
    const reads = client.browse.mock.calls.length;

    await userEvent.type(screen.getByLabelText("Path on this machine"), "to");
    await waitFor(() =>
      expect(screen.queryByRole("menuitem", { name: /notes/ })).toBeNull(),
    );
    expect(screen.getByRole("menuitem", { name: /tools/ })).toBeInTheDocument();
    expect(client.browse.mock.calls.length).toBe(reads);
  });
});

// Regression, user report ("when I click it the height a little bit jumps a little"):
// the crumb bar and the typed-path bar were both `min-h-11`, but the crumb row settled
// at 45px and the input row at 44, so the pencil toggle shifted everything below by 1px.
describe("the path band", () => {
  const heights = (band: Element) =>
    [...band.classList].filter((token) => /h-\d|min-h/.test(token)).sort();

  it("is one fixed-height band under both of its spellings", async () => {
    sheet({ isAdding: true });
    await screen.findByRole("menuitem", { name: /tools/ });
    const crumbs = screen
      .getByRole("button", { name: "~" })
      .closest("div.bg-panel-2")!;

    expect(heights(crumbs)).toEqual(["h-11", "mouse:h-9"]);

    await userEvent.click(screen.getByRole("button", { name: "Type a path" }));
    const typed = screen
      .getByLabelText("Path on this machine")
      .closest("div.bg-panel-2")!;

    // Same paper, same height: the pencil toggles what is INSIDE the band.
    expect(heights(typed)).toEqual(heights(crumbs));
  });

  it("gives the new-folder line that same band", async () => {
    sheet({ isAdding: true });
    await screen.findByRole("menuitem", { name: /tools/ });
    await userEvent.click(screen.getByRole("button", { name: "New folder" }));
    const naming = screen.getByLabelText("New folder name").closest("div.h-11");

    expect(naming).not.toBeNull();
    expect(heights(naming!)).toEqual(["h-11", "mouse:h-9"]);
  });
});


// Regression, user report ("the projects manager blinks and it is not a projects manager
// at all, only a way to add another project — none of the rows are there"): the folder
// mark on the sessions list mounted this sheet with `isAdding`, so it opened straight on
// the gateway's folder browser — an empty list that filled in one network round-trip
// later, with no project rows and no trash beside them, and no way back to the inventory.
describe("the projects mark opens the inventory", () => {
  it("puts creation in the dark heading and gives each path its own line", async () => {
    sheet();
    const create = screen.getByRole("button", { name: "New project…" });
    const heading = create.closest("header")!;
    expect(heading).toHaveClass("bg-dialog-title");

    const row = await screen.findByRole("menuitem", { name: /vis/ });
    expect(row.textContent).toContain("3 transcripts, 1 running");
    const path = screen.getByText("~/code/vis");
    expect(path).toHaveClass("block");
    expect(path.parentElement).toBe(row.querySelector("span.min-w-0.flex-1"));
  });
  it("lists the machine's projects, each with its trash, before any browse", async () => {
    const { client, onRemove } = sheet();

    const row = await screen.findByRole("menuitem", { name: /vis/ });
    expect(row).toBeInTheDocument();
    expect(screen.getByRole("menuitem", { name: /spel/ })).toBeInTheDocument();
    // Nothing was asked of the gateway: the inventory is what this device already knows.
    expect(client.browse.mock.calls.length).toBe(0);

    await userEvent.click(
      screen.getByRole("button", { name: "Remove every transcript in vis" }),
    );
    expect(onRemove).toHaveBeenCalledWith(PROJECTS[0]);
  });

  it("leaves `New project…` the way it was entered", async () => {
    sheet();
    await screen.findByRole("menuitem", { name: /vis/ });

    await userEvent.click(screen.getByRole("button", { name: "New project…" }));
    await screen.findByRole("menuitem", { name: /tools/ });

    await userEvent.click(
      screen.getByRole("button", { name: "Back to projects on tower" }),
    );
    expect(await screen.findByRole("menuitem", { name: /spel/ })).toBeInTheDocument();
  });

  it("keeps the way OUT when the caller asked for the browser by name", async () => {
    sheet({ isAdding: true });
    await screen.findByRole("menuitem", { name: /tools/ });

    expect(screen.queryByRole("button", { name: /^Back to projects/ })).toBeNull();
    expect(
      screen.getByRole("button", { name: "Close new project on tower" }),
    ).toBeInTheDocument();
  });
});
