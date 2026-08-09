import { describe, expect, it } from "vitest";
import source from "./ManageProjectsSheet.tsx?raw";
import { startingDir } from "./ManageProjectsSheet";

// Regression, user report ("the manage projects looks absolutely awful
// on the desktop and iphone too — buttons are not canonicalized, components are not
// reused"). The sheet painted every box it stood in by hand, each a near-copy of
// something `Menu` already shipped, and each drifted:
//
//   * its own panel — the one surface in the app with NO way out but the scrim, and on
//     a 1440x900 desktop it rendered from y=300 with a 630px budget, putting `Use
//     project` 30px below the window with nothing able to scroll it back;
//   * its own `BAND`/`QUIET_BAND`, a second spelling of `MenuHeading`;
//   * its own `ROW`, a second spelling of `MenuItem` — and the one list in the app
//     that stayed at 44px rows under a mouse;
//   * its own `CHIP`, a second spelling of `MenuItem`'s badge;
//   * its own 44px pencil, borderless, answering neither hover nor focus;
//   * `quiet` for the secondary verb where every other dialog footer uses `secondary`.

describe("ManageProjectsSheet paints no box of its own", () => {
  // Regression, user report ("can Manage projects look exactly the same as the draft
  // picker?"): the draft picker is `AnchoredPanel` + a loud `MenuHeading` + `MenuItem`
  // rows, and this sheet was the one surface answering the same kind of question through
  // `Modal` + `DialogFrame` — a different box, a different header, a different entrance
  // and full-bleed height, one tap away from the menu that opened it.
  it("is the app’s one anchored panel, not a dialog of its own", () => {
    expect(source).toContain("<AnchoredPanel");
    expect(source).toContain('size="browse"');
    expect(source).toContain('role="dialog"');
    expect(source).not.toContain("<Modal onDismiss=");
    expect(source).not.toContain("<DialogFrame");
    // The panel paints its own box, so this file still paints none of it.
    expect(source).not.toContain("createPortal");
    expect(source).not.toContain("sm:w-96");
    expect(source).not.toContain("'--menu-top'");
  });

  it("names itself with the menu's own band and its one way out", () => {
    expect(source).toContain("closeLabel={`Close projects on ${label}`}");
    expect(source).not.toContain('title="Manage projects"');
    expect(source).not.toContain("const BAND");
    expect(source).not.toContain("const QUIET_BAND");
  });

  // Regression, user report: adding a project offered no way out of its own — the
  // band was a BACK arrow into the project inventory, so the only exit from "add a
  // project" was a screen the human never asked for. Adding closes, like every
  // other panel: its band carries the close, and the scrim dismisses it.
  it("closes out of adding instead of retreating into the inventory", () => {
    expect(source).not.toContain("<MenuBack");
    expect(source).toContain(
      "closeLabel={`Close add a project on ${label}`}",
    );
    expect(source).toContain("onDismiss={onCancel}");
  });

  it("lists folders with the shipped menu row and its badge", () => {
    expect(source).toContain("<MenuItem");
    expect(source).not.toContain("const ROW");
    expect(source).not.toContain("const CHIP");
  });

  it("uses the shipped icon button for the pencil, ink at rest", () => {
    expect(source).toContain("<IconButton");
    expect(source).toContain('variant="quiet"');
    expect(source).not.toContain("inline-flex size-11 shrink-0");
  });

  it("commits with the footer every other dialog in the app commits with", () => {
    expect(source).toContain("justify-end gap-2");
    expect(source).toContain('variant="secondary"');
    expect(source).not.toContain("justify-between gap-2");
  });

  it("does not caption the footer with the path the crumbs already say", () => {
    // The footer repeated the destination as a third spelling of it — the crumb bar
    // above already names the folder, and the button says what will happen to it.
    expect(source).not.toContain("homeify(aiming, home)");
    expect(source).not.toContain("const aiming");
  });

  it("docks the commit footer instead of scrolling it away", () => {
    // The list is the only part of this sheet that scrolls; the two verbs stay put.
    expect(source).toContain("min-h-0 flex-1 touch-pan-y overflow-y-auto");
    expect(source).toContain("shrink-0 border-t border-dialog-edge bg-panel-2");
  });

  it("keeps a crumb a real target rather than 14px of bare text", () => {
    expect(source).toContain("min-h-11 truncate px-1");
  });

  it("takes the rows out of play with inert, never with aria-hidden alone", () => {
    expect(source).toContain("inert={folder !== null}");
    expect(source).not.toContain("aria-hidden={folder !== null}");
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

  it("is what the sheet opens on", () => {
    expect(source).toContain("startingDir(startAt)");
  });

  // ...and the project you came from is named in that listing, so a folder one level
  // up is still recognisable as where you already are.
  it("badges the current project in both lists", () => {
    expect(source).toContain("entry.root === startAt ? 'current'");
    expect(source).toContain("entry.path === startAt");
  });
});

// Regression, user report ("Use project + New folder should be disabled and say it's
// already a project"). Aiming at a folder this machine ALREADY runs sessions in left
// both verbs live: "Use project" re-added an existing root and said nothing.
describe("a folder that is already a project offers no verb", () => {
  it("reads the aim against the machine\u2019s known roots, browsing only", () => {
    expect(source).toContain(
      "const alreadyProject = folder === null && !!target && knownRoots.has(target);",
    );
  });

  it("takes both footer buttons down", () => {
    expect(source).toContain("disabled={saving || !here || alreadyProject}");
    expect(source).toContain(
      "saving || !target || alreadyProject || (folder !== null && !folder.trim())",
    );
  });

  it("says why, on the leading edge of the footer", () => {
    expect(source).toContain(
      '<p className="mr-auto text-meta text-dialog-hint">It\u2019s already a project</p>',
    );
  });
});

// Regression, user report ("dialogs should occupy full height on the iPhone"): the
// sheet capped itself at `max-h-[80vh]` at the call site, so it stopped 169px short
// of the glass and the frame's own geometry never applied.
describe("the sheet does not size itself", () => {
  it("leaves height to DialogFrame", () => {
    expect(source).not.toContain("max-h-[80vh]");
  });
});

// Regression, user report ("clicking the edit button in choosing the project flickers
// and reflows"): the pencil handed the input `~/vis`, whose DIR is the parent — so the
// toggle re-listed one level up, filtered it to `vis*`, and the rows jumped; and even
// the same folder under its other spelling triggered a fresh fetch.
describe("taking the pencil does not move the list", () => {
  it("hands over a path that names the folder itself", () => {
    expect(source).toContain("function withSlash(path: string): string");
    expect(source).toContain(
      "setTyped(typed === null ? withSlash(homeify(here, home)) : null)",
    );
  });

  it("skips the fetch when the wanted directory is the one already listed", () => {
    expect(source).toContain("if (settled) return;");
    expect(source).toContain(
      "(wanted === listing.path || wanted === homeify(listing.path, listing.home))",
    );
  });

  it("paints the typed band on the crumb band\u2019s own paper", () => {
    const bands = source.match(/\$\{PATH_BAND\} gap-\d bg-panel-2/g) ?? [];
    expect(bands.length).toBe(2);
  });
});

// Regression, user report ("when I click it the height a little bit jumps a little"):
// the crumb bar and the typed-path bar were both `min-h-11`, but the crumb row settled
// at 45px and the input row at 44, so the pencil toggle shifted everything below by 1px.
describe("the path band", () => {
  const src = source;

  it("is one fixed-height band, spelled once", () => {
    expect(src).toContain("const PATH_BAND =");
    expect(src).toContain("flex h-11 shrink-0 items-center");
    expect(src).toContain("mouse:h-9");
  });

  it("never lets a band size itself to its content", () => {
    expect(src).not.toContain("min-h-11 shrink-0 items-center");
    expect(src.match(/\$\{PATH_BAND\}/g)?.length).toBe(3);
  });
});
