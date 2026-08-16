import type { ComponentProps } from "react";
import { renderToStaticMarkup } from "react-dom/server";
import { describe, expect, it } from "vitest";

import uiSource from "./ui.tsx?raw";
import appSource from "../App.tsx?raw";
import storageSource from "../lib/storage.ts?raw";
import sessionsListSource from "../screens/SessionsScreen.tsx?raw";
import gatewaySource from "../lib/gateway.ts?raw";
import settingsSource from "../screens/SettingsScreen.tsx?raw";
import chatSource from "./ChatContent.tsx?raw";
import docSource from "./DocArtifact.tsx?raw";
import markdownArtifactSource from "./MarkdownArtifact.tsx?raw";
import tableBarSource from "./DataTable.tsx?raw";
import boundarySource from "./ErrorBoundary.tsx?raw";
import artifactsSheetSource from "./ArtifactsSheet.tsx?raw";
import manageProjectsSource from "./ManageProjectsSheet.tsx?raw";
import imageViewerSource from "./ImageViewer.tsx?raw";
import humanInputSource from "./HumanInputPrompt.tsx?raw";
import providerAuthSource from "./ProviderAuth.tsx?raw";
import routerSource from "../screens/RouterScreen.tsx?raw";
import sessionScreenSource from "../screens/SessionScreen.tsx?raw";
import connectSource from "../screens/ConnectScreen.tsx?raw";
import machinesSource from "./Machines.tsx?raw";

import { DraftIcon, PlusIcon, ProjectsIcon } from "./icons";
import { MACHINE_COLORS } from "../lib/machine-colors";
import {
  BackButton,
  BandLabel,
  BandTally,
  EditableName,
  BandButton,
  Button,
  Chip,
  ChoiceCell,
  ChoiceRow,
  ComposerButton,
  CopyChip,
  CloseButton,
  ConfirmRow,
  DialogFrame,
  DialogHeader,
  HeaderActions,
  HeaderMeta,
  HeaderTally,
  HeaderTitle,
  Pager,
  pageWindow,
  IconButton,
  KebabButton,
  ListRow,
  LiveCount,
  LoadMore,
  LiveTally,
  LIST_EDGE,
  machineTagFace,
  MachineMark,
  MachineProjectsButton,
  MachineRail,
  MachineSwitcher,
  MachineTab,
  MetaButton,
  NewSessionButton,
  NotifyConnectionRow,
  OptionRow,
  Pill,
  Disclosure,
  Switch,
  TextButton,
  ProjectCrumb,
  RowDisclosure,
  SearchField,
  SectionGap,
  SectionHeader,
  SectionShelf,
  Spinner,
  UnreadBadge,
} from "./ui";
import { MenuHeading } from "./Menu";

// Regression (reported: "why we still have this chevron here showing something is
// collapsible if we cannot click it — let's have just one color"): the caret half of
// the New session split control was painted in the dark title-bar ink, so an amber
// primary carried a charcoal slab that read as switched-off chrome — the one half
// that DOES open a menu looked like the one half nobody may press.
describe("split button", () => {
  const split = () =>
    renderToStaticMarkup(
      <span className="flex items-stretch">
        <Button pressEffect="none" className="border-r-0">
          New session
        </Button>
        <Button
          pressEffect="none"
          aria-haspopup="menu"
          className="border-l-accent-foreground/30"
        >
          <span aria-hidden>▾</span>
        </Button>
      </span>,
    );

  it("paints both halves in the one accent, split by a hairline of its own ink", () => {
    const html = split();

    expect(html.match(/(?<!:)bg-accent(?![/-])/g)).toHaveLength(2);
    expect(html).toContain("border-l-accent-foreground/30");
    expect(html).not.toContain("bg-dialog-title");
    expect(html).not.toContain("text-dialog-title-foreground");
  });

  it("leaves the caret pressable, so the chevron is a promise the control keeps", () => {
    const html = split();

    expect(html.match(/<button/g)).toHaveLength(2);
    expect(html).toContain('aria-haspopup="menu"');
    expect(html).not.toContain('disabled=""');
  });
});

// The live count wears the SAME filled block as the unread badge, in green:
// `macbook \u25ae3\u25ae\u25ae4\u25ae` — one shape, two colours, running then
// waiting. It replaced a bracketed `[3]`, which read lighter than the badge
// beside it, and before that a `\u25cf` whose metrics sat below the digits.
describe("LiveTally", () => {
  it("is a filled green block, not bracketed text or a glyph", () => {
    const html = renderToStaticMarkup(<LiveTally count={5} />);

    expect(html).toContain("bg-ok-surface");
    expect(html).toContain("text-ok-foreground");
    expect(html).toContain(">5<");
    expect(html).not.toContain("[");
    expect(html).not.toContain("\u25cf");
  });

  // `--ok` is the app's green INK (LIVE text, the 6px machine dot). Poured into
  // a badge it is a slab twice as dark as the amber block beside it and carries
  // its digit at 5:1; the fill has to be the lightened `ok-surface` peer.
  it("fills with the green surface, never with the green ink", () => {
    const html = renderToStaticMarkup(<LiveTally count={5} />);

    expect(html).not.toMatch(/bg-ok(?!-surface)/);
  });

  it("says what the number counts, for a reader that cannot see green", () => {
    const html = renderToStaticMarkup(<LiveTally count={1} />);

    expect(html).toContain('<span class="sr-only"> live</span>');
  });

  it("renders nothing when nothing is running", () => {
    expect(renderToStaticMarkup(<LiveTally count={0} />)).toBe("");
  });
});

// Unread is a notification, not a second tally: beside the bracketed live
// count it has to be told apart from it WITHOUT the reader remembering a colour
// code, so it wears the same filled amber block the session row uses for "new".
describe("UnreadBadge", () => {
  it("is a filled block, not a bare number beside the live count", () => {
    const html = renderToStaticMarkup(<UnreadBadge count={3} />);

    expect(html).toContain("bg-accent");
    expect(html).toContain("text-accent-foreground");
    expect(html).toContain(">3<");
    expect(html).not.toContain("[");
  });

  it("says what the number counts, for a reader that cannot see amber", () => {
    const html = renderToStaticMarkup(<UnreadBadge count={1} />);

    expect(html).toContain('<span class="sr-only"> unread</span>');
  });

  it("renders nothing when there is nothing new", () => {
    expect(renderToStaticMarkup(<UnreadBadge count={0} />)).toBe("");
  });
});

// Regression, user report ("the machine is not updated"): the machine tag shipped as a
// banner INSIDE the list, gated on a fleet section count this screen can never reach, so
// every machine on screen kept the plain white name it always had. The tag is a face on
// the chrome that actually renders now.
describe("machineTagFace", () => {
  // Regression, user report ("the foreground is black and it doesn't look good"): the
  // tag was a FILLED block of hue, which at L 0.62 can only carry dark ink, so the
  // machine's name was the only black word on the screen. Hue is the edge now.
  it("names the machine with its own hue on the leading edge, in the page's ink", () => {
    const face = machineTagFace(MACHINE_COLORS[0]!);

    expect(face).toContain(MACHINE_COLORS[0]!.rail);
    expect(face).toContain("border-l-2");
    expect(face).toContain("text-white");
    expect(face).not.toContain("text-machine-ink");
    expect(face).not.toContain(MACHINE_COLORS[0]!.dot);
    expect(face).toContain("px-1.5");
  });

  // A tag as wide as its column is the full-bleed bar again, which is what the spine
  // replaced; and a long name must truncate rather than push the machine's verbs off.
  it("hugs the name and truncates instead of growing", () => {
    const face = machineTagFace(MACHINE_COLORS[0]!);

    expect(face).toContain("w-fit");
    expect(face).toContain("max-w-full");
    expect(face).toContain("truncate");
  });

  it("gives two machines two different tags", () => {
    expect(machineTagFace(MACHINE_COLORS[0]!)).not.toBe(
      machineTagFace(MACHINE_COLORS[1]!),
    );
  });

  // Without a hue it still has to read as a tag: an unpainted name is the white ink
  // this replaced.
  it("falls back to the grey edge rather than bare ink", () => {
    expect(machineTagFace()).toContain("border-dialog-edge");
    expect(machineTagFace()).toContain("bg-level-machine");
  });

  // The machine's name is also its RENAME control, and the field it becomes used to
  // wear `bg-transparent p-0` spelled inside `EditableName` — so the tag lost its hue
  // and its padding the moment a caret arrived. Paper belongs to the face.
  it("survives being the rename control", () => {
    const html = renderToStaticMarkup(
      <EditableName
        face={machineTagFace(MACHINE_COLORS[0]!)}
        label="Rename tower"
        value="tower"
        onCommit={() => {}}
      />,
    );

    expect(html).toContain(MACHINE_COLORS[0]!.rail);
    expect(html).toContain("px-1.5");
  });
});

// A project FOLDS, and the fold is the naming half of its own header.
describe("ProjectCrumb", () => {
  it("is a disclosure that names its project and its state", () => {
    const html = renderToStaticMarkup(
      <ProjectCrumb
        name="vis"
        qualifier="~/vis"
        isOpen
        onToggle={() => {}}
        label="Collapse vis"
      />,
    );

    expect(html).toContain('aria-expanded="true"');
    expect(html).toContain('aria-label="Collapse vis"');
    expect(html).toContain("vis");
    expect(html).toContain("~/vis");
  });

  // The chevron rides in the mark column `HeaderTitle` already reserves, so folding
  // a project costs a glyph and never an indent: the name stays on the one leading
  // edge every row of this list shares.
  it("keeps the name on the list's leading edge", () => {
    const folded = renderToStaticMarkup(
      <ProjectCrumb
        name="vis"
        isOpen={false}
        onToggle={() => {}}
        label="Expand vis"
      />,
    );

    expect(folded).toContain(LIST_EDGE);
    expect(folded).not.toContain("pl-6");
    expect(folded).toContain('aria-expanded="false"');
  });

  // The trailing cluster is NOT inside the fold: "New session" is the verb this
  // screen exists for and must never be swallowed by a disclosure.
  it("takes only the naming half of the band", () => {
    const html = renderToStaticMarkup(
      <ProjectCrumb
        name="vis"
        isOpen
        onToggle={() => {}}
        label="Collapse vis"
      />,
    );

    expect(html).toContain("flex-1");
    expect(html).not.toContain("w-full");
  });
});

describe("MachineRail", () => {
  // A machine's hue separates two computers before a word is read. It ran as a 2px
  // border INSIDE the card, one pixel from the card's own border — a grey hairline
  // immediately followed by a coloured one, doing one job twice — and, being a
  // border, it also stole 2px of layout the trailing edge had no match for (left ink
  // 19px against right ink 17px). It is the card's LEFT FRAME now: the card gives
  // that side up, both sides are 2px, and the rail simply colours one of them.
  it("is the frame, in the machine colour", () => {
    const html = renderToStaticMarkup(
      <MachineRail color={MACHINE_COLORS[3]!}>rows</MachineRail>,
    );
    expect(html).toContain("border-l-2");
    expect(html).toContain(MACHINE_COLORS[3]!.rail);
    expect(html).toContain("rows");
  });

  it("gives two machines two different rails", () => {
    const first = renderToStaticMarkup(
      <MachineRail color={MACHINE_COLORS[0]!}>a</MachineRail>,
    );
    const second = renderToStaticMarkup(
      <MachineRail color={MACHINE_COLORS[1]!}>a</MachineRail>,
    );
    expect(first).not.toBe(second);
  });

  // Without a hue it still has to PAINT: this is the card's edge, and a frame that
  // disappears where a colour is missing is a hole in the panel, not a subtlety.
  it("falls back to the list frame rather than vanishing", () => {
    const html = renderToStaticMarkup(<MachineRail>rows</MachineRail>);
    expect(html).toContain("border-l-2");
    expect(html).toContain("border-dialog-edge");
  });
});

describe("SectionHeader", () => {
  // Regression, user report ("there is no much difference visually between the machine
  // and the project"): the list carried two bands one hairline apart. There is one
  // band now — the project's — and the machine is a chip above the list.
  it("is one band, with no second tone to be confused with", () => {
    const html = renderToStaticMarkup(<SectionHeader>rows</SectionHeader>);
    expect(html).toContain("border-b border-dialog-edge");
    expect(html).not.toContain("border-b-2");
    expect(html).toContain("min-h-13");
    expect(html).toContain("mouse:min-h-9");
    expect(html).toContain("items-stretch");
    expect(html).not.toContain("py-2");
    // The only header in the list is the one that sticks.
    expect(html).toContain("sticky top-0 z-10");
    expect(html).toContain("bg-level-project");
    expect(html).not.toContain("bg-level-machine");
    expect(html).not.toContain("bg-panel-2");
  });

  it("gives its title the list's own type step", () => {
    expect(
      renderToStaticMarkup(
        <SectionHeader>
          <HeaderTitle name="vis" />
        </SectionHeader>,
      ),
    ).toContain("text-title");
    expect(renderToStaticMarkup(<HeaderTitle name="orphan" />)).toContain(
      "text-title",
    );
  });

  it("marks a machine with a block bigger than a session status dot", () => {
    expect(
      renderToStaticMarkup(
        <MachineMark size="banner" color={MACHINE_COLORS[2]!} />,
      ),
    ).toContain("size-2.5");
    expect(
      renderToStaticMarkup(<MachineMark color={MACHINE_COLORS[2]!} />),
    ).toContain("size-1.5");
  });
});

describe("MachineMark", () => {
  it("is the rail hue as a solid block, and decoration only", () => {
    const html = renderToStaticMarkup(
      <MachineMark color={MACHINE_COLORS[7]} />,
    );

    expect(html).toContain(MACHINE_COLORS[7].dot);
    expect(html).toContain('aria-hidden="true"');
    expect(html).not.toContain("bg-ok");
  });
});

// Regression (reported: "the new session is button so frequently used that we should
// take it from the ⋯ and put on every machine header before the ⋯, as a yellow
// button"): the verb this whole screen exists for was the first row of a menu, so the
// thing people do all day cost a tap, a popover and a read before it could be pressed.
describe("NewSessionButton", () => {
  const html = (props: Partial<Parameters<typeof NewSessionButton>[0]> = {}) =>
    renderToStaticMarkup(
      <NewSessionButton machine="tower" onPress={() => {}} {...props} />,
    );

  it("is the yellow one: the verb of the screen, not a row of a menu", () => {
    expect(html()).toContain("bg-accent");
    expect(html()).toContain("New session");
  });

  // Regression, user report ("make those buttons less verbose — nine yellow slabs
  // saying the same eleven characters"): the word was right once and repetitive nine
  // times, because a project header appears once per repository.
  it("is a PLUS at rest: the word repeats once per project, the mark does not", () => {
    expect(html()).toContain("<svg");
    expect(html()).not.toContain(">New session<");
    // The name survives whole where a reader and a pointer can still get at it.
    expect(html({ where: "vis" })).toContain(
      'aria-label="New session on tower"',
    );
    expect(html({ where: "vis" })).toContain(
      'title="New session on tower, in vis"',
    );
  });

  it("names the machine it will start on, because every header carries one", () => {
    expect(html({ machine: "nuc" })).toContain(
      'aria-label="New session on nuc"',
    );
  });

  it("puts the project on the tooltip, where the header has no room for a path", () => {
    expect(html({ where: "vis" })).toContain(
      'title="New session on tower, in vis"',
    );
    expect(html()).toContain('title="New session on tower"');
  });

  it("does not move under the press: it anchors the folder browser", () => {
    expect(html()).not.toContain("active:scale");
  });

  // Regression, user report: the 28px desktop box still read as a tall slab beside the
  // 24px machine action. The shared 16px line box fits safely inside a 24px control.
  it("uses the same compact mouse height as the neighboring small action", () => {
    expect(html()).toContain("mouse:h-6");
    expect(html()).toContain("mouse:min-h-6");
    expect(html()).toContain("self-center");
    expect(html()).toContain(" h-8 ");
    expect(html()).toContain("mouse:text-meta");
    expect(html()).not.toContain("mouse:h-7");
  });

  it("is refused while the machine is busy or not answering", () => {
    expect(html({ disabled: true })).toContain('disabled=""');
  });
});

// Regression, user report ("this new session button should be the same as other
// buttons"): every icon-only control was written by hand at its call site, so the
// machine header's `⋯` was a 32px bordered box while the project header's, one row
// below it, was a 44px borderless slab with a bigger glyph — and neither of them
// looked like the yellow button they stood beside.
describe("IconButton", () => {
  const html = (props: Partial<Parameters<typeof IconButton>[0]> = {}) =>
    renderToStaticMarkup(
      <IconButton label="Actions for tower" {...props}>
        <span aria-hidden>⋯</span>
      </IconButton>,
    );

  it("is the app’s button with a glyph where its word would be", () => {
    expect(html()).toContain("border-edge-strong");
    expect(html()).toContain("min-h-7");
    expect(html()).toContain("focus-visible:ring-accent/60");
  });

  // Reported: "the height of the new session button on iOS is too big". The touch
  // box WAS the paint (`h-11`), so the amber slab filled the whole header band.
  it("wears the same compact box as the yellow button beside it", () => {
    const primary = renderToStaticMarkup(
      <NewSessionButton machine="tower" onPress={() => {}} />,
    );

    for (const rhythm of [
      " h-8 ",
      "self-center",
      "mouse:h-6",
      "mouse:min-h-6",
    ]) {
      expect(html()).toContain(rhythm);
      expect(primary).toContain(rhythm);
    }
  });

  it("keeps the 44px finger target the 32px face gave up", () => {
    const primary = renderToStaticMarkup(
      <NewSessionButton machine="tower" onPress={() => {}} />,
    );

    // 32px of ink + 6px above + 6px below = 44px of touchable button.
    for (const reach of [
      "after:absolute",
      "after:-top-1.5",
      "after:-bottom-1.5",
    ]) {
      expect(html()).toContain(reach);
      expect(primary).toContain(reach);
    }
    // A cursor needs no invisible reach, and the desktop box is 24px anyway.
    expect(primary).toContain("mouse:after:content-none");
  });

  it("is named, because it carries no word", () => {
    expect(html()).toContain('aria-label="Actions for tower"');
  });

  it("does not move under the press: it anchors a menu", () => {
    expect(html()).not.toContain("active:scale");
  });
});

// Regression, user report ("there is this exit button in the artifacts and it also
// looks awful"): the artifacts sheet, an opened artifact and every dialog each spelled
// their own close out again, so the sheet ended up wearing a bordered chip in a strip
// of bordered chips where every other surface wears chrome.
//
// Regression, user report ("can we not have just one close button that looks the same"):
// three components and eight call sites later they were still five different boxes —
// 36×48 on a dialog band, 36×44 on a menu band, 36×44 carrying its own black paper on
// the artifacts strip, 28×28 on a queued turn and on a pasted block, 28×32 over an
// attachment, 40×30 in the fleet search — measured on an iPhone 14 with the shipped
// stylesheet. One component now, with no tone, no size and no hairline to choose.
describe("CloseButton", () => {
  const html = (props: Partial<Parameters<typeof CloseButton>[0]> = {}) =>
    renderToStaticMarkup(
      <CloseButton label="Close artifacts" onClick={() => {}} {...props} />,
    );

  it("is welded to the band it closes, by that band's own hairline", () => {
    expect(html()).toContain("border-l");
    expect(html()).toContain("border-current/20");
    expect(html()).not.toMatch(/class="[^"]*\bborder\s/);
  });

  // Closing is not a destructive act until you mean it.
  it("goes red only under the pointer", () => {
    expect(html()).toContain("hover:bg-err/15");
    expect(html()).toContain("hover:text-err");
    expect(html()).not.toContain('text-err"');
  });

  // Regression, user report ("the headers ... the close button have incorrect width ...
  // even here on the iPhone it's visually so visible ... and all of those have the same
  // problem"): the way out of a whole screen was a 32×32 mark parked in the middle of a
  // 48px band. The hairline that welds it stopped 8px short at both ends, the cell was
  // narrower than its band was tall, and the one gesture that leaves a screen carried a
  // 32×32 target under the app's own 44px minimum. A ✕ inside a composer chip is right
  // at 32; a ✕ that ENDS A BAND is that band's own last cell.
  it("ends a band with the band's own cell, and is a mark inside a control", () => {
    const band = html({ isBand: true });
    expect(band).toContain("w-12");
    expect(band).toContain("mouse:w-9");
    expect(band).toContain("self-stretch");
    // A cell takes its height from the band, so a wrapped three-line question cannot
    // leave paper above and below the way out; it still spells no height of its own.
    expect(/\bh-\d/.test(band), "a height of its own").toBe(false);
    expect(band).not.toContain("self-center");
    expect(band).not.toContain("size-8");

    const mark = html();
    expect(mark).toContain("size-8");
    expect(mark).toContain("mouse:size-6");
    expect(mark).toContain("self-center");
    expect(mark).not.toContain("self-stretch");
    // A square is declared once: no separate width, no separate height. None of the
    // five old boxes survive either.
    expect(/\bw-\d/.test(mark), "a width of its own").toBe(false);
    expect(/\bh-\d/.test(mark), "a height of its own").toBe(false);
    for (const box of [
      "min-h-8",
      "mouse:min-h-6",
      "min-w-9",
      "w-7",
      "min-h-7",
    ]) {
      expect(mark, box).not.toContain(box);
    }
  });

  // The band cell is ONE box and not one per band: it can only be a square because
  // every band that hosts it stands at the same height.
  it("is square because every band that hosts it is one height", () => {
    const bands = {
      "the dialog band": renderToStaticMarkup(
        <DialogHeader
          title="Application settings"
          onClose={() => {}}
          closeLabel="Close Application settings"
        />,
      ),
      "the menu heading": renderToStaticMarkup(
        <MenuHeading onClose={() => {}} closeLabel="Close projects on tower">
          Projects · tower
        </MenuHeading>,
      ),
    };
    for (const [where, markup] of Object.entries(bands)) {
      expect(markup, where).toContain("min-h-12");
      expect(markup, where).toContain("mouse:min-h-9");
      expect(markup, where).toContain("items-stretch");
      expect(markup, where).toContain("self-stretch");
    }
  });

  // Regression, user report ("Why not black like all buttons"): the artifacts sheet has
  // no title band to inherit a foreground from — its one row is the filter strip — so
  // its ✕ used to bring a black block of its own, which was a second look for the same
  // gesture. It inherits the strip's ink like every other ✕ now.
  it("takes only the ink it stands in, on every surface", () => {
    expect(html()).toContain("text-current");
    expect(html()).not.toContain("bg-dialog-title");
    expect(html()).not.toContain("text-dialog-title-foreground");
    // The artifacts sheet had no band to inherit from, so its ✕ read as the one white
    // button in an app whose every other way out is a light mark on the dark band. It
    // wears the band now instead of painting itself.
    expect(artifactsSheetSource).toContain("<DialogHeader");
    expect(artifactsSheetSource).not.toContain("<CloseButton");
    expect(artifactsSheetSource).not.toContain("tone=");
  });

  // A control with no face left to choose has nothing left to disagree about: two ways
  // out of the same KIND may differ in NOTHING but the name of what they leave.
  it("renders the same button wherever it is asked for", () => {
    const chip = renderToStaticMarkup(
      <CloseButton
        label="Remove notes.md"
        disabled={false}
        onMouseDown={() => {}}
      />,
    ).replaceAll("Remove notes.md", "X");
    expect(chip).toBe(
      html({ label: "Clear search" }).replaceAll("Clear search", "X"),
    );

    const dialog = html({ isBand: true, label: "Close artifacts" }).replaceAll(
      "Close artifacts",
      "X",
    );
    const menu = html({
      isBand: true,
      label: "Close projects on tower",
    }).replaceAll("Close projects on tower", "X");
    expect(menu).toBe(dialog);
  });

  it("is named for what it closes", () => {
    expect(html()).toContain('aria-label="Close artifacts"');
  });
});

// Regression, user report ("these two buttons should be up and then this dialog can be
// smaller"): a dialog's verbs lived in a docked footer, and the model picker's two stood
// a screenful of empty panel below the providers they act on. A band's trailing end is a
// run of CELLS — the ✕ already was one — so a verb that belongs to the band stands in the
// same box rather than as a bordered button parked on a title.
describe("BandButton", () => {
  const html = (props: Partial<Parameters<typeof BandButton>[0]> = {}) =>
    renderToStaticMarkup(<BandButton {...props}>Refresh</BandButton>);

  it("is the ✕'s cell with a word in it", () => {
    // Welded by the band's own hairline, and as tall as the band is, so a finger gets
    // the whole 48px height the way out beside it gets.
    expect(html()).toContain("border-l");
    expect(html()).toContain("border-current/20");
    expect(html()).toContain("self-stretch");
    expect(html()).not.toContain("self-center");
    // No height of its own, and no frame of its own: the band spells both.
    expect(/\bh-\d/.test(html()), "a height of its own").toBe(false);
    expect(html()).not.toMatch(/class="[^"]*\bborder\s/);
  });

  it("takes only the ink of the band it stands in", () => {
    expect(html()).toContain("text-current");
    expect(html()).not.toContain("bg-dialog-title");
    expect(html()).not.toContain("text-dialog-title-foreground");
    // A busy verb says so in its own word (`Refreshing…`), so the cell only fades.
    expect(html({ disabled: true })).toContain("disabled:opacity-60");
  });

  // Regression, user report ("why isn't the global save next to that whole close on the
  // header bar"): an opened note kept its own docked footer — a bordered `Button` under
  // the comments, at the far end of the column from the ✕ — after every other dialog verb
  // in the app had become a cell of the band that names what it acts on. The annotator
  // hands ONE cell up, and both surfaces that open a document give it to their band.
  it("is where an opened document's own verb stands, on both surfaces", () => {
    expect(markdownArtifactSource).toContain("<BandButton");
    expect(markdownArtifactSource.match(/onClick={save}/g)).toHaveLength(1);
    // Nothing is docked under the note any more, and the band reports the version.
    expect(markdownArtifactSource).not.toContain("border-t border-dialog-edge px-3 py-3 pb-[max(");
    for (const source of [docSource, artifactsSheetSource]) {
      expect(source).toContain("actions={actions}");
    }
  });
});

// Regression, user report ("still the ⋯ between the machine and project are different
// fix it! MARGIN RIGHT DIFFERS AND ALSO WHY THERE ARE BORDERS"): the two
// kebabs had become the same Button, but each call site still spelled out its own
// popup semantics and glyph, and the app's default bordered box turned a header
// glyph into a second rival to the yellow verb standing beside it.
describe("KebabButton", () => {
  const html = (props: Partial<Parameters<typeof KebabButton>[0]> = {}) =>
    renderToStaticMarkup(<KebabButton label="Actions for tower" {...props} />);

  it("is one control: the machine’s and the project’s render the same box", () => {
    expect(
      html({ label: "Actions for tower" }).replace("Actions for tower", "X"),
    ).toBe(html({ label: "Actions for vis" }).replace("Actions for vis", "X"));
  });

  // Reported: the `⋯` and the row chevrons "have borders". They did — the frame
  // arrived on hover, and on a touch screen a tap counts as hover, so the glyph
  // boxed itself and stayed boxed with no pointer to leave.
  it("wears no border in ANY state: a header glyph is ink, not a rival box", () => {
    expect(html()).toContain("border-transparent");
    expect(html()).not.toContain("border-edge-strong");
    expect(html()).not.toContain("hover:border");
  });

  it("carries the popup semantics itself, so no call site can forget them", () => {
    expect(html()).toContain('aria-haspopup="menu"');
    expect(html({ isOpen: true })).toContain('aria-expanded="true"');
    expect(html()).toContain('aria-label="Actions for tower"');
  });

  it("keeps the header’s compact rhythm and one glyph size", () => {
    expect(html()).toContain(" h-8 ");
    expect(html()).toContain("mouse:h-6");
    expect(html()).not.toContain("active:scale");
  });

  // Over a thumbnail the app's paper is not underneath it, so the same control brings
  // its own ink instead of a call-site `bg-*` that Tailwind's emission order decides.
  // Reported too ("not visible and goes outside of card!"): it wore the EDGE box as
  // well, whose negative right margin reclaims a ROW's trailing gutter. Placed
  // `right-1` on an artifact tile, that margin dragged the glyph past the card's edge.
  it("has an overlay face for the artifact tile, with no height of its own", () => {
    const over = html({ variant: "overlay", density: "default" });
    expect(over).toContain("bg-dialog-title");
    expect(over).toContain("text-dialog-title-foreground");
    // `bg-ink/80` reads as ink and paints near-white in a light theme.
    expect(over).not.toContain("bg-ink/80");
    expect(over).not.toContain("self-center");
    // It ends no row, so it reclaims no row's gutter and centres its own glyph.
    expect(over).not.toContain("-mr-3");
    expect(over).not.toContain("justify-items-end");
    // The row version still does, and that is the only difference between the two.
    expect(html()).toContain("-mr-3");
    expect(html()).toContain("justify-items-end");
  });
});

// Regression, user report ("MARGIN RIGHT DIFFERS AND ALSO WHY THERE IS NO MARGIN
// BEFORE NEW SESSION"): the machine header padded its own right edge while the project
// header one row below ended flush against the screen, and the yellow verb was welded
// to the words beside it. The trailing cluster is one component now, so all three gaps
// are decided once.
describe("HeaderActions", () => {
  const html = renderToStaticMarkup(
    <HeaderActions>
      <HeaderMeta>2 projects</HeaderMeta>
      <KebabButton label="Actions for tower" />
    </HeaderActions>,
  );

  it("owns the right edge of every header in the list", () => {
    expect(html).toContain("pr-3");
    expect(html).toContain("sm:pr-4");
  });

  it("spaces its controls from each other and nothing else", () => {
    expect(html).not.toContain("pl-2");
    expect(html).toContain("gap-2");
  });

  it("never stretches: a header control is centred in whatever row it landed in", () => {
    expect(html).toContain("shrink-0");
    expect(html).toContain("self-center");
  });

  it("is the only one padding that side, so the header stops doing it", () => {
    const band = renderToStaticMarkup(<SectionHeader>project</SectionHeader>);
    expect(band).not.toContain("pr-");
    expect(band).not.toContain("px-");
    // The leading edge belongs to whichever half starts the header, so a pressable
    // one can reach the screen edge with its hover.
    expect(renderToStaticMarkup(<HeaderTitle name="tower" />)).toContain(
      "pl-3",
    );
    expect(renderToStaticMarkup(<HeaderTitle name="tower" />)).toContain(
      "sm:pl-4",
    );
  });
});

describe("LiveCount", () => {
  it("says nothing when nothing is running", () => {
    expect(renderToStaticMarkup(<LiveCount count={0} />)).toBe("");
  });

  it("wears the same pulse a live session row does", () => {
    const html = renderToStaticMarkup(<LiveCount count={3} />);
    expect(html).toContain("animate-pulse bg-ok motion-reduce:animate-none");
    expect(html).toContain("3 live");
  });
});

// Regression, same report: the project header carried a FIXED 160px count column inside
// its own toggle, so on a 390px iPhone the name it exists to show was truncated to
// `~/v…` while "699 sessions" kept every pixel it asked for.
// Regression, user report ("the project should never have the chevron, and never
// a 'Show more' — it should have paging, supported by the backend"): a project
// header used to hide its whole history behind a disclosure, and the history grew
// one endless column through "Show more" over rows the client had downloaded.
describe("Pager", () => {
  // A step is PAINTED when its own slot is not `invisible`; the slot itself is
  // always in the DOM, because a control that vanishes moves its neighbour.
  const isPainted = (html: string, label: string) => {
    const at = html.indexOf(`aria-label="${label}"`);
    expect(at).toBeGreaterThan(-1);
    return !html
      .slice(html.lastIndexOf("<button", at), at)
      .includes("invisible");
  };

  it("renders nothing for a project that fits on one page", () => {
    expect(
      renderToStaticMarkup(
        <Pager page={1} pageCount={1} onPage={() => {}} label="vis sessions" />,
      ),
    ).toBe("");
  });

  // Regression, user report ("on page one there should be no `<`; and what if I
  // want to jump to page 5?"): the band painted a dead disabled step at each end
  // and offered nothing but one-page steps, so page 5 of 73 cost four taps.
  //
  // Regression, user report ("clicking `>` many times, its width changes and I
  // cannot click again to keep going"): dropping the step from the DOM re-centred
  // the band, so stepping off page one slid `>` left out from under the finger
  // already on it. The step is unpainted and unannounced, but its slot stays.
  it("drops the step it cannot take without moving the one it can", () => {
    const first = renderToStaticMarkup(
      <Pager page={1} pageCount={7} onPage={() => {}} label="vis sessions" />,
    );
    expect(first).toContain('aria-label="Pages of vis sessions"');
    expect(first).toContain("Page 1 of 7");
    expect(isPainted(first, "Previous page")).toBe(false);
    expect(isPainted(first, "Next page")).toBe(true);
    expect(first).not.toContain('disabled=""');
    // The unpainted slot is held, unannounced, and out of the tab order.
    expect(first).toContain('aria-hidden="true"');
    expect(first).toContain('tabindex="-1"');
    // Both ends are fixed and only the numbers between them breathe.
    expect(first).toContain("flex-1 items-center justify-center");
    expect(first).not.toContain("justify-center gap-1 border-t");

    const last = renderToStaticMarkup(
      <Pager page={7} pageCount={7} onPage={() => {}} label="vis sessions" />,
    );
    expect(isPainted(last, "Previous page")).toBe(true);
    expect(isPainted(last, "Next page")).toBe(false);
  });

  // Regression, user report: the `<` and `>` were spread the full width of the
  // list, "too much and hard to click" — 360px apart on a phone, so no thumb can
  // reach both and click-click-click through pages. The shelf still runs the full
  // width; the control on it is content-sized and held at its trailing end.
  //
  // Regression, user report with a screenshot of a phone: the capped cluster
  // (`w-full max-w-[19rem]`) could not keep the promise the cap was for. A flex item
  // cannot shrink below its own content, so on page 4 — where the window opens to
  // `1 2 3 4 5 … 80`, 319px of a 304px cap — the box overflowed and `>` sat 15px
  // right of where it sits on every other page, outside the trailing column.
  it("keeps the two steps within a thumb's reach of the numbers", () => {
    const html = renderToStaticMarkup(
      <Pager page={4} pageCount={73} onPage={() => {}} label="vis sessions" />,
    );
    // Content-sized: `>` ends the cluster, the cluster ends at the shelf's trailing
    // edge, so the window can only breathe to the LEFT.
    expect(html).toContain('class="flex items-center gap-1"');
    expect(html).not.toContain("max-w-[19rem]");
    // It never grows and never negotiates a basis on the shelf's wrapping row; from
    // `sm` up, where the numbers are painted, it takes the shelf's trailing end.
    expect(html).toContain('class="flex min-w-0 shrink-0 justify-end sm:grow"');
  });

  // Regression, user report with a screenshot of a phone ("this is not looking
  // good"): on page 4 of a 798-session project the shelf was TWO lines with a hole
  // in it — the count alone on the first beside 300px of empty paper, the numbers
  // alone on the second — and the sticky strip grew from 41px to 59px as the reader
  // paged, measured at 430px. A phone line cannot hold both: at 390px the 115px
  // count and the 304px window need 431px of 362px.
  it("says the position in six characters where the numbers cannot fit", () => {
    const html = renderToStaticMarkup(
      <Pager page={4} pageCount={80} onPage={() => {}} label="vis sessions" />,
    );
    // The phone form: `4 / 80`, in the shelf's own meta voice, in a box whose width
    // is the same on every page of every project — so the shelf holds its height.
    expect(html).toContain("min-w-14");
    expect(html).toContain("tabular-nums");
    expect(html).toContain("4 / 80");
    // The two forms are exclusive, and the numbers are the half that gives way.
    const label =
      /<span aria-hidden="true" class="([^"]*)"/.exec(html)?.[1] ?? "";
    expect(label).toContain("sm:hidden");
    expect(html).toContain(
      'class="hidden flex-1 items-center justify-center gap-1 sm:flex"',
    );
    // `display: none` takes the numbers out of the accessibility tree too, so the
    // position is announced ONCE, by the live region both forms are drawn from.
    expect(html.match(/Page 4 of 80/g)?.length).toBe(1);
    expect(html).toContain('<span aria-live="polite" class="sr-only">');
    // Both steps are still there to walk with.
    expect(isPainted(html, "Previous page")).toBe(true);
    expect(isPainted(html, "Next page")).toBe(true);
  });

  it("makes every printed page a one-tap jump, current one marked", () => {
    const html = renderToStaticMarkup(
      <Pager page={5} pageCount={73} onPage={() => {}} label="vis sessions" />,
    );
    for (const n of [1, 4, 5, 6, 73]) {
      expect(html).toContain(`aria-label="Page ${n}"`);
    }
    expect(html).toContain('aria-current="page"');
  });

  // A strip of 73 numbers does not fit a 390px phone, and a gap marker that hides
  // exactly one page is a lie that costs a tap.
  it("windows the numbers around the current page and pins both ends", () => {
    expect(pageWindow(1, 1)).toEqual([1]);
    expect(pageWindow(3, 5)).toEqual([1, 2, 3, 4, 5]);
    expect(pageWindow(5, 73)).toEqual([1, null, 4, 5, 6, null, 73]);
    expect(pageWindow(1, 73)).toEqual([1, 2, null, 73]);
    expect(pageWindow(72, 73)).toEqual([1, null, 71, 72, 73]);
    expect(pageWindow(4, 73)).toEqual([1, 2, 3, 4, 5, null, 73]);
  });
});

// Regression, user report ("I think there's no visual differentiation between the
// paging and also there is no visual differentiation between the projects and it all
// looks like kind of the same thing"): the pager was painted at the FOOT of a group's
// rows — `border-t border-dialog-edge` on the rows' own paper, one hairline above the
// next project's header — so `1 2 … 80 ›` read as the last row of `vis` or the first
// row of `vis-companion`. It rides on the header's shelf now, and two projects are
// separated by a trough instead of by the hairline two sessions share.
describe("SectionShelf", () => {
  const html = renderToStaticMarkup(
    <SectionShelf>
      <HeaderMeta>
        <HeaderTally count={794} unit="session" />
      </HeaderMeta>
      <Pager page={2} pageCount={100} onPage={() => {}} label="vis sessions" />
    </SectionShelf>,
  );

  const face = /^<div class="([^"]*)"/.exec(html)?.[1] ?? "";

  it("stands on the header's paper, not on the rows'", () => {
    expect(face).toContain("bg-level-project");
    expect(face).not.toContain("bg-panel");
    // One closing hairline for the header/shelf pair, and none above it: a rule
    // between a band and its own shelf is the doubled line this list was reported for.
    expect(face).toContain("border-b border-dialog-edge");
    expect(face).not.toContain("border-t");
  });

  it("hangs under the band at exactly the band's own height, and behind it", () => {
    const band = renderToStaticMarkup(<SectionHeader>project</SectionHeader>);
    expect(band).toContain("min-h-13");
    expect(face).toContain("sticky top-13");
    expect(band).toContain("mouse:min-h-9");
    expect(face).toContain("mouse:top-9");
    // A group scrolling away passes its shelf UNDER the next header.
    expect(band).toContain("z-10");
    expect(face).toContain("z-9");
  });

  it("is shorter than the band it hangs from", () => {
    expect(face).toContain("min-h-9");
    expect(face).toContain("mouse:min-h-8");
  });

  // Regression, user report with a screenshot of a phone: the shelf wrapped as soon
  // as the pager's window opened, so it was 41px on page 1 and 59px on page 4 — a
  // STICKY strip changing height under the thumb that pressed it, with the count
  // stranded beside 300px of empty paper. The pager carries a fixed-width phone form
  // now, so both halves share one line at 320px.
  it("holds one line, and wraps rather than crushing either of its two halves", () => {
    const pager = renderToStaticMarkup(
      <Pager page={4} pageCount={100} onPage={() => {}} label="vis sessions" />,
    );
    // The phone form is 56px on every page: 115px of count and 132px of pager fit a
    // 320px line with room to spare.
    expect(pager).toContain("min-w-14");
    expect(pager).toContain("shrink-0");
    // Kept for the honest case only — a count so long it cannot share the line.
    expect(face).toContain("flex-wrap");
    expect(html).toContain("794 sessions");
    expect(face).not.toContain("truncate");
  });

  it("owns the list's two edges, so the pager spells neither", () => {
    expect(face).toContain("pl-3");
    expect(face).toContain("sm:pl-4");
    expect(face).toContain("pr-3");
    expect(face).toContain("sm:pr-4");
    const pager = renderToStaticMarkup(
      <Pager page={2} pageCount={100} onPage={() => {}} label="vis sessions" />,
    );
    expect(pager).not.toContain("pl-3");
    expect(pager).not.toContain("pr-3");
    expect(pager).not.toContain("border-t border-dialog-edge");
    // It never grows on the phone line it shares with the count, and takes the
    // shelf's trailing end from `sm` up, where its numbers are painted.
    expect(pager).toContain(
      'class="flex min-w-0 shrink-0 justify-end sm:grow"',
    );
  });

  it("carries the group's count, which the header's cluster no longer does", () => {
    const shelf = /<SectionShelf>[\s\S]*?<\/SectionShelf>/.exec(
      sessionsListSource,
    )?.[0];
    expect(shelf).toContain(
      '<HeaderTally count={sessions.length} unit="session" />',
    );
    expect(shelf).toContain("<LiveCount count={liveCount} />");
    expect(shelf).toContain("<Pager page={shownPage}");
    // ...and it is the ONLY place either of them is rendered on this screen.
    expect(sessionsListSource.match(/<Pager /g)?.length).toBe(1);
    // Scoped to the PROJECT header: the fleet view's machine band keeps a tally of
    // its own, which is a different section counting a different thing.
    const projectBand = /<ProjectCrumb[\s\S]*?<\/SectionHeader>/.exec(
      sessionsListSource,
    )?.[0];
    expect(projectBand).toContain("<NewSessionButton");
    expect(projectBand).not.toContain("HeaderTally");
    expect(projectBand).not.toContain("LiveCount");
    // The loading band stands in for a shelf too, or the list jumps by one when the
    // rows land.
    expect(sessionsListSource).toMatch(/<SectionShelf>\s*<SkeletonBar/);
  });
});

// Regression, same report ("no visual differentiation between the projects"): a
// project boundary was ONE hairline — the same hairline two sessions of one project
// are separated by — so four checkouts read as one long list.
describe("SectionGap", () => {
  const html = renderToStaticMarkup(<SectionGap />);

  it("is 8px of the machine's own paper, one step deeper than the card", () => {
    expect(html).toContain("h-2");
    expect(html).toContain("bg-level-machine");
    expect(html).toContain('aria-hidden="true"');
  });

  it("opens every group but the first, and every machine but the first", () => {
    expect(sessionsListSource).toContain("{groupIndex > 0 && <SectionGap />}");
    expect(sessionsListSource).toContain(
      "{sectionIndex > 0 && <SectionGap />}",
    );
  });
});

describe("HeaderTally", () => {
  // Regression, user report: the phone header printed "725" over a list of
  // sessions and dropped the word "sessions" to win back width. A number with no
  // noun is not a shorter sentence, it is a different one.
  it("prints the number AND its noun on every screen", () => {
    const html = renderToStaticMarkup(
      <HeaderTally count={699} unit="session" />,
    );
    expect(html).not.toContain("hidden sm:inline");
    expect(html).not.toContain("sr-only");
    expect(html).toContain("699");
    expect(html).toContain("sessions");
  });

  it("counts one of a thing in the singular", () => {
    const html = renderToStaticMarkup(<HeaderTally count={1} unit="project" />);
    expect(html).toContain("1");
    expect(html).toContain("project");
    expect(html).not.toContain("projects");
  });
});

// Regression, user report ("some things are having margin left like the ⋯ then
// chevrons to open the session details are not having — i dnt want these margins"):
// measured on a 390px iPhone, the machine's mark began at x=14 but its NAME at 28,
// the project's name at 36, and a session's title at 10 — the deepest thing on the
// screen starting furthest left, so depth read backwards. On the other side the two
// header `⋯` stopped at x=378 while the session row's disclosure ran flush to 390.
// Regression, user report: the machine name had to be editable from the list
// itself ("clicking on machine name to edit it and save"), and the edit must not
// move anything. The resting name and the field it becomes are one box: same class
// list, and the field is sized in CHARACTERS of the header's mono face, so the
// address beside it does not shift when the caret arrives.
describe("HeaderTitle rename", () => {
  const resting = renderToStaticMarkup(
    <HeaderTitle name="tower" qualifier="10.0.0.5:7890" />,
  );
  const editable = renderToStaticMarkup(
    <HeaderTitle
      name="tower"
      qualifier="10.0.0.5:7890"
      onRename={() => {}}
      renameLabel="Rename tower"
    />,
  );

  it("is INK, not a second control: the name keeps its own face", () => {
    const face = "font-mono font-bold text-white";
    expect(resting).toContain(face);
    expect(editable).toContain(face);
    // Truncation moved onto the name itself when the qualifier left its line.
    expect(resting).toContain("min-w-0 truncate");
    expect(editable).toContain("min-w-0 truncate");
    // No border, no box, no height of its own — anything that paints a frame
    // around the name is a control competing with the two buttons beside it.
    expect(editable).not.toContain("border");
    expect(editable).not.toContain("min-h-");
  });

  it("names the press for a screen reader and stays a name without the callback", () => {
    expect(editable).toContain('aria-label="Rename tower"');
    expect(editable).toContain("<button");
    expect(resting).not.toContain("<button");
  });
});

describe("the list grid", () => {
  const leading = (html: string) =>
    html.includes("pl-3") && html.includes("sm:pl-4");

  it("starts every header on one leading edge", () => {
    expect(
      leading(
        renderToStaticMarkup(
          <HeaderTitle
            mark={<MachineMark color={MACHINE_COLORS[0]!} />}
            name="tower"
          />,
        ),
      ),
    ).toBe(true);
    // Both levels are the same component now: the project header stopped being a
    // disclosure when its history moved onto a pager.
    expect(
      leading(
        renderToStaticMarkup(
          <HeaderTitle
            name="vis"
            qualifier="~/vis"
            qualifierTitle="/Users/dev/vis"
          />,
        ),
      ),
    ).toBe(true);
  });

  // Regression, user report ("this should go under", drawn on the `vis` header): the
  // project header set its path on the SAME line as the folder name, so the two
  // shared what the trailing cluster left — the name was capped at 60% of that
  // remainder and the path truncated mid-token (`~/vis/apps/vis-c…` on a phone).
  it("stacks a qualifier UNDER the name instead of rationing one line with it", () => {
    const html = renderToStaticMarkup(
      <HeaderTitle
        name="companion"
        qualifier="~/vis/apps/vis-companion"
        qualifierTitle="/Users/dev/vis/apps/vis-companion"
      />,
    );

    expect(html).toContain("flex-col");
    // The two ways the old single line was paid for, both gone.
    expect(html).not.toContain("items-baseline");
    expect(html).not.toContain("max-w-[60%]");
    // Name first, path under it, and each line still truncates in the column the
    // trailing cluster leaves rather than pushing the header wider.
    expect(html.indexOf(">companion<")).toBeLessThan(
      html.indexOf("~/vis/apps"),
    );
    expect(html.split("truncate").length - 1).toBe(2);
    expect(html).toContain('title="/Users/dev/vis/apps/vis-companion"');
    // The loading band stands in for the SAME two lines, through the same two
    // slots: a one-line skeleton grows by a line the moment data lands.
    expect(sessionsListSource).toMatch(/name=\{<SkeletonBar type="text-title"/);
    expect(sessionsListSource).toMatch(
      /qualifier=\{\s*<SkeletonBar type="text-chip"/,
    );
  });

  // The last 8px of the same misalignment: a mark sized to its own ink moved the
  // header NAME beside it.
  // Regression, user report ("on iPhone its not aligned properly"): the glyph
  // column existed only when something filled it, so the machine's NAME began at
  // x=36 behind its hue block while the project header one row below — which has
  // no mark since its disclosure became a pager — began at x=14. The deeper row
  // started further left, and the two names never read as a hierarchy.
  it("reserves one glyph column, marked or not", () => {
    const marked = renderToStaticMarkup(
      <HeaderTitle
        mark={<MachineMark color={MACHINE_COLORS[0]!} />}
        name="tower"
      />,
    );
    const bare = renderToStaticMarkup(
      <HeaderTitle name="vis" qualifier="~/vis" />,
    );
    for (const html of [marked, bare]) {
      expect(html).toContain("grid size-3.5 shrink-0 place-items-center");
    }
  });

  // The trailing gutter lives INSIDE the last control, not on the cluster: a box
  // that respects the gutter and then centres a 12px glyph in 28px of its own put
  // the right-hand INK 30px from the paper while the left-hand ink sat at 19px.
  it("ends every row on one trailing edge, carried by the control itself", () => {
    // The cluster owns the gutter unconditionally — a project header drops its `⋯`
    // while a filter is live, and the amber verb must not then run to the paper.
    const cluster = renderToStaticMarkup(<HeaderActions>x</HeaderActions>);
    expect(cluster).toContain("pr-3");
    expect(cluster).toContain("sm:pr-4");

    for (const html of [
      renderToStaticMarkup(<KebabButton label="Actions for vis" />),
      renderToStaticMarkup(
        <RowDisclosure isOpen={false} label="Show details" />,
      ),
    ]) {
      expect(html).toContain("pr-3");
      expect(html).toContain("sm:pr-4");
      expect(html).toContain("justify-items-end");
      // ...and reclaims the cluster's gutter, so the BOX reaches the paper while the
      // GLYPH stops where the leading glyph starts.
      expect(html).toContain("-mr-3");
      expect(html).toContain("sm:-mr-4");
      // It ends ON the paper's edge, which already draws that line.
      expect(html).toContain("border-r-0");
    }
  });
});

// The disclosure is the `⋯`'s sibling — the rarer FACTS of a row where the kebab
// holds its rarer VERBS — so it is the same box in the same column, not a hand-built
// strip welded to the screen edge at 40% opacity.
describe("RowDisclosure", () => {
  const html = (isOpen: boolean) =>
    renderToStaticMarkup(
      <RowDisclosure isOpen={isOpen} label="Show details for Untitled" />,
    );

  it("is the same button as the kebab beside it", () => {
    const kebab = renderToStaticMarkup(<KebabButton label="Actions for vis" />);
    for (const token of [
      "min-w-10",
      "sm:min-w-12",
      "mouse:min-w-10",
      " h-8 ",
      "mouse:h-6",
    ]) {
      expect(html(false)).toContain(token);
      expect(kebab).toContain(token);
    }
  });

  it("names what it opens and reports whether it is open", () => {
    expect(html(false)).toContain('aria-expanded="false"');
    expect(html(true)).toContain('aria-expanded="true"');
    expect(html(false)).toContain('aria-label="Show details for Untitled"');
  });

  it("never rests on an opacity that would fail contrast while it does", () => {
    expect(html(false)).not.toContain("opacity-40");
  });
});

// The dialog that "Manage projects" opens arrived by dropping into the middle of
// the glass while the `⋯` menu beside it slid up from the bottom edge: two layers
// with the same job and two physics. Below `sm:` a dialog is a SHEET.
// Regression, user report ("dialogs should occupy full height on the iPhone, and on
// desktop they should have similar heights and widths"): the sheet stopped at its
// content's height and `size` gave two different desktop widths.
describe("Modal and DialogFrame as a phone sheet", () => {
  const source = uiSource;

  it("lets the sheet take the whole glass on a phone and centres it from sm: up", () => {
    expect(source).toContain(
      "${position} inset-0 z-50 flex justify-center bg-ink/85",
    );
    expect(source).toContain("'items-end' : 'items-stretch'");
    expect(source).toContain("sm:items-center");
    // No padding at all on the phone: a sheet touches all four edges.
    expect(source).toContain("sm:pb-[max(1rem,env(safe-area-inset-bottom))]");
  });

  it("gives every dialog ONE desktop box and a full-height phone sheet", () => {
    expect(source).toContain(
      "DIALOG_DESKTOP_HEIGHT = 'sm:h-[min(38rem,100%)]'",
    );
    expect(source).toContain("'sm:max-w-4xl' : 'sm:max-w-xl'");
    expect(source).toContain("'max-h-full sm:h-auto' : DIALOG_DESKTOP_HEIGHT");
    // One width for every dialog that asks ONE question, so a question and a file
    // browser are the same rectangle. `wide` is the settings box and nothing else:
    // two columns side by side is a LAYOUT, and it is the only one in the app.
    expect(source).not.toContain("sm:max-w-md");
    expect(source).not.toContain("sm:max-w-lg");
    expect(settingsSource).toContain('<Modal size="wide" onDismiss={onClose}>');
  });

  it("slides the frame in from below by its own height, and only tips in on desktop", () => {
    const html = renderToStaticMarkup(
      <DialogFrame title="Manage projects">body</DialogFrame>,
    );
    expect(html).toContain("starting:translate-y-full");
    expect(html).toContain("sm:starting:translate-y-2");
    // The menu sheet's own top edge, so the two layers read as one family.
    expect(html).toContain("border-t-2 border-accent");
    expect(html).toContain("sm:border sm:border-dialog-edge");
    // Full-bleed on the phone, it owns BOTH safe areas itself.
    expect(html).toContain("pt-[env(safe-area-inset-top)]");
    expect(html).toContain("pb-[env(safe-area-inset-bottom)]");
    expect(html).toContain("sm:pb-0");
    // A column that fills its parent, so the body scrolls and the footer docks.
    expect(html).toContain("flex min-h-0 flex-1 flex-col");
  });
});

// Regression, user report ("the cog dialog looks slightly better than manage
// projects — make manage projects use the same outer component, canonicalize"):
// application settings hand-rolled its own scrim and its own `<section>` beside
// `Modal`/`DialogFrame`, so the app had two dialogs with two heights, two widths
// and two entrances. There is one now, and it wears the better glass of the two.
describe("one outer dialog component", () => {
  const settings = settingsSource;

  it("opens settings in Modal + DialogFrame like every other dialog", () => {
    expect(settings).toContain('<Modal size="wide" onDismiss={onClose}>');
    expect(settings).toContain("<DialogFrame");
    // No second scrim, no second dialog box, no second close button.
    expect(settings).not.toContain("fixed inset-0 z-50");
    expect(settings).not.toContain('aria-modal="true"');
    expect(settings).not.toContain(
      "max-h-[calc(100%-env(safe-area-inset-top))]",
    );
  });

  it("keeps the glass that dialog had, in the one Modal", () => {
    expect(uiSource).toContain("bg-ink/85 backdrop-blur-[2px]");
    expect(uiSource).toContain("starting:opacity-0");
  });
});

// Regression, user report ("unified settings"): the app had TWO settings dialogs
// that could never be open at once — the cog's application settings, and a machine's
// own settings behind a `⋯` three screens away — so "where do I change this?" was
// answered by remembering which of two doors a choice lived behind, and pairing a
// machine was filed under the device while the machine it produced was filed
// somewhere else. There is one dialog: this device on the left, the machines on the
// right.
describe("settings is ONE dialog with two columns", () => {
  const settings = settingsSource;

  it("is the only settings dialog in the app", () => {
    // The gateway half is a COLUMN BODY now, not a dialog of its own.
    expect(settings).toContain("function GatewayPanels");
    expect(settings).not.toContain("function GatewaySettingsDialog");
    expect(settings).not.toContain("function ApplicationSettingsDialog");
    expect(settings).toContain("export function SettingsDialog");
  });

  it("puts the columns side by side with room, and stacks them on a phone", () => {
    expect(settings).toContain("grid-cols-1");
    expect(settings).toContain("sm:grid-cols-2");
    // One rule between the columns on desktop, one between the stacked halves on
    // a phone — never both at once.
    expect(settings).toContain("sm:divide-x");
    expect(settings).toContain("sm:divide-y-0");
  });

  it("pairs a machine from the band's own word, over the column rather than inside it", () => {
    // The verb used to be a button that CLOSED this dialog and navigated to the
    // machines screen. Then it was two pairing cards standing permanently open
    // under the list, so the column opened on forms for a machine that does not
    // exist yet, and the fleet the cog was pressed FOR started below them. Then it
    // was an amber ＋ — the mark this app already spends on a NEW SESSION, so one
    // glyph meant two things and it was the only thing in the band with a face.
    // It is the band's one WORD now, and what it opens is the app's own `fit` sheet.
    const band = settings.slice(
      settings.indexOf("<SettingsColumn"),
      settings.indexOf("{/* THE COG"),
    );
    expect(band).toContain('variant="primary"');
    expect(band).toContain('density="compact"');
    expect(band).toContain("Add a machine");
    expect(settings).not.toContain("<PlusIcon");
    const sheet = settings.slice(settings.indexOf('<Modal size="fit"'));
    expect(sheet).toContain('title="Add a machine"');
    expect(sheet).toContain("<AddMachine");
    expect(settings).not.toContain(">Pair machine</Button>");
    expect(settings).not.toContain("onPair");
  });

  it("welds a machine's opened panels to its row with a rule, and keeps each band's line short", () => {
    // Reported: "providers don't look good, there is no border on top, some
    // overlong needless texts". The panel stack only DIVIDED between groups, so
    // the first band (Providers) met the machine row it belongs to with no
    // hairline at all, and the two longest descriptions wrapped to three and four
    // lines on a 390px phone above rows 48px tall.
    const stack = settings.slice(
      settings.indexOf("touch-pan-y"),
      settings.indexOf("<ProvidersPanel"),
    );
    expect(stack).toContain("border-t border-dialog-edge");
    for (const description of settings.matchAll(/description="([^"]+)"/g))
      expect(description[1].length).toBeLessThanOrEqual(80);
  });

  it("leaves the providers band its title and its verb, and nothing else", () => {
    // Reported over this screenshot: the providers description says nothing the
    // rows do not, and "8/8 SIGNED IN" is a count of a list already on screen —
    // both crowded the one band whose rows each carry their own status. The
    // Reported over this screenshot: the two bands wore the SAME amber button, so
    // the providers band read as a second machines band. The machines column keeps
    // its full-size primary verb; the providers band, nested one level in, drops to
    // a quiet face so it sits under the column it belongs to instead of competing.
    const band = settings.slice(
      settings.indexOf("<SettingsColumn"),
      settings.indexOf("{/* THE COG"),
    );
    expect(band).toContain('aria-label="Add a machine"');
    expect(band).toContain('variant="primary"');
    expect(band).toContain(">\n                Add a machine\n              </Button>");
    // Then the quiet word in the band was still a 42px chip at the TOP of a list
    // it appends to the BOTTOM of: it left the band entirely and became the list's
    // own last row, full width under the last provider.
    expect(settings).not.toContain("action={<AddProviderButton");
    const providerButton = providerAuthSource.slice(
      providerAuthSource.indexOf("A ROW, NOT A CHIP"),
      providerAuthSource.indexOf("{isPicking &&"),
    );
    // Reported next over the same screenshot: that last row is the panel's one
    // action, so it carries the amber the band's action used to carry.
    expect(providerButton).toContain('variant="primary"');
    expect(providerButton).not.toContain('variant="quiet"');
    expect(providerButton).toContain("w-full justify-center");
    expect(providerButton).toContain("Add a provider");
  });

  // Reported over the same screenshot: the same song for MCP.
  it("strips the MCP band to its title and lands its verb as the list's last row", () => {
    // MCP SERVERS carried a sentence and a "0 configured" counter over a list that
    // states its own emptiness, and its verb was a chip floating in a padded box.
    const mcp = settings.slice(
      settings.indexOf('title="MCP servers"'),
      settings.indexOf("MCP transport"),
    );
    expect(mcp).not.toContain("description=");
    expect(mcp).not.toContain("meta=");
    expect(mcp).toContain('variant="primary"');
    expect(mcp).toContain("w-full justify-center");
    expect(mcp).toContain("Add an MCP server");
  });

  // Reported over the same screenshot: the voice bands were the worst of the lot,
  // and a reader asked what an `Off` switch for spoken replies is even for.
  it("unboxes the voice lists and drops the silence that nothing needed", () => {
    // Every voice, and every engine, sat in its own hairline box inside a padded
    // box inside the panel — three frames deep for one row of text.
    const voices = settings.slice(
      settings.indexOf('title="Voices"'),
      settings.indexOf("How far one engine has got"),
    );
    expect(voices).not.toContain("description=");
    expect(voices).not.toContain("meta=");
    expect(voices).not.toContain("bg-panel-2");
    expect(voices).toContain("w-full justify-center");
    expect(voices).toContain("Import a voice…");
    const engines = settings.slice(
      settings.indexOf("function EngineRow"),
      settings.indexOf('title="Speech engines"'),
    );
    expect(engines).not.toContain("bg-panel-2");
    // Nothing speaks unless this device started the turn by voice, so `Off` named
    // a state the reader already has by not talking to it. The band ROUTES.
    const spoken = settings.slice(
      settings.indexOf("SPEECH_ROUTE_FACES"),
      settings.indexOf("function SettingsColumn"),
    );
    expect(spoken).not.toContain('off:');
    expect(spoken).not.toContain("description=");
  });
  // Reported over this screenshot: why is that button not simply full width on a
  // phone, and the green dots do not line up with the text.
  it("gives a lone verb the phone's full width and rides each status dot on the name's line", () => {
    // The notification verb hugged the right edge of a full-bleed panel with
    // nothing beside it, so it read as the leftover of a row that lost its text.
    const notify = uiSource.slice(
      uiSource.indexOf("export function NotifyConnectionRow"),
      uiSource.indexOf("THE ✕, AND THERE IS EXACTLY ONE OF IT"),
    );
    expect(notify).toContain("w-full justify-center sm:w-auto");
    // And a dot centred in a two-line row sat between the name and its meta line,
    // marking neither. Both lists give it the NAME's own type step — `text-body`
    // is the 18px line box, so no hand-set `leading-*` is needed and the type
    // scale keeps owning the rhythm (`scripts/touch-density.test.mjs`). The
    // two-line provider row also pins it to the first line.
    expect(machinesSource).toContain("shrink-0 font-mono text-body");
    expect(providerAuthSource).toContain("shrink-0 self-start font-mono text-body");
  });

  it("hides a machine's own verbs behind its row, and keeps no panel of them", () => {
    // `Saved connection` stood open under the list holding a name field, `Make
    // primary` and `Forget this machine`: three controls for ONE machine, always
    // on screen, aimed at whichever row the column happened to be READING.
    expect(settings).not.toContain('title="Saved connection"');
    expect(settings).not.toContain("Forget this machine");
    expect(settings).toContain("onMakePrimary={onMakePrimary}");
    expect(settings).toContain("onForget={onRemove}");
    // And every verb names the machine it acts on, rather than the read one.
    expect(settings).toContain("onMakePrimary?: (conn: GatewayConn)");
    expect(settings).toContain("onRemove?: (conn: GatewayConn)");
  });

  it("hides each machine's settings under that machine's own row", () => {
    // The column used to paint ONE machine's panels under the WHOLE list — the
    // machine whose row was pressed last, marked `CURRENT` — so a press replaced
    // the settings already on screen instead of opening the row it landed on.
    expect(settings).toContain("<MachineRows");
    expect(settings).toContain("openUrls={openUrls}");
    expect(settings).toContain("renderPanel={(conn) => (");
    expect(settings).not.toContain("onSelectGateway");
    expect(settings).not.toContain("activeUrl");
  });

  it("gives each column its own scroll on desktop", () => {
    // The grid stops at the dialog's height; the column bodies do the scrolling, so
    // reaching a machine's last panel never drags Theme off the top of the screen.
    expect(settings).toContain("sm:overflow-hidden");
    expect(settings).toContain("sm:overflow-y-auto");
    expect(settings).toContain("sm:min-h-0");
  });
});

// The user's ask for the machines column ("more hidden, triggered by some action,
// button, icon like maybe with the slide like the star in the session") gave a
// machine the session row's verbs — and the destructive one has to ask. The
// session list had been asking IN the row for a while, hand-built at the call
// site; two copies of one question is how two answers end up different sizes.
describe("the confirm that IS the row", () => {
  it("is one control, and both lists ask through it", () => {
    expect(sessionsListSource).toContain("<ConfirmRow");
    expect(machinesSource).toContain("<ConfirmRow");
    // Neither screen paints the red wash of the committing half any more.
    expect(sessionsListSource).not.toContain("bg-err-surface");
    expect(machinesSource).not.toContain("bg-err-surface");
  });

  it("puts the refusal first and spends the red on the commitment alone", () => {
    const html = renderToStaticMarkup(
      <ConfirmRow
        question="Delete alpha?"
        confirmLabel="Yes, delete"
        onKeep={() => {}}
        onConfirm={() => {}}
      />,
    );
    const [keep, commit] = html.split("<button").slice(1);
    expect(keep).toContain("No, keep");
    expect(keep).not.toContain("bg-err-surface");
    expect(commit).toContain("Yes, delete");
    expect(commit).toContain("bg-err-surface");
    expect(commit).toContain("text-err-ink");
    // The question is the group's own label: the row it stands in for is still
    // on screen, so only a reader who cannot see it needs it spelled out.
    expect(html).toContain('aria-label="Delete alpha?"');
    // Both answers stand a row tall — 48px under a finger, 32px under a cursor.
    expect(html).toContain("min-h-12");
    expect(html).toContain("mouse:min-h-8");
  });

  // Reported over a machine's providers ("why do we not have a border here when
  // removing?"): the block REPLACES the row it is asking about, so the only
  // edges around it were the list's own neutral 1px dividers — the same rule two
  // calm rows share — and the cost sentence, hand-built at the call site, hung
  // under the PREVIOUS provider and read as that row's meta line.
  it("is a BOX in the ink it asks in, and the cost stands inside it", () => {
    const html = renderToStaticMarkup(
      <ConfirmRow
        question="Remove Codex?"
        cost="Signs out on the gateway machine."
        confirmLabel="Yes, remove"
        onKeep={() => {}}
        onConfirm={() => {}}
      />,
    );
    // Four sides in the destructive edge, and the box's own top rule stands IN
    // FOR the list rule above it rather than stacking on it.
    expect(html).toContain('class="-mt-px border border-err-edge"');
    expect(html).toContain("Signs out on the gateway machine.");
    // The rule between the sentence and the answers belongs to the sentence, so
    // both answers keep the whole 48px a finger is owed.
    expect(html).toContain("border-b border-err-edge px-3 py-2");
    expect(html).toContain("flex min-h-12 items-stretch mouse:min-h-8");
    // A question with nothing to spell out is the same box, minus that rule.
    const bare = renderToStaticMarkup(
      <ConfirmRow
        question="Delete alpha?"
        confirmLabel="Yes, delete"
        onKeep={() => {}}
        onConfirm={() => {}}
      />,
    );
    expect(bare).toContain('class="-mt-px border border-err-edge"');
    expect(bare).not.toContain("<p");
  });

  it("owns the sentence both lists used to build at the call site", () => {
    for (const source of [providerAuthSource, machinesSource]) {
      expect(source).toContain("cost=");
      // The paragraph that used to stand OUTSIDE the frame, in each screen's
      // own words.
      expect(source).not.toContain("px-3 pt-2 font-mono text-chip");
    }
  });
});

// Regression, user report ("why doesn't the delete button on a project have the
// hover of the full height of its parent?"): an `edge` IconButton wore the compact
// scale's fixed 32px face and centred it, so the trash ending a "Manage projects"
// row painted a floating hover band inside a taller row, with a dead strip above
// and below it. A control that ENDS a row hovers the row.
describe("a row-ending icon button fills its row", () => {
  // The class list `edge` puts on the box, read out of the source it is written in.
  const edgeBox = () => {
    const edge = uiSource.slice(uiSource.indexOf("const box = edge"));
    return edge.slice(0, edge.indexOf("\n", edge.indexOf("?")));
  };

  it("stretches instead of centring a fixed face", () => {
    const line = edgeBox();
    expect(line).toContain("h-auto");
    expect(line).toContain("self-stretch");
    // A stretched box needs no invisible reach for its target.
    expect(line).toContain("after:content-none");
  });

  // Regression, user report ("during the search the > are not having correct line
  // height"): the cancellation was spelled once, unvarianted, so at mouse density the
  // compact scale's own `mouse:h-6` outlived it. Measured on the 1440px desktop list,
  // a session row ran 239–271 and its disclosure 239–263 — a 24px slab pinned to the
  // TOP of a 32px row, its chevron four pixels above the title it belongs to and its
  // hover band stopping eight pixels short of the row's rule.
  it("cancels the fixed face at EVERY density, not only the default one", () => {
    const line = edgeBox();
    expect(line).toContain("mouse:h-auto");
  });
});

// Regression, user report ("session element right side before the > has no enough
// padding right because hovering over the element looks awful"): the pressable half of
// a session row painted its hover slab to x=340 and its own facts ended at 340 too, so
// the ink sat on the boundary of its own highlight, a hair from the `›`.
describe("a row's pressable slab", () => {
  it("pads its trailing inside edge exactly as LIST_EDGE pads the leading one", () => {
    expect(uiSource).toContain("export const LIST_EDGE = 'pl-3 sm:pl-4';");
    expect(uiSource).toContain("export const LIST_EDGE_END = 'pr-3 sm:pr-4';");
  });

  it("never lets a row spell that padding itself", () => {
    expect(sessionsListSource).not.toContain("pr-3 sm:pr-4");
    expect(sessionsListSource).toContain("${LIST_EDGE} ${LIST_EDGE_END}");
  });
});

// Regression, user report ("there is too much margin between the > for a session and
// the live/idle stuff"): the trailing cluster added a gutter of its OWN in front of a
// control that already carries its hit box as padding, so on a 390px iPhone the status
// ink stopped at 328 and the chevron's glyph started at 362 — a 34px hole on the left
// of a mark sitting 13px from the paper on its right.
describe("the trailing control cluster", () => {
  it("adds no gutter of its own in front of the first control", () => {
    expect(uiSource).toContain(
      "const LIST_TRAIL = 'flex shrink-0 items-stretch gap-2 self-stretch pr-3 sm:pr-4';",
    );
  });
});

// Regression, user report ("remove this setting and always show the icon with draft"):
// "Where a new session starts" was an app switch that hid the draft half of the
// project header's split button, so the private copy — the reason drafts exist —
// was invisible until you found a preference in a dialog two screens away.
describe("where a new session starts", () => {
  it("is not a setting at all: no draft preference is left anywhere", () => {
    expect(settingsSource).not.toContain("offerDrafts");
    expect(settingsSource).not.toContain("Where a new session starts");
    expect(storageSource).not.toContain("OfferDrafts");
    expect(sessionsListSource).not.toContain("offerDrafts");
  });

  it("always offers the draft half of the project header's button", () => {
    expect(sessionsListSource).toContain(
      "onNewDraft={(anchor, root) => openDraftsAt(anchor, machine.conn, root)}",
    );
  });

  it("picks every setting with the one ChoiceCell, spelled once", () => {
    // Every segmented choice the dialog offers - theme, page size, where a reply is
    // spoken, which voice speaks it - is the SAME cell.
    expect(settingsSource.match(/<ChoiceCell/g)?.length).toBeGreaterThanOrEqual(
      2,
    );
    // The cell moved into the vocabulary; the dialog no longer owns a copy.
    expect(settingsSource.match(/function ChoiceCell\(/g)?.length).toBe(
      undefined,
    );
    expect(uiSource.match(/function ChoiceCell\(/g)?.length).toBe(1);
  });
});

// Regression, user report ("New session in a draft should be under the project line,
// not the machine one — and there is already a New session button there"): the draft
// verb was a row in the machine's kebab menu, two headers above the project it forks,
// while the project header carried the plain verb alone.
describe("NewSessionButton, split", () => {
  it("carries the draft as a joined second half, not a second button", () => {
    const html = renderToStaticMarkup(
      <NewSessionButton
        machine="visgw"
        where="vis"
        onPress={() => {}}
        onDraft={() => {}}
      />,
    );
    // One cluster, two halves of the same amber box: the verb drops its trailing
    // border and the draft half wears the seam.
    expect(html).toContain("border-r-0");
    expect(html).toContain("border-l-accent-foreground/30");
    // It NAMES the project it forks and the machine it forks on: several headers are
    // on screen at once and "New session" alone says nothing to a screen reader.
    expect(html).toContain(
      'aria-label="New session in a draft of vis on visgw"',
    );
  });

  it("is a plain button when drafts are not offered", () => {
    const html = renderToStaticMarkup(
      <NewSessionButton machine="visgw" where="vis" onPress={() => {}} />,
    );
    expect(html).not.toContain("border-r-0");
    expect(html).not.toContain("in a draft");
  });

  it("hangs the draft question off the project header, never the machine menu", () => {
    // The project header's split button is the only way in...
    expect(sessionsListSource).toContain("onDraft={onNewDraft ?");
    expect(sessionsListSource).toContain("const openDraftsAt = useCallback(");
    // ...and the machine's own menu keeps only what is genuinely a machine verb.
    expect(sessionsListSource).not.toContain('title="New session in a draft…"');
    // The parked drafts read belongs to the project that was tapped, not to whatever
    // the machine happened to touch last.
    expect(sessionsListSource).toContain("projectPath(session) === draftRoot");
  });
});

// Regression, user report ("when I am going to the latest page on the session list
// there is a very unpleasant reflow and flicker"): page 1 and the page COUNT were cut
// from the filtered, re-ordered rows this screen paints, while pages 2 and up were
// re-fetched from `GET /v1/sessions?root=`, which hides nothing and re-orders nothing.
// The two lists never agreed — the gateway counted 1034 sessions in a project the list
// painted 763 of — so the last page painted its three real rows (239px) and then swapped
// them 119ms later for an unrelated ten-row window (582px).
describe("a project's pages are cut from the list on screen", () => {
  const sessions = sessionsListSource;
  const gateway = gatewaySource;

  it("slices the rows it paints, and asks no second source for them", () => {
    expect(sessions).toContain("projectPage(sessions, page, pageSize)");
    expect(sessions).not.toContain("listProjectPage");
    expect(gateway).not.toContain("listProjectPage");
    expect(gateway).not.toContain("&root=${encodeURIComponent(root)}");
  });

  it("keeps no disclosure and no 'Show more'", () => {
    expect(sessions).not.toContain("HeaderToggle");
    expect(sessions).not.toContain("Show {remaining} more");
    expect(sessions).toContain("<Pager");
  });
});

// Regression, user report ("the components on the WEB like the answers"): running
// prose was justified in three different spellings — the chat's own local
// `runningText` with hyphenation, and Settings' paragraphs justified with NO
// hyphenation at all, which is what turns a narrow column into rivers. There is now
// one rule, `PROSE`, and one ragged fallback for a run that cannot be broken.
describe("running prose has exactly one rule", () => {
  const RULE = "hyphens-auto [hyphenate-limit-chars:6_3_3] text-pretty";

  it("declares it once in ui.tsx, justified, with hyphenation attached", () => {
    expect(uiSource).toContain(
      `export const PROSE =\n  '${RULE} text-justify';`,
    );
    expect(uiSource).toContain(
      `export const PROSE_RAGGED =\n  '${RULE} text-left';`,
    );
  });

  it("is what the answers and the user bubble wear", () => {
    expect(chatSource).toContain("const runningText = PROSE;");
    expect(chatSource).toContain("isJustifiable ? PROSE : PROSE_RAGGED");
  });

  it("leaves no hand-spelled justification anywhere else", () => {
    for (const source of [chatSource, settingsSource, sessionsListSource]) {
      expect(source).not.toContain("text-justify");
    }
  });
});

// Regression, user report ("it should be this search more subtle and looking more
// connected to our designs"): the search box was rounder, taller and whiter than the
// controls it sat beside — a white slab on paper that carries no other box at rest.
describe("SearchField", () => {
  const field = uiSource.slice(uiSource.indexOf("export const SearchField"));
  // The field's own box: the first class template in the component is its `<label>`.
  const box = (/className={`([^`]*)`}/.exec(field)?.[1] ?? "").split(/\s+/);

  it("wears Button's own face and only lights up when focused", () => {
    expect(uiSource).toContain("export const SearchField");
    // Same box as `Button`: flat corners, its border and type step.
    expect(field).toContain("rounded-none");
    expect(field).not.toContain("rounded ");
    // Paper at rest; the input surface and the ring arrive with the caret.
    expect(field).toContain("bg-transparent");
    expect(field).toContain("focus-within:bg-input");
    expect(field).toContain("focus-within:border-accent");
  });

  // Regression, user report ("search HEIGHT still too big taking into account the
  // other buttons"): the field stood 44px on the bar while `Preferences` next to it
  // stands 32px (24px for a mouse), so the one framed box up there was 12px taller
  // than every control it shares the row with. A `Button` already answers this: it
  // paints a 32px face and reaches the finger's 44px through invisible slop. The
  // field does the same, split into TWO strips so the face itself stays the input's
  // own — a press in the middle of the text still places a caret where it landed.
  it("wears the bar’s own face and reaches the touch step around it", () => {
    expect(box).toContain("h-8");
    expect(box).toContain("mouse:h-6");
    expect(box).not.toContain("h-11");
    expect(box).toContain("relative");
    expect(box).toContain("before:-top-1.5");
    expect(box).toContain("after:-bottom-1.5");
    expect(box).toContain("before:h-1.5");
    expect(box).toContain("after:h-1.5");
    // A mouse needs no slop, and the strips would only eat the rows around it.
    expect(box).toContain("mouse:before:content-none");
    expect(box).toContain("mouse:after:content-none");
  });

  // Same report: Clear was a 12px glyph centred in its own 28px box sitting INSIDE
  // the field's inset, so the ✕ ink stopped about 20px short of the border while the
  // placeholder started 10px in — the asymmetry an eye reads as "far from right".
  it("lets Clear absorb the field’s own trailing inset", () => {
    // The field gives back the inset the ✕ would otherwise sit inside, so the square
    // runs to the border and centres its mark there, and both inks agree.
    expect(field).toMatch(/<CloseButton[\s\S]*?className="-me-3 sm:-me-4"/);
    expect(box).toContain("px-3");
    expect(box).toContain("sm:px-4");
  });

  // Regression, user report (paraphrased: the second band looked worse — put search
  // back on the header): a search box is recognised by the magnifying glass INSIDE the
  // open field, and this one carried no mark at all, so a bare framed box on the bar
  // read as "some input" rather than "search".
  it("carries the magnifying glass inside the open field", () => {
    expect(field).toContain("<SearchIcon");
    // Leading, before the input: the mark introduces the field, it does not end it.
    expect(field.indexOf("<SearchIcon")).toBeLessThan(field.indexOf("<input"));
    expect(uiSource).toContain("SearchIcon");
  });

  // It is a SEARCH field, so the phone keyboard says so and nothing autocorrects a
  // machine name into prose.
  it("asks the phone for a search keyboard", () => {
    expect(field).toContain('type="search"');
    expect(field).toContain('enterKeyHint="search"');
    expect(field).toContain('autoCorrect="off"');
  });
});

// Regression, user report (paraphrased: should the machine tabs be regular buttons?):
// each tab carried its own border and stood beside a filled button, so the row read as
// several competing boxes instead of one switch beside one verb.
describe("MachineSwitcher", () => {
  it("is one track that stands at the button's own height", () => {
    const html = renderToStaticMarkup(
      <MachineSwitcher>
        <MachineTab isOn onClick={() => {}}>
          tower
        </MachineTab>
      </MachineSwitcher>,
    );
    // 2px of track padding around a 28px tile is the 32px of `Button` density
    // "compact"; `mouse:` takes both down to 24 together.
    expect(html).toContain("p-0.5");
    // Track = duller paper, tile = the page's own paper lifted out of it.
    expect(html).toContain("bg-level-machine");
    // No frame: 2px padding + a 28px tile is 32px only if nothing borders it.
    expect(html).not.toContain("border");
    expect(html).toContain("h-7");
    expect(html).toContain("mouse:h-5");
    // Six machines scroll INSIDE the clipped track rather than widening the row.
    expect(html).toContain("overflow-x-auto");
    // Regression, user report: "definitely there should be no rounded corners" — this
    // screen is a stack of square bands and the track was the only pill on the page.
    expect(html).not.toContain("rounded");
  });

  it("gives the chosen machine a raised tile and the rest no box at all", () => {
    const on = renderToStaticMarkup(
      <MachineTab isOn onClick={() => {}}>
        tower
      </MachineTab>,
    );
    const off = renderToStaticMarkup(
      <MachineTab isOn={false} onClick={() => {}}>
        mini
      </MachineTab>,
    );
    expect(on).toContain('aria-pressed="true"');
    expect(on).toContain("bg-panel");
    // Amber is this product's VERB colour; a selected tab in it reads as a button.
    expect(on).not.toContain("bg-accent");
    // Nothing inside the track is bordered, selected or not.
    for (const html of [on, off]) expect(html).not.toContain("border");
    expect(off).toContain("text-dialog-hint");
  });

  // Regression, user report: the tab carried a live count and an unread count, so the
  // reader had to learn a colour code to tell two numbers apart. News is a HIGHLIGHT.
  it("marks unread with one amber mark and bold ink, never a number", () => {
    const news = renderToStaticMarkup(
      <MachineTab isOn={false} hasUnread onClick={() => {}}>
        tower
      </MachineTab>,
    );
    const quiet = renderToStaticMarkup(
      <MachineTab isOn={false} onClick={() => {}}>
        tower
      </MachineTab>,
    );
    expect(news).toContain("bg-accent");
    expect(news).toContain("font-bold");
    expect(news).toContain("unread");
    expect(news).not.toMatch(/>\s*\d+\s*</);
    expect(quiet).not.toContain("bg-accent");
    expect(quiet).toContain("text-dialog-hint");
  });

  // Regression, user report ("offline stuff should just not be accessible... it should be
  // shown as offline and clicking should show reconnecting"): a machine that was not
  // answering was a live tab in the same ink and the same weight, wearing the word
  // "offline" — the one label on this strip that GREW when its machine got worse — and
  // pressing it scoped the screen to a machine with nothing to show.
  it("drains a machine that is not answering and makes its press a retry", () => {
    const down = renderToStaticMarkup(
      <MachineTab
        isOn={false}
        isDown
        label="Reconnect to tower"
        title="tower is not answering - Failed to fetch"
        onClick={() => {}}
      >
        <MachineMark color={MACHINE_COLORS[0]!} isHollow />
        tower
      </MachineTab>,
    );
    // It is a VERB now, not one of the states this switch is choosing between.
    expect(down).not.toContain("aria-pressed");
    expect(down).toContain('aria-label="Reconnect to tower"');
    expect(down).toContain("is not answering");
    // No word inside the tile until it is pressed, and never the old label.
    expect(down).not.toContain(">offline<");
    // Drained: hint ink, never the raised paper tile the chosen machine wears.
    expect(down).not.toContain("bg-panel");
    expect(down).toContain("text-dialog-hint/60");
    // The hue is emptied, not swapped: the machine keeps its identity while it is down.
    expect(down).toContain(`border ${MACHINE_COLORS[0]!.rail}`);
    expect(down).not.toContain(MACHINE_COLORS[0]!.dot);
  });

  // Regression, user report (paraphrased: "the error should be RED and say something
  // like 'Unable to connect'"): a retry that came back dead printed "no answer" in the
  // tile's own hint ink, so the one word on this strip that is not a name looked like
  // more chrome, and a press that went nowhere read as a press that did nothing.
  it("answers the press in the tile that was pressed", () => {
    const busy = renderToStaticMarkup(
      <MachineTab isOn={false} isDown note="reconnecting..." onClick={() => {}}>
        tower
      </MachineTab>,
    );
    const failed = renderToStaticMarkup(
      <MachineTab
        isOn={false}
        isDown
        note="Unable to connect"
        isNoteError
        onClick={() => {}}
      >
        tower
      </MachineTab>,
    );
    expect(busy).toContain("reconnecting...");
    expect(busy).toContain('aria-live="polite"');
    expect(busy).not.toContain("text-err");
    expect(failed).toContain("Unable to connect");
    // The failure is the one thing here worth an ink of its own.
    expect(failed).toContain("text-err");
    // A down tile carries no news mark: its badge would be a stale count.
    expect(
      renderToStaticMarkup(
        <MachineTab isOn={false} isDown hasUnread onClick={() => {}}>
          tower
        </MachineTab>,
      ),
    ).not.toContain("bg-accent");
  });
});

// Regression, user report ("the plus is here and here" — a plus on the machine band and
// a plus on every project header below it, meaning two different creations): the machine
// band's control never was a create at all. `openManageProjects` opens the sheet on
// `Projects · <machine>` — choose one, remove one, `New project…` at its foot — so the
// word promised one of the three things behind it and a plus would have promised a
// session's meaning one row up.
describe("MachineProjectsButton", () => {
  const html = renderToStaticMarkup(
    <MachineProjectsButton machine="tower" onPress={() => {}} />,
  );

  it("wears the folder it opens, never the plus that means a session", () => {
    expect(html).toContain(
      renderToStaticMarkup(<ProjectsIcon className="size-4" />),
    );
    expect(html).not.toContain(
      renderToStaticMarkup(<PlusIcon className="size-4" />),
    );
  });

  it("is named for what it opens, because it carries no word", () => {
    expect(html).toContain('aria-label="Projects on tower"');
    expect(html).toContain("bg-accent");
    // Several machines are on screen at once in the All view: the label says which.
    expect(
      renderToStaticMarkup(
        <MachineProjectsButton machine="nuc" onPress={() => {}} />,
      ),
    ).toContain('aria-label="Projects on nuc"');
  });

  it("holds still under the finger, because it anchors the sheet it opens", () => {
    // A transform would move the box the projects sheet was measured against.
    expect(html).not.toContain("active:scale");
  });

  it("stands at the header row's own compact height", () => {
    expect(html).toContain("min-h-7");
    expect(html).toContain("shrink-0");
  });

  // An empty machine has no project row under its band to say what a folder means.
  it("spells itself out where the mark has no example beside it", () => {
    const word = renderToStaticMarkup(
      <MachineProjectsButton machine="tower" face="word" onPress={() => {}} />,
    );
    expect(word).toContain(">Projects<");
    expect(word).not.toContain("<svg");
    expect(word).toContain('aria-label="Projects on tower"');
  });

  it("is used by every place the machine is named, and hand-rolled in none", () => {
    // The row above the card when scoped, each machine's band in the All view, and
    // the empty body that has nothing else to press.
    expect(sessionsListSource.match(/<MachineProjectsButton/g)?.length).toBe(3);
    expect(sessionsListSource).not.toContain(">New project</Button>");
    // The word it used to carry named a create the sheet does not start.
    expect(sessionsListSource).not.toContain("NewProjectButton");
    expect(uiSource).not.toContain("NewProjectButton");
  });
});

// Regression, same report: THREE verbs stand on this screen — a machine's projects, a
// session in a project, that session in a draft — and a glyph used twice makes two of
// them one control. One meaning, one mark.
describe("three verbs, three marks", () => {
  const glyph = (markup: string) => markup.match(/<svg[\s\S]*?<\/svg>/g) ?? [];
  const session = renderToStaticMarkup(
    <NewSessionButton
      machine="tower"
      where="vis"
      onPress={() => {}}
      onDraft={() => {}}
    />,
  );
  const projects = renderToStaticMarkup(
    <MachineProjectsButton machine="tower" onPress={() => {}} />,
  );

  it("gives the plus to a session, the folder to a machine, the fork to a draft", () => {
    const [start, draft] = glyph(session);
    const [inventory] = glyph(projects);
    expect(start).toBe(renderToStaticMarkup(<PlusIcon className="size-4" />));
    expect(draft).toBe(renderToStaticMarkup(<DraftIcon className="size-4" />));
    expect(inventory).toBe(
      renderToStaticMarkup(<ProjectsIcon className="size-4" />),
    );
    expect(new Set([start, draft, inventory]).size).toBe(3);
  });
});

// Regression, user report ("when we create a new session there's this 'Creating' showing
// but not in the new session button but outside — I want it to show in the button
// itself"): the busy word was parked on the app bar, so the fleet said it was busy while
// the button that had actually been pressed sat there looking untouched.
describe("NewSessionButton, busy", () => {
  const busy = renderToStaticMarkup(
    <NewSessionButton
      machine="tower"
      busyLabel="Creating..."
      onPress={() => {}}
    />,
  );

  it("wears the work in its own face, where the press happened", () => {
    expect(busy).toContain(">Creating...<");
    expect(busy).not.toContain(">New session<");
  });

  it("refuses a second press while that create is in flight", () => {
    expect(busy).toContain('disabled=""');
    // The draft half is the same create on the same project: it goes with it.
    const split = renderToStaticMarkup(
      <NewSessionButton
        machine="tower"
        where="vis"
        busyLabel="Forking..."
        onPress={() => {}}
        onDraft={() => {}}
      />,
    );
    expect(split.match(/disabled=""/g)?.length).toBe(2);
  });

  it("still names its machine, so the announcement says which one", () => {
    expect(busy).toContain('aria-label="New session on tower"');
    expect(busy).toContain('aria-live="polite"');
  });

  it("is fed by the header that started the create, not by the whole screen", () => {
    // The busy word is keyed to one project header; the bar keeps it only for a create
    // started from its own menu, where no button exists to speak for it.
    expect(sessionsListSource).toContain("busyLabel={");
    expect(sessionsListSource).toContain("creating && creating.at === null");
    expect(sessionsListSource).not.toContain("createBusyLabel");
  });
});

// Regression, user report ("when we want to remove the session it's showing the full
// dialogue instead of just a small dialogue on the phone"): a two-line confirmation took
// the whole glass, so "Delete this session?" read like a screen you had navigated to.
describe("Modal, fit", () => {
  it("has a size that stops at its content, next to the full-screen one", () => {
    expect(uiSource).toContain("size?: 'full' | 'fit' | 'wide';");
    // The sheet still arrives from the bottom edge — same scrim, same physics.
    expect(uiSource).toContain(
      "size === 'fit' ? 'items-end' : 'items-stretch'",
    );
    expect(uiSource).toContain(
      "size === 'fit' ? 'max-h-full sm:h-auto' : DIALOG_DESKTOP_HEIGHT",
    );
  });

  it("is what the rename/delete question opens in", () => {
    expect(sessionsListSource).toContain(
      '<Modal size="fit" onDismiss={closeRowAction}>',
    );
  });

  // Regression, user report (rename field hidden under the iOS keyboard, then the
  // note annotator's composer under it): the native keyboard pins only the app
  // shell to its visible height. A body portal remains as tall as the glass and
  // leaves a bottom sheet underneath the keyboard. ONE rule, one function: every
  // full-screen layer asks `overlayLayer` where it mounts and how it is positioned.
  it("keeps every full-screen layer inside the viewport-pinned app shell", () => {
    expect(appSource).toContain("data-viewport-shell");
    expect(uiSource).toContain(
      "document.querySelector<HTMLElement>('[data-viewport-shell]')",
    );
    expect(uiSource).toContain(
      "host === document.body ? 'fixed' : 'absolute'",
    );
    expect(uiSource).toContain("const { host: portalHost, position } = overlayLayer();");
    expect(uiSource).toContain("portalHost,\n  );");
    // The opened document is the app's other full-screen layer, and it asks the
    // same question instead of hanging off the body at `100dvh`.
    expect(docSource).toContain("overlayLayer");
    expect(docSource).toContain("overlayLayer().host,");
    expect(docSource).not.toContain("document.body,");
    expect(docSource).not.toContain("h-[100dvh]");
  });

  // Regression, user report ("cannot we make it less height, like it goes from
  // bottom only and occupies only the height its required?"): a `fit` sheet is
  // welded to the BOTTOM edge, and it still padded its top with the notch inset —
  // 47px of dead panel paper above the title on every iPhone. The frame cannot see
  // which sheet it stands in, so the sheet tells it.
  it("tells the frame inside it that no notch stands above a fit sheet", () => {
    expect(uiSource).toContain("const IsFitSheet = createContext(false);");
    expect(uiSource).toContain("<IsFitSheet.Provider value={size === 'fit'}>");
    expect(uiSource).toContain(
      "isFitSheet ? '' : 'pt-[env(safe-area-inset-top)]'",
    );
  });

  // The pause that BLOCKS a run is a question too, and it was the last surface
  // hand-rolling a scrim beside this one.
  it("is what a human-input pause opens in", () => {
    expect(humanInputSource).toContain("<Modal");
    expect(humanInputSource).toContain('size="fit"');
    expect(humanInputSource).not.toContain("fixed inset-0 z-50");
  });

  // Regression, user report ("these two buttons should be up and then this dialog can
  // be smaller"): the model picker was the last surface hand-rolling a scrim, and it
  // pinned its panel at 92% of the glass whatever it held — so six provider rows ended
  // half a phone above `Refresh` and `Manage providers`, which were welded to the foot
  // of all that empty paper. The verbs are cells of the band now and there is no footer
  // left, so the sheet is the band plus its rows.
  it("is what the model picker opens in, with its verbs in the band", () => {
    expect(routerSource).toContain('<Modal size="fit" onDismiss={onClose}>');
    expect(routerSource).toContain("<DialogFrame");
    expect(routerSource).not.toContain("fixed inset-0 z-50");
    expect(routerSource).not.toContain('aria-modal="true"');
    expect(routerSource).not.toContain("h-[92%]");
    expect(routerSource).not.toContain("<footer");

    const band = routerSource.slice(
      routerSource.indexOf("actions={"),
      routerSource.indexOf('<div className="space-y-3'),
    );
    expect(band).toContain("'Refreshing…' : 'Refresh'");
    // The band's second cell is the door to where the accounts live, and it is named
    // in full for a screen reader while the band reads the pinned model.
    expect(band).toContain('aria-label="Manage providers"');
    expect(band).toContain("Providers");
  });
});

// Regression, user report ("in the artifacts when I read the md file there is this
// close button which doesn't look like rest of the close buttons"): an opened
// document wore a caption STRIP with the X hung off its end on panel paper, while
// every other opened surface wears the one `DialogHeader` band with a title-toned
// close. Three more surfaces spelled a button out by hand beside it — the doc
// card's `Open` chip in `bg-button`, the data table's `BAR_BUTTON`/`PAGER_BUTTON`
// class constants, and the error screen's reload — so the app carried four button
// faces nobody had chosen.
describe("every surface uses the vocabulary's own controls", () => {
  it("titles an opened document with the one header band and the one way out", () => {
    expect(docSource).toContain("<DialogHeader");
    expect(docSource).toContain("closeLabel={`Close ${name}`}");
    // The X is the header's own; nothing hangs it off a caption strip.
    expect(docSource).not.toContain("<CloseButton");
    expect(docSource).not.toContain("self-stretch");
  });

  it("leaves no hand-rolled button in the document, table or error surfaces", () => {
    for (const source of [docSource, tableBarSource, boundarySource]) {
      expect(source).toMatch(/from ['"]\.\/ui['"]/);
    }
    // A control is a COMPONENT before it is a class list.
    expect(docSource).not.toContain("<button");
    expect(boundarySource).not.toContain("<button");
    expect(tableBarSource).not.toContain("BAR_BUTTON");
    expect(tableBarSource).not.toContain("PAGER_BUTTON");
    expect(tableBarSource).not.toContain("border border-edge-strong px-3");
  });
});

// The user's own words, twice: "I really don't understand how we can have so many
// different buttons styles … PLEASE ENSURE WE HAVE REUSABLE COMPONENTS", then "go
// over the other components AND STANDARDIZE". Six faces were being re-spelled at a
// dozen call sites — a filter chip, a "load more" bar, a `Copy` chip, a pressable
// list row, a trace disclosure, an option row and a remove `×` — so the same control
// looked different depending on which screen you found it on.
describe("the second vocabulary: chips, rows, disclosures", () => {
  const first = (html: string) =>
    (/<button[^>]*class="([^"]*)"/.exec(html)?.[1] ?? "").split(" ");

  describe("Chip", () => {
    const html = (isOn: boolean) =>
      renderToStaticMarkup(
        <Chip isOn={isOn} onClick={() => {}}>
          IMAGES
        </Chip>,
      );

    it("says whether it is the one that is on", () => {
      expect(html(true)).toContain('aria-pressed="true"');
      expect(html(false)).toContain('aria-pressed="false"');
      expect(first(html(true))).toContain("bg-accent");
      expect(first(html(false))).toContain("bg-transparent");
    });

    it("keeps the touch box and only tightens it for a cursor", () => {
      expect(first(html(false))).toContain("min-h-7");
      expect(first(html(false))).toContain("mouse:min-h-6");
    });

    // `border-edge` is the FIELD hairline and always has `bg-input` under it to
    // separate the box; alone on the page it measures 1.18:1. An enum toggle's
    // choices stand one row from the boolean toggle's `Switch` in the same settings
    // list, so both wear the frame every other resting control wears.
    it("draws the resting frame the rest of the vocabulary draws", () => {
      expect(first(html(false))).toContain("border-edge-strong");
      expect(first(html(false))).not.toContain("border-edge");
      expect(first(html(true))).toContain("border-accent");
    });
  });

  describe("LoadMore", () => {
    const html = () =>
      renderToStaticMarkup(
        <LoadMore label="Load 12 more artifacts" onClick={() => {}}>
          Load 12 more
        </LoadMore>,
      );

    it("owns its arrow and hears its own name", () => {
      expect(html()).toContain('aria-label="Load 12 more artifacts"');
      expect(html()).toContain("<svg");
      expect(html()).toContain("Load 12 more");
    });
  });

  describe("CopyChip", () => {
    const html = () =>
      renderToStaticMarkup(
        <CopyChip value="abc" label="Copy session id">
          abc12345
        </CopyChip>,
      );

    it("is one box wide enough for 'Copied', so it never jumps", () => {
      expect(first(html())).toContain("min-w-[6ch]");
      expect(first(html())).toContain("h-6");
    });

    // Regression, reported as "remove the # for copy and use the real icon": the
    // session id chip led with a `#` glyph, which names an identifier and says
    // nothing about pressing it.
    it("leads with the copy icon, never a glyph", () => {
      expect(html()).toContain("<svg");
      expect(html()).not.toContain("#");
    });

    it("carries a name and, when there is more to say, a title", () => {
      expect(html()).toContain('aria-label="Copy session id"');
      const titled = renderToStaticMarkup(
        <CopyChip
          value="abc"
          label="Copy session id"
          title="Copy session id\nabc"
        >
          abc
        </CopyChip>,
      );
      expect(titled).toContain("title=");
    });
  });

  describe("ListRow", () => {
    const html = (props: Partial<Parameters<typeof ListRow>[0]> = {}) =>
      renderToStaticMarkup(
        <ListRow onClick={() => {}} {...props}>
          anthropic
        </ListRow>,
      );

    it("is one slab at one height, framed only inside a card", () => {
      expect(first(html())).toContain("min-h-12");
      expect(first(html())).not.toContain("border");
      expect(first(html({ isFramed: true }))).toContain("border");
    });

    it("marks the selected one with the amber edge, framed or not", () => {
      expect(first(html({ isSelected: true }))).toContain("bg-panel-2");
      expect(first(html({ isFramed: true, isSelected: true }))).toContain(
        "border-accent",
      );
    });
  });

  describe("Disclosure", () => {
    const html = (props: Partial<Parameters<typeof Disclosure>[0]> = {}) =>
      renderToStaticMarkup(
        <Disclosure isOpen={false} onClick={() => {}} {...props}>
          <span>THINKING</span>
        </Disclosure>,
      );

    it("reports its state and stays the transcript's scroll anchor", () => {
      expect(html()).toContain('aria-expanded="false"');
      expect(html({ isOpen: true })).toContain('aria-expanded="true"');
      // SessionScreen keeps the viewport still by finding exactly this attribute.
      expect(html()).toContain("data-disclosure-toggle");
    });

    it("wears the ink of what it opens and nothing else", () => {
      expect(first(html({ tone: "step" }))).toContain("text-accent-ink");
      expect(first(html({ tone: "thinking" }))).toContain("text-thinking");
      expect(first(html())).toContain("text-footer-muted");
    });

    // Bold AND italic at the same time: the reasoning this band opens is set in
    // italic, so the name of that band is italic too, exactly as the TUI paints
    // it. It used to spell `not-italic` and cancel the slant.
    it("slants the name of the band whose own text is slanted", () => {
      expect(first(html({ tone: "thinking" }))).toContain("italic");
      expect(first(html({ tone: "thinking" }))).toContain("font-bold");
      expect(first(html({ tone: "thinking" }))).not.toContain("not-italic");
      expect(first(html({ tone: "step" }))).not.toContain("italic");
    });
  });

  // A band says in ONE word what it holds — `PYTHON` over a program, `RESULT`
  // over what it printed, `THINKING` over the reasoning — and that word wears
  // one weight whether or not its row can be pressed.
  describe("BandLabel", () => {
    const label = renderToStaticMarkup(<BandLabel>RESULT</BandLabel>);
    const pressable = first(
      renderToStaticMarkup(
        <Disclosure isOpen={false} tone="step">
          PYTHON
        </Disclosure>,
      ),
    );

    it("names a band in the weight the pressable one wears", () => {
      expect(label).toContain("RESULT");
      for (const token of [
        "font-extrabold",
        "tracking-[0.06em]",
        "text-accent-ink",
      ]) {
        expect(label).toContain(token);
        expect(pressable).toContain(token);
      }
    });

    it("keeps the count beside a name out of the name's weight", () => {
      expect(renderToStaticMarkup(<BandTally> +3 more</BandTally>)).toContain(
        "font-normal",
      );
    });

    it("is the only place that weight is spelled", () => {
      expect(chatSource).not.toContain("font-extrabold");
      expect(chatSource).toContain("<BandLabel");
      expect(chatSource).toContain("<BandTally");
    });
  });

  describe("ChoiceRow", () => {
    const html = (isOn: boolean) =>
      renderToStaticMarkup(
        <ChoiceRow isOn={isOn} mark="●" onClick={() => {}}>
          production
        </ChoiceRow>,
      );

    it("turns amber when it is the answer, and the glyph is decoration", () => {
      expect(first(html(true))).toContain("border-accent");
      expect(first(html(false))).toContain("border-edge");
      expect(html(true)).toContain('aria-hidden="true"');
      expect(html(true)).toContain("production");
    });
  });

  describe("the composer's removes", () => {
    // A queued turn, a pasted block and an attachment are dropped by the app's one
    // ✕ — they used to be a 28×28 chip-ender with a hairline of its own.
    it("are the one CloseButton, positioned and nothing more", () => {
      const removes = [
        ...sessionScreenSource.matchAll(/<CloseButton[\s\S]*?\/>/g),
      ].map(([element]) => element);
      expect(removes).toHaveLength(3);
      for (const element of removes) {
        expect(element).toContain("label=");
        expect(element).not.toContain("edge");
        expect(element).not.toContain("tone=");
        const className = /className="([^"]*)"/.exec(element)?.[1];
        // POSITION only, and every token in it is a position: the attachment's ✕
        // hangs on the chip's right edge (`my-auto` centres a square that no longer
        // stretches between the chip's top and bottom), and the queued row's is
        // pulled out of the row's own padding onto the tray's edge.
        for (const token of className?.split(" ") ?? []) {
          expect(token, token).toMatch(
            /^(absolute|inset-y-0|right-0|my-auto|-me-2\.5)$/,
          );
        }
      }
    });
  });
});

// The same report, one layer down: the call sites. A control is a COMPONENT before
// it is a class list, so these screens may not spell one out again.
describe("every screen uses the second vocabulary too", () => {
  it("leaves no hand-rolled button in the transcript, the sheet, the router, the form", () => {
    for (const source of [
      chatSource,
      humanInputSource,
      providerAuthSource,
      routerSource,
    ]) {
      expect(source).not.toContain("<button");
    }
    // The transcript's `Copy` chips and its three expanders are the shared ones.
    expect(chatSource).toContain("<CopyChip");
    expect(chatSource).toContain("<Disclosure");
    expect(chatSource).toContain("<LoadMore");
    expect(chatSource).not.toContain("function CopyButton");
    expect(chatSource).not.toContain("disclosureClass");
  });

  it("files the artifacts sheet's chips, version rows and pager under the vocabulary", () => {
    expect(artifactsSheetSource).toContain("<Chip");
    expect(artifactsSheetSource).toContain("<ListRow");
    expect(artifactsSheetSource).toContain("<LoadMore");
    // The filter strip's own three-way class list is gone.
    expect(artifactsSheetSource).not.toContain(
      "border-accent bg-accent font-bold text-accent-foreground",
    );
  });

  it("files settings' toggles and rows under it as well", () => {
    expect(settingsSource).toContain("<Chip");
    // The provider rows moved out of this screen and into the panel that owns
    // them, so the row vocabulary is checked where the rows now live.
    expect(providerAuthSource).toContain("<ListRow");
    expect(settingsSource).not.toContain("min-h-8 border px-2 py-0.5");
    expect(settingsSource).not.toContain(
      "flex min-h-12 w-full items-center gap-2 px-3 py-2",
    );
  });

  it("leaves the composer's three removes as one control and its menu as menu rows", () => {
    expect(sessionScreenSource).toContain("<CloseButton");
    expect(sessionScreenSource).toContain("<MenuItem");
    expect(sessionScreenSource).toContain("<CopyChip");
    expect(sessionScreenSource).not.toContain(
      "hover:bg-warn-surface hover:text-err",
    );
    expect(sessionScreenSource).not.toContain('role="menuitem"');
  });

  it("gives the human-input form the one option row", () => {
    expect(humanInputSource).toContain("<ChoiceRow");
    expect(humanInputSource).not.toContain(
      "border-accent bg-hover text-accent-ink",
    );
  });
});

// The third pass over the same report: the four names a button comes in, and the
// composer — the one strip the app is used through — which had written its own
// four boxes, its own two text controls, its own back arrow and its own floating
// pill, none of them agreeing with each other on a rhythm.
describe("the button's four ranks", () => {
  const classes = (html: string) =>
    (/<button[^>]*class="([^"]*)"/.exec(html)?.[1] ?? "").split(" ");

  it("names the RANK of a verb, never the paint it happens to wear", () => {
    expect(uiSource).toContain(
      "variant?: 'primary' | 'secondary' | 'quiet' | 'danger' | 'overlay' | 'remove';",
    );
    // `solid`/`ghost` described a fill; `inverse` was a fifth face with one call
    // site. A rank has to be choosable without knowing the palette.
    expect(uiSource).not.toContain("'solid'");
    expect(uiSource).not.toContain("'ghost'");
    expect(uiSource).not.toContain("inverse:");
  });

  it("keeps the amber primary and gives the secondary its frame", () => {
    expect(classes(renderToStaticMarkup(<Button>Send</Button>))).toContain(
      "bg-accent",
    );
    expect(
      classes(
        renderToStaticMarkup(<Button variant="secondary">Cancel</Button>),
      ),
    ).toContain("border-edge-strong");
  });

  it("leaves no screen asking for a variant that no longer exists", () => {
    for (const source of [
      chatSource,
      settingsSource,
      sessionsListSource,
      sessionScreenSource,
      routerSource,
      providerAuthSource,
      artifactsSheetSource,
      connectSource,
      machinesSource,
      docSource,
      tableBarSource,
      boundarySource,
    ]) {
      expect(source).not.toContain('variant="solid"');
      expect(source).not.toContain('variant="ghost"');
      expect(source).not.toContain('variant="inverse"');
    }
  });
});

// Regression, user report ("not every one has the same height ... this copy button
// has some space top bottom"): the transcript's card headers each spelled their own
// band. A result with no body was text plus `py-1`, a result with a body was `min-h-6`
// around a 24px `Copy` chip, and a program header was `min-h-6` with NO vertical
// padding at all, so its chip sat rule-to-rule — while the same header with a
// `Disclosure` in it measured 41px. Four heights in one column.
describe("the transcript's card header band", () => {
  const band = /const CARD_BAND =\s*"([^"]*)"/.exec(chatSource)?.[1] ?? "";

  it("is one band, tall enough to give the Copy chip its air", () => {
    // A `CopyChip` is `h-6` and a `Disclosure` is `min-h-8`: the band is `min-h-8`
    // and CENTRES them, so the chip gets its 4px above and below while the taller
    // control fills the row instead of stacking padding on top of its own height.
    expect(band).toContain("min-h-8");
    expect(band).toContain("items-center");
    expect(band).toContain("px-2");
    expect(band).not.toMatch(/\bp[ytb]-/);
  });

  it("is worn by every card header and re-spelled by none", () => {
    expect(
      (chatSource.match(/\$\{CARD_BAND\}|className={CARD_BAND}/g) ?? []).length,
    ).toBe(3);
    expect(chatSource).not.toContain("min-h-6");
  });
});

describe("the composer's own controls", () => {
  const classes = (html: string) =>
    (/<button[^>]*class="([^"]*)"/.exec(html)?.[1] ?? "").split(" ");

  it("gives attach, dictate, send and stop ONE rhythm and one press", () => {
    const box = (tone: "quiet" | "send" | "stop" | "recording" | "voice") =>
      classes(
        renderToStaticMarkup(
          <ComposerButton tone={tone} label="Send message">
            {"\u2191"}
          </ComposerButton>,
        ),
      );
    // The glyphs in the strip are one box; the send is the square that ends it
    // and the stop takes exactly the send's slot, so the strip cannot disagree
    // with itself about where it ends.
    expect(box("quiet")).toContain("h-8");
    expect(box("quiet")).toContain("mouse:h-7");
    expect(box("recording")).toContain("h-8");
    expect(box("send")).toContain("size-8");
    expect(box("send")).toContain("mouse:size-7");
    expect(box("stop")).toContain("size-full");
    // The voice MODE keeps the dictation box exactly: switching mode must not
    // move the strip, only repaint the control.
    expect(box("voice")).toContain("h-8");
    expect(box("voice")).toContain("w-7");
    expect(box("voice")).toContain("mouse:h-7");
    expect(box("voice")).toContain("bg-accent");
    for (const tone of [
      "quiet",
      "send",
      "stop",
      "recording",
      "voice",
    ] as const) {
      expect(box(tone)).toContain("active:scale-[0.94]");
      // None of the four had a focus ring when each was written by hand.
      expect(box(tone)).toContain("focus-visible:ring-accent/60");
    }
  });

  it("is icon-only, so it is named", () => {
    expect(
      renderToStaticMarkup(
        <ComposerButton label="Dictate message">{"\u25cf"}</ComposerButton>,
      ),
    ).toContain('aria-label="Dictate message"');
  });

  // The app has no haptics, so the only way a press-and-hold can report itself
  // is on screen: the paper rises through the button for exactly as long as the
  // switch takes, and a reader who asked for less motion is shown nothing
  // rather than a jump.
  it("shows a press-and-hold filling, and only while it is held", () => {
    const held = renderToStaticMarkup(
      <ComposerButton isHolding label="Dictate message">
        {"●"}
      </ComposerButton>,
    );
    const idle = renderToStaticMarkup(
      <ComposerButton label="Dictate message">{"●"}</ComposerButton>,
    );
    expect(held).toContain("origin-bottom");
    expect(held).toContain("duration-[450ms]");
    expect(held).toContain("starting:scale-y-0");
    expect(held).toContain("motion-reduce:hidden");
    expect(idle).not.toContain("origin-bottom");
  });

  it("reports the turn's model and level in one type step, hovered one way", () => {
    const picker = classes(
      renderToStaticMarkup(<MetaButton isPicker>opus</MetaButton>),
    );
    const plain = classes(renderToStaticMarkup(<MetaButton>high</MetaButton>));
    expect(picker).toContain("underline");
    expect(plain).not.toContain("underline");
    for (const one of [picker, plain]) {
      expect(one).toContain("text-chip");
      expect(one).toContain("hover:text-accent-ink");
    }
  });

  // Regression: MetaButton destructured `children` and then rendered a
  // self-closing <button />, so the strip under the composer showed the model
  // name and the reasoning level as two empty boxes with a hairline between
  // them — the divider was the only thing on screen.
  it("says the word it was given", () => {
    expect(
      renderToStaticMarkup(<MetaButton isPicker>opus</MetaButton>),
    ).toContain("opus");
    expect(renderToStaticMarkup(<MetaButton>quick</MetaButton>)).toContain(
      "quick",
    );
  });

  it("presses prose by moving the paper, dotted only where a word stands in", () => {
    const token = classes(
      renderToStaticMarkup(<TextButton isToken>{"[paste #1]"}</TextButton>),
    );
    const plain = classes(renderToStaticMarkup(<TextButton>draft</TextButton>));
    expect(token).toContain("decoration-dotted");
    expect(plain).not.toContain("decoration-dotted");
    // One hover for both: the surface moves, the ink does not.
    for (const one of [token, plain]) expect(one).toContain("hover:bg-hover");
  });

  it("completes @ and / with one row, and never steals the caret", () => {
    const html = renderToStaticMarkup(<OptionRow isActive>notes.md</OptionRow>);
    expect(html).toContain('role="option"');
    expect(html).toContain('aria-selected="true"');
    expect(classes(html)).toContain("bg-accent");
    expect(
      classes(renderToStaticMarkup(<OptionRow>notes.md</OptionRow>)),
    ).toContain("hover:bg-hover");
    // The pointer press is CANCELLED by the control, not by whoever remembers.
    expect(uiSource).toContain(
      "onPointerDown={(event) => event.preventDefault()}",
    );
  });

  it("leaves by the band's own leading half, notch included", () => {
    const html = renderToStaticMarkup(<BackButton label="Back to sessions" />);
    expect(html).toContain('aria-label="Back to sessions"');
    expect(classes(html)).toContain("pl-[env(safe-area-inset-left)]");
  });

  it("floats over the transcript with its own paper and its own lift", () => {
    const pill = classes(renderToStaticMarkup(<Pill>Latest</Pill>));
    expect(pill).toContain("bg-button");
    expect(pill).toContain("shadow-[4px_4px_0_var(--dialog-shadow)]");
  });
});

describe("a setting is picked and switched by one control each", () => {
  const classes = (html: string) =>
    (/<button[^>]*class="([^"]*)"/.exec(html)?.[1] ?? "").split(" ");

  it("fills the chosen cell in amber and marks it once", () => {
    const on = renderToStaticMarkup(
      <ChoiceCell title="Gruvbox" sub="dark" isSelected />,
    );
    expect(classes(on)).toContain("bg-accent");
    expect(on).toContain("\u25cf");
    expect(on).toContain('aria-pressed="true"');
    const off = renderToStaticMarkup(
      <ChoiceCell title="Gruvbox" sub="dark" isSelected={false} />,
    );
    expect(classes(off)).toContain("bg-input");
    expect(off).toContain("\u25cb");
    // The grid draws the hairlines; a cell that framed itself would double them.
    expect(classes(off)).not.toContain("border");
  });

  it("says ON or OFF in words, and says when it is still asking", () => {
    const on = renderToStaticMarkup(<Switch label="Web search" isOn />);
    expect(on).toContain('role="switch"');
    expect(on).toContain('aria-checked="true"');
    expect(on).toContain('aria-label="Web search: on"');
    expect(on).toContain("ON");
    expect(
      renderToStaticMarkup(<Switch label="Web search" isOn isBusy />),
    ).toContain("\u00b7\u00b7");
  });

  // Regression, user report ("these off buttons are not visible at all: why no
  // border?"): OFF drew `border-transparent` over `bg-panel-2`, and `--panel2` is
  // the same value as `--surface` in both bundled palettes — a transparent frame
  // over the very paper the control stands on, so a settings row ended in a grey
  // word with no box at all.
  it("wears the resting frame when it is off, over no paper of its own", () => {
    const off = classes(
      renderToStaticMarkup(<Switch label="Web search" isOn={false} />),
    );
    expect(off).toContain("border-edge-strong");
    expect(off).toContain("bg-transparent");
    expect(off).not.toContain("border-transparent");
    expect(off).not.toContain("bg-panel-2");
    // One hover system, and it only ever moves the surface.
    expect(off).toContain("hover:bg-hover");
    expect(off).not.toContain("hover:text-white");
    // ON is the amber slab, framed in its own colour, so the box never changes.
    const on = classes(
      renderToStaticMarkup(<Switch label="Web search" isOn />),
    );
    expect(on).toContain("border-accent");
    expect(on).toContain("bg-accent");
    expect(on).not.toContain("border-transparent");
  });
});

// Regression, user report ("I am on the same device and I see four entries — what is
// even the purpose? I should just know if I am connected or not"): the notifications
// panel answered an OPERATOR's question — every push token the gateway holds, one row
// each, so one reinstalled iPhone stood in it four times — while the reader's own
// question survived only as the verb printed on a button.
//
// Regression, user report ("I am disconnected and the button is OFF? I should have the
// ACTION BUTTON, like CONNECT, not OFF/ON"): the answer finally arrived, and then the
// control beside it printed the state it was ALREADY in — `Not connected`, `visgw will
// not alert this device.`, `OFF` — so the row said no three times over and never once
// said how to say yes.
describe("the notifications row answers one question", () => {
  const row = (
    props: Partial<ComponentProps<typeof NotifyConnectionRow>> = {},
  ) =>
    renderToStaticMarkup(
      <NotifyConnectionRow
        machine="visgw"
        isOn={false}
        onClick={() => {}}
        {...props}
      />,
    );

  it("states whether this device is connected — in the verb, and only there", () => {
    const on = row({ isOn: true });
    expect(on).toContain(">Disconnect<");
    expect(on).not.toContain("visgw alerts this device when a turn finishes.");

    const off = row();
    expect(off).toContain(">Connect<");
    expect(off).not.toContain("visgw will not alert this device.");
  });

  it("presses the VERB, never the state it is already in", () => {
    const off = row();
    expect(off).toContain(">Connect<");
    expect(off).toContain('aria-label="Connect notifications from visgw"');
    // The state is the sentence's job; this control's whole job is the way out of it.
    expect(off).not.toContain(">OFF<");
    expect(off).not.toContain('role="switch"');
    expect(off).not.toContain("aria-checked");

    const on = row({ isOn: true });
    expect(on).toContain(">Disconnect<");
    expect(on).toContain('aria-label="Disconnect notifications from visgw"');
    expect(on).not.toContain(">ON<");

    // Connecting is the invitation and wears the amber; leaving never shouts.
    expect(off).toContain("bg-accent");
    expect(on).not.toContain("bg-accent");
  });

  it("keeps ONE control for both verbs, in the same place in both states", () => {
    const buttons = (markup: string) => (markup.match(/<button/g) ?? []).length;
    expect(buttons(row({ isOn: true }))).toBe(1);
    expect(buttons(row())).toBe(1);
  });

  it("says which way it is moving, and asks before it answers", () => {
    expect(row({ isBusy: true })).toContain("Connecting…");
    expect(row({ isOn: true, isBusy: true })).toContain("Disconnecting…");

    const checking = row({ isChecking: true });
    expect(checking).toContain("Checking…");
    // Neither a verdict nor a verb before the machine has answered: there is no
    // direction to offer yet.
    expect(checking).not.toContain("Not connected");
    expect(checking).not.toContain(">Connect<");
    expect(checking).toContain('aria-busy="true"');
  });

  // Reported over the settings dialog: the notifications panel is too big, I want
  // only one Connect/Disconnect button there.
  it("is the button and nothing else — no verdict line, no sentence", () => {
    const markup = row({ machine: "gateway.example.com" });
    expect(markup).not.toContain("gateway.example.com will not alert this device.");
    expect(markup).not.toContain("Not connected");
    expect(markup).not.toContain("Connected");
    expect(markup).toContain(">Connect<");
    // One line of controls, so the row is the button's own height.
    expect(markup).not.toContain("min-h-12");
  });
});

// The call sites, one layer down again.
describe("the session screen and the settings dialog spell no control out", () => {
  it("leaves not one hand-rolled button in either", () => {
    expect(sessionScreenSource).not.toContain("<button");
    expect(settingsSource).not.toContain("<button");
  });

  it("uses ONE microphone control: tap acts, hold switches the mode", () => {
    expect(sessionScreenSource).not.toContain("tap V again");
    expect(sessionScreenSource).toContain(
      "Voice conversation · Listening · tap the microphone again to finish",
    );
    expect(sessionScreenSource).toContain(
      "Listening · tap the microphone again to finish",
    );
    expect(sessionScreenSource).toContain(
      "Voice conversation · Vis is working",
    );
    expect(sessionScreenSource).toContain(
      "Voice conversation · Speaking · tap the microphone to stop",
    );
    // The mode menu, the disclosure that opened it (and the `border-l` divider
    // painted on that disclosure), and the separate leave button are all gone:
    // one control carries the mode now, so there is nothing left to disclose
    // and nothing left to leave by.
    expect(sessionScreenSource).not.toContain('aria-label="Microphone mode"');
    expect(sessionScreenSource).not.toContain('label="Choose microphone mode"');
    expect(sessionScreenSource).not.toContain("voiceModeMenuOpen");
    expect(sessionScreenSource).not.toContain(
      'label="Leave voice conversation"',
    );
    expect(sessionScreenSource).not.toContain("border-l border-dialog-edge");
    // A hidden gesture is only hidden if nothing says it: the accessible name
    // carries the act AND the switch, in both modes.
    expect(sessionScreenSource).toContain(
      '"Dictate message — hold to switch to voice conversation"',
    );
    expect(sessionScreenSource).toContain(
      '"Start voice utterance — hold to switch to dictation"',
    );
    // A finger is not the only pointer, and a keyboard cannot press and hold.
    expect(sessionScreenSource).toContain("onContextMenu");
    expect(sessionScreenSource).toContain(
      'event.key === "Enter" && event.shiftKey',
    );
    // Holding ARMS the conversation; it must not start recording in the same
    // gesture, so the entry path opens the route and stops there.
    expect(sessionScreenSource).toMatch(
      /const enterVoiceConversation[\s\S]*?await beginVoiceAudioSession\(\);\n  \};/,
    );
  });

  it("paints completed terminal prose before transcript hydration", () => {
    expect(sessionScreenSource).toContain(
      "content: terminalBlocks?.length ? terminalBlocks : turn.content",
    );
  });

  it("keeps painted answer prose through mobile wake reconciliation", () => {
    expect(sessionScreenSource).toContain(
      "const liveHadProse = liveTurnCarriesProse(liveBefore)",
    );
    expect(sessionScreenSource).toContain(
      "const liveHadOutput = liveTurnCarriesOutput(liveBefore)",
    );
    expect(sessionScreenSource).toMatch(
      /liveTurnSettledRow\([\s\S]*?liveRequest,[\s\S]*?liveHadOutput,[\s\S]*?liveHadProse,[\s\S]*?\)/,
    );
  });

  it("holds one Android audio route for the whole voice conversation", () => {
    expect(sessionScreenSource).toContain("await beginVoiceAudioSession()");
    expect(sessionScreenSource).toContain("await endVoiceAudioSession()");
  });

  it("uses the composer's own vocabulary where it used to repeat itself", () => {
    expect(sessionScreenSource).toContain("<ComposerButton");
    expect(sessionScreenSource).toContain("<MetaButton");
    expect(sessionScreenSource).toContain("<OptionRow");
    expect(sessionScreenSource).toContain("<TextButton");
    expect(sessionScreenSource).toContain("<BackButton");
    expect(sessionScreenSource).toContain("<Pill");
    // The transcript's "load earlier" is the artifacts sheet's own bar, turned over.
    expect(sessionScreenSource).toContain("<LoadMore");
    expect(sessionScreenSource).toContain("isEarlier");
    expect(sessionScreenSource).not.toContain("grid h-8 w-7");
    expect(sessionScreenSource).not.toContain(
      "tracking-[0.08em] text-dialog-hint-key underline",
    );
  });

  it("moves the settings picker and the switch into the vocabulary", () => {
    expect(settingsSource).toContain("<ChoiceCell");
    expect(settingsSource).toContain("<Switch");
    expect(settingsSource).not.toContain("function ChoiceCell");
    expect(settingsSource).not.toContain("function Switch");
  });

  // Regression, user report ("I am on the same device and I see four entries — what
  // is even the purpose? I should just know if I am connected or not"): the
  // notifications panel rendered one row per push TOKEN the gateway holds, so one
  // iPhone reinstalled three times filled it with four masked-token rows of the same
  // phone, and `registered && notify` — the only thing the reader asked for — was
  // left to the verb printed on a button.
  it("answers the notifications question with one row, not a token list", () => {
    // Native push and Web Push are two transports for ONE question, so both panels
    // ask it with the same row.
    expect(settingsSource.match(/<NotifyConnectionRow/g)).toHaveLength(2);
    expect(settingsSource).not.toContain("Notify me from this machine");
    expect(settingsSource).not.toContain("Stop notifying me from this machine");
    expect(settingsSource).not.toContain("devices?.map(");
    expect(settingsSource).not.toContain(
      "No devices registered with this machine.",
    );
    expect(settingsSource).not.toContain("Checking registered devices…");
    // The list is still READ — matching this device's masks against it is how the
    // panel knows it is registered at all — it is simply never rendered.
    expect(settingsSource).toContain("masks.includes(d.token_preview)");
  });

  it("picks a saved machine with the one pressable row", () => {
    // The rows moved out of the screen and into `Machines`, shared with the cog's
    // settings dialog — so the rule follows the rows.
    expect(machinesSource).toContain("<ListRow");
    expect(machinesSource).not.toContain("<button");
    expect(connectSource).not.toContain("<button");
  });

  // Regression, user reports on the machines column, paraphrased in the order they
  // arrived: the ⋯ and the swiping were both unwanted, the long description was
  // rejected outright and the ＋'s alignment with it; then the two rows selected in
  // red and the ＋ were rejected again, with the swipe asked for as a permanent
  // right-hand strip and the ⋯ dropped; then "you removed
  // the slides from the session list and also from the machine — we should have the
  // slide and just fix it". The band spelled a 310-character paragraph, then a
  // one-line one, plus a meta naming the very machine the rows under it already name
  // — 71px of header over a 48px row — and the ＋ was an amber slab of the mark this
  // app spends on a NEW SESSION. The verbs went from a slide with a `⋯` beside it, to
  // a strip of full-width words under the one row being read, to marks painted
  // permanently in every row's trailing cell. The SLIDE is the surface both lists
  // keep; the `⋯` is what goes.
  it("gives the band one line and keeps every row's verbs under its own slide", () => {
    // ONE row-verb surface in this app, on both lists, with nothing standing beside it.
    // (The row's key moved out to the block that holds the row AND the settings
    // that machine discloses under it.)
    expect(machinesSource).toContain(
      "<SwipeActions label={name} actions={actions}>",
    );
    expect(sessionsListSource).toContain("<SwipeActions");
    expect(machinesSource).not.toContain("KebabButton");
    expect(machinesSource).not.toContain("Actions for");
    expect(machinesSource).not.toContain('density="panel"');
    expect(machinesSource).not.toContain("isReading && verbs.length");
    // And no cluster of permanent marks in a row's trailing cell either: three
    // glyphs per row cost 92px of a 320px row the name came for.
    expect(machinesSource).not.toContain("RowVerbs");
    expect(sessionsListSource).not.toContain("RowVerbs");
    expect(uiSource).not.toContain("export function RowVerbs(");

    // The verb is the band's trailing CELL, centred against the title's own cell,
    // and that cell is what wraps — never the line the verb stands on.
    const band =
      /bg-level-machine">\s*<div className="([^"]*)"/.exec(
        settingsSource,
      )?.[1] ?? "";
    expect(band).toContain("items-center");
    expect(band).toContain("min-h-12");
    expect(band).not.toContain("items-baseline");

    // MACHINES says its own name and nothing else: no sentence telling the reader
    // to tap a row, and no meta naming one machine over a list of them.
    const machinesColumn =
      /<SettingsColumn\s+title="Machines"([\s\S]*?)>\s*\{\/\*/.exec(
        settingsSource,
      )?.[1] ?? "";
    expect(machinesColumn.length).toBeGreaterThan(0);
    expect(machinesColumn).not.toContain("description=");
    expect(machinesColumn).not.toContain("meta=");
    expect(settingsSource).not.toContain("Swipe a row");
    expect(settingsSource).not.toContain("Tap a machine");
  });

  // Regression, user reports on addresses, in the order they arrived: "Addresses
  // should be also simplified and in slide", then "why are addresses not in the
  // machine's own line? it should be a simple dropdown after I click bind a
  // different address — I don't need a separate place for it". The ADDRESS panel was
  // a SECOND list asking the same question the machines list asks: it spent a
  // 110-character band description, a hint sentence under the row in use, a
  // paragraph under the list and an `Automatic` button beside it, painted the word
  // USE on every row, and it existed only for the one machine being read.
  it("asks for an address on the machine's own line, and keeps no panel of its own", () => {
    // Not moved, not kept beside the new line: gone, with the prose it carried.
    expect(settingsSource).not.toContain("AddressPanel");
    expect(settingsSource).not.toContain("REACH_HINT");
    expect(settingsSource).not.toContain("Automatic: this device prefers");
    expect(settingsSource).not.toContain("Pinned: this device always uses");

    // The route a machine takes is asked for in the row's own slide, beside its other
    // verbs — the app's one strip, opening the app's one menu.
    expect(machinesSource).toContain("Bind ${name} to a different address");
    expect(machinesSource).toContain("<AddressMenu");
    expect(machinesSource).toContain("<MenuItem");
    expect(machinesSource).toContain("<AddressIcon");
    // Only where there is a choice: one address and no pin carries no verb at all.
    expect(machinesSource).toContain("if (bindable)");
    // And no paragraph came back with it: what makes an address durable is said in
    // the menu, on the row it belongs to.
    expect(machinesSource).not.toContain("description=");
  });

  it("keeps no control nobody uses", () => {
    // `Card` and `Section` had no call site left anywhere in the app.
    expect(uiSource).not.toContain("export function Card(");
    expect(uiSource).not.toContain("export function Section(");
  });
});

// Regression, user report ("see the X element — it should be black like everywhere"):
// the search field's Clear rendered rgb(111, 106, 99) (`text-dialog-hint`) beside a
// query and a `Preferences` both at rgb(38, 38, 38), measured on the live bar. The same
// faded ink was worn by every other ✕ in the app — the dialog band's, the image
// viewer's, the artifacts sheet's, the menu sheet's and the composer's — so no ✕
// anywhere carried the page's own ink. A ✕ IS INK: only the pointer turns it red.
describe("every ✕ in the app", () => {
  it("rests in the ink of the surface it sits on, never in a hint", () => {
    const close = renderToStaticMarkup(
      <CloseButton label="Clear search" onClick={() => {}} />,
    );
    expect(close).toContain("text-current");
    expect(close).toContain("hover:text-err");
    expect(close).not.toContain("text-dialog-hint");

    const field = uiSource.slice(
      uiSource.indexOf("export const SearchField"),
      uiSource.indexOf("export function Banner"),
    );
    expect(field).toContain("<CloseButton");
    expect(field).not.toContain('variant="quiet"');
  });
});

// Regression, user report ("the close buttons look bizarre and they are not the same —
// the one in the queued messages is not black like the other close buttons"): the app
// drew its ✕ at TWO sizes. The band's way out rendered the icon set's own 14px cross,
// while the composer's — a queued message, a paste, an attachment — and the search
// field's Clear shrank it to `size-3`. A 24-unit mark scaled
// to 12px carries a 0.9px stroke: measured on the live tray at 390px, the small cross
// bottomed out at #3a3a3a where the ink it names (`--fg`) is #262626, so a mark that IS
// black rendered grey beside the black one in the band above it, at 59% of its ink. The
// same control washed amber (`bg-warn-surface`) under the pointer while every other way
// out of something washed red.
describe("one ✕, at one size, under one wash", () => {
  const inButton = (html: string) => html.slice(html.indexOf("<button"));
  const markOf = (html: string) =>
    (/<svg[^>]*class="([^"]*)"/.exec(inButton(html))?.[1] ?? "").split(" ");
  const boxOf = (html: string) =>
    (/<button[^>]*class="([^"]*)"/.exec(inButton(html))?.[1] ?? "").split(" ");
  const ways = {
    "a band": renderToStaticMarkup(
      <CloseButton label="Close artifacts" onClick={() => {}} />,
    ),
    "a queued message": renderToStaticMarkup(
      <CloseButton label="Remove queued message 1" onClick={() => {}} />,
    ),
    "a query": renderToStaticMarkup(
      <SearchField
        value="release"
        onValue={() => {}}
        label="Search sessions"
      />,
    ),
  };

  it("draws the icon set's own cross on every surface it leaves", () => {
    for (const [where, html] of Object.entries(ways)) {
      expect(markOf(html), where).toContain("size-3.5");
      expect(markOf(html), where).not.toContain("size-3");
    }
    // And no call site in the vocabulary shrinks it back down again.
    expect(uiSource).not.toContain('<CloseIcon className="size-3"');
  });

  it("goes red under the pointer wherever it is drawn, never amber", () => {
    for (const [where, html] of Object.entries(ways)) {
      expect(boxOf(html), where).toContain("hover:bg-err/15");
      expect(boxOf(html), where).toContain("hover:text-err");
      expect(boxOf(html), where).not.toContain("hover:bg-warn-surface");
    }
  });

  // The report that finished the job: one component, so one box. Anything in the app
  // that draws a ✕ has to be it — a second `<CloseIcon` at a call site is a second
  // close button growing back.
  it("is the only ✕ in the app, and the app has only one of it", () => {
    const sources = import.meta.glob(["../**/*.tsx"], {
      query: "?raw",
      import: "default",
      eager: true,
    }) as Record<string, string>;
    const drawn: string[] = [];
    for (const [path, source] of Object.entries(sources)) {
      if (path.endsWith(".test.tsx")) continue;
      if (path.endsWith("/icons.tsx")) continue;
      const marks = [...source.matchAll(/<CloseIcon\b/g)].length;
      if (path.endsWith("/ui.tsx")) {
        // The one component, and the one mark inside it.
        expect(marks).toBe(1);
        continue;
      }
      if (marks > 0) drawn.push(path);
    }
    expect(drawn).toEqual([]);
    expect(uiSource).not.toContain("export function DialogClose");
    expect(uiSource).not.toContain("export function RemoveButton");
    expect([
      ...uiSource.matchAll(/export function CloseButton\b/g),
    ]).toHaveLength(1);
  });
});

// Regression, user report ("some of the X are a different X than the dialog ones, and
// white instead of black"): the way out painted its own resting ink — the page's
// `--fg` on a panel band — while the band under it painted `text-accent-foreground`.
// Measured live on the "New project" menu heading, the mark disagreed with the words
// beside it in five of the six shipped themes: a #f3f4f6 ✕ on the #ffc420 band of
// blockether-dark (1.5:1) whose own heading was #0f1117, a #1e1e1e ✕ on the #2563eb
// band of vis-light whose heading was #f0f4fc, grey #839496 on solarized's blue. The
// dialog's ✕ was right for the same reason the menu's was wrong: it names the band's
// token instead of the page's.
describe("the way out wears the ink of its band", () => {
  it("brings no resting ink of its own", () => {
    const close = renderToStaticMarkup(
      <CloseButton label="Close artifacts" onClick={() => {}} />,
    );
    expect(close).toContain("text-current");
    // Only the pointer inks it, and only red.
    expect(
      close.replace(/hover:text-\S+|focus-visible:text-\S+/g, ""),
    ).not.toMatch(
      /\btext-(white|dialog-title-foreground|accent-foreground|dialog-hint)\b/,
    );
  });

  // A band that hosts the mark has to SAY its foreground, or inheriting it means
  // inheriting the page's again.
  it("is hosted only by bands that declare one", () => {
    const menu = renderToStaticMarkup(
      <MenuHeading onClose={() => {}} closeLabel="Close projects on tower">
        Projects · tower
      </MenuHeading>,
    );
    expect(menu).toContain("text-accent-foreground");

    const dialog = renderToStaticMarkup(
      <DialogHeader
        title="Application settings"
        onClose={() => {}}
        closeLabel="Close Application settings"
      />,
    );
    expect(dialog).toContain("text-dialog-title-foreground");
  });
});

// The vocabulary's own rule, made executable: a `className` at a call site may only
// POSITION. It had drifted in nine places — four copies of `min-h-9 px-3 font-mono
// text-meta` two hundred lines apart in one settings screen, a spinner choosing its
// own ink in three files, a trash can painting itself red, a zoom button dropping its
// own frame — and every one of them was a FACE settled by whichever call site Tailwind
// happened to emit last rather than by the component that owns it.
describe("a call site positions, and the component paints", () => {
  const sources = import.meta.glob(["../**/*.tsx"], {
    query: "?raw",
    import: "default",
    eager: true,
  }) as Record<string, string>;

  const controls = new Set(
    [...uiSource.matchAll(/export (?:function|const) ([A-Z]\w+)/g)].map(
      ([, name]) => name,
    ),
  );

  // Anything you can SEE. A width, a margin and a stacking context are PLACEMENT
  // — `flex-1`, `min-w-14`, `absolute inset-x-0` — and stay legal; ink, paper,
  // frame, type, padding and height belong to the owner.
  const paint =
    /^(?:sm:|md:|lg:|mouse:|hover:|focus:|focus-visible:|active:|disabled:|motion-reduce:|dark:)*(?:text-|font-|bg-|border|rounded|shadow|opacity-|italic|uppercase|tracking-|leading-|p-|px-|py-|pt-|pb-|pl-|pr-|min-h-|h-\d)/;

  function paintAtCallSites(source: string): string[] {
    const offenders: string[] = [];
    for (const [, name, between, classes] of source.matchAll(
      /<([A-Z]\w+)([^>]*?)className=\{?["'`]([^"'`]*)["'`]/g,
    )) {
      if (!controls.has(name) || between.includes(">")) continue;
      const seen = classes.split(/\s+/).filter((one) => paint.test(one));
      if (seen.length > 0) offenders.push(`${name}: ${seen.join(", ")}`);
    }
    return offenders;
  }

  it("tells placement from paint", () => {
    expect(paintAtCallSites('<Button className="flex-1 shrink-0" />')).toEqual(
      [],
    );
    expect(
      paintAtCallSites(
        '<Button className="min-h-9 px-3 font-mono text-meta" />',
      ),
    ).toEqual(["Button: min-h-9, px-3, font-mono, text-meta"]);
    expect(paintAtCallSites('<Spinner className="text-accent-ink" />')).toEqual(
      ["Spinner: text-accent-ink"],
    );
    // A control this file does not own paints where it stands.
    expect(paintAtCallSites('<div className="bg-panel" />')).toEqual([]);
  });

  it("hands no paint to a control `ui.tsx` owns", () => {
    const offenders = Object.entries(sources).flatMap(([path, source]) =>
      path.includes("/ui.tsx") ||
      path.includes("/dev/") ||
      path.includes(".test.")
        ? []
        : paintAtCallSites(source).map((one) => `${path}: ${one}`),
    );

    expect(offenders).toEqual([]);
  });

  // Regression, user report ("the portal close vs the dialog close and dialog headers
  // are different. WE SHOULD NORMALIZE"): seven surfaces opened over another surface,
  // no two of their title bars agreed on height, alignment or padding, and four of the
  // closes were hand-built at the call site. `DialogHeader.test.tsx` pins what the one
  // band does; this is the rule that keeps a second one from being drawn.
  it("lets no surface paint its own dialog title bar or its own way out", () => {
    const owners = Object.entries(sources).filter(
      ([path]) => !path.includes("/dev/") && !path.includes(".test."),
    );
    const bands = owners.filter(
      ([, source]) =>
        (source.match(/<header[^>]*bg-dialog-title/g) ?? []).length > 0,
    );
    expect(bands.map(([path]) => path)).toEqual(["./ui.tsx"]);
    // The hand-built closes all wore this hairline against the title bar's ink.
    expect(
      owners
        .filter(([, source]) =>
          source.includes(
            "border-l border-dialog-title-foreground/20 text-dialog-title-foreground",
          ),
        )
        .map(([path]) => path),
    ).toEqual([]);
  });

  it("gives a settings panel's verbs one density", () => {
    const panel = renderToStaticMarkup(
      <Button density="panel">Notify me from this machine</Button>,
    );
    expect(panel).toContain("font-mono");
    expect(panel).toContain("min-h-9");
    expect(renderToStaticMarkup(<Button>Save</Button>)).not.toContain(
      "font-mono",
    );
    // The panel's two verbs became one row (`NotifyConnectionRow`), which carries
    // its own; what is left in this screen is the door to the OS that row cannot
    // open itself, plus the MCP and Voices lists' own last rows.
    expect(settingsSource.match(/density="panel"/g)).toHaveLength(3);
  });

  it("gives the spinner the app's waiting ink as a tone", () => {
    expect(renderToStaticMarkup(<Spinner tone="accent" />)).toContain(
      "text-accent-ink",
    );
    expect(renderToStaticMarkup(<Spinner />)).not.toContain("text-accent-ink");
  });

  it("keeps a disclosure's own gutter on the disclosure", () => {
    expect(
      renderToStaticMarkup(
        <Disclosure isOpen={false} bleed tone="step">
          Ran a tool
        </Disclosure>,
      ),
    ).toContain("-ml-2");
    expect(
      renderToStaticMarkup(<Disclosure isOpen={false}>Ran a tool</Disclosure>),
    ).not.toContain("-ml-2");
  });

  it("lets a dialog band clear the notch and stand over another band", () => {
    expect(
      renderToStaticMarkup(<DialogHeader title="Pasted #1" isUnderNotch />),
    ).toContain("pt-[env(safe-area-inset-top)]");
    // ...as a strip STANDING ON the band, never taken out of its `min-h-12` row —
    // `DialogHeader.test.tsx` holds the numbers that reported it.
    expect(
      renderToStaticMarkup(<DialogHeader title="Pasted #1" isUnderNotch />),
    ).toContain("box-content");
    expect(
      renderToStaticMarkup(<DialogHeader title="report.png" isStacked />),
    ).toContain("border-dialog-title-foreground/20");
    expect(
      renderToStaticMarkup(<DialogHeader title="Pasted #1" />),
    ).not.toContain("safe-area-inset-top");
  });

  it("draws the zoom bar as one frame", () => {
    expect(
      renderToStaticMarkup(
        <Button variant="secondary" isJoined>
          100%
        </Button>,
      ),
    ).toContain("border-x-0");
    expect(imageViewerSource).toContain("isJoined");
  });

  // The trash in "Manage projects" was the one destructive icon in the app that was
  // red at REST, beside a `CloseButton` that is ink until the pointer arrives — one
  // gesture wearing two faces on the same sheet. The variant carries that ink for the
  // marks that are NOT the ✕; the ✕ itself is `CloseButton` and never a variant.
  it("removes a project in the app's one destructive ink", () => {
    expect(manageProjectsSource).toContain('variant="remove"');
    expect(manageProjectsSource).not.toContain('className="text-err"');
  });
});
// The user's own words: "go over all close buttons and ensure we are using them
// consistently". The MARK was already the app's one way out, but its NAME was
// not: `DialogFrame` welded "Close dialog" onto five different surfaces, a menu band
// said plain "Close", and an artifact opened inside the artifacts sheet called the
// same X "Back to artifacts" while the document opened beside it called it
// "Close report.pdf" — one gesture with four names, and a screen reader on a stack of
// three open bands could not tell the human which one it was about to leave.
describe("one way out, and it says what it closes", () => {
  const sources = import.meta.glob(["../**/*.tsx"], {
    query: "?raw",
    import: "default",
    eager: true,
  }) as Record<string, string>;

  /**
   * Every opening `<Tag …>` in a file, as text. Braces are counted so the `>` inside
   * an arrow function or a nested `footer={<div>…}` never ends the element early.
   */
  const elementsOf = (source: string, tag: string) => {
    const found: string[] = [];
    const open = new RegExp(`<${tag}\\b`, "g");
    for (let match = open.exec(source); match; match = open.exec(source)) {
      let depth = 0;
      for (let i = match.index; i < source.length; i += 1) {
        const ch = source[i];
        if (ch === "{") depth += 1;
        else if (ch === "}") depth -= 1;
        else if (ch === ">" && depth === 0) {
          found.push(source.slice(match.index, i + 1));
          break;
        }
      }
    }
    return found;
  };

  const shipped = Object.entries(sources).filter(
    ([path]) => !path.endsWith(".test.tsx"),
  );

  it("never leaves an icon-only way out unnamed", () => {
    // The scan itself, on the two shapes it has to tell apart.
    const sample = [
      "<MenuHeading onClose={onCancel}>{`Projects · ${label}`}</MenuHeading>",
      "<MenuHeading onClose={onCancel} closeLabel={`Close projects`}>x</MenuHeading>",
    ].join("\n");
    const [bare, named] = elementsOf(sample, "MenuHeading");
    expect(bare?.includes("closeLabel")).toBe(false);
    expect(named?.includes("closeLabel")).toBe(true);

    const unnamed: string[] = [];
    for (const [path, source] of shipped) {
      for (const tag of ["DialogHeader", "MenuHeading"]) {
        for (const element of elementsOf(source, tag)) {
          const named =
            element.includes("closeLabel") || element.includes("closeWith(");
          if (element.includes("onClose") && !named) {
            unnamed.push(`${path} <${tag}>`);
          }
        }
      }
      for (const element of elementsOf(source, "CloseButton")) {
        if (!element.includes("label")) unnamed.push(`${path} <CloseButton>`);
      }
    }
    expect(unnamed).toEqual([]);
  });

  it('never names a way out just "Close"', () => {
    const generic: string[] = [];
    for (const [path, source] of shipped) {
      if (/\blabel="Close"/.test(source)) generic.push(path);
    }
    expect(generic).toEqual([]);
  });

  it("takes the dialog's own title as the name of its way out", () => {
    const html = renderToStaticMarkup(
      <DialogFrame title="Machine settings" onClose={() => {}}>
        body
      </DialogFrame>,
    );
    expect(html).toContain('aria-label="Close Machine settings"');
    // The five words that used to name five different surfaces.
    expect(uiSource).not.toContain("Close dialog");
  });

  it("lets a dialog say what LEAVING does instead", () => {
    const html = renderToStaticMarkup(
      <DialogFrame
        title="Which branch?"
        closeLabel="Cancel this request"
        onClose={() => {}}
      >
        body
      </DialogFrame>,
    );
    expect(html).toContain('aria-label="Cancel this request"');
    expect(humanInputSource).toContain("closeLabel: 'Cancel this request'");
  });

  it("gives a band with nowhere to go no way out at all", () => {
    expect(
      renderToStaticMarkup(<DialogHeader title="How to fix it" />),
    ).not.toContain("<button");
    expect(
      renderToStaticMarkup(<MenuHeading>Projects · tower</MenuHeading>),
    ).not.toContain("<button");
  });

  it("closes a menu band by the name of the panel it holds", () => {
    expect(
      renderToStaticMarkup(
        <MenuHeading onClose={() => {}} closeLabel="Close projects on tower">
          Projects · tower
        </MenuHeading>,
      ),
    ).toContain('aria-label="Close projects on tower"');
  });

  it("calls the artifact's way out the same thing on both surfaces", () => {
    expect(artifactsSheetSource).toContain("closeLabel={`Close ${name}`}");
    expect(docSource).toContain("closeLabel={`Close ${name}`}");
    expect(artifactsSheetSource).not.toContain(
      'closeLabel="Back to artifacts"',
    );
  });
});

// A pocket voice IS a reference recording, so "create a voice" is an upload. The band
// that does it stands in the MACHINE's own settings column, beside that machine's other
// inventories, because the clip is stored on the machine and every session there speaks
// with the same catalogue.
describe("the machine's voices", () => {
  const panel = settingsSource.slice(
    settingsSource.indexOf("function VoicesPanel"),
    settingsSource.indexOf("function FormLabel"),
  );

  it("is a band on the machine's column and disappears where speech is not installed", () => {
    expect(settingsSource).toContain("<VoicesPanel client={client} />");
    // 501 is a Vis with no voice extension — the ordinary one. Speech is not required
    // to run Vis, so the band goes away entirely instead of explaining a feature this
    // machine does not have, or worse, painting a red banner about it.
    expect(panel).toContain("e.status === 501");
    expect(panel).toContain("if (isAbsent) return null;");
  });

  it("offers the import on the engine's own word, never by being refused", () => {
    // An engine that cannot clone answers 409 to an upload. The screen reads the flag
    // the gateway already published rather than finding out the hard way.
    expect(panel).toContain("catalogue?.engine?.is_voice_import === true");
    expect(panel).toContain("{canImport && (");
  });

  it("sends the recording itself, and the words it says with it", () => {
    expect(panel).toContain("client.importSpeechVoice(clip, {");
    // The transcript is what the model is TOLD, which is what makes a clone track the
    // voice instead of guessing the words.
    expect(panel).toContain("text: says.trim() || undefined");
    expect(panel).toContain('accept="audio/*"');
    expect(panel).toContain('aria-label="Recording to import as a voice"');
  });

  it("only takes back what somebody brought, and asks first", () => {
    expect(panel).toContain("voice.is_imported && confirming !== voice.id");
    expect(panel).toContain("<ConfirmRow");
    expect(panel).toContain("client.forgetSpeechVoice(voice.id)");
  });

  it("is built from the closed vocabulary", () => {
    expect(panel).toContain("<SettingsPanel");
    expect(panel).toContain("<Button");
    expect(panel).toContain("<Input");
    expect(panel).toContain("<Banner");
    expect(panel).not.toContain("<button");
    // The one raw element is the file picker the platform owns; it is never seen.
    expect(panel.match(/<input/g)).toHaveLength(1);
  });
});


// Reported over the settings dialog: PROVIDERS, NOTIFICATIONS and MCP SERVERS did
// not stand apart from the MACHINES band above them, so the hierarchy read flat.
describe("a panel band sits UNDER its column band, never beside it", () => {
  const band = (marker: string) => {
    const start = settingsSource.indexOf(marker);
    return settingsSource.slice(start, settingsSource.indexOf("</header>", start));
  };
  const column = band("function SettingsColumn");
  const panel = band("export function SettingsPanel");

  it("keeps the paper, the size and the white for the column alone", () => {
    expect(column).toContain("bg-level-machine");
    expect(column).toContain("text-ui font-black");
    expect(column).toContain("text-white");

    expect(panel).not.toContain("bg-level-machine");
    expect(panel).not.toContain("bg-panel-2");
    expect(panel).not.toContain("text-white");
    // One step smaller, in the hint colour, marked only by the accent tick.
    expect(panel).toContain("text-chip font-bold uppercase");
    expect(panel).toContain("text-dialog-hint");
    expect(panel).toContain("border-l-2 border-accent");
  });
});
