import { renderToStaticMarkup } from "react-dom/server";
import { describe, expect, it } from "vitest";

import uiSource from "./ui.tsx?raw";
import storageSource from "../lib/storage.ts?raw";
import sessionsListSource from "../screens/SessionsScreen.tsx?raw";
import gatewaySource from "../lib/gateway.ts?raw";
import settingsSource from "../screens/SettingsScreen.tsx?raw";
import chatSource from "./ChatContent.tsx?raw";
import docSource from "./DocArtifact.tsx?raw";
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

import { MACHINE_COLORS } from "../lib/machine-colors";
import {
  BackButton,
  EditableName,
  Button,
  Chip,
  ChoiceCell,
  ChoiceRow,
  ComposerButton,
  CopyChip,
  DialogClose,
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
  MachineRail,
  MachineSwitcher,
  MachineTab,
  MetaButton,
  NewSessionButton,
  OptionRow,
  Pill,
  Disclosure,
  RemoveButton,
  Switch,
  TextButton,
  ProjectCrumb,
  RowDisclosure,
  SectionHeader,
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
  it("names the machine in a block of its own hue, in ink", () => {
    const face = machineTagFace(MACHINE_COLORS[0]!);

    expect(face).toContain(MACHINE_COLORS[0]!.dot);
    // A filled block's ink, never the page's: the palette is one lightness and was
    // tuned as INK, so the page colour on top of a hue is a 3.2:1 machine name.
    expect(face).toContain("text-machine-ink");
    expect(face).not.toContain("text-ink ");
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
  it("falls back to a painted block rather than bare ink", () => {
    expect(machineTagFace()).toContain("bg-edge-strong");
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

    expect(html).toContain(MACHINE_COLORS[0]!.dot);
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
      <ProjectCrumb name="vis" isOpen={false} onToggle={() => {}} label="Expand vis" />,
    );

    expect(folded).toContain(LIST_EDGE);
    expect(folded).not.toContain("pl-6");
    expect(folded).toContain('aria-expanded="false"');
  });

  // The trailing cluster is NOT inside the fold: "New session" is the verb this
  // screen exists for and must never be swallowed by a disclosure.
  it("takes only the naming half of the band", () => {
    const html = renderToStaticMarkup(
      <ProjectCrumb name="vis" isOpen onToggle={() => {}} label="Collapse vis" />,
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
    expect(html).toContain("border-l-4");
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
describe("DialogClose", () => {
  const html = (props: Partial<Parameters<typeof DialogClose>[0]> = {}) =>
    renderToStaticMarkup(
      <DialogClose label="Close artifacts" onClose={() => {}} {...props} />,
    );

  it("is welded to the band it closes, by that band’s own hairline", () => {
    expect(html()).toContain("border-l");
    expect(html()).not.toMatch(/class="[^"]*\bborder\s/);
  });

  // Closing is not a destructive act until you mean it.
  it("goes red only under the pointer", () => {
    expect(html()).toContain("hover:bg-err/15");
    expect(html()).toContain("hover:text-err");
    expect(html()).not.toContain('text-err"');
  });

  it("changes nothing but the paper it sits on", () => {
    expect(html()).toContain("border-dialog-title-foreground/20");
    expect(html({ tone: "panel" })).toContain("border-dialog-edge");
  });

  // Regression, user report ("Why not black like all buttons"): the artifacts sheet has
  // no title band to inherit a foreground from — its one row is the filter strip — so
  // its ✕ rested as ink on paper beside a ‹ that is a black block.
  it("brings the title band with it where a surface has none", () => {
    const block = html({ tone: "block" });
    expect(block).toContain("bg-dialog-title");
    expect(block).toContain("text-dialog-title-foreground");
    expect(block).not.toContain("text-current");
  });

  it("is named for what it closes", () => {
    expect(html()).toContain('aria-label="Close artifacts"');
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
    return !html.slice(html.lastIndexOf("<button", at), at).includes("invisible");
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
  // reach both and click-click-click through pages. The band still runs the full
  // width; the control inside it is capped and centred.
  it("keeps the two steps within a thumb's reach of the numbers", () => {
    const html = renderToStaticMarkup(
      <Pager page={4} pageCount={73} onPage={() => {}} label="vis sessions" />,
    );
    // A FIXED cap, not `w-fit`: a cluster that sizes to its own window re-centres
    // whenever the window grows, which is what slid `>` out from under the finger.
    expect(html).toContain("flex w-full max-w-[19rem] items-center gap-1");
    expect(html).toContain("flex justify-center border-t");
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
    const face = "font-mono font-bold text-white max-w-[60%]";
    expect(resting).toContain(face);
    expect(editable).toContain(face);
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

  // The last 8px of the same misalignment: a mark sized to its own ink moved the
  // header NAME beside it.
  // Regression, user report ("on iPhone its not aligned properly"): the glyph
  // column existed only when something filled it, so the machine's NAME began at
  // x=36 behind its hue block while the project header one row below — which has
  // no mark since its disclosure became a pager — began at x=14. The deeper row
  // started further left, and the two names never read as a hierarchy.
  it("reserves one glyph column, marked or not", () => {
    const marked = renderToStaticMarkup(
      <HeaderTitle mark={<MachineMark color={MACHINE_COLORS[0]!} />} name="tower" />,
    );
    const bare = renderToStaticMarkup(<HeaderTitle name="vis" qualifier="~/vis" />);
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
    expect(source).toContain("fixed inset-0 z-50 flex justify-center bg-ink/85");
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

  it("pairs a machine from the column the machines live in", () => {
    expect(settings).toContain(">Pair machine</Button>");
  });

  it("switches machine inside the dialog instead of closing it", () => {
    expect(settings).toContain("<MachineSwitcher>");
    expect(settings).toContain("onSelectGateway");
  });

  it("gives each column its own scroll on desktop", () => {
    // The grid stops at the dialog's height; the column bodies do the scrolling, so
    // reaching a machine's last panel never drags Theme off the top of the screen.
    expect(settings).toContain("sm:overflow-hidden");
    expect(settings).toContain("sm:overflow-y-auto");
    expect(settings).toContain("sm:min-h-0");
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
    expect(settingsSource.match(/<ChoiceCell/g)?.length).toBe(2);
    // The cell moved into the vocabulary; the dialog no longer owns a copy.
    expect(settingsSource.match(/function ChoiceCell\(/g)?.length).toBe(undefined);
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

  it('slices the rows it paints, and asks no second source for them', () => {
    expect(sessions).toContain('projectPage(sessions, page, pageSize)');
    expect(sessions).not.toContain('listProjectPage');
    expect(gateway).not.toContain('listProjectPage');
    expect(gateway).not.toContain('&root=${encodeURIComponent(root)}');
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
    expect(uiSource).toContain(`export const PROSE =\n  '${RULE} text-justify';`);
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
describe('SearchField', () => {
  const field = uiSource.slice(uiSource.indexOf("export const SearchField"));
  // The field's own box: the first class template in the component is its `<label>`.
  const box = (/className={`([^`]*)`}/.exec(field)?.[1] ?? '').split(/\s+/);

  it('wears Button\'s own face and only lights up when focused', () => {
    expect(uiSource).toContain('export const SearchField');
    // Same box as `Button`: flat corners, its border and type step.
    expect(field).toContain('rounded-none');
    expect(field).not.toContain('rounded ');
    // Paper at rest; the input surface and the ring arrive with the caret.
    expect(field).toContain('bg-transparent');
    expect(field).toContain('focus-within:bg-input');
    expect(field).toContain('focus-within:border-accent');
  });

  // Regression, user report ("search HEIGHT still too big taking into account the
  // other buttons"): the field stood 44px on the bar while `Preferences` next to it
  // stands 32px (24px for a mouse), so the one framed box up there was 12px taller
  // than every control it shares the row with. A `Button` already answers this: it
  // paints a 32px face and reaches the finger's 44px through invisible slop. The
  // field does the same, split into TWO strips so the face itself stays the input's
  // own — a press in the middle of the text still places a caret where it landed.
  it('wears the bar’s own face and reaches the touch step around it', () => {
    expect(box).toContain('h-8');
    expect(box).toContain('mouse:h-6');
    expect(box).not.toContain('h-11');
    expect(box).toContain('relative');
    expect(box).toContain('before:-top-1.5');
    expect(box).toContain('after:-bottom-1.5');
    expect(box).toContain('before:h-1.5');
    expect(box).toContain('after:h-1.5');
    // A mouse needs no slop, and the strips would only eat the rows around it.
    expect(box).toContain('mouse:before:content-none');
    expect(box).toContain('mouse:after:content-none');
  });

  // Same report: Clear was a 12px glyph centred in its own 28px box sitting INSIDE
  // the field's inset, so the ✕ ink stopped about 20px short of the border while the
  // placeholder started 10px in — the asymmetry an eye reads as "far from right".
  it('lets Clear absorb the field’s own trailing inset', () => {
    // The list rows' `edge` geometry: the box runs to the border and pads its glyph
    // by exactly the inset the field gives its leading side, so both inks agree.
    expect(field).toMatch(/<IconButton\s+edge/);
    expect(box).toContain('px-3');
    expect(box).toContain('sm:px-4');
  });

  // Regression, user report (paraphrased: the second band looked worse — put search
  // back on the header): a search box is recognised by the magnifying glass INSIDE the
  // open field, and this one carried no mark at all, so a bare framed box on the bar
  // read as "some input" rather than "search".
  it('carries the magnifying glass inside the open field', () => {
    expect(field).toContain('<SearchIcon');
    // Leading, before the input: the mark introduces the field, it does not end it.
    expect(field.indexOf('<SearchIcon')).toBeLessThan(field.indexOf('<input'));
    expect(uiSource).toContain('SearchIcon');
  });

  // It is a SEARCH field, so the phone keyboard says so and nothing autocorrects a
  // machine name into prose.
  it('asks the phone for a search keyboard', () => {
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
});

// Regression, user report ("when we create a new session there's this 'Creating' showing
// but not in the new session button but outside — I want it to show in the button
// itself"): the busy word was parked on the app bar, so the fleet said it was busy while
// the button that had actually been pressed sat there looking untouched.
describe("NewSessionButton, busy", () => {
  const busy = renderToStaticMarkup(
    <NewSessionButton machine="tower" busyLabel="Creating..." onPress={() => {}} />,
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
    expect(uiSource).toContain("size === 'fit' ? 'items-end' : 'items-stretch'");
    expect(uiSource).toContain("size === 'fit' ? 'max-h-full sm:h-auto' : DIALOG_DESKTOP_HEIGHT");
  });

  it("is what the rename/delete question opens in", () => {
    expect(sessionsListSource).toContain('<Modal size="fit" onDismiss={closeRowAction}>');
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
    expect(docSource).not.toContain("<DialogClose");
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
        <CopyChip value="abc" label="Copy session id" mark="#">
          abc12345
        </CopyChip>,
      );

    it("is one box wide enough for 'Copied', so it never jumps", () => {
      expect(first(html())).toContain("min-w-[6ch]");
      expect(first(html())).toContain("h-6");
    });

    it("carries a name and, when there is more to say, a title", () => {
      expect(html()).toContain('aria-label="Copy session id"');
      const titled = renderToStaticMarkup(
        <CopyChip value="abc" label="Copy session id" title="Copy session id\nabc">
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

  describe("RemoveButton", () => {
    const html = (props: Partial<Parameters<typeof RemoveButton>[0]> = {}) =>
      renderToStaticMarkup(<RemoveButton label="Remove notes.md" {...props} />);

    it("is named, red only under the pointer, and a 28px target", () => {
      expect(html()).toContain('aria-label="Remove notes.md"');
      expect(first(html())).toContain("hover:text-err");
      expect(first(html())).toContain("min-h-7");
      expect(first(html())).toContain("w-7");
    });

    it("grows its divider by prop, never by a class at the call site", () => {
      expect(first(html({ edge: true }))).toContain("border-l");
      expect(first(html())).not.toContain("border-l");
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
    expect(settingsSource).toContain("<ListRow");
    expect(settingsSource).not.toContain("min-h-8 border px-2 py-0.5");
    expect(settingsSource).not.toContain(
      "flex min-h-12 w-full items-center gap-2 px-3 py-2",
    );
  });

  it("leaves the composer's three removes as one control and its menu as menu rows", () => {
    expect(sessionScreenSource).toContain("<RemoveButton");
    expect(sessionScreenSource).toContain("<MenuItem");
    expect(sessionScreenSource).toContain("<CopyChip");
    expect(sessionScreenSource).not.toContain("hover:bg-warn-surface hover:text-err");
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
      "variant?: 'primary' | 'secondary' | 'quiet' | 'danger' | 'overlay' | 'close';",
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
      classes(renderToStaticMarkup(<Button variant="secondary">Cancel</Button>)),
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
    const box = (tone: "quiet" | "send" | "stop" | "recording") =>
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
    for (const tone of ["quiet", "send", "stop", "recording"] as const) {
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
    expect(renderToStaticMarkup(<MetaButton isPicker>opus</MetaButton>)).toContain(
      "opus",
    );
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
    const html = renderToStaticMarkup(
      <OptionRow isActive>notes.md</OptionRow>,
    );
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
    const html = renderToStaticMarkup(
      <BackButton label="Back to sessions" />,
    );
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
});

// The call sites, one layer down again.
describe("the session screen and the settings dialog spell no control out", () => {
  it("leaves not one hand-rolled button in either", () => {
    expect(sessionScreenSource).not.toContain("<button");
    expect(settingsSource).not.toContain("<button");
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
    expect(sessionScreenSource).not.toContain("tracking-[0.08em] text-dialog-hint-key underline");
  });

  it("moves the settings picker and the switch into the vocabulary", () => {
    expect(settingsSource).toContain("<ChoiceCell");
    expect(settingsSource).toContain("<Switch");
    expect(settingsSource).not.toContain("function ChoiceCell");
    expect(settingsSource).not.toContain("function Switch");
  });

  it("picks a saved machine with the one pressable row", () => {
    expect(connectSource).toContain("<ListRow");
    expect(connectSource).not.toContain("<button");
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
// faded ink was worn by every other ✕ in the app — `DialogClose` on both its papers (the
// image viewer, the artifacts sheet, the menu sheet) and `RemoveButton` — so no ✕
// anywhere carried the page's own ink. A ✕ IS INK: only the pointer turns it red.
describe("every ✕ in the app", () => {
  it("rests in the ink of the surface it sits on, never in a hint", () => {
    const clear = renderToStaticMarkup(
      <Button variant="close" aria-label="Clear search" />,
    );
    expect(clear).toContain("text-white");
    expect(clear).toContain("hover:text-err");
    expect(clear).not.toContain("text-dialog-hint");

    const field = uiSource.slice(
      uiSource.indexOf("export const SearchField"),
      uiSource.indexOf("export function Banner"),
    );
    expect(field).toContain('variant="close"');
    expect(field).not.toContain('variant="quiet"');

    for (const tone of ["title", "panel"] as const) {
      const close = renderToStaticMarkup(
        <DialogClose label="Close artifacts" tone={tone} onClose={() => {}} />,
      );
      expect(close).not.toContain("text-dialog-hint");
      expect(close).not.toContain("text-dialog-title-foreground/70");
    }

    expect(
      renderToStaticMarkup(<RemoveButton label="Remove notes.md" />),
    ).not.toContain("text-dialog-hint");
  });
});

// Regression, user report ("some of the X are a different X than the dialog ones, and
// white instead of black"): `DialogClose` painted its own resting ink — the page's
// `--fg` on a panel band — while the band under it painted `text-accent-foreground`.
// Measured live on the "Add a project" menu heading, the mark disagreed with the words
// beside it in five of the six shipped themes: a #f3f4f6 ✕ on the #ffc420 band of
// blockether-dark (1.5:1) whose own heading was #0f1117, a #1e1e1e ✕ on the #2563eb
// band of vis-light whose heading was #f0f4fc, grey #839496 on solarized's blue. The
// dialog's ✕ was right for the same reason the menu's was wrong: it names the band's
// token instead of the page's.
describe("the way out wears the ink of its band", () => {
  it("brings no resting ink of its own", () => {
    for (const tone of ["title", "panel"] as const) {
      const close = renderToStaticMarkup(
        <DialogClose label="Close artifacts" tone={tone} onClose={() => {}} />,
      );
      expect(close).toContain("text-current");
      // Only the pointer inks it, and only red.
      expect(close.replace(/hover:text-\S+|focus-visible:text-\S+/g, "")).not.toMatch(
        /\btext-(white|dialog-title-foreground|accent-foreground|dialog-hint)\b/,
      );
    }
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
    expect(paintAtCallSites('<Button className="flex-1 shrink-0" />')).toEqual([]);
    expect(
      paintAtCallSites('<Button className="min-h-9 px-3 font-mono text-meta" />'),
    ).toEqual(["Button: min-h-9, px-3, font-mono, text-meta"]);
    expect(paintAtCallSites('<Spinner className="text-accent-ink" />')).toEqual([
      "Spinner: text-accent-ink",
    ]);
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

  it("gives a settings panel's verbs one density", () => {
    const panel = renderToStaticMarkup(
      <Button density="panel">Notify me from this machine</Button>,
    );
    expect(panel).toContain("font-mono");
    expect(panel).toContain("min-h-9");
    expect(renderToStaticMarkup(<Button>Save</Button>)).not.toContain("font-mono");
    expect(settingsSource.match(/density="panel"/g)).toHaveLength(4);
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
    expect(
      renderToStaticMarkup(<DialogHeader title="report.png" isStacked />),
    ).toContain("border-dialog-title-foreground/20");
    expect(renderToStaticMarkup(<DialogHeader title="Pasted #1" />)).not.toContain(
      "safe-area-inset-top",
    );
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
  // red at REST, beside a `RemoveButton` and a `DialogClose` that are ink until the
  // pointer arrives — one gesture wearing two faces on the same sheet.
  it("removes a project in the app's one destructive ink", () => {
    expect(manageProjectsSource).toContain('variant="close"');
    expect(manageProjectsSource).not.toContain('className="text-err"');
  });
});
// The user's own words: "go over all close buttons and ensure we are using them
// consistently". The MARK was already the app's one `DialogClose`, but its NAME was
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
      for (const element of elementsOf(source, "DialogClose")) {
        if (!element.includes("label")) unnamed.push(`${path} <DialogClose>`);
      }
    }
    expect(unnamed).toEqual([]);
  });

  it("never names a way out just \"Close\"", () => {
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
    expect(renderToStaticMarkup(<DialogHeader title="How to fix it" />)).not.toContain(
      "<button",
    );
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
    expect(artifactsSheetSource).not.toContain('closeLabel="Back to artifacts"');
  });
});
