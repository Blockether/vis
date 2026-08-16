// @vitest-environment jsdom
import { act } from "react";
import { createRoot } from "react-dom/client";
import { renderToStaticMarkup } from "react-dom/server";
import { beforeEach, describe, expect, it, vi } from "vitest";

import {
  ANNOTATION_COLORS,
  annotationColor,
  annotationWash,
  type DocumentChrome,
  MarkdownAnnotator,
  UNSAVED_NOTE,
} from "./MarkdownArtifact";

// The device's draft store is one bridge call away (`lib/annotation-drafts`), and these
// tests own what it holds: the native half is a map they can empty.
const native = vi.hoisted(() => ({ store: new Map<string, string>() }));

vi.mock("@capacitor/preferences", () => ({
  Preferences: {
    get: async ({ key }: { key: string }) => ({
      value: native.store.get(key) ?? null,
    }),
    set: async ({ key, value }: { key: string; value: string }) => {
      native.store.set(key, value);
    },
    remove: async ({ key }: { key: string }) => {
      native.store.delete(key);
    },
  },
}));

import {
  annotationDraftKey,
  resetAnnotationDraftCache,
  writeAnnotationDraft,
} from "../lib/annotation-drafts";

const html = (node: Parameters<typeof renderToStaticMarkup>[0]) =>
  renderToStaticMarkup(node);

const noop = async () => undefined;

/**
 * A document is always read inside somebody else's chrome, and this stands in for
 * it with the two parts a band has: the verbs it offers, and what it reports.
 */
const chrome = ({ actions, note, body }: Parameters<DocumentChrome>[0]) => (
  <div>
    <header>
      {actions}
      {note}
    </header>
    {body}
  </div>
);

/**
 * One finger, down and up on the same spot. React reads the POINTER, not
 * `click`, so a flick of the page can be told from a tap on a passage; jsdom
 * carries no `PointerEvent`, and a `MouseEvent` under that type name has the
 * coordinates the handler reads.
 */
const pointer = (type: string, x: number, y: number) =>
  new MouseEvent(type, { bubbles: true, clientX: x, clientY: y });

const tap = (element: Element, at = { x: 20, y: 20 }) => {
  element.dispatchEvent(pointer("pointerdown", at.x, at.y));
  element.dispatchEvent(pointer("pointerup", at.x, at.y));
};

/** A press that travelled: the page was scrolled, the note was not tapped. */
const flick = (element: Element) => {
  element.dispatchEvent(pointer("pointerdown", 20, 200));
  element.dispatchEvent(pointer("pointerup", 22, 40));
};

/** React reads a controlled field through the native setter, never `value =`. */
const typeInto = (field: HTMLTextAreaElement, text: string) => {
  const setter = Object.getOwnPropertyDescriptor(
    HTMLTextAreaElement.prototype,
    "value",
  )!.set!;
  act(() => {
    setter.call(field, text);
    field.dispatchEvent(new Event("input", { bubbles: true }));
  });
};

/** WHICH document these remarks belong to, the way the app spells it. */
const DRAFT_KEY = annotationDraftKey("http://10.0.0.5:7777", "s1", "i1", "PLAN.md");

/** An annotator wired to the device's draft store, as an opened artifact is. */
const mountAnnotator = (
  text: string,
  onSave: (body: string) => Promise<number | undefined> = noop,
) => {
  globalThis.IS_REACT_ACT_ENVIRONMENT = true;
  const host = document.createElement("div");
  document.body.append(host);
  const root = createRoot(host);
  act(() => {
    root.render(
      <MarkdownAnnotator
        text={text}
        onSave={onSave}
        chrome={chrome}
        draftKey={DRAFT_KEY}
      />,
    );
  });
  return {
    host,
    band: () => host.querySelector("header")!,
    comments: () => host.querySelector('ul[aria-label="Comments"]'),
    verb: (label: string) =>
      [...host.querySelectorAll("button")].find(
        (button) => button.textContent === label,
      )!,
    done: () => {
      act(() => root.unmount());
      host.remove();
    },
  };
};

/** What a reader does: open the whole-document composer, type, commit. */
const remark = (host: HTMLElement, body: string) => {
  const open = host.querySelector<HTMLButtonElement>(
    'button[aria-label="Comment on the whole document"]',
  )!;
  act(() => open.click());
  typeInto(
    host.querySelector<HTMLTextAreaElement>('textarea[aria-label="Comment"]')!,
    body,
  );
  const commit = [...host.querySelectorAll("button")].find((button) =>
    /^(Add|Update) comment$/.test(button.textContent ?? ""),
  )!;
  act(() => commit.click());
};

beforeEach(() => {
  native.store.clear();
  globalThis.localStorage.clear();
  resetAnnotationDraftCache();
});

describe("an opened markdown note", () => {
  it("renders the note as prose, and instructs nobody", () => {
    const markup = html(
      <MarkdownAnnotator
        text={"# Ship it\n\n- one\n- two\n"}
        onSave={noop}
        chrome={chrome}
      />,
    );
    expect(markup).toContain("<h1");
    expect(markup).toContain("Ship it");
    expect(markup).not.toContain("# Ship it");
    // Regression, user report ("that tap something something has to go"): a standing
    // instruction for the gesture the reader had just performed sat under the prose,
    // on the one screen a keyboard is covering.
    expect(markup).not.toContain("Tap a passage");
    // The band offers both things done to the WHOLE document, side by side.
    expect(markup).toContain("Comment all");
    expect(markup).toContain("Save");
    expect(markup.indexOf("Comment all")).toBeLessThan(markup.indexOf("Save"));
  });

  it("shows the comments the note already carries, outside its prose", () => {
    const markup = html(
      <MarkdownAnnotator
        text={
          "# Ship it\n\n## Comments\n\n- **“Ship it”** — When exactly?\n" as string
        }
        onSave={noop}
        chrome={chrome}
      />,
    );
    expect(markup).toContain('aria-label="Comments"');
    expect(markup).toContain("When exactly?");
    // The section is the annotation layer, not part of the document's prose.
    expect(markup).not.toContain("## Comments");
    expect(markup).not.toContain("- **");
  });
});

// Regression: on an iPhone a long press inside the note raised iOS's own
// callout (Copy / Look Up) and settled the range after `touchend`, so the
// composer never opened and a note could not be commented on at all. A tap on a
// block is the touch gesture now.
describe("picking a passage on a touch screen", () => {
  it("quotes the tapped block without any text selection", () => {
    globalThis.IS_REACT_ACT_ENVIRONMENT = true;
    const host = document.createElement("div");
    document.body.append(host);
    const root = createRoot(host);
    act(() => {
      root.render(
        <MarkdownAnnotator
          text={"# Ship it\n\nWe cut on Friday.\n"}
        onSave={noop}
        chrome={chrome}
        />,
      );
    });

    const paragraph = host.querySelector("p");
    expect(paragraph?.textContent).toBe("We cut on Friday.");
    act(() => {
      tap(paragraph!);
    });

    // Regression, user report ("in the input I don't need to show what is
    // selected — the colour already tells me"): the composer opens with the field
    // and nothing else. The passage is what says which passage it is.
    expect(host.querySelector('textarea[aria-label="Comment"]')).not.toBeNull();
    expect(host.textContent).not.toContain("Comment on “We cut on Friday.”");

    // The native callout is what broke it, so the prose does not offer one on a
    // touch screen; a mouse keeps its drag-selection.
    const prose = paragraph?.closest("div[class*=overflow-y-auto]");
    expect(prose?.className).toContain("select-none");
    expect(prose?.className).toContain("mouse:select-text");
    expect(prose?.className).toContain("[-webkit-touch-callout:none]");

    act(() => root.unmount());
    host.remove();
  });

  // Regression, user report ("why isn't the global save next to that whole close on
  // the header bar"): the document's one verb was a docked footer under the comments —
  // a 28px face on a 53px strip at the far end of the column from the ✕ — while every
  // other dialog verb in this app is a cell of the band that names what it acts on.
  it("hands its one verb to the band and docks no footer of its own", () => {
    const markup = html(
      <MarkdownAnnotator
        text={"# Ship it\n\nWe cut on Friday.\n"}
        onSave={noop}
        chrome={chrome}
      />,
    );
    // The column itself does not scroll — only the prose inside it does — and it
    // ends above the home indicator now that nothing is docked under it.
    expect(markup).toContain("overflow-hidden");
    expect(markup).toContain("flex-1 touch-manipulation overflow-y-auto");
    expect(markup).toContain("env(safe-area-inset-bottom)");

    const band = markup.slice(0, markup.indexOf("</header>"));
    const column = markup.slice(markup.indexOf("</header>"));
    // The ✕'s own cell with a word in it: welded by the band's hairline and as
    // tall as the band, never a bordered button parked on a title.
    expect(band).toContain(">Save<");
    expect(band).toContain("border-l");
    expect(band).toContain("self-stretch");
    expect(column).not.toContain(">Save<");
  });
});

// A remark used to be an anonymous grey box under a page of untouched prose:
// nothing said which line it was about. Each comment now has an ordinal and a
// colour, and the quoted block wears both.
describe("marking up the passages a comment is about", () => {
  it("underlines each quoted block in its comment's colour and numbers it", () => {
    globalThis.IS_REACT_ACT_ENVIRONMENT = true;
    const host = document.createElement("div");
    document.body.append(host);
    const root = createRoot(host);
    act(() => {
      root.render(
        <MarkdownAnnotator
          text={
            "# Ship it\n\nWe cut on Friday.\n\n## Comments\n\n" +
            "- **\u201cShip it\u201d** \u2014 When exactly?\n" +
            "- **\u201cWe cut on Friday.\u201d** \u2014 Who signs off?\n"
          }
        onSave={noop}
        chrome={chrome}
        />,
      );
    });

    const heading = host.querySelector("h1") as HTMLElement;
    const paragraph = host.querySelector("p") as HTMLElement;
    expect(heading.style.textDecorationLine).toBe("underline");
    // jsdom normalises a hex colour to `rgb(...)`, so the palette is compared
    // through the same normalisation rather than by spelling.
    const asRgb = (hex: string) => {
      const probe = document.createElement("span");
      probe.style.color = hex;
      return probe.style.color;
    };
    expect(heading.style.textDecorationColor).toBe(asRgb(annotationColor(0)));
    expect(paragraph.style.textDecorationColor).toBe(asRgb(annotationColor(1)));
    expect(paragraph.style.textDecorationColor).not.toBe(
      heading.style.textDecorationColor,
    );

    const ordinals = Array.from(
      host.querySelectorAll<HTMLElement>("sup[data-comment-ordinal]"),
    ).map((mark) => mark.textContent);
    expect(ordinals).toEqual(["1", "2"]);

    // The list below says the same thing, and removal names the number.
    expect(
      host.querySelector('button[aria-label="Remove comment 2"]'),
    ).not.toBeNull();

    // The card's ordinal is a plain number in its comment's colour: no filled
    // circle, no background, nothing that reads as a control — even though the card
    // around it IS one now, since pressing it reopens the remark.
    const cardOrdinals = Array.from(
      host.querySelectorAll<HTMLElement>(
        'ul[aria-label="Comments"] li button[aria-label^="Edit comment"] sup',
      ),
    );
    expect(cardOrdinals.map((chip) => chip.textContent)).toEqual(["1", "2"]);
    for (const [at, chip] of cardOrdinals.entries()) {
      expect(chip.style.backgroundColor).toBe("");
      expect(chip.className).not.toContain("rounded");
      expect(chip.style.color).toBe(asRgb(annotationColor(at)));
    }

    act(() => {
      root.unmount();
    });
    host.remove();
  });
});

describe("the annotation palette", () => {
  // A mark painted in a hex chosen for the cream light theme is invisible the
  // moment a dark palette is picked: every hue is a shared theme token.
  it("is spelled in theme variables, never in hard-coded hex", () => {
    for (const colour of ANNOTATION_COLORS) {
      expect(colour).toMatch(
        /^(var\(--[a-z0-9-]+\)|color-mix\(in oklab,.*var\(--.*\))$/,
      );
      expect(colour).not.toMatch(/#[0-9a-f]{3}/i);
    }
    // Ten threads on one note, and no two of them the same ink.
    expect(ANNOTATION_COLORS).toHaveLength(10);
    expect(new Set(ANNOTATION_COLORS).size).toBe(10);
    expect(annotationColor(10)).toBe(annotationColor(0));
    expect(annotationWash(0)).toContain(annotationColor(0));
    expect(annotationWash(0)).toContain("color-mix");
  });
});

describe("an opened plain-text artifact", () => {
  it("reads the file verbatim, one quotable line per block", () => {
    const markup = html(
      <MarkdownAnnotator
        text={"# not a heading\nsecond line\n"}
        onSave={noop}
        chrome={chrome}
        plain
      />,
    );
    // Plain text has no markdown to render: the hash is ink, not a heading.
    expect(markup).not.toContain("<h1");
    expect(markup).toContain("# not a heading");
    expect(markup).toContain("second line");
    // Each line is a <p>, so a tap quotes it exactly as a paragraph is quoted.
    expect(markup.match(/<p /g)?.length ?? 0).toBeGreaterThanOrEqual(2);
    expect(markup).not.toContain("Tap a passage");
  });
});

// A remark is not always about a sentence: "this plan is stale" is about the
// note itself, and a reader with nothing to point at could not say it.
describe("a comment on the whole note", () => {
  it("opens the composer with no quote and lists the remark as the whole document", () => {
    const host = document.createElement("div");
    document.body.append(host);
    const root = createRoot(host);
    act(() => {
      root.render(
        <MarkdownAnnotator
          text={"# Ship it\n\nWe cut on Friday.\n"}
        onSave={noop}
        chrome={chrome}
        />,
      );
    });

    const open = host.querySelector<HTMLButtonElement>(
      'button[aria-label="Comment on the whole document"]',
    );
    expect(open).toBeTruthy();
    act(() => {
      open!.click();
    });
    expect(host.textContent).toContain("Comment on the whole document");

    const field = host.querySelector<HTMLTextAreaElement>(
      'textarea[aria-label="Comment"]',
    )!;
    typeInto(field, "This plan is stale.");
    const add = [...host.querySelectorAll("button")].find(
      (button) => button.textContent === "Add comment",
    )!;
    act(() => {
      add.click();
    });

    const list = host.querySelector('ul[aria-label="Comments"]')!;
    expect(list.textContent).toContain("Whole document");
    expect(list.textContent).toContain("This plan is stale.");
    // Nothing in the prose is marked: the remark points at no passage.
    expect(host.querySelector("h1")!.style.textDecorationLine).toBe("");

    act(() => {
      root.unmount();
    });
    host.remove();
  });

  it("draws the card ordinal as a superscript, smaller than the remark", () => {
    const markup = html(
      <MarkdownAnnotator
        text={
          "# Ship it\n\n## Comments\n\n- **Whole document** \u2014 Stale.\n"
        }
        onSave={noop}
        chrome={chrome}
      />,
    );
    expect(markup).toContain("<sup");
    expect(markup).toContain("Whole document");
    expect(markup).toContain("Stale.");
  });
});

// Regression: every tap on the note opened the composer, so flicking the page to
// read further quoted whatever paragraph the finger happened to land on; and a
// mis-tap could only be undone through Cancel, while the passage the composer
// was quoting carried no mark at all.
describe("the tap that quotes a passage", () => {
  const mount = (text: string) => {
    globalThis.IS_REACT_ACT_ENVIRONMENT = true;
    const host = document.createElement("div");
    document.body.append(host);
    const root = createRoot(host);
    act(() => {
      root.render(
        <MarkdownAnnotator text={text} onSave={noop} chrome={chrome} />,
      );
    });
    return {
      host,
      paragraph: host.querySelector("p") as HTMLElement,
      done: () => {
        act(() => root.unmount());
        host.remove();
      },
    };
  };

  it("leaves the note alone when the finger was scrolling", () => {
    const { host, paragraph, done } = mount("# Ship it\n\nWe cut on Friday.\n");
    act(() => {
      flick(paragraph);
    });
    expect(paragraph.dataset.quotePending).toBeUndefined();
    expect(host.querySelector('textarea[aria-label="Comment"]')).toBeNull();
    done();
  });

  it("marks the picked passage and releases it when it is tapped again", () => {
    const { host, paragraph, done } = mount("# Ship it\n\nWe cut on Friday.\n");
    act(() => {
      tap(paragraph);
    });
    expect(paragraph.dataset.quotePending).toBe("true");

    act(() => {
      tap(paragraph);
    });
    expect(host.querySelector('textarea[aria-label="Comment"]')).toBeNull();
    expect(paragraph.dataset.quotePending).toBeUndefined();
    done();
  });

  // Regression, user report ("when we select I don't need that yellow rail on the
  // left, and the input doesn't have to show what is selected"): the pending
  // passage carried a 2px accent rail down its leading edge AND a caption under
  // the prose repeating its words, on the one screen a keyboard is covering.
  it("marks the pending passage with its paper alone — no rail, no caption", () => {
    const { host, paragraph, done } = mount("# Ship it\n\nWe cut on Friday.\n");
    act(() => {
      tap(paragraph);
    });

    expect(paragraph.dataset.quotePending).toBe("true");
    expect(paragraph.style.boxShadow).toBe("");
    expect(host.textContent).not.toContain("Comment on “We cut on Friday.”");
    expect(host.querySelector('textarea[aria-label="Comment"]')).not.toBeNull();
    done();
  });

  // A remark about the WHOLE note has no painted passage to point at, so that one
  // composer still names what it is about.
  it("still names the whole-document remark, which marks nothing", () => {
    const { host, done } = mount("# Ship it\n\nWe cut on Friday.\n");
    const open = host.querySelector<HTMLButtonElement>(
      'button[aria-label="Comment on the whole document"]',
    );
    act(() => {
      open!.click();
    });

    expect(host.textContent).toContain("Comment on the whole document");
    done();
  });

  // Regression, user report ("the input should be ABOVE THE KEYBOARD"): the column
  // reserved the home indicator under the composer even while the keyboard covered
  // it. `--safe-bottom` is `0px` for exactly that moment (`useSafeBottomStyle`).
  it("ends at the keyboard, not at a home indicator the keyboard covers", () => {
    const { host, done } = mount("# Ship it\n\nWe cut on Friday.\n");
    expect(host.innerHTML).toContain(
      "pb-[var(--safe-bottom,env(safe-area-inset-bottom))]",
    );
    expect(host.innerHTML).not.toContain("pb-[env(safe-area-inset-bottom)]");
    done();
  });
});

// Regression, user report ("IN THE DEVICE MEMORY, IF I ADD A COMMENT BUT ACCIDENTALLY LEAVE
// THE ARTIFACT AND COME BACK, IT SHOULD SAY THIS IS AN UNSAVED DRAFT"): adding a remark and
// saving it are deliberately two presses — three remarks ship as one revision — and
// everything between them lived in this component's state, so leaving the overlay threw the
// typed remark away without a word.
describe("a document reopened on unsaved work", () => {
  const TEXT = "# Ship it\n\nWe cut on Friday.\n";

  it("comes back carrying the remark, and the band says it is unsaved", () => {
    const first = mountAnnotator(TEXT);
    remark(first.host, "Stale.");
    expect(first.band().textContent).toContain(UNSAVED_NOTE);
    first.done();

    // The overlay is gone. The device is not.
    const again = mountAnnotator(TEXT);
    expect(again.comments()!.textContent).toContain("Stale.");
    expect(again.band().textContent).toContain(UNSAVED_NOTE);
    again.done();
  });

  it("forgets the draft once the save lands", async () => {
    const saved = mountAnnotator(TEXT, async () => 7);
    remark(saved.host, "Stale.");
    await act(async () => {
      saved.verb("Save").click();
      await Promise.resolve();
    });
    expect(saved.band().textContent).toContain("Saved as v7");
    expect(saved.band().textContent).not.toContain(UNSAVED_NOTE);
    saved.done();

    const again = mountAnnotator(TEXT);
    expect(again.comments()).toBeNull();
    expect(again.band().textContent).not.toContain(UNSAVED_NOTE);
    again.done();
  });

  it("does not call a leftover that says what the file says unsaved work", () => {
    const carried =
      "# Ship it\n\nWe cut on Friday.\n\n## Comments\n\n- **“We cut on Friday.”** — Stale.\n";
    writeAnnotationDraft(DRAFT_KEY, [
      { quote: "We cut on Friday.", body: "Stale." },
    ]);
    const opened = mountAnnotator(carried);
    expect(opened.comments()!.textContent).toContain("Stale.");
    expect(opened.band().textContent).not.toContain(UNSAVED_NOTE);
    opened.done();
  });
});

// Regression, user report ("apart from the trash icon there should be a way that I can click
// the comment and EDIT it"): a written remark offered the bin and nothing else, so fixing one
// word meant deleting it and writing it again from the passage it was about.
describe("a remark already written", () => {
  const TEXT = "# Ship it\n\nWe cut on Friday.\n";

  it("opens in the composer when its card is pressed, and replaces itself", () => {
    const opened = mountAnnotator(TEXT);
    remark(opened.host, "Stale.");

    const card = opened.host.querySelector<HTMLButtonElement>(
      'button[aria-label="Edit comment 1"]',
    )!;
    act(() => card.click());

    const field = opened.host.querySelector<HTMLTextAreaElement>(
      'textarea[aria-label="Comment"]',
    )!;
    // The same words, in the same composer, under a verb that says what it will do.
    expect(field.value).toBe("Stale.");
    expect(opened.verb("Update comment")).toBeTruthy();

    typeInto(field, "Still stale.");
    act(() => opened.verb("Update comment").click());

    // REPLACED, not appended: one remark about the document, saying the new thing.
    expect(opened.comments()!.querySelectorAll("li")).toHaveLength(1);
    expect(opened.comments()!.textContent).toContain("Still stale.");
    opened.done();
  });

  it("is set as quoted prose — italic, justified — and keeps its own bin", () => {
    const opened = mountAnnotator(TEXT);
    remark(opened.host, "Stale.");

    const body = [...opened.comments()!.querySelectorAll("span")].find(
      (span) => span.textContent === "Stale.",
    )!;
    expect(body.className).toContain("text-justify");
    expect(body.className).toContain("italic");

    // Pressing the card edits; the one mark on it still removes.
    expect(
      opened.host.querySelector('button[aria-label="Remove comment 1"]'),
    ).not.toBeNull();
    opened.done();
  });
});

// Regression, user report ("that comment note should be called something like comment global
// or comment whole, and it should be right next to save"): the one remark that is NOT about a
// passage stood under the prose at the opposite end of the screen from Save, which is the
// other thing done to the whole document.
describe("the band of an opened document", () => {
  it("stands the whole-document remark beside Save, one hairline away", () => {
    const opened = mountAnnotator("# Ship it\n\nWe cut on Friday.\n");
    const all = opened.host.querySelector<HTMLButtonElement>(
      'button[aria-label="Comment on the whole document"]',
    )!;
    const save = opened.verb("Save");

    expect(all.closest("header")).toBe(opened.band());
    expect(save.previousElementSibling).toBe(all);
    // Nothing to save yet, and the cell says so by being unpressable.
    expect(save.disabled).toBe(true);

    // It closes while the composer is open: what it would open is already on screen.
    act(() => all.click());
    expect(all.disabled).toBe(true);
    opened.done();
  });
});
