// @vitest-environment jsdom
import { act } from "react";
import { createRoot } from "react-dom/client";
import { renderToStaticMarkup } from "react-dom/server";
import { describe, expect, it } from "vitest";

import {
  ANNOTATION_COLORS,
  annotationColor,
  annotationWash,
  MarkdownAnnotator,
} from "./MarkdownArtifact";

const html = (node: Parameters<typeof renderToStaticMarkup>[0]) =>
  renderToStaticMarkup(node);

const noop = async () => undefined;

describe("an opened markdown note", () => {
  it("renders the note as prose and invites a selection", () => {
    const markup = html(
      <MarkdownAnnotator text={"# Ship it\n\n- one\n- two\n"} onSave={noop} />,
    );
    expect(markup).toContain("<h1");
    expect(markup).toContain("Ship it");
    expect(markup).not.toContain("# Ship it");
    expect(markup).toContain("Tap a passage to comment on it.");
    expect(markup).toContain("Save");
  });

  it("shows the comments the note already carries, outside its prose", () => {
    const markup = html(
      <MarkdownAnnotator
        text={
          "# Ship it\n\n## Comments\n\n- **“Ship it”** — When exactly?\n" as string
        }
        onSave={noop}
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
        />,
      );
    });

    const paragraph = host.querySelector("p");
    expect(paragraph?.textContent).toBe("We cut on Friday.");
    act(() => {
      paragraph?.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    });

    expect(host.textContent).toContain("Comment on “We cut on Friday.”");
    expect(host.querySelector('textarea[aria-label="Comment"]')).not.toBeNull();

    // The native callout is what broke it, so the prose does not offer one on a
    // touch screen; a mouse keeps its drag-selection.
    const prose = paragraph?.closest("div[class*=overflow-y-auto]");
    expect(prose?.className).toContain("select-none");
    expect(prose?.className).toContain("mouse:select-text");
    expect(prose?.className).toContain("[-webkit-touch-callout:none]");

    act(() => root.unmount());
    host.remove();
  });

  it("keeps Save reachable: the note scrolls, the controls are pinned", () => {
    const markup = html(
      <MarkdownAnnotator
        text={"# Ship it\n\nWe cut on Friday.\n"}
        onSave={noop}
      />,
    );
    // The column itself does not scroll — only the prose inside it does.
    expect(markup).toContain("overflow-hidden");
    expect(markup).toContain("flex-1 touch-manipulation overflow-y-auto");
    expect(markup).toContain("env(safe-area-inset-bottom)");
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
    // circle, no background, nothing that reads as a control.
    const cardOrdinals = Array.from(
      host.querySelectorAll<HTMLElement>(
        'ul[aria-label="Comments"] > li > span',
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
  // moment the gateway ships a dark one: every hue is a shared theme token.
  it("is spelled in theme variables, never in hard-coded hex", () => {
    for (const colour of ANNOTATION_COLORS) {
      expect(colour).toMatch(/^var\(--[a-z0-9-]+\)$/);
    }
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
        plain
      />,
    );
    // Plain text has no markdown to render: the hash is ink, not a heading.
    expect(markup).not.toContain("<h1");
    expect(markup).toContain("# not a heading");
    expect(markup).toContain("second line");
    // Each line is a <p>, so a tap quotes it exactly as a paragraph is quoted.
    expect(markup.match(/<p /g)?.length ?? 0).toBeGreaterThanOrEqual(2);
    expect(markup).toContain("Tap a passage to comment on it.");
  });
});
