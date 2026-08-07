// @vitest-environment jsdom
import { act } from "react";
import { createRoot } from "react-dom/client";
import { renderToStaticMarkup } from "react-dom/server";
import { describe, expect, it } from "vitest";

import { MarkdownAnnotator } from "./MarkdownArtifact";

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
