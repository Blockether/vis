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
    expect(markup).toContain("Select a passage to comment on it.");
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
