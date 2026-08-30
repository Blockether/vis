// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";

import type { FileSuggestion, SlashCommand } from "../lib/types";
import {
  ComposerSuggestions,
  composerSuggestionListId,
} from "./ComposerSuggestions";

const files: FileSuggestion[] = [
  { name: "src/App.tsx", size: "24 KB", age: "2m", status: "modified" },
  { name: "README.md", size: "8 KB", age: "1d", status: "clean" },
];
const commands: SlashCommand[] = [
  { name: "/help", doc: "Show the available slash commands." },
  { name: "/rename", doc: "Rename this session." },
];

afterEach(cleanup);

describe("composer suggestions", () => {
  it("renders and selects file mentions through the canonical list", () => {
    const onSelect = vi.fn();
    render(
      <ComposerSuggestions
        kind="files"
        items={files}
        selectedIndex={1}
        onSelect={onSelect}
      />,
    );

    expect(
      screen.getByRole("listbox", { name: "File mentions" }),
    ).toHaveAttribute("id", composerSuggestionListId("files"));
    expect(screen.getByRole("option", { name: /README.md/ })).toHaveAttribute(
      "aria-selected",
      "true",
    );
    fireEvent.click(screen.getByRole("option", { name: /src\/App.tsx/ }));
    expect(onSelect).toHaveBeenCalledWith(files[0]);
  });

  it("renders slash commands with the same list semantics", () => {
    const onSelect = vi.fn();
    render(
      <ComposerSuggestions
        kind="slashes"
        items={commands}
        selectedIndex={0}
        onSelect={onSelect}
      />,
    );

    expect(
      screen.getByRole("listbox", { name: "Slash commands" }),
    ).toHaveAttribute("id", composerSuggestionListId("slashes"));
    fireEvent.click(screen.getByRole("option", { name: /rename/i }));
    expect(onSelect).toHaveBeenCalledWith(commands[1]);
  });

  it("does not mount an empty completion surface", () => {
    const { container } = render(
      <ComposerSuggestions
        kind="files"
        items={[]}
        selectedIndex={0}
        onSelect={vi.fn()}
      />,
    );
    expect(container).toBeEmptyDOMElement();
  });
});
