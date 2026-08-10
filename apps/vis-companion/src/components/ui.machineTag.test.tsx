// @vitest-environment jsdom
import { render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { describe, expect, it, vi } from "vitest";

import { MACHINE_COLORS } from "../lib/machine-colors";
import { EditableName, machineTagFace } from "./ui";

/** The face the header hands the tag, as a class list a matcher can take. */
const FACE = machineTagFace(MACHINE_COLORS[0]!).split(/\s+/).filter(Boolean);

// THE MACHINE'S NAME IS A TAG IN ITS OWN HUE, and the same tag is the control
// that renames it. These are rendered and driven, not read out of the source:
// the two bugs this screen actually shipped — a banner that never rendered at
// all, and a field that lost its paper the moment a caret arrived — were both
// invisible to a test that only matched class strings in a file.

/** The tag as the header wears it: a name that becomes a field in place. */
function MachineTag({ value = "tower", onCommit = () => {} }: { value?: string; onCommit?: (name: string) => void }) {
  return (
    <EditableName
      face={machineTagFace(MACHINE_COLORS[0]!)}
      label={`Rename ${value}`}
      value={value}
      onCommit={onCommit}
    />
  );
}

describe("the machine tag", () => {
  it("names the machine on a block of its own hue", () => {
    render(<MachineTag />);

    const tag = screen.getByRole("button", { name: "Rename tower" });
    expect(tag).toHaveTextContent("tower");
    expect(tag).toHaveClass(...FACE);
  });

  it("gives two machines two different tags", () => {
    expect(machineTagFace(MACHINE_COLORS[0]!)).not.toBe(machineTagFace(MACHINE_COLORS[1]!));
  });

  it("hugs the name instead of growing into a bar", () => {
    // A tag as wide as its column is the full-bleed banner again, which is the
    // exact thing the 2px spine down the list replaced.
    const face = machineTagFace(MACHINE_COLORS[0]!);
    expect(face).toContain("w-fit");
    expect(face).toContain("truncate");
    expect(face).toContain("max-w-full");
  });

  it("still dresses a machine that has no hue of its own", () => {
    // An unpainted name is the plain white ink this tag exists to replace.
    expect(machineTagFace().trim()).not.toBe("");
    expect(machineTagFace()).not.toBe(machineTagFace(MACHINE_COLORS[0]!));
  });

  // Regression, user report ("when I'm clicking the tag to change it, it's
  // blank"): the field the tag becomes must carry the name it is editing, and
  // must keep the tag's paper and padding — `EditableName` used to spell
  // `bg-transparent p-0` itself, so the hue vanished under the caret.
  it("opens a field that already holds the name, on the same paper", async () => {
    const user = userEvent.setup();
    render(<MachineTag />);

    await user.click(screen.getByRole("button", { name: "Rename tower" }));

    const field = screen.getByRole("textbox", { name: "Rename tower" });
    expect(field).toHaveValue("tower");
    expect(field).toHaveFocus();
    expect(field).toHaveClass(...FACE);
    expect(screen.queryByRole("button", { name: "Rename tower" })).not.toBeInTheDocument();
  });

  it("commits the typed name on Enter and puts the tag back", async () => {
    const user = userEvent.setup();
    const onCommit = vi.fn();
    render(<MachineTag onCommit={onCommit} />);

    await user.click(screen.getByRole("button", { name: "Rename tower" }));
    await user.clear(screen.getByRole("textbox", { name: "Rename tower" }));
    await user.type(screen.getByRole("textbox", { name: "Rename tower" }), "workshop{Enter}");

    expect(onCommit).toHaveBeenCalledWith("workshop");
    expect(screen.getByRole("button", { name: "Rename tower" })).toBeInTheDocument();
  });

  it("restores the name on Escape without renaming anything", async () => {
    const user = userEvent.setup();
    const onCommit = vi.fn();
    render(<MachineTag onCommit={onCommit} />);

    await user.click(screen.getByRole("button", { name: "Rename tower" }));
    await user.type(screen.getByRole("textbox", { name: "Rename tower" }), "-2{Escape}");

    expect(onCommit).not.toHaveBeenCalled();
    expect(screen.getByRole("button", { name: "Rename tower" })).toHaveTextContent("tower");
  });

  // A phone keyboard is dismissed far more often than Enter is pressed.
  it("commits when the field is left", async () => {
    const user = userEvent.setup();
    const onCommit = vi.fn();
    render(
      <>
        <MachineTag onCommit={onCommit} />
        <button type="button">elsewhere</button>
      </>,
    );

    await user.click(screen.getByRole("button", { name: "Rename tower" }));
    await user.type(screen.getByRole("textbox", { name: "Rename tower" }), "-2");
    await user.click(screen.getByRole("button", { name: "elsewhere" }));

    expect(onCommit).toHaveBeenCalledWith("tower-2");
  });
});
