// @vitest-environment jsdom
import { render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { afterEach, describe, expect, it, vi } from "vitest";

import { DialogFrame, DialogHeader } from "./ui";

afterEach(() => {
  document.body.innerHTML = "";
});

// Regression, user report ("the portal close vs the dialog close and dialog headers are
// different. WE SHOULD NORMALIZE").
//
// Seven surfaces opened over another surface and no two of their headers agreed: two
// heights (`min-h-9` and `min-h-12`), two alignments (a centred title in `DialogFrame`
// and the artifact overlay against a left title with a subtitle in machine settings,
// application settings, the model picker and the paste editor), two paddings — and FOUR
// of the closes were hand-built at the call site, in two different boxes, none of them
// the `DialogClose` this app says is its only way out.
describe("every dialog header is the one dialog header", () => {
  const band = () => document.querySelector("header")!;

  it("is ONE band that spells its own height and paper", () => {
    render(
      <DialogFrame title="Machine settings" onClose={() => {}}>
        <p>body</p>
      </DialogFrame>,
    );

    expect(document.querySelectorAll("header")).toHaveLength(1);
    // The same height the list headers on the same screen stand at, tightened only for
    // a pointer.
    expect(band().className).toContain("min-h-12");
    expect(band().className).toContain("mouse:min-h-9");
    expect(band().className).toContain("bg-dialog-title");
  });

  it("aligns the title left and lets it hold a subtitle", () => {
    render(
      <DialogHeader title="Model" subtitle="gateway.example.com" closeLabel="Close Model" onClose={() => {}} />,
    );

    // Centring cost `px-12` of dead space on both sides to clear a close welded to one
    // of them, and could not hold the gateway / model / paste line four surfaces need.
    expect(band().className).not.toContain("justify-center");
    expect(band().className).not.toContain("px-12");
    expect(screen.getByText("gateway.example.com")).toBeTruthy();
    const title = screen.getByText("Model");
    expect(title.getBoundingClientRect().left).toBeLessThanOrEqual(
      screen.getByText("gateway.example.com").getBoundingClientRect().left,
    );
  });

  // A `vis.ask` question IS the title, and one clipped to a single line is no longer a
  // question anyone can answer. `HumanInputPrompt.test.tsx` pins the depth.
  it("wraps a question instead of eating it", () => {
    render(<DialogHeader title="Which of these three branches should the run start from?" />);
    const title = screen.getByText(/Which of these three branches/);
    expect(title.className).toContain("line-clamp-3");
    expect(title.className).not.toContain("truncate");
  });

  it("routes every close through the one way out, and makes it say its name", async () => {
    const onClose = vi.fn();
    const view = render(
      <DialogFrame title="report.pdf" onClose={onClose}>
        <p>body</p>
      </DialogFrame>,
    );

    // The way out is icon-only, so it says WHAT it closes: three of these bands can
    // stand over one another and a plain "Close" names all three the same.
    const close = screen.getByRole("button", { name: "Close report.pdf" });
    expect(band().contains(close)).toBe(true);
    await userEvent.click(close);
    expect(onClose).toHaveBeenCalledTimes(1);

    // A dialog may say what LEAVING does instead of naming itself...
    view.rerender(
      <DialogFrame title="Question" closeLabel="Cancel this request" onClose={onClose}>
        <p>body</p>
      </DialogFrame>,
    );
    expect(screen.getByRole("button", { name: "Cancel this request" })).toBeTruthy();

    // ...and a band with no way out builds no button at all.
    view.rerender(
      <DialogFrame title="Question">
        <p>body</p>
      </DialogFrame>,
    );
    expect(band().querySelectorAll("button")).toHaveLength(0);
  });
});
