// @vitest-environment jsdom
import { screen } from "@testing-library/react";
import { describe, expect, it } from "vitest";

import { renderSessionScreen, sessionFixture } from "./session-screen-harness";

describe("scrolling a transcript", () => {
  // Regression, user report (desktop session screenshot): the transcript section
  // kept its intrinsic width inside the shell, exposing page overflow and a native
  // horizontal scrollbar beneath the composer.
  it("owns the full width of the viewport frame", () => {
    renderSessionScreen({ session: sessionFixture({ id: "desktop" }) });
    const viewport = screen.getByRole("region", { name: "Transcript" });
    const session = viewport.closest("section") as HTMLElement;
    expect(session.className).toContain("w-full");
  });
});
