import { describe, expect, it } from "vitest";

import ui from "./ui.tsx?raw";
import artifacts from "./ArtifactsSheet.tsx?raw";
import connect from "../screens/ConnectScreen.tsx?raw";
import incompatible from "../screens/IncompatibleScreen.tsx?raw";
import router from "../screens/RouterScreen.tsx?raw";
import session from "../screens/SessionScreen.tsx?raw";
import viewer from "./ImageViewer.tsx?raw";
import settings from "../screens/SettingsScreen.tsx?raw";

// Regression, user report ("the portal close vs the dialog close and dialog headers
// are different. WE SHOULD NORMALIZE").
//
// Seven surfaces opened over another surface and no two of their headers agreed:
// two heights (`min-h-9` and `min-h-12`), two alignments (a centred title in
// `DialogFrame` and the artifact overlay against a left title with a subtitle in
// machine settings, application settings, the model picker and the paste editor),
// two paddings — and FOUR of the closes were hand-built at the call site, in two
// different boxes, none of them the `DialogClose` this app says is its only way out.
const SURFACES: [string, string][] = [
  ["ui", ui],
  ["ArtifactsSheet", artifacts],
  ["ConnectScreen", connect],
  ["IncompatibleScreen", incompatible],
  ["RouterScreen", router],
  ["SessionScreen", session],
  ["SettingsScreen", settings],
  ["ImageViewer", viewer],
];

describe("every dialog header is the one dialog header", () => {
  it("leaves no surface painting its own title bar", () => {
    // `bg-dialog-title` is the title bar's paper, and exactly one `<header>` in the
    // app lays it down: `DialogHeader`'s own. Other uses of the token (a back
    // button's block, a transcript's column band) are not headers and are left alone.
    const definition = ui.match(/<header[^>]*bg-dialog-title/g) ?? [];
    expect(
      definition,
      "ui.tsx should define the band exactly once",
    ).toHaveLength(1);

    for (const [name, source] of SURFACES) {
      if (source === ui) continue;
      const headers = source.match(/<header[^>]*bg-dialog-title/g) ?? [];
      expect(headers, `${name} still builds its own dialog header`).toEqual([]);
    }
  });

  it("leaves no surface building its own way out", () => {
    for (const [name, source] of SURFACES) {
      // The hand-built closes all wore this hairline against the title bar's ink.
      expect(source, `${name} hand-builds a close button`).not.toContain(
        "border-l border-dialog-title-foreground/20 text-dialog-title-foreground",
      );
    }
  });

  it("gives the band the same height as the list headers on the same screen", () => {
    expect(ui).toContain(
      "flex min-h-12 shrink-0 items-stretch bg-dialog-title",
    );
    expect(ui).toContain("mouse:min-h-9");
  });

  it("aligns the title left and lets it hold a subtitle", () => {
    // Centring cost `px-12` of dead space on both sides to clear a close welded to
    // one of them, and could not hold the gateway / model / paste line four of these
    // surfaces need.
    expect(ui).not.toContain("justify-center bg-dialog-title px-12");
    expect(ui).toContain("subtitle");
  });

  // A `vis.ask` question IS the title, and one clipped to a single line is no longer
  // a question anyone can answer. `HumanInputPrompt.test.tsx` pins the depth.
  it("wraps a question instead of eating it", () => {
    expect(ui).toContain(
      "line-clamp-3 font-mono text-body font-bold tracking-wide",
    );
  });

  it("routes every close through DialogClose", () => {
    expect(ui).toContain(
      "<DialogClose label={closeLabel ?? 'Close'} tone=\"title\"",
    );
  });
});
