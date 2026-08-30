import { describe, expect, it } from "vitest";

import settingsScreenSource from "./SettingsScreen.tsx?raw";

describe("settings feature boundaries", () => {
  it("leaves the screen to orchestrate feature panels rather than define them", () => {
    const panels = [
      ...settingsScreenSource.matchAll(
        /^(?:export\s+)?function\s+([A-Z][A-Za-z0-9]*Panel)\s*\(/gm,
      ),
    ].map(([, name]) => name);

    expect(panels).toEqual([]);
  });
});
