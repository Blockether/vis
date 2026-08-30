import { describe, expect, it } from "vitest";

import sessionsScreenSource from "./SessionsScreen.tsx?raw";

describe("sessions feature boundaries", () => {
  it("leaves session presentation outside the fleet orchestrator", () => {
    const ownedRenderers = [
      ...sessionsScreenSource.matchAll(
        /^(?:const\s+)?(SessionRow|SessionStats|NavigatorSkeleton|NeedsYou|ProjectGroup)\s*=|^function\s+(SessionStats|NavigatorSkeleton|NeedsYou|ProjectGroup)\s*\(/gm,
      ),
    ].map((match) => match[1] ?? match[2]);

    expect(ownedRenderers).toEqual([]);
  });
});
