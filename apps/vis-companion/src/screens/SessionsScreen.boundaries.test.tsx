import { describe, expect, it } from "vitest";

import sessionsScreenSource from "./SessionsScreen.tsx?raw";

describe("sessions feature boundaries", () => {
  it("leaves session-row rendering outside the fleet orchestrator", () => {
    const ownedRenderers = [
      ...sessionsScreenSource.matchAll(
        /^(?:const\s+)?(SessionRow|SessionStats|NavigatorSkeleton)\s*=|^function\s+(SessionStats|NavigatorSkeleton)\s*\(/gm,
      ),
    ].map((match) => match[1] ?? match[2]);

    expect(ownedRenderers).toEqual([]);
  });
});
