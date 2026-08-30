import { describe, expect, it } from "vitest";

import sessionListSource from "../components/SessionList.tsx?raw";
import sessionsScreenSource from "./SessionsScreen.tsx?raw";
import fleetRailSource from "./sessions/FleetRail.tsx?raw";
import sessionProjectGroupsSource from "./sessions/SessionProjectGroups.tsx?raw";

describe("sessions feature boundaries", () => {
  it("leaves session presentation outside the fleet orchestrator", () => {
    const ownedRenderers = [
      ...sessionsScreenSource.matchAll(
        /^(?:const\s+)?(SessionRow|SessionStats|NavigatorSkeleton|NeedsYou|ProjectGroup)\s*=|^function\s+(SessionStats|NavigatorSkeleton|NeedsYou|ProjectGroup)\s*\(/gm,
      ),
    ].map((match) => match[1] ?? match[2]);

    expect(ownedRenderers).toEqual([]);
  });

  it("keeps desktop fleet navigation outside the fleet orchestrator", () => {
    const localRail = sessionsScreenSource.match(
      /^function\s+(FleetRail|RailGroup)\s*\(/m,
    );

    expect(localRail?.[1] ?? null).toBeNull();
    expect(sessionsScreenSource).toContain("<FleetRail");
    expect(fleetRailSource).toContain("<ListRow");
    expect(fleetRailSource).toContain('density="compact"');
    expect(fleetRailSource).not.toContain("<button");
  });

  it("passes row interaction through one reusable feature contract", () => {
    const signatures = ["NeedsYou", "ProjectGroup"].map((name) => {
      const match = sessionProjectGroupsSource.match(
        new RegExp(
          `${name} = memo\\(function ${name}\\(\\{([\\s\\S]*?)\\}: \{`,
        ),
      );
      expect(match, `${name} signature`).not.toBeNull();
      return match?.[1] ?? "";
    });
    const looseActions = [
      "onOpen",
      "onRename",
      "onFork",
      "onDelete",
      "onToggleStar",
      "rowAction",
      "deleteBusy",
      "deleteError",
      "onConfirmDelete",
      "onCancelDelete",
    ].filter((name) =>
      signatures.some((signature) =>
        new RegExp(`^\\s*${name},?$`, "m").test(signature),
      ),
    );

    expect(looseActions).toEqual([]);
    expect(
      signatures.every((signature) => /^\s*context,?$/m.test(signature)),
    ).toBe(true);
  });

  it("gives each session row commands and deletion state instead of loose callbacks", () => {
    const match = sessionListSource.match(
      /SessionRow = memo\(function SessionRow\(\{([\s\S]*?)\}: \{/,
    );
    expect(match, "SessionRow signature").not.toBeNull();
    const signature = match?.[1] ?? "";
    const looseActions = [
      "onOpen",
      "onRename",
      "onFork",
      "onDelete",
      "onToggleStar",
      "isConfirmingDelete",
      "deleteBusy",
      "deleteError",
      "onConfirmDelete",
      "onCancelDelete",
    ].filter((name) => new RegExp(`^\s*${name},?$`, "m").test(signature));

    expect(looseActions).toEqual([]);
    expect(signature).toMatch(/^\s*commands,?$/m);
    expect(signature).toMatch(/^\s*deletion,?$/m);
  });

  it("passes project groups through fleet domain contracts", () => {
    const match = sessionProjectGroupsSource.match(
      /ProjectGroup = memo\(function ProjectGroup\(\{([\s\S]*?)\}: \{/,
    );
    expect(match, "ProjectGroup signature").not.toBeNull();
    const signature = match?.[1] ?? "";
    const leakedFields = [
      "project",
      "root",
      "sessions",
      "tally",
      "conn",
      "getClient",
      "matches",
      "needle",
      "drafts",
      "rowActions",
      "pageSize",
      "epoch",
      "admitted",
      "isVisible",
      "list",
    ].filter((name) => new RegExp(`^\s*${name},?$`, "m").test(signature));

    expect(leakedFields).toEqual([]);
    expect(signature).toMatch(/^\s*group,?$/m);
    expect(signature).toMatch(/^\s*machine,?$/m);
    expect(signature).toMatch(/^\s*context,?$/m);
    expect(signature).toMatch(/^\s*reading,?$/m);
  });

  it("lets the rename dialog own its draft and request state", () => {
    expect(sessionsScreenSource).toContain("<RenameSessionDialog");
    expect(sessionsScreenSource).not.toContain("const [renameDraft");
    expect(sessionsScreenSource).not.toContain('title="Rename session"');
    expect(sessionsScreenSource).not.toContain('placeholder="Session name"');
  });
});
