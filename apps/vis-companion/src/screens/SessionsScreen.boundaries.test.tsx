import { describe, expect, it } from "vitest";

import sessionListSource from "../components/SessionList.tsx?raw";
import sessionsScreenSource from "./SessionsScreen.tsx?raw";
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
      signatures.every((signature) => /^\s*rowActions,?$/m.test(signature)),
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
});
