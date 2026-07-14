/* eslint-disable @typescript-eslint/no-explicit-any */
jest.mock("expo-notifications", () => ({
  setNotificationHandler: jest.fn(),
  getPermissionsAsync: jest.fn(),
  requestPermissionsAsync: jest.fn(),
  scheduleNotificationAsync: jest.fn(),
}));

import * as Notifications from "expo-notifications";

import {
  ensureNotificationPermissions,
  formatTurnDoneNotice,
  notifyTurnDone,
  shouldNotifyTurnDone,
} from "./Notifications";

const mockN = Notifications as jest.Mocked<typeof Notifications>;

describe("shouldNotifyTurnDone", () => {
  it("fires for a terminal event when enabled and backgrounded", () => {
    expect(
      shouldNotifyTurnDone({
        type: "turn.completed",
        enabled: true,
        appActive: false,
      }),
    ).toBe(true);
    expect(
      shouldNotifyTurnDone({
        type: "turn.failed",
        enabled: true,
        appActive: false,
      }),
    ).toBe(true);
  });

  it("does NOT fire while the app is foreground-active (user is watching)", () => {
    expect(
      shouldNotifyTurnDone({
        type: "turn.completed",
        enabled: true,
        appActive: true,
      }),
    ).toBe(false);
  });

  it("does NOT fire when notifications are disabled", () => {
    expect(
      shouldNotifyTurnDone({
        type: "turn.completed",
        enabled: false,
        appActive: false,
      }),
    ).toBe(false);
  });

  it("does NOT fire for non-terminal events", () => {
    for (const type of [
      "turn.started",
      "block.started",
      "block.output",
      "context.updated",
    ]) {
      expect(
        shouldNotifyTurnDone({ type, enabled: true, appActive: false }),
      ).toBe(false);
    }
  });

  it("does NOT fire for an undefined event type", () => {
    expect(
      shouldNotifyTurnDone({
        type: undefined,
        enabled: true,
        appActive: false,
      }),
    ).toBe(false);
  });
});

describe("formatTurnDoneNotice", () => {
  it("titles a completed turn with the session name and carries the request as the body", () => {
    const { title, body } = formatTurnDoneNotice({
      failed: false,
      sessionTitle: "vis-core",
      request: "add streaming",
    });
    expect(title).toBe("vis-core — turn finished");
    expect(body).toBe("add streaming");
  });

  it("titles a failed turn", () => {
    expect(
      formatTurnDoneNotice({ failed: true, sessionTitle: "core" }).title,
    ).toBe("core — turn failed");
  });

  it("falls back to 'vis' when there is no session title", () => {
    expect(
      formatTurnDoneNotice({ failed: false, sessionTitle: "  " }).title,
    ).toBe("vis — turn finished");
    expect(formatTurnDoneNotice({ failed: false }).title).toBe(
      "vis — turn finished",
    );
  });

  it("uses a default body when the turn has no request text", () => {
    expect(formatTurnDoneNotice({ failed: false }).body).toBe(
      "Your turn is ready.",
    );
    expect(formatTurnDoneNotice({ failed: true }).body).toBe(
      "The turn ended with an error.",
    );
  });

  it("truncates a long request body to 117 chars + ellipsis", () => {
    const long = "x".repeat(300);
    const { body } = formatTurnDoneNotice({ failed: false, request: long });
    expect(body.length).toBe(118); // 117 + ellipsis char
    expect(body.endsWith("\u2026")).toBe(true);
  });
});

describe("permissions + notifyTurnDone (native)", () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it("notifyTurnDone no-ops before permission is granted", async () => {
    await notifyTurnDone({ failed: false, sessionTitle: "x" });
    expect(mockN.scheduleNotificationAsync).not.toHaveBeenCalled();
  });

  it("returns true when permission is already granted", async () => {
    mockN.getPermissionsAsync.mockResolvedValue({ granted: true } as any);
    expect(await ensureNotificationPermissions()).toBe(true);
    expect(mockN.requestPermissionsAsync).not.toHaveBeenCalled();
  });

  it("requests permission when undetermined and can ask again", async () => {
    mockN.getPermissionsAsync.mockResolvedValue({
      granted: false,
      canAskAgain: true,
    } as any);
    mockN.requestPermissionsAsync.mockResolvedValue({ granted: true } as any);
    expect(await ensureNotificationPermissions()).toBe(true);
    expect(mockN.requestPermissionsAsync).toHaveBeenCalledTimes(1);
  });

  it("returns false without asking when it can't ask again", async () => {
    mockN.getPermissionsAsync.mockResolvedValue({
      granted: false,
      canAskAgain: false,
    } as any);
    expect(await ensureNotificationPermissions()).toBe(false);
    expect(mockN.requestPermissionsAsync).not.toHaveBeenCalled();
  });

  it("returns false when the native permission call throws", async () => {
    mockN.getPermissionsAsync.mockRejectedValue(new Error("no native module"));
    expect(await ensureNotificationPermissions()).toBe(false);
  });

  it("schedules an immediate notification once permission is granted", async () => {
    mockN.getPermissionsAsync.mockResolvedValue({ granted: true } as any);
    await ensureNotificationPermissions();
    mockN.scheduleNotificationAsync.mockResolvedValue("id" as any);
    await notifyTurnDone({
      failed: true,
      sessionTitle: "core",
      request: "ship it",
    });
    expect(mockN.scheduleNotificationAsync).toHaveBeenCalledTimes(1);
    const arg = mockN.scheduleNotificationAsync.mock.calls[0]![0] as any;
    expect(arg.content.title).toBe("core — turn failed");
    expect(arg.content.body).toBe("ship it");
    expect(arg.trigger).toBeNull();
  });

  it("swallows a scheduling error", async () => {
    mockN.getPermissionsAsync.mockResolvedValue({ granted: true } as any);
    await ensureNotificationPermissions();
    mockN.scheduleNotificationAsync.mockRejectedValue(new Error("boom"));
    await expect(notifyTurnDone({ failed: false })).resolves.toBeUndefined();
  });
});
