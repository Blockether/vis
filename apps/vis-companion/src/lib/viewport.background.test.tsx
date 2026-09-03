// @vitest-environment jsdom
import { act, useRef } from "react";
import { render, screen } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { useVisualViewportShell } from "./viewport";

const native = vi.hoisted(() => ({
  keyboard: new Map<string, (info: { keyboardHeight: number }) => void>(),
  app: new Map<string, (state?: { isActive: boolean }) => void>(),
}));

vi.mock("@capacitor/core", () => ({
  Capacitor: { getPlatform: () => "ios" },
}));

vi.mock("@capacitor/keyboard", () => ({
  Keyboard: {
    addListener: (event: string, listener: (info: { keyboardHeight: number }) => void) => {
      native.keyboard.set(event, listener);
      return Promise.resolve({ remove: () => void native.keyboard.delete(event) });
    },
    setScroll: () => Promise.resolve(),
  },
}));

vi.mock("@capacitor/app", () => ({
  App: {
    addListener: (event: string, listener: (state?: { isActive: boolean }) => void) => {
      native.app.set(event, listener);
      return Promise.resolve({ remove: () => void native.app.delete(event) });
    },
  },
}));

function ViewportProbe() {
  const shell = useRef<HTMLDivElement>(null);
  useVisualViewportShell(shell);
  return (
    <div ref={shell} data-testid="shell">
      <textarea aria-label="Message" />
    </div>
  );
}

beforeEach(() => {
  vi.useFakeTimers();
  native.keyboard.clear();
  native.app.clear();
  window.innerWidth = 390;
  window.innerHeight = 844;
  Object.defineProperty(window, "visualViewport", {
    configurable: true,
    value: {
      width: 390,
      height: 844,
      offsetTop: 0,
      addEventListener: () => undefined,
      removeEventListener: () => undefined,
    },
  });
  vi.stubGlobal("requestAnimationFrame", () => 1);
  vi.stubGlobal("cancelAnimationFrame", () => undefined);
});

afterEach(() => {
  vi.useRealTimers();
  vi.unstubAllGlobals();
});

describe("the native shell after backgrounding", () => {
  // Regression, Vis session 78b0c0b5-f5ba-453f-97ee-af0a85f72d25: iOS ended
  // keyboard editing while the WebView was suspended, but stale DOM focus and the
  // old keyboard-height pin returned as an empty band where the keyboard had been.
  it("reopens the keyboard instead of preserving its empty band", () => {
    render(<ViewportProbe />);
    const shell = screen.getByTestId("shell");
    const composer = screen.getByRole("textbox", { name: "Message" });
    composer.focus();

    act(() => native.keyboard.get("keyboardWillShow")?.({ keyboardHeight: 300 }));
    expect(shell).toHaveStyle({ height: "544px" });

    act(() => native.app.get("resume")?.());

    expect(shell.style.height).toBe("");
    expect(document.activeElement).not.toBe(composer);

    act(() => vi.advanceTimersByTime(200));
    expect(document.activeElement).toBe(composer);
  });

  // Regression, TestFlight build 5275: leaving the foreground blurred the composer
  // from JS. WebKit reported that programmatic focus clear to the UI process during
  // the background scene update, UIKit turned it into a keyboard task no thread was
  // left to run, and the watchdog killed Vis with 0x8BADF00D after ten seconds.
  it("never touches DOM focus while the app leaves the foreground", () => {
    render(<ViewportProbe />);
    const composer = screen.getByRole("textbox", { name: "Message" });
    const blur = vi.spyOn(composer, "blur");
    composer.focus();

    act(() => native.keyboard.get("keyboardWillShow")?.({ keyboardHeight: 300 }));
    expect(document.activeElement).toBe(composer);

    act(() => native.app.get("appStateChange")?.({ isActive: false }));
    expect(blur).not.toHaveBeenCalled();
    expect(document.activeElement).toBe(composer);

    // Back on screen the keyboard machinery answers again, and only a real focus
    // change raises the keyboard the suspension took away.
    act(() => native.app.get("appStateChange")?.({ isActive: true }));
    act(() => vi.advanceTimersByTime(200));
    expect(blur).toHaveBeenCalled();
    expect(document.activeElement).toBe(composer);
  });

  // Regression, TestFlight build 5275: the restore is deferred past the resume, so
  // an app that goes away again inside that window would have raised the keyboard
  // from the background — the same watchdog kill by a later route.
  it("leaves focus alone when the app goes away again before the restore", () => {
    render(<ViewportProbe />);
    const composer = screen.getByRole("textbox", { name: "Message" });
    composer.focus();

    act(() => native.keyboard.get("keyboardWillShow")?.({ keyboardHeight: 300 }));
    act(() => native.app.get("appStateChange")?.({ isActive: false }));
    act(() => native.app.get("appStateChange")?.({ isActive: true }));
    act(() => native.app.get("appStateChange")?.({ isActive: false }));

    const focus = vi.spyOn(composer, "focus");
    act(() => vi.advanceTimersByTime(200));
    expect(focus).not.toHaveBeenCalled();
  });
});
