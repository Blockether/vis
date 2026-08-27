// @vitest-environment jsdom
import { act, useRef } from "react";
import { render, screen } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { useVisualViewportShell } from "./viewport";

const native = vi.hoisted(() => ({
  keyboard: new Map<string, (info: { keyboardHeight: number }) => void>(),
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
    addListener: () => Promise.resolve({ remove: () => undefined }),
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

/** The only signal a web view has for "a keyboard is attached": what points at it. */
function pointing(kind: "fine" | "coarse") {
  vi.stubGlobal("matchMedia", (query: string) => ({
    matches: query.includes("pointer: fine") ? kind === "fine" : false,
    media: query,
    addEventListener: () => undefined,
    removeEventListener: () => undefined,
  }));
}

function window_(width: number, height: number) {
  window.innerWidth = width;
  window.innerHeight = height;
  Object.defineProperty(window, "visualViewport", {
    configurable: true,
    value: {
      width,
      height,
      offsetTop: 0,
      addEventListener: () => undefined,
      removeEventListener: () => undefined,
    },
  });
}

beforeEach(() => {
  vi.useFakeTimers();
  native.keyboard.clear();
  vi.stubGlobal("requestAnimationFrame", () => 1);
  vi.stubGlobal("cancelAnimationFrame", () => undefined);
});

afterEach(() => {
  vi.useRealTimers();
  vi.unstubAllGlobals();
});

describe("focusing the composer with a keyboard already attached", () => {
  // Regression, user report (paraphrased: on a desktop the app should neither raise
  // nor simulate a keyboard): a focus reserved the predicted software-keyboard band —
  // 41% of the window — for keys that never come up, so the composer floated above a
  // dead strip of background until the native accessory-bar event corrected it.
  it("predicts no software keyboard for a hardware pointer", () => {
    pointing("fine");
    window_(1180, 820);
    render(<ViewportProbe />);
    const shell = screen.getByTestId("shell");
    const composer = screen.getByRole("textbox", { name: "Message" });

    act(() => composer.focus());

    expect(shell.style.height).toBe("");
  });

  it("still pins to the accessory bar the hardware keyboard does raise", () => {
    pointing("fine");
    window_(1180, 820);
    render(<ViewportProbe />);
    const shell = screen.getByTestId("shell");
    const composer = screen.getByRole("textbox", { name: "Message" });

    act(() => composer.focus());
    act(() => native.keyboard.get("keyboardWillShow")?.({ keyboardHeight: 69 }));

    expect(shell).toHaveStyle({ height: "751px" });
  });

  // Regression, user report (paraphrased: writing on the desktop still opened a dead
  // grey band under the composer): iPadOS hands over the keyboard's whole frame even
  // when a hardware keyboard parks all but its shortcut bar below the screen, so the
  // window reserved a third of itself for keys nobody could see and left the composer
  // floating above the background, with the system's bar alone at the bottom edge.
  it("reserves nothing for a keyboard frame parked off the screen", () => {
    pointing("fine");
    window_(1194, 834);
    render(<ViewportProbe />);
    const shell = screen.getByTestId("shell");
    const composer = screen.getByRole("textbox", { name: "Message" });

    act(() => composer.focus());
    act(() => native.keyboard.get("keyboardWillShow")?.({ keyboardHeight: 353 }));

    expect(shell.style.height).toBe("");
  });
  it("still places the composer a frame early on a touch screen", () => {
    pointing("coarse");
    window_(390, 844);
    render(<ViewportProbe />);
    const shell = screen.getByTestId("shell");
    const composer = screen.getByRole("textbox", { name: "Message" });

    act(() => composer.focus());

    expect(shell).toHaveStyle({ height: "549px" });
  });
});
