// @vitest-environment jsdom
import { act, useRef } from "react";
import { render, screen } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { useVisualViewportShell } from "./viewport";

const native = vi.hoisted(() => ({
  keyboard: new Map<string, (info: { keyboardHeight: number }) => void>(),
  accessoryBar: [] as boolean[],
  isMac: false,
}));

vi.mock("./host", () => ({
  isIosAppOnMac: () => native.isMac,
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
    setAccessoryBarVisible: ({ isVisible }: { isVisible: boolean }) => {
      native.accessoryBar.push(isVisible);
      return Promise.resolve();
    },
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
      <div aria-label="Sessions">
        <input aria-label="Rename last session" />
      </div>
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
  native.isMac = false;
  vi.stubGlobal("requestAnimationFrame", () => 1);
  vi.stubGlobal("cancelAnimationFrame", () => undefined);
});

afterEach(() => {
  vi.useRealTimers();
  vi.unstubAllGlobals();
});

describe("focusing an editor with a keyboard already attached", () => {
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

  // Regression, user report: the hardware keyboard's tiny shortcut-bar event still
  // shortened the app shell, exposing a translucent keyboard-like strip under the
  // composer on a desktop-class window even though no software keyboard was present.
  it("reserves nothing for the hardware keyboard shortcut bar", () => {
    pointing("fine");
    window_(1180, 820);
    render(<ViewportProbe />);
    const shell = screen.getByTestId("shell");
    const composer = screen.getByRole("textbox", { name: "Message" });

    act(() => composer.focus());
    act(() => native.keyboard.get("keyboardWillShow")?.({ keyboardHeight: 69 }));

    expect(shell.style.height).toBe("");
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

  // Regression, user report (paraphrased: installed on a MacBook, tapping the input
  // still shows a grey field where the keyboard would be): a Mac window is iOS WebKit,
  // and under a trackpad it still answers `(pointer: coarse)` — so the two guards
  // above never fired there. The focus pinned the shell to two thirds of the window,
  // and UIKit's frame for the keyboard it never draws confirmed the pin for good.
  it("reserves nothing in a Mac window, whatever the pointer query says", () => {
    native.isMac = true;
    pointing("coarse");
    window_(1280, 800);
    render(<ViewportProbe />);
    const shell = screen.getByTestId("shell");
    const composer = screen.getByRole("textbox", { name: "Message" });

    act(() => composer.focus());
    expect(shell.style.height).toBe("");

    act(() => native.keyboard.get("keyboardWillShow")?.({ keyboardHeight: 336 }));
    expect(shell.style.height).toBe("");
  });
  // Regression, user report (paraphrased: on the Mac a gray element still appears
  // while I type): reserving nothing for the shortcut bar never removed the bar.
  // UIKit kept hanging its form accessory off the focused field — a ~36px neutral
  // panel, inset from the window edges, in none of the app's colours, laid over
  // the composer for as long as it held focus — because nothing ever asked for it
  // to go away.
  it("asks UIKit to take the form accessory bar away", () => {
    native.accessoryBar.length = 0;
    pointing("fine");
    window_(1180, 820);

    render(<ViewportProbe />);

    expect(native.accessoryBar).toEqual([false]);
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
  // Regression, Vis session 5b630f33-96ef-44de-bc05-9db6cbe71845: renaming the
  // last session pinned the native shell above the iPhone keyboard but left that row
  // at its old list position, entirely behind the keys.
  it("scrolls the last inline rename into the shortened native shell", () => {
    pointing("coarse");
    window_(390, 844);
    render(<ViewportProbe />);
    const shell = screen.getByTestId("shell");
    const rename = screen.getByRole("textbox", { name: "Rename last session" });
    const reveal = vi.fn();
    Object.defineProperty(rename, "scrollIntoView", { configurable: true, value: reveal });

    act(() => rename.focus());

    expect(shell).toHaveStyle({ height: "549px" });
    expect(reveal).toHaveBeenCalledWith({ block: "nearest" });
  });
});
