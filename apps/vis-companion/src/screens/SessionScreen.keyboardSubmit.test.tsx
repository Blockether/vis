// @vitest-environment jsdom
import { fireEvent, screen } from "@testing-library/react";
import { describe, expect, it, vi } from "vitest";

import { renderSessionScreen } from "./session-screen-harness";

const keyboardHide = vi.hoisted(() => vi.fn(() => Promise.resolve()));

vi.mock("@capacitor/core", async (importOriginal) => {
  const original = await importOriginal<typeof import("@capacitor/core")>();
  return {
    ...original,
    Capacitor: new Proxy(original.Capacitor, {
      get(target, property, receiver) {
        if (property === "getPlatform") return () => "ios";
        if (property === "isNativePlatform") return () => true;
        return Reflect.get(target, property, receiver);
      },
    }),
  };
});

vi.mock("@capacitor/keyboard", async (importOriginal) => {
  const original = await importOriginal<typeof import("@capacitor/keyboard")>();
  return {
    ...original,
    Keyboard: new Proxy(original.Keyboard, {
      get(target, property, receiver) {
        if (property === "hide") return keyboardHide;
        return Reflect.get(target, property, receiver);
      },
    }),
  };
});

describe("sending from the native composer", () => {
  it("keeps the keyboard and focused composer stable", async () => {
    renderSessionScreen();
    const composer = await screen.findByLabelText("Message Vis");
    const send = screen.getByRole("button", { name: "Send message" });
    fireEvent.change(composer, { target: { value: "Keep writing" } });
    composer.focus();

    fireEvent.mouseDown(send);
    fireEvent.click(send);

    expect(document.activeElement).toBe(composer);
    expect(keyboardHide).not.toHaveBeenCalled();
  });
});
