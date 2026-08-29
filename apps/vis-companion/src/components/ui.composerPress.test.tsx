// @vitest-environment jsdom
import { describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen, waitFor } from "@testing-library/react";

import { ComposerButton, MetaButton, NewSessionButton } from "./ui";
import { renderSessionScreen } from "../screens/session-screen-harness";

/**
 * ONE FINGER on a control: down and up on the same spot, and NO `click` — what
 * iOS hands the page when its synthetic-click path drops the tap. jsdom carries
 * no `PointerEvent`, and a `MouseEvent` under that type name has the button and
 * the coordinates the handler reads.
 */
const tap = (element: Element, at = { x: 0, y: 0 }) => {
  const finger = (type: string) =>
    new MouseEvent(type, { bubbles: true, clientX: at.x, clientY: at.y });
  fireEvent(element, finger("pointerdown"));
  fireEvent(element, finger("pointerup"));
};

// Regression, reported from a phone: "in a new session sending the request
// breaks — I have to tap the top so the keyboard hides, and only then does
// pressing send work". The composer stood above the keyboard the whole time and
// nothing was ever POSTed: the send waited for a `click`, and iOS builds that
// click itself from the touch, which it may decide was a hover instead.
describe("the send button under a finger", () => {
  it("sends on a tap that never becomes a click", async () => {
    const submitTurn = vi.fn(() => Promise.resolve(null));
    renderSessionScreen({ client: { submitTurn } });

    const box = await screen.findByLabelText("Message Vis");
    fireEvent.change(box, { target: { value: "run the tests" } });
    tap(screen.getByRole("button", { name: "Send message" }));

    await waitFor(() => expect(submitTurn).toHaveBeenCalledTimes(1));
    expect(await screen.findByText("run the tests")).toBeInTheDocument();
  });

  // Regression, WebKit issue #164077: an iOS autocorrection could update the
  // textarea without notifying React before the send press.
  it("sends the text committed by iOS before React state catches up", async () => {
    const submitTurn = vi.fn(() => Promise.resolve(null));
    renderSessionScreen({ client: { submitTurn } });

    const box = (await screen.findByLabelText("Message Vis")) as HTMLTextAreaElement;
    fireEvent.change(box, { target: { value: "To będzie dlugie zdanie" } });
    box.value = "To będzie długie zdanie?";
    tap(screen.getByRole("button", { name: "Send message" }));

    await waitFor(() =>
      expect(submitTurn).toHaveBeenCalledWith(
        expect.any(String),
        "To będzie długie zdanie?",
        expect.any(Object),
      ),
    );
  });
});

describe("the composer's own press", () => {
  const button = (props: Partial<Parameters<typeof ComposerButton>[0]> = {}) => {
    render(
      <ComposerButton label="Send message" {...props}>
        {"\u2191"}
      </ComposerButton>,
    );
    return screen.getByRole("button", { name: "Send message" });
  };

  it("acts once when the click DOES arrive after the same tap", () => {
    const press = vi.fn();
    const send = button({ onClick: press });

    tap(send);
    fireEvent.click(send);

    expect(press).toHaveBeenCalledTimes(1);
  });

  it("acts once for a keyboard, which only ever sends a click", () => {
    const press = vi.fn();

    fireEvent.click(button({ onClick: press }));

    expect(press).toHaveBeenCalledTimes(1);
  });

  it("does nothing when the finger slid off before it lifted", () => {
    const press = vi.fn();

    tap(button({ onClick: press }), { x: 999, y: 999 });

    expect(press).not.toHaveBeenCalled();
  });

  it("stays silent while it is disabled", () => {
    const press = vi.fn();
    const send = button({ onClick: press, disabled: true });

    tap(send);
    fireEvent.click(send);

    expect(press).not.toHaveBeenCalled();
  });

  it("still hands the press-and-hold both halves of the gesture", () => {
    const held: string[] = [];
    const send = button({
      onPointerDown: () => held.push("down"),
      onPointerUp: () => held.push("up"),
      onClick: () => held.push("press"),
    });

    tap(send);

    expect(held).toEqual(["down", "up", "press"]);
  });
});

// Regression, WebKit issue #211179: a touch can end without the synthetic
// `click` WKWebView normally builds. New session and the response dials then
// waited for another interaction even though the finger had already released.
describe("the app controls under a finger", () => {
  it("starts a new session on a tap that never becomes a click", () => {
    const press = vi.fn();
    render(<NewSessionButton machine="alpha" onPress={press} />);
    const create = screen.getByRole("button", { name: "New session on alpha" });

    tap(create);
    expect(press).toHaveBeenCalledTimes(1);

    fireEvent.click(create);
    expect(press).toHaveBeenCalledTimes(1);
  });

  it("changes a response dial on a tap that never becomes a click", () => {
    const press = vi.fn();
    render(<MetaButton onClick={press}>deep</MetaButton>);
    const dial = screen.getByRole("button", { name: "deep" });

    tap(dial);
    expect(press).toHaveBeenCalledTimes(1);

    fireEvent.click(dial);
    expect(press).toHaveBeenCalledTimes(1);
  });
});
