// @vitest-environment jsdom
import { act, fireEvent, screen, waitFor } from "@testing-library/react";
import { flushSync } from "react-dom";
import { describe, expect, it, vi } from "vitest";

import * as sessionHeader from "../components/SessionHeader";
import { draftMessageKey, peekDraftMessage } from "../lib/draft-messages";
import { renderSessionScreen, sessionFixture } from "./session-screen-harness";

// Regression: typing lag was reported with both English and Polish keyboards.
// Native edits must not also rewrite the textarea's reset text on every input:
// React's controlled textarea mirrors the value into defaultValue / child text.
describe("native composer input", () => {
  it.each(["The keyboard is still slow", "Klawiatura nadal się zacina"])(
    "does not rewrite the reset text while typing %s",
    (text) => {
      renderSessionScreen();
      const composer = screen.getByLabelText(
        "Message Vis",
      ) as HTMLTextAreaElement;
      composer.focus();
      const observer = new MutationObserver(() => {});
      observer.observe(composer, {
        childList: true,
        characterData: true,
        subtree: true,
      });

      for (let length = 1; length <= text.length; length += 1) {
        fireEvent.input(composer, {
          target: { value: text.slice(0, length) },
          inputType: "insertText",
        });
      }

      expect(composer.value).toBe(text);
      const resetMutations = observer.takeRecords();
      observer.disconnect();
      expect(resetMutations).toHaveLength(0);
    },
  );

  it.each(["insertReplacementText", "insertCompositionText"])(
    "keeps the native value and selection during %s",
    (inputType) => {
      const view = renderSessionScreen();
      const composer = screen.getByLabelText(
        "Message Vis",
      ) as HTMLTextAreaElement;
      composer.focus();
      fireEvent.input(composer, { target: { value: "We are writng text" } });
      const valueWrites = vi.spyOn(composer, "value", "set");
      const composing = inputType === "insertCompositionText";
      if (composing) fireEvent.compositionStart(composer);

      fireEvent.input(composer, {
        target: {
          value: "We are writing text",
          selectionStart: 14,
          selectionEnd: 14,
        },
        inputType,
        isComposing: composing,
      });
      view.rerenderSession("s1");
      if (composing) fireEvent.compositionEnd(composer);

      expect(composer.value).toBe("We are writing text");
      expect(composer.selectionStart).toBe(14);
      expect(composer.selectionEnd).toBe(14);
      expect(document.activeElement).toBe(composer);
      expect(valueWrites).not.toHaveBeenCalled();
      valueWrites.mockRestore();
    },
  );

  it("clears the native editor immediately when a message is sent", () => {
    const submitTurn = vi.fn((_sid: string, _request: string) =>
      Promise.resolve({ turn_id: "native-send", status: "running" }),
    );
    renderSessionScreen({ client: { submitTurn } });
    const composer = screen.getByLabelText(
      "Message Vis",
    ) as HTMLTextAreaElement;
    const text = "Send the latest words, including żółć";
    fireEvent.input(composer, { target: { value: text } });

    fireEvent.click(screen.getByRole("button", { name: "Send message" }));

    expect(submitTurn).toHaveBeenCalled();
    expect(submitTurn.mock.calls[0]?.[1]).toBe(text);
    expect(composer.value).toBe("");
  });

  // WebKit issue #164077: a correction can precede its input notification.
  // Unrelated transcript renders must not write the stale React snapshot back.
  it("preserves a native correction before the input event arrives", () => {
    const view = renderSessionScreen();
    const composer = screen.getByLabelText(
      "Message Vis",
    ) as HTMLTextAreaElement;
    fireEvent.input(composer, { target: { value: "To będzie dlugie zdanie" } });
    composer.value = "To będzie długie zdanie";
    composer.setSelectionRange(15, 15);

    view.rerenderSession("s1");

    expect(composer.value).toBe("To będzie długie zdanie");
    expect(composer.selectionStart).toBe(15);
  });

  it("restores the native draft when sending fails", async () => {
    const submitTurn = vi.fn(() => Promise.reject(new Error("Could not send")));
    renderSessionScreen({ client: { submitTurn } });
    const composer = screen.getByLabelText(
      "Message Vis",
    ) as HTMLTextAreaElement;
    fireEvent.input(composer, { target: { value: "Keep these words" } });

    fireEvent.click(screen.getByRole("button", { name: "Send message" }));
    expect(composer.value).toBe("");

    await waitFor(() => expect(composer.value).toBe("Keep these words"));
    expect(screen.getByText("Could not send")).toBeInTheDocument();
  });

  it("does not carry native text into another session", () => {
    const view = renderSessionScreen();
    const composer = screen.getByLabelText(
      "Message Vis",
    ) as HTMLTextAreaElement;
    fireEvent.input(composer, { target: { value: "Only for this session" } });

    view.rerenderSession("another-composer");

    expect(screen.getByLabelText("Message Vis")).toBe(composer);
    expect(composer.value).toBe("");
  });

  // Regression: even an uncontrolled editor synchronously rendered the whole
  // session on each input and selection event, ahead of the keyboard's next frame.
  it("keeps screen rendering out of native input and selection delivery", async () => {
    const header = vi.spyOn(sessionHeader, "SessionHeader");
    try {
      await act(async () => {
        renderSessionScreen();
      });
      const composer = screen.getByLabelText(
        "Message Vis",
      ) as HTMLTextAreaElement;
      composer.focus();
      header.mockClear();

      act(() => {
        flushSync(() => {
          fireEvent.input(composer, {
            target: { value: "Piszę bez przycięć" },
          });
          fireEvent.select(composer, {
            target: { selectionStart: 5, selectionEnd: 5 },
          });
        });
        expect(composer.value).toBe("Piszę bez przycięć");
        expect(header).not.toHaveBeenCalled();
      });
      expect(header).toHaveBeenCalled();
    } finally {
      header.mockRestore();
    }
  });

  it("records native text before a deferred screen render or navigation", async () => {
    const sid = "input-before-render";
    await act(async () => {
      renderSessionScreen({ session: sessionFixture({ id: sid }) });
    });
    const composer = screen.getByLabelText(
      "Message Vis",
    ) as HTMLTextAreaElement;
    const key = draftMessageKey("http://gateway.example.com", sid);

    act(() => {
      fireEvent.input(composer, {
        target: { value: "Nie zgub tej wiadomości" },
      });
      expect(peekDraftMessage(key).text).toBe("Nie zgub tej wiadomości");
    });
  });
  it("does not overwrite a newer native correction when a typing render resumes", async () => {
    const sid = "correction-during-render";
    await act(async () => {
      renderSessionScreen({ session: sessionFixture({ id: sid }) });
    });
    const composer = screen.getByLabelText(
      "Message Vis",
    ) as HTMLTextAreaElement;
    act(() => {
      flushSync(() => {
        fireEvent.input(composer, { target: { value: "Dlugie zdanie" } });
      });
      // WebKit can commit a correction before its next input notification.
      composer.value = "Długie zdanie";
      composer.setSelectionRange(6, 6);
    });
    expect(composer.value).toBe("Długie zdanie");
    expect(composer.selectionStart).toBe(6);
    expect(
      peekDraftMessage(draftMessageKey("http://gateway.example.com", sid)).text,
    ).toBe("Długie zdanie");
  });

  it("sends and clears text before its first typing render commits", async () => {
    const submitTurn = vi.fn((_sid: string, _request: string) =>
      Promise.resolve({ turn_id: "fast-send", status: "running" }),
    );
    await act(async () => {
      renderSessionScreen({ client: { submitTurn } });
    });
    const composer = screen.getByLabelText(
      "Message Vis",
    ) as HTMLTextAreaElement;
    act(() => {
      flushSync(() => {
        fireEvent.input(composer, { target: { value: "Wyślij całe zdanie" } });
      });
      flushSync(() => {
        fireEvent.click(screen.getByRole("button", { name: "Send message" }));
      });
      expect(composer.value).toBe("");
    });
    expect(submitTurn.mock.calls[0]?.[1]).toBe("Wyślij całe zdanie");
    expect(composer.value).toBe("");
  });

  it("inserts a collapsed paste into native text ahead of the rendering snapshot", async () => {
    await act(async () => {
      renderSessionScreen();
    });
    const composer = screen.getByLabelText(
      "Message Vis",
    ) as HTMLTextAreaElement;
    act(() => {
      flushSync(() => {
        fireEvent.input(composer, { target: { value: "Before  after" } });
      });
      composer.setSelectionRange(7, 7);
      flushSync(() => {
        fireEvent.paste(composer, {
          clipboardData: {
            files: [],
            getData: () => "Many lines of text\n".repeat(30),
          },
        });
      });
    });
    expect(composer.value).toMatch(/^Before \[Pasted #1[^\]]*\] after$/);
  });

  it("does not replay pending input into the next session", async () => {
    let view: ReturnType<typeof renderSessionScreen>;
    await act(async () => {
      view = renderSessionScreen();
    });
    const composer = screen.getByLabelText(
      "Message Vis",
    ) as HTMLTextAreaElement;
    act(() => {
      flushSync(() => {
        fireEvent.input(composer, {
          target: { value: "Belongs to the previous session" },
        });
      });
      flushSync(() => view.rerenderSession("session-after-pending-input"));
    });
    expect(composer.value).toBe("");
    expect(
      peekDraftMessage(draftMessageKey("http://gateway.example.com", "s1"))
        .text,
    ).toBe("Belongs to the previous session");
  });
});
