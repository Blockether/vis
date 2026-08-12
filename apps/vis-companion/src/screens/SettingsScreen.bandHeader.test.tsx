// @vitest-environment jsdom
import { render } from "@testing-library/react";
import { afterEach, describe, expect, it } from "vitest";

import { SettingsPanel } from "./SettingsScreen";

afterEach(() => {
  document.body.innerHTML = "";
});

const band = (meta: string) => {
  render(
    <SettingsPanel
      title="Notifications"
      description="Alerts from THIS machine only — every paired machine has its own switch."
      meta={meta}
    >
      <div />
    </SettingsPanel>,
  );
  const header = document.querySelector("header");
  if (!header) throw new Error("the band has no header");
  return header;
};

// Regression, user report ("this element looks awful on iOS", the Notifications
// band): the status was `shrink-0` beside a `flex-1` title, so a long one --
// "0 devices · via <relay host>" -- took 339 of a 390px iPhone's 390, leaving the
// title box 15px wide clipped to "NOTIFI…", the description wrapping one word per
// line, and the band standing 213px tall.
describe("a settings band header carrying a long status", () => {
  it("measures the title at its own width so the status wraps instead", () => {
    const header = band("0 devices · via relay.example.com");

    expect(header.className).toContain("flex-wrap");
    const title = header.querySelector("h3");
    expect(title?.className).toContain("flex-auto");
  });

  it("never lets the status claim its width ahead of the name", () => {
    const status = band("0 devices · via relay.example.com").querySelector("span");

    expect(status?.className).not.toContain("shrink-0");
    expect(status?.className).toContain("max-w-full");
  });

  it("gives the description the whole band, never a column beside the status", () => {
    const header = band("0 devices · via relay.example.com");
    const description = header.querySelector("p");

    expect(description?.parentElement).toBe(header);
    expect(description?.className).toContain("w-full");
  });
});
