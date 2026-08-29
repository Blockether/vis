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

    // The band is one centred row — the name's cell, then the verb's — so the
    // wrapping the status needs belongs to the CELL that holds the name, not to
    // the row that centres a 32px mark against whatever that cell grows to.
    expect(header.className).toContain("items-center");
    expect(header.className).not.toContain("items-baseline");
    const cell = header.querySelector("div");
    expect(cell?.className).toContain("flex-wrap");
    expect(cell?.className).toContain("items-baseline");
    const title = header.querySelector("h3");
    expect(title?.className).toContain("flex-auto");
  });

  it("never lets the status claim its width ahead of the name", () => {
    const status = band("0 devices · via relay.example.com").querySelector("span");

    expect(status?.className).not.toContain("shrink-0");
    expect(status?.className).toContain("max-w-full");
  });

  it("carries no sentence under the name", () => {
    // A band NAMES its group; the description that used to ride under the title
    // said what the rows under it already say, and the prop is gone.
    expect(band("0 devices · via relay.example.com").querySelector("p")).toBeNull();
  });
});
