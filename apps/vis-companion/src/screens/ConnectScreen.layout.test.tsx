// @vitest-environment jsdom
import { render } from "@testing-library/react";
import { afterEach, describe, expect, it } from "vitest";

import { ConnectScreen } from "./ConnectScreen";

afterEach(() => {
  document.body.innerHTML = "";
});

// Regression, user report ("width discrepancies between the session list and the
// machines"): the Machines screen was capped at a narrow desktop column while the
// session list used the full app frame, so switching screens moved both side edges.
describe("Machines screen frame", () => {
  it("uses the same full-width desktop frame as the session list", () => {
    const view = render(
      <ConnectScreen
        conns={[]}
        active={null}
        primary={null}
        onAdd={async () => {}}
        onSettings={() => {}}
      />,
    );

    const frames = [...view.container.querySelectorAll("*")].filter((element) =>
      element.className.toString().includes("max-w-[1400px]"),
    );
    expect(frames).not.toHaveLength(0);
    expect(
      [...view.container.querySelectorAll("*")].filter((element) =>
        element.className.toString().includes("max-w-3xl"),
      ),
    ).toHaveLength(0);
  });
});
