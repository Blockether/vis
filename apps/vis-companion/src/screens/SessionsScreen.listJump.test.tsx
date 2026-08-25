// @vitest-environment jsdom
import { screen, waitFor } from "@testing-library/react";
import { describe, expect, it } from "vitest";

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

// The old fleet view could insert a late machine's whole section above the row under the
// reader's thumb. There is no unscoped list now: a slow selected machine stays selected,
// and an already-loaded inactive machine never paints behind its tab.
describe("the sessions list while a machine loads", () => {
  it("keeps the first machine selected instead of painting an inactive machine", async () => {
    const view = renderSessionsScreen({
      machines: [
        {
          label: "alpha",
          sessions: [listSession({ id: "alpha-0", title: "First" })],
          holdsList: true,
        },
        { label: "beta", sessions: [listSession({ id: "beta-0", title: "Second" })] },
      ],
    });
    try {
      const alpha = screen.getByRole("button", { name: /^alpha/ });
      const beta = screen.getByRole("button", { name: /^beta/ });
      expect(alpha.getAttribute("aria-pressed")).toBe("true");
      expect(beta.getAttribute("aria-pressed")).toBe("false");
      expect(screen.queryByText("Second")).toBeNull();

      view.releasePages();
      await waitFor(() => expect(screen.getByText("First")).toBeTruthy());
      expect(screen.queryByText("Second")).toBeNull();
    } finally {
      view.restore();
      view.unmount();
    }
  });
});
