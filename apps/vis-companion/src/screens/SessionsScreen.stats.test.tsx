// @vitest-environment jsdom
import { screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { afterEach, describe, expect, it } from "vitest";

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

let restore = () => {};
afterEach(() => restore());

// Regression, user report ("remove the top errors and top tools, we don't use them
// anymore"): the expanded session card printed TOP TOOLS and TOP ERRORS pairs under
// its number grid — two rankings nobody read, one of them the only warn-coloured ink
// on the card. A gateway that still sends `top_tools`/`top_errors` must not bring
// them back.
describe("the expanded session card", () => {
  const usage = {
    turn_count: 2,
    iteration_count: 5,
    tool_call_count: 59,
    fold_count: 0,
    input_tokens: 85_000,
    output_tokens: 2_300,
    cache_read_share_percent: 77,
    reusable_prefix_coverage_percent: 98,
    prompt_cache_reusable_tokens: 81_000,
    prompt_cache_reused_tokens: 79_400,
    cost_usd: 0.21,
    duration_ms: 47_000,
    provider: "anthropic-coding-plan",
    model: "claude-opus-5",
    top_tools: [{ name: "python_execution", count: 59 }],
    error_count: 3,
    top_errors: [{ name: "python_execution", count: 3 }],
  };

  const open = async () => {
    const view = renderSessionsScreen({
      machines: [
        {
          sessions: [listSession({ id: "s1", title: "Chat session" })],
          routes: { "/v1/sessions/s1/usage": { usage } },
        },
      ],
    });
    restore = view.restore;
    await screen.findByText("Chat session");
    await userEvent.click(
      screen.getByRole("button", { name: "Show details for Chat session" }),
    );
    return view;
  };

  it("keeps the totals and drops the tool and error rankings", async () => {
    await open();
    // The grid the report kept, proving the card did expand.
    expect(await screen.findByText("Cost")).toBeTruthy();
    expect(screen.getByText("85k")).toBeTruthy();
    expect(screen.getByText("2.3k")).toBeTruthy();
    expect(screen.getByText("$0.21")).toBeTruthy();
    expect(screen.getByText("59")).toBeTruthy();

    // Model and Active are the whole labelled meta row now.
    expect(screen.getByText("Model")).toBeTruthy();
    expect(screen.getByText("Active")).toBeTruthy();
    expect(screen.queryByText("Top tools")).toBeNull();
    expect(screen.queryByText("Top errors")).toBeNull();
    expect(screen.queryByText(/python_execution/)).toBeNull();
  });

  it("puts both cache metrics in a separate explained row", async () => {
    await open();
    const cachedInput = await screen.findByText("Cached input");
    const reuseCoverage = screen.getByText("Reuse coverage");
    expect(screen.getByText("Share of all input served from provider cache")).toBeTruthy();
    expect(screen.getByText("Share of reusable prior input recovered from cache")).toBeTruthy();
    expect(cachedInput.closest("dl")).toBe(reuseCoverage.closest("dl"));
    expect(cachedInput.closest("dl")).not.toBe(screen.getByText("Turns").closest("dl"));
    expect(screen.getByText("77%")).toBeTruthy();
    expect(screen.getByText("98%")).toBeTruthy();
  });

  it("uses the compact type step for both cache values", async () => {
    await open();
    const cacheValue = await screen.findByText("77%");
    expect(cacheValue.classList.contains("text-chip")).toBe(true);
    expect(cacheValue.classList.contains("text-meta")).toBe(false);
  });

  it("leaves no warn-coloured value behind on the card", async () => {
    const view = await open();
    await screen.findByText("Cost");
    expect(view.container.querySelectorAll(".text-warn-strong")).toHaveLength(0);
  });
});
