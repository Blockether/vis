import { describe, expect, it } from "vitest";

import { settledTranscriptCoversLiveTurn } from "./live-turn-handover";

const now = 1_700_000_000_000;

describe("settled transcript handover", () => {
  it("does not retire visible output against an unrelated new settled row", () => {
    expect(
      settledTranscriptCoversLiveTurn(
        [
          {
            id: "engine-previous",
            status: "completed",
            user_request: "the previous voice turn",
            created_at: now,
          },
        ],
        new Set(),
        {
          id: "gateway-current",
          request: "the current voice turn",
          startedAt: now,
        },
      ),
    ).toBe(false);
  });

  it("hands over when the engine row has the same request but a different id", () => {
    expect(
      settledTranscriptCoversLiveTurn(
        [
          {
            id: "engine-current",
            status: "completed",
            user_request: "the current voice turn",
            created_at: now,
          },
        ],
        new Set(),
        {
          id: "gateway-current",
          request: "the current voice turn",
          startedAt: now,
        },
      ),
    ).toBe(true);
  });

  it("keeps painted output until the settled row actually carries output", () => {
    const placeholder = {
      id: "engine-current",
      status: "completed",
      user_request: "the current voice turn",
      created_at: now,
      content: [],
      iterations: [],
    };

    expect(
      settledTranscriptCoversLiveTurn([placeholder], new Set(), {
        id: "gateway-current",
        request: "the current voice turn",
        startedAt: now,
        requireOutput: true,
      }),
    ).toBe(false);

    expect(
      settledTranscriptCoversLiveTurn(
        [
          {
            ...placeholder,
            content: [
              { id: "answer", type: "prose", markdown: "Already visible." },
            ],
          },
        ],
        new Set(),
        {
          id: "gateway-current",
          request: "the current voice turn",
          startedAt: now,
          requireOutput: true,
        },
      ),
    ).toBe(true);
  });

  it("keeps painted prose until the persisted row carries prose, not only reasoning", () => {
    const reasoningOnly = {
      id: "engine-current",
      status: "completed",
      user_request: "the current voice turn",
      created_at: now,
      content: [],
      iterations: [{ position: 0, thinking: "Private reasoning arrived first." }],
    };

    expect(
      settledTranscriptCoversLiveTurn([reasoningOnly], new Set(), {
        id: "gateway-current",
        request: "the current voice turn",
        startedAt: now,
        requireOutput: true,
        requireProse: true,
      }),
    ).toBe(false);

    expect(
      settledTranscriptCoversLiveTurn(
        [
          {
            ...reasoningOnly,
            iterations: [
              {
                position: 0,
                thinking: "Private reasoning arrived first.",
                assistant_prose: "The answer that was already painted.",
              },
            ],
          },
        ],
        new Set(),
        {
          id: "gateway-current",
          request: "the current voice turn",
          startedAt: now,
          requireOutput: true,
          requireProse: true,
        },
      ),
    ).toBe(true);
  });

  it("accepts an exact persisted id without depending on request projection", () => {
    expect(
      settledTranscriptCoversLiveTurn(
        [{ id: "same", status: "completed", created_at: now }],
        new Set(),
        { id: "same", request: "display form", startedAt: now },
      ),
    ).toBe(true);
  });
});
