import React, { useState } from "react";
import { Pressable, ScrollView, StyleSheet, Text, View } from "react-native";

import { CtxUtilization, GatewayEvent, TraceIteration } from "./VisClient";
import { c, mono } from "./theme";
import { Highlight } from "./Highlight";
import { Markdown, renderInline } from "./Markdown";

/* ── live turn state ──────────────────────────────────────────────────
   The RN companion reduces the gateway SSE stream (GET /v1/sessions/:sid/
   events) into per-turn live state so a running turn paints its tool-call
   cards as they happen. Model prose/thinking does NOT stream token-by-token;
   the gateway sends complete text on iteration.completed.

   The wire is flat JSON per event (wire/sse-frame → json-str): keyword map
   keys arrive snake_case with the namespace dropped, so `:vis/tool-name` →
   `tool_name`, `:tool-color-role` → `tool_color_role`, `:result-summary` →
   `result_summary`. Keyword VALUES keep their namespace, so a colour role
   rides as the string `"tool-color/search"`. */

export type LiveCard = {
  /* stable per (iteration, block) so block.started and its block.output
     land on the SAME card */
  key: string;
  toolName?: string | undefined;
  label: string;
  colorRole?: string | undefined;
  code?: string | undefined;
  summary?: string | undefined;
  body?: string | undefined;
  error?: string | undefined;
  durationMs?: number | undefined;
  running: boolean;
  hideCode: boolean;
};

/* One pinned block in a running turn's thread, in ARRIVAL order — the same
   TUI-parity model the web channel uses (#live append): tool op-cards stream
   as they run, then each iteration's complete thinking + prose pin PERMANENTLY
   (they are NOT overwritten by the next iteration, so nothing vanishes after an
   SSE tick). Model text never streams token-by-token; it lands whole on
   iteration.completed. */
export type LiveBlock =
  | { kind: "card"; key: string; card: LiveCard }
  | { kind: "thinking"; key: string; text: string }
  | { kind: "prose"; key: string; text: string };

export type LiveTurn = {
  /* Every block pinned so far, in arrival order — never overwritten. */
  blocks: LiveBlock[];
  /* Transient live reasoning ticker (only if a build streams reasoning.delta);
     cleared once the iteration pins its complete thinking block. */
  thinking: string;
  done: boolean;
};

export type LiveState = Record<string, LiveTurn>;

/* Native-tool wire name → op-card badge label. Mirrors form/label-overrides;
   everything else uppercases its wire name. */
const LABEL_OVERRIDES: Record<string, string> = {
  python_execution: "RESULT",
  repl_eval: "REPL",
  shell_run: "SHELL RUN",
  shell_bg: "SHELL BACKGROUND",
};

export const toolLabel = (toolName?: string): string => {
  if (!toolName) return "RUN";
  return LABEL_OVERRIDES[toolName] ?? toolName.toUpperCase();
};

/* tool-color/<role> → a hex accent (the web channel's --tool-* vars, hand-
   mapped for the phone). Keyed off form/tool-color-roles. */
const ROLE_COLORS: Record<string, string> = {
  read: "#2563EB",
  search: "#7C3AED",
  preview: "#0891B2",
  edit: "#B45309",
  create: "#16A34A",
  delete: "#DC2626",
  move: "#0D9488",
  shell: "#4B5563",
  meta: "#6F6A63",
  test: "#CA8A04",
};

export const toolColor = (role?: string): string => {
  if (!role) return c.dim;
  const key = role.includes("/") ? role.slice(role.indexOf("/") + 1) : role;
  return ROLE_COLORS[key] ?? c.dim;
};

/* Context-fullness percent from the session-view utilization the model reads
   (`session_utilization` / the live `context.updated` event). `saturation` is
   already a 0..100 percent server-side (ctx-engine rounds 100*req/window); if it
   is absent, derive it from the most recent request size vs the free headroom.
   Clamped 0..100; null when there is nothing to show yet. */
export const ctxPct = (u?: CtxUtilization | null): number | null => {
  if (!u) return null;
  if (typeof u.saturation === "number" && isFinite(u.saturation)) {
    return Math.max(0, Math.min(100, Math.round(u.saturation)));
  }
  const req = u.last_request_tokens;
  const head = u.headroom_tokens;
  if (typeof req === "number" && typeof head === "number" && req + head > 0) {
    return Math.max(0, Math.min(100, Math.round((100 * req) / (req + head))));
  }
  return null;
};

const emptyLiveTurn = (): LiveTurn => ({
  blocks: [],
  thinking: "",
  done: false,
});

/* A successful native tool's op-card already says what ran, so its synthesized
   invocation source is redundant chrome — DROP it. python_execution (the
   model's own program) and any errored form keep their code. Mirrors
   form/hide-tool-code?. */
const hideCodeFor = (toolName?: string, error?: string): boolean =>
  !!toolName && toolName !== "python_execution" && !error;

const upsertBlock = (blocks: LiveBlock[], block: LiveBlock): LiveBlock[] => {
  const i = blocks.findIndex((b) => b.key === block.key);
  if (i < 0) return [...blocks, block];
  const next = blocks.slice();
  const prev = next[i]!;
  /* block.output enriches the card block.started already pinned — MERGE fields
     so the same card updates IN PLACE, keeping its arrival position. */
  next[i] =
    prev.kind === "card" && block.kind === "card"
      ? { kind: "card", key: block.key, card: { ...prev.card, ...block.card } }
      : block;
  return next;
};

const blockKey = (ev: GatewayEvent): string =>
  `${ev.iteration ?? 0}:${ev.block_id ?? 0}`;

/* Fold one SSE event into the live-turn map (immutably — a new object per
   change so React re-renders the running bubble). Every block PINS in arrival
   order and is never overwritten by a later iteration, so a multi-iteration
   turn keeps ALL of its thinking + prose + op-cards (TUI / web parity) instead
   of collapsing to only the last iteration's text — the bug where prose and
   thinking vanished after each SSE tick. */
export const reduceLiveEvent = (
  state: LiveState,
  ev: GatewayEvent,
): LiveState => {
  const tid = ev.turn_id;
  if (!tid) return state;
  const cur = state[tid] ?? emptyLiveTurn();
  const iter = ev.iteration ?? 0;

  switch (ev.type) {
    case "turn.started":
      return { ...state, [tid]: emptyLiveTurn() };

    case "block.started": {
      const key = blockKey(ev);
      const card: LiveCard = {
        key,
        toolName: ev.tool_name,
        label: toolLabel(ev.tool_name),
        colorRole: ev.tool_color_role,
        code: ev.code,
        running: true,
        hideCode: hideCodeFor(ev.tool_name, undefined),
      };
      return {
        ...state,
        [tid]: {
          ...cur,
          blocks: upsertBlock(cur.blocks, { kind: "card", key, card }),
        },
      };
    }

    case "block.output": {
      const key = blockKey(ev);
      /* a vis_silent result suppresses the row entirely */
      if (ev.silent) {
        return {
          ...state,
          [tid]: { ...cur, blocks: cur.blocks.filter((b) => b.key !== key) },
        };
      }
      const card: LiveCard = {
        key,
        toolName: ev.tool_name,
        label: toolLabel(ev.tool_name),
        colorRole: ev.tool_color_role,
        code: ev.code,
        summary: ev.result_summary,
        body: (ev.result_render ?? ev.stdout) || undefined,
        error: ev.error,
        durationMs: ev.duration_ms,
        running: false,
        hideCode: hideCodeFor(ev.tool_name, ev.error),
      };
      return {
        ...state,
        [tid]: {
          ...cur,
          blocks: upsertBlock(cur.blocks, { kind: "card", key, card }),
        },
      };
    }

    case "iteration.completed": {
      /* Pin THIS iteration's complete thinking + prose as their own blocks,
         keyed by iteration so replays stay idempotent and no later iteration
         overwrites an earlier one's text. */
      let blocks = cur.blocks;
      const thinking = (ev.thinking ?? "").trim();
      if (thinking)
        blocks = upsertBlock(blocks, {
          kind: "thinking",
          key: `thinking:${iter}`,
          text: thinking,
        });
      const prose = (ev.assistant_prose ?? "").trim();
      if (prose)
        blocks = upsertBlock(blocks, {
          kind: "prose",
          key: `prose:${iter}`,
          text: prose,
        });
      return { ...state, [tid]: { ...cur, blocks, thinking: "" } };
    }

    case "iteration.error": {
      const thinking = (ev.thinking ?? "").trim();
      if (!thinking) return state;
      return {
        ...state,
        [tid]: {
          ...cur,
          blocks: upsertBlock(cur.blocks, {
            kind: "thinking",
            key: `thinking:${iter}`,
            text: thinking,
          }),
        },
      };
    }

    case "turn.completed":
    case "turn.failed":
      return { ...state, [tid]: { ...cur, done: true } };

    default:
      return state;
  }
};

/* Rebuild the settled tool cards of a turn from its persisted trace
   (GET /turns/:tid/trace). Mirrors the live block.started/block.output reducer
   so a scrolled-back turn shows the SAME cards it streamed: `src` is the code,
   `result_render` the body. The persisted form carries no error flag (its `tag`
   is the observation/mutation class, not a status), so trace cards never style
   as errors — a failed tool still shows its result text in the body. Empty when
   the turn ran no tools. Pure — unit-tested. */
export const traceCards = (iterations: TraceIteration[]): LiveCard[] => {
  const cards: LiveCard[] = [];
  iterations.forEach((it, i) => {
    const iter = it.position ?? it.iteration ?? i;
    (it.forms ?? []).forEach((f, j) => {
      cards.push({
        key: `${iter}:${j}`,
        toolName: f.tool_name,
        label: toolLabel(f.tool_name),
        colorRole: f.tool_color_role,
        code: f.src,
        summary: f.result_summary,
        body: f.result_render || undefined,
        durationMs: f.duration_ms,
        running: false,
        hideCode: hideCodeFor(f.tool_name, undefined),
      });
    });
  });
  return cards;
};

const fmtMs = (ms?: number): string | null => {
  if (!ms || ms <= 0) return null;
  if (ms < 1000) return `${ms}ms`;
  const s = ms / 1000;
  return `${s.toFixed(s < 10 ? 1 : 0)}s`;
};

const ToolCard = ({ card }: { card: LiveCard }) => {
  /* Native op-cards (cat/rg/patch/…) ship COLLAPSED — badge + one-line summary
     only; tap the head to reveal detail. python_execution is the model's OWN
     program, so its RESULT card opens by default and shows the python + result
     together, matching the TUI. */
  const [open, setOpen] = useState(card.toolName === "python_execution");
  const color = toolColor(card.colorRole);
  const showCode = open && !card.hideCode && !!card.code;
  const dur = fmtMs(card.durationMs);
  const hasDetail =
    (!card.hideCode && !!card.code) || !!card.error || !!card.body;
  return (
    <View style={styles.card}>
      <Pressable
        style={styles.cardHead}
        onPress={() => setOpen((v) => !v)}
        disabled={!hasDetail}
      >
        <Text style={[styles.caret, hasDetail ? { color } : styles.caretIdle]}>
          {hasDetail ? (open ? "\u25BE" : "\u25B8") : "\u2022"}
        </Text>
        {card.summary ? (
          <Text style={styles.cardSummary} numberOfLines={1}>
            {renderInline(card.summary, card.key)}
          </Text>
        ) : card.running ? (
          <Text style={styles.cardRunning}>running{"\u2026"}</Text>
        ) : null}
        {dur ? <Text style={styles.cardDur}>{dur}</Text> : null}
      </Pressable>
      {open ? (
        <View style={styles.cardDetail}>
          {showCode ? (
            <ScrollView horizontal showsHorizontalScrollIndicator={false}>
              <Highlight code={card.code ?? ""} style={styles.cardCode} />
            </ScrollView>
          ) : null}
          {card.error ? (
            <ScrollView style={styles.cardScroll} nestedScrollEnabled>
              <Text style={styles.cardErr}>{card.error}</Text>
            </ScrollView>
          ) : card.body ? (
            <View style={styles.resultZone}>
              {showCode ? (
                <Text style={styles.resultLabel}>RESULT</Text>
              ) : null}
              <ScrollView style={styles.cardScroll} nestedScrollEnabled>
                <Markdown text={card.body} dense />
              </ScrollView>
            </View>
          ) : null}
        </View>
      ) : null}
    </View>
  );
};

export const LiveCards = ({ cards }: { cards: LiveCard[] }) => {
  if (!cards.length) return null;
  return (
    <View style={styles.cards}>
      {cards.map((card) => (
        <ToolCard key={card.key} card={card} />
      ))}
    </View>
  );
};

const ThinkingBlock = ({ text }: { text: string }) => (
  <View style={styles.thinkingCard}>
    <Text style={styles.thinkingLabel}>THINKING</Text>
    <Text style={styles.thinkingText}>{text}</Text>
  </View>
);

const ProseBlock = ({ text }: { text: string }) => (
  <View style={styles.proseBlock}>
    <Markdown text={text} />
  </View>
);

/* A running turn's thread, in arrival order: tool op-cards, plus each
   iteration's pinned thinking + prose (TUI / web parity). */
export const LiveBlocks = ({ blocks }: { blocks: LiveBlock[] }) => {
  if (!blocks.length) return null;
  return (
    <View style={styles.blocks}>
      {blocks.map((b) =>
        b.kind === "card" ? (
          <ToolCard key={b.key} card={b.card} />
        ) : b.kind === "thinking" ? (
          <ThinkingBlock key={b.key} text={b.text} />
        ) : (
          <ProseBlock key={b.key} text={b.text} />
        ),
      )}
    </View>
  );
};

const styles = StyleSheet.create({
  cards: { gap: 6, marginTop: 2 },
  card: {
    backgroundColor: c.codeBg,
    borderLeftWidth: 2,
    borderLeftColor: c.hair,
    paddingHorizontal: 8,
    paddingVertical: 6,
    gap: 5,
  },
  cardHead: { flexDirection: "row", alignItems: "center", gap: 6 },
  caret: { fontSize: 9, width: 10, textAlign: "center" },
  caretIdle: { color: c.tsep },
  cardDetail: { gap: 5, marginTop: 1 },
  cardScroll: { maxHeight: 240 },
  resultZone: {
    backgroundColor: c.field,
    borderLeftWidth: 2,
    borderLeftColor: c.accent,
    borderRadius: 2,
    paddingHorizontal: 6,
    paddingVertical: 4,
    gap: 2,
  },
  resultLabel: {
    fontFamily: mono,
    fontSize: 8,
    fontWeight: "700",
    letterSpacing: 1,
    color: c.dim,
  },
  cardSummary: { flex: 1, fontFamily: mono, fontSize: 11, color: c.ink2 },
  cardRunning: {
    flex: 1,
    fontFamily: mono,
    fontSize: 11,
    color: c.dim,
    fontStyle: "italic",
  },
  cardDur: { fontFamily: mono, fontSize: 9, color: c.tsep },
  cardCode: { fontFamily: mono, fontSize: 10, lineHeight: 14, color: c.codeInk },
  cardBody: { fontFamily: mono, fontSize: 10.5, lineHeight: 15, color: c.dim },
  cardErr: { fontFamily: mono, fontSize: 10, lineHeight: 14, color: c.err },
  blocks: { gap: 8, marginTop: 2 },
  proseBlock: {},
  thinkingCard: {
    backgroundColor: c.codeBg,
    borderLeftWidth: 2,
    borderLeftColor: c.accent,
    borderRadius: 2,
    paddingHorizontal: 8,
    paddingVertical: 6,
    gap: 3,
  },
  thinkingLabel: {
    fontFamily: mono,
    fontSize: 8.5,
    fontWeight: "800",
    letterSpacing: 0.8,
    color: c.dim,
  },
  thinkingText: {
    fontFamily: mono,
    fontSize: 11,
    lineHeight: 16,
    color: c.dim,
  },
});
