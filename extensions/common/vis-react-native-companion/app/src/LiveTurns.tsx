import React from "react";
import { ScrollView, StyleSheet, Text, View } from "react-native";

import { CtxUtilization, GatewayEvent, TraceIteration } from "./VisClient";
import { c, mono } from "./theme";

/* ── live turn state ──────────────────────────────────────────────────
   The RN companion reduces the gateway SSE stream (GET /v1/sessions/:sid/
   events) into per-turn live state so a running turn paints its tool-call
   cards + streaming prose AS THEY HAPPEN, instead of the poll-only
   "thinking…" placeholder that waited for the settled answer.

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

export type LiveTurn = {
  cards: LiveCard[];
  /* the growing prose tail (content.delta carries the whole tail each frame,
     so we REPLACE, never append) */
  prose: string;
  /* the moving reasoning ticker tail (reasoning.delta, likewise a replace) */
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
  cards: [],
  prose: "",
  thinking: "",
  done: false,
});

/* A successful native tool's op-card already says what ran, so its synthesized
   invocation source is redundant chrome — DROP it. python_execution (the
   model's own program) and any errored form keep their code. Mirrors
   form/hide-tool-code?. */
const hideCodeFor = (toolName?: string, error?: string): boolean =>
  !!toolName && toolName !== "python_execution" && !error;

const upsertCard = (cards: LiveCard[], card: LiveCard): LiveCard[] => {
  const i = cards.findIndex((k) => k.key === card.key);
  if (i < 0) return [...cards, card];
  const next = cards.slice();
  next[i] = { ...next[i], ...card };
  return next;
};

const blockKey = (ev: GatewayEvent): string =>
  `${ev.iteration ?? 0}:${ev.block_id ?? 0}`;

/* Fold one SSE event into the live-turn map (immutably — a new object per
   change so React re-renders the running bubble). */
export const reduceLiveEvent = (
  state: LiveState,
  ev: GatewayEvent,
): LiveState => {
  const tid = ev.turn_id;
  if (!tid) return state;
  const cur = state[tid] ?? emptyLiveTurn();

  switch (ev.type) {
    case "turn.started":
      return { ...state, [tid]: emptyLiveTurn() };

    case "block.started": {
      const card: LiveCard = {
        key: blockKey(ev),
        toolName: ev.tool_name,
        label: toolLabel(ev.tool_name),
        colorRole: ev.tool_color_role,
        code: ev.code,
        running: true,
        hideCode: hideCodeFor(ev.tool_name, undefined),
      };
      return {
        ...state,
        [tid]: { ...cur, cards: upsertCard(cur.cards, card), thinking: "" },
      };
    }

    case "block.output": {
      const key = blockKey(ev);
      /* a vis_silent result suppresses the row entirely */
      if (ev.silent) {
        return {
          ...state,
          [tid]: { ...cur, cards: cur.cards.filter((k) => k.key !== key) },
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
        [tid]: { ...cur, cards: upsertCard(cur.cards, card) },
      };
    }

    case "reasoning.delta":
      return { ...state, [tid]: { ...cur, thinking: ev.text ?? cur.thinking } };

    case "content.delta":
      return {
        ...state,
        [tid]: { ...cur, prose: ev.text ?? cur.prose, thinking: "" },
      };

    case "iteration.completed":
    case "iteration.error":
      return { ...state, [tid]: { ...cur, thinking: "" } };

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
  const color = toolColor(card.colorRole);
  const showCode = !card.hideCode && !!card.code;
  const dur = fmtMs(card.durationMs);
  return (
    <View style={styles.card}>
      <View style={styles.cardHead}>
        <View style={[styles.badge, { backgroundColor: color }]}>
          <Text style={styles.badgeText}>{card.label}</Text>
        </View>
        {card.summary ? (
          <Text style={styles.cardSummary} numberOfLines={2}>
            {card.summary}
          </Text>
        ) : card.running ? (
          <Text style={styles.cardRunning}>running{"\u2026"}</Text>
        ) : null}
        {dur ? <Text style={styles.cardDur}>{dur}</Text> : null}
      </View>
      {showCode ? (
        <ScrollView horizontal showsHorizontalScrollIndicator={false}>
          <Text style={styles.cardCode}>{card.code}</Text>
        </ScrollView>
      ) : null}
      {card.error ? (
        <Text style={styles.cardErr} numberOfLines={8}>
          {card.error}
        </Text>
      ) : card.body ? (
        <Text style={styles.cardBody} numberOfLines={12}>
          {card.body}
        </Text>
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
  cardHead: { flexDirection: "row", alignItems: "center", gap: 7 },
  badge: { paddingHorizontal: 5, paddingVertical: 1 },
  badgeText: {
    fontFamily: mono,
    fontSize: 9,
    fontWeight: "700",
    letterSpacing: 0.6,
    color: "#FFFFFF",
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
  cardCode: { fontFamily: mono, fontSize: 11, color: c.codeInk },
  cardBody: { fontFamily: mono, fontSize: 10.5, lineHeight: 15, color: c.dim },
  cardErr: { fontFamily: mono, fontSize: 10.5, lineHeight: 15, color: c.err },
});
