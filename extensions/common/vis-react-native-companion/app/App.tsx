import React, {
  memo,
  useCallback,
  useEffect,
  useMemo,
  useRef,
  useState,
} from "react";
import {
  ActivityIndicator,
  AppState,
  FlatList,
  Image,
  KeyboardAvoidingView,
  Platform,
  Pressable,
  RefreshControl,
  ScrollView,
  StatusBar,
  StyleSheet,
  Text,
  TextInput,
  View,
} from "react-native";
import { SafeAreaProvider, SafeAreaView } from "react-native-safe-area-context";
import { Feather } from "@expo/vector-icons";

import {
  GatewayProject,
  GatewayTurn,
  CtxUtilization,
  ProviderLimitsReport,
  ProviderModels,
  ProviderStatusReport,
  SessionModel,
  SessionSoul,
  SuggestRow,
  TurnAttachment,
  VisGatewayClient,
  gatewayErrorMessage,
  isGatewayConnectionMessage,
} from "./src/VisClient";
import { c, mono, shortId, timeHHMM } from "./src/theme";
import { ActionBtn, DialogModal, IconBtn } from "./src/UI";
import { SettingsPane } from "./src/Settings";
import { Markdown } from "./src/Markdown";
import {
  LiveCard,
  LiveCards,
  LiveState,
  ctxPct,
  reduceLiveEvent,
  traceCards,
} from "./src/LiveTurns";
import {
  ensureNotificationPermissions,
  notifyTurnDone,
  shouldNotifyTurnDone,
} from "./src/Notifications";
import { SessionsDrawer } from "./src/SessionsDrawer";
import {
  activeFileMention,
  applyFileMention,
  suggestLabel,
} from "./src/ComposerSuggest";
import { VoiceButton } from "./src/VoiceButton";
import {
  AttachmentTray,
  PendingAttachment,
  pickFiles,
  pickImages,
} from "./src/Attachments";

declare const process: {
  env: {
    EXPO_PUBLIC_VIS_GATEWAY_URL?: string;
    EXPO_PUBLIC_VIS_TOKEN?: string;
  };
};

const DEFAULT_GATEWAY = "http://127.0.0.1:7890";

const envGateway = process.env.EXPO_PUBLIC_VIS_GATEWAY_URL ?? DEFAULT_GATEWAY;
const envToken = process.env.EXPO_PUBLIC_VIS_TOKEN;

type Message = {
  id: string;
  role: "user" | "vis";
  text: string;
  pending?: boolean | undefined;
  time?: string | undefined;
  meta?: string | undefined;
  atts?: TurnAttachment[] | undefined;
  cards?: LiveCard[] | undefined;
  thinking?: string | undefined;
};

/* Only running/queued mean the gateway is actually working the turn. The wire's
   terminal statuses are "done" (the common finished value — NOT "completed"),
   plus failed / error / cancelled / interrupted / suspended; allow-listing a few
   terminals painted every finished turn as eternally "thinking…". */
const turnLive = (turn: GatewayTurn): boolean =>
  turn.status === "running" || turn.status === "queued";

const fmtDuration = (ms?: number): string | null => {
  if (!ms || ms <= 0) return null;
  if (ms < 1000) return `${ms}ms`;
  const s = ms / 1000;
  if (s < 60) return `${s.toFixed(s < 10 ? 1 : 0)}s`;
  const m = Math.floor(s / 60);
  return `${m}m ${Math.round(s - m * 60)}s`;
};

const fmtTokens = (n?: number): string | null => {
  if (n == null) return null;
  return n >= 1000 ? `${(n / 1000).toFixed(1)}k` : `${n}`;
};

/* "42s · $0.4677 · 87.3k→1.3k tok · 3 iter" — every finished turn carries
   duration_ms, cost.total_cost and tokens.input/output on the wire. */
const turnMeta = (turn: GatewayTurn): string | undefined => {
  const parts: string[] = [];
  const dur = fmtDuration(turn.duration_ms);
  if (dur) parts.push(dur);
  const cost = turn.cost?.total_cost;
  if (cost != null) parts.push(`$${cost.toFixed(4)}`);
  const inTok = fmtTokens(turn.tokens?.input);
  const outTok = fmtTokens(turn.tokens?.output);
  if (inTok || outTok) parts.push(`${inTok ?? "?"}→${outTok ?? "?"} tok`);
  if ((turn.iteration_count ?? 0) > 1)
    parts.push(`${turn.iteration_count} iter`);
  return parts.length ? parts.join(" · ") : undefined;
};

const imageAtts = (turn: GatewayTurn): TurnAttachment[] =>
  (turn.attachments ?? []).filter(
    (a) =>
      a.base64 &&
      (a.kind === "image" || (a.media_type ?? "").startsWith("image/")),
  );

const messageText = (turn: GatewayTurn): Message[] => {
  const id = turn.id ?? turn.turn_id ?? Math.random().toString(36);
  const user = turn.request?.trim();
  const atts = imageAtts(turn);
  const answer = (turn.answer_md ?? turn.answer)?.trim();
  const live = !answer && turnLive(turn);
  /* Terminal turn that never produced an answer (failed / interrupted / error):
     show its status quietly instead of dots-forever or a silent hole. */
  const note =
    !answer &&
    !live &&
    turn.status &&
    turn.status !== "done" &&
    turn.status !== "completed"
      ? `(${turn.status})`
      : undefined;

  return [
    ...(user || atts.length
      ? [
          {
            id: `${id}-user`,
            role: "user" as const,
            text: user ?? "",
            time: timeHHMM(turn.started_at),
            atts: atts.length ? atts : undefined,
          },
        ]
      : []),
    ...(answer || live || note
      ? [
          {
            id: `${id}-vis`,
            role: "vis" as const,
            text: answer ?? note ?? "",
            pending: live,
            time: timeHHMM(turn.completed_at),
            meta: live ? undefined : turnMeta(turn),
          },
        ]
      : []),
  ];
};

const modelLabel = (model: SessionModel): string => {
  if (!model) return "router default";
  if (typeof model === "string") return model;
  if (model.provider && model.model)
    return `${model.provider} / ${model.model}`;
  return model.model ?? model.provider ?? "router default";
};

const providerReportText = (report?: {
  status?: ProviderStatusReport | undefined;
  limits?: ProviderLimitsReport | undefined;
}): string | null => {
  if (!report) return null;
  const parts: string[] = [];
  if (report.status?.configured === false) parts.push("not configured");
  else if (report.status?.configured === true) parts.push("configured");
  if (report.limits?.status) parts.push(String(report.limits.status));
  if (report.limits?.message) parts.push(String(report.limits.message));
  const stat = report.limits?.static;
  if (stat && typeof stat === "object") {
    for (const [k, v] of Object.entries(stat).slice(0, 2)) {
      if (["string", "number", "boolean"].includes(typeof v))
        parts.push(`${k}: ${String(v)}`);
    }
  }
  return parts.length ? parts.join(" · ") : null;
};

const PendingDots = () => {
  const [tick, setTick] = useState(0);
  useEffect(() => {
    const timer = setInterval(() => setTick((t) => (t + 1) % 4), 350);
    return () => clearInterval(timer);
  }, []);
  return <Text style={styles.pending}>{`thinking${".".repeat(tick)}`}</Text>;
};

/* Flat TUI transcript row: `▌ role ──────── HH:MM` head rule, then the body
   straight on the ground — no card, no border, no bubble. */
const MessageRow = memo(
  ({ item }: { item: Message }) => {
    const roleColor = item.role === "user" ? c.roleUser : c.roleVis;
    return (
      <View style={styles.msg}>
        <View style={styles.msgHead}>
          <View style={[styles.gutter, { backgroundColor: roleColor }]} />
          <Text
            style={[
              styles.msgRole,
              { color: item.role === "user" ? c.amberDeep : c.roleVis },
            ]}
          >
            {item.role === "user" ? "you" : "vis"}
          </Text>
          <View style={styles.headRule} />
          {item.time ? <Text style={styles.msgTime}>{item.time}</Text> : null}
        </View>
        {item.atts?.length ? (
          <View style={styles.attRow}>
            {item.atts.map((a, i) => (
              <Image
                key={`${item.id}-att${i}`}
                source={{
                  uri: `data:${a.media_type ?? "image/png"};base64,${a.base64}`,
                }}
                style={styles.attImg}
                resizeMode="cover"
              />
            ))}
          </View>
        ) : null}
        {item.thinking ? (
          <Text style={styles.thinking}>{`\u2731 ${item.thinking}`}</Text>
        ) : null}
        {item.cards?.length ? <LiveCards cards={item.cards} /> : null}
        {item.text ? (
          <Markdown text={item.text} />
        ) : item.pending && !item.cards?.length && !item.thinking ? (
          <PendingDots />
        ) : null}
        {!item.pending && item.meta ? (
          <Text style={styles.msgMeta}>{item.meta}</Text>
        ) : null}
      </View>
    );
  },
  (a, b) => {
    const x = a.item;
    const y = b.item;
    /* Skip re-rendering (and re-parsing Markdown for) a row whose visible content
     is unchanged — the 1.8s poll + SSE ticks swap the array identity constantly,
     but only the running turn's row actually changes. */
    return (
      x.id === y.id &&
      x.text === y.text &&
      x.pending === y.pending &&
      x.time === y.time &&
      x.meta === y.meta &&
      x.thinking === y.thinking &&
      x.cards === y.cards &&
      x.atts === y.atts
    );
  },
);

export default function App() {
  return (
    <SafeAreaProvider>
      <Root />
    </SafeAreaProvider>
  );
}

function Root() {
  const [gatewayUrl, setGatewayUrl] = useState(envGateway);
  const [token, setToken] = useState(envToken ?? "");
  const [showSettings, setShowSettings] = useState(false);
  const [showSessions, setShowSessions] = useState(false);
  const [showModel, setShowModel] = useState(false);
  const [sessions, setSessions] = useState<SessionSoul[]>([]);
  const [projects, setProjects] = useState<GatewayProject[]>([]);
  const [activeSession, setActiveSession] = useState<SessionSoul | null>(null);
  const [turns, setTurns] = useState<GatewayTurn[]>([]);
  const [model, setModel] = useState<SessionModel>(null);
  const [catalog, setCatalog] = useState<ProviderModels[]>([]);
  const [providerReports, setProviderReports] = useState<
    Record<
      string,
      {
        status: ProviderStatusReport | undefined;
        limits: ProviderLimitsReport | undefined;
      }
    >
  >({});
  const [showRename, setShowRename] = useState(false);
  const [renameDraft, setRenameDraft] = useState("");
  const [picking, setPicking] = useState(false);
  const [catalogHint, setCatalogHint] = useState<string | null>(null);
  const [input, setInput] = useState("");
  const [pendingAtts, setPendingAtts] = useState<PendingAttachment[]>([]);
  const [suggestRows, setSuggestRows] = useState<SuggestRow[]>([]);
  const [suggestBusy, setSuggestBusy] = useState(false);
  const [connecting, setConnecting] = useState(false);
  const [sending, setSending] = useState(false);
  const [refreshing, setRefreshing] = useState(false);
  const [capturing, setCapturing] = useState(false);
  const [note, setNote] = useState<string | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [gatewayProblem, setGatewayProblem] = useState<string | null>(null);
  const [live, setLive] = useState<LiveState>({});
  /* Context-fullness the model reads (session_utilization). Seeded from GET
     /v1/sessions/:sid/context on open, then kept live by the `context.updated`
     SSE event — the SAME canonical mirror the web/TUI context rail shows. */
  const [ctxUtil, setCtxUtil] = useState<CtxUtilization | null>(null);
  /* Settled tool cards per turn_id, rehydrated from GET /turns/:tid/trace so a
     finished turn keeps its op-cards in scrollback (the /turns poll carries no
     trace). traceReqRef dedups in-flight / already-failed fetches. */
  const [traceCache, setTraceCache] = useState<Record<string, LiveCard[]>>({});
  const [notify, setNotify] = useState(true);
  const listRef = useRef<FlatList<Message>>(null);
  const atBottomRef = useRef(true);
  /* On session-open we JUMP to the newest message (no animation through the
     whole backlog) — set by openSession, consumed by onContentSizeChange. */
  const jumpRef = useRef(false);
  const draggingRef = useRef(false);
  /* Read fresh inside the long-lived SSE callback (which only re-subscribes on
     session/client change): the notify pref, foreground state, latest turns
     (for the notification body), and the set of turn_ids already notified. */
  const notifyRef = useRef(true);
  const appActiveRef = useRef(AppState.currentState === "active");
  const turnsRef = useRef<GatewayTurn[]>([]);
  const notifiedRef = useRef<Set<string>>(new Set());
  const traceReqRef = useRef<Set<string>>(new Set());
  const gatewaySheetDismissedRef = useRef(false);

  const client = useMemo(
    () => new VisGatewayClient({ gatewayUrl, token }),
    [gatewayUrl, token],
  );

  useEffect(() => {
    gatewaySheetDismissedRef.current = false;
  }, [gatewayUrl, token]);
  const mention = useMemo(() => activeFileMention(input), [input]);

  const baseMessages = useMemo(() => turns.flatMap(messageText), [turns]);
  const connected = !gatewayProblem && !error && activeSession != null;
  const runningTurn = useMemo(
    () => [...turns].reverse().find((t) => turnLive(t) && (t.id ?? t.turn_id)),
    [turns],
  );

  /* Overlay the running turn's LIVE tool-call cards + streaming prose (reduced
     from the SSE stream) onto its vis bubble. Only the running turn carries an
     overlay; once it settles, the poll's answer_md takes over. */
  const runningId = runningTurn?.id ?? runningTurn?.turn_id;
  const liveTurn = runningId ? live[runningId] : undefined;
  const messages = useMemo(() => {
    const hasTrace = Object.keys(traceCache).length > 0;
    if ((!runningId || !liveTurn) && !hasTrace) return baseMessages;
    const proseText = liveTurn?.prose.trim() ?? "";
    const thinkingText = liveTurn?.thinking.trim() ?? "";
    return baseMessages.map((m) => {
      if (runningId && liveTurn && m.id === `${runningId}-vis`) {
        return {
          ...m,
          text: proseText || m.text,
          pending: m.pending && !proseText,
          cards: liveTurn.cards,
          thinking: thinkingText || undefined,
        };
      }
      /* Settled turn: attach its rehydrated trace cards. Never the running turn
         — that already carries the LIVE cards above. */
      if (hasTrace && m.role === "vis" && m.id.endsWith("-vis")) {
        const tid = m.id.slice(0, -"-vis".length);
        const cards = tid !== runningId ? traceCache[tid] : undefined;
        if (cards && cards.length) return { ...m, cards };
      }
      return m;
    });
  }, [baseMessages, runningId, liveTurn, traceCache]);

  const fail = useCallback(
    (err: unknown) => {
      const msg = gatewayErrorMessage(gatewayUrl, err);
      if (isGatewayConnectionMessage(msg)) {
        setGatewayProblem(msg);
        setError(null);
        setNote(null);
        if (!gatewaySheetDismissedRef.current) setShowSettings(true);
      } else {
        setError(msg);
      }
    },
    [gatewayUrl],
  );

  const refreshTurns = useCallback(
    async (sessionId = activeSession?.id) => {
      if (!sessionId) return;
      const next = await client.listTurns(sessionId);
      /* Keep the previous array identity when the wire payload is unchanged —
         otherwise the 1.8s poll re-renders the list and fights manual scrolling. */
      setTurns((prev) =>
        JSON.stringify(prev) === JSON.stringify(next) ? prev : next,
      );
    },
    [activeSession?.id, client],
  );

  const openSession = useCallback(
    async (session: SessionSoul) => {
      setActiveSession(session);
      setShowSessions(false);
      setTurns([]);
      setTraceCache({});
      traceReqRef.current = new Set();
      /* Re-arm auto-follow + request a one-shot jump so a switched-into session
         lands on its latest message, even if the reader had scrolled up in the
         previous one (atBottomRef otherwise carries the stale `false`). */
      atBottomRef.current = true;
      draggingRef.current = false;
      jumpRef.current = true;
      try {
        setTurns(await client.listTurns(session.id));
        setModel(await client.sessionModel(session.id));
        /* onContentSizeChange (jumpRef) handles the common case, but a windowed
           FlatList mounts its cells async, so a single fire can land before the
           last rows are measured. Re-assert scrollToEnd across a few frames so a
           switched-into session reliably settles on the newest message. */
        for (const ms of [0, 80, 220, 450]) {
          setTimeout(() => {
            if (jumpRef.current || atBottomRef.current) {
              listRef.current?.scrollToEnd({ animated: false });
            }
          }, ms);
        }
      } catch (err) {
        fail(err);
      }
    },
    [client, fail],
  );

  const connect = useCallback(async () => {
    setConnecting(true);
    setError(null);
    try {
      const existing = await client.listSessions();
      const latest = [...existing].sort(
        (a, b) =>
          (b.last_active_at ?? b.created_at ?? 0) -
          (a.last_active_at ?? a.created_at ?? 0),
      )[0];
      const session = latest ?? (await client.createSession());
      setSessions(existing.length ? existing : [session]);
      setGatewayProblem(null);
      gatewaySheetDismissedRef.current = false;
      setNote(null);
      setShowSettings(false);
      await openSession(session);
    } catch (err) {
      fail(err);
    } finally {
      setConnecting(false);
    }
  }, [client, fail, openSession]);

  useEffect(() => {
    void connect();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  useEffect(() => {
    turnsRef.current = turns;
  }, [turns]);

  useEffect(() => {
    notifyRef.current = notify;
  }, [notify]);

  /* Track foreground/background so a completion only pings when the operator
     ISN'T already watching the live stream. */
  useEffect(() => {
    const sub = AppState.addEventListener("change", (state) => {
      appActiveRef.current = state === "active";
    });
    return () => sub.remove();
  }, []);

  /* Ask for notification permission once on mount; a denial just means
     notifyTurnDone silently no-ops (and we reflect it by disabling the pref). */
  useEffect(() => {
    void ensureNotificationPermissions().then((ok) => {
      if (!ok) setNotify(false);
    });
  }, []);

  /* Canonical @-file picker: UI trigger lives here; ranking comes from
     GET /v1/sessions/:sid/suggest, the same gateway service every channel uses. */
  useEffect(() => {
    const sid = activeSession?.id;
    if (!sid || !mention) {
      setSuggestRows([]);
      setSuggestBusy(false);
      return;
    }
    let alive = true;
    setSuggestBusy(true);
    const timer = setTimeout(() => {
      void client
        .suggest(sid, mention.q)
        .then((rows) => {
          if (alive) setSuggestRows(rows.slice(0, 6));
        })
        .catch((err) => {
          if (__DEV__) console.warn("[vis] suggest failed", err);
          if (alive) setSuggestRows([]);
        })
        .finally(() => {
          if (alive) setSuggestBusy(false);
        });
    }, 120);
    return () => {
      alive = false;
      clearTimeout(timer);
    };
  }, [activeSession?.id, client, mention]);

  useEffect(() => {
    if (!activeSession?.id) return;
    const timer = setInterval(() => {
      refreshTurns().catch(fail);
    }, 1800);
    return () => clearInterval(timer);
  }, [activeSession?.id, fail, refreshTurns]);

  /* Rehydrate settled tool cards from each finished multi-iteration turn's
     trace, so scrolled-back turns keep their op-cards (the /turns poll carries
     none). Lazy + cached + deduped; a pre-trace gateway 404s and is ignored. */
  useEffect(() => {
    const sid = activeSession?.id;
    if (!sid) return;
    const need = turns.filter((t) => {
      const tid = t.turn_id ?? t.id;
      return (
        !!tid &&
        !turnLive(t) &&
        (t.iteration_count ?? 0) > 1 &&
        !traceCache[tid] &&
        !traceReqRef.current.has(tid)
      );
    });
    if (!need.length) return;
    let cancelled = false;
    void (async () => {
      for (const t of need) {
        const tid = (t.turn_id ?? t.id) as string;
        traceReqRef.current.add(tid);
        try {
          const cards = traceCards(await client.turnTrace(sid, tid));
          if (!cancelled && cards.length) {
            setTraceCache((prev) => ({ ...prev, [tid]: cards }));
          }
        } catch (err) {
          if (__DEV__) console.warn("[vis] turnTrace failed", err);
        }
      }
    })();
    return () => {
      cancelled = true;
    };
  }, [turns, activeSession?.id, client, traceCache]);

  /* Live SSE overlay: subscribe to the gateway event stream for the active
     session and reduce block.started / block.output / content.delta /
     reasoning.delta into per-turn live state so tool cards stream in real time.
     The 1.8s poll stays as the settled-state source (and SSE fallback). */
  useEffect(() => {
    const sid = activeSession?.id;
    if (!sid) return;
    setLive({});
    notifiedRef.current = new Set();
    /* Native SSE (react-native-sse) owns the socket AND reconnection: on a drop
       it re-requests with Last-Event-ID and the gateway resumes losslessly, so
       there is no app-level retry. cursor=0 on first connect replays the
       in-flight turn; the reducer is idempotent (cards upsert by (iteration,
       block), prose is a whole-tail replace) so any replay never double-renders.

       Failures were previously SILENT (no onError/onOpen wired), so a stream that
       never opened — unreachable gateway, 401, a module that never bundled — read
       on-device as "cards just don't stream" with zero signal. Surface every
       lifecycle now: onError sets a visible note (and logs), onOpen clears it. A
       throw from streamEvents itself (constructor / missing native module) is
       caught here so the transcript still polls instead of the effect dying. */
    let close: (() => void) | undefined;
    try {
      close = client.streamEvents(
        sid,
        {
          onOpen: () => {
            if (__DEV__) console.log("[vis] SSE open", sid);
            setNote((n) => (n && n.startsWith("live stream") ? null : n));
          },
          onEvent: (ev) => {
            setLive((prev) => reduceLiveEvent(prev, ev));
            /* live context-fullness — context.updated carries the fresh utilization */
            if (ev.type === "context.updated" && ev.utilization)
              setCtxUtil(ev.utilization);
            /* fire a local notification on turn completion when backgrounded */
            const tid = ev.turn_id;
            if (
              tid &&
              !notifiedRef.current.has(tid) &&
              shouldNotifyTurnDone({
                type: ev.type,
                enabled: notifyRef.current,
                appActive: appActiveRef.current,
              })
            ) {
              notifiedRef.current.add(tid);
              void notifyTurnDone({
                failed: ev.type === "turn.failed",
                sessionTitle: activeSession?.title,
                request: turnsRef.current.find(
                  (t) => (t.id ?? t.turn_id) === tid,
                )?.request,
              });
            }
          },
          onError: (err) => {
            const status = (err as { xhrStatus?: number })?.xhrStatus;
            const msg = status
              ? `HTTP ${status}`
              : gatewayErrorMessage(gatewayUrl, err) || "connection lost";
            if (__DEV__) console.warn("[vis] SSE error", err);
            if (isGatewayConnectionMessage(msg)) {
              setGatewayProblem(msg);
              if (!gatewaySheetDismissedRef.current) setShowSettings(true);
              setNote(null);
            } else {
              setNote(`live stream: ${msg} (retrying…)`);
            }
          },
        },
        0,
      );
    } catch (err) {
      const msg = gatewayErrorMessage(gatewayUrl, err);
      if (__DEV__) console.warn("[vis] SSE failed to start", err);
      if (isGatewayConnectionMessage(msg)) {
        setGatewayProblem(msg);
        if (!gatewaySheetDismissedRef.current) setShowSettings(true);
        setNote(null);
      } else {
        setNote(`live stream unavailable: ${msg}`);
      }
    }
    return () => close?.();
  }, [activeSession?.id, client]);

  /* Seed the context-fullness rail on session open (the live context.updated
     event keeps it fresh after). A pre-context gateway 404s → clear, never throw. */
  useEffect(() => {
    const sid = activeSession?.id;
    setCtxUtil(null);
    if (!sid) return;
    let alive = true;
    void client
      .sessionContext(sid)
      .then((snap) => {
        if (alive) setCtxUtil(snap.session_utilization ?? null);
      })
      .catch((err) => {
        if (__DEV__) console.warn("[vis] sessionContext failed", err);
      });
    return () => {
      alive = false;
    };
  }, [activeSession?.id, client]);

  /* Auto-follow lives on the FlatList's onContentSizeChange: it fires for new
     messages AND for a streaming answer growing, and it is suppressed while the
     reader's finger is down — a poll landing mid-gesture must never yank. */

  const refreshSessions = useCallback(async () => {
    try {
      const ss = await client.listSessions();
      setSessions(ss);
      /* Session projects are a newer gateway feature — an older/stale gateway 404s
         /v1/projects. Never let that blank the sessions list; fall back to no
         projects so the drawer still shows every session. */
      try {
        setProjects(await client.listProjects());
      } catch (projectErr) {
        if (__DEV__) console.warn("[vis] listProjects failed", projectErr);
        setProjects([]);
      }
    } catch (err) {
      fail(err);
    }
  }, [client, fail]);

  const onPullRefresh = useCallback(async () => {
    setRefreshing(true);
    try {
      await refreshTurns();
      setError(null);
    } catch (err) {
      fail(err);
    } finally {
      setRefreshing(false);
    }
  }, [fail, refreshTurns]);

  const send = useCallback(async () => {
    const text = input.trim();
    const atts = pendingAtts;
    if ((!text && !atts.length) || !activeSession) return;
    setInput("");
    setPendingAtts([]);
    setSending(true);
    setError(null);
    setNote(null);
    try {
      await client.submitTurn(activeSession.id, text, atts);
      await refreshTurns(activeSession.id);
    } catch (err) {
      fail(err);
      setInput(text);
      setPendingAtts(atts);
    } finally {
      setSending(false);
    }
  }, [activeSession, client, fail, input, pendingAtts, refreshTurns]);

  const addAttachments = useCallback(
    async (pick: () => Promise<PendingAttachment[]>) => {
      try {
        const picked = await pick();
        if (picked.length) setPendingAtts((prev) => [...prev, ...picked]);
      } catch (err) {
        setNote(err instanceof Error ? err.message : String(err));
      }
    },
    [],
  );

  const cancelRunning = useCallback(async () => {
    const tid = runningTurn?.id ?? runningTurn?.turn_id;
    if (!activeSession || !tid) return;
    try {
      await client.cancelTurn(activeSession.id, tid);
      await refreshTurns(activeSession.id);
    } catch (err) {
      fail(err);
    }
  }, [activeSession, client, fail, refreshTurns, runningTurn]);

  const createSession = useCallback(async () => {
    setConnecting(true);
    setError(null);
    try {
      const session = await client.createSession();
      setSessions((prev) => [session, ...prev]);
      await openSession(session);
    } catch (err) {
      fail(err);
    } finally {
      setConnecting(false);
    }
  }, [client, fail, openSession]);

  const deleteSession = useCallback(
    async (session: SessionSoul) => {
      try {
        await client.deleteSession(session.id);
        const rest = sessions.filter((s) => s.id !== session.id);
        setSessions(rest);
        if (activeSession?.id === session.id) {
          const next = rest[0] ?? (await client.createSession());
          if (!rest.length) setSessions([next]);
          await openSession(next);
        }
      } catch (err) {
        fail(err);
      }
    },
    [activeSession?.id, client, fail, openSession, sessions],
  );

  const renameSession = useCallback(
    async (session: SessionSoul, title: string) => {
      try {
        const soul = await client.renameSession(session.id, title);
        setSessions((prev) =>
          prev.map((s) => (s.id === session.id ? { ...s, ...soul } : s)),
        );
        if (activeSession?.id === session.id)
          setActiveSession((prev) => (prev ? { ...prev, title } : prev));
      } catch (err) {
        fail(err);
      }
    },
    [activeSession?.id, client, fail],
  );

  /* ── projects / projects ───────────────────── */

  const createProject = useCallback(
    async (name: string) => {
      try {
        const g = await client.createProject(name);
        setProjects((prev) => [...prev, g]);
      } catch (err) {
        fail(err);
      }
    },
    [client, fail],
  );

  const renameProject = useCallback(
    async (project: GatewayProject, name: string) => {
      try {
        const g = await client.updateProject(project.id, { name });
        setProjects((prev) =>
          prev.map((x) => (x.id === project.id ? { ...x, ...g } : x)),
        );
        setSessions((prev) =>
          prev.map((s) =>
            s.project_id === project.id ? { ...s, project_name: name } : s,
          ),
        );
      } catch (err) {
        fail(err);
      }
    },
    [client, fail],
  );

  const deleteProject = useCallback(
    async (project: GatewayProject) => {
      try {
        await client.deleteProject(project.id);
        setProjects((prev) => prev.filter((x) => x.id !== project.id));
        /* members scatter back to projectless — clear their membership locally */
        setSessions((prev) =>
          prev.map((s) =>
            s.project_id === project.id
              ? { ...s, project_id: null, project_name: null }
              : s,
          ),
        );
      } catch (err) {
        fail(err);
      }
    },
    [client, fail],
  );

  const assignProject = useCallback(
    async (session: SessionSoul, projectId: string | null) => {
      /* optimistic: reflect membership + bump both projects' counts at once */
      setSessions((prev) =>
        prev.map((s) =>
          s.id === session.id
            ? {
                ...s,
                project_id: projectId,
                project_name:
                  projects.find((g) => g.id === projectId)?.name ?? null,
              }
            : s,
        ),
      );
      try {
        const soul = await client.assignProject(session.id, projectId);
        setSessions((prev) =>
          prev.map((s) => (s.id === session.id ? { ...s, ...soul } : s)),
        );
      } catch (err) {
        fail(err);
        void refreshSessions();
      }
    },
    [client, fail, projects, refreshSessions],
  );

  /* Move a session up/down within its project's manual order (movable tabs).
     Optimistically re-numbers project_position, then persists the full order. */
  const reorderProject = useCallback(
    async (session: SessionSoul, dir: "up" | "down") => {
      const pid = session.project_id;
      if (!pid) return;
      const members = sessions
        .filter((s) => s.project_id === pid)
        .sort(
          (a, b) =>
            (a.project_position ?? 0) - (b.project_position ?? 0) ||
            (b.last_active_at ?? 0) - (a.last_active_at ?? 0),
        );
      const idx = members.findIndex((s) => s.id === session.id);
      const swapWith = dir === "up" ? idx - 1 : idx + 1;
      if (idx < 0 || swapWith < 0 || swapWith >= members.length) return;
      const reordered = [...members];
      const a = reordered[idx]!;
      reordered[idx] = reordered[swapWith]!;
      reordered[swapWith] = a;
      const order = reordered.map((s) => s.id);
      const posById = new Map(order.map((id, i) => [id, i] as const));
      setSessions((prev) =>
        prev.map((s) =>
          posById.has(s.id)
            ? { ...s, project_position: posById.get(s.id)! }
            : s,
        ),
      );
      try {
        await client.reorderProjectSessions(pid, order);
      } catch (err) {
        fail(err);
        void refreshSessions();
      }
    },
    [client, fail, sessions, refreshSessions],
  );

  const openModelDialog = useCallback(async () => {
    setShowModel(true);
    setProviderReports({});
    try {
      setModel(
        activeSession ? await client.sessionModel(activeSession.id) : null,
      );
    } catch (err) {
      fail(err);
    }
    try {
      const catalog = await client.providerCatalog();
      setCatalog(catalog);
      /* Provider status/limits are canonical gateway reports too; load them
         opportunistically so the mobile picker shows the SAME health/quota
         hints as the desktop provider panes. Never block choosing a model. */
      const reports = await Promise.all(
        catalog.map(async (p) => {
          const [status, limits] = await Promise.all([
            client.providerStatus(p.id).catch(() => undefined),
            client.providerLimits(p.id).catch(() => undefined),
          ]);
          return [p.id, { status, limits }] as const;
        }),
      );
      setProviderReports(Object.fromEntries(reports));
      /* An older running gateway serves /v1/models WITHOUT the catalog key. */
      setCatalogHint(
        catalog.length > 0
          ? null
          : "No models in the gateway catalog \u2014 if providers are configured, this gateway predates the /v1/models catalog; restart the vis gateway.",
      );
    } catch {
      setCatalog([]);
      setProviderReports({});
      setCatalogHint("Could not load the model catalog from the gateway.");
    }
  }, [activeSession, client, fail]);

  /* Tap a chip → the session routes through it immediately (blank pair
     resets to the router default) — the web picker's exact behavior. */
  const pickModel = useCallback(
    async (provider: string, modelName: string) => {
      if (!activeSession || picking) return;
      setPicking(true);
      try {
        setModel(
          await client.setSessionModel(activeSession.id, provider, modelName),
        );
      } catch (err) {
        fail(err);
      } finally {
        setPicking(false);
      }
    },
    [activeSession, client, fail, picking],
  );

  const openRenameDialog = useCallback(() => {
    if (!activeSession) return;
    setRenameDraft(activeSession.title?.trim() ?? "");
    setShowRename(true);
  }, [activeSession]);

  const applyRename = useCallback(async () => {
    if (!activeSession || !renameDraft.trim()) return;
    await renameSession(activeSession, renameDraft.trim());
    setShowRename(false);
  }, [activeSession, renameDraft, renameSession]);

  const currentPick =
    model && typeof model !== "string"
      ? { provider: model.provider ?? "", model: model.model ?? "" }
      : null;

  const gatewayHost = gatewayUrl
    .replace(/^https?:\/\//, "")
    .replace(/\/+$/, "");
  const canSend =
    connected &&
    !sending &&
    (input.trim().length > 0 || pendingAtts.length > 0);

  return (
    <View style={styles.safe}>
      <SafeAreaView style={styles.flex} edges={["top", "bottom"]}>
        <StatusBar barStyle="dark-content" backgroundColor={c.paper} />
        <KeyboardAvoidingView
          behavior={Platform.select({ ios: "padding", default: undefined })}
          style={styles.flex}
        >
          {/* ── header: flat, one hairline under it ─────────────────── */}
          <View style={styles.header}>
            <IconBtn
              name="sidebar"
              onPress={() => {
                void refreshSessions();
                setShowSessions(true);
              }}
            />
            <Pressable
              onPress={openRenameDialog}
              style={styles.barTitle}
              hitSlop={4}
            >
              <Text numberOfLines={1} style={styles.barName}>
                {activeSession?.title?.trim() || "Untitled"}
              </Text>
              <Feather name="edit-2" size={11} color={c.tsep} />
            </Pressable>
            <IconBtn
              name="settings"
              onPress={() => {
                gatewaySheetDismissedRef.current = false;
                setShowSettings(true);
              }}
              active={showSettings}
            />
          </View>

          {/* ── transcript: flat rows on the warm ground ─────────────── */}
          <FlatList
            ref={listRef}
            data={messages}
            keyExtractor={(m) => m.id}
            style={styles.flex}
            contentContainerStyle={styles.transcript}
            onScroll={(e) => {
              const { contentOffset, layoutMeasurement, contentSize } =
                e.nativeEvent;
              atBottomRef.current =
                contentOffset.y + layoutMeasurement.height >=
                contentSize.height - 48;
            }}
            onScrollBeginDrag={() => {
              draggingRef.current = true;
            }}
            onScrollEndDrag={() => {
              draggingRef.current = false;
            }}
            onMomentumScrollEnd={() => {
              draggingRef.current = false;
            }}
            onContentSizeChange={() => {
              /* Session-open: jump straight to the newest message, no animation
               through the backlog. Otherwise follow a growing/streaming answer,
               but never while the reader's finger is down. */
              if (jumpRef.current && messages.length) {
                jumpRef.current = false;
                listRef.current?.scrollToEnd({ animated: false });
              } else if (atBottomRef.current && !draggingRef.current) {
                listRef.current?.scrollToEnd({ animated: true });
              }
            }}
            scrollEventThrottle={16}
            removeClippedSubviews={Platform.OS === "android"}
            initialNumToRender={12}
            maxToRenderPerBatch={10}
            windowSize={11}
            updateCellsBatchingPeriod={40}
            keyboardShouldPersistTaps="handled"
            keyboardDismissMode="on-drag"
            refreshControl={
              <RefreshControl
                refreshing={refreshing}
                onRefresh={() => void onPullRefresh()}
                tintColor={c.amber}
              />
            }
            renderItem={({ item }) => <MessageRow item={item} />}
            ListEmptyComponent={
              <View style={styles.emptyWrap}>
                {connecting ? (
                  <ActivityIndicator color={c.amber} />
                ) : (
                  <>
                    <Text style={styles.emptyTitle}>
                      {connected ? "fresh session" : "not connected"}
                    </Text>
                    <Text style={styles.emptyBody}>
                      {connected
                        ? "Say something \u2014 vis is listening."
                        : "Open Settings and point me at a running vis gateway."}
                    </Text>
                  </>
                )}
              </View>
            }
          />

          {/* ── mic / error note ───────────────────────────────────── */}
          {note ? (
            <Pressable onPress={() => setNote(null)} style={styles.note}>
              <Feather name="info" size={12} color={c.chipInk} />
              <Text style={styles.noteText}>{note}</Text>
            </Pressable>
          ) : null}

          {mention && (suggestRows.length > 0 || suggestBusy) ? (
            <View style={styles.suggestBox}>
              <View style={styles.suggestHead}>
                <Feather name="at-sign" size={12} color={c.amberDeep} />
                <Text style={styles.suggestTitle}>file suggestions</Text>
                {suggestBusy ? (
                  <ActivityIndicator size="small" color={c.amber} />
                ) : null}
              </View>
              {suggestRows.map((row) => (
                <Pressable
                  key={row.name}
                  onPress={() => {
                    setInput(applyFileMention(input, mention, row));
                    setSuggestRows([]);
                  }}
                  style={styles.suggestRow}
                >
                  <Text numberOfLines={1} style={styles.suggestName}>
                    {row.name}
                  </Text>
                  {suggestLabel(row) ? (
                    <Text numberOfLines={1} style={styles.suggestMeta}>
                      {suggestLabel(row)}
                    </Text>
                  ) : null}
                </Pressable>
              ))}
            </View>
          ) : null}

          {/* ── attachment tray + flat composer ─────────────────────── */}
          <AttachmentTray
            items={pendingAtts}
            onRemove={(key) =>
              setPendingAtts((prev) => prev.filter((a) => a.key !== key))
            }
          />
          <View style={styles.composer}>
            {!capturing ? (
              <>
                <Text style={styles.prompt}>{"\u203a"}</Text>
                <TextInput
                  value={input}
                  onChangeText={setInput}
                  placeholder={
                    connected ? "message vis\u2026" : "connect first\u2026"
                  }
                  placeholderTextColor={c.tsep}
                  style={styles.input}
                  editable={connected && !sending}
                  multiline
                  onSubmitEditing={() => void send()}
                />
                <IconBtn
                  name="paperclip"
                  size={16}
                  color={c.dim}
                  disabled={!connected}
                  onPress={() => void addAttachments(pickFiles)}
                />
                <IconBtn
                  name="image"
                  size={16}
                  color={c.dim}
                  disabled={!connected}
                  onPress={() => void addAttachments(pickImages)}
                />
              </>
            ) : null}
            <VoiceButton
              client={client}
              sessionId={activeSession?.id ?? null}
              onTranscript={(text) =>
                setInput((prev) => (prev ? `${prev.trimEnd()} ${text}` : text))
              }
              onNote={setNote}
              onPhase={setCapturing}
            />
            {!capturing ? (
              runningTurn ? (
                <Pressable
                  onPress={() => void cancelRunning()}
                  style={[styles.sendBtn, styles.stopBtn]}
                  hitSlop={6}
                >
                  <Feather name="square" size={12} color="#FFFFFF" />
                </Pressable>
              ) : (
                <Pressable
                  onPress={() => void send()}
                  disabled={!canSend}
                  style={[styles.sendBtn, !canSend && { opacity: 0.35 }]}
                  hitSlop={6}
                >
                  {sending ? (
                    <ActivityIndicator size="small" color={c.amberInk} />
                  ) : (
                    <Feather name="arrow-up" size={15} color={c.amberInk} />
                  )}
                </Pressable>
              )
            ) : null}
          </View>

          {/* ── footer: one mono status line, TUI-style ──────────────── */}
          <View style={styles.footer}>
            <Pressable
              onPress={() => void openModelDialog()}
              style={styles.footRouting}
              hitSlop={6}
            >
              <Feather name="zap" size={11} color={c.amberDeep} />
              <Text numberOfLines={1} style={styles.footRoutingName}>
                {modelLabel(model)}
              </Text>
            </Pressable>
            {ctxPct(ctxUtil) != null ? (
              <Text style={styles.ctxChip}>{`ctx ${ctxPct(ctxUtil)}%`}</Text>
            ) : null}
            <Text numberOfLines={1} style={styles.footerText}>
              {gatewayHost}
              {activeSession ? ` \u00b7 ${shortId(activeSession.id)}` : ""}
            </Text>
          </View>
        </KeyboardAvoidingView>
      </SafeAreaView>

      {/* ── sessions drawer ──────────────────────────────────────── */}
      <SessionsDrawer
        visible={showSessions}
        sessions={sessions}
        projects={projects}
        activeId={activeSession?.id ?? null}
        onClose={() => setShowSessions(false)}
        onSelect={(s) => void openSession(s)}
        onCreate={() => {
          setShowSessions(false);
          void createSession();
        }}
        onDelete={(s) => void deleteSession(s)}
        onRename={(s, title) => void renameSession(s, title)}
        onCreateProject={(name) => void createProject(name)}
        onRenameProject={(g, name) => void renameProject(g, name)}
        onDeleteProject={(g) => void deleteProject(g)}
        onAssign={(s, gid) => void assignProject(s, gid)}
        onReorder={(s, dir) => void reorderProject(s, dir)}
      />

      {/* ── settings — the web channel's Settings dialog, native ──── */}
      <DialogModal
        visible={showSettings}
        title={gatewayProblem ? "Connect gateway" : "Settings"}
        onClose={() => {
          if (gatewayProblem) gatewaySheetDismissedRef.current = true;
          setShowSettings(false);
        }}
        dismissable
        fullScreen
        flush
      >
        <SettingsPane
          client={client}
          gatewayUrl={gatewayUrl}
          token={token}
          connecting={connecting}
          connectionProblem={gatewayProblem}
          onGatewayUrl={setGatewayUrl}
          onToken={setToken}
          onReconnect={() => void connect()}
          sessionId={activeSession?.id ?? null}
          notify={notify}
          onNotify={(v) => {
            setNotify(v);
            if (v)
              void ensureNotificationPermissions().then((ok) => setNotify(ok));
          }}
        />
      </DialogModal>

      {/* ── session model picker — the web picker, native ─────────── */}
      <DialogModal
        visible={showModel}
        title="Session model"
        onClose={() => setShowModel(false)}
      >
        <ScrollView
          style={styles.modelScroll}
          contentContainerStyle={styles.modelScrollBody}
        >
          <Text style={styles.activeModel}>
            This session:{" "}
            <Text style={styles.activeModelStrong}>{modelLabel(model)}</Text>
          </Text>
          <View style={styles.modelChips}>
            <Pressable
              onPress={() => void pickModel("", "")}
              style={[
                styles.modelChip,
                !currentPick?.model && styles.modelChipOn,
              ]}
            >
              <Feather
                name={!currentPick?.model ? "check" : "star"}
                size={11}
                color={!currentPick?.model ? "#FFFFFF" : c.dim}
              />
              <Text
                style={[
                  styles.modelChipText,
                  !currentPick?.model && styles.modelChipTextOn,
                ]}
              >
                router default
              </Text>
            </Pressable>
          </View>
          {catalog.length === 0 ? (
            <Text style={styles.emptyBody}>
              {catalogHint ?? "No providers configured yet."}
            </Text>
          ) : (
            catalog.map((p) => {
              const report = providerReportText(providerReports[p.id]);
              return (
                <View key={p.id} style={styles.modelGroup}>
                  <Text style={styles.modelGroupLabel}>{p.label ?? p.id}</Text>
                  {report ? (
                    <Text style={styles.modelGroupMeta}>{report}</Text>
                  ) : null}
                  <View style={styles.modelChips}>
                    {p.models.map((name) => {
                      const on =
                        currentPick?.provider === p.id &&
                        currentPick?.model === name;
                      return (
                        <Pressable
                          key={name}
                          onPress={() => void pickModel(p.id, name)}
                          style={[styles.modelChip, on && styles.modelChipOn]}
                        >
                          {on ? (
                            <Feather name="check" size={11} color="#FFFFFF" />
                          ) : null}
                          <Text
                            style={[
                              styles.modelChipText,
                              on && styles.modelChipTextOn,
                            ]}
                          >
                            {name}
                          </Text>
                        </Pressable>
                      );
                    })}
                  </View>
                </View>
              );
            })
          )}
        </ScrollView>
      </DialogModal>

      {/* ── rename session ───────────────────────────────────────── */}
      <DialogModal
        visible={showRename}
        title="Rename session"
        onClose={() => setShowRename(false)}
      >
        <TextInput
          value={renameDraft}
          onChangeText={setRenameDraft}
          autoFocus
          autoCorrect={false}
          placeholder="session title"
          placeholderTextColor={c.tsep}
          style={styles.field}
          onSubmitEditing={() => void applyRename()}
        />
        <ActionBtn
          label="Save"
          tone="amber"
          disabled={!renameDraft.trim()}
          onPress={() => void applyRename()}
        />
      </DialogModal>

      {/* ── error dialog ─────────────────────────────────────────── */}
      <DialogModal
        visible={error != null}
        title="Problem"
        tone="error"
        onClose={() => setError(null)}
      >
        <Text style={styles.errorText}>{error}</Text>
        <ActionBtn
          label="Dismiss"
          tone="ghost"
          onPress={() => setError(null)}
        />
      </DialogModal>
    </View>
  );
}

const styles = StyleSheet.create({
  safe: { flex: 1, backgroundColor: c.paper },
  flex: { flex: 1 },
  header: {
    flexDirection: "row",
    alignItems: "center",
    gap: 8,
    paddingHorizontal: 10,
    paddingVertical: 5,
    borderBottomWidth: StyleSheet.hairlineWidth,
    borderBottomColor: c.hair,
    backgroundColor: c.paper,
  },
  barTitle: {
    flex: 1,
    flexDirection: "row",
    alignItems: "center",
    gap: 6,
    minWidth: 0,
  },
  barName: {
    flexShrink: 1,
    fontFamily: mono,
    fontSize: 13,
    fontWeight: "700",
    color: c.ink,
  },
  transcript: {
    paddingHorizontal: 12,
    paddingTop: 10,
    paddingBottom: 40,
    gap: 14,
    flexGrow: 1,
  },
  msg: { gap: 5 },
  msgHead: { flexDirection: "row", alignItems: "center", gap: 7 },
  gutter: { width: 3, height: 12 },
  msgRole: {
    fontFamily: mono,
    fontSize: 10,
    fontWeight: "700",
    letterSpacing: 1,
  },
  headRule: {
    flex: 1,
    height: StyleSheet.hairlineWidth,
    backgroundColor: c.hair,
  },
  msgTime: { fontFamily: mono, fontSize: 9, color: c.tsep },
  attRow: { flexDirection: "row", flexWrap: "wrap", gap: 6 },
  attImg: { width: 96, height: 96, backgroundColor: c.tsepBg },
  msgMeta: { fontFamily: mono, fontSize: 9.5, color: c.dim, marginTop: 2 },
  pending: { fontFamily: mono, fontSize: 12, color: c.dim },
  thinking: {
    fontFamily: mono,
    fontSize: 11,
    lineHeight: 16,
    color: c.dim,
    fontStyle: "italic",
  },
  emptyWrap: {
    flex: 1,
    alignItems: "center",
    justifyContent: "center",
    gap: 6,
    paddingTop: 80,
  },
  emptyTitle: {
    fontFamily: mono,
    fontSize: 13,
    fontWeight: "700",
    color: c.ink,
  },
  emptyBody: {
    fontSize: 12,
    color: c.dim,
    textAlign: "center",
    paddingHorizontal: 40,
  },
  note: {
    flexDirection: "row",
    alignItems: "center",
    gap: 6,
    marginHorizontal: 12,
    marginBottom: 6,
    backgroundColor: c.chipBg,
    paddingHorizontal: 10,
    paddingVertical: 7,
  },
  noteText: { flex: 1, fontFamily: mono, fontSize: 11.5, color: c.chipInk },
  suggestBox: {
    marginHorizontal: 12,
    marginBottom: 6,
    borderWidth: 1,
    borderColor: c.lineSoft,
    backgroundColor: c.field,
    paddingHorizontal: 10,
    paddingVertical: 7,
    gap: 6,
  },
  suggestHead: { flexDirection: "row", alignItems: "center", gap: 6 },
  suggestTitle: {
    flex: 1,
    fontFamily: mono,
    fontSize: 10,
    color: c.amberDeep,
    textTransform: "uppercase",
  },
  suggestRow: {
    gap: 2,
    paddingVertical: 5,
    borderTopWidth: StyleSheet.hairlineWidth,
    borderTopColor: c.lineSoft,
  },
  suggestName: { fontFamily: mono, fontSize: 12, color: c.ink },
  suggestMeta: { fontSize: 10, color: c.dim },
  composer: {
    flexDirection: "row",
    alignItems: "flex-end",
    gap: 2,
    paddingLeft: 12,
    paddingRight: 8,
    paddingTop: 6,
    paddingBottom: 4,
    borderTopWidth: StyleSheet.hairlineWidth,
    borderTopColor: c.hair,
    backgroundColor: c.paper,
  },
  prompt: {
    fontFamily: mono,
    fontSize: 16,
    fontWeight: "700",
    color: c.accent,
    lineHeight: 30,
    marginRight: 4,
  },
  input: {
    flex: 1,
    minHeight: 30,
    maxHeight: 110,
    color: c.ink,
    fontSize: 13,
    paddingVertical: 6,
    paddingHorizontal: 0,
    backgroundColor: "transparent",
  },
  sendBtn: {
    width: 30,
    height: 30,
    alignItems: "center",
    justifyContent: "center",
    backgroundColor: c.accent,
  },
  stopBtn: { backgroundColor: c.err },
  footer: {
    flexDirection: "row",
    alignItems: "center",
    gap: 8,
    paddingHorizontal: 12,
    paddingVertical: 4,
  },
  ctxChip: {
    fontFamily: mono,
    fontSize: 10,
    color: c.dim,
    borderWidth: 1,
    borderColor: c.lineSoft,
    borderRadius: 4,
    paddingHorizontal: 5,
    paddingVertical: 2,
  },
  footRouting: {
    flexDirection: "row",
    alignItems: "center",
    gap: 5,
    maxWidth: 230,
    borderWidth: StyleSheet.hairlineWidth,
    borderColor: c.lineSoft,
    borderRadius: 11,
    backgroundColor: "#FFFFFF",
    paddingHorizontal: 9,
    paddingVertical: 5,
  },
  footRoutingName: { fontFamily: mono, fontSize: 10, color: c.accentInk },
  footerText: {
    flex: 1,
    textAlign: "right",
    fontFamily: mono,
    fontSize: 10,
    color: c.dim,
  },
  fieldLabel: {
    fontFamily: mono,
    fontSize: 10.5,
    letterSpacing: 1,
    textTransform: "uppercase",
    color: c.dim,
  },
  field: {
    borderWidth: 1,
    borderColor: c.line,
    backgroundColor: c.field,
    color: c.ink,
    fontFamily: mono,
    fontSize: 12,
    paddingHorizontal: 10,
    paddingVertical: 6,
  },
  modelScroll: { maxHeight: 500 },
  modelScrollBody: { gap: 16, paddingBottom: 6 },
  activeModel: { fontSize: 13, color: c.dim, lineHeight: 18 },
  activeModelStrong: { fontFamily: mono, fontWeight: "700", color: c.ink },
  modelGroup: {
    gap: 10,
    backgroundColor: "#FFFFFF",
    borderRadius: 18,
    borderWidth: StyleSheet.hairlineWidth,
    borderColor: c.lineSoft,
    padding: 14,
  },
  modelGroupLabel: {
    fontFamily: mono,
    fontSize: 10.5,
    letterSpacing: 1,
    textTransform: "uppercase",
    color: c.dim,
  },
  modelGroupMeta: { fontSize: 10.5, color: c.dim, marginTop: -4 },
  modelChips: { flexDirection: "row", flexWrap: "wrap", gap: 8 },
  modelChip: {
    flexDirection: "row",
    alignItems: "center",
    gap: 5,
    borderWidth: StyleSheet.hairlineWidth,
    borderColor: c.lineSoft,
    borderRadius: 13,
    backgroundColor: "#FFFFFF",
    paddingHorizontal: 12,
    paddingVertical: 9,
  },
  modelChipOn: { backgroundColor: c.accent, borderColor: c.accent },
  modelChipText: { fontFamily: mono, fontSize: 12, color: c.ink },
  modelChipTextOn: { color: "#FFFFFF", fontWeight: "700" },
  errorText: { color: c.ink, fontSize: 13, lineHeight: 18 },
});
