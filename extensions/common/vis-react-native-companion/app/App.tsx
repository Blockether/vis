import React, { memo, useCallback, useEffect, useMemo, useRef, useState } from "react";
import {
  ActivityIndicator,
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
  View
} from "react-native";
import { SafeAreaProvider, SafeAreaView } from "react-native-safe-area-context";
import { Feather } from "@expo/vector-icons";

import {
  GatewayTurn,
  ProviderModels,
  SessionModel,
  SessionSoul,
  TurnAttachment,
  VisGatewayClient
} from "./src/VisClient";
import { c, mono, shortId, timeHHMM } from "./src/theme";
import { ActionBtn, DialogModal, IconBtn } from "./src/ui";
import { SettingsPane } from "./src/Settings";
import { Markdown } from "./src/Markdown";
import { LiveCard, LiveCards, LiveState, reduceLiveEvent } from "./src/LiveTurns";
import { SessionsDrawer } from "./src/SessionsDrawer";
import { VoiceButton } from "./src/VoiceButton";
import {
  AttachmentTray,
  PendingAttachment,
  pickFiles,
  pickImages
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
  if ((turn.iteration_count ?? 0) > 1) parts.push(`${turn.iteration_count} iter`);
  return parts.length ? parts.join(" · ") : undefined;
};

const imageAtts = (turn: GatewayTurn): TurnAttachment[] =>
  (turn.attachments ?? []).filter(
    (a) => a.base64 && (a.kind === "image" || (a.media_type ?? "").startsWith("image/"))
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
    !answer && !live && turn.status && turn.status !== "done" && turn.status !== "completed"
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
            atts: atts.length ? atts : undefined
          }
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
            meta: live ? undefined : turnMeta(turn)
          }
        ]
      : [])
  ];
};

const modelLabel = (model: SessionModel): string => {
  if (!model) return "router default";
  if (typeof model === "string") return model;
  if (model.provider && model.model) return `${model.provider} / ${model.model}`;
  return model.model ?? model.provider ?? "router default";
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
const MessageRow = memo(({ item }: { item: Message }) => {
  const roleColor = item.role === "user" ? c.roleUser : c.roleVis;
  return (
    <View style={styles.msg}>
      <View style={styles.msgHead}>
        <View style={[styles.gutter, { backgroundColor: roleColor }]} />
        <Text style={[styles.msgRole, { color: item.role === "user" ? c.amberDeep : c.roleVis }]}>
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
              source={{ uri: `data:${a.media_type ?? "image/png"};base64,${a.base64}` }}
              style={styles.attImg}
              resizeMode="cover"
            />
          ))}
        </View>
      ) : null}
      {item.thinking ? <Text style={styles.thinking}>{`\u2731 ${item.thinking}`}</Text> : null}
      {item.cards?.length ? <LiveCards cards={item.cards} /> : null}
      {item.text ? (
        <Markdown text={item.text} />
      ) : item.pending && !item.cards?.length && !item.thinking ? (
        <PendingDots />
      ) : null}
      {!item.pending && item.meta ? <Text style={styles.msgMeta}>{item.meta}</Text> : null}
    </View>
  );
}, (a, b) => {
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
});

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
  const [activeSession, setActiveSession] = useState<SessionSoul | null>(null);
  const [turns, setTurns] = useState<GatewayTurn[]>([]);
  const [model, setModel] = useState<SessionModel>(null);
  const [catalog, setCatalog] = useState<ProviderModels[]>([]);
  const [showRename, setShowRename] = useState(false);
  const [renameDraft, setRenameDraft] = useState("");
  const [picking, setPicking] = useState(false);
  const [catalogHint, setCatalogHint] = useState<string | null>(null);
  const [input, setInput] = useState("");
  const [pendingAtts, setPendingAtts] = useState<PendingAttachment[]>([]);
  const [connecting, setConnecting] = useState(false);
  const [sending, setSending] = useState(false);
  const [refreshing, setRefreshing] = useState(false);
  const [capturing, setCapturing] = useState(false);
  const [note, setNote] = useState<string | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [live, setLive] = useState<LiveState>({});
  const listRef = useRef<FlatList<Message>>(null);
  const atBottomRef = useRef(true);
  /* On session-open we JUMP to the newest message (no animation through the
     whole backlog) — set by openSession, consumed by onContentSizeChange. */
  const jumpRef = useRef(false);
  const draggingRef = useRef(false);

  const client = useMemo(() => new VisGatewayClient({ gatewayUrl, token }), [gatewayUrl, token]);

  const baseMessages = useMemo(() => turns.flatMap(messageText), [turns]);
  const connected = !error && activeSession != null;
  const runningTurn = useMemo(
    () => [...turns].reverse().find((t) => turnLive(t) && (t.id ?? t.turn_id)),
    [turns]
  );

  /* Overlay the running turn's LIVE tool-call cards + streaming prose (reduced
     from the SSE stream) onto its vis bubble. Only the running turn carries an
     overlay; once it settles, the poll's answer_md takes over. */
  const runningId = runningTurn?.id ?? runningTurn?.turn_id;
  const liveTurn = runningId ? live[runningId] : undefined;
  const messages = useMemo(() => {
    if (!runningId || !liveTurn) return baseMessages;
    const proseText = liveTurn.prose.trim();
    const thinkingText = liveTurn.thinking.trim();
    return baseMessages.map((m) =>
      m.id === `${runningId}-vis`
        ? {
            ...m,
            text: proseText || m.text,
            pending: m.pending && !proseText,
            cards: liveTurn.cards,
            thinking: thinkingText || undefined
          }
        : m
    );
  }, [baseMessages, runningId, liveTurn]);

  const fail = useCallback((err: unknown) => {
    setError(err instanceof Error ? err.message : String(err));
  }, []);

  const refreshTurns = useCallback(
    async (sessionId = activeSession?.id) => {
      if (!sessionId) return;
      const next = await client.listTurns(sessionId);
      /* Keep the previous array identity when the wire payload is unchanged —
         otherwise the 1.8s poll re-renders the list and fights manual scrolling. */
      setTurns((prev) => (JSON.stringify(prev) === JSON.stringify(next) ? prev : next));
    },
    [activeSession?.id, client]
  );

  const openSession = useCallback(
    async (session: SessionSoul) => {
      setActiveSession(session);
      setShowSessions(false);
      setTurns([]);
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
    [client, fail]
  );

  const connect = useCallback(async () => {
    setConnecting(true);
    setError(null);
    try {
      const existing = await client.listSessions();
      const latest = [...existing].sort(
        (a, b) =>
          (b.last_active_at ?? b.created_at ?? 0) - (a.last_active_at ?? a.created_at ?? 0)
      )[0];
      const session = latest ?? (await client.createSession());
      setSessions(existing.length ? existing : [session]);
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
    if (!activeSession?.id) return;
    const timer = setInterval(() => {
      refreshTurns().catch(fail);
    }, 1800);
    return () => clearInterval(timer);
  }, [activeSession?.id, fail, refreshTurns]);

  /* Live SSE overlay: subscribe to the gateway event stream for the active
     session and reduce block.started / block.output / content.delta /
     reasoning.delta into per-turn live state so tool cards stream in real time.
     The 1.8s poll stays as the settled-state source (and SSE fallback). */
  useEffect(() => {
    const sid = activeSession?.id;
    if (!sid) return;
    setLive({});
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
          onEvent: (ev) => setLive((prev) => reduceLiveEvent(prev, ev)),
          onError: (err) => {
            const msg =
              (err as { message?: string; xhrStatus?: number })?.xhrStatus
                ? `HTTP ${(err as { xhrStatus?: number }).xhrStatus}`
                : (err as { message?: string })?.message || "connection lost";
            if (__DEV__) console.warn("[vis] SSE error", err);
            setNote(`live stream: ${msg} (retrying\u2026)`);
          }
        },
        0
      );
    } catch (err) {
      if (__DEV__) console.warn("[vis] SSE failed to start", err);
      setNote(`live stream unavailable: ${err instanceof Error ? err.message : String(err)}`);
    }
    return () => close?.();
  }, [activeSession?.id, client]);

  /* Auto-follow lives on the FlatList's onContentSizeChange: it fires for new
     messages AND for a streaming answer growing, and it is suppressed while the
     reader's finger is down — a poll landing mid-gesture must never yank. */

  const refreshSessions = useCallback(async () => {
    try {
      setSessions(await client.listSessions());
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
    []
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
    [activeSession?.id, client, fail, openSession, sessions]
  );

  const renameSession = useCallback(
    async (session: SessionSoul, title: string) => {
      try {
        const soul = await client.renameSession(session.id, title);
        setSessions((prev) => prev.map((s) => (s.id === session.id ? { ...s, ...soul } : s)));
        if (activeSession?.id === session.id) setActiveSession((prev) => (prev ? { ...prev, title } : prev));
      } catch (err) {
        fail(err);
      }
    },
    [activeSession?.id, client, fail]
  );

  const openModelDialog = useCallback(async () => {
    setShowModel(true);
    try {
      setModel(activeSession ? await client.sessionModel(activeSession.id) : null);
    } catch (err) {
      fail(err);
    }
    try {
      const catalog = await client.providerCatalog();
      setCatalog(catalog);
      /* An older running gateway serves /v1/models WITHOUT the catalog key. */
      setCatalogHint(
        catalog.length > 0
          ? null
          : "No models in the gateway catalog \u2014 if providers are configured, this gateway predates the /v1/models catalog; restart the vis gateway."
      );
    } catch {
      setCatalog([]);
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
        setModel(await client.setSessionModel(activeSession.id, provider, modelName));
      } catch (err) {
        fail(err);
      } finally {
        setPicking(false);
      }
    },
    [activeSession, client, fail, picking]
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
    model && typeof model !== "string" ? { provider: model.provider ?? "", model: model.model ?? "" } : null;

  const gatewayHost = gatewayUrl.replace(/^https?:\/\//, "").replace(/\/+$/, "");
  const canSend = connected && !sending && (input.trim().length > 0 || pendingAtts.length > 0);

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
          <IconBtn name="sidebar" onPress={() => { void refreshSessions(); setShowSessions(true); }} />
          <Pressable onPress={openRenameDialog} style={styles.barTitle} hitSlop={4}>
            <Text numberOfLines={1} style={styles.barName}>
              {activeSession?.title?.trim() || "Untitled"}
            </Text>
            <Feather name="edit-2" size={11} color={c.tsep} />
          </Pressable>
          <IconBtn name="settings" onPress={() => setShowSettings(true)} active={showSettings} />
        </View>

        {/* ── transcript: flat rows on the warm ground ─────────────── */}
        <FlatList
          ref={listRef}
          data={messages}
          keyExtractor={(m) => m.id}
          style={styles.flex}
          contentContainerStyle={styles.transcript}
          onScroll={(e) => {
            const { contentOffset, layoutMeasurement, contentSize } = e.nativeEvent;
            atBottomRef.current =
              contentOffset.y + layoutMeasurement.height >= contentSize.height - 48;
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
            <RefreshControl refreshing={refreshing} onRefresh={() => void onPullRefresh()} tintColor={c.amber} />
          }
          renderItem={({ item }) => <MessageRow item={item} />}
          ListEmptyComponent={
            <View style={styles.emptyWrap}>
              {connecting ? (
                <ActivityIndicator color={c.amber} />
              ) : (
                <>
                  <Text style={styles.emptyTitle}>{connected ? "fresh session" : "not connected"}</Text>
                  <Text style={styles.emptyBody}>
                    {connected
                      ? "Say something \u2014 vis is listening."
                      : "Open \u2699 and point me at a running vis gateway."}
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

        {/* ── attachment tray + flat composer ─────────────────────── */}
        <AttachmentTray items={pendingAtts} onRemove={(key) => setPendingAtts((prev) => prev.filter((a) => a.key !== key))} />
        <View style={styles.composer}>
          {!capturing ? (
            <>
              <Text style={styles.prompt}>{"\u203a"}</Text>
              <TextInput
                value={input}
                onChangeText={setInput}
                placeholder={connected ? "message vis\u2026" : "connect first\u2026"}
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
            onTranscript={(text) => setInput((prev) => (prev ? `${prev.trimEnd()} ${text}` : text))}
            onNote={setNote}
            onPhase={setCapturing}
          />
          {!capturing ? (
            runningTurn ? (
              <Pressable onPress={() => void cancelRunning()} style={[styles.sendBtn, styles.stopBtn]} hitSlop={6}>
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
          <Pressable onPress={() => void openModelDialog()} style={styles.footRouting} hitSlop={6}>
            <Feather name="zap" size={11} color={c.amberDeep} />
            <Text numberOfLines={1} style={styles.footRoutingName}>
              {modelLabel(model)}
            </Text>
          </Pressable>
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
        activeId={activeSession?.id ?? null}
        onClose={() => setShowSessions(false)}
        onSelect={(s) => void openSession(s)}
        onCreate={() => {
          setShowSessions(false);
          void createSession();
        }}
        onDelete={(s) => void deleteSession(s)}
        onRename={(s, title) => void renameSession(s, title)}
      />

      {/* ── settings — the web channel's Settings dialog, native ──── */}
      <DialogModal visible={showSettings} title="Settings" onClose={() => setShowSettings(false)} flush>
        <SettingsPane
          client={client}
          gatewayUrl={gatewayUrl}
          token={token}
          connecting={connecting}
          onGatewayUrl={setGatewayUrl}
          onToken={setToken}
          onReconnect={() => void connect()}
        />
      </DialogModal>

      {/* ── session model picker — the web picker, native ─────────── */}
      <DialogModal visible={showModel} title="Session model" onClose={() => setShowModel(false)}>
        <ScrollView style={styles.modelScroll} contentContainerStyle={styles.modelScrollBody}>
          <Text style={styles.activeModel}>
            This session: <Text style={styles.activeModelStrong}>{modelLabel(model)}</Text>
          </Text>
          <View style={styles.modelChips}>
            <Pressable
              onPress={() => void pickModel("", "")}
              style={[styles.modelChip, !currentPick?.model && styles.modelChipOn]}
            >
              <Feather
                name={!currentPick?.model ? "check" : "star"}
                size={11}
                color={!currentPick?.model ? c.amberBright : c.dim}
              />
              <Text style={[styles.modelChipText, !currentPick?.model && styles.modelChipTextOn]}>
                router default
              </Text>
            </Pressable>
          </View>
          {catalog.length === 0 ? (
            <Text style={styles.emptyBody}>{catalogHint ?? "No providers configured yet."}</Text>
          ) : (
            catalog.map((p) => (
              <View key={p.id} style={styles.modelGroup}>
                <Text style={styles.modelGroupLabel}>{p.label ?? p.id}</Text>
                <View style={styles.modelChips}>
                  {p.models.map((name) => {
                    const on = currentPick?.provider === p.id && currentPick?.model === name;
                    return (
                      <Pressable
                        key={name}
                        onPress={() => void pickModel(p.id, name)}
                        style={[styles.modelChip, on && styles.modelChipOn]}
                      >
                        {on ? <Feather name="check" size={11} color={c.amberBright} /> : null}
                        <Text style={[styles.modelChipText, on && styles.modelChipTextOn]}>{name}</Text>
                      </Pressable>
                    );
                  })}
                </View>
              </View>
            ))
          )}
        </ScrollView>
      </DialogModal>

      {/* ── rename session ───────────────────────────────────────── */}
      <DialogModal visible={showRename} title="Rename session" onClose={() => setShowRename(false)}>
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
        <ActionBtn label="Save" tone="amber" disabled={!renameDraft.trim()} onPress={() => void applyRename()} />
      </DialogModal>

      {/* ── error dialog ─────────────────────────────────────────── */}
      <DialogModal visible={error != null} title="Problem" tone="error" onClose={() => setError(null)}>
        <Text style={styles.errorText}>{error}</Text>
        <ActionBtn label="Dismiss" tone="ghost" onPress={() => setError(null)} />
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
    backgroundColor: c.paper
  },
  barTitle: { flex: 1, flexDirection: "row", alignItems: "center", gap: 6, minWidth: 0 },
  barName: { flexShrink: 1, fontFamily: mono, fontSize: 13, fontWeight: "700", color: c.ink },
  transcript: { paddingHorizontal: 12, paddingTop: 10, paddingBottom: 40, gap: 14, flexGrow: 1 },
  msg: { gap: 5 },
  msgHead: { flexDirection: "row", alignItems: "center", gap: 7 },
  gutter: { width: 3, height: 12 },
  msgRole: { fontFamily: mono, fontSize: 10, fontWeight: "700", letterSpacing: 1 },
  headRule: { flex: 1, height: StyleSheet.hairlineWidth, backgroundColor: c.hair },
  msgTime: { fontFamily: mono, fontSize: 9, color: c.tsep },
  attRow: { flexDirection: "row", flexWrap: "wrap", gap: 6 },
  attImg: { width: 96, height: 96, backgroundColor: c.tsepBg },
  msgMeta: { fontFamily: mono, fontSize: 9.5, color: c.dim, marginTop: 2 },
  pending: { fontFamily: mono, fontSize: 12, color: c.dim },
  thinking: { fontFamily: mono, fontSize: 11, lineHeight: 16, color: c.dim, fontStyle: "italic" },
  emptyWrap: { flex: 1, alignItems: "center", justifyContent: "center", gap: 6, paddingTop: 80 },
  emptyTitle: { fontFamily: mono, fontSize: 13, fontWeight: "700", color: c.ink },
  emptyBody: { fontSize: 12, color: c.dim, textAlign: "center", paddingHorizontal: 40 },
  note: {
    flexDirection: "row",
    alignItems: "center",
    gap: 6,
    marginHorizontal: 12,
    marginBottom: 6,
    backgroundColor: c.chipBg,
    paddingHorizontal: 10,
    paddingVertical: 7
  },
  noteText: { flex: 1, fontFamily: mono, fontSize: 11.5, color: c.chipInk },
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
    backgroundColor: c.paper
  },
  prompt: {
    fontFamily: mono,
    fontSize: 16,
    fontWeight: "700",
    color: c.amber,
    lineHeight: 30,
    marginRight: 4
  },
  input: {
    flex: 1,
    minHeight: 30,
    maxHeight: 110,
    color: c.ink,
    fontSize: 13,
    paddingVertical: 6,
    paddingHorizontal: 0,
    backgroundColor: "transparent"
  },
  sendBtn: {
    width: 30,
    height: 30,
    alignItems: "center",
    justifyContent: "center",
    backgroundColor: c.amber
  },
  stopBtn: { backgroundColor: c.err },
  footer: {
    flexDirection: "row",
    alignItems: "center",
    gap: 8,
    paddingHorizontal: 12,
    paddingVertical: 4
  },
  footRouting: {
    flexDirection: "row",
    alignItems: "center",
    gap: 5,
    maxWidth: 230,
    borderWidth: 1,
    borderColor: c.lineSoft,
    backgroundColor: c.field,
    paddingHorizontal: 8,
    paddingVertical: 3
  },
  footRoutingName: { fontFamily: mono, fontSize: 10, color: c.amberDeep },
  footerText: { flex: 1, textAlign: "right", fontFamily: mono, fontSize: 10, color: c.dim },
  fieldLabel: {
    fontFamily: mono,
    fontSize: 10.5,
    letterSpacing: 1,
    textTransform: "uppercase",
    color: c.dim
  },
  field: {
    borderWidth: 1,
    borderColor: c.line,
    backgroundColor: c.field,
    color: c.ink,
    fontFamily: mono,
    fontSize: 12,
    paddingHorizontal: 10,
    paddingVertical: 6
  },
  modelScroll: { maxHeight: 460 },
  modelScrollBody: { gap: 14, paddingBottom: 4 },
  activeModel: { fontSize: 12, color: c.dim },
  activeModelStrong: { fontFamily: mono, fontWeight: "700", color: c.ink },
  modelGroup: { gap: 8 },
  modelGroupLabel: {
    fontFamily: mono,
    fontSize: 10.5,
    letterSpacing: 1,
    textTransform: "uppercase",
    color: c.dim
  },
  modelChips: { flexDirection: "row", flexWrap: "wrap", gap: 6 },
  modelChip: {
    flexDirection: "row",
    alignItems: "center",
    gap: 5,
    borderWidth: 1,
    borderColor: c.line,
    backgroundColor: c.field,
    paddingHorizontal: 11,
    paddingVertical: 7
  },
  modelChipOn: { backgroundColor: c.ink, borderColor: c.ink },
  modelChipText: { fontFamily: mono, fontSize: 12, color: c.ink },
  modelChipTextOn: { color: c.amberBright, fontWeight: "700" },
  errorText: { color: c.ink, fontSize: 13, lineHeight: 18 }
});
