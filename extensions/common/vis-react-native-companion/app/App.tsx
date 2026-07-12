import React, { useCallback, useEffect, useMemo, useRef, useState } from "react";
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
const MessageRow = ({ item }: { item: Message }) => {
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
      {item.pending ? <PendingDots /> : item.text ? <Markdown text={item.text} /> : null}
      {!item.pending && item.meta ? <Text style={styles.msgMeta}>{item.meta}</Text> : null}
    </View>
  );
};

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
  const listRef = useRef<FlatList<Message>>(null);
  const atBottomRef = useRef(true);
  const draggingRef = useRef(false);

  const client = useMemo(() => new VisGatewayClient({ gatewayUrl, token }), [gatewayUrl, token]);

  const messages = useMemo(() => turns.flatMap(messageText), [turns]);
  const connected = !error && activeSession != null;
  const runningTurn = useMemo(
    () => [...turns].reverse().find((t) => turnLive(t) && (t.id ?? t.turn_id)),
    [turns]
  );

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
      try {
        setTurns(await client.listTurns(session.id));
        setModel(await client.sessionModel(session.id));
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
            if (atBottomRef.current && !draggingRef.current) {
              listRef.current?.scrollToEnd({ animated: true });
            }
          }}
          scrollEventThrottle={16}
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
              <Feather name="star" size={11} color={!currentPick?.model ? c.amberBright : c.ink} />
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
    maxWidth: 230
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
  modelScroll: { maxHeight: 420 },
  modelScrollBody: { gap: 12 },
  activeModel: { fontSize: 12, color: c.dim },
  activeModelStrong: { fontFamily: mono, fontWeight: "700", color: c.ink },
  modelGroup: { gap: 6 },
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
    backgroundColor: c.tsepBg,
    paddingHorizontal: 8,
    paddingVertical: 4
  },
  modelChipOn: { backgroundColor: c.ink },
  modelChipText: { fontFamily: mono, fontSize: 11, color: c.ink },
  modelChipTextOn: { color: c.amberBright },
  errorText: { color: c.ink, fontSize: 13, lineHeight: 18 }
});
