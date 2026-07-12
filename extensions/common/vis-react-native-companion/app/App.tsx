import React, { useCallback, useEffect, useMemo, useRef, useState } from "react";
import {
  ActivityIndicator,
  FlatList,
  KeyboardAvoidingView,
  Platform,
  Pressable,
  SafeAreaView,
  ScrollView,
  StatusBar,
  StyleSheet,
  Text,
  TextInput,
  View
} from "react-native";

import { GatewayTurn, SessionSoul, VisGatewayClient } from "./src/visClient";

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
  pending?: boolean;
};

const messageText = (turn: GatewayTurn): Message[] => {
  const id = turn.id ?? turn.turn_id ?? Math.random().toString(36);
  const user = turn.request?.trim();
  const answer = (turn.answer_md ?? turn.answer)?.trim();
  const status = turn.status && turn.status !== "completed" ? `(${turn.status})` : "";

  return [
    ...(user ? [{ id: `${id}-user`, role: "user" as const, text: user }] : []),
    ...(answer || status
      ? [{ id: `${id}-vis`, role: "vis" as const, text: answer || status, pending: !answer }]
      : [])
  ];
};

export default function App() {
  const [gatewayUrl, setGatewayUrl] = useState(envGateway);
  const [token, setToken] = useState(envToken ?? "");
  const [sessions, setSessions] = useState<SessionSoul[]>([]);
  const [activeSession, setActiveSession] = useState<SessionSoul | null>(null);
  const [turns, setTurns] = useState<GatewayTurn[]>([]);
  const [input, setInput] = useState("");
  const [connecting, setConnecting] = useState(false);
  const [sending, setSending] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const listRef = useRef<FlatList<Message>>(null);

  const client = useMemo(
    () => new VisGatewayClient({ gatewayUrl, token }),
    [gatewayUrl, token]
  );

  const messages = useMemo(() => turns.flatMap(messageText), [turns]);

  const refreshTurns = useCallback(
    async (sessionId = activeSession?.id) => {
      if (!sessionId) return;
      const nextTurns = await client.listTurns(sessionId);
      setTurns(nextTurns);
    },
    [activeSession?.id, client]
  );

  const connect = useCallback(async () => {
    setConnecting(true);
    setError(null);
    try {
      const existing = await client.listSessions();
      const session = existing[0] ?? (await client.createSession());
      setSessions(existing.length ? existing : [session]);
      setActiveSession(session);
      setTurns(await client.listTurns(session.id));
    } catch (err) {
      setError(err instanceof Error ? err.message : String(err));
    } finally {
      setConnecting(false);
    }
  }, [client]);

  useEffect(() => {
    void connect();
  }, [connect]);

  useEffect(() => {
    if (!activeSession?.id) return;
    const timer = setInterval(() => {
      refreshTurns().catch((err) => setError(err instanceof Error ? err.message : String(err)));
    }, 1800);
    return () => clearInterval(timer);
  }, [activeSession?.id, refreshTurns]);

  useEffect(() => {
    if (messages.length > 0) {
      requestAnimationFrame(() => listRef.current?.scrollToEnd({ animated: true }));
    }
  }, [messages.length]);

  const send = useCallback(async () => {
    const text = input.trim();
    if (!text || !activeSession) return;
    setInput("");
    setSending(true);
    setError(null);
    try {
      await client.submitTurn(activeSession.id, text);
      await refreshTurns(activeSession.id);
    } catch (err) {
      setError(err instanceof Error ? err.message : String(err));
      setInput(text);
    } finally {
      setSending(false);
    }
  }, [activeSession, client, input, refreshTurns]);

  const createSession = useCallback(async () => {
    setConnecting(true);
    setError(null);
    try {
      const session = await client.createSession();
      setSessions((prev) => [session, ...prev]);
      setActiveSession(session);
      setTurns([]);
    } catch (err) {
      setError(err instanceof Error ? err.message : String(err));
    } finally {
      setConnecting(false);
    }
  }, [client]);

  return (
    <SafeAreaView style={styles.safe}>
      <StatusBar barStyle="light-content" />
      <KeyboardAvoidingView
        behavior={Platform.select({ ios: "padding", android: undefined })}
        style={styles.root}
      >
        <View style={styles.header}>
          <View>
            <Text style={styles.brand}>vis</Text>
            <Text style={styles.subtitle}>React Native companion</Text>
          </View>
          {(connecting || sending) && <ActivityIndicator color="#a7f3d0" />}
        </View>

        <View style={styles.configCard}>
          <Text style={styles.label}>Gateway URL</Text>
          <TextInput
            autoCapitalize="none"
            autoCorrect={false}
            onChangeText={setGatewayUrl}
            placeholder="http://host:port"
            placeholderTextColor="#64748b"
            style={styles.input}
            value={gatewayUrl}
          />
          <Text style={styles.label}>Bearer token (if required)</Text>
          <TextInput
            autoCapitalize="none"
            autoCorrect={false}
            onChangeText={setToken}
            placeholder="optional"
            placeholderTextColor="#64748b"
            secureTextEntry
            style={styles.input}
            value={token}
          />
          <View style={styles.actions}>
            <Pressable onPress={connect} style={styles.secondaryButton}>
              <Text style={styles.secondaryButtonText}>Reconnect</Text>
            </Pressable>
            <Pressable onPress={createSession} style={styles.secondaryButton}>
              <Text style={styles.secondaryButtonText}>New chat</Text>
            </Pressable>
          </View>
        </View>

        {error && <Text style={styles.error}>{error}</Text>}

        <ScrollView horizontal showsHorizontalScrollIndicator={false} style={styles.sessionStrip}>
          {sessions.map((session) => (
            <Pressable
              key={session.id}
              onPress={() => {
                setActiveSession(session);
                refreshTurns(session.id).catch((err) =>
                  setError(err instanceof Error ? err.message : String(err))
                );
              }}
              style={[styles.sessionPill, activeSession?.id === session.id && styles.sessionPillActive]}
            >
              <Text style={styles.sessionPillText}>{session.title || session.id.slice(0, 8)}</Text>
            </Pressable>
          ))}
        </ScrollView>

        <FlatList
          ref={listRef}
          contentContainerStyle={styles.thread}
          data={messages}
          keyExtractor={(item) => item.id}
          ListEmptyComponent={
            <View style={styles.empty}>
              <Text style={styles.emptyTitle}>Ask vis from your phone.</Text>
              <Text style={styles.emptyText}>The app posts turns to the gateway and refreshes the shared transcript.</Text>
            </View>
          }
          renderItem={({ item }) => (
            <View style={[styles.bubble, item.role === "user" ? styles.userBubble : styles.visBubble]}>
              <Text style={styles.role}>{item.role === "user" ? "You" : "Vis"}</Text>
              <Text style={[styles.message, item.pending && styles.pending]}>{item.text}</Text>
            </View>
          )}
        />

        <View style={styles.composer}>
          <TextInput
            multiline
            onChangeText={setInput}
            placeholder="Message vis…"
            placeholderTextColor="#64748b"
            style={styles.composerInput}
            value={input}
          />
          <Pressable disabled={sending || !input.trim()} onPress={send} style={styles.sendButton}>
            <Text style={styles.sendButtonText}>Send</Text>
          </Pressable>
        </View>
      </KeyboardAvoidingView>
    </SafeAreaView>
  );
}

const styles = StyleSheet.create({
  safe: { flex: 1, backgroundColor: "#020617" },
  root: { flex: 1, padding: 16, gap: 12 },
  header: { flexDirection: "row", alignItems: "center", justifyContent: "space-between" },
  brand: { color: "#ecfdf5", fontSize: 34, fontWeight: "800", letterSpacing: -1 },
  subtitle: { color: "#94a3b8", fontSize: 13, marginTop: -4 },
  configCard: { backgroundColor: "#0f172a", borderColor: "#1e293b", borderRadius: 18, borderWidth: 1, padding: 12, gap: 8 },
  label: { color: "#94a3b8", fontSize: 12, fontWeight: "700", textTransform: "uppercase" },
  input: { backgroundColor: "#020617", borderColor: "#334155", borderRadius: 12, borderWidth: 1, color: "#e2e8f0", padding: 10 },
  actions: { flexDirection: "row", gap: 8 },
  secondaryButton: { backgroundColor: "#134e4a", borderRadius: 999, paddingHorizontal: 14, paddingVertical: 8 },
  secondaryButtonText: { color: "#ccfbf1", fontWeight: "700" },
  error: { color: "#fecaca", backgroundColor: "#7f1d1d", borderRadius: 12, overflow: "hidden", padding: 10 },
  sessionStrip: { maxHeight: 44 },
  sessionPill: { backgroundColor: "#0f172a", borderRadius: 999, marginRight: 8, paddingHorizontal: 12, paddingVertical: 8 },
  sessionPillActive: { backgroundColor: "#047857" },
  sessionPillText: { color: "#ecfdf5", fontWeight: "700" },
  thread: { flexGrow: 1, gap: 12, justifyContent: "flex-end", paddingVertical: 10 },
  empty: { alignItems: "center", gap: 8, padding: 24 },
  emptyTitle: { color: "#ecfdf5", fontSize: 20, fontWeight: "800", textAlign: "center" },
  emptyText: { color: "#94a3b8", textAlign: "center" },
  bubble: { borderRadius: 18, maxWidth: "88%", padding: 12 },
  userBubble: { alignSelf: "flex-end", backgroundColor: "#1d4ed8" },
  visBubble: { alignSelf: "flex-start", backgroundColor: "#111827", borderColor: "#1f2937", borderWidth: 1 },
  role: { color: "#a7f3d0", fontSize: 12, fontWeight: "800", marginBottom: 4, textTransform: "uppercase" },
  message: { color: "#f8fafc", fontSize: 16, lineHeight: 22 },
  pending: { color: "#94a3b8", fontStyle: "italic" },
  composer: { alignItems: "flex-end", backgroundColor: "#0f172a", borderColor: "#1e293b", borderRadius: 22, borderWidth: 1, flexDirection: "row", gap: 8, padding: 8 },
  composerInput: { color: "#e2e8f0", flex: 1, maxHeight: 120, minHeight: 44, paddingHorizontal: 10, paddingVertical: 10 },
  sendButton: { backgroundColor: "#10b981", borderRadius: 16, paddingHorizontal: 16, paddingVertical: 12 },
  sendButtonText: { color: "#022c22", fontWeight: "900" }
});
