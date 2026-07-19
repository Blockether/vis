import React, {
  useCallback,
  useEffect,
  useMemo,
  useRef,
  useState,
} from "react";
import {
  Pressable,
  ScrollView,
  StyleSheet,
  Text,
  TextInput,
  View,
  useWindowDimensions,
} from "react-native";
import { Feather } from "@expo/vector-icons";

import { c, mono } from "./theme";
import { ActionBtn } from "./UI";
import {
  GatewayPairingScanner,
  describePairing,
} from "./GatewayPairingScanner";
import type { GatewayPairing } from "./GatewayPairing";
import type {
  SettingsGroup,
  ToggleRow,
  VisGatewayClient,
  WorkspaceInfo,
} from "./VisClient";

/* ── the web channel's Settings dialog, native ──────────────────────
   VS Code-style: full-width live search + count, a left category rail,
   grouped toggle rows on the right (boolean → pill switch, enum → mono
   cycle chip) — same registry (`toggles-for-channel`), same grouping,
   served by the gateway's canonical /v1/settings API. The app-local Gateway
   fields ride along as the last group. */

const GATEWAY_GROUP = "gateway";
const WORKSPACE_GROUP = "workspace";

/* iOS-style switch, blue when on, sliding knob. */
const PillSwitch = ({ on, onPress }: { on: boolean; onPress: () => void }) => (
  <Pressable
    onPress={onPress}
    hitSlop={6}
    style={[st.switch, on && st.switchOn]}
  >
    <View style={[st.knob, on && st.knobOn]} />
  </Pressable>
);

const rowMatches = (row: ToggleRow, q: string): boolean =>
  q === "" ||
  row.label.toLowerCase().includes(q) ||
  (row.description ?? "").toLowerCase().includes(q) ||
  row.id.toLowerCase().includes(q);

export const SettingsPane = ({
  client,
  gatewayUrl,
  token,
  connecting,
  connectionProblem,
  onGatewayUrl,
  onToken,
  onReconnect,
  notify,
  onNotify,
  sessionId,
}: {
  client: VisGatewayClient;
  gatewayUrl: string;
  token: string;
  connecting: boolean;
  connectionProblem?: string | null;
  onGatewayUrl: (value: string) => void;
  onToken: (value: string) => void;
  onReconnect: () => void;
  notify: boolean;
  onNotify: (value: boolean) => void;
  /* Active session — enables the canonical /v1/sessions/:sid/workspace section. */
  sessionId?: string | null;
}) => {
  const { width: winW } = useWindowDimensions();
  const isWide = winW >= 700;
  const [groups, setGroups] = useState<SettingsGroup[]>([]);
  const [stale, setStale] = useState(false);
  const [query, setQuery] = useState("");
  const [active, setActive] = useState<string | null>(null);
  const scrollRef = useRef<ScrollView | null>(null);
  const sectionY = useRef<Record<string, number>>({});

  /* ── workspace + filesystem roots (canonical /v1/sessions/:sid/workspace) —
     the SAME state + mutations the web footer and TUI directory picker use. ── */
  const [ws, setWs] = useState<WorkspaceInfo | null>(null);
  const [wsBusy, setWsBusy] = useState(false);
  const [rootDraft, setRootDraft] = useState("");
  const [wsErr, setWsErr] = useState<string | null>(null);
  const [scanOpen, setScanOpen] = useState(false);
  const [scannerNonce, setScannerNonce] = useState(0);
  const [pairedNote, setPairedNote] = useState<string | null>(null);

  useEffect(() => {
    if (!sessionId) {
      setWs(null);
      return;
    }
    let alive = true;
    client
      .sessionWorkspace(sessionId)
      .then((w) => {
        if (alive) setWs(w);
      })
      .catch(() => {
        /* older gateway predates /v1/workspace — leave null (shows the note) */
      });
    return () => {
      alive = false;
    };
  }, [client, sessionId]);

  const addRoot = useCallback(() => {
    const path = rootDraft.trim();
    if (!path || !sessionId) return;
    setWsBusy(true);
    setWsErr(null);
    void client
      .addRoot(sessionId, path)
      .then((w) => {
        setWs(w);
        setRootDraft("");
      })
      .catch((e) => setWsErr(e instanceof Error ? e.message : String(e)))
      .finally(() => setWsBusy(false));
  }, [client, sessionId, rootDraft]);

  const removeRoot = useCallback(
    (path: string) => {
      if (!sessionId) return;
      setWsBusy(true);
      setWsErr(null);
      void client
        .removeRoot(sessionId, path)
        .then((w) => setWs(w))
        .catch((e) => setWsErr(e instanceof Error ? e.message : String(e)))
        .finally(() => setWsBusy(false));
    },
    [client, sessionId],
  );

  useEffect(() => {
    let alive = true;
    client
      // Scope to THIS channel so web/tui-only controls (e.g. the web theme
      // picker, `:channels #{:web}`) don't leak into the mobile Settings.
      .settingsGroups("react-native")
      .then((gs) => {
        if (alive) {
          setGroups(gs);
          setStale(false);
        }
      })
      .catch(() => {
        /* An older running gateway predates /v1/settings. */
        if (alive) setStale(true);
      });
    return () => {
      alive = false;
    };
  }, [client]);

  const mutate = useCallback(
    (row: ToggleRow) => {
      const action = row.choices?.length ? "cycle" : "toggle";
      void client
        .settingsMutate(row.id, action)
        .then((fresh) =>
          setGroups((gs) =>
            gs.map((g) => ({
              ...g,
              toggles: g.toggles.map((t) => (t.id === row.id ? fresh : t)),
            })),
          ),
        )
        .catch(() => setStale(true));
    },
    [client],
  );

  const q = query.trim().toLowerCase();
  const visible = useMemo(
    () =>
      groups
        .map((g) => ({
          ...g,
          toggles: g.toggles.filter((t) => rowMatches(t, q)),
        }))
        .filter((g) => g.toggles.length > 0),
    [groups, q],
  );
  const count = visible.reduce((n, g) => n + g.toggles.length, 0);
  const gatewayVisible =
    q === "" || "gateway url bearer token reconnect notifications".includes(q);
  const workspaceVisible =
    !!sessionId && (q === "" || "workspace roots filesystem".includes(q));

  const jumpTo = (id: string) => {
    setActive(id);
    const y = sectionY.current[id];
    if (y != null)
      scrollRef.current?.scrollTo({ y: Math.max(0, y - 4), animated: true });
  };

  const applyPairing = (pairing: GatewayPairing) => {
    onGatewayUrl(pairing.gatewayUrl);
    if (pairing.token != null) onToken(pairing.token);
    setPairedNote(`paired ${describePairing(pairing)} — reconnect when ready`);
  };

  const refreshScannerAfterScroll = useCallback(() => {
    if (scanOpen) setScannerNonce((n) => n + 1);
  }, [scanOpen]);

  useEffect(() => {
    if (!connectionProblem) return;
    setQuery("");
    setActive(GATEWAY_GROUP);
    requestAnimationFrame(() => {
      const y = sectionY.current[GATEWAY_GROUP];
      if (y != null)
        scrollRef.current?.scrollTo({ y: Math.max(0, y - 4), animated: true });
    });
  }, [connectionProblem]);

  return (
    <View style={st.pane}>
      {/* ── full-width live search — the web's .settings-search ── */}
      <View style={st.search}>
        <Feather name="search" size={14} color={c.dim} />
        <TextInput
          value={query}
          onChangeText={setQuery}
          placeholder="Search settings…"
          placeholderTextColor={c.dim}
          autoCapitalize="none"
          autoCorrect={false}
          style={st.searchInput}
        />
        <Text style={st.count}>
          {count + (gatewayVisible ? 4 : 0) + (workspaceVisible ? 1 : 0)}{" "}
          settings
        </Text>
      </View>

      <View style={[st.cols, !isWide && st.colsStack]}>
        {/* ── category rail — the web's .settings-toc ── */}
        {isWide ? (
          <ScrollView style={st.toc} contentContainerStyle={st.tocBody}>
            {visible.map((g) => (
              <Pressable
                key={g.id}
                onPress={() => jumpTo(g.id)}
                style={[st.tocItem, active === g.id && st.tocItemActive]}
              >
                <Text
                  numberOfLines={1}
                  style={[st.tocLabel, active === g.id && st.tocLabelActive]}
                >
                  {g.title}
                </Text>
              </Pressable>
            ))}
            {gatewayVisible ? (
              <Pressable
                onPress={() => jumpTo(GATEWAY_GROUP)}
                style={[
                  st.tocItem,
                  active === GATEWAY_GROUP && st.tocItemActive,
                ]}
              >
                <Text
                  numberOfLines={1}
                  style={[
                    st.tocLabel,
                    active === GATEWAY_GROUP && st.tocLabelActive,
                  ]}
                >
                  Gateway
                </Text>
              </Pressable>
            ) : null}
            {workspaceVisible ? (
              <Pressable
                onPress={() => jumpTo(WORKSPACE_GROUP)}
                style={[
                  st.tocItem,
                  active === WORKSPACE_GROUP && st.tocItemActive,
                ]}
              >
                <Text
                  numberOfLines={1}
                  style={[
                    st.tocLabel,
                    active === WORKSPACE_GROUP && st.tocLabelActive,
                  ]}
                >
                  Workspace
                </Text>
              </Pressable>
            ) : null}
          </ScrollView>
        ) : null}

        {/* ── grouped rows — the web's .settings-groups ── */}
        <ScrollView
          ref={scrollRef}
          style={st.groups}
          contentContainerStyle={st.groupsBody}
          alwaysBounceVertical
          canCancelContentTouches
          keyboardDismissMode="interactive"
          keyboardShouldPersistTaps="always"
          directionalLockEnabled={false}
          removeClippedSubviews={false}
          scrollEventThrottle={16}
          nestedScrollEnabled
          showsVerticalScrollIndicator
          onScrollEndDrag={refreshScannerAfterScroll}
          onMomentumScrollEnd={refreshScannerAfterScroll}
        >
          {stale ? (
            <View style={st.noticeCard}>
              <Text style={st.noticeTitle}>Gateway settings unavailable</Text>
              <Text style={st.noticeCopy}>
                This gateway is offline or was started before the /v1/settings
                API existed. The connection controls stay available below;
                restart or reconnect the gateway to load providers, model,
                engine, and channel settings.
              </Text>
            </View>
          ) : null}
          {/* ── app-local Gateway group — first when connection is broken ── */}
          {gatewayVisible ? (
            <View
              onLayout={(e) => {
                sectionY.current[GATEWAY_GROUP] = e.nativeEvent.layout.y;
              }}
            >
              <Text style={st.groupTitle}>Gateway</Text>
              <View style={st.sectionCard}>
                {connectionProblem ? (
                  <View style={st.problemBox}>
                    <Feather name="wifi-off" size={16} color={c.err} />
                    <View style={st.problemText}>
                      <Text style={st.problemTitle}>
                        Gateway connection required
                      </Text>
                      <Text style={st.problemCopy}>{connectionProblem}</Text>
                      <Text style={st.problemHint}>
                        Prefer the Tailscale QR when your Mac is on Tailnet;
                        same Wi‑Fi is the fallback. Then tap Reconnect.
                      </Text>
                    </View>
                  </View>
                ) : null}
                <View style={st.row}>
                  <View style={st.rowText}>
                    <Text style={st.rowLabel}>Turn notifications</Text>
                    <Text style={st.rowDesc}>
                      Ping me when a turn finishes while the app is in the
                      background.
                    </Text>
                  </View>
                  <PillSwitch on={notify} onPress={() => onNotify(!notify)} />
                </View>
                <Text style={st.fieldLabel}>Gateway URL</Text>
                <TextInput
                  value={gatewayUrl}
                  onChangeText={onGatewayUrl}
                  autoCapitalize="none"
                  autoCorrect={false}
                  style={st.field}
                />
                <Text style={st.fieldLabel}>Bearer token</Text>
                <TextInput
                  value={token}
                  onChangeText={onToken}
                  autoCapitalize="none"
                  autoCorrect={false}
                  secureTextEntry
                  style={st.field}
                />
                {pairedNote ? (
                  <Text style={st.okNote}>{pairedNote}</Text>
                ) : null}
                <Text style={st.sectionCopy} pointerEvents="none">
                  Prefer Tailscale when available: the gateway QR advertises the
                  100.x address first. Use same Wi‑Fi only as the fallback.
                </Text>
                {scanOpen ? (
                  <View style={st.scannerSlot}>
                    <GatewayPairingScanner
                      key={scannerNonce}
                      onPair={applyPairing}
                      onClose={() => setScanOpen(false)}
                    />
                  </View>
                ) : null}
                <View style={st.reconnect}>
                  <ActionBtn
                    label="Scan QR"
                    tone="ghost"
                    onPress={() => setScanOpen(true)}
                  />
                  <ActionBtn
                    label={connecting ? "connecting…" : "Reconnect"}
                    tone="amber"
                    disabled={connecting}
                    onPress={onReconnect}
                  />
                </View>
              </View>
            </View>
          ) : null}

          {visible.map((g) => (
            <View
              key={g.id}
              onLayout={(e) => {
                sectionY.current[g.id] = e.nativeEvent.layout.y;
              }}
            >
              <Text style={st.groupTitle}>{g.title}</Text>
              <View style={st.sectionCard}>
                {g.toggles.map((row) => (
                  <View key={row.id} style={st.row}>
                    <View style={st.rowText}>
                      <Text style={st.rowLabel}>{row.label}</Text>
                      {row.description ? (
                        <Text style={st.rowDesc}>{row.description}</Text>
                      ) : null}
                    </View>
                    {row.choices?.length ? (
                      <Pressable
                        onPress={() => mutate(row)}
                        hitSlop={6}
                        style={st.cycle}
                      >
                        <Text style={st.cycleLabel}>{row.value ?? "?"}</Text>
                      </Pressable>
                    ) : (
                      <PillSwitch
                        on={row.enabled === true}
                        onPress={() => mutate(row)}
                      />
                    )}
                  </View>
                ))}
              </View>
            </View>
          ))}
          {/* ── workspace + filesystem roots (canonical /v1/sessions/:sid/workspace) ── */}
          {workspaceVisible ? (
            <View
              onLayout={(e) => {
                sectionY.current[WORKSPACE_GROUP] = e.nativeEvent.layout.y;
              }}
            >
              <Text style={st.groupTitle}>Workspace</Text>
              {ws ? (
                <>
                  <View style={st.row}>
                    <View style={st.rowText}>
                      <Text style={st.rowLabel}>{ws.label ?? "root"}</Text>
                      <Text style={st.rowDesc} numberOfLines={1}>
                        {ws.root ?? ws.repo_root ?? "\u2014"}
                      </Text>
                    </View>
                  </View>
                  {(ws.filesystem_roots ?? []).map((r, i) => (
                    <View key={r.dir ?? String(i)} style={st.row}>
                      <View style={st.rowText}>
                        <Text style={st.rowDesc} numberOfLines={1}>
                          {r.dir ?? "\u2014"}
                        </Text>
                      </View>
                      <Pressable
                        onPress={() => r.dir && removeRoot(r.dir)}
                        hitSlop={6}
                        disabled={wsBusy}
                        style={st.cycle}
                      >
                        <Feather name="x" size={12} color={c.err} />
                      </Pressable>
                    </View>
                  ))}
                  <Text style={st.fieldLabel}>add filesystem root</Text>
                  <View style={st.wsAdd}>
                    <TextInput
                      value={rootDraft}
                      onChangeText={setRootDraft}
                      placeholder="/absolute/path"
                      placeholderTextColor={c.dim}
                      autoCapitalize="none"
                      autoCorrect={false}
                      style={[st.field, st.wsField]}
                    />
                    <ActionBtn
                      label="Add"
                      tone="amber"
                      disabled={wsBusy || rootDraft.trim() === ""}
                      onPress={addRoot}
                    />
                  </View>
                  {wsErr ? <Text style={st.staleNote}>{wsErr}</Text> : null}
                </>
              ) : (
                <Text style={st.staleNote}>
                  This gateway does not expose the session workspace (older /v1)
                  — restart the vis gateway to manage filesystem roots here.
                </Text>
              )}
            </View>
          ) : null}
        </ScrollView>
      </View>
    </View>
  );
};

const st = StyleSheet.create({
  pane: {
    flex: 1,
    minHeight: 0,
    alignSelf: "stretch",
    backgroundColor: "#F2F2F7",
  },
  /* .settings-search */
  search: {
    flexDirection: "row",
    alignItems: "center",
    gap: 8,
    marginHorizontal: 18,
    marginTop: 16,
    marginBottom: 12,
    paddingHorizontal: 13,
    paddingVertical: 10,
    borderRadius: 14,
    backgroundColor: "rgba(118,118,128,0.12)",
  },
  searchInput: {
    flex: 1,
    minWidth: 0,
    padding: 0,
    fontSize: 13,
    color: c.ink,
  },
  count: { fontSize: 11, color: c.dim, fontWeight: "600" },
  /* .settings-cols */
  cols: { flex: 1, flexDirection: "row", minHeight: 0 },
  colsStack: { flexDirection: "column" },
  /* .settings-toc */
  toc: {
    width: 112,
    flexGrow: 0,
    flexShrink: 0,
    borderRightWidth: StyleSheet.hairlineWidth,
    borderRightColor: "rgba(60,60,67,0.18)",
  },
  tocBody: { padding: 10, gap: 4 },
  tocItem: { paddingVertical: 8, paddingHorizontal: 10, borderRadius: 10 },
  tocItemActive: { backgroundColor: c.accentSoft },
  tocLabel: { fontSize: 12, color: c.dim, fontWeight: "500" },
  tocLabelActive: { color: c.ink, fontWeight: "700" },
  /* .settings-groups */
  groups: { flex: 1, minWidth: 0 },
  groupsBody: { paddingHorizontal: 18, paddingBottom: 44, flexGrow: 1 },
  groupTitle: {
    fontSize: 13,
    fontWeight: "700",
    color: c.dim,
    marginTop: 14,
    marginBottom: 7,
    marginLeft: 2,
  },
  sectionCard: {
    overflow: "hidden",
    borderRadius: 18,
    backgroundColor: "#FFFFFF",
    borderWidth: StyleSheet.hairlineWidth,
    borderColor: "rgba(60,60,67,0.16)",
  },
  /* .toggle-row */
  row: {
    flexDirection: "row",
    alignItems: "center",
    gap: 12,
    minHeight: 56,
    paddingHorizontal: 16,
    paddingVertical: 11,
    borderBottomWidth: StyleSheet.hairlineWidth,
    borderBottomColor: "rgba(60,60,67,0.14)",
  },
  rowText: { flex: 1, minWidth: 0 },
  rowLabel: { fontSize: 15, fontWeight: "600", color: c.ink },
  rowDesc: { fontSize: 12.5, color: c.dim, marginTop: 3, lineHeight: 17 },
  /* .switch / .knob */
  switch: {
    width: 40,
    height: 22,
    borderRadius: 11,
    backgroundColor: c.hair,
    justifyContent: "center",
  },
  switchOn: { backgroundColor: c.accent },
  knob: {
    position: "absolute",
    top: 2,
    left: 2,
    width: 18,
    height: 18,
    borderRadius: 9,
    backgroundColor: c.field,
    shadowColor: "#141414",
    shadowOpacity: 0.25,
    shadowRadius: 2,
    shadowOffset: { width: 0, height: 1 },
    elevation: 2,
  },
  knobOn: { left: 20 },
  /* .toggle-cycle */
  cycle: {
    backgroundColor: "rgba(118,118,128,0.12)",
    borderRadius: 11,
    paddingHorizontal: 11,
    paddingVertical: 6,
  },
  cycleLabel: { fontFamily: mono, fontSize: 11, color: c.chipInk },
  staleNote: { fontSize: 11, color: c.dim, marginTop: 10, lineHeight: 15 },
  noticeCard: {
    marginTop: 14,
    padding: 14,
    borderRadius: 16,
    backgroundColor: "#FFFFFF",
    borderWidth: StyleSheet.hairlineWidth,
    borderColor: "rgba(60,60,67,0.16)",
  },
  noticeTitle: { fontSize: 14, fontWeight: "700", color: c.ink },
  noticeCopy: { fontSize: 12.5, color: c.dim, lineHeight: 18, marginTop: 5 },
  problemBox: {
    flexDirection: "row",
    gap: 11,
    marginHorizontal: 14,
    marginTop: 14,
    marginBottom: 4,
    padding: 13,
    borderRadius: 14,
    backgroundColor: "rgba(255,59,48,0.09)",
    borderWidth: StyleSheet.hairlineWidth,
    borderColor: "rgba(255,59,48,0.24)",
  },
  problemText: { flex: 1, minWidth: 0, gap: 3 },
  problemTitle: { fontSize: 14, fontWeight: "700", color: c.ink },
  problemCopy: { fontSize: 12.5, color: c.err, lineHeight: 17 },
  problemHint: { fontSize: 12, color: c.dim, lineHeight: 16 },
  /* gateway fields */
  fieldLabel: {
    fontSize: 12,
    fontWeight: "700",
    color: c.dim,
    marginTop: 14,
    marginBottom: 6,
    marginHorizontal: 14,
  },
  field: {
    marginHorizontal: 14,
    backgroundColor: "rgba(118,118,128,0.10)",
    borderRadius: 12,
    paddingHorizontal: 13,
    paddingVertical: 11,
    fontSize: 14,
    color: c.ink,
  },
  okNote: {
    fontSize: 12,
    color: c.ok,
    marginTop: 10,
    marginHorizontal: 14,
    lineHeight: 17,
  },
  sectionCopy: {
    fontSize: 12.5,
    color: c.dim,
    lineHeight: 18,
    marginTop: 10,
    marginHorizontal: 14,
  },
  scannerSlot: {
    marginTop: 18,
    marginHorizontal: 14,
    marginBottom: 4,
  },
  reconnect: {
    flexDirection: "row",
    gap: 10,
    flexWrap: "wrap",
    marginTop: 14,
    marginHorizontal: 14,
    marginBottom: 14,
  },
  wsAdd: { flexDirection: "row", alignItems: "center", gap: 8, marginTop: 2 },
  wsField: { flex: 1 },
});
