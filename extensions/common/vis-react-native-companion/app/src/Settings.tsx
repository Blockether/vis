import React, { useCallback, useEffect, useMemo, useRef, useState } from "react";
import {
  Pressable,
  ScrollView,
  StyleSheet,
  Text,
  TextInput,
  View,
  useWindowDimensions
} from "react-native";
import { Feather } from "@expo/vector-icons";

import { c, mono } from "./theme";
import { ActionBtn } from "./ui";
import type { SettingsGroup, ToggleRow, VisGatewayClient } from "./VisClient";

/* ── the web channel's Settings dialog, native ──────────────────────
   VS Code-style: full-width live search + count, a left category rail,
   grouped toggle rows on the right (boolean → pill switch, enum → mono
   cycle chip) — same registry (`toggles-for-channel`), same grouping,
   served by the gateway's canonical /v1/settings API. The app-local Gateway
   fields ride along as the last group. */

const GATEWAY_GROUP = "gateway";

/* The web's .switch/.knob — 40×22 pill, amber when on, sliding knob. */
const PillSwitch = ({ on, onPress }: { on: boolean; onPress: () => void }) => (
  <Pressable onPress={onPress} hitSlop={6} style={[st.switch, on && st.switchOn]}>
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
  onGatewayUrl,
  onToken,
  onReconnect
}: {
  client: VisGatewayClient;
  gatewayUrl: string;
  token: string;
  connecting: boolean;
  onGatewayUrl: (value: string) => void;
  onToken: (value: string) => void;
  onReconnect: () => void;
}) => {
  const { height: winH } = useWindowDimensions();
  const [groups, setGroups] = useState<SettingsGroup[]>([]);
  const [stale, setStale] = useState(false);
  const [query, setQuery] = useState("");
  const [active, setActive] = useState<string | null>(null);
  const scrollRef = useRef<ScrollView | null>(null);
  const sectionY = useRef<Record<string, number>>({});

  useEffect(() => {
    let alive = true;
    client
      .settingsGroups()
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
              toggles: g.toggles.map((t) => (t.id === row.id ? fresh : t))
            }))
          )
        )
        .catch(() => setStale(true));
    },
    [client]
  );

  const q = query.trim().toLowerCase();
  const visible = useMemo(
    () =>
      groups
        .map((g) => ({ ...g, toggles: g.toggles.filter((t) => rowMatches(t, q)) }))
        .filter((g) => g.toggles.length > 0),
    [groups, q]
  );
  const count = visible.reduce((n, g) => n + g.toggles.length, 0);
  const gatewayVisible = q === "" || "gateway url bearer token reconnect".includes(q);

  const jumpTo = (id: string) => {
    setActive(id);
    const y = sectionY.current[id];
    if (y != null) scrollRef.current?.scrollTo({ y: Math.max(0, y - 4), animated: true });
  };

  return (
    <View style={[st.pane, { height: Math.min(560, Math.round(winH * 0.72)) }]}>
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
          {count + (gatewayVisible ? 2 : 0)} settings
        </Text>
      </View>

      <View style={st.cols}>
        {/* ── category rail — the web's .settings-toc ── */}
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
              style={[st.tocItem, active === GATEWAY_GROUP && st.tocItemActive]}
            >
              <Text
                numberOfLines={1}
                style={[st.tocLabel, active === GATEWAY_GROUP && st.tocLabelActive]}
              >
                Gateway
              </Text>
            </Pressable>
          ) : null}
        </ScrollView>

        {/* ── grouped rows — the web's .settings-groups ── */}
        <ScrollView ref={scrollRef} style={st.groups} contentContainerStyle={st.groupsBody}>
          {stale ? (
            <Text style={st.staleNote}>
              This gateway was started before the /v1/settings API existed — restart
              the vis gateway to manage engine toggles here.
            </Text>
          ) : null}
          {visible.map((g) => (
            <View
              key={g.id}
              onLayout={(e) => {
                sectionY.current[g.id] = e.nativeEvent.layout.y;
              }}
            >
              <Text style={st.groupTitle}>{g.title}</Text>
              {g.toggles.map((row) => (
                <View key={row.id} style={st.row}>
                  <View style={st.rowText}>
                    <Text style={st.rowLabel}>{row.label}</Text>
                    {row.description ? <Text style={st.rowDesc}>{row.description}</Text> : null}
                  </View>
                  {row.choices?.length ? (
                    <Pressable onPress={() => mutate(row)} hitSlop={6} style={st.cycle}>
                      <Text style={st.cycleLabel}>{row.value ?? "?"}</Text>
                    </Pressable>
                  ) : (
                    <PillSwitch on={row.enabled === true} onPress={() => mutate(row)} />
                  )}
                </View>
              ))}
            </View>
          ))}

          {/* ── app-local Gateway group ── */}
          {gatewayVisible ? (
            <View
              onLayout={(e) => {
                sectionY.current[GATEWAY_GROUP] = e.nativeEvent.layout.y;
              }}
            >
              <Text style={st.groupTitle}>Gateway</Text>
              <Text style={st.fieldLabel}>gateway url</Text>
              <TextInput
                value={gatewayUrl}
                onChangeText={onGatewayUrl}
                autoCapitalize="none"
                autoCorrect={false}
                style={st.field}
              />
              <Text style={st.fieldLabel}>bearer token</Text>
              <TextInput
                value={token}
                onChangeText={onToken}
                autoCapitalize="none"
                autoCorrect={false}
                secureTextEntry
                style={st.field}
              />
              <View style={st.reconnect}>
                <ActionBtn
                  label={connecting ? "connecting…" : "Reconnect"}
                  tone="amber"
                  disabled={connecting}
                  onPress={onReconnect}
                />
              </View>
            </View>
          ) : null}
        </ScrollView>
      </View>
    </View>
  );
};

const st = StyleSheet.create({
  pane: { alignSelf: "stretch" },
  /* .settings-search */
  search: {
    flexDirection: "row",
    alignItems: "center",
    gap: 6,
    paddingHorizontal: 10,
    paddingVertical: 7,
    borderBottomWidth: StyleSheet.hairlineWidth,
    borderBottomColor: c.line
  },
  searchInput: {
    flex: 1,
    minWidth: 0,
    padding: 0,
    fontSize: 13,
    color: c.ink
  },
  count: { fontFamily: mono, fontSize: 9.5, color: c.dim },
  /* .settings-cols */
  cols: { flex: 1, flexDirection: "row", minHeight: 0 },
  /* .settings-toc */
  toc: {
    width: 96,
    flexGrow: 0,
    flexShrink: 0,
    borderRightWidth: StyleSheet.hairlineWidth,
    borderRightColor: c.line
  },
  tocBody: { padding: 6 },
  tocItem: { paddingVertical: 6, paddingHorizontal: 8 },
  tocItemActive: { backgroundColor: c.chipBg },
  tocLabel: { fontSize: 11.5, color: c.dim },
  tocLabelActive: { color: c.ink, fontWeight: "600" },
  /* .settings-groups */
  groups: { flex: 1, minWidth: 0 },
  groupsBody: { paddingHorizontal: 12, paddingBottom: 14 },
  groupTitle: {
    fontSize: 9.5,
    fontFamily: mono,
    textTransform: "uppercase",
    letterSpacing: 1.2,
    color: c.dim,
    marginTop: 10,
    marginBottom: 2
  },
  /* .toggle-row */
  row: {
    flexDirection: "row",
    alignItems: "center",
    gap: 10,
    paddingVertical: 8,
    borderBottomWidth: StyleSheet.hairlineWidth,
    borderBottomColor: c.lineSoft
  },
  rowText: { flex: 1, minWidth: 0 },
  rowLabel: { fontSize: 12.5, fontWeight: "600", color: c.ink },
  rowDesc: { fontSize: 10.5, color: c.dim, marginTop: 1 },
  /* .switch / .knob */
  switch: {
    width: 40,
    height: 22,
    borderRadius: 11,
    backgroundColor: c.hair,
    justifyContent: "center"
  },
  switchOn: { backgroundColor: c.amber },
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
    elevation: 2
  },
  knobOn: { left: 20 },
  /* .toggle-cycle */
  cycle: { backgroundColor: c.chipBg, paddingHorizontal: 10, paddingVertical: 4 },
  cycleLabel: { fontFamily: mono, fontSize: 11, color: c.chipInk },
  staleNote: { fontSize: 11, color: c.dim, marginTop: 10, lineHeight: 15 },
  /* gateway fields */
  fieldLabel: {
    fontFamily: mono,
    fontSize: 9.5,
    letterSpacing: 1,
    textTransform: "uppercase",
    color: c.dim,
    marginTop: 8,
    marginBottom: 2
  },
  field: {
    backgroundColor: c.paper,
    borderBottomWidth: StyleSheet.hairlineWidth,
    borderBottomColor: c.line,
    paddingHorizontal: 8,
    paddingVertical: 6,
    fontFamily: mono,
    fontSize: 12,
    color: c.ink
  },
  reconnect: { marginTop: 10 }
});
