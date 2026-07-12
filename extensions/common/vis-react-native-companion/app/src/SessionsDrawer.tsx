import React, { useEffect, useMemo, useRef, useState } from "react";
import {
  Animated,
  FlatList,
  Pressable,
  StyleSheet,
  Text,
  TextInput,
  View
} from "react-native";
import { Feather } from "@expo/vector-icons";
import { LinearGradient } from "expo-linear-gradient";

import { GatewayGroup, SessionSoul } from "./VisClient";
import { c, mono, projectColor, relTime, shortId, tint } from "./theme";
import { ActionBtn, DialogModal } from "./ui";

const PANEL_W = 316;

/* The drawer renders a mixed list: project headers + their sessions + an
   "ungrouped" bucket. A discriminated union keeps the FlatList virtualized
   while carrying both row kinds. */
type Row =
  | { kind: "group"; group: GatewayGroup; count: number; collapsed: boolean }
  | { kind: "ungrouped-label" }
  | { kind: "session"; session: SessionSoul };

const recency = (s: SessionSoul): number =>
  s.last_active_at ?? s.updated_at ?? s.created_at ?? 0;

/* Slide-in sessions panel — the web channel's sidebar, warm-paper style:
   sessions organized into projects (session groups), each with its own accent
   color, status dot, title, channel tag + age, rename / delete / move per row. */
export const SessionsDrawer = ({
  visible,
  sessions,
  groups,
  activeId,
  onClose,
  onSelect,
  onCreate,
  onDelete,
  onRename,
  onCreateGroup,
  onRenameGroup,
  onDeleteGroup,
  onAssign
}: {
  visible: boolean;
  sessions: SessionSoul[];
  groups: GatewayGroup[];
  activeId: string | null;
  onClose: () => void;
  onSelect: (session: SessionSoul) => void;
  onCreate: () => void;
  onDelete: (session: SessionSoul) => void;
  onRename: (session: SessionSoul, title: string) => void;
  onCreateGroup: (name: string) => void;
  onRenameGroup: (group: GatewayGroup, name: string) => void;
  onDeleteGroup: (group: GatewayGroup) => void;
  onAssign: (session: SessionSoul, groupId: string | null) => void;
}) => {
  const slide = useRef(new Animated.Value(-PANEL_W)).current;
  const [confirmDelete, setConfirmDelete] = useState<SessionSoul | null>(null);
  const [renaming, setRenaming] = useState<SessionSoul | null>(null);
  const [renameDraft, setRenameDraft] = useState("");
  const [moving, setMoving] = useState<SessionSoul | null>(null);
  const [newProject, setNewProject] = useState(false);
  const [projectDraft, setProjectDraft] = useState("");
  const [editGroup, setEditGroup] = useState<GatewayGroup | null>(null);
  const [groupDraft, setGroupDraft] = useState("");
  const [confirmGroupDelete, setConfirmGroupDelete] = useState<GatewayGroup | null>(null);
  const [collapsed, setCollapsed] = useState<Record<string, boolean>>({});

  useEffect(() => {
    Animated.timing(slide, {
      toValue: visible ? 0 : -PANEL_W,
      duration: 180,
      useNativeDriver: true
    }).start();
  }, [slide, visible]);

  /* Flatten projects + sessions into one virtualized row list. Projects come
     first (by position, then name), each followed by its sessions (newest
     first); ungrouped sessions land in a trailing bucket. */
  const rows = useMemo<Row[]>(() => {
    const byGroup = new Map<string, SessionSoul[]>();
    const ungrouped: SessionSoul[] = [];
    for (const s of sessions) {
      if (s.group_id) {
        const list = byGroup.get(s.group_id) ?? [];
        list.push(s);
        byGroup.set(s.group_id, list);
      } else {
        ungrouped.push(s);
      }
    }
    const byRecency = (a: SessionSoul, b: SessionSoul) => recency(b) - recency(a);
    ungrouped.sort(byRecency);

    const sortedGroups = [...groups].sort(
      (a, b) => (a.position ?? 0) - (b.position ?? 0) || a.name.localeCompare(b.name)
    );

    const out: Row[] = [];
    for (const g of sortedGroups) {
      const members = (byGroup.get(g.id) ?? []).sort(byRecency);
      const isCollapsed = collapsed[g.id] ?? false;
      out.push({ kind: "group", group: g, count: members.length, collapsed: isCollapsed });
      if (!isCollapsed) for (const s of members) out.push({ kind: "session", session: s });
    }
    /* Sessions whose group_id points at a project the list doesn't know about
       (e.g. a channel-scoped one filtered out) still surface as ungrouped. */
    for (const [gid, members] of byGroup) {
      if (!groups.some((g) => g.id === gid)) ungrouped.push(...members);
    }
    if (ungrouped.length && sortedGroups.length) out.push({ kind: "ungrouped-label" });
    for (const s of ungrouped.sort(byRecency)) out.push({ kind: "session", session: s });
    return out;
  }, [sessions, groups, collapsed]);

  const toggle = (gid: string) => setCollapsed((m) => ({ ...m, [gid]: !(m[gid] ?? false) }));

  const renderSession = (item: SessionSoul, inProject: boolean, accent?: string) => {
    const active = item.id === activeId;
    return (
      <Pressable
        onPress={() => onSelect(item)}
        style={({ pressed }) => [
          styles.row,
          inProject && styles.rowIndent,
          inProject && accent ? { borderLeftColor: accent } : null,
          active && styles.rowActive,
          pressed && { opacity: 0.85 }
        ]}
      >
        <View
          style={[
            styles.dot,
            active && styles.dotActive,
            !active && accent ? { backgroundColor: accent } : null
          ]}
        />
        <View style={styles.rowBody}>
          <Text numberOfLines={1} style={[styles.rowTitle, active && styles.rowTitleActive]}>
            {item.title?.trim() || `session ${shortId(item.id)}`}
          </Text>
          <Text numberOfLines={1} style={[styles.rowMeta, active && styles.rowMetaActive]}>
            {[item.channel, relTime(recency(item)), shortId(item.id)].filter(Boolean).join(" \u00b7 ")}
          </Text>
        </View>
        <Pressable hitSlop={6} onPress={() => setMoving(item)} style={styles.rowIcon}>
          <Feather name="move" size={13} color={active ? c.amberBright : c.dim} />
        </Pressable>
        <Pressable
          hitSlop={6}
          onPress={() => {
            setRenaming(item);
            setRenameDraft(item.title ?? "");
          }}
          style={styles.rowIcon}
        >
          <Feather name="edit-2" size={13} color={active ? c.amberBright : c.dim} />
        </Pressable>
        <Pressable hitSlop={6} onPress={() => setConfirmDelete(item)} style={styles.rowIcon}>
          <Feather name="trash-2" size={13} color={active ? c.amberBright : c.dim} />
        </Pressable>
      </Pressable>
    );
  };

  const renderRow = (item: Row) => {
    if (item.kind === "ungrouped-label") {
      return (
        <View style={styles.ungroupedHead}>
          <Feather name="inbox" size={13} color={c.dim} />
          <Text style={styles.ungroupedName}>ungrouped</Text>
        </View>
      );
    }
    if (item.kind === "group") {
      const g = item.group;
      const accent = projectColor(g.id, g.color);
      return (
        <View
          style={[
            styles.projectHead,
            { backgroundColor: tint(accent, "14"), borderColor: tint(accent, "44") }
          ]}
        >
          <View style={[styles.projectBar, { backgroundColor: accent }]} />
          <Pressable hitSlop={6} onPress={() => toggle(g.id)} style={styles.projectChevron}>
            <Feather name={item.collapsed ? "chevron-right" : "chevron-down"} size={14} color={c.ink} />
          </Pressable>
          <Feather name="layers" size={13} color={accent} />
          <Pressable style={styles.projectNameBtn} onPress={() => toggle(g.id)}>
            <Text numberOfLines={1} style={styles.projectName}>
              {g.name}
            </Text>
          </Pressable>
          <View style={[styles.projectCount, { backgroundColor: tint(accent, "26") }]}>
            <Text style={[styles.projectCountText, { color: accent }]}>{item.count}</Text>
          </View>
          <Pressable
            hitSlop={6}
            onPress={() => {
              setEditGroup(g);
              setGroupDraft(g.name);
            }}
            style={styles.rowIcon}
          >
            <Feather name="edit-2" size={12} color={c.dim} />
          </Pressable>
          <Pressable hitSlop={6} onPress={() => setConfirmGroupDelete(g)} style={styles.rowIcon}>
            <Feather name="trash-2" size={12} color={c.dim} />
          </Pressable>
        </View>
      );
    }
    /* a session row — indented + accent-tinted when it sits under a project */
    const parent = item.session.group_id;
    const inProject = groups.length > 0 && !!parent;
    const accent = inProject
      ? projectColor(parent, groups.find((g) => g.id === parent)?.color)
      : undefined;
    return renderSession(item.session, inProject, accent);
  };

  if (!visible) return null;
  return (
    <View style={styles.overlay}>
      <View style={styles.root}>
        <Pressable style={styles.scrim} onPress={onClose} />
        <Animated.View style={[styles.panel, { transform: [{ translateX: slide }] }]}>
          <LinearGradient
            colors={[c.amberBright, c.amber]}
            start={{ x: 0, y: 0 }}
            end={{ x: 1, y: 1 }}
            style={styles.titleBar}
          >
            <Text style={styles.title}>sessions</Text>
            <Pressable onPress={onClose} hitSlop={8}>
              <Feather name="x" size={16} color={c.amberInk} />
            </Pressable>
          </LinearGradient>

          <FlatList
            data={rows}
            keyExtractor={(item, i) =>
              item.kind === "group"
                ? `g:${item.group.id}`
                : item.kind === "session"
                  ? `s:${item.session.id}`
                  : `label:${i}`
            }
            contentContainerStyle={styles.listPad}
            renderItem={({ item }) => renderRow(item)}
            ListEmptyComponent={<Text style={styles.empty}>no sessions yet</Text>}
          />

          <View style={styles.footRow}>
            <Pressable
              onPress={() => {
                setProjectDraft("");
                setNewProject(true);
              }}
              style={({ pressed }) => [styles.footBtn, pressed && { opacity: 0.7 }]}
            >
              <Feather name="layers" size={15} color={c.ink} />
              <Text style={styles.footLabel}>project</Text>
            </Pressable>
            <Pressable
              onPress={onCreate}
              style={({ pressed }) => [styles.footBtn, styles.footBtnPrimary, pressed && { opacity: 0.7 }]}
            >
              <Feather name="plus" size={15} color={c.amberInk} />
              <Text style={[styles.footLabel, styles.footLabelPrimary]}>session</Text>
            </Pressable>
          </View>
        </Animated.View>
      </View>

      {/* delete session */}
      <DialogModal
        visible={confirmDelete != null}
        title="Delete session"
        tone="error"
        onClose={() => setConfirmDelete(null)}
      >
        <Text style={styles.confirmText}>
          Delete {confirmDelete?.title?.trim() || `session ${shortId(confirmDelete?.id)}`}? The
          transcript is closed for good.
        </Text>
        <View style={styles.confirmRow}>
          <View style={{ flex: 1 }}>
            <ActionBtn label="Cancel" tone="ghost" onPress={() => setConfirmDelete(null)} />
          </View>
          <View style={{ flex: 1 }}>
            <ActionBtn
              label="Delete"
              tone="danger"
              onPress={() => {
                if (confirmDelete) onDelete(confirmDelete);
                setConfirmDelete(null);
              }}
            />
          </View>
        </View>
      </DialogModal>

      {/* rename session */}
      <DialogModal visible={renaming != null} title="Rename session" onClose={() => setRenaming(null)}>
        <TextInput
          value={renameDraft}
          onChangeText={setRenameDraft}
          placeholder="session title"
          placeholderTextColor={c.dim}
          style={styles.renameInput}
          autoFocus
        />
        <ActionBtn
          label="Rename"
          tone="amber"
          disabled={!renameDraft.trim()}
          onPress={() => {
            if (renaming && renameDraft.trim()) onRename(renaming, renameDraft.trim());
            setRenaming(null);
          }}
        />
      </DialogModal>

      {/* move session to a project */}
      <DialogModal visible={moving != null} title="Move to project" onClose={() => setMoving(null)}>
        <Pressable
          style={[styles.pickRow, !moving?.group_id && styles.pickRowActive]}
          onPress={() => {
            if (moving) onAssign(moving, null);
            setMoving(null);
          }}
        >
          <Feather name="inbox" size={14} color={c.ink} />
          <Text style={styles.pickLabel}>No project</Text>
          {!moving?.group_id && <Feather name="check" size={14} color={c.roleVis} />}
        </Pressable>
        {groups.map((g) => {
          const on = moving?.group_id === g.id;
          const accent = projectColor(g.id, g.color);
          return (
            <Pressable
              key={g.id}
              style={[styles.pickRow, on && styles.pickRowActive]}
              onPress={() => {
                if (moving) onAssign(moving, g.id);
                setMoving(null);
              }}
            >
              <Feather name="layers" size={14} color={accent} />
              <Text numberOfLines={1} style={styles.pickLabel}>
                {g.name}
              </Text>
              {on && <Feather name="check" size={14} color={c.roleVis} />}
            </Pressable>
          );
        })}
        {groups.length === 0 && (
          <Text style={styles.empty}>no projects yet — create one from the drawer footer</Text>
        )}
      </DialogModal>

      {/* new project */}
      <DialogModal visible={newProject} title="New project" onClose={() => setNewProject(false)}>
        <TextInput
          value={projectDraft}
          onChangeText={setProjectDraft}
          placeholder="project name"
          placeholderTextColor={c.dim}
          style={styles.renameInput}
          autoFocus
        />
        <ActionBtn
          label="Create"
          tone="amber"
          disabled={!projectDraft.trim()}
          onPress={() => {
            if (projectDraft.trim()) onCreateGroup(projectDraft.trim());
            setNewProject(false);
          }}
        />
      </DialogModal>

      {/* rename project */}
      <DialogModal visible={editGroup != null} title="Rename project" onClose={() => setEditGroup(null)}>
        <TextInput
          value={groupDraft}
          onChangeText={setGroupDraft}
          placeholder="project name"
          placeholderTextColor={c.dim}
          style={styles.renameInput}
          autoFocus
        />
        <ActionBtn
          label="Rename"
          tone="amber"
          disabled={!groupDraft.trim()}
          onPress={() => {
            if (editGroup && groupDraft.trim()) onRenameGroup(editGroup, groupDraft.trim());
            setEditGroup(null);
          }}
        />
      </DialogModal>

      {/* delete project */}
      <DialogModal
        visible={confirmGroupDelete != null}
        title="Delete project"
        tone="error"
        onClose={() => setConfirmGroupDelete(null)}
      >
        <Text style={styles.confirmText}>
          Delete project {confirmGroupDelete?.name}? Its sessions are kept — they just scatter back to
          ungrouped.
        </Text>
        <View style={styles.confirmRow}>
          <View style={{ flex: 1 }}>
            <ActionBtn label="Cancel" tone="ghost" onPress={() => setConfirmGroupDelete(null)} />
          </View>
          <View style={{ flex: 1 }}>
            <ActionBtn
              label="Delete"
              tone="danger"
              onPress={() => {
                if (confirmGroupDelete) onDeleteGroup(confirmGroupDelete);
                setConfirmGroupDelete(null);
              }}
            />
          </View>
        </View>
      </DialogModal>
    </View>
  );
};

const styles = StyleSheet.create({
  overlay: { ...StyleSheet.absoluteFillObject, zIndex: 50, elevation: 50 },
  root: { flex: 1, flexDirection: "row" },
  scrim: { ...StyleSheet.absoluteFillObject, backgroundColor: "rgba(38,38,38,0.45)" },
  panel: {
    width: PANEL_W,
    backgroundColor: c.paper,
    borderRightWidth: 1,
    borderRightColor: c.line,
    paddingTop: 54,
    flex: 1
  },
  titleBar: {
    paddingHorizontal: 14,
    paddingVertical: 10,
    flexDirection: "row",
    alignItems: "center",
    justifyContent: "space-between"
  },
  title: {
    fontFamily: mono,
    fontSize: 13,
    fontWeight: "700",
    letterSpacing: 1.5,
    textTransform: "uppercase",
    color: c.amberInk
  },
  listPad: { padding: 12, gap: 7 },
  projectHead: {
    flexDirection: "row",
    alignItems: "center",
    gap: 8,
    paddingHorizontal: 8,
    paddingVertical: 8,
    marginTop: 4,
    borderWidth: 1,
    borderRadius: 12,
    overflow: "hidden"
  },
  projectBar: { position: "absolute", left: 0, top: 0, bottom: 0, width: 4 },
  projectChevron: { padding: 2, marginLeft: 2 },
  projectNameBtn: { flex: 1, minWidth: 0 },
  projectName: {
    fontFamily: mono,
    fontSize: 11.5,
    fontWeight: "700",
    letterSpacing: 0.5,
    textTransform: "uppercase",
    color: c.ink
  },
  projectCount: {
    minWidth: 22,
    paddingHorizontal: 7,
    paddingVertical: 2,
    borderRadius: 999,
    alignItems: "center"
  },
  projectCountText: { fontFamily: mono, fontSize: 10.5, fontWeight: "700" },
  ungroupedHead: {
    flexDirection: "row",
    alignItems: "center",
    gap: 6,
    paddingHorizontal: 6,
    paddingVertical: 6,
    marginTop: 4
  },
  ungroupedName: {
    fontFamily: mono,
    fontSize: 11,
    fontWeight: "700",
    letterSpacing: 0.5,
    textTransform: "uppercase",
    color: c.dim
  },
  row: {
    flexDirection: "row",
    alignItems: "center",
    gap: 8,
    borderWidth: 1,
    borderColor: c.line,
    backgroundColor: c.field,
    borderRadius: 10,
    paddingHorizontal: 10,
    paddingVertical: 10
  },
  rowIndent: { marginLeft: 14, borderLeftWidth: 3, borderLeftColor: c.lineSoft },
  rowActive: { backgroundColor: c.ink, borderColor: c.ink },
  dot: { width: 8, height: 8, borderRadius: 4, backgroundColor: c.line },
  dotActive: { backgroundColor: c.amberBright },
  rowBody: { flex: 1, gap: 2 },
  rowTitle: { color: c.ink, fontSize: 14, fontWeight: "700" },
  rowTitleActive: { color: c.amberBright },
  rowMeta: { fontFamily: mono, fontSize: 10.5, color: c.dim },
  rowMetaActive: { color: c.lineSoft },
  rowIcon: { padding: 4 },
  empty: { fontFamily: mono, fontSize: 12, color: c.dim, textAlign: "center", paddingVertical: 24 },
  footRow: { flexDirection: "row", gap: 8, marginHorizontal: 12, marginBottom: 24, marginTop: 6 },
  footBtn: {
    flex: 1,
    flexDirection: "row",
    alignItems: "center",
    justifyContent: "center",
    gap: 6,
    borderWidth: 1,
    borderStyle: "dashed",
    borderColor: c.line,
    borderRadius: 12,
    paddingVertical: 12,
    backgroundColor: c.tsepBg
  },
  footBtnPrimary: { borderStyle: "solid", backgroundColor: c.chipBg, borderColor: c.amber },
  footLabel: {
    fontFamily: mono,
    fontSize: 12,
    fontWeight: "700",
    letterSpacing: 1,
    textTransform: "uppercase",
    color: c.ink
  },
  footLabelPrimary: { color: c.amberInk },
  confirmText: { color: c.ink, fontSize: 14, lineHeight: 20 },
  confirmRow: { flexDirection: "row", gap: 8 },
  pickRow: {
    flexDirection: "row",
    alignItems: "center",
    gap: 10,
    borderWidth: 1,
    borderColor: c.line,
    backgroundColor: c.field,
    borderRadius: 10,
    paddingHorizontal: 10,
    paddingVertical: 10
  },
  pickRowActive: { borderColor: c.amber, backgroundColor: c.chipBg },
  pickLabel: { flex: 1, color: c.ink, fontSize: 13, fontWeight: "600" },
  renameInput: {
    borderWidth: 1,
    borderColor: c.line,
    backgroundColor: c.tsepBg,
    color: c.ink,
    fontFamily: mono,
    fontSize: 13,
    borderRadius: 8,
    paddingHorizontal: 10,
    paddingVertical: 8
  }
});
