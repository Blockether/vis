import React, { useEffect, useRef, useState } from "react";
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

import { SessionSoul } from "./VisClient";
import { c, mono, relTime, shortId } from "./theme";
import { ActionBtn, DialogModal } from "./ui";

const PANEL_W = 316;

/* Slide-in sessions panel — the web channel's sidebar, warm-paper style:
   status dot, title, channel tag + age, rename / delete per row. */
export const SessionsDrawer = ({
  visible,
  sessions,
  activeId,
  onClose,
  onSelect,
  onCreate,
  onDelete,
  onRename
}: {
  visible: boolean;
  sessions: SessionSoul[];
  activeId: string | null;
  onClose: () => void;
  onSelect: (session: SessionSoul) => void;
  onCreate: () => void;
  onDelete: (session: SessionSoul) => void;
  onRename: (session: SessionSoul, title: string) => void;
}) => {
  const slide = useRef(new Animated.Value(-PANEL_W)).current;
  const [confirmDelete, setConfirmDelete] = useState<SessionSoul | null>(null);
  const [renaming, setRenaming] = useState<SessionSoul | null>(null);
  const [renameDraft, setRenameDraft] = useState("");

  useEffect(() => {
    Animated.timing(slide, {
      toValue: visible ? 0 : -PANEL_W,
      duration: 180,
      useNativeDriver: true
    }).start();
  }, [slide, visible]);

  const sorted = [...sessions].sort(
    (a, b) =>
      (b.last_active_at ?? b.updated_at ?? b.created_at ?? 0) -
      (a.last_active_at ?? a.updated_at ?? a.created_at ?? 0)
  );

  if (!visible) return null;
  return (
    <View style={styles.overlay}>
      <View style={styles.root}>
        <Pressable style={styles.scrim} onPress={onClose} />
        <Animated.View style={[styles.panel, { transform: [{ translateX: slide }] }]}>
          <View style={styles.titleBar}>
            <Text style={styles.title}>sessions</Text>
            <Pressable onPress={onClose} hitSlop={8}>
              <Feather name="x" size={16} color={c.amberInk} />
            </Pressable>
          </View>

          <FlatList
            data={sorted}
            keyExtractor={(s) => s.id}
            contentContainerStyle={styles.listPad}
            renderItem={({ item }) => {
              const active = item.id === activeId;
              return (
                <Pressable
                  onPress={() => onSelect(item)}
                  style={({ pressed }) => [
                    styles.row,
                    active && styles.rowActive,
                    pressed && { opacity: 0.85 }
                  ]}
                >
                  <View style={[styles.dot, active && styles.dotActive]} />
                  <View style={styles.rowBody}>
                    <Text
                      numberOfLines={1}
                      style={[styles.rowTitle, active && styles.rowTitleActive]}
                    >
                      {item.title?.trim() || `session ${shortId(item.id)}`}
                    </Text>
                    <Text numberOfLines={1} style={[styles.rowMeta, active && styles.rowMetaActive]}>
                      {[item.channel, relTime(item.last_active_at ?? item.updated_at ?? item.created_at), shortId(item.id)]
                        .filter(Boolean)
                        .join(" \u00b7 ")}
                    </Text>
                  </View>
                  <Pressable
                    hitSlop={6}
                    onPress={() => {
                      setRenaming(item);
                      setRenameDraft(item.title ?? "");
                    }}
                    style={styles.rowIcon}
                  >
                    <Feather name="edit-2" size={14} color={active ? c.amberBright : c.dim} />
                  </Pressable>
                  <Pressable hitSlop={6} onPress={() => setConfirmDelete(item)} style={styles.rowIcon}>
                    <Feather name="trash-2" size={14} color={active ? c.amberBright : c.dim} />
                  </Pressable>
                </Pressable>
              );
            }}
            ListEmptyComponent={<Text style={styles.empty}>no sessions yet</Text>}
          />

          <Pressable onPress={onCreate} style={({ pressed }) => [styles.newBtn, pressed && { opacity: 0.7 }]}>
            <Feather name="plus" size={15} color={c.ink} />
            <Text style={styles.newLabel}>new session</Text>
          </Pressable>
        </Animated.View>
      </View>

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
    backgroundColor: c.amber,
    paddingHorizontal: 12,
    paddingVertical: 8,
    flexDirection: "row",
    alignItems: "center",
    justifyContent: "space-between"
  },
  title: {
    fontFamily: mono,
    fontSize: 13,
    fontWeight: "700",
    letterSpacing: 1,
    textTransform: "uppercase",
    color: c.amberInk
  },
  listPad: { padding: 10, gap: 8 },
  row: {
    flexDirection: "row",
    alignItems: "center",
    gap: 8,
    borderWidth: 1,
    borderColor: c.line,
    backgroundColor: c.field,
    paddingHorizontal: 10,
    paddingVertical: 10
  },
  rowActive: { backgroundColor: c.ink, borderColor: c.ink },
  dot: { width: 8, height: 8, backgroundColor: c.line },
  dotActive: { backgroundColor: c.amberBright },
  rowBody: { flex: 1, gap: 2 },
  rowTitle: { color: c.ink, fontSize: 14, fontWeight: "700" },
  rowTitleActive: { color: c.amberBright },
  rowMeta: { fontFamily: mono, fontSize: 10.5, color: c.dim },
  rowMetaActive: { color: c.lineSoft },
  rowIcon: { padding: 4 },
  empty: { fontFamily: mono, fontSize: 12, color: c.dim, textAlign: "center", paddingVertical: 24 },
  newBtn: {
    flexDirection: "row",
    alignItems: "center",
    justifyContent: "center",
    gap: 6,
    borderWidth: 1,
    borderStyle: "dashed",
    borderColor: c.line,
    marginHorizontal: 10,
    marginBottom: 24,
    paddingVertical: 12,
    backgroundColor: c.tsepBg
  },
  newLabel: {
    fontFamily: mono,
    fontSize: 12,
    fontWeight: "700",
    letterSpacing: 1,
    textTransform: "uppercase",
    color: c.ink
  },
  confirmText: { color: c.ink, fontSize: 14, lineHeight: 20 },
  confirmRow: { flexDirection: "row", gap: 8 },
  renameInput: {
    borderWidth: 1,
    borderColor: c.line,
    backgroundColor: c.tsepBg,
    color: c.ink,
    fontFamily: mono,
    fontSize: 13,
    paddingHorizontal: 10,
    paddingVertical: 8
  }
});