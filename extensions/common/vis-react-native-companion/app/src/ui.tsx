import React from "react";
import { KeyboardAvoidingView, Platform, Pressable, StyleSheet, Text, View } from "react-native";
import { Feather } from "@expo/vector-icons";
import { LinearGradient } from "expo-linear-gradient";

import { c, mono } from "./theme";

/* Flat square icon button — TUI header button: no chrome until pressed. */
export const IconBtn = ({
  name,
  onPress,
  size = 15,
  color = c.ink,
  active = false,
  disabled = false
}: {
  name: keyof typeof Feather.glyphMap;
  onPress: () => void;
  size?: number;
  color?: string;
  active?: boolean;
  disabled?: boolean;
}) => (
  <Pressable
    onPress={onPress}
    disabled={disabled}
    hitSlop={8}
    style={({ pressed }) => [
      styles.iconBtn,
      active && styles.iconBtnActive,
      pressed && styles.iconBtnPressed,
      disabled && styles.iconBtnDisabled
    ]}
  >
    <Feather name={name} size={size} color={active ? c.amberBright : color} />
  </Pressable>
);

/* The vis dialog — amber title bar, warm body, square corners.
   Rendered as an ABSOLUTE OVERLAY, not an RN <Modal>: transparent modals
   are unreliable on the iOS 26 simulator and cannot nest, and an overlay
   is flatter anyway. Mount it near the root so it covers the screen. */
export const DialogModal = ({
  visible,
  title,
  onClose,
  tone = "amber",
  flush = false,
  children
}: {
  visible: boolean;
  title: string;
  onClose: () => void;
  tone?: "amber" | "error";
  /* flush = edge-to-edge body (the Settings pane brings its own chrome). */
  flush?: boolean;
  children: React.ReactNode;
}) => {
  if (!visible) return null;
  return (
    <KeyboardAvoidingView
      behavior={Platform.select({ ios: "padding", default: undefined })}
      style={styles.overlay}
    >
      <Pressable style={styles.scrim} onPress={onClose}>
        <Pressable style={[styles.dialog, tone === "error" && styles.dialogError]} onPress={() => {}}>
          <LinearGradient
            colors={tone === "error" ? [c.err, c.err] : [c.amberBright, c.amber]}
            start={{ x: 0, y: 0 }}
            end={{ x: 0, y: 1 }}
            style={styles.titleBar}
          >
            <Text style={[styles.title, tone === "error" && styles.titleError]}>{title}</Text>
            <Pressable onPress={onClose} hitSlop={8}>
              <Feather name="x" size={14} color={tone === "error" ? "#FFFFFF" : c.amberInk} />
            </Pressable>
          </LinearGradient>
          <View style={[styles.body, flush && styles.bodyFlush]}>{children}</View>
        </Pressable>
      </Pressable>
    </KeyboardAvoidingView>
  );
};

/* Solid square action button: BLOCKY, mono, uppercase. */
export const ActionBtn = ({
  label,
  onPress,
  tone = "ink",
  disabled = false
}: {
  label: string;
  onPress: () => void;
  tone?: "ink" | "amber" | "danger" | "ghost";
  disabled?: boolean;
}) => (
  <Pressable
    onPress={onPress}
    disabled={disabled}
    style={({ pressed }) => [
      styles.action,
      tone === "amber" && styles.actionAmber,
      tone === "danger" && styles.actionDanger,
      tone === "ghost" && styles.actionGhost,
      pressed && { opacity: 0.75 },
      disabled && { opacity: 0.4 }
    ]}
  >
    <Text
      style={[
        styles.actionLabel,
        tone === "amber" && { color: c.amberInk },
        tone === "ghost" && { color: c.ink }
      ]}
    >
      {label}
    </Text>
  </Pressable>
);

const styles = StyleSheet.create({
  iconBtn: {
    width: 30,
    height: 30,
    alignItems: "center",
    justifyContent: "center",
    backgroundColor: "transparent"
  },
  iconBtnActive: { backgroundColor: c.ink },
  iconBtnPressed: { backgroundColor: c.tsepBg },
  iconBtnDisabled: { opacity: 0.4 },
  overlay: { ...StyleSheet.absoluteFillObject, zIndex: 60, elevation: 60 },
  scrim: {
    flex: 1,
    backgroundColor: "rgba(38,38,38,0.45)",
    alignItems: "center",
    justifyContent: "center",
    padding: 24
  },
  dialog: {
    alignSelf: "stretch",
    backgroundColor: c.field,
    borderWidth: 1,
    borderColor: c.line
  },
  dialogError: { borderColor: c.err },
  titleBar: {
    paddingHorizontal: 12,
    paddingVertical: 6,
    flexDirection: "row",
    alignItems: "center",
    justifyContent: "space-between"
  },
  title: {
    fontFamily: mono,
    fontSize: 11.5,
    fontWeight: "700",
    letterSpacing: 1,
    textTransform: "uppercase",
    color: c.amberInk
  },
  titleError: { color: "#FFFFFF" },
  body: { padding: 12, gap: 8 },
  bodyFlush: { padding: 0, gap: 0 },
  action: {
    backgroundColor: c.ink,
    paddingHorizontal: 12,
    paddingVertical: 7,
    alignItems: "center"
  },
  actionAmber: { backgroundColor: c.amber },
  actionDanger: { backgroundColor: c.err },
  actionGhost: { backgroundColor: c.tsepBg },
  actionLabel: {
    fontFamily: mono,
    fontSize: 10.5,
    fontWeight: "700",
    letterSpacing: 1,
    textTransform: "uppercase",
    color: "#FFFFFF"
  }
});
