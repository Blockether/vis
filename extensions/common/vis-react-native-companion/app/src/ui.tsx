import React from "react";
import {
  KeyboardAvoidingView,
  Platform,
  Pressable,
  StyleSheet,
  Text,
  View,
} from "react-native";
import { Feather } from "@expo/vector-icons";

import { c, mono } from "./theme";

/* Native rounded icon button — quiet until pressed/active. */
export const IconBtn = ({
  name,
  onPress,
  size = 15,
  color = c.ink,
  active = false,
  disabled = false,
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
      disabled && styles.iconBtnDisabled,
    ]}
  >
    <Feather name={name} size={size} color={active ? "#FFFFFF" : color} />
  </Pressable>
);

/* The vis dialog — native rounded sheet, neutral chrome.
   Rendered as an ABSOLUTE OVERLAY, not an RN <Modal>: transparent modals
   are unreliable on the iOS 26 simulator and cannot nest, and an overlay
   is flatter anyway. Mount it near the root so it covers the screen. */
export const DialogModal = ({
  visible,
  title,
  onClose,
  tone = "amber",
  flush = false,
  dismissable = true,
  children,
}: {
  visible: boolean;
  title: string;
  onClose: () => void;
  tone?: "amber" | "error";
  /* flush = edge-to-edge body (the Settings pane brings its own chrome). */
  flush?: boolean;
  /* false keeps the sheet pinned until the caller clears the condition. */
  dismissable?: boolean;
  children: React.ReactNode;
}) => {
  if (!visible) return null;
  return (
    <KeyboardAvoidingView
      behavior={Platform.select({ ios: "padding", default: undefined })}
      style={styles.overlay}
    >
      <Pressable
        style={styles.scrim}
        onPress={dismissable ? onClose : undefined}
      >
        <Pressable
          style={[styles.dialog, tone === "error" && styles.dialogError]}
          onPress={() => {}}
        >
          <View
            style={[styles.titleBar, tone === "error" && styles.titleBarError]}
          >
            <Text style={[styles.title, tone === "error" && styles.titleError]}>
              {title}
            </Text>
            {dismissable ? (
              <Pressable onPress={onClose} hitSlop={8} style={styles.closeBtn}>
                <Feather
                  name="x"
                  size={15}
                  color={tone === "error" ? "#FFFFFF" : c.dim}
                />
              </Pressable>
            ) : null}
          </View>
          <View style={[styles.body, flush && styles.bodyFlush]}>
            {children}
          </View>
        </Pressable>
      </Pressable>
    </KeyboardAvoidingView>
  );
};

/* Native action button. `amber` is the primary blue action tone. */
export const ActionBtn = ({
  label,
  onPress,
  tone = "ink",
  disabled = false,
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
      disabled && { opacity: 0.4 },
    ]}
  >
    <Text
      style={[
        styles.actionLabel,
        tone === "amber" && { color: "#FFFFFF" },
        tone === "ghost" && { color: c.ink },
      ]}
    >
      {label}
    </Text>
  </Pressable>
);

const styles = StyleSheet.create({
  iconBtn: {
    width: 34,
    height: 34,
    alignItems: "center",
    justifyContent: "center",
    borderRadius: 17,
    backgroundColor: "transparent",
  },
  iconBtnActive: { backgroundColor: c.accent },
  iconBtnPressed: { backgroundColor: "rgba(118,118,128,0.14)" },
  iconBtnDisabled: { opacity: 0.4 },
  overlay: { ...StyleSheet.absoluteFillObject, zIndex: 60, elevation: 60 },
  scrim: {
    flex: 1,
    backgroundColor: "rgba(20,20,20,0.28)",
    alignItems: "center",
    justifyContent: "center",
    padding: 18,
  },
  dialog: {
    alignSelf: "stretch",
    overflow: "hidden",
    backgroundColor: "#F2F2F7",
    borderRadius: 22,
    shadowColor: "#000",
    shadowOpacity: 0.18,
    shadowRadius: 28,
    shadowOffset: { width: 0, height: 18 },
    elevation: 12,
  },
  dialogError: { backgroundColor: c.errBg },
  titleBar: {
    minHeight: 50,
    paddingHorizontal: 18,
    paddingVertical: 12,
    flexDirection: "row",
    alignItems: "center",
    justifyContent: "space-between",
    backgroundColor: "rgba(255,255,255,0.86)",
    borderBottomWidth: StyleSheet.hairlineWidth,
    borderBottomColor: "rgba(60,60,67,0.18)",
  },
  titleBarError: { backgroundColor: c.err },
  title: {
    fontSize: 17,
    fontWeight: "700",
    letterSpacing: -0.2,
    color: c.ink,
  },
  titleError: { color: "#FFFFFF" },
  closeBtn: {
    width: 30,
    height: 30,
    borderRadius: 15,
    alignItems: "center",
    justifyContent: "center",
    backgroundColor: "rgba(118,118,128,0.12)",
  },
  body: { padding: 14, gap: 10 },
  bodyFlush: { padding: 0, gap: 0 },
  action: {
    minHeight: 36,
    borderRadius: 12,
    backgroundColor: c.ink,
    paddingHorizontal: 14,
    paddingVertical: 9,
    alignItems: "center",
    justifyContent: "center",
  },
  actionAmber: { backgroundColor: c.accent },
  actionDanger: { backgroundColor: c.err },
  actionGhost: { backgroundColor: "rgba(118,118,128,0.12)" },
  actionLabel: {
    fontFamily: mono,
    fontSize: 10.5,
    fontWeight: "700",
    letterSpacing: 1,
    textTransform: "uppercase",
    color: "#FFFFFF",
  },
});
