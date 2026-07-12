import React from "react";
import { Image, Pressable, StyleSheet, Text, View } from "react-native";
import { Feather } from "@expo/vector-icons";
import * as ImagePicker from "expo-image-picker";
import * as DocumentPicker from "expo-document-picker";

import { c, mono } from "./theme";
import { OutgoingAttachment } from "./VisClient";

/* One picked-but-unsent upload. `uri` previews the thumbnail; `base64`
   is what rides the gateway JSON (`POST /v1/sessions/:sid/turns
   {attachments: [{base64, filename}]}` — the engine sniffs the MIME from
   magic bytes, so only images survive validation; both pickers are
   image-scoped for that reason). */
export type PendingAttachment = OutgoingAttachment & { key: string; uri?: string };

const uniq = (() => {
  let n = 0;
  return (): string => `att-${Date.now()}-${(n += 1)}`;
})();

/* data-URL → bare base64 (strip the `data:image/png;base64,` head) */
const stripDataUrl = (data: string): string => {
  const at = data.indexOf(",");
  return data.startsWith("data:") && at >= 0 ? data.slice(at + 1) : data;
};

/* file:// uri → base64 via fetch+FileReader — no expo-file-system needed. */
const uriToBase64 = async (uri: string): Promise<string> => {
  const blob = await (await fetch(uri)).blob();
  return new Promise<string>((resolve, reject) => {
    const reader = new FileReader();
    reader.onerror = () => reject(new Error("could not read the picked file"));
    reader.onload = () => resolve(stripDataUrl(String(reader.result ?? "")));
    reader.readAsDataURL(blob);
  });
};

/* photo library (multi-select). base64 comes straight off the picker. */
export const pickImages = async (): Promise<PendingAttachment[]> => {
  const res = await ImagePicker.launchImageLibraryAsync({
    mediaTypes: ["images"],
    allowsMultipleSelection: true,
    selectionLimit: 6,
    base64: true,
    quality: 0.85
  });
  if (res.canceled) return [];
  return res.assets
    .filter((a) => a.base64)
    .map((a) => ({
      key: uniq(),
      base64: a.base64 as string,
      filename: a.fileName ?? `photo-${Date.now()}.jpg`,
      uri: a.uri
    }));
};

/* camera capture — one shot. */
export const captureImage = async (): Promise<PendingAttachment[]> => {
  const perm = await ImagePicker.requestCameraPermissionsAsync();
  if (!perm.granted) throw new Error("camera permission denied");
  const res = await ImagePicker.launchCameraAsync({ base64: true, quality: 0.85 });
  if (res.canceled) return [];
  return res.assets
    .filter((a) => a.base64)
    .map((a) => ({
      key: uniq(),
      base64: a.base64 as string,
      filename: a.fileName ?? `camera-${Date.now()}.jpg`,
      uri: a.uri
    }));
};

/* Files app (document picker) — image files from anywhere (iCloud, Downloads…). */
export const pickFiles = async (): Promise<PendingAttachment[]> => {
  const res = await DocumentPicker.getDocumentAsync({
    type: "image/*",
    multiple: true,
    copyToCacheDirectory: true
  });
  if (res.canceled) return [];
  const out: PendingAttachment[] = [];
  for (const asset of res.assets) {
    out.push({
      key: uniq(),
      base64: await uriToBase64(asset.uri),
      filename: asset.name,
      uri: asset.uri
    });
  }
  return out;
};

/* ── tray: one removable chip per pending upload, above the composer —
   the web channel's `.attachments` tray, flat. ── */
export const AttachmentTray = ({
  items,
  onRemove
}: {
  items: PendingAttachment[];
  onRemove: (key: string) => void;
}) => {
  if (!items.length) return null;
  return (
    <View style={styles.tray}>
      {items.map((a) => (
        <View key={a.key} style={styles.chip}>
          {a.uri ? <Image source={{ uri: a.uri }} style={styles.thumb} /> : null}
          <Text numberOfLines={1} style={styles.chipName}>
            {a.filename ?? "image"}
          </Text>
          <Pressable onPress={() => onRemove(a.key)} hitSlop={8}>
            <Feather name="x" size={12} color={c.dim} />
          </Pressable>
        </View>
      ))}
    </View>
  );
};

const styles = StyleSheet.create({
  tray: {
    flexDirection: "row",
    flexWrap: "wrap",
    gap: 6,
    paddingHorizontal: 12,
    paddingBottom: 6
  },
  chip: {
    flexDirection: "row",
    alignItems: "center",
    gap: 6,
    backgroundColor: c.tsepBg,
    paddingLeft: 4,
    paddingRight: 8,
    paddingVertical: 4,
    maxWidth: 180
  },
  thumb: { width: 22, height: 22, backgroundColor: c.lineSoft },
  chipName: { flexShrink: 1, fontFamily: mono, fontSize: 10.5, color: c.ink2 }
});
