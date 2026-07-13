import React, { useCallback, useState } from "react";
import { Pressable, StyleSheet, Text, View } from "react-native";
import {
  CameraView,
  type BarcodeScanningResult,
  useCameraPermissions,
} from "expo-camera";
import { Feather } from "@expo/vector-icons";

import { c, mono } from "./theme";
import { ActionBtn } from "./ui";
import {
  parseGatewayPairing,
  pairingDisplayHost,
  type GatewayPairing,
} from "./GatewayPairing";

export const GatewayPairingScanner = ({
  onPair,
  onClose,
}: {
  onPair: (pairing: GatewayPairing) => void;
  onClose: () => void;
}) => {
  const [permission, requestPermission] = useCameraPermissions();
  const [locked, setLocked] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const handleScan = useCallback(
    ({ data }: BarcodeScanningResult) => {
      if (locked) return;
      setLocked(true);
      try {
        const pairing = parseGatewayPairing(data);
        onPair(pairing);
        onClose();
      } catch (e) {
        setError(e instanceof Error ? e.message : String(e));
        setTimeout(() => setLocked(false), 900);
      }
    },
    [locked, onClose, onPair],
  );

  if (!permission?.granted) {
    return (
      <View style={st.wrap}>
        <Feather name="camera" size={24} color={c.amberDeep} />
        <Text style={st.title}>Scan gateway QR</Text>
        <Text style={st.copy}>
          Allow camera access, then scan a VIS pairing QR. It may point at your
          LAN IP or a Tailscale 100.x address.
        </Text>
        <View style={st.actions}>
          <ActionBtn
            label="Allow camera"
            tone="amber"
            onPress={requestPermission}
          />
          <ActionBtn label="Cancel" tone="ghost" onPress={onClose} />
        </View>
      </View>
    );
  }

  return (
    <View style={st.wrap}>
      <View style={st.cameraBox}>
        <CameraView
          style={StyleSheet.absoluteFill}
          facing="back"
          barcodeScannerSettings={{ barcodeTypes: ["qr"] }}
          onBarcodeScanned={locked ? undefined : handleScan}
        />
        <View style={st.reticle} pointerEvents="none" />
      </View>
      <Text style={st.title}>Scan VIS gateway QR</Text>
      <Text style={st.copy}>
        The QR fills Gateway URL + bearer token. Works best over Tailscale or
        the same Wi‑Fi.
      </Text>
      {error ? <Text style={st.err}>{error}</Text> : null}
      <ActionBtn label="Cancel" tone="ghost" onPress={onClose} />
    </View>
  );
};

export const describePairing = (pairing: GatewayPairing): string =>
  `${pairingDisplayHost(pairing)}${pairing.token ? " · token" : " · no token"}`;

const st = StyleSheet.create({
  wrap: {
    gap: 12,
    padding: 14,
    backgroundColor: "#FFFFFF",
    borderRadius: 18,
    borderWidth: StyleSheet.hairlineWidth,
    borderColor: "rgba(60,60,67,0.18)",
  },
  cameraBox: {
    height: 280,
    overflow: "hidden",
    borderRadius: 16,
    backgroundColor: "#000",
  },
  reticle: {
    position: "absolute",
    left: "14%",
    right: "14%",
    top: "18%",
    bottom: "18%",
    borderWidth: 2,
    borderColor: c.amberBright,
  },
  title: {
    fontSize: 16,
    fontWeight: "700",
    color: c.ink,
    letterSpacing: -0.2,
  },
  copy: { color: c.dim, fontSize: 13, lineHeight: 18 },
  err: { color: c.err, fontFamily: mono, fontSize: 11 },
  actions: { flexDirection: "row", gap: 8, flexWrap: "wrap" },
});
