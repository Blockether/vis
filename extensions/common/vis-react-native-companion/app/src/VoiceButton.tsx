import React, { useCallback, useEffect, useRef, useState } from "react";
import { ActivityIndicator, Animated, Platform, Pressable, StyleSheet, Text, View } from "react-native";
import { Feather } from "@expo/vector-icons";
import {
  AudioQuality,
  IOSOutputFormat,
  RecordingOptions,
  requestRecordingPermissionsAsync,
  setAudioModeAsync,
  useAudioRecorder,
  useAudioRecorderState
} from "expo-audio";

import { VisGatewayClient } from "./VisClient";
import { c, mono } from "./theme";

/* 16-bit PCM WAV @16kHz mono — what the gateway's Parakeet endpoint
   (`/ui/session/:sid/voice`, sherpa-onnx WaveReader) accepts. Android's
   MediaRecorder cannot produce WAV, so voice is iOS-first for now. */
const WAV_RECORDING: RecordingOptions = {
  extension: ".wav",
  sampleRate: 16000,
  numberOfChannels: 1,
  bitRate: 256000,
  android: { outputFormat: "mpeg4", audioEncoder: "aac", extension: ".m4a" },
  ios: {
    outputFormat: IOSOutputFormat.LINEARPCM,
    audioQuality: AudioQuality.HIGH,
    linearPCMBitDepth: 16,
    linearPCMIsBigEndian: false,
    linearPCMIsFloat: false
  },
  web: {}
};

type Phase = "idle" | "checking" | "downloading" | "recording" | "transcribing";

const fmtClock = (ms: number): string => {
  const s = Math.floor(ms / 1000);
  return `${String(Math.floor(s / 60)).padStart(2, "0")}:${String(s % 60).padStart(2, "0")}`;
};

/* Mic button + capture strip. Mirrors the web composer's voice flow:
   model gating (download w/ progress) → record (timer + cancel/accept)
   → transcribe → text lands in the composer via onTranscript. */
export const VoiceButton = ({
  client,
  sessionId,
  onTranscript,
  onNote,
  onPhase
}: {
  client: VisGatewayClient;
  sessionId: string | null;
  onTranscript: (text: string) => void;
  onNote: (note: string | null) => void;
  onPhase: (capturing: boolean) => void;
}) => {
  const recorder = useAudioRecorder(WAV_RECORDING);
  const recorderState = useAudioRecorderState(recorder, 500);
  const [phase, setPhase] = useState<Phase>("idle");
  const pollRef = useRef<ReturnType<typeof setInterval> | null>(null);
  const pulse = useRef(new Animated.Value(1)).current;

  useEffect(() => {
    onPhase(phase === "recording");
  }, [onPhase, phase]);

  useEffect(() => {
    if (phase !== "recording") return;
    const loop = Animated.loop(
      Animated.sequence([
        Animated.timing(pulse, { toValue: 0.35, duration: 600, useNativeDriver: true }),
        Animated.timing(pulse, { toValue: 1, duration: 600, useNativeDriver: true })
      ])
    );
    loop.start();
    return () => loop.stop();
  }, [phase, pulse]);

  useEffect(
    () => () => {
      if (pollRef.current) clearInterval(pollRef.current);
    },
    []
  );

  const beginRecording = useCallback(async () => {
    await setAudioModeAsync({ allowsRecording: true, playsInSilentMode: true });
    await recorder.prepareToRecordAsync();
    recorder.record();
    setPhase("recording");
    onNote(null);
  }, [onNote, recorder]);

  const ensureModelThenRecord = useCallback(async () => {
    if (!sessionId) return;
    setPhase("checking");
    const st = await client.voiceModelState(sessionId);
    if (st.status === "ready") {
      await beginRecording();
      return;
    }
    if (st.status === "unavailable" || st.status === "failed") {
      setPhase("idle");
      onNote(`Voice: ${st.error ?? st.status}`);
      return;
    }
    /* absent | downloading → kick the download, poll progress; the user
       taps the mic again once the model is ready (same as the web UI). */
    await client.voiceModelState(sessionId, true);
    setPhase("downloading");
    onNote("Downloading voice model\u2026 0%");
    pollRef.current = setInterval(() => {
      client
        .voiceModelState(sessionId)
        .then((s) => {
          if (s.status === "ready") {
            if (pollRef.current) clearInterval(pollRef.current);
            setPhase("idle");
            onNote("Voice model ready \u2014 tap the mic to record.");
          } else if (s.status === "failed" || s.status === "unavailable") {
            if (pollRef.current) clearInterval(pollRef.current);
            setPhase("idle");
            onNote(`Voice model: ${s.error ?? s.status}`);
          } else {
            onNote(`Downloading voice model\u2026 ${s.progress ?? 0}%`);
          }
        })
        .catch(() => undefined);
    }, 1500);
  }, [beginRecording, client, onNote, sessionId]);

  const onMicPress = useCallback(async () => {
    if (!sessionId || phase === "checking" || phase === "downloading" || phase === "transcribing") return;
    if (Platform.OS !== "ios") {
      onNote("Voice needs WAV capture \u2014 iOS only for now.");
      return;
    }
    try {
      const perm = await requestRecordingPermissionsAsync();
      if (!perm.granted) {
        onNote("Microphone blocked \u2014 allow mic access in Settings.");
        return;
      }
      await ensureModelThenRecord();
    } catch (err) {
      setPhase("idle");
      onNote(`Voice capture failed: ${err instanceof Error ? err.message : String(err)}`);
    }
  }, [ensureModelThenRecord, onNote, phase, sessionId]);

  const finish = useCallback(
    async (accept: boolean) => {
      try {
        await recorder.stop();
      } catch {
        /* already stopped */
      }
      if (!accept) {
        setPhase("idle");
        return;
      }
      const uri = recorder.uri;
      if (!uri || !sessionId) {
        setPhase("idle");
        onNote("Nothing captured.");
        return;
      }
      setPhase("transcribing");
      try {
        const text = await client.transcribeVoice(sessionId, uri);
        if (text) onTranscript(text);
        else onNote("Heard nothing \u2014 try again closer to the mic.");
        setPhase("idle");
      } catch (err) {
        setPhase("idle");
        onNote(`Transcription failed: ${err instanceof Error ? err.message : String(err)}`);
      }
    },
    [client, onNote, onTranscript, recorder, sessionId]
  );

  if (phase === "recording") {
    return (
      <View style={styles.strip}>
        <Animated.View style={[styles.recDot, { opacity: pulse }]} />
        <Text style={styles.clock}>{fmtClock(recorderState.durationMillis)}</Text>
        <Text style={styles.stripLabel}>recording</Text>
        <Pressable hitSlop={8} onPress={() => void finish(false)} style={styles.stripBtn}>
          <Feather name="x" size={15} color={c.err} />
        </Pressable>
        <Pressable hitSlop={8} onPress={() => void finish(true)} style={[styles.stripBtn, styles.stripAccept]}>
          <Feather name="check" size={15} color={c.amberInk} />
        </Pressable>
      </View>
    );
  }

  const busy = phase === "checking" || phase === "downloading" || phase === "transcribing";
  return (
    <Pressable
      onPress={() => void onMicPress()}
      disabled={busy || !sessionId}
      hitSlop={8}
      style={({ pressed }) => [styles.mic, pressed && { backgroundColor: c.lineSoft }, (busy || !sessionId) && { opacity: 0.5 }]}
    >
      {busy ? (
        <ActivityIndicator size="small" color={c.amber} />
      ) : (
        <Feather name="mic" size={15} color={c.ink} />
      )}
    </Pressable>
  );
};

const styles = StyleSheet.create({
  mic: {
    width: 30,
    height: 30,
    alignItems: "center",
    justifyContent: "center",
    backgroundColor: "transparent"
  },
  strip: {
    flex: 1,
    flexDirection: "row",
    alignItems: "center",
    gap: 10,
    backgroundColor: c.tsepBg,
    paddingHorizontal: 10,
    height: 32
  },
  recDot: { width: 8, height: 8, backgroundColor: c.err },
  clock: { fontFamily: mono, fontSize: 12, fontWeight: "700", color: c.ink },
  stripLabel: { flex: 1, fontFamily: mono, fontSize: 11, color: c.dim, letterSpacing: 1 },
  stripBtn: {
    width: 28,
    height: 28,
    alignItems: "center",
    justifyContent: "center",
    backgroundColor: "transparent"
  },
  stripAccept: { backgroundColor: c.amber }
});