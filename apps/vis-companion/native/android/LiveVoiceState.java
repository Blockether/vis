package com.blockether.viscompanion;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;

/** Bounded bridge credit. The plugin serializes access to this state. */
final class LiveVoiceState {
    static boolean supportsPlatform(int api) { return api >= 31; }

    static final int RATE = 24000;
    static final int FRAME = 480;
    static final int INPUT_FRAMES = 100;
    static final int OUTPUT_SAMPLES = 24000;
    long captureId = 0;
    long outputId = 0;
    long sequence = 0;
    long acknowledged = -1;
    long written = 0;
    long consumed = 0;
    long lastFrame = -1;
    final ArrayDeque<Frame> output = new ArrayDeque<>();

    /** What the voice notice shows. It holds no transcript and no session name. */
    static final class Display {
        static final Display CONNECTING = new Display("connecting", false, null);
        /** A dictation in the composer. Its only control is Stop, which keeps the words. */
        static final Display RECORDING = new Display("recording", false, null);
        final String state;
        final boolean muted;
        final String task;

        Display(String state, boolean muted, String task) {
            if (!java.util.Arrays.asList("connecting", "listening", "speaking", "muted", "waiting", "recording")
                    .contains(state)
                    || (task != null && !java.util.Arrays.asList("running", "queued", "cancelling").contains(task)))
                throw new IllegalArgumentException("Invalid voice status.");
            this.state = state;
            this.muted = muted;
            this.task = task;
        }

        String title() {
            switch (state) {
                case "listening": return "Listening";
                case "speaking": return "Speaking";
                case "muted": return "Microphone muted";
                case "waiting": return "Needs your answer";
                case "recording": return "Recording";
                default: return "Connecting";
            }
        }

        String text() {
            if (state.equals("recording")) return "Dictation";
            if (state.equals("waiting")) return "Open Vis to answer";
            if (state.equals("muted")) return "Vis cannot hear you";
            if ("running".equals(task)) return "Voice session, task running";
            if ("queued".equals(task)) return "Voice session, task queued";
            if ("cancelling".equals(task)) return "Voice session, cancellation requested";
            return "Voice session";
        }

        /** A fixed order. Only End works before the session is live. */
        String[] controls() {
            if (state.equals("connecting")) return new String[] {"end"};
            if (state.equals("recording")) return new String[] {"stop_recording"};
            return new String[] {muted ? "unmute" : "mute", "stop_speech", "end"};
        }

        static String label(String control) {
            switch (control) {
                case "mute": return "Mute";
                case "unmute": return "Unmute";
                case "stop_speech": return "Stop speech";
                case "stop_recording": return "Stop";
                default: return "End";
            }
        }
    }

    static final class RouteWatch {
        private final int expected;
        private boolean active = true;
        RouteWatch(int expected) { this.expected = expected; }
        boolean changed(int current) { return active && current != expected; }
        void stop() { active = false; }
    }

    static final class SpeakerRoute {
        final int previous;
        final int selected;
        private SpeakerRoute(int previous, int selected) {
            this.previous = previous;
            this.selected = selected;
        }
        static SpeakerRoute select(int previous, boolean external, int speaker) {
            if (external || previous == speaker) return null;
            return new SpeakerRoute(previous, speaker);
        }
        boolean owns(int current) { return selected == current; }
    }

    static final class Frame {
        final long id;
        final long end;
        final int samples;
        Frame(long id, long end, int samples) {
            this.id = id;
            this.end = end;
            this.samples = samples;
        }
    }

    void capture(long id) {
        if (id <= captureId) throw new IllegalArgumentException("Stale capture generation.");
        captureId = id;
        sequence = 0;
        acknowledged = -1;
    }

    boolean stopCapture(long id) {
        if (id < captureId) throw new IllegalArgumentException("Stale capture generation.");
        if (id == captureId) return false;
        capture(id);
        return true;
    }

    long captured() {
        if (sequence - acknowledged - 1 >= INPUT_FRAMES)
            throw new IllegalStateException("Capture bridge overflow.");
        return sequence++;
    }

    void acknowledge(long id, long value) {
        if (id != captureId || value <= acknowledged || value >= sequence)
            throw new IllegalArgumentException("Invalid capture receipt.");
        acknowledged = value;
    }

    boolean resetOutput(long id) {
        if (id < outputId) throw new IllegalArgumentException("Stale output generation.");
        if (id == outputId) return false;
        outputId = id;
        written = consumed = 0;
        lastFrame = -1;
        output.clear();
        return true;
    }

    void enqueue(long id, long frameId, int samples) {
        if (id != outputId || frameId <= lastFrame || samples < 1 || samples > FRAME)
            throw new IllegalArgumentException("Invalid output frame.");
        if (written - consumed + samples > OUTPUT_SAMPLES || output.size() >= OUTPUT_SAMPLES)
            throw new IllegalStateException("Playback bridge overflow.");
        written += samples;
        lastFrame = frameId;
        output.addLast(new Frame(frameId, written, samples));
    }

    List<Frame> played(long head) {
        if (head < consumed || head > written)
            throw new IllegalStateException("Invalid playback position.");
        consumed = head;
        List<Frame> result = new ArrayList<>();
        while (!output.isEmpty() && output.peekFirst().end <= head)
            result.add(output.removeFirst());
        return result;
    }
}
