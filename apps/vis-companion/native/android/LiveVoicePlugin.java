package com.blockether.viscompanion;

import android.Manifest;
import android.content.Context;
import android.media.AudioAttributes;
import android.media.AudioFocusRequest;
import android.media.AudioFormat;
import android.media.AudioDeviceInfo;
import android.media.AudioManager;
import android.media.AudioRecord;
import android.media.AudioRouting;
import android.media.AudioTrack;
import android.media.MediaRecorder;
import android.media.audiofx.AcousticEchoCanceler;
import android.os.Handler;
import android.os.Looper;
import android.util.Base64;
import com.getcapacitor.JSObject;
import com.getcapacitor.PermissionState;
import com.getcapacitor.Plugin;
import com.getcapacitor.PluginCall;
import com.getcapacitor.PluginMethod;
import com.getcapacitor.annotation.CapacitorPlugin;
import com.getcapacitor.annotation.Permission;
import com.getcapacitor.annotation.PermissionCallback;
import java.nio.ByteBuffer;
import java.util.ArrayDeque;

/**
 * PCM transport. A session starts in the foreground and continues behind a locked screen.
 * Physical echo performance still needs human verification.
 */
@CapacitorPlugin(name = "LiveVoice", permissions = {
    @Permission(alias = "microphone", strings = {Manifest.permission.RECORD_AUDIO})
})
public class LiveVoicePlugin extends Plugin {
    private final Handler handler = new Handler(Looper.getMainLooper());
    private final ArrayDeque<ByteBuffer> pending = new ArrayDeque<>();
    private LiveVoiceState state = new LiveVoiceState();
    private String sessionId;
    private AudioRecord recorder;
    private AcousticEchoCanceler echo;
    private AudioTrack track;
    private AudioManager manager;
    private AudioFocusRequest focus;
    private AudioManager.OnCommunicationDeviceChangedListener communicationListener;
    private LiveVoiceState.RouteWatch communicationWatch;
    private int previousMode;
    private AudioDeviceInfo previousDevice;
    private LiveVoiceState.SpeakerRoute speakerRoute;
    private boolean ownsMode;
    private boolean foreground = true;
    private boolean held;
    private boolean heldMicrophone;
    /** A web dictation owns the service and its notice. A voice session never runs at the same time. */
    private boolean dictating;
    private boolean permissionPending;
    private PluginCall permissionCall;
    private long rawHead;
    private long headWrap;
    private int inputRoute;
    private int outputRoute;

    private final com.getcapacitor.WebViewListener navigationListener = new com.getcapacitor.WebViewListener() {
        @Override public void onPageStarted(android.webkit.WebView view) {
            fail("Live voice stopped when the document changed.");
        }
    };

    private boolean watchesNavigation;

    @Override protected synchronized void handleOnStart() {
        // Bridge construction replaces its listener list after plugin load().
        if (!watchesNavigation) {
            getBridge().addWebViewListener(navigationListener);
            watchesNavigation = true;
        }
    }

    private JSObject capabilities(boolean enabled) {
        JSObject result = new JSObject();
        boolean platformSupported = LiveVoiceState.supportsPlatform(android.os.Build.VERSION.SDK_INT);
        boolean supported = platformSupported && AcousticEchoCanceler.isAvailable();
        result.put("platform", "android");
        result.put("available", supported);
        result.put("echoCancellation", enabled && supported);
        result.put("requiresVerification", true);
        if (!platformSupported) result.put("reason", "Live voice requires Android 12 or later.");
        else if (!supported) result.put("reason", "Platform echo cancellation is unavailable.");
        return result;
    }

    private void owner(PluginCall call) {
        if (sessionId == null || !sessionId.equals(call.getString("sessionId")))
            throw new IllegalArgumentException("Voice session does not own this bridge.");
    }

    private long integer(PluginCall call, String key) {
        Double value = call.getDouble(key);
        if (value == null || !Double.isFinite(value) || value < 0
                || value > 9007199254740991d || value != Math.floor(value))
            throw new IllegalArgumentException("Invalid voice identifier.");
        return value.longValue();
    }

    @PluginMethod
    public synchronized void getCapabilities(PluginCall call) {
        call.resolve(capabilities(true));
    }

    @PluginMethod
    public synchronized void prepare(PluginCall call) {
        try {
            String id = call.getString("sessionId");
            if (!foreground || id == null || id.isEmpty() || sessionId != null)
                throw new IllegalStateException("Voice bridge is unavailable or already owned.");
            if (integer(call, "sampleRate") != LiveVoiceState.RATE
                    || integer(call, "frameSamples") != LiveVoiceState.FRAME
                    || integer(call, "inputBufferFrames") != LiveVoiceState.INPUT_FRAMES
                    || integer(call, "outputBufferSamples") != LiveVoiceState.OUTPUT_SAMPLES)
                throw new IllegalArgumentException("Unsupported PCM limits.");
            if (!LiveVoiceState.supportsPlatform(android.os.Build.VERSION.SDK_INT)
                    || !AcousticEchoCanceler.isAvailable()) {
                call.resolve(capabilities(false));
                return;
            }
            holdProcess();
            sessionId = id;
            state = new LiveVoiceState();
            manager = (AudioManager) getContext().getSystemService(Context.AUDIO_SERVICE);
            call.resolve(capabilities(true));
        } catch (Exception error) { call.reject(error.getMessage()); }
    }

    /**
     * Android stops the microphone of a background process that has no foreground service.
     * The microphone type needs the granted permission, so capture start calls this again.
     */
    private void holdProcess() {
        boolean microphone = getContext().checkSelfPermission(Manifest.permission.RECORD_AUDIO)
                == android.content.pm.PackageManager.PERMISSION_GRANTED;
        if (held && heldMicrophone == microphone) return;
        endDictation();
        LiveVoiceService.start(getContext(), microphone, LiveVoiceState.Display.CONNECTING, this::control);
        held = true;
        heldMicrophone = microphone;
    }

    /**
     * A control on the voice notice. The web session owns the voice state and answers the event.
     * Mute and End stop the microphone here first, so that they work when the web view is slow.
     */
    private synchronized void control(String control) {
        if (control.equals("stop_recording")) {
            if (!dictating) return;
            JSObject event = new JSObject();
            event.put("control", control);
            notifyListeners("dictation", event);
            return;
        }
        if (sessionId == null) return;
        JSObject event = new JSObject();
        event.put("sessionId", sessionId);
        event.put("control", control);
        notifyListeners("control", event);
        if (control.equals("mute")) releaseCapture();
        else if (control.equals("end")) release();
    }

    @PluginMethod
    public synchronized void setStatus(PluginCall call) {
        try {
            owner(call);
            LiveVoiceService.show(getContext(), new LiveVoiceState.Display(
                    call.getString("state"), call.getBoolean("muted", false), call.getString("task")));
            call.resolve();
        } catch (Exception error) { call.reject(error.getMessage()); }
    }

    /**
     * Display of a web dictation. The web view records, and this service keeps its microphone
     * behind a locked screen. The service stays while the recording uploads and transcribes, and
     * its notice shows the progress. Transcribed ends the service and shows the outcome briefly.
     * A transcription and its outcome only update a shown dictation, so an outbox retry shows none.
     */
    @PluginMethod
    public synchronized void setDictation(PluginCall call) {
        String next = call.getString("state");
        if ("recording".equals(next)) {
            if (dictating) { call.resolve(); return; }
            if (!foreground || held) { call.reject("The voice notice is unavailable."); return; }
            try {
                LiveVoiceService.start(getContext(), getContext().checkSelfPermission(Manifest.permission.RECORD_AUDIO)
                        == android.content.pm.PackageManager.PERMISSION_GRANTED,
                        LiveVoiceState.Display.RECORDING, this::control);
                dictating = true;
                call.resolve();
            } catch (Exception error) { call.reject(error.getMessage()); }
        } else if ("transcribing".equals(next)) {
            try {
                if (dictating) LiveVoiceService.show(getContext(), LiveVoiceState.Display.transcribing(
                        call.getString("detail"), call.getInt("progress")));
                call.resolve();
            } catch (Exception error) { call.reject(error.getMessage()); }
        } else if ("transcribed".equals(next)) {
            if (dictating) {
                endDictation();
                LiveVoiceService.transcribed(getContext());
            }
            call.resolve();
        } else if ("idle".equals(next)) {
            endDictation();
            call.resolve();
        } else {
            call.reject("Invalid dictation state.");
        }
    }

    private void endDictation() {
        if (!dictating) return;
        dictating = false;
        LiveVoiceService.stop(getContext());
    }

    private void audioSession() {
        if (ownsMode) return;
        AudioAttributes attributes = attributes();
        LiveVoiceState ownedState = state;
        focus = new AudioFocusRequest.Builder(AudioManager.AUDIOFOCUS_GAIN_TRANSIENT)
                .setAudioAttributes(attributes)
                .setOnAudioFocusChangeListener(change -> {
                    synchronized (LiveVoicePlugin.this) {
                        if (change < 0 && state == ownedState && ownsMode)
                            fail("Audio focus was interrupted.");
                    }
                }, handler).build();
        if (manager.requestAudioFocus(focus) != AudioManager.AUDIOFOCUS_REQUEST_GRANTED)
            throw new IllegalStateException("Audio focus is unavailable.");
        previousMode = manager.getMode();
        ownsMode = true;
        manager.setMode(AudioManager.MODE_IN_COMMUNICATION);
        selectSpeaker();
        watchCommunicationRoute();
    }

    private void watchCommunicationRoute() {
        if (android.os.Build.VERSION.SDK_INT < 31) return;
        AudioDeviceInfo current = manager.getCommunicationDevice();
        int expected = speakerRoute != null ? speakerRoute.selected
                : current == null ? 0 : current.getId();
        LiveVoiceState.RouteWatch watch = new LiveVoiceState.RouteWatch(expected);
        communicationWatch = watch;
        communicationListener = device -> {
            synchronized (LiveVoicePlugin.this) {
                if (communicationWatch == watch && watch.changed(device == null ? 0 : device.getId()))
                    fail("The communication route changed.");
            }
        };
        manager.addOnCommunicationDeviceChangedListener(handler::post, communicationListener);
    }

    private void selectSpeaker() {
        if (android.os.Build.VERSION.SDK_INT >= 31) {
            previousDevice = manager.getCommunicationDevice();
            boolean external = previousDevice != null
                    && previousDevice.getType() != AudioDeviceInfo.TYPE_BUILTIN_EARPIECE
                    && previousDevice.getType() != AudioDeviceInfo.TYPE_BUILTIN_SPEAKER;
            if (external) return;
            for (AudioDeviceInfo device : manager.getAvailableCommunicationDevices()) {
                if (device.getType() == AudioDeviceInfo.TYPE_BUILTIN_SPEAKER) {
                    LiveVoiceState.SpeakerRoute choice = LiveVoiceState.SpeakerRoute.select(
                            previousDevice == null ? 0 : previousDevice.getId(), external, device.getId());
                    if (choice == null) return;
                    if (!manager.setCommunicationDevice(device))
                        throw new IllegalStateException("The speaker route is unavailable.");
                    speakerRoute = choice;
                    return;
                }
            }
            throw new IllegalStateException("The speaker route is unavailable.");
        }
        throw new IllegalStateException("Live voice requires Android 12 or later.");
    }

    private void restoreRoute() {
        if (android.os.Build.VERSION.SDK_INT >= 31 && speakerRoute != null) {
            AudioDeviceInfo current = manager.getCommunicationDevice();
            if (current != null && speakerRoute.owns(current.getId())) {
                if (previousDevice == null || !manager.setCommunicationDevice(previousDevice))
                    manager.clearCommunicationDevice();
            }
        }
        previousDevice = null;
        speakerRoute = null;
    }

    private AudioAttributes attributes() {
        return new AudioAttributes.Builder().setUsage(AudioAttributes.USAGE_VOICE_COMMUNICATION)
                .setContentType(AudioAttributes.CONTENT_TYPE_SPEECH).build();
    }

    @PluginMethod
    public synchronized void startCapture(PluginCall call) {
        try {
            owner(call);
            long id = integer(call, "captureId");
            if (id <= state.captureId || recorder != null || permissionPending)
                throw new IllegalArgumentException("Capture is active or stale.");
            if (getPermissionState("microphone") != PermissionState.GRANTED) {
                permissionPending = true;
                permissionCall = call;
                requestPermissionForAlias("microphone", call, "capturePermission");
                return;
            }
            beginCapture(call, id);
        } catch (Exception error) { call.reject(error.getMessage()); }
    }

    @PermissionCallback
    private synchronized void capturePermission(PluginCall call) {
        if (call == null || permissionCall != call) return;
        permissionPending = false;
        permissionCall = null;
        try {
            owner(call);
            if (integer(call, "captureId") <= state.captureId)
                throw new IllegalArgumentException("Stale capture generation.");
            if (getPermissionState("microphone") != PermissionState.GRANTED)
                throw new IllegalStateException("Microphone permission was denied.");
            beginCapture(call, integer(call, "captureId"));
        } catch (Exception error) { call.reject(error.getMessage()); }
    }

    private void beginCapture(PluginCall call, long id) {
        try {
            state.capture(id);
            audioSession();
            int minimum = AudioRecord.getMinBufferSize(LiveVoiceState.RATE,
                    AudioFormat.CHANNEL_IN_MONO, AudioFormat.ENCODING_PCM_16BIT);
            if (minimum <= 0) throw new IllegalStateException("PCM capture is unsupported.");
            if (getContext().checkSelfPermission(Manifest.permission.RECORD_AUDIO)
                    != android.content.pm.PackageManager.PERMISSION_GRANTED)
                throw new SecurityException("Microphone permission was revoked.");
            holdProcess();
            recorder = new AudioRecord(MediaRecorder.AudioSource.VOICE_COMMUNICATION,
                    LiveVoiceState.RATE, AudioFormat.CHANNEL_IN_MONO,
                    AudioFormat.ENCODING_PCM_16BIT, Math.max(minimum, 4 * LiveVoiceState.FRAME));
            if (recorder.getState() != AudioRecord.STATE_INITIALIZED)
                throw new IllegalStateException("PCM capture could not start.");
            echo = AcousticEchoCanceler.create(recorder.getAudioSessionId());
            if (echo == null || echo.setEnabled(true) != 0 || !echo.getEnabled())
                throw new IllegalStateException("Echo cancellation could not be enabled.");
            echo.setEnableStatusListener((effect, enabled) -> {
                synchronized (LiveVoicePlugin.this) {
                    if (effect == echo && !enabled) fail("Echo cancellation was disabled.");
                }
            });
            echo.setControlStatusListener((effect, controlled) -> {
                synchronized (LiveVoicePlugin.this) {
                    if (effect == echo && !controlled) fail("Echo cancellation control was lost.");
                }
            });
            inputRoute = 0;
            recorder.addOnRoutingChangedListener(routeListener, handler);
            recorder.startRecording();
            if (recorder.getRecordingState() != AudioRecord.RECORDSTATE_RECORDING)
                throw new IllegalStateException("PCM capture did not start.");
            AudioRecord input = recorder;
            String owner = sessionId;
            new Thread(() -> captureLoop(input, owner, id), "vis-live-capture").start();
            call.resolve();
        } catch (Exception error) {
            fail("Microphone or echo cancellation could not start.");
            call.reject("Microphone or echo cancellation could not start.");
        }
    }

    private void captureLoop(AudioRecord input, String owner, long id) {
        byte[] frame = new byte[LiveVoiceState.FRAME * 2];
        int offset = 0;
        try {
            while (true) {
                int count = input.read(frame, offset, frame.length - offset, AudioRecord.READ_BLOCKING);
                synchronized (this) {
                    if (input != recorder || !owner.equals(sessionId) || id != state.captureId) return;
                    if (count <= 0) throw new IllegalStateException();
                    offset += count;
                    if (offset == frame.length) {
                        JSObject event = new JSObject();
                        event.put("sessionId", owner);
                        event.put("captureId", id);
                        event.put("sequence", state.captured());
                        event.put("pcm", Base64.encodeToString(frame, Base64.NO_WRAP));
                        notifyListeners("capture", event);
                        offset = 0;
                    }
                }
            }
        } catch (Exception error) {
            synchronized (this) {
                if (input == recorder) fail("PCM capture failed or exceeded bridge credit.");
            }
        }
    }

    private final AudioRouting.OnRoutingChangedListener routeListener = routing -> {
        synchronized (this) {
            if (routing != recorder && routing != track) return;
            int route = routing.getRoutedDevice() == null ? -1 : routing.getRoutedDevice().getId();
            int previous = routing == recorder ? inputRoute : outputRoute;
            if (previous != 0 && previous != route) { fail("The audio route changed."); return; }
            if (routing == recorder) inputRoute = route; else outputRoute = route;
        }
    };

    @PluginMethod
    public synchronized void stopCapture(PluginCall call) {
        try {
            owner(call);
            if (state.stopCapture(integer(call, "captureId"))) releaseCapture();
            call.resolve();
        } catch (Exception error) { call.reject(error.getMessage()); }
    }

    @PluginMethod
    public synchronized void acknowledgeCapture(PluginCall call) {
        try {
            owner(call);
            state.acknowledge(integer(call, "captureId"), integer(call, "sequence"));
            call.resolve();
        } catch (Exception error) { call.reject(error.getMessage()); }
    }

    @PluginMethod
    public synchronized void play(PluginCall call) {
        try {
            owner(call);
            long id = integer(call, "outputId");
            if (id != state.outputId) { call.reject("Stale output generation."); return; }
            String pcm = call.getString("pcm");
            if (pcm == null || pcm.length() > LiveVoiceState.FRAME * 4)
                throw new IllegalArgumentException("Invalid PCM frame.");
            byte[] bytes = Base64.decode(pcm, Base64.NO_WRAP);
            if (bytes.length % 2 != 0) throw new IllegalArgumentException("Invalid PCM frame.");
            state.enqueue(id, integer(call, "frameId"), bytes.length / 2);
            audioSession();
            if (track == null) {
                int minimum = AudioTrack.getMinBufferSize(LiveVoiceState.RATE,
                        AudioFormat.CHANNEL_OUT_MONO, AudioFormat.ENCODING_PCM_16BIT);
                if (minimum <= 0) throw new IllegalStateException("PCM playback is unsupported.");
                track = new AudioTrack.Builder().setAudioAttributes(attributes())
                        .setAudioFormat(new AudioFormat.Builder().setSampleRate(LiveVoiceState.RATE)
                            .setChannelMask(AudioFormat.CHANNEL_OUT_MONO)
                            .setEncoding(AudioFormat.ENCODING_PCM_16BIT).build())
                        .setTransferMode(AudioTrack.MODE_STREAM)
                        .setBufferSizeInBytes(Math.max(minimum, LiveVoiceState.OUTPUT_SAMPLES * 2)).build();
                outputRoute = 0;
                track.addOnRoutingChangedListener(routeListener, handler);
                track.play();
            }
            pending.add(ByteBuffer.wrap(bytes));
            handler.removeCallbacks(pump);
            handler.post(pump);
            call.resolve();
        } catch (Exception error) {
            // A rejected frame must not leave earlier audio or capture running.
            if (sessionId != null && sessionId.equals(call.getString("sessionId")))
                fail("PCM playback failed or exceeded bridge credit.");
            call.reject("PCM playback failed or exceeded bridge credit.");
        }
    }

    private final Runnable pump = new Runnable() {
        @Override public void run() {
            synchronized (LiveVoicePlugin.this) {
                if (track == null) return;
                try {
                    while (!pending.isEmpty()) {
                        ByteBuffer buffer = pending.peekFirst();
                        int count = track.write(buffer, buffer.remaining(), AudioTrack.WRITE_NON_BLOCKING);
                        if (count < 0) throw new IllegalStateException();
                        if (buffer.hasRemaining()) break;
                        pending.removeFirst();
                    }
                    long raw = Integer.toUnsignedLong(track.getPlaybackHeadPosition());
                    if (raw < rawHead) headWrap += 1L << 32;
                    rawHead = raw;
                    for (LiveVoiceState.Frame frame : state.played(headWrap + raw)) {
                        JSObject event = new JSObject();
                        event.put("sessionId", sessionId);
                        event.put("outputId", state.outputId);
                        event.put("frameId", frame.id);
                        event.put("playedSamples", frame.samples);
                        notifyListeners("played", event);
                    }
                    if (!state.output.isEmpty()) handler.postDelayed(this, 10);
                } catch (Exception error) { fail("PCM playback stopped unexpectedly."); }
            }
        }
    };

    @PluginMethod
    public synchronized void stopOutput(PluginCall call) {
        try {
            owner(call);
            if (state.resetOutput(integer(call, "outputId"))) releaseOutput();
            call.resolve();
        } catch (Exception error) { call.reject(error.getMessage()); }
    }

    private void cancelPermission() {
        PluginCall pending = permissionCall;
        permissionCall = null;
        permissionPending = false;
        if (pending != null) pending.reject("Capture request was cancelled.");
    }

    private void releaseCapture() {
        cancelPermission();
        AudioRecord old = recorder;
        recorder = null;
        if (old != null) {
            old.removeOnRoutingChangedListener(routeListener);
            try { old.stop(); } catch (IllegalStateException ignored) { }
            old.release();
        }
        if (echo != null) {
            AcousticEchoCanceler oldEcho = echo;
            echo = null;
            oldEcho.setEnableStatusListener(null);
            oldEcho.setControlStatusListener(null);
            oldEcho.release();
        }
    }

    private void releaseOutput() {
        handler.removeCallbacks(pump);
        pending.clear();
        if (track != null) {
            track.removeOnRoutingChangedListener(routeListener);
            try { track.pause(); track.flush(); } catch (IllegalStateException ignored) { }
            track.release();
            track = null;
        }
        rawHead = headWrap = 0;
    }

    private void release() {
        if (communicationWatch != null) communicationWatch.stop();
        communicationWatch = null;
        if (android.os.Build.VERSION.SDK_INT >= 31 && communicationListener != null) {
            manager.removeOnCommunicationDeviceChangedListener(communicationListener);
            communicationListener = null;
        }
        releaseCapture();
        releaseOutput();
        if (held) LiveVoiceService.stop(getContext());
        held = heldMicrophone = false;
        if (manager != null) {
            if (ownsMode && manager.getMode() == AudioManager.MODE_IN_COMMUNICATION) {
                restoreRoute();
                manager.setMode(previousMode);
            }
            if (focus != null) manager.abandonAudioFocusRequest(focus);
        }
        previousDevice = null;
        speakerRoute = null;
        ownsMode = false;
        focus = null;
        sessionId = null;
        permissionPending = false;
        permissionCall = null;
    }

    private synchronized void fail(String message) {
        if (sessionId == null) return;
        String owner = sessionId;
        release();
        JSObject event = new JSObject();
        event.put("sessionId", owner);
        event.put("message", message);
        notifyListeners("failure", event);
    }

    @PluginMethod
    public synchronized void close(PluginCall call) {
        if (sessionId != null && sessionId.equals(call.getString("sessionId"))) release();
        call.resolve();
    }

    // A locked screen is not a stop. Only a new session needs the foreground.
    @Override protected synchronized void handleOnPause() { foreground = false; }
    @Override protected synchronized void handleOnResume() { foreground = true; }
    @Override protected synchronized void handleOnStop() { foreground = false; }
    @Override protected synchronized void handleOnDestroy() {
        getBridge().removeWebViewListener(navigationListener);
        watchesNavigation = false;
        release();
        endDictation();
    }
}
