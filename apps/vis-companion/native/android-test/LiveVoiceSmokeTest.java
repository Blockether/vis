package com.blockether.viscompanion;

import static org.junit.Assert.*;
import android.Manifest;
import android.content.Context;
import android.content.pm.PackageManager;
import android.media.AudioManager;
import androidx.test.core.app.ActivityScenario;
import androidx.test.ext.junit.runners.AndroidJUnit4;
import com.getcapacitor.JSObject;
import com.getcapacitor.PluginCall;
import org.junit.Test;
import org.junit.runner.RunWith;

/** Run with host audio disabled. This test never requests or opens a microphone. */
@RunWith(AndroidJUnit4.class)
public class LiveVoiceSmokeTest {
    private static final class Call extends PluginCall {
        JSObject value;
        String error;
        boolean completed;
        int settlements;
        Call(JSObject data) { super(null, "LiveVoice", "test", "test", data); }
        @Override public void resolve(JSObject result) { value = result; completed = true; settlements++; }
        @Override public void resolve() { completed = true; settlements++; }
        @Override public void reject(String message) { error = message; completed = true; settlements++; }
    }

    private static void field(LiveVoicePlugin plugin, String name, Object value) {
        try {
            var field = LiveVoicePlugin.class.getDeclaredField(name);
            field.setAccessible(true);
            field.set(plugin, value);
        } catch (Exception error) { throw new AssertionError(error); }
    }

    private static Object field(LiveVoicePlugin plugin, String name) {
        try {
            var field = LiveVoicePlugin.class.getDeclaredField(name);
            field.setAccessible(true);
            return field.get(plugin);
        } catch (Exception error) { throw new AssertionError(error); }
    }

    private static void permissionCallback(LiveVoicePlugin plugin, Call call) {
        try {
            var method = LiveVoicePlugin.class.getDeclaredMethod("capturePermission", PluginCall.class);
            method.setAccessible(true);
            method.invoke(plugin, call);
        } catch (Exception error) { throw new AssertionError(error); }
    }

    private static Call pending(LiveVoicePlugin plugin, int captureId) {
        field(plugin, "sessionId", "injected-session");
        Call call = new Call(new JSObject().put("sessionId", "injected-session").put("captureId", captureId));
        field(plugin, "permissionPending", true);
        field(plugin, "permissionCall", call);
        return call;
    }

    @Test public void stopAndCloseSettleOnlyTheOwnedPendingPermission() {
        try (ActivityScenario<MainActivity> scenario = ActivityScenario.launch(MainActivity.class)) {
            scenario.onActivity(activity -> {
                LiveVoicePlugin plugin = (LiveVoicePlugin) activity.getBridge().getPlugin("LiveVoice").getInstance();
                Call old = pending(plugin, 1);
                plugin.stopCapture(new Call(new JSObject().put("sessionId", "injected-session").put("captureId", 2)));
                assertEquals(1, old.settlements);
                assertEquals(false, field(plugin, "permissionPending"));
                assertNull(field(plugin, "permissionCall"));
                permissionCallback(plugin, null);
                Call next = pending(plugin, 3);
                permissionCallback(plugin, null);
                permissionCallback(plugin, old);
                assertEquals(1, old.settlements);
                assertEquals(0, next.settlements);
                assertSame(next, field(plugin, "permissionCall"));
                plugin.close(new Call(new JSObject().put("sessionId", "other-session")));
                assertEquals(0, next.settlements);
                plugin.close(new Call(new JSObject().put("sessionId", "injected-session")));
                assertEquals(1, next.settlements);
                permissionCallback(plugin, next);
                assertEquals(1, next.settlements);
                assertNull(field(plugin, "sessionId"));
                assertNull(field(plugin, "recorder"));
            });
        }
    }

    @Test public void documentStartTearsDownOnlyAnOwnedSession() {
        try (ActivityScenario<MainActivity> scenario = ActivityScenario.launch(MainActivity.class)) {
            scenario.onActivity(activity -> {
                LiveVoicePlugin plugin = (LiveVoicePlugin) activity.getBridge().getPlugin("LiveVoice").getInstance();
                var view = activity.getBridge().getWebView();
                // Exercise Capacitor's installed page-start dispatcher without network or audio.
                view.getWebViewClient().onPageStarted(view, "https://127.0.0.1/", null);
                assertNull(field(plugin, "sessionId"));
                Call call = pending(plugin, 1);
                view.getWebViewClient().onPageStarted(view, "https://127.0.0.1/", null);
                assertNull(field(plugin, "sessionId"));
                assertEquals(1, call.settlements);
                permissionCallback(plugin, call);
                assertEquals(1, call.settlements);
                assertNull(field(plugin, "recorder"));
            });
        }
    }

    @Test public void actualWebViewReloadSettlesThePendingRequest() throws Exception {
        try (ActivityScenario<MainActivity> scenario = ActivityScenario.launch(MainActivity.class)) {
            var started = new java.util.concurrent.CountDownLatch(1);
            Call[] pending = new Call[1];
            com.getcapacitor.WebViewListener observer = new com.getcapacitor.WebViewListener() {
                @Override public void onPageStarted(android.webkit.WebView view) { started.countDown(); }
            };
            scenario.onActivity(activity -> {
                LiveVoicePlugin plugin = (LiveVoicePlugin) activity.getBridge().getPlugin("LiveVoice").getInstance();
                activity.getBridge().addWebViewListener(observer);
                pending[0] = pending(plugin, 1);
                activity.getBridge().getWebView().reload();
            });
            assertTrue("WebView reload did not start", started.await(10, java.util.concurrent.TimeUnit.SECONDS));
            scenario.onActivity(activity -> {
                LiveVoicePlugin plugin = (LiveVoicePlugin) activity.getBridge().getPlugin("LiveVoice").getInstance();
                activity.getBridge().removeWebViewListener(observer);
                assertNull(field(plugin, "sessionId"));
                assertNull(field(plugin, "recorder"));
                assertEquals(1, pending[0].settlements);
            });
        }
    }

    @Test public void capabilitiesAndPreparationNeverAcquireTheMicrophone() {
        try (ActivityScenario<MainActivity> scenario = ActivityScenario.launch(MainActivity.class)) {
            scenario.onActivity(activity -> {
                LiveVoicePlugin plugin = (LiveVoicePlugin) activity.getBridge()
                        .getPlugin("LiveVoice").getInstance();
                AudioManager audio = (AudioManager) activity.getSystemService(Context.AUDIO_SERVICE);
                int mode = audio.getMode();
                assertEquals(PackageManager.PERMISSION_DENIED,
                        activity.checkSelfPermission(Manifest.permission.RECORD_AUDIO));
                Call capabilities = new Call(new JSObject());
                plugin.getCapabilities(capabilities);
                assertTrue(capabilities.completed);
                assertNull(capabilities.error);
                assertEquals("android", capabilities.value.getString("platform"));
                assertTrue(capabilities.value.getBool("requiresVerification"));
                JSObject options = new JSObject();
                options.put("sessionId", "synthetic-voice-test");
                options.put("sampleRate", 24000);
                options.put("frameSamples", 480);
                options.put("inputBufferFrames", 100);
                options.put("outputBufferSamples", 24000);
                Call prepared = new Call(options);
                plugin.prepare(prepared);
                assertTrue(prepared.completed);
                assertNull(prepared.error);
                assertEquals(capabilities.value.getBool("available"), prepared.value.getBool("available"));
                assertEquals(mode, audio.getMode());
                assertEquals(PackageManager.PERMISSION_DENIED,
                        activity.checkSelfPermission(Manifest.permission.RECORD_AUDIO));
                Call close = new Call(options);
                plugin.close(close);
                assertTrue(close.completed);
                plugin.close(new Call(options));
                Call stopped = new Call(options.put("outputId", 1));
                plugin.stopOutput(stopped);
                assertNotNull(stopped.error);
            });
        }
    }
}
