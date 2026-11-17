package com.blockether.viscompanion;

import androidx.activity.BackEventCompat;
import androidx.activity.OnBackPressedCallback;
import androidx.activity.OnBackPressedDispatcher;

import com.getcapacitor.JSObject;
import com.getcapacitor.Plugin;
import com.getcapacitor.PluginCall;
import com.getcapacitor.PluginMethod;
import com.getcapacitor.annotation.CapacitorPlugin;

/**
 * Passes each step of the system back stroke to the page. The system owns the screen edges,
 * so the page gets no touch from them. Since Android 14 the system reports how far the
 * stroke has come, and the page moves its pane with that number. This plugin does not make
 * the step back: it gives the end of the stroke to the callback below it, and the App plugin
 * sends its one `backButton` event as before.
 */
@CapacitorPlugin(name = "BackGesture")
public class BackGesturePlugin extends Plugin {
    private OnBackPressedCallback stroke;

    /** The page calls this when it listens, so this callback is above the one of the App plugin. */
    @PluginMethod
    public void watch(PluginCall call) {
        getActivity()
                .runOnUiThread(
                        () -> {
                            if (stroke == null) {
                                stroke = stroke(getActivity().getOnBackPressedDispatcher());
                                getActivity()
                                        .getOnBackPressedDispatcher()
                                        .addCallback(getActivity(), stroke);
                            }
                            call.resolve();
                        });
    }

    private OnBackPressedCallback stroke(OnBackPressedDispatcher dispatcher) {
        return new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackStarted(BackEventCompat event) {
                step(event);
            }

            @Override
            public void handleOnBackProgressed(BackEventCompat event) {
                step(event);
            }

            @Override
            public void handleOnBackCancelled() {
                notifyListeners("cancel", new JSObject());
            }

            @Override
            public void handleOnBackPressed() {
                // The dispatcher takes the top enabled callback. With this one off, the
                // step goes to the next callback: the App plugin.
                setEnabled(false);
                try {
                    dispatcher.onBackPressed();
                } finally {
                    setEnabled(true);
                }
            }
        };
    }

    private void step(BackEventCompat event) {
        JSObject stroke = new JSObject();
        stroke.put("progress", event.getProgress());
        notifyListeners("progress", stroke);
    }
}
