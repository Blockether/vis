package com.blockether.viscompanion;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import com.getcapacitor.JSObject;
import com.getcapacitor.Plugin;
import com.getcapacitor.PluginCall;
import com.getcapacitor.PluginMethod;
import com.getcapacitor.annotation.CapacitorPlugin;

/** Thin system-browser/loopback bridge shared by every PKCE adapter. */
@CapacitorPlugin(name = "OAuthLoopback")
public class OAuthLoopbackPlugin extends Plugin {
    private OAuthLoopback receiver;
    private PluginCall pending;
    private String flowId;

    @PluginMethod
    public void authorize(PluginCall call) {
        getActivity().runOnUiThread(() -> {
            if (pending != null) { call.reject("Another sign-in browser is active."); return; }
            try {
                String id = call.getString("flowId"); Double deadline = call.getDouble("expiresAt");
                if (id == null || id.isEmpty() || deadline == null || !Double.isFinite(deadline)) throw new IllegalArgumentException();
                OAuthLoopback started = new OAuthLoopback(call.getString("authorizationUrl"), call.getString("redirectUri"),
                    call.getString("state"), deadline.longValue());
                receiver = started; pending = call; flowId = id;
                started.start(url -> getActivity().runOnUiThread(() -> { if (pending == call) finish(url); }));
                openBrowser();
            } catch (Exception error) {
                if (pending == call) finish(null); else call.reject("Cannot receive this sign-in callback.");
            }
        });
    }
    private void openBrowser() {
        // The standard Custom Tabs marker asks supporting browsers for system-owned UI;
        // other browsers use ACTION_VIEW. Vis never embeds or reads a provider web page.
        Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse(receiver.authorization.toString()));
        Bundle extras = new Bundle(); extras.putBinder("android.support.customtabs.extra.SESSION", null);
        intent.putExtras(extras); getActivity().startActivity(intent);
    }
    @PluginMethod
    public void reopen(PluginCall call) {
        getActivity().runOnUiThread(() -> {
            if (receiver == null || !flowId.equals(call.getString("flowId"))) { call.reject("No active sign-in browser."); return; }
            try { openBrowser(); call.resolve(); } catch (RuntimeException error) { call.reject("Cannot reopen sign-in."); }
        });
    }
    @PluginMethod
    public void cancel(PluginCall call) {
        getActivity().runOnUiThread(() -> {
            if (flowId != null && flowId.equals(call.getString("flowId"))) finish(null);
            call.resolve();
        });
    }
    private void finish(String url) {
        PluginCall call = pending; if (call == null) return;
        pending = null; flowId = null;
        OAuthLoopback old = receiver; receiver = null; if (old != null) old.cancel();
        if (url == null) { call.reject("Sign-in closed or expired."); return; }
        // No callback is put in the Intent: the code stays inside this bound plugin call.
        Intent back = new Intent(getContext(), getActivity().getClass());
        back.addFlags(Intent.FLAG_ACTIVITY_REORDER_TO_FRONT | Intent.FLAG_ACTIVITY_SINGLE_TOP);
        try { getActivity().startActivity(back); } catch (RuntimeException ignored) { /* Returning manually still completes the same flow. */ }
        JSObject result = new JSObject(); result.put("url", url); call.resolve(result);
    }
    @Override
    protected void handleOnDestroy() { finish(null); }
}
