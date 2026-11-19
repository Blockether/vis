package com.blockether.viscompanion;

import android.app.Activity;
import android.app.Dialog;
import android.content.ActivityNotFoundException;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.Color;
import android.net.Uri;
import android.text.TextUtils;
import android.util.TypedValue;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.view.WindowInsets;
import android.webkit.HttpAuthHandler;
import android.webkit.WebChromeClient;
import android.webkit.WebResourceRequest;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.TextView;

import androidx.webkit.ProxyConfig;
import androidx.webkit.ProxyController;
import androidx.webkit.WebViewFeature;

import com.getcapacitor.JSArray;
import com.getcapacitor.JSObject;
import com.getcapacitor.Plugin;
import com.getcapacitor.PluginCall;
import com.getcapacitor.PluginMethod;
import com.getcapacitor.annotation.CapacitorPlugin;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executor;

/**
 * An in-app browser for private links: links to hosts that only the gateway can reach.
 *
 * The browser sends the allowed domains through the forward proxy of the gateway and the other
 * hosts direct (a proxy override with reverse bypass). The proxy asks for the gateway token as
 * the user name. The proxy override applies to each web view of the app, so the plugin removes
 * it when the browser closes. The gateway refuses denied domains and ports off its list.
 */
@CapacitorPlugin(name = "ProxyBrowser")
public class ProxyBrowserPlugin extends Plugin {
    /** A wrong token gives a new proxy challenge. After this count the browser stops. */
    private static final int MAX_PROXY_ANSWERS = 2;

    private BrowserDialog dialog;
    private String proxyHost;
    private String token;
    private int proxyAnswers;

    private static boolean isSupported() {
        return WebViewFeature.isFeatureSupported(WebViewFeature.PROXY_OVERRIDE)
                && WebViewFeature.isFeatureSupported(WebViewFeature.PROXY_OVERRIDE_REVERSE_BYPASS);
    }

    @PluginMethod
    public void isAvailable(PluginCall call) {
        JSObject result = new JSObject();
        result.put("available", isSupported());
        call.resolve(result);
    }

    @PluginMethod
    public void open(PluginCall call) {
        String url = call.getString("url");
        String host = call.getString("proxyHost");
        Integer port = call.getInt("proxyPort");
        String secret = call.getString("token");
        List<String> allowed = strings(call.getArray("allowedDomains"));
        Uri page = url == null ? null : Uri.parse(url);
        if (page == null
                || !("http".equals(page.getScheme()) || "https".equals(page.getScheme()))
                || TextUtils.isEmpty(host)
                || port == null
                || TextUtils.isEmpty(secret)
                || allowed.isEmpty()) {
            call.reject("The private link or the gateway proxy is not valid.");
            return;
        }
        if (!isSupported()) {
            call.reject("This Android WebView cannot use a proxy.", "UNAVAILABLE");
            return;
        }
        ProxyConfig.Builder config =
                new ProxyConfig.Builder()
                        .addProxyRule(proxyAddress(host, port), ProxyConfig.MATCH_ALL_SCHEMES)
                        // With reverse bypass, the rules name the hosts that use the proxy.
                        .setReverseBypassEnabled(true);
        for (String pattern : allowed) {
            String base = pattern.startsWith("*.") ? pattern.substring(2) : pattern;
            config.addBypassRule(base);
            config.addBypassRule("*." + base);
        }
        Executor ui = command -> getActivity().runOnUiThread(command);
        ProxyController.getInstance()
                .setProxyOverride(
                        config.build(),
                        ui,
                        () -> {
                            proxyHost = host;
                            token = secret;
                            proxyAnswers = 0;
                            show(url);
                            call.resolve();
                        });
    }

    /** WebView names the proxy by its host, on some versions with its port. */
    private boolean isProxyHost(String host) {
        if (host == null || proxyHost == null) return false;
        String bare = host.replaceAll("^\\[|\\]?(:\\d+)?$", "");
        return bare.equalsIgnoreCase(proxyHost) || host.equalsIgnoreCase(proxyHost);
    }

    private static String proxyAddress(String host, int port) {
        return (host.contains(":") ? "[" + host + "]" : host) + ":" + port;
    }

    private static List<String> strings(JSArray array) {
        List<String> values = new ArrayList<>();
        if (array == null) return values;
        for (int i = 0; i < array.length(); i++) {
            String value = array.optString(i, "").trim().toLowerCase();
            if (!value.isEmpty()) values.add(value);
        }
        return values;
    }

    private void show(String url) {
        if (dialog != null && dialog.isShowing()) {
            dialog.web.loadUrl(url);
            return;
        }
        dialog = new BrowserDialog(getActivity());
        dialog.setOnDismissListener(d -> close());
        dialog.show();
        dialog.web.loadUrl(url);
    }

    private void close() {
        if (dialog != null) {
            dialog.web.stopLoading();
            dialog.web.destroy();
            dialog = null;
        }
        token = null;
        if (isSupported()) {
            ProxyController.getInstance().clearProxyOverride(Runnable::run, () -> {});
        }
    }

    @Override
    protected void handleOnDestroy() {
        if (dialog != null) dialog.dismiss();
    }

    /** A full screen window with a bar (close, title, back, reload) over the web view. */
    private final class BrowserDialog extends Dialog {
        final WebView web;
        private final TextView title;
        private final ProgressBar progress;

        BrowserDialog(Activity activity) {
            super(activity, android.R.style.Theme_DeviceDefault_NoActionBar);
            LinearLayout root = new LinearLayout(activity);
            root.setOrientation(LinearLayout.VERTICAL);
            root.setBackgroundColor(Color.BLACK);
            root.setOnApplyWindowInsetsListener(
                    (view, insets) -> {
                        view.setPadding(
                                insets.getSystemWindowInsetLeft(),
                                insets.getSystemWindowInsetTop(),
                                insets.getSystemWindowInsetRight(),
                                insets.getSystemWindowInsetBottom());
                        return insets;
                    });

            LinearLayout bar = new LinearLayout(activity);
            bar.setOrientation(LinearLayout.HORIZONTAL);
            bar.setGravity(Gravity.CENTER_VERTICAL);
            Button done = button(activity, "\u2715", "Close");
            title = new TextView(activity);
            title.setTextColor(Color.WHITE);
            title.setSingleLine(true);
            title.setEllipsize(TextUtils.TruncateAt.MIDDLE);
            title.setTextSize(TypedValue.COMPLEX_UNIT_SP, 15);
            Button back = button(activity, "\u2039", "Back");
            Button reload = button(activity, "\u21bb", "Reload");
            bar.addView(done);
            bar.addView(title, new LinearLayout.LayoutParams(0, ViewGroup.LayoutParams.WRAP_CONTENT, 1));
            bar.addView(back);
            bar.addView(reload);
            root.addView(bar);

            progress = new ProgressBar(activity, null, android.R.attr.progressBarStyleHorizontal);
            progress.setMax(100);
            root.addView(progress, new LinearLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, dp(activity, 3)));

            web = new WebView(activity);
            WebSettings settings = web.getSettings();
            settings.setJavaScriptEnabled(true);
            settings.setDomStorageEnabled(true);
            settings.setAllowFileAccess(false);
            settings.setAllowContentAccess(false);
            settings.setSupportMultipleWindows(false);
            web.setWebViewClient(new ProxyClient());
            web.setWebChromeClient(
                    new WebChromeClient() {
                        @Override
                        public void onProgressChanged(WebView view, int value) {
                            progress.setProgress(value);
                            progress.setVisibility(value >= 100 ? View.INVISIBLE : View.VISIBLE);
                        }

                        @Override
                        public void onReceivedTitle(WebView view, String text) {
                            if (!TextUtils.isEmpty(text)) title.setText(text);
                        }
                    });
            root.addView(web, new LinearLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, 0, 1));

            done.setOnClickListener(view -> dismiss());
            back.setOnClickListener(view -> onBackPressed());
            reload.setOnClickListener(view -> web.reload());
            requestWindowFeature(Window.FEATURE_NO_TITLE);
            setContentView(root);
        }

        /** Back goes to the previous page first. On the first page it closes the browser. */
        @Override
        public void onBackPressed() {
            if (web.canGoBack()) web.goBack();
            else super.onBackPressed();
        }

        private void showHost(String url) {
            String host = Uri.parse(url).getHost();
            if (host != null) title.setText(host);
        }

        private final class ProxyClient extends WebViewClient {
            @Override
            public boolean shouldOverrideUrlLoading(WebView view, WebResourceRequest request) {
                Uri target = request.getUrl();
                String scheme = target.getScheme();
                if ("http".equals(scheme) || "https".equals(scheme)) return false;
                // mailto:, tel: and app links go to the app of the phone that opens them.
                try {
                    getContext().startActivity(new Intent(Intent.ACTION_VIEW, target));
                } catch (ActivityNotFoundException ignored) {
                    // No app opens this link. The page stays as it is.
                }
                return true;
            }

            @Override
            public void onPageStarted(WebView view, String url, Bitmap favicon) {
                showHost(url);
            }

            /** The proxy of the gateway asks for the gateway token as the user name. */
            @Override
            public void onReceivedHttpAuthRequest(
                    WebView view, HttpAuthHandler handler, String host, String realm) {
                if (token != null
                        && proxyHost != null
                        && isProxyHost(host)
                        && proxyAnswers < MAX_PROXY_ANSWERS) {
                    proxyAnswers++;
                    handler.proceed(token, "vis");
                } else {
                    handler.cancel();
                }
            }
        }
    }

    private static Button button(Activity activity, String text, String label) {
        Button button = new Button(activity, null, android.R.attr.borderlessButtonStyle);
        button.setText(text);
        button.setTextColor(Color.WHITE);
        button.setTextSize(TypedValue.COMPLEX_UNIT_SP, 20);
        button.setContentDescription(label);
        button.setMinWidth(dp(activity, 48));
        button.setMinimumWidth(dp(activity, 48));
        return button;
    }

    private static int dp(Activity activity, int value) {
        return Math.round(value * activity.getResources().getDisplayMetrics().density);
    }
}
