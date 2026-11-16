package com.blockether.viscompanion;

import android.content.ClipboardManager;
import android.content.Context;
import android.graphics.Rect;
import android.os.SystemClock;
import android.view.ActionMode;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;

import com.getcapacitor.Plugin;
import com.getcapacitor.PluginCall;
import com.getcapacitor.PluginMethod;
import com.getcapacitor.annotation.CapacitorPlugin;

/**
 * Shows the floating system toolbar, with Paste only, at a rectangle of the page. Paste goes
 * to the focused field of the page as the paste shortcut of a keyboard. The page gets a
 * normal paste, and this plugin never reads the clipboard.
 */
@CapacitorPlugin(name = "PasteMenu")
public class PasteMenuPlugin extends Plugin {
    private ActionMode shown;
    private long shownAt;
    private Rect target = new Rect();

    @PluginMethod
    public void show(PluginCall call) {
        Rect rect = rect(call);
        if (rect == null) {
            call.reject("No rectangle for the paste menu.");
            return;
        }
        getActivity()
                .runOnUiThread(
                        () -> {
                            // The second tap of a double tap keeps the toolbar. A later tap
                            // closes it, as in a system text field.
                            if (shown != null) {
                                if (SystemClock.uptimeMillis() - shownAt > 600) shown.finish();
                                call.resolve();
                                return;
                            }
                            ClipboardManager clipboard =
                                    (ClipboardManager)
                                            getContext().getSystemService(Context.CLIPBOARD_SERVICE);
                            // Without content there is nothing to paste and no toolbar.
                            if (clipboard != null && clipboard.hasPrimaryClip()) {
                                View page = getBridge().getWebView();
                                target = rect;
                                shownAt = SystemClock.uptimeMillis();
                                shown = page.startActionMode(toolbar(page), ActionMode.TYPE_FLOATING);
                            }
                            call.resolve();
                        });
    }

    /** The keyboard resizes the page and the box moves. The toolbar goes to its new place. */
    @PluginMethod
    public void move(PluginCall call) {
        Rect rect = rect(call);
        if (rect == null) {
            call.reject("No rectangle for the paste menu.");
            return;
        }
        getActivity()
                .runOnUiThread(
                        () -> {
                            if (shown != null) {
                                target = rect;
                                shown.invalidateContentRect();
                            }
                            call.resolve();
                        });
    }

    @PluginMethod
    public void hide(PluginCall call) {
        getActivity()
                .runOnUiThread(
                        () -> {
                            if (shown != null) shown.finish();
                            call.resolve();
                        });
    }

    /** The rectangle of the call in pixels of the page view, or null when the call has none. */
    private Rect rect(PluginCall call) {
        Double x = call.getDouble("x");
        Double y = call.getDouble("y");
        Double width = call.getDouble("width");
        Double height = call.getDouble("height");
        if (x == null || y == null || width == null || height == null) return null;
        float density = getBridge().getWebView().getResources().getDisplayMetrics().density;
        return new Rect(
                Math.round((float) (x * density)),
                Math.round((float) (y * density)),
                Math.round((float) ((x + width) * density)),
                Math.round((float) ((y + height) * density)));
    }

    private ActionMode.Callback2 toolbar(View page) {
        return new ActionMode.Callback2() {
            @Override
            public boolean onCreateActionMode(ActionMode mode, Menu menu) {
                menu.add(Menu.NONE, android.R.id.paste, Menu.NONE, android.R.string.paste)
                        .setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
                return true;
            }

            @Override
            public boolean onPrepareActionMode(ActionMode mode, Menu menu) {
                return false;
            }

            @Override
            public boolean onActionItemClicked(ActionMode mode, MenuItem item) {
                if (item.getItemId() != android.R.id.paste) return false;
                long now = SystemClock.uptimeMillis();
                int control = KeyEvent.META_CTRL_ON | KeyEvent.META_CTRL_LEFT_ON;
                for (int action : new int[] {KeyEvent.ACTION_DOWN, KeyEvent.ACTION_UP})
                    page.dispatchKeyEvent(
                            new KeyEvent(now, now, action, KeyEvent.KEYCODE_V, 0, control));
                mode.finish();
                return true;
            }

            @Override
            public void onDestroyActionMode(ActionMode mode) {
                if (shown == mode) shown = null;
            }

            @Override
            public void onGetContentRect(ActionMode mode, View view, Rect outRect) {
                outRect.set(target);
            }
        };
    }

    @Override
    protected void handleOnDestroy() {
        if (shown != null) shown.finish();
    }
}
