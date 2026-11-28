package com.blockether.viscompanion;

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ServiceInfo;
import android.graphics.drawable.Icon;
import android.os.Build;
import android.os.IBinder;
import android.os.PowerManager;

/**
 * Keeps the process, its microphone and the processor while a voice session or a
 * dictation continues behind a locked screen. It holds no audio. The plugin starts
 * it in the foreground. Its notice shows the state and its controls.
 */
public class LiveVoiceService extends Service {
    /** The plugin that owns the voice session. This service has no session. */
    interface Controls { void control(String control); }

    private static final String CHANNEL = "live-voice";
    private static final String MICROPHONE = "microphone";
    private static final String CONTROL = "live-voice.control.";
    private static final int ID = 7701;
    private static final int OUTCOME_ID = 7702;
    // The class lock guards these. Never call the plugin while it is held.
    private static Controls controls;
    private static LiveVoiceState.Display display = LiveVoiceState.Display.CONNECTING;
    private static long startedAt;
    private static boolean shown;
    private PowerManager.WakeLock awake;

    /** A plain start. A stop before the first command must not crash the process. */
    static synchronized void start(Context context, boolean microphone,
            LiveVoiceState.Display first, Controls owner) {
        if (controls == null) {
            display = first;
            startedAt = System.currentTimeMillis();
        }
        controls = owner;
        context.startService(new Intent(context, LiveVoiceService.class).putExtra(MICROPHONE, microphone));
    }

    static synchronized void stop(Context context) {
        controls = null;
        shown = false;
        context.stopService(new Intent(context, LiveVoiceService.class));
    }

    /** Display only. A notice that is not in the foreground stays hidden. */
    static synchronized void show(Context context, LiveVoiceState.Display next) {
        display = next;
        if (shown) context.getSystemService(NotificationManager.class).notify(ID, notice(context));
    }

    /**
     * Says that a dictation's words are in the message. A plain notice, not the service's, so it
     * stays after the service stops and goes away by itself.
     */
    static void transcribed(Context context) {
        Notification.Builder notice = new Notification.Builder(context, CHANNEL)
                .setSmallIcon(context.getApplicationInfo().icon)
                .setContentTitle("Transcribed")
                .setContentText("Your words are in the message")
                .setOnlyAlertOnce(true)
                .setVisibility(Notification.VISIBILITY_PUBLIC)
                .setAutoCancel(true)
                .setTimeoutAfter(4000);
        Intent open = context.getPackageManager().getLaunchIntentForPackage(context.getPackageName());
        if (open != null)
            notice.setContentIntent(PendingIntent.getActivity(context, 0, open, PendingIntent.FLAG_IMMUTABLE));
        context.getSystemService(NotificationManager.class).notify(OUTCOME_ID, notice.build());
    }

    private static Notification notice(Context context) {
        Notification.Builder notice = new Notification.Builder(context, CHANNEL)
                .setSmallIcon(context.getApplicationInfo().icon)
                .setContentTitle(display.title())
                .setContentText(display.text())
                .setWhen(startedAt)
                .setShowWhen(true)
                .setUsesChronometer(true)
                .setOnlyAlertOnce(true)
                // The text has no transcript and no session name, so a locked screen can show it.
                .setVisibility(Notification.VISIBILITY_PUBLIC)
                .setOngoing(true);
        if (display.state.equals("transcribing"))
            notice.setProgress(100, Math.max(display.progress, 0), display.progress < 0);
        Intent open = context.getPackageManager().getLaunchIntentForPackage(context.getPackageName());
        if (open != null)
            notice.setContentIntent(PendingIntent.getActivity(context, 0, open, PendingIntent.FLAG_IMMUTABLE));
        for (String control : display.controls()) {
            // A service intent needs no unlock. Each control has its own action name.
            PendingIntent press = PendingIntent.getService(context, 0,
                    new Intent(context, LiveVoiceService.class).setAction(CONTROL + control),
                    PendingIntent.FLAG_IMMUTABLE);
            notice.addAction(new Notification.Action.Builder(
                    (Icon) null, LiveVoiceState.Display.label(control), press).build());
        }
        return notice.build();
    }

    // The plugin stops this service when the session ends, which releases the lock.
    @android.annotation.SuppressLint("WakelockTimeout")
    @Override public int onStartCommand(Intent intent, int flags, int startId) {
        String action = intent == null ? null : intent.getAction();
        if (action != null && action.startsWith(CONTROL)) {
            Controls owner;
            boolean active;
            synchronized (LiveVoiceService.class) {
                owner = controls;
                active = shown;
            }
            if (owner != null) owner.control(action.substring(CONTROL.length()));
            // A control with no session must not leave a service behind.
            if (!active) stopSelf();
            return START_NOT_STICKY;
        }
        if (awake == null) {
            // A muted session has no audio stream to keep the processor on.
            awake = getSystemService(PowerManager.class)
                    .newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, "vis:live-voice");
            awake.acquire();
        }
        getSystemService(NotificationManager.class).createNotificationChannel(
                new NotificationChannel(CHANNEL, "Voice session", NotificationManager.IMPORTANCE_LOW));
        synchronized (LiveVoiceService.class) {
            if (Build.VERSION.SDK_INT >= 30) {
                int types = ServiceInfo.FOREGROUND_SERVICE_TYPE_MEDIA_PLAYBACK;
                // Android 14 rejects the microphone type before the permission is granted.
                if (intent != null && intent.getBooleanExtra(MICROPHONE, false))
                    types |= ServiceInfo.FOREGROUND_SERVICE_TYPE_MICROPHONE;
                startForeground(ID, notice(this), types);
            } else {
                startForeground(ID, notice(this));
            }
            shown = true;
        }
        return START_NOT_STICKY;
    }

    @Override public void onDestroy() {
        synchronized (LiveVoiceService.class) { shown = false; }
        if (awake != null) awake.release();
        awake = null;
        super.onDestroy();
    }

    @Override public void onTaskRemoved(Intent root) { stopSelf(); }

    @Override public IBinder onBind(Intent intent) { return null; }
}
