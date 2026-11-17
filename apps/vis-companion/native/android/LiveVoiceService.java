package com.blockether.viscompanion;

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ServiceInfo;
import android.os.Build;
import android.os.IBinder;
import android.os.PowerManager;

/**
 * Keeps the process, its microphone and the processor while a voice session
 * continues behind a locked screen. It holds no audio. The plugin starts it in
 * the foreground.
 */
public class LiveVoiceService extends Service {
    private static final String CHANNEL = "live-voice";
    private static final String MICROPHONE = "microphone";
    private static final int ID = 7701;
    private PowerManager.WakeLock awake;

    /** A plain start. A stop before the first command must not crash the process. */
    static void start(Context context, boolean microphone) {
        context.startService(new Intent(context, LiveVoiceService.class).putExtra(MICROPHONE, microphone));
    }

    static void stop(Context context) {
        context.stopService(new Intent(context, LiveVoiceService.class));
    }

    // The plugin stops this service when the session ends, which releases the lock.
    @android.annotation.SuppressLint("WakelockTimeout")
    @Override public int onStartCommand(Intent intent, int flags, int startId) {
        if (awake == null) {
            // A muted session has no audio stream to keep the processor on.
            awake = getSystemService(PowerManager.class)
                    .newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, "vis:live-voice");
            awake.acquire();
        }
        getSystemService(NotificationManager.class).createNotificationChannel(
                new NotificationChannel(CHANNEL, "Voice session", NotificationManager.IMPORTANCE_LOW));
        Notification.Builder notice = new Notification.Builder(this, CHANNEL)
                .setSmallIcon(getApplicationInfo().icon)
                .setContentTitle("Voice session is active")
                .setContentText("Open the app to mute or end voice.")
                .setOngoing(true);
        Intent open = getPackageManager().getLaunchIntentForPackage(getPackageName());
        if (open != null)
            notice.setContentIntent(PendingIntent.getActivity(this, 0, open, PendingIntent.FLAG_IMMUTABLE));
        if (Build.VERSION.SDK_INT >= 30) {
            int types = ServiceInfo.FOREGROUND_SERVICE_TYPE_MEDIA_PLAYBACK;
            // Android 14 rejects the microphone type before the permission is granted.
            if (intent != null && intent.getBooleanExtra(MICROPHONE, false))
                types |= ServiceInfo.FOREGROUND_SERVICE_TYPE_MICROPHONE;
            startForeground(ID, notice.build(), types);
        } else {
            startForeground(ID, notice.build());
        }
        return START_NOT_STICKY;
    }

    @Override public void onDestroy() {
        if (awake != null) awake.release();
        awake = null;
        super.onDestroy();
    }

    @Override public void onTaskRemoved(Intent root) { stopSelf(); }

    @Override public IBinder onBind(Intent intent) { return null; }
}
