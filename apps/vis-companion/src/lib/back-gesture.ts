/**
 * ANDROID READS ITS BACK GESTURE ITSELF. The finger never reaches the page: the system
 * owns both screen edges, and the page gets one `backButton` event when the stroke
 * ends. So the transcript jumped where the phone's own apps show the page under the
 * finger. Since Android 14 the system reports each step of the stroke, and the
 * `BackGesture` plugin passes the steps on. The step itself stays the `backButton` event.
 */
import { Capacitor, registerPlugin, type PluginListenerHandle } from '@capacitor/core';

interface BackGesturePlugin {
  /** Start to report the stroke. Called once the page listens. */
  watch(): Promise<void>;
  addListener(
    event: 'progress',
    listener: (stroke: { progress: number }) => void,
  ): Promise<PluginListenerHandle>;
  addListener(event: 'cancel', listener: () => void): Promise<PluginListenerHandle>;
}

const BackGesture = registerPlugin<BackGesturePlugin>('BackGesture');

/**
 * Report each step of the phone's own back stroke: how much of the way it has come,
 * from 0 to 1, and `null` when the finger takes it back. Answers the way to stop.
 * Nothing is reported where no system reads the stroke: iOS, the web, older Android.
 */
export function watchBackGesture(onStep: (step: number | null) => void): () => void {
  if (Capacitor.getPlatform() !== 'android' || !Capacitor.isPluginAvailable('BackGesture')) {
    return () => {};
  }
  let isStopped = false;
  const handles: PluginListenerHandle[] = [];
  const keep = (handle: PluginListenerHandle) => {
    if (isStopped) void handle.remove();
    else handles.push(handle);
  };
  void Promise.all([
    BackGesture.addListener('progress', (stroke) => onStep(stroke.progress)).then(keep),
    BackGesture.addListener('cancel', () => onStep(null)).then(keep),
  ])
    .then(() => BackGesture.watch())
    // A build without the plugin keeps the plain step.
    .catch(() => undefined);
  return () => {
    isStopped = true;
    for (const handle of handles.splice(0)) void handle.remove();
  };
}
