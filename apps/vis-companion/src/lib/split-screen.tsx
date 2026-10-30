import { type ComponentType, lazy, type ReactNode, Suspense, useState } from 'react';

import { warm } from './warm';

/**
 * A screen in its own chunk that mounts from memory once that chunk is in.
 *
 * `lazy` alone suspends on the first render of each launch, also when a warm-up
 * already loaded the module: its promise settles a task later. React 19 then
 * holds the fallback that it showed for 300 ms. Measured in Chromium, the first
 * session that a launch opened stood 330 ms on the empty sheet while the main
 * thread was idle.
 */
export function splitScreen<P extends object>(
  load: () => Promise<ComponentType<P>>,
  fallback: ReactNode,
): { Screen: ComponentType<P>; warm: () => void } {
  let loaded: ComponentType<P> | null = null;
  let loading: Promise<ComponentType<P>> | null = null;
  const lazyScreen = () => lazy(async () => ({ default: await read() }));
  // `lazy` keeps a failed load for good, so a failure gets a new one.
  let Lazy = lazyScreen();
  const read = () => {
    loading ??= load().then(
      (screen) => (loaded = screen),
      (cause: unknown) => {
        // A failed chunk is asked for again at the next mount.
        loading = null;
        Lazy = lazyScreen();
        throw cause;
      },
    );
    return loading;
  };
  function Screen(props: P) {
    // Chosen once for each mount: a new element type later would mount the screen again.
    const [Loaded] = useState(() => loaded);
    if (Loaded) return <Loaded {...props} />;
    return (
      <Suspense fallback={fallback}>
        <Lazy {...props} />
      </Suspense>
    );
  }
  return { Screen, warm: () => warm(read()) };
}
