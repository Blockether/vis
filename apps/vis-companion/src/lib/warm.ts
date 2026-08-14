// Starting a chunk nobody is waiting for.
//
// `lazy` keeps the heavy screens out of the launch chunk, and every split point
// warms itself once the shell is up so the first tap mounts from memory instead
// of a round trip. That warm-up is the one load in the app with NO caller: no
// `await`, no render waiting on it, no boundary above it. A promise like that
// has to own its failure, because every way it can fail is real — a chunk the
// last deploy replaced, a WebView that lost the network, or a module graph
// still resolving when the page it belonged to went away. An unhandled
// rejection is then reported by the platform: `unhandledrejection` in the
// WebView, a failed run in the suite, out of code whose whole job was to be
// optional. It cost the Android companion workflow a red run on a suite where
// every test passed.
//
// Losing a warm-up costs nothing else: `lazy` asks for the same chunk again at
// the tap, behind the Suspense boundary that exists for exactly that.
export function warm(load: Promise<unknown>): void {
  void load.catch(() => {});
}
