/**
 * Watching the channel a rejection nobody took is reported on — TEST ONLY,
 * imported by no shipped module.
 *
 * On device that channel is the WebView's `unhandledrejection` event; under the
 * runner it is Node's own `process`, and that one is what turns a floating
 * rejection into a failed run. jsdom implements neither, so the watch reaches
 * for the runner's, typed right here rather than pulled in as `@types/node` for
 * an app that ships no Node.
 */
interface RejectionChannel {
  on(event: "unhandledRejection", listener: (reason: unknown) => void): void;
  off(event: "unhandledRejection", listener: (reason: unknown) => void): void;
}

export interface RejectionWatch {
  /** Every rejection that reached the platform while the watch was up. */
  readonly escaped: unknown[];
  /** Call this in a `finally`, or the next test inherits the listener. */
  stop(): void;
}

/** Start recording rejections nobody handled. */
export function watchUnhandledRejections(): RejectionWatch {
  const channel = (globalThis as { process?: RejectionChannel }).process;
  if (!channel) throw new Error("no unhandled-rejection channel to watch");
  const escaped: unknown[] = [];
  const record = (reason: unknown) => escaped.push(reason);
  channel.on("unhandledRejection", record);
  return { escaped, stop: () => channel.off("unhandledRejection", record) };
}

/**
 * A rejection only counts as unhandled at the END of the turn it was rejected
 * in, so a watch is readable one turn later, not the same one.
 */
export async function settleRejections(): Promise<void> {
  await new Promise((resolve) => setTimeout(resolve, 20));
}
