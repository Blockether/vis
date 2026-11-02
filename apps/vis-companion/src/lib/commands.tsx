import { createContext, useContext, useEffect, useLayoutEffect, useRef, type ReactNode } from 'react';

/**
 * WHO CAN RUN A VERB RIGHT NOW.
 *
 * A screen registers the verbs that its own controls already run, under the ids of the
 * shared key map (`keymap.ts`): the composer registers `pick-file`, the shell registers
 * `show-sessions`. The C-x keys and the command palette run a verb through this
 * registry, so they never copy what a control does. A verb with no handler is not
 * available: the band and the palette leave it out.
 *
 * A handler that is `undefined`, `null` or `false` is registered as not available, so a
 * screen can gate a verb on its own state without a second hook.
 */
export type CommandHandlers = Readonly<Record<string, (() => unknown) | false | null | undefined>>;

type Layer = { current: CommandHandlers };

export class CommandRegistry {
  private readonly layers = new Set<Layer>();
  private readonly listeners = new Set<() => void>();
  private version = 0;

  add(layer: Layer): () => void {
    this.layers.add(layer);
    this.changed();
    return () => {
      this.layers.delete(layer);
      this.changed();
    };
  }

  /** The handler of `id`. The newest registration wins. */
  handler(id: string): (() => unknown) | null {
    let found: (() => unknown) | null = null;
    for (const layer of this.layers) {
      const handler = layer.current[id];
      if (typeof handler === 'function') found = handler;
    }
    return found;
  }

  isAvailable(id: string): boolean {
    return this.handler(id) !== null;
  }

  /** Run `id`. Answers false when no screen can run it now. */
  run(id: string): boolean {
    const handler = this.handler(id);
    if (!handler) return false;
    void handler();
    return true;
  }

  changed(): void {
    this.version += 1;
    for (const listener of this.listeners) listener();
  }

  subscribe = (listener: () => void): (() => void) => {
    this.listeners.add(listener);
    return () => this.listeners.delete(listener);
  };

  getVersion = (): number => this.version;
}

const CommandContext = createContext<CommandRegistry | null>(null);

export function CommandProvider({ registry, children }: { registry: CommandRegistry; children: ReactNode }) {
  return <CommandContext.Provider value={registry}>{children}</CommandContext.Provider>;
}

export function useCommandRegistry(): CommandRegistry | null {
  return useContext(CommandContext);
}

/**
 * Register `handlers` while the calling component is mounted. Each run reads the
 * handler of the latest render, so a handler may close over fresh state. Outside a
 * `CommandProvider` (a story or a unit test) this does nothing.
 */
export function useCommands(handlers: CommandHandlers): void {
  const registry = useContext(CommandContext);
  const layer = useRef<Layer>({ current: handlers });
  // The ids that can run now. A change tells the band and the palette to look again.
  const available = Object.keys(handlers)
    .filter((id) => typeof handlers[id] === 'function')
    .join(' ');
  useLayoutEffect(() => {
    layer.current.current = handlers;
  });
  useEffect(() => registry?.add(layer.current), [registry]);
  useEffect(() => {
    registry?.changed();
  }, [registry, available]);
}
