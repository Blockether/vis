// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from 'vitest';

import {
  approxBytes,
  buildHeatmap,
  installPerfProbes,
  perfActive,
  perfEnabled,
  perfReport,
  registerMemoryOwner,
  registerMemorySource,
  reloadWithStoredPerf,
  setPerfEnabled,
  siteOf,
  type MemoryCell,
  type WatchCollection,
} from './perf';

let undo: (() => void) | null = null;

afterEach(() => {
  undo?.();
  undo = null;
  localStorage.clear();
});

/** Stands in for the garbage collector: the test says when a target is reclaimed. */
function manualCollection() {
  const held = new Map<object, object>();
  let settle: (value: object) => void = () => {};
  const watch: WatchCollection = (collected) => {
    settle = collected;
    return {
      register: (target, value) => {
        held.set(target, value);
      },
      unregister: (token) => {
        for (const [target, value] of held) if (value === token) held.delete(target);
      },
    };
  };
  return {
    watch,
    watching: (target: object) => held.has(target),
    collect(target: object) {
      const value = held.get(target);
      if (!value) throw new Error('The target is not watched.');
      held.delete(target);
      settle(value);
    },
  };
}

function install() {
  const gc = manualCollection();
  undo = installPerfProbes(globalThis, gc.watch);
  return gc;
}

/** Live listeners for one event type, so the page's own listeners do not count. */
function live(type: string): number {
  return perfReport()
    .listeners.groups.filter((group) => group.type === type)
    .reduce((sum, group) => sum + group.live, 0);
}

function sitesNamed(sites: { site: string; live: number }[], name: string): number {
  return sites.filter((entry) => entry.site.startsWith(`${name} `)).reduce((sum, entry) => sum + entry.live, 0);
}

function restore(owner: object, key: string, descriptor: PropertyDescriptor | undefined) {
  if (descriptor) Object.defineProperty(owner, key, descriptor);
  else delete (owner as Record<string, unknown>)[key];
}

describe('perfEnabled', () => {
  it('is always on in the perf build', () => {
    expect(perfEnabled('', 'perf')).toBe(true);
  });

  it('remembers ?perf=1 until ?perf=0 turns it off', () => {
    expect(perfEnabled('', 'production')).toBe(false);
    expect(perfEnabled('?perf=1', 'production')).toBe(true);
    expect(perfEnabled('', 'production')).toBe(true);
    expect(perfEnabled('?perf=0', 'production')).toBe(false);
    expect(perfEnabled('', 'production')).toBe(false);
  });

  it('follows the switch in Settings', () => {
    expect(setPerfEnabled(true)).toBe(true);
    expect(perfEnabled('', 'production')).toBe(true);
    expect(setPerfEnabled(false)).toBe(true);
    expect(perfEnabled('', 'production')).toBe(false);
  });

  it('applies an address switch the device does not store', () => {
    const setItem = vi.spyOn(localStorage, 'setItem').mockImplementation(() => {
      throw new DOMException('Storage is full.', 'QuotaExceededError');
    });
    try {
      expect(perfEnabled('?perf=1', 'production')).toBe(true);
      expect(perfEnabled('', 'production')).toBe(false);
    } finally {
      setItem.mockRestore();
    }
  });
});

describe('reloadWithStoredPerf', () => {
  function page(href: string) {
    return { href, reload: vi.fn(), replace: vi.fn() };
  }

  it('drops the address switch so the stored choice decides', () => {
    const current = page('http://127.0.0.1:5274/?perf=1&gw=local#/s/abc');

    reloadWithStoredPerf(current);

    expect(current.replace).toHaveBeenCalledWith('http://127.0.0.1:5274/?gw=local#/s/abc');
    expect(current.reload).not.toHaveBeenCalled();
  });

  it('reloads an address without a switch as it is', () => {
    const current = page('http://127.0.0.1:5274/#/s/abc');

    reloadWithStoredPerf(current);

    expect(current.reload).toHaveBeenCalledOnce();
    expect(current.replace).not.toHaveBeenCalled();
  });
});

describe('installPerfProbes', () => {
  it('stays off in a WebView without WeakRef', () => {
    const scope = globalThis as { WeakRef?: WeakRefConstructor };
    const native = scope.WeakRef;
    delete scope.WeakRef;
    try {
      undo = installPerfProbes();
      expect(perfActive()).toBe(false);
    } finally {
      scope.WeakRef = native;
    }
  });
});

describe('siteOf', () => {
  it('names the first frame outside the probes', () => {
    const stack = [
      'Error',
      '    at EventTarget.visPerfAddEventListener (http://127.0.0.1:5274/assets/index.js:10:5)',
      '    at anySignal (http://127.0.0.1:5274/assets/index.js:5098:12)',
    ].join('\n');
    expect(siteOf(stack)).toBe('anySignal (index.js:5098)');
  });

  it('reads WebKit frames and anonymous functions', () => {
    const webkit = [
      'visPerfSetInterval@app://localhost/assets/index.js:1:2',
      'visPerfSetInterval@app://localhost/assets/index.js:3:4',
      'startPolling@app://localhost/assets/index.js:40:3',
    ].join('\n');
    expect(siteOf(webkit)).toBe('startPolling (index.js:40)');
    expect(siteOf('Error\n    at http://127.0.0.1:5274/assets/index.js:7:1')).toBe('(anonymous) (index.js:7)');
    expect(siteOf(undefined)).toBe('(unknown)');
  });
});

describe('listener probes', () => {
  it('counts each distinct listener until it is removed', () => {
    install();
    const target = new EventTarget();
    const onPing = () => {};
    target.addEventListener('vis-ping', onPing);
    target.addEventListener('vis-ping', onPing);
    target.addEventListener('vis-ping', () => {}, { capture: true });
    expect(live('vis-ping')).toBe(2);
    target.removeEventListener('vis-ping', onPing);
    expect(live('vis-ping')).toBe(1);
  });

  it('releases a once listener when it fires and a signal-bound one on abort', () => {
    install();
    const target = new EventTarget();
    // The page's own realm, as in a browser: jsdom's signal for jsdom's targets.
    const controller = new window.AbortController();
    let fired = 0;
    target.addEventListener(
      'vis-ping',
      () => {
        fired += 1;
      },
      { once: true },
    );
    target.addEventListener(
      'vis-ping',
      () => {
        fired += 1;
      },
      { signal: controller.signal },
    );
    expect(live('vis-ping')).toBe(2);
    target.dispatchEvent(new Event('vis-ping'));
    expect(fired).toBe(2);
    expect(live('vis-ping')).toBe(1);
    controller.abort();
    expect(live('vis-ping')).toBe(0);
  });

  it('keeps counting with a signal from another realm', () => {
    install();
    const target = new EventTarget();
    // Node's controller, not jsdom's: the probe's own method refuses its signal.
    const controller = new AbortController();
    target.addEventListener('vis-ping', () => {}, { signal: controller.signal });
    expect(live('vis-ping')).toBe(1);
    controller.abort();
    expect(live('vis-ping')).toBe(0);
  });

  it('stops counting the listeners of a target the garbage collector reclaimed', () => {
    const gc = install();
    const target = new EventTarget();
    target.addEventListener('vis-ping', () => {});
    target.addEventListener('vis-pong', () => {}, { once: true });
    expect(live('vis-ping') + live('vis-pong')).toBe(2);
    gc.collect(target);
    expect(live('vis-ping') + live('vis-pong')).toBe(0);
  });

  it('stops watching a target once its last listener is removed', () => {
    const gc = install();
    const target = new EventTarget();
    const onPing = () => {};
    target.addEventListener('vis-ping', onPing);
    expect(gc.watching(target)).toBe(true);
    target.removeEventListener('vis-ping', onPing);
    expect(gc.watching(target)).toBe(false);
  });

  it('reports listeners left on elements removed from the page', () => {
    install();
    const panel = document.createElement('section');
    document.body.append(panel);
    panel.addEventListener('vis-ping', () => {});
    expect(perfReport().listeners.detached).toBe(0);
    panel.remove();
    const { listeners } = perfReport();
    expect(listeners.detached).toBe(1);
    expect(listeners.detachedByTarget).toEqual([{ site: 'section', live: 1 }]);
  });

  it('groups listeners by the function that added them', () => {
    install();
    const target = new EventTarget();
    function subscribeToPings() {
      target.addEventListener('vis-ping', () => {});
    }
    subscribeToPings();
    subscribeToPings();
    const group = perfReport().listeners.groups.find((entry) => entry.type === 'vis-ping');
    expect(group).toMatchObject({ target: 'EventTarget', live: 2, added: 2 });
    expect(group?.site).toMatch(/^subscribeToPings \(perf\.test\.ts:\d+\)$/);
  });
});

describe('timer probes', () => {
  it('counts an interval until it is cleared', () => {
    install();
    function startPolling() {
      return setInterval(() => {}, 60_000);
    }
    const polling = startPolling();
    expect(sitesNamed(perfReport().intervals.sites, 'startPolling')).toBe(1);
    clearInterval(polling);
    expect(sitesNamed(perfReport().intervals.sites, 'startPolling')).toBe(0);
  });

  it('counts a timeout until it runs', async () => {
    install();
    function later(done: () => void) {
      setTimeout(done, 0);
    }
    await new Promise<void>((resolve) => {
      later(resolve);
      expect(sitesNamed(perfReport().timeouts.sites, 'later')).toBe(1);
    });
    expect(sitesNamed(perfReport().timeouts.sites, 'later')).toBe(0);
  });
});

describe('observer probes', () => {
  it('reports observed elements, including ones removed from the page', () => {
    install();
    const box = document.createElement('div');
    document.body.append(box);
    const observer = new MutationObserver(() => {});
    observer.observe(box, { attributes: true });
    const mutations = () => perfReport().observers.find((entry) => entry.kind === 'MutationObserver');
    expect(mutations()).toEqual({ kind: 'MutationObserver', observers: 1, targets: 1, detached: 0 });
    box.remove();
    expect(mutations()).toMatchObject({ targets: 1, detached: 1 });
    observer.disconnect();
    expect(mutations()).toMatchObject({ observers: 0, targets: 0 });
  });
});

describe('object URL probes', () => {
  it('counts object URLs and the bytes they pin until they are revoked', () => {
    const create = Object.getOwnPropertyDescriptor(URL, 'createObjectURL');
    const revoke = Object.getOwnPropertyDescriptor(URL, 'revokeObjectURL');
    let serial = 0;
    URL.createObjectURL = () => `blob:vis/${(serial += 1)}`;
    URL.revokeObjectURL = () => {};
    try {
      install();
      const address = URL.createObjectURL(new Blob(['hello']));
      expect(perfReport().objectUrls).toEqual({ live: 1, bytes: 5 });
      URL.revokeObjectURL(address);
      expect(perfReport().objectUrls).toEqual({ live: 0, bytes: 0 });
    } finally {
      undo?.();
      undo = null;
      restore(URL, 'createObjectURL', create);
      restore(URL, 'revokeObjectURL', revoke);
    }
  });
});

describe('memory sources', () => {
  it('ignores caches while the probes are off', () => {
    const stop = registerMemorySource('transcripts', () => [{ source: 'transcripts', bytes: 1, entries: 1 }]);
    expect(perfReport().cells).toEqual([]);
    stop();
  });

  it('collects every registered cache and drops one that fails', () => {
    install();
    const cell: MemoryCell = { source: 'transcripts', session: 'a', bytes: 100, entries: 2 };
    const stop = registerMemorySource('transcripts', () => [cell]);
    registerMemorySource('broken', () => {
      throw new Error('The cache is gone.');
    });
    expect(perfReport().cells).toEqual([cell]);
    stop();
    expect(perfReport().cells).toEqual([]);
  });

  it('creates nothing for an owner while the probes are off', () => {
    let asked = 0;
    const stop = registerMemoryOwner('client', {}, () => {
      asked += 1;
      return [];
    });
    install();
    expect(perfReport().cells).toEqual([]);
    expect(asked).toBe(0);
    stop();
  });

  it('reports what a live owner holds until it stops', () => {
    install();
    const owner = { cells: [{ source: 'attachments', session: 'a', bytes: 5, entries: 1 }] };
    const stop = registerMemoryOwner('client', owner, (held) => held.cells);
    expect(perfReport().cells).toEqual(owner.cells);
    stop();
    expect(perfReport().cells).toEqual([]);
  });
});

describe('approxBytes', () => {
  it('weighs strings by length and objects by their slots', () => {
    expect(approxBytes('abc')).toBe(19);
    expect(approxBytes(42)).toBe(8);
    expect(approxBytes(null)).toBe(0);
    expect(approxBytes({ text: 'hi' })).toBe(24 + 8 + 18);
    expect(approxBytes(['a', 'b'])).toBe(16 + 16 + 17 + 17);
  });

  it('counts a shared object once and a Blob by its size', () => {
    const inner = { text: 'x'.repeat(100) };
    expect(approxBytes([inner, inner])).toBe(16 + 16 + 24 + 8 + 116);
    expect(approxBytes([new Blob(['hello'])])).toBe(16 + 8 + 5);
  });

  it('measures an array again after its length changed', () => {
    const rows = ['a'];
    expect(approxBytes(rows)).toBe(16 + 8 + 17);
    rows.push('b');
    expect(approxBytes(rows)).toBe(16 + 16 + 17 + 17);
  });
});

describe('buildHeatmap', () => {
  const cells: MemoryCell[] = [
    { source: 'transcripts', session: 'a', title: 'Fix login', bytes: 900, entries: 3 },
    { source: 'stream buffer', session: 'a', bytes: 100, entries: 40 },
    { source: 'transcripts', session: 'b', bytes: 300, entries: 1 },
    { source: 'stream buffer', session: 'c', bytes: 50, entries: 5 },
    { source: 'gateway clients', bytes: 20, entries: 2 },
    { source: 'goals', session: 'd', bytes: 0, entries: 0 },
  ];

  it('ranks sessions by total and caches by size', () => {
    const map = buildHeatmap(cells, 'bytes');
    expect(map.columns).toEqual(['transcripts', 'stream buffer', 'gateway clients']);
    expect(map.rows.map((row) => [row.session, row.title, row.total])).toEqual([
      ['a', 'Fix login', 1000],
      ['b', 'b', 300],
      ['c', 'c', 50],
    ]);
    expect(map.machine).toMatchObject({ session: null, total: 20 });
    expect(map.peak).toBe(900);
    expect(map.hidden).toBe(0);
  });

  it('switches to item counts and folds sessions beyond the limit', () => {
    const map = buildHeatmap(cells, 'entries', 1);
    expect(map.columns).toEqual(['stream buffer', 'transcripts', 'gateway clients']);
    expect(map.rows.map((row) => row.session)).toEqual(['a']);
    expect(map.hidden).toBe(2);
    expect(map.peak).toBe(40);
  });
});
