// @vitest-environment jsdom
// The blank-page guard ships inside `index.html`, so the page itself is under test:
// it has to work when the module bundle never runs.
import html from '../index.html?raw';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

const guard = [...html.matchAll(/<script>([\s\S]*?)<\/script>/g)]
  .map((match) => match[1])
  .find((code) => code.includes('vis-boot-failure'));

function boot() {
  const reload = vi.fn();
  // The parameter shadows the global, so the guard reloads through the spy.
  new Function('location', guard ?? '')({ reload });
  return { reload, root: document.getElementById('root')! };
}

const surface = () => document.getElementById('vis-boot-failure');

async function settle() {
  await Promise.resolve();
}

// Each case boots its own guard; drop the listeners of the earlier ones.
const listeners: [EventTarget, string, EventListenerOrEventListenerObject, unknown][] = [];

beforeEach(() => {
  for (const target of [window, document]) {
    const add = target.addEventListener.bind(target);
    vi.spyOn(target, 'addEventListener').mockImplementation((type, listener, options) => {
      if (listener) listeners.push([target, type, listener, options]);
      add(type, listener, options);
    });
  }
  vi.useFakeTimers();
  document.body.innerHTML = '<div id="root"></div>';
  sessionStorage.clear();
});

afterEach(() => {
  for (const [target, type, listener, options] of listeners.splice(0)) {
    target.removeEventListener(type, listener, options as boolean | EventListenerOptions);
  }
  vi.restoreAllMocks();
  vi.useRealTimers();
});

describe('the blank-page guard in index.html', () => {
  it('runs after #root and before the module bundle', () => {
    expect(guard).toContain('MutationObserver');
    const at = html.indexOf('vis-boot-failure');
    expect(html.indexOf('<div id="root">')).toBeLessThan(at);
    expect(at).toBeLessThan(html.indexOf('type="module"'));
  });

  it('stays out of the way of an app that starts', async () => {
    const { root } = boot();
    root.append(document.createElement('main'));
    await settle();
    vi.advanceTimersByTime(60_000);
    expect(surface()).toBeNull();
  });

  it('replaces a tree that an uncaught error removed', async () => {
    const { root } = boot();
    root.append(document.createElement('main'));
    await settle();
    window.dispatchEvent(new ErrorEvent('error', { message: 'Uncaught TypeError: boom' }));
    root.replaceChildren();
    await settle();
    expect(surface()?.textContent).toContain('Vis stopped rendering');
    expect(surface()?.textContent).toContain('Uncaught TypeError: boom');
  });

  it('reports a module script that failed to load', () => {
    boot();
    const script = document.createElement('script');
    script.src = 'http://127.0.0.1:5273/src/main.tsx';
    document.body.append(script);
    script.dispatchEvent(new Event('error'));
    expect(surface()?.textContent).toContain('Vis could not start');
    expect(surface()?.textContent).toContain('/src/main.tsx');
  });

  it('gives up waiting for a bundle that never mounts', () => {
    boot();
    vi.advanceTimersByTime(19_000);
    expect(surface()).toBeNull();
    vi.advanceTimersByTime(1_000);
    expect(surface()?.textContent).toContain('did not start');
  });

  it('removes the surface when the app mounts after all', async () => {
    const { root } = boot();
    vi.advanceTimersByTime(20_000);
    expect(surface()?.textContent).toContain('Vis could not start');
    root.append(document.createElement('main'));
    await settle();
    expect(surface()).toBeNull();
  });

  it('reloads once when you come back to the broken page', () => {
    const { reload } = boot();
    window.dispatchEvent(new Event('focus'));
    expect(reload).not.toHaveBeenCalled();
    vi.advanceTimersByTime(20_000);
    window.dispatchEvent(new Event('focus'));
    document.dispatchEvent(new Event('visibilitychange'));
    expect(reload).toHaveBeenCalledOnce();
    vi.advanceTimersByTime(15_000);
    window.dispatchEvent(new Event('pageshow'));
    expect(reload).toHaveBeenCalledTimes(2);
  });

  it('reloads from its button', () => {
    const { reload } = boot();
    vi.advanceTimersByTime(20_000);
    surface()?.querySelector('button')?.click();
    expect(reload).toHaveBeenCalledOnce();
  });
});
