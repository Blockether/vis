// @vitest-environment jsdom
// The blank-page guard ships inside `index.html`, so the page itself is under test:
// it has to work when the module bundle never runs.
import html from '../index.html?raw';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

const guard = [...html.matchAll(/<script>([\s\S]*?)<\/script>/g)]
  .map((match) => match[1])
  .find((code) => code.includes('vis-boot-recovery'));

function boot() {
  const reload = vi.fn();
  // The parameter shadows the global, so the guard reloads through the spy.
  new Function('location', guard ?? '')({ reload });
  return { reload, root: document.getElementById('root')! };
}

const surface = () => document.getElementById('vis-boot-recovery');

async function settle() {
  await Promise.resolve();
}

function setVisibility(state: DocumentVisibilityState) {
  Object.defineProperty(document, 'visibilityState', { configurable: true, value: state });
  document.dispatchEvent(new Event('visibilitychange'));
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
  Object.defineProperty(document, 'visibilityState', { configurable: true, value: 'visible' });
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
  it('stays out of the way of an app that starts', async () => {
    const { reload, root } = boot();
    root.append(document.createElement('main'));
    await settle();
    vi.advanceTimersByTime(120_000);
    expect(surface()).toBeNull();
    expect(reload).not.toHaveBeenCalled();
  });

  it('starts the app again when a module script failed to load', () => {
    const { reload } = boot();
    const script = document.createElement('script');
    script.src = 'http://127.0.0.1:5273/src/main.tsx';
    document.body.append(script);
    script.dispatchEvent(new Event('error'));
    expect(surface()?.textContent).toContain('/src/main.tsx');
    vi.advanceTimersByTime(999);
    expect(reload).not.toHaveBeenCalled();
    vi.advanceTimersByTime(1);
    expect(reload).toHaveBeenCalledOnce();
  });

  it('starts the app again when a module does not link', () => {
    const { reload } = boot();
    window.dispatchEvent(
      new ErrorEvent('error', { message: "SyntaxError: Importing binding name 'x' is not found." }),
    );
    expect(surface()?.textContent).toContain("binding name 'x'");
    vi.advanceTimersByTime(1_000);
    expect(reload).toHaveBeenCalledOnce();
  });

  it('starts the app again when it never mounts', () => {
    const { reload } = boot();
    vi.advanceTimersByTime(19_000);
    expect(surface()).toBeNull();
    vi.advanceTimersByTime(1_000);
    vi.advanceTimersByTime(1_000);
    expect(reload).toHaveBeenCalledOnce();
  });

  it('waits longer after each failed start, up to 30 seconds', () => {
    sessionStorage.setItem('vis.bootRetries', '3');
    const { reload } = boot();
    window.dispatchEvent(new ErrorEvent('error', { message: 'boom' }));
    vi.advanceTimersByTime(7_999);
    expect(reload).not.toHaveBeenCalled();
    vi.advanceTimersByTime(1);
    expect(reload).toHaveBeenCalledOnce();
    expect(sessionStorage.getItem('vis.bootRetries')).toBe('4');

    sessionStorage.setItem('vis.bootRetries', '9');
    document.body.innerHTML = '<div id="root"></div>';
    const second = boot();
    window.dispatchEvent(new ErrorEvent('error', { message: 'boom' }));
    vi.advanceTimersByTime(30_000);
    expect(second.reload).toHaveBeenCalledOnce();
  });

  it('waits for a hidden page to be shown', () => {
    const { reload } = boot();
    setVisibility('hidden');
    window.dispatchEvent(new ErrorEvent('error', { message: 'boom' }));
    vi.advanceTimersByTime(60_000);
    expect(reload).not.toHaveBeenCalled();
    setVisibility('visible');
    vi.advanceTimersByTime(1_000);
    expect(reload).toHaveBeenCalledOnce();
  });

  it('stands down and resets the delay when the app mounts after all', async () => {
    sessionStorage.setItem('vis.bootRetries', '2');
    const { reload, root } = boot();
    window.dispatchEvent(new ErrorEvent('error', { message: 'boom' }));
    root.append(document.createElement('main'));
    await settle();
    vi.advanceTimersByTime(60_000);
    expect(surface()).toBeNull();
    expect(reload).not.toHaveBeenCalled();
    expect(sessionStorage.getItem('vis.bootRetries')).toBeNull();
  });

  it('starts the app again when the tree is lost after it mounted', async () => {
    const { reload, root } = boot();
    root.append(document.createElement('main'));
    await settle();
    window.dispatchEvent(new ErrorEvent('error', { message: 'Uncaught TypeError: boom' }));
    expect(surface()).toBeNull();
    root.replaceChildren();
    await settle();
    expect(surface()?.textContent).toContain('Uncaught TypeError: boom');
    vi.advanceTimersByTime(1_000);
    expect(reload).toHaveBeenCalledOnce();
  });
});
