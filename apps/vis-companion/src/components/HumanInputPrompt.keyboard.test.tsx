// @vitest-environment jsdom
// Regression, user report: on the iPhone app a double tap on the one-time code did
// not paste, and the keyboard and the screen jumped. The first tap pinned the shell
// above the keyboard, so the sheet docked to its bottom jumped up under the finger.
// The second tap missed the field, focus dropped and the keyboard fell again.
import { act, cleanup, render, screen } from '@testing-library/react';
import { useRef, useState } from 'react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { HumanInputSheet } from './HumanInputPrompt';
import { DialogFrame, Modal } from './ui';
import settingsSource from '../screens/SettingsScreen.tsx?raw';
import { HUMAN_INPUT_REQUESTS } from '../dev/humanInputVariants';
import { initialHumanInputValues, type HumanInputRequest, type HumanInputValues } from '../lib/human-input';
import { useVisualViewportShell } from '../lib/viewport';

const native = vi.hoisted(() => ({
  keyboard: new Map<string, (info: { keyboardHeight: number }) => void>(),
}));

vi.mock('@capacitor/core', async (importOriginal) => {
  const actual = await importOriginal<typeof import('@capacitor/core')>();
  return { ...actual, Capacitor: { ...actual.Capacitor, getPlatform: () => 'ios' } };
});

vi.mock('@capacitor/keyboard', () => ({
  Keyboard: {
    addListener: (event: string, listener: (info: { keyboardHeight: number }) => void) => {
      native.keyboard.set(event, listener);
      return Promise.resolve({ remove: () => void native.keyboard.delete(event) });
    },
    setScroll: () => Promise.resolve(),
    setAccessoryBarVisible: () => Promise.resolve(),
  },
}));

vi.mock('@capacitor/app', () => ({
  App: {
    addListener: () => Promise.resolve({ remove: () => undefined }),
  },
}));

const HEIGHT = 844;

function pointing(kind: 'fine' | 'coarse') {
  vi.stubGlobal('matchMedia', (query: string) => ({
    matches: query.includes('pointer: fine') ? kind === 'fine' : kind === 'coarse' && query.includes('pointer: coarse'),
    media: query,
    addEventListener: () => undefined,
    removeEventListener: () => undefined,
  }));
}

function phone() {
  window.innerWidth = 390;
  window.innerHeight = HEIGHT;
  Object.defineProperty(window, 'visualViewport', {
    configurable: true,
    value: {
      width: 390,
      height: HEIGHT,
      offsetTop: 0,
      addEventListener: () => undefined,
      removeEventListener: () => undefined,
    },
  });
}

/** The app shell with its iOS keyboard driver, and the session pane a form stands in. */
function SessionWithForm({ request }: { request: HumanInputRequest }) {
  const shell = useRef<HTMLDivElement>(null);
  useVisualViewportShell(shell);
  const [values, setValues] = useState<HumanInputValues>(() => initialHumanInputValues(request));
  return (
    <div ref={shell} data-viewport-shell="" data-testid="shell">
      <section data-session-surface="">
        <HumanInputSheet
          request={request}
          values={values}
          onChange={(id, value) => setValues((current) => ({ ...current, [id]: value }))}
          onSubmit={() => undefined}
          onCancel={() => undefined}
        />
      </section>
    </div>
  );
}

/** The app shell with a wide sheet that stops at its content, like Settings. */
function ShellWithSettingsSheet({ hasField = true }: { hasField?: boolean }) {
  const shell = useRef<HTMLDivElement>(null);
  useVisualViewportShell(shell);
  return (
    <div ref={shell} data-viewport-shell="" data-testid="shell">
      <Modal size="wide" isKeyboardReserved onDismiss={() => undefined}>
        <DialogFrame title="Settings">{hasField ? <input aria-label="Machine name" /> : <p>Machines</p>}</DialogFrame>
      </Modal>
    </div>
  );
}

const shellHeight = () => {
  const height = screen.getByTestId('shell').style.height;
  return height === '' ? HEIGHT : Number.parseFloat(height);
};

const reserve = () => {
  const scrim = document.querySelector<HTMLElement>('[data-testid="shell"] [role="presentation"]');
  const value = scrim?.style.getPropertyValue('--keyboard-reserve') ?? '';
  return value === '' ? 0 : Number.parseFloat(value);
};

/** Where the sheet's lower edge stands, measured from the top of the glass. */
const sheetBottom = () => shellHeight() - reserve();

beforeEach(() => {
  native.keyboard.clear();
  localStorage.clear();
  vi.useFakeTimers();
  vi.stubGlobal('requestAnimationFrame', (run: FrameRequestCallback) => {
    run(0);
    return 1;
  });
  vi.stubGlobal('cancelAnimationFrame', () => undefined);
  phone();
});

afterEach(() => {
  cleanup();
  vi.useRealTimers();
  vi.unstubAllGlobals();
});

describe('a form with a typed field on the iPhone', () => {
  it('keeps the one-time code under the finger while the keyboard comes and goes', () => {
    pointing('coarse');
    render(<SessionWithForm request={HUMAN_INPUT_REQUESTS.otp} />);
    const code = screen.getByLabelText(/One-time code/);

    const resting = sheetBottom();
    expect(reserve()).toBeGreaterThan(0);
    expect(resting).toBeLessThan(HEIGHT);

    act(() => code.focus());
    expect(shellHeight()).toBeLessThan(HEIGHT);
    expect(sheetBottom()).toBe(resting);

    act(() => native.keyboard.get('keyboardWillShow')?.({ keyboardHeight: HEIGHT - resting }));
    expect(reserve()).toBe(0);
    expect(sheetBottom()).toBe(resting);

    act(() => {
      native.keyboard.get('keyboardWillHide')?.({ keyboardHeight: 0 });
      native.keyboard.get('keyboardDidHide')?.({ keyboardHeight: 0 });
    });
    expect(shellHeight()).toBe(HEIGHT);
    expect(sheetBottom()).toBe(resting);
  });

  it('stays docked to the bottom edge with a hardware keyboard', () => {
    pointing('fine');
    render(<SessionWithForm request={HUMAN_INPUT_REQUESTS.otp} />);
    expect(reserve()).toBe(0);
  });
});

// The same jump in Settings: a tap on a machine name or a provider key field.
describe('the settings sheet on the iPhone', () => {
  it('reserves the keyboard band and lets a tall sheet keep its height', () => {
    pointing('coarse');
    render(<ShellWithSettingsSheet />);
    const scrim = document.querySelector<HTMLElement>('[data-testid="shell"] [role="presentation"]');
    const resting = sheetBottom();
    expect(reserve()).toBeGreaterThan(0);
    expect(scrim).toHaveClass('items-end-safe');

    act(() => screen.getByRole('textbox', { name: 'Machine name' }).focus());
    expect(shellHeight()).toBeLessThan(HEIGHT);
    expect(sheetBottom()).toBe(resting);
  });

  // Regression, user report: Settings opened on the machine list floated above an empty
  // keyboard band, and the session list showed below it.
  it('stays docked to the bottom edge while no typed field is on the sheet', async () => {
    pointing('coarse');
    const { rerender } = render(<ShellWithSettingsSheet hasField={false} />);
    const scrim = document.querySelector<HTMLElement>('[data-testid="shell"] [role="presentation"]');
    expect(reserve()).toBe(0);
    expect(scrim).toHaveClass('items-end');

    // The sheet sees its fields through a MutationObserver. It reports in a microtask
    // after the commit, so a second act lets that report land.
    const settle = () => act(async () => await Promise.resolve());
    act(() => rerender(<ShellWithSettingsSheet hasField />));
    await settle();
    expect(reserve()).toBeGreaterThan(0);
    expect(scrim).toHaveClass('items-end-safe');

    act(() => rerender(<ShellWithSettingsSheet hasField={false} />));
    await settle();
    expect(reserve()).toBe(0);
  });

  // Regression, user report: Settings with the pairing field open floated above the empty
  // keyboard band, and the session list showed below it. The band is the sheet's paper.
  it('paints the reserved band with the sheet paper down to the bottom edge', async () => {
    pointing('coarse');
    const { rerender } = render(<ShellWithSettingsSheet />);
    const box = () => screen.getByRole('dialog', { name: 'Settings' }).parentElement;
    expect(reserve()).toBeGreaterThan(0);
    expect(box()).toHaveClass('after:bg-panel', 'after:top-full', 'after:h-[var(--keyboard-reserve,0px)]');
    expect(box()).toHaveClass('sm:after:hidden');

    act(() => rerender(<ShellWithSettingsSheet hasField={false} />));
    await act(async () => await Promise.resolve());
    expect(reserve()).toBe(0);
    expect(box()).not.toHaveClass('after:bg-panel');
  });

  it('is the sheet that Settings opens', () => {
    expect(settingsSource).toContain('<Modal size="wide" isKeyboardReserved onDismiss={onClose}>');
  });
});
