// @vitest-environment jsdom
import { fireEvent, screen, waitFor } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';

import { renderSessionScreen } from './session-screen-harness';

const keyboardHide = vi.hoisted(() => vi.fn(() => Promise.resolve()));

vi.mock('@capacitor/core', async (importOriginal) => {
  const original = await importOriginal<typeof import('@capacitor/core')>();
  return {
    ...original,
    Capacitor: new Proxy(original.Capacitor, {
      get(target, property, receiver) {
        if (property === 'getPlatform') return () => 'ios';
        if (property === 'isNativePlatform') return () => true;
        return Reflect.get(target, property, receiver);
      },
    }),
  };
});

vi.mock('@capacitor/keyboard', async (importOriginal) => {
  const original = await importOriginal<typeof import('@capacitor/keyboard')>();
  return {
    ...original,
    Keyboard: new Proxy(original.Keyboard, {
      get(target, property, receiver) {
        if (property === 'hide') return keyboardHide;
        return Reflect.get(target, property, receiver);
      },
    }),
  };
});

describe('sending from the native composer', () => {
  // Regression: native text can arrive before React's change event. The old
  // empty-state disabled button prevented send() from reading the live value.
  it.each(['click', 'touch'])(
    'sends native text via %s without an extra change event',
    async (press) => {
      const submitTurn = vi.fn(() => new Promise(() => {}));
      renderSessionScreen({ client: { submitTurn } });
      const composer = await screen.findByLabelText('Message Vis');
      const send = screen.getByRole('button', { name: 'Send message' });
      Object.getOwnPropertyDescriptor(HTMLTextAreaElement.prototype, 'value')!.set!.call(
        composer,
        'Native keyboard text',
      );

      if (press === 'touch') {
        fireEvent(send, new MouseEvent('pointerdown', { bubbles: true, button: 0 }));
        fireEvent(send, new MouseEvent('pointerup', { bubbles: true, button: 0 }));
        expect(submitTurn).toHaveBeenCalledTimes(1);
      }
      fireEvent.click(send);

      await waitFor(() => expect(submitTurn).toHaveBeenCalledTimes(1));
      expect(submitTurn.mock.calls[0]).toEqual(['s1', 'Native keyboard text', expect.any(Object)]);
    },
  );

  it('does not submit an empty or whitespace-only native value', async () => {
    const submitTurn = vi.fn(() => new Promise(() => {}));
    renderSessionScreen({ client: { submitTurn } });
    const composer = await screen.findByLabelText('Message Vis');
    const send = screen.getByRole('button', { name: 'Send message' });
    fireEvent.click(send);
    fireEvent.change(composer, { target: { value: 'Visible before native deletion' } });
    Object.getOwnPropertyDescriptor(HTMLTextAreaElement.prototype, 'value')!.set!.call(
      composer,
      '   ',
    );
    fireEvent.click(send);
    expect(submitTurn).not.toHaveBeenCalled();
  });
  it('keeps the keyboard and focused composer stable', async () => {
    renderSessionScreen();
    const composer = await screen.findByLabelText('Message Vis');
    const send = screen.getByRole('button', { name: 'Send message' });
    fireEvent.change(composer, { target: { value: 'Keep writing' } });
    composer.focus();

    fireEvent.mouseDown(send);
    fireEvent.click(send);

    expect(document.activeElement).toBe(composer);
    expect(keyboardHide).not.toHaveBeenCalled();
  });

  it('smoothly reveals the submitted prompt after it mounts', async () => {
    renderSessionScreen({
      client: { submitTurn: () => new Promise(() => {}) },
    });
    const composer = await screen.findByLabelText('Message Vis');
    const viewport = screen.getByRole('region', { name: 'Transcript' });
    let scrollTop = 0;
    Object.defineProperties(viewport, {
      scrollHeight: {
        configurable: true,
        get: () => (document.querySelector('[data-live="true"]') ? 1_200 : 600),
      },
      clientHeight: { configurable: true, value: 600 },
      scrollTop: {
        configurable: true,
        get: () => scrollTop,
        set: (value: number) => {
          scrollTop = value;
        },
      },
    });
    const scrollTo = vi.fn();
    viewport.scrollTo = scrollTo;
    fireEvent.change(composer, { target: { value: 'Show this prompt' } });

    fireEvent.click(screen.getByRole('button', { name: 'Send message' }));

    await waitFor(() => expect(scrollTo).toHaveBeenCalledWith({ top: 1_200, behavior: 'smooth' }));
    const smoothCall = scrollTo.mock.calls.findIndex(([options]) => options.behavior === 'smooth');
    expect(
      scrollTo.mock.calls.slice(smoothCall + 1).filter(([options]) => options.behavior === 'auto'),
    ).toHaveLength(0);
    fireEvent(viewport, new Event('scrollend'));
  });
});
