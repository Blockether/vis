// @vitest-environment jsdom
import { act, fireEvent, render } from '@testing-library/react';
import { useRef } from 'react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import { CLAMP_CLASS, CLAMP_MOTION_MS, disclosureAnchor, useClampMotion } from './clamp-motion';

// A clamped body is 80 px tall and an open one is 200 px.
function Body() {
  const ref = useRef<HTMLDivElement>(null);
  const { isOpen, clampClass, toggle } = useClampMotion(ref);
  return (
    <>
      <div ref={ref} data-body className={clampClass} />
      <button type="button" onClick={toggle}>
        {isOpen ? 'Show less' : 'Show more'}
      </button>
    </>
  );
}

const rect = (top: number, height: number) =>
  ({ top, bottom: top + height, left: 0, right: 100, width: 100, height, x: 0, y: top }) as DOMRect;

describe('useClampMotion', () => {
  let played: Keyframe[][];
  let finish: (() => void)[];
  let reduced: boolean;

  beforeEach(() => {
    played = [];
    finish = [];
    reduced = false;
    vi.spyOn(HTMLElement.prototype, 'getBoundingClientRect').mockImplementation(function (this: HTMLElement) {
      return rect(0, this.classList.contains(CLAMP_CLASS) ? 80 : 200);
    });
    HTMLElement.prototype.animate = vi.fn(function (frames: Keyframe[]) {
      played.push(frames);
      return {
        cancel: vi.fn(),
        addEventListener: (type: string, listener: () => void) => {
          if (type === 'finish') finish.push(listener);
        },
      } as unknown as Animation;
    }) as unknown as HTMLElement['animate'];
    window.matchMedia = ((query: string) => ({ matches: reduced && query.includes('reduce') })) as typeof window.matchMedia;
  });

  afterEach(() => {
    vi.restoreAllMocks();
    delete (HTMLElement.prototype as Partial<HTMLElement>).animate;
  });

  // Regression, user report in this Vis session (2026-11-02): "Show more" and "Show less"
  // on a clamped Council request or agent task had no motion, and the view jumped.
  it('slides open from the clamped height, and clamps a closing body only after its slide', () => {
    const view = render(<Body />);
    const body = view.container.querySelector('[data-body]')!;
    fireEvent.click(view.getByRole('button', { name: 'Show more' }));
    expect(body).not.toHaveClass(CLAMP_CLASS);
    expect(played).toEqual([
      [
        { height: '80px', overflow: 'hidden' },
        { height: '200px', overflow: 'hidden' },
      ],
    ]);
    expect(HTMLElement.prototype.animate).toHaveBeenCalledWith(expect.anything(), {
      duration: CLAMP_MOTION_MS,
      easing: expect.any(String),
      fill: 'forwards',
    });

    fireEvent.click(view.getByRole('button', { name: 'Show less' }));
    expect(played[1]).toEqual([
      { height: '200px', overflow: 'hidden' },
      { height: '80px', overflow: 'hidden' },
    ]);
    expect(body).not.toHaveClass(CLAMP_CLASS);
    act(() => finish[1]());
    expect(body).toHaveClass(CLAMP_CLASS);
  });

  it('ignores the end of a slide that a second press replaced', () => {
    const view = render(<Body />);
    const body = view.container.querySelector('[data-body]')!;
    fireEvent.click(view.getByRole('button', { name: 'Show more' }));
    fireEvent.click(view.getByRole('button', { name: 'Show less' }));
    fireEvent.click(view.getByRole('button', { name: 'Show more' }));
    act(() => finish[1]());
    expect(body).not.toHaveClass(CLAMP_CLASS);
  });

  it('clamps at once for a reader who wants reduced motion', () => {
    reduced = true;
    const view = render(<Body />);
    const body = view.container.querySelector('[data-body]')!;
    fireEvent.click(view.getByRole('button', { name: 'Show more' }));
    fireEvent.click(view.getByRole('button', { name: 'Show less' }));
    expect(played).toEqual([]);
    expect(body).toHaveClass(CLAMP_CLASS);
  });
});

describe('disclosureAnchor', () => {
  afterEach(() => vi.restoreAllMocks());

  // The same report: the transcript kept the toggle under the body in place, so opening
  // the body pushed the whole transcript up.
  it.each([
    ['the body when its top is in view', 120, 'body'],
    ['the toggle when the body starts above the view', -300, 'toggle'],
  ])('keeps %s', (_, bodyTop, expected) => {
    const view = render(
      <div data-viewport>
        <div id="clamped" data-name="body" />
        <button type="button" aria-controls="clamped" data-name="toggle" />
      </div>,
    );
    vi.spyOn(HTMLElement.prototype, 'getBoundingClientRect').mockImplementation(function (this: HTMLElement) {
      return rect(this.dataset.name === 'body' ? bodyTop : 0, 10);
    });
    const viewport = view.container.querySelector('[data-viewport]')!;
    const toggle = view.getByRole('button');
    expect((disclosureAnchor(viewport, toggle) as HTMLElement).dataset.name).toBe(expected);
  });
});
