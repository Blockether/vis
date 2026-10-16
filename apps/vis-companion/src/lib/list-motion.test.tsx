// @vitest-environment jsdom
import { render } from '@testing-library/react';
import { useRef } from 'react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import { LIST_MOTION_MS, MOTION_KEY, useListMotion } from './list-motion';

// Each entry is 40 px tall and stands at its index; the list starts at 100 px.
function List({ keys }: { keys: string[] }) {
  const ref = useRef<HTMLDivElement>(null);
  useListMotion(ref, keys);
  return (
    <div ref={ref} data-list>
      {keys.map((key, index) => (
        <div key={key} {...{ [MOTION_KEY]: key }} data-index={index} />
      ))}
    </div>
  );
}

type Played = { key: string | null; frames: Keyframe[] };

describe('useListMotion', () => {
  let played: Played[];
  let reduced: boolean;
  // The shift a running slide adds to an entry; a browser counts it in both reads.
  let shifts: Map<string, number>;

  beforeEach(() => {
    played = [];
    reduced = false;
    shifts = new Map();
    vi.spyOn(HTMLElement.prototype, 'getBoundingClientRect').mockImplementation(function (this: HTMLElement) {
      const shift = shifts.get(this.getAttribute(MOTION_KEY) ?? '') ?? 0;
      const top = this.hasAttribute('data-list') ? 100 : 100 + Number(this.dataset.index) * 40 + shift;
      return { top, bottom: top + 40, left: 0, right: 100, width: 100, height: 40, x: 0, y: top } as DOMRect;
    });
    HTMLElement.prototype.animate = vi.fn(function (this: HTMLElement, frames: Keyframe[]) {
      played.push({ key: this.getAttribute(MOTION_KEY), frames });
      return { cancel: vi.fn(), addEventListener: vi.fn() } as unknown as Animation;
    }) as unknown as HTMLElement['animate'];
    window.matchMedia = ((query: string) => ({ matches: reduced && query.includes('reduce') })) as typeof window.matchMedia;
  });

  afterEach(() => {
    vi.restoreAllMocks();
    delete (HTMLElement.prototype as Partial<HTMLElement>).animate;
  });

  // Regression, user report in this Vis session: deleting or archiving a group made the
  // groups below it jump into place with no motion.
  it('slides the entries below a removed one up from their old places', () => {
    const view = render(<List keys={['a', 'b', 'c']} />);
    expect(played).toEqual([]);
    view.rerender(<List keys={['b', 'c']} />);
    expect(played).toEqual([
      { key: 'b', frames: [{ transform: 'translateY(40px)' }, { transform: 'none' }] },
      { key: 'c', frames: [{ transform: 'translateY(40px)' }, { transform: 'none' }] },
    ]);
    expect(HTMLElement.prototype.animate).toHaveBeenCalledWith(expect.anything(), {
      duration: LIST_MOTION_MS,
      easing: 'ease-out',
    });
  });

  it('fades a new entry in and moves the ones it pushes down', () => {
    const view = render(<List keys={['a', 'b']} />);
    view.rerender(<List keys={['new', 'a', 'b']} />);
    expect(played).toEqual([
      { key: 'new', frames: [{ opacity: 0 }, { opacity: 1 }] },
      { key: 'a', frames: [{ transform: 'translateY(-40px)' }, { transform: 'none' }] },
      { key: 'b', frames: [{ transform: 'translateY(-40px)' }, { transform: 'none' }] },
    ]);
  });

  it('stops a running slide when the next change puts the entry back where it stands', () => {
    const view = render(<List keys={['a', 'b']} />);
    view.rerender(<List keys={['b']} />);
    const slide = vi.mocked(HTMLElement.prototype.animate).mock.results[0]?.value as Animation;
    // The running slide still holds `b` 40 px down, which is where it stands again.
    shifts.set('b', 40);
    vi.spyOn(window, 'getComputedStyle').mockImplementation(
      (node) =>
        ({
          transform: `matrix(1, 0, 0, 1, 0, ${shifts.get((node as HTMLElement).getAttribute(MOTION_KEY) ?? '') ?? 0})`,
        }) as CSSStyleDeclaration,
    );
    view.rerender(<List keys={['a', 'b']} />);
    expect(slide.cancel).toHaveBeenCalled();
    expect(played.filter((one) => one.key === 'b')).toHaveLength(1);
  });

  it('stays still while the same entries render again', () => {
    const view = render(<List keys={['a', 'b']} />);
    view.rerender(<List keys={['a', 'b']} />);
    expect(played).toEqual([]);
  });

  it('stays still for a reader who asks for reduced motion', () => {
    reduced = true;
    const view = render(<List keys={['a', 'b', 'c']} />);
    view.rerender(<List keys={['b', 'c']} />);
    expect(played).toEqual([]);
  });
});
