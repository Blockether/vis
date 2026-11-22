// @vitest-environment jsdom
import { act, fireEvent, render, screen, within } from '@testing-library/react';
import { expect, it, vi } from 'vitest';
import { liveViewFromWire } from '../lib/live-view';
import { LiveCanvas } from './LiveCanvas';
import { CanvasThreads } from './CanvasThreads';
import { failAppAction } from '../lib/app-actions';
import { GatewayError } from '../lib/gateway';

function board(withArrows = false) {
  const node = liveViewFromWire({ id: 'view', title: 'Test', seq: 0, nodes: [{
    id: 'board', type: 'canvas', cards: [{ id: 'card', title: 'Plan' }, ...(withArrows ? [{
      id: 'next', title: 'Build', edges: [{ from: 'card', line: 'solid', animation: 'flow', is_active: true }],
    }] : [])],
  }] })?.nodes[0];
  if (node?.type !== 'canvas') throw new Error('Expected a canvas');
  const { rerender } = render(<LiveCanvas node={node} mark={() => null} />);
  const frame = screen.getByRole('group', { name: /^Canvas,/ });
  frame.setPointerCapture = vi.fn();
  frame.hasPointerCapture = () => false;
  const world = frame.firstElementChild as HTMLElement;
  const pose = () => {
    const [x, y, scale] = world.style.transform.match(/-?[\d.]+/g)!.map(Number);
    return { x, y, scale };
  };
  return { frame, pose, point: (settings: Partial<typeof node>) => {
    rerender(<LiveCanvas node={{ ...node, ...settings }} mark={() => null} />);
  } };
}

it('does not pan when a mouse returns after release outside the board', () => {
  const { frame, pose } = board();
  const original = pose();
  fireEvent.pointerDown(frame, { pointerId: 1, pointerType: 'mouse', buttons: 1, clientX: 100 });
  fireEvent.pointerMove(frame, { pointerId: 1, pointerType: 'mouse', buttons: 0, clientX: 180 });
  expect(pose()).toEqual(original);
});

it('ends a Safari pinch when the owner locks the board', () => {
  const { frame, pose, point } = board();
  const gesture = (type: string, scale: number) => {
    const event = new Event(type, { bubbles: true, cancelable: true });
    Object.assign(event, { scale, clientX: 100, clientY: 100 });
    fireEvent(frame, event);
  };
  gesture('gesturestart', 1);
  const original = pose();
  gesture('gesturechange', 2);
  expect(pose()).not.toEqual(original);
  point({ is_locked: true });
  const locked = pose();
  gesture('gesturechange', 3);
  expect(pose()).toEqual(locked);
});

it('ends a mouse drag when the owner points the camera again', () => {
  const { frame, pose, point } = board();
  const mouse = (clientX: number) => ({ pointerId: 1, pointerType: 'mouse', buttons: 1, clientX });
  const original = pose();
  fireEvent.pointerDown(frame, mouse(100));
  fireEvent.pointerMove(frame, mouse(120));
  expect(pose()).not.toEqual(original);
  point({ zoom: 2 });
  const owner = pose();
  fireEvent.pointerMove(frame, mouse(140));
  expect(pose()).toEqual(owner);
});

it('keeps the whole board fitted when the frame changes size', () => {
  const sizes = new Map<Element, () => void>();
  const width = vi.spyOn(HTMLElement.prototype, 'clientWidth', 'get').mockReturnValue(320);
  const height = vi.spyOn(HTMLElement.prototype, 'clientHeight', 'get').mockReturnValue(200);
  vi.stubGlobal('ResizeObserver', class {
    private callback: () => void;
    constructor(callback: () => void) { this.callback = callback; }
    observe(element: Element) { sizes.set(element, this.callback); }
    unobserve(element: Element) { sizes.delete(element); }
    disconnect() {}
  });
  try {
    const { frame, pose, point } = board();
    point({ zoom: 2 });
    fireEvent.click(screen.getByRole('button', { name: 'Fit the board' }));
    const fitted = pose();
    width.mockReturnValue(120);
    act(() => sizes.get(frame)?.());
    expect(pose().scale).toBeLessThan(fitted.scale);
  } finally {
    width.mockRestore();
    height.mockRestore();
    vi.unstubAllGlobals();
  }
});

it.each(['pointer', 'keyboard'])('lets the next %s toolbar activation through after a cancelled drag', (input) => {
  const { frame, pose } = board();
  const mouse = (clientX: number) => ({ pointerId: 1, pointerType: 'mouse', buttons: 1, clientX });
  const original = pose();
  fireEvent.pointerDown(frame, mouse(100));
  fireEvent.pointerMove(frame, mouse(120));
  fireEvent.pointerCancel(frame, mouse(120));
  expect(pose()).not.toEqual(original);
  const fit = screen.getByRole('button', { name: 'Fit the board' });
  if (input === 'pointer') fireEvent.pointerDown(fit, mouse(20));
  fireEvent.click(fit, { detail: input === 'pointer' ? 1 : 0 });
  expect(pose()).toEqual(original);
});

it('moves the board with the midpoint of two fingers', () => {
  const { frame, pose } = board();
  const original = pose();
  const finger = (pointerId: number, clientX: number) => ({ pointerId, clientX, clientY: 100, pointerType: 'touch', buttons: 1 });
  fireEvent.pointerDown(frame, finger(1, 100));
  fireEvent.pointerDown(frame, finger(2, 200));
  fireEvent.pointerMove(frame, finger(1, 120));
  fireEvent.pointerMove(frame, finger(2, 220));
  expect(pose().x - original.x).toBeCloseTo(20);
  expect(pose().y).toBeCloseTo(original.y);
  expect(pose().scale).toBeCloseTo(original.scale);
});


it('stops arrow motion offscreen and when reduced motion is on', () => {
  let observe: (entries: { isIntersecting: boolean }[]) => void = () => {};
  let preferenceChanged = () => {};
  const preference = {
    matches: false,
    addEventListener: (_type: string, listener: () => void) => { preferenceChanged = listener; },
    removeEventListener: vi.fn(),
  };
  const pauseAnimations = vi.fn();
  const unpauseAnimations = vi.fn();
  class Observer {
    constructor(callback: typeof observe) { observe = callback; }
    observe() {}
    disconnect() {}
  }
  const svg = SVGSVGElement.prototype;
  vi.stubGlobal('matchMedia', () => preference);
  vi.stubGlobal('IntersectionObserver', Observer);
  Object.assign(svg, { pauseAnimations, unpauseAnimations });
  try {
    board(true);
    act(() => observe([{ isIntersecting: true }]));
    expect(unpauseAnimations).toHaveBeenCalledTimes(1);
    act(() => observe([{ isIntersecting: false }]));
    expect(pauseAnimations).toHaveBeenCalledTimes(2);
    act(() => observe([{ isIntersecting: true }]));
    expect(unpauseAnimations).toHaveBeenCalledTimes(2);
    preference.matches = true;
    act(preferenceChanged);
    expect(pauseAnimations).toHaveBeenCalledTimes(3);
  } finally {
    Reflect.deleteProperty(svg, 'pauseAnimations');
    Reflect.deleteProperty(svg, 'unpauseAnimations');
    vi.unstubAllGlobals();
  }
});


it('does not offer clipped controls when the owner locks the camera', () => {
  const node = liveViewFromWire({ id: 'view', title: 'Test', seq: 0, nodes: [{
    id: 'board', type: 'canvas', is_selectable: true, is_locked: true,
    cards: [{ id: 'far', title: 'Off board' }, { id: 'near', title: 'On board' }],
  }] })?.nodes[0];
  if (node?.type !== 'canvas') throw new Error('Expected a canvas');
  const width = vi.spyOn(HTMLElement.prototype, 'clientWidth', 'get').mockReturnValue(320);
  const height = vi.spyOn(HTMLElement.prototype, 'clientHeight', 'get').mockReturnValue(200);
  const size = vi.spyOn(HTMLElement.prototype, 'getBoundingClientRect').mockImplementation(function (this: HTMLElement) {
    if (this.hasAttribute('data-canvas')) return new DOMRect(0, 0, 320, 200);
    if (this.closest('[data-canvas-card="far"]')) return new DOMRect(0, -80, 40, 30);
    return new DOMRect(0, 40, 40, 30);
  });
  try {
    const { rerender } = render(<LiveCanvas node={node} mark={() => null} onSelect={() => {}} />);
    expect(screen.getByRole('button', { name: 'Off board' }).closest('[inert]')).not.toBeNull();
    expect(screen.getByRole('button', { name: 'On board' }).closest('[inert]')).toBeNull();
    rerender(<LiveCanvas node={{ ...node, is_locked: false }} mark={() => null} onSelect={() => {}} />);
    expect(screen.getByRole('button', { name: 'Off board' }).closest('[inert]')).toBeNull();
  } finally {
    width.mockRestore();
    height.mockRestore();
    size.mockRestore();
  }
});

it('moves focus into comments and returns it to the opener on Escape', () => {
  const node = liveViewFromWire({ id: 'view', title: 'Test', seq: 0, nodes: [{
    id: 'board', type: 'canvas', cards: [{ id: 'card', title: 'Plan' }],
    threads: [{ id: 'thread', item_id: 'card', messages: [], draft: 'Keep this.' }],
  }] })?.nodes[0];
  if (node?.type !== 'canvas') throw new Error('Expected a canvas');
  const parentKey = vi.fn();
  render(<div onKeyDown={parentKey}>
    <button>Outside</button>
    <LiveCanvas node={node} mark={() => null} />
  </div>);
  const opener = screen.getByRole('button', { name: /1 comment on Plan/ });
  opener.focus();
  fireEvent.click(opener);
  const close = screen.getByRole('button', { name: 'Close comments' });
  expect(close).toHaveFocus();
  fireEvent.keyDown(close, { key: 'Escape' });
  expect(close).not.toBeInTheDocument();
  expect(opener).toHaveFocus();
  expect(parentKey).not.toHaveBeenCalled();
});

it('takes focus when an open comments panel becomes modal', () => {
  const node = liveViewFromWire({ id: 'view', title: 'Test', seq: 0, nodes: [{
    id: 'board', type: 'canvas', cards: [{ id: 'card', title: 'Plan' }],
  }] })?.nodes[0];
  if (node?.type !== 'canvas') throw new Error('Expected a canvas');
  const onClose = vi.fn();
  const comments = (isSheet: boolean) => (
    <div>
      <button>Outside</button>
      <CanvasThreads node={node} itemId="card" isSheet={isSheet} bottom={44} isComposing={false}
        onCompose={() => {}} onOpen={() => {}} onClose={onClose} />
    </div>
  );
  const { rerender } = render(comments(false));
  const outside = screen.getByRole('button', { name: 'Outside' });
  const close = screen.getByRole('button', { name: 'Close comments' });
  outside.focus();
  rerender(comments(true));
  expect(close).toHaveFocus();

  rerender(comments(false));
  expect(close).toHaveFocus();
  expect(outside.closest('[inert]')).toBeNull();
  outside.focus();
  expect(outside).toHaveFocus();

  rerender(comments(true));
  fireEvent.keyDown(document.activeElement!, { key: 'Escape' });
  expect(onClose).toHaveBeenCalledOnce();
});

it.each([
  ['reply', 'a canvas thread holds at most 50 messages'],
  ['new draft', 'the canvas has no item with that id'],
])('shows a refused %s and keeps the draft text', async (kind, message) => {
  const isReply = kind === 'reply';
  const node = liveViewFromWire({ id: 'view', title: 'Test', seq: 0, nodes: [{
    id: 'board', type: 'canvas', cards: [{ id: 'card', title: 'Plan' }],
    threads: isReply ? [{ id: 'thread', item_id: 'card', messages: Array.from({ length: 50 }, (_, i) => ({
      id: `message-${i}`, author: { kind: 'human' }, body: 'Check the plan.', created_at: 1,
    })) }] : [],
  }] })?.nodes[0];
  if (node?.type !== 'canvas') throw new Error('Expected a canvas');
  const actions = { isReadOnly: false, draft: async () => failAppAction(new GatewayError(400, message, {
    error: { type: 'vis/view-invalid-action', message },
  })) };
  render(<CanvasThreads node={node} itemId="card" isSheet={false} bottom={44} isComposing={!isReply}
    actions={actions} onCompose={() => {}} onOpen={() => {}} onClose={() => {}} />);
  const panel = within(screen.getByRole('region', { name: 'Comments on Plan' }));
  if (isReply) fireEvent.click(panel.getByRole('button', { name: 'Reply' }));
  const field = panel.getByRole('textbox');
  fireEvent.change(field, { target: { value: 'My draft' } });
  await act(async () => fireEvent.click(panel.getByRole('button', { name: isReply ? 'Save' : 'Comment' })));
  expect(panel.getByRole('alert')).toHaveTextContent(message);
  expect(field).toHaveValue('My draft');
  fireEvent.change(field, { target: { value: 'New text' } });
  expect(panel.queryByRole('alert')).toBeNull();
});
