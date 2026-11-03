// @vitest-environment jsdom
// Audit report (round 2): a picture whose data the browser cannot decode kept an
// empty plate with a Full screen control, and never said that the preview is broken.
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import { afterEach, describe, expect, it } from 'vitest';

import type { LiveImageNode, LivePicture } from '../lib/live-view';
import { ImageNodeView } from './AppImage';

afterEach(cleanup);

// A PNG header without image data: the host reads its size, the browser cannot decode it.
const BROKEN: LivePicture = { media_type: 'image/png', data: 'iVBORw0KGgoAAAANSUhEUgAAAGQAAAAy', width: 100, height: 50 };

const node = (over: Partial<LiveImageNode> = {}): LiveImageNode =>
  ({ id: 'shot', type: 'image', alt: 'Corrupt screenshot', ...BROKEN, ...over }) as LiveImageNode;

describe('ImageNodeView', () => {
  it('shows the facts and the broken state when the picture cannot be decoded', () => {
    render(<ImageNodeView node={node()} />);
    fireEvent.error(screen.getByRole('img', { name: 'Corrupt screenshot' }));
    expect(screen.getByRole('img', { name: /Corrupt screenshot, .*the preview could not be decoded$/ })).toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /full screen/i })).toBeNull();
  });

  it('falls back to side by side when a swipe picture cannot be decoded', () => {
    render(<ImageNodeView node={node({ before: BROKEN })} />);
    fireEvent.click(screen.getByRole('button', { name: /Swipe/ }));
    fireEvent.error(screen.getByRole('img', { name: 'Before: Corrupt screenshot' }));
    expect(screen.queryByRole('slider')).toBeNull();
    expect(screen.getByText('Before')).toBeInTheDocument();
    expect(screen.getByText('After')).toBeInTheDocument();
  });
});
