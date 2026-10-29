// @vitest-environment jsdom
import { render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { afterEach, describe, expect, it, vi } from 'vitest';

import { BandButton, DialogFrame, DialogHeader } from './ui';

afterEach(() => {
  document.body.innerHTML = '';
});

// Regression, user report ("the portal close vs the dialog close and dialog headers are
// different. WE SHOULD NORMALIZE").
//
// Seven surfaces opened over another surface and no two of their headers agreed: two
// heights (`min-h-9` and `min-h-12`), two alignments (a centred title in `DialogFrame`
// and the artifact overlay against a left title with a subtitle in machine settings,
// application settings, the model picker and the paste editor), two paddings — and FOUR
// of the closes were hand-built at the call site, in two different boxes, none of them
// the `CloseButton` this app says is its only way out.
describe('every dialog header is the one dialog header', () => {
  const band = () => document.querySelector('header')!;

  it('is ONE band that spells its own height and paper', () => {
    render(
      <DialogFrame title="Machine settings" onClose={() => {}}>
        <p>body</p>
      </DialogFrame>,
    );

    expect(document.querySelectorAll('header')).toHaveLength(1);
    // A title band inside a dialog/region is not the application's banner landmark.
    expect(band().getAttribute('role')).toBe('presentation');
  });

  it('can omit the visible headline while keeping the dialog and heading named', () => {
    const view = render(
      <DialogFrame title="app.ts" subtitle="src/app.ts">
        <p>body</p>
      </DialogFrame>,
    );

    expect(screen.getByRole('heading', { name: 'app.ts' })).not.toHaveClass('sr-only');
    view.rerender(
      <DialogFrame title="app.ts" subtitle="src/app.ts" isTitleHidden>
        <p>body</p>
      </DialogFrame>,
    );

    expect(screen.getByRole('dialog', { name: 'app.ts' })).toBeVisible();
    expect(screen.getByRole('heading', { name: 'app.ts' })).toHaveClass('sr-only');
    expect(screen.getByText('src/app.ts')).toBeVisible();
  });

  it('keeps a filename on one line without losing its full name', () => {
    const name = 'vis-memory-overlay-full-size-scrolled-upright.png';
    render(<DialogHeader title={name} isTitleTruncated closeLabel={`Close ${name}`} onClose={() => {}} />);

    const title = screen.getByRole('heading', { name });
    expect(title).toHaveAttribute('title', name);
    expect(screen.getByRole('button', { name: `Close ${name}` })).toBeVisible();
  });

  it('routes every close through the one way out, and makes it say its name', async () => {
    const onClose = vi.fn();
    const view = render(
      <DialogFrame title="report.pdf" onClose={onClose}>
        <p>body</p>
      </DialogFrame>,
    );

    // The way out is icon-only, so it says WHAT it closes: three of these bands can
    // stand over one another and a plain "Close" names all three the same.
    const close = screen.getByRole('button', { name: 'Close report.pdf' });
    expect(band().contains(close)).toBe(true);
    await userEvent.click(close);
    expect(onClose).toHaveBeenCalledTimes(1);

    // A dialog may say what LEAVING does instead of naming itself...
    view.rerender(
      <DialogFrame title="Question" closeLabel="Cancel this request" onClose={onClose}>
        <p>body</p>
      </DialogFrame>,
    );
    expect(screen.getByRole('button', { name: 'Cancel this request' })).toBeVisible();

    // ...and a band with no way out builds no button at all.
    view.rerender(
      <DialogFrame title="Question">
        <p>body</p>
      </DialogFrame>,
    );
    expect(band().querySelectorAll('button')).toHaveLength(0);
  });

  // Regression, user report ("these two buttons should be up and then this dialog can
  // be smaller"): a dialog's own verbs stood in a footer docked under the body, so the
  // model picker's `Refresh` and `Manage providers` were half a phone below the rows
  // they act on — and the sheet was pinned at 92% of the glass to hold the gap between
  // them. A band can carry a verb, in the same cell its ✕ already stands in.
  it("carries a band's own verbs between the name and the way out", async () => {
    const onRefresh = vi.fn();
    render(
      <DialogFrame
        title="Model"
        onClose={() => {}}
        actions={<BandButton onClick={onRefresh}>Refresh</BandButton>}
      >
        <p>body</p>
      </DialogFrame>,
    );

    const cells = [...band().children];
    const refresh = screen.getByRole('button', { name: 'Refresh' });
    const close = screen.getByRole('button', { name: 'Close Model' });

    // A verb is a CELL of the band, and the way out is still the last one.
    expect(cells).toContain(refresh);
    expect(cells.indexOf(refresh)).toBeLessThan(cells.indexOf(close));

    await userEvent.click(refresh);
    expect(onRefresh).toHaveBeenCalledTimes(1);
  });
});
