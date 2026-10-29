// @vitest-environment jsdom
import { render, screen } from '@testing-library/react';
import { afterEach, describe, expect, it } from 'vitest';

import { DialogFrame } from './ui';

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
});
