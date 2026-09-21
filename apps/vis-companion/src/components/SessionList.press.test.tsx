// @vitest-environment jsdom
import { render, screen } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';

import { STORY_GATEWAYS, STORY_SESSION_ROW } from '../dev/story-data';
import { EMPTY_DRAFT_MESSAGE } from '../lib/draft-messages';
import { SessionRow } from './SessionList';

// Regression, user report with a screenshot (paraphrased: selecting a row does not
// highlight the whole row — only the chevron ends up on a different background). The
// press was painted by the open-session button, and the disclosure chevron is that
// button's SIBLING in the row, so the highlight stopped short of the row's own edge
// and the chevron kept standing on the list's paper.
describe('a pressed session row', () => {
  it('stands its chevron on the same paper as its name', () => {
    render(
      <SessionRow
        session={STORY_SESSION_ROW}
        draft={EMPTY_DRAFT_MESSAGE}
        conn={STORY_GATEWAYS[0]}
        match={null}
        needle=""
        commands={{
          open: vi.fn(),
          rename: vi.fn(async () => {}),
          requestDelete: vi.fn(),
          toggleStar: vi.fn(),
        }}
        deletion={null}
      />,
    );
    const chevron = screen.getByRole('button', { name: /details for/i });
    const paper = chevron.closest('[class*="has-[[data-row-surface]:active]:bg-hover"]');
    expect(paper?.querySelector('[data-row-surface]')?.tagName).toBe('BUTTON');
  });
});
