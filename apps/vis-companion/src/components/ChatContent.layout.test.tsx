// @vitest-environment jsdom
import { render } from '@testing-library/react';
import { describe, expect, it } from 'vitest';
import { AssistantMessage, Markdown } from './ChatContent';

const table = '| Scenario | Result |\n| --- | --- |\n| First | 2.38 |\n| Last | -1.40 |';

// Regression: answers had an extra right inset; collapsed table borders painted
// beyond the right edge and lost their bottom edge inside the WebKit scroller.
describe('answer and table geometry', () => {
  it('lets the transcript own both answer insets', () => {
    const view = render(
      <AssistantMessage
        turn={{
          turn_id: 'layout',
          status: 'completed',
          iterations: [{ assistant_prose: 'Narration', answer: table }],
        }}
      />,
    );
    const answer = view.container.querySelector('.bg-answer')!;
    expect(answer.classList.contains('pr-3')).toBe(false);
    expect(answer.classList.contains('pl-3')).toBe(false);
    const narration = view.getByText('Narration').closest('.text-vis-message')!;
    expect(narration.classList.contains('pr-3')).toBe(false);
  });

  it('keeps the grid inside the scrollable table without collapsed paint', () => {
    const view = render(<Markdown>{table}</Markdown>);
    const grid = view.getByRole('table');
    expect(grid).toHaveClass('border-separate', 'border-spacing-0');
    expect(grid).not.toHaveClass('border-collapse');
    expect(grid.parentElement).toHaveClass('overflow-x-auto', 'border', 'border-code-edge');
    expect(view.getByRole('region', { name: 'Table' })).toHaveAttribute('tabindex', '0');
    for (const cell of grid.querySelectorAll('th, td')) {
      expect(cell).toHaveClass('border-r', 'last:border-r-0');
      expect(cell).not.toHaveClass('border');
    }
  });

  it.each([false, true])('retains a complete final edge, compact=%s', (compact) => {
    const view = render(<Markdown compact={compact}>{table}</Markdown>);
    const grid = view.getByRole('table') as HTMLTableElement;
    expect(grid.parentElement).toHaveClass('border', 'border-code-edge');
    for (const cell of grid.rows[grid.rows.length - 1].cells) {
      expect(cell).toHaveClass('border-t');
      expect(cell).not.toHaveClass('border-b');
    }
    expect(grid.rows).toHaveLength(3);
  });
});
