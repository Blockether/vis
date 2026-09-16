// @vitest-environment jsdom
import { fireEvent, render, screen } from '@testing-library/react';
import { describe, expect, it } from 'vitest';
import { ActivityPanel } from './ActivityPanel';
import {
  activityProjectionFromWire,
  type ActivityProjection,
  type ActivityTextFormat,
} from '../lib/activity';
import { ACTIVITY_RESULTS } from '../dev/story-data';

const issue = '[#252](https://github.com/Blockether/vis/issues/252)';

function activity(format?: ActivityTextFormat, state = 'succeeded'): ActivityProjection {
  const projection = structuredClone(ACTIVITY_RESULTS);
  projection.rows = [
    {
      ...projection.rows[0],
      operation: 'vis.issue_find',
      state: state as ActivityProjection['rows'][number]['state'],
      resources: [],
      evidence: [],
      children: undefined,
      error_summary: undefined,
      presentation: {
        headline: 'Find issues',
        summary: `query: ${issue}`,
        ...(format ? { summary_format: format } : {}),
        content: [{ type: 'text', text: 'Full issue result' }],
        sections: [
          {
            headline: 'Related issues',
            summary: issue,
            ...(format ? { summary_format: format } : {}),
            content: [{ type: 'text', text: 'Related issue detail' }],
          },
        ],
      },
    },
  ];
  return projection;
}

function paint(projection: ActivityProjection) {
  const view = render(<ActivityPanel activity={projection} />);
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  return view;
}

// Regression #254: explicitly marked summaries must retain links while collapsed.
describe('Activity presentation summary links', () => {
  it('admits the declared format on root and section summaries', () => {
    const projection = activity('markdown');
    expect(activityProjectionFromWire(projection)).toEqual(projection);
  });

  it.each(['html', null, 42])('rejects unsupported summary format %s', (format) => {
    for (const section of [false, true]) {
      const projection = activity('markdown');
      const presentation = projection.rows[0].presentation!;
      Object.assign(section ? presentation.sections![0] : presentation, { summary_format: format });
      expect(activityProjectionFromWire(projection)).toBeNull();
    }
  });

  it.each(['running', 'succeeded', 'failed', 'cancelled'])(
    'keeps links outside disclosure buttons for %s rows',
    (state) => {
      paint(activity('markdown', state));
      const root = screen.getByRole('button', { name: /Find issues/ });
      const section = screen.getByRole('button', { name: 'Related issues' });
      const links = screen.getAllByRole('link', { name: '#252' });
      expect(links).toHaveLength(2);
      for (const link of links) {
        expect(link.closest('button')).toBeNull();
        expect(link).toHaveAttribute('href', 'https://github.com/Blockether/vis/issues/252');
        expect(link).toHaveAttribute('rel', 'noopener noreferrer');
        const open = root.getAttribute('aria-expanded');
        fireEvent.click(link);
        expect(root).toHaveAttribute('aria-expanded', open);
        expect(section).toHaveAttribute('aria-expanded', 'false');
      }
      fireEvent.click(root);
      fireEvent.click(section);
      expect(screen.getAllByRole('link', { name: '#252' })).toHaveLength(2);
      expect(screen.getByText('Related issue detail')).toBeInTheDocument();
    },
  );

  it.each([undefined, 'inline'] as const)('keeps %s summaries literal', (format) => {
    paint(activity(format));
    expect(screen.queryByRole('link')).toBeNull();
    expect(screen.getByText(`query: ${issue}`)).toBeInTheDocument();
    expect(screen.getByText(issue)).toBeInTheDocument();
  });

  it('never treats an engine error as authored Markdown', () => {
    const projection = activity('markdown', 'failed');
    projection.rows[0].error_summary = `Error ${issue}`;
    projection.rows[0].presentation!.sections = [];
    paint(projection);
    expect(screen.queryByRole('link')).toBeNull();
    expect(screen.getAllByText(`Error ${issue}`).length).toBeGreaterThan(0);
  });

  it('does not create unsafe links, media, HTML or block markup', () => {
    const projection = activity('markdown');
    projection.rows[0].presentation!.summary =
      '[js](javascript:alert(1)) [file](file:///tmp/log) [relative](/settings) ' +
      '![image](https://example.com/image.png) <img src=x> **bold**';
    projection.rows[0].presentation!.sections = [];
    paint(projection);
    const summary = document.querySelector('[data-activity-summary]')!;
    expect(summary.querySelector('a, img, p, h1, ul, table')).toBeNull();
    expect(summary.querySelector('strong')).toHaveTextContent('bold');
  });

  it('keeps an empty marked summary empty', () => {
    const projection = activity('markdown');
    projection.rows[0].presentation!.summary = '';
    projection.rows[0].presentation!.sections![0].summary = '';
    paint(projection);
    expect(document.querySelector('[data-activity-summary]')).toBeNull();
  });
});
