// @vitest-environment jsdom
import { fireEvent, render, screen } from '@testing-library/react';
import { afterEach, describe, expect, it } from 'vitest';

import { ListRow } from '../components/ui';
import { SettingsBandsOpen, SettingsPanel } from './settings/SettingsLayout';

afterEach(() => {
  document.body.innerHTML = '';
});

const band = (meta: string) => {
  render(
    <SettingsPanel title="Notifications" meta={meta}>
      <div />
    </SettingsPanel>,
  );
  const header = document.querySelector('header');
  if (!header) throw new Error('the band has no header');
  return header;
};

// Regression, user report ("this element looks awful on iOS", the Notifications
// band): the status was `shrink-0` beside a `flex-1` title, so a long one --
// "0 devices · via <relay host>" -- took 339 of a 390px iPhone's 390, leaving the
// title box 15px wide clipped to "NOTIFI…", the description wrapping one word per
// line, and the band standing 213px tall.
describe('a settings band header carrying a long status', () => {
  it('measures the title at its own width so the status wraps instead', () => {
    const header = band('0 devices · via relay.example.com');
    expect(header.firstElementChild?.className).not.toContain('items-baseline');
    const title = header.querySelector('[role="heading"]');
    expect(title?.parentElement?.className).toContain('flex-wrap');
    expect(title?.className).toContain('flex-auto');
  });

  it('never lets the status claim its width ahead of the name', () => {
    const status = band('0 devices · via relay.example.com').querySelector('span');

    expect(status?.className).not.toContain('shrink-0');
  });

  it('carries no sentence under the name', () => {
    // A band NAMES its group; the description that used to ride under the title
    // said what the rows under it already say, and the prop is gone.
    expect(band('0 devices · via relay.example.com').querySelector('p')).toBeNull();
  });
});

// Regression, user report over the settings dialog on a phone (a screenshot marked on
// both edges): the ✕, the band marks and the row chevrons stood on three different
// trailing edges, because a band centred its mark in a fixed 48px cell INSIDE its own
// gutter while a row's chevron stopped on the gutter itself.
describe('a band and the rows under it keep one rail', () => {
  const gutter = (element: Element) =>
    element.className
      .split(/\s+/)
      .filter((one) => /^(?:sm:)?px-/.test(one))
      .sort()
      .join(' ');

  const foldedBand = () => {
    render(
      <SettingsPanel
        title="Diagnostics"
        disclosure={{ isOpen: false, onToggle: () => {}, label: 'Show diagnostics' }}
      >
        <div />
      </SettingsPanel>,
    );
    return document.querySelector('header')!.firstElementChild!;
  };

  it('ends the band mark on the gutter its heading starts from', () => {
    const slot = foldedBand().lastElementChild!;

    expect(slot.className).toContain('justify-end');
    // What used to hold it: 48px of centred air, standing inside the gutter.
    expect(slot.className).not.toMatch(/\bw-12\b/);
  });

  it('draws the band chevron at the size a row draws it', () => {
    const mark = foldedBand().lastElementChild!.querySelector('svg');

    expect(mark?.getAttribute('class')).toContain('size-3');
  });

  it('gives the band and a list row the same gutter at every width', () => {
    const band = foldedBand();
    render(<ListRow>Macbook</ListRow>);

    expect(gutter(band)).toBe('px-3 sm:px-4');
    expect(gutter(screen.getByRole('button', { name: 'Macbook' }))).toBe(gutter(band));
  });
});

describe('every settings band with a body folds', () => {
  it('starts folded and shows its body behind its own chevron', () => {
    render(
      <SettingsPanel title="Theme">
        <p>Vis Light</p>
      </SettingsPanel>,
    );
    expect(screen.queryByText('Vis Light')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Show Theme' }));
    expect(screen.getByText('Vis Light')).toBeVisible();
    fireEvent.click(screen.getByRole('button', { name: 'Hide Theme' }));
    expect(screen.queryByText('Vis Light')).toBeNull();
  });

  it('starts open where the caller or a search asks for it', () => {
    render(
      <>
        <SettingsPanel title="Add a machine" defaultOpen>
          <p>Pairing code</p>
        </SettingsPanel>
        <SettingsBandsOpen.Provider value>
          <SettingsPanel title="Planning">
            <p>Plan mode</p>
          </SettingsPanel>
        </SettingsBandsOpen.Provider>
      </>,
    );
    expect(screen.getByText('Pairing code')).toBeVisible();
    expect(screen.getByText('Plan mode')).toBeVisible();
  });

  it('keeps the band action a separate control that opens its band', () => {
    let added = 0;
    render(
      <SettingsPanel
        title="MCP servers"
        action={
          <button type="button" onClick={() => (added += 1)}>
            Add an MCP server
          </button>
        }
      >
        <p>context7</p>
      </SettingsPanel>,
    );
    expect(screen.queryByText('context7')).toBeNull();
    // The verb runs once and shows what it adds; it does not toggle the fold.
    fireEvent.click(screen.getByRole('button', { name: 'Add an MCP server' }));
    expect(added).toBe(1);
    expect(screen.getByText('context7')).toBeVisible();
    fireEvent.click(screen.getByRole('button', { name: 'Add an MCP server' }));
    expect(screen.getByText('context7')).toBeVisible();
    fireEvent.click(screen.getByRole('button', { name: 'Hide MCP servers' }));
    expect(screen.queryByText('context7')).toBeNull();
    expect(document.querySelector('header svg')?.getAttribute('class')).toContain('size-3');
  });

  it('shows no chevron on a band without a body', () => {
    render(
      <SettingsPanel title="Notifications" action={<button type="button">Notifications off</button>}>
        {false}
      </SettingsPanel>,
    );
    expect(screen.queryByRole('button', { name: /Notifications$/ })).toBeNull();
    expect(screen.getByRole('heading', { name: 'Notifications' })).toBeVisible();
  });
});
