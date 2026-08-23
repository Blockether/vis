import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';

import { MenuBack, MenuHeading, MenuItem, MenuNote, MENU_WIDTH } from './Menu';

// Regression, user report ("when I open the ⋯ the view is not coherent between the
// one ⋯ on the machine and another ⋯ on the project level"): the machine menu was a
// 320px panel with an amber band and two-line rows, the project menu a 256px panel
// with no band and one hand-built row. The same glyph, one line apart in the same
// list, opened two different-looking things — so `⋯` stopped being a promise.
describe('Menu parts', () => {
  // Regression, user report ("delete button for project management is wrongly
  // positioned"): the trash is an `edge` IconButton, which reclaims the row's
  // trailing gutter with `-mr-3`. The action wrapper never had one, so the button
  // hung 12px past the sheet's own paper.
  it('gives an action the row’s own trailing gutter to reclaim', () => {
    const html = renderToStaticMarkup(
      <MenuItem title="vis" action={<button type="button">x</button>} onSelect={() => {}} />,
    );

    expect(html).toContain('items-stretch border-b border-dialog-edge pr-3');
  });

  it('places from exactly the width the panel paints', () => {
    // The popover is positioned before it has ever been measured, so the number
    // and the `sm:w-80` class have to travel together.
    expect(MENU_WIDTH).toBe(320);
  });

  describe('MenuHeading', () => {
    it('names what the rows act on in the black title band', () => {
      const html = renderToStaticMarkup(<MenuHeading>studio-mbp</MenuHeading>);

      expect(html).toContain('bg-dialog-title');
      expect(html).toContain('text-dialog-title-foreground');
      expect(html).toContain('uppercase');
      expect(html).toContain('studio-mbp');
    });

    // Subordinate sections stay quiet beneath the primary title band.
    it('keeps a second band in the same menu quiet', () => {
      const html = renderToStaticMarkup(<MenuHeading tone="quiet">Or a draft</MenuHeading>);

      expect(html).not.toContain('bg-accent');
      expect(html).toContain('bg-panel-2');
      expect(html).toContain('text-dialog-hint');
    });

    it('truncates instead of wrapping a long machine name onto two lines', () => {
      expect(renderToStaticMarkup(<MenuHeading>a</MenuHeading>)).toContain('truncate');
    });
  });

  describe('MenuBack', () => {
    it('is the same band, made pressable, so a step is left the way it was entered', () => {
      const html = renderToStaticMarkup(
        <MenuBack label="Back to actions for tower" onBack={() => {}}>
          Start the session in
        </MenuBack>,
      );

      expect(html).toContain('bg-dialog-title');
      expect(html).toContain('<button');
      expect(html).toContain('aria-label="Back to actions for tower"');
      expect(html).toContain('min-h-12');
      expect(html).toContain('mouse:min-h-9');
    });
  });

  describe('MenuItem', () => {
    const html = (props: Partial<Parameters<typeof MenuItem>[0]> = {}) =>
      renderToStaticMarkup(<MenuItem title="Manage projects" onSelect={() => {}} {...props} />);

    it('is a menuitem with a real thumb target', () => {
      expect(html()).toContain('role="menuitem"');
      expect(html()).toContain('min-h-11');
    });

    // A workspace decision is unrecoverable-ish once the agent starts writing, so
    // no row is allowed to be a bare noun.
    it('carries the consequence of pressing it under the title', () => {
      const markup = html({ hint: 'browse, create, and choose folders' });

      expect(markup).toContain('browse, create, and choose folders');
      expect(markup).toContain('text-dialog-hint');
    });

    it('marks the default answer with a badge rather than a second colour', () => {
      const markup = html({ badge: 'Default' });

      expect(markup).toContain('Default');
      expect(markup).toContain('border-edge');
    });

    it('paints a destructive row in the app’s red, and it is still the same row', () => {
      const markup = html({ tone: 'danger', title: 'Remove sessions' });

      expect(markup).toContain('text-err');
      expect(markup).toContain('hover:bg-err/15');
      expect(markup).toContain('min-h-11');
      expect(markup).toContain('role="menuitem"');
    });

    // Regression, user report ("fix this misalignment of icons and text", on the
    // composer's attach menu): the row was `items-start`, so a row of one line —
    // the only kind with an icon and no hint — pinned its icon and title to the top
    // of the 44px thumb target. Measured off the report's own screenshot: 12px of
    // paper above the title, 23.5px below it, twice over.
    it('centres a one-line row in the thumb target it has to fill', () => {
      const markup = html({ icon: <svg />, badge: 'Default' });

      expect(markup).toContain('min-h-11');
      expect(markup).toContain('items-center');
      expect(markup).not.toContain('items-start');
      // Nothing beside the title is nudged down: the centred row already put it on
      // the title's line.
      expect(markup).not.toContain('mt-0.5');
    });

    // The nudge is not gone, it is CONDITIONAL: two lines have a first one to hang
    // the icon and the badge off, and centring them across both would float them
    // against the hint instead.
    it('hangs the icon and the badge off the title’s line once a hint stacks under it', () => {
      const markup = html({ icon: <svg />, badge: 'Default', hint: '3 transcripts' });

      expect(markup.match(/self-start mt-0\.5/g)).toHaveLength(2);
    });

    it('is a real button, so a sheet can hang off the row that opened it', () => {
      expect(html()).toContain('<button');
      expect(html()).not.toContain('<a ');
    });
  });

  describe('MenuNote', () => {
    it('says what a menu with nothing to offer is doing', () => {
      const html = renderToStaticMarkup(<MenuNote>No drafts parked yet.</MenuNote>);

      expect(html).toContain('No drafts parked yet.');
      expect(html).not.toContain('<button');
    });
  });
});
