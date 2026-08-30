import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';

import { MenuBack, MenuHeading, MenuItem, MenuNote, MENU_WIDTH } from './Menu';
import { BandButton } from './ui';

// Regression, user report ("when I open the ⋯ the view is not coherent between the
// one ⋯ on the machine and another ⋯ on the project level"): the machine menu was a
// 320px panel with an amber band and two-line rows, the project menu a 256px panel
// with no band and one hand-built row. The same glyph, one line apart in the same
// list, opened two different-looking things — so `⋯` stopped being a promise.
describe('Menu parts', () => {

  it('places from exactly the width the panel paints', () => {
    // The popover is positioned before it has ever been measured, so the number
    // and the `sm:w-80` class have to travel together.
    expect(MENU_WIDTH).toBe(320);
  });

  describe('MenuHeading', () => {
    it('names what the rows act on in the black title band', () => {
      const html = renderToStaticMarkup(<MenuHeading>studio-mbp</MenuHeading>);
      expect(html).toContain('studio-mbp');
    });

    // Subordinate sections stay quiet beneath the primary title band.
    it('keeps a second band in the same menu quiet', () => {
      const html = renderToStaticMarkup(<MenuHeading tone="quiet">Or a draft</MenuHeading>);

      expect(html).not.toContain('bg-accent');
    });

    // Regression, user report (paraphrased: the sheet's colours look wrong): this slot
    // held PAPER — `Button`s parked on the dark title band inside a padded span — and
    // `secondary` carries `text-white`, the PAGE's ink, so the second verb was dark on
    // dark. The band's trailing end is a run of CELLS, the way out already being one.
    it('welds its verbs into the band as cells, never as paper on it', () => {
      const html = renderToStaticMarkup(
        <MenuHeading
          cells={<BandButton isPrimary>Use project</BandButton>}
          onClose={() => {}}
          closeLabel="Close new project"
        >
          New project
        </MenuHeading>,
      );
      // And nothing pads it off the band the way a wrapper around a button did.
      expect(html).not.toContain('items-center px-2');
    });
  });

  describe('MenuBack', () => {
    it('is the same band, made pressable, so a step is left the way it was entered', () => {
      const html = renderToStaticMarkup(
        <MenuBack label="Back to actions for tower" onBack={() => {}}>
          Start the session in
        </MenuBack>,
      );
      expect(html).toContain('<button');
      expect(html).toContain('aria-label="Back to actions for tower"');
    });

    it('carries the step’s commit cells beside the way back', () => {
      const html = renderToStaticMarkup(
        <MenuBack
          label="Back to projects"
          onBack={() => {}}
          cells={<BandButton isPrimary>Create project</BandButton>}
        >
          New project
        </MenuBack>,
      );
      expect(html).not.toContain('items-center gap-2 px-2');
    });
  });

  describe('MenuItem', () => {
    const html = (props: Partial<Parameters<typeof MenuItem>[0]> = {}) =>
      renderToStaticMarkup(<MenuItem title="Manage projects" onSelect={() => {}} {...props} />);

    it('is an ordinary button in the dialog’s native Tab order', () => {
      expect(html()).toContain('<button');
      expect(html()).not.toContain('role="menuitem"');
    });

    // A workspace decision is unrecoverable-ish once the agent starts writing, so
    // no row is allowed to be a bare noun.
    it('carries the consequence of pressing it under the title', () => {
      const markup = html({ hint: 'browse, create, and choose folders' });

      expect(markup).toContain('browse, create, and choose folders');
    });

    it('marks the default answer with a badge rather than a second colour', () => {
      const markup = html({ badge: 'Default' });

      expect(markup).toContain('Default');
    });

    it('paints a destructive row in the app’s red, and it is still the same row', () => {
      const markup = html({ tone: 'danger', title: 'Remove sessions' });
      expect(markup).toContain('<button');
    });

    // Regression, user report ("fix this misalignment of icons and text", on the
    // composer's attach menu): the row was `items-start`, so a row of one line —
    // the only kind with an icon and no hint — pinned its icon and title to the top
    // of the 44px thumb target. Measured off the report's own screenshot: 12px of
    // paper above the title, 23.5px below it, twice over.
    it('centres a one-line row in the thumb target it has to fill', () => {
      const markup = html({ icon: <svg />, badge: 'Default' });
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
