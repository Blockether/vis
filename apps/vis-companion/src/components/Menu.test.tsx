import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';

import { MenuBack, MenuItem, MENU_WIDTH } from './Menu';

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

  });

  describe('MenuItem', () => {
    const html = (props: Partial<Parameters<typeof MenuItem>[0]> = {}) =>
      renderToStaticMarkup(<MenuItem title="Manage projects" onSelect={() => {}} {...props} />);

    it('is an ordinary button in the dialog’s native Tab order', () => {
      expect(html()).toContain('<button');
      expect(html()).not.toContain('role="menuitem"');
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

});
