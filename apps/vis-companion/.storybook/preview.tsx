import type { Decorator, Preview } from '@storybook/react-vite';
import { useLayoutEffect, type ReactNode } from 'react';
import { applyTheme, resolveTheme } from '../src/lib/theme';
import { DEFAULT_THEME, THEMES } from '../src/lib/themes.generated';
import '../src/index.css';

/**
 * The palette is the APP's, applied the app's own way: `applyTheme` stamps
 * `data-theme` on the document exactly as `main.tsx` does at launch, and the six
 * entries in the toolbar are `THEMES` — the catalog `clojure -X:companion-themes`
 * generates from the engine's `theme.clj`. A palette therefore cannot exist in
 * this gallery and be missing from the product, and a contrast measured here is
 * the contrast that ships.
 *
 * It runs in a LAYOUT effect so the paper is right on the first painted frame:
 * `themes.generated.css` keys every variable off `[data-theme]`, and a story that
 * paints once before the attribute lands is a story photographed unstyled.
 */
function Themed({ id, children }: { id: string; children: ReactNode }) {
  useLayoutEffect(() => {
    applyTheme(resolveTheme(id));
  }, [id]);
  return <div className="min-h-dvh bg-ink">{children}</div>;
}

const withTheme: Decorator = (Story, { globals }) => (
  <Themed id={String(globals.theme ?? DEFAULT_THEME.id)}>
    <Story />
  </Themed>
);

/**
 * THE FRAME ANSWERS THE TWO QUESTIONS, AND NO TOOLBAR OVERRULES IT.
 *
 * `sm:` asks "is there room" and `mouse:` asks "is a pointer driving this"
 * (`@media (width >= 40rem) and (pointer: fine)`, `src/index.css`), so the viewport
 * is what decides a control's box. Measured in this gallery: a default `Button`
 * paints 28px under a 10px label in the phone frame and 32px under an 11px label in
 * the desktop one, while a `density="compact"` Button goes the other way, 32px on
 * the phone and 24px under the pointer. The four sizes are the design skill's
 * canonical viewports.
 */
const preview: Preview = {
  decorators: [withTheme],
  tags: ['autodocs'],
  parameters: {
    layout: 'fullscreen',

    viewport: {
      options: {
        phone: { name: 'Phone 393x852', styles: { width: '393px', height: '852px' } },
        phoneSmall: { name: 'Phone small 375x812', styles: { width: '375px', height: '812px' } },
        tablet: { name: 'Tablet 834x1194', styles: { width: '834px', height: '1194px' } },
        desktop: { name: 'Desktop 1280x800', styles: { width: '1280px', height: '800px' } },
      },
    },

    a11y: {
      // A story with broken semantics is not a picture to approve. The browser
      // project runs this same axe pass in CI; the panel is only its explanation.
      test: 'error',
    },
  },
  initialGlobals: {
    theme: DEFAULT_THEME.id,
    viewport: { value: 'phone', isRotated: false },
  },
  globalTypes: {
    theme: {
      description: 'Palette',
      toolbar: {
        title: 'Theme',
        icon: 'paintbrush',
        dynamicTitle: true,
        items: THEMES.map((theme) => ({ value: theme.id, title: theme.label })),
      },
    },
  },
};

export default preview;
