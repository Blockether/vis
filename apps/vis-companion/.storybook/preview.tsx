import type { Decorator, Preview } from '@storybook/react-vite';
import { configure } from 'storybook/test';
import { useLayoutEffect, type ReactNode } from 'react';
import { applyTheme, resolveTheme } from '../src/lib/theme';
import { DEFAULT_THEME, THEMES } from '../src/lib/themes.generated';
import '../src/index.css';

// A whole screen can need longer than Testing Library's one-second default to
// reach its first frame on a busy machine, so every `findBy*` in this gallery
// waits five seconds before it gives up.
configure({ asyncUtilTimeout: 5000 });

/**
 * The palette is the APP's, applied the app's own way: `applyTheme` stamps
 * `data-theme` on the document exactly as `main.tsx` does at launch, and every
 * entry in the toolbar comes from `THEMES` — the catalog `clojure -X:companion-themes`
 * generates from the engine's `theme.clj`. A palette therefore cannot exist in
 * this gallery and be missing from the product.
 *
 * It runs in a LAYOUT effect so the paper is right on the first painted frame:
 * `themes.generated.css` keys every variable off `[data-theme]`, and a story that
 * paints once before the attribute lands flashes unstyled.
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
 * The viewport frame decides a control's box: `sm:` asks whether there is room and
 * `mouse:` whether a pointer drives the page (`src/index.css`).
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
      // A story with broken semantics fails its test; the panel explains why.
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
