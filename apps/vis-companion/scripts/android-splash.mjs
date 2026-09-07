import { readFileSync, writeFileSync } from 'node:fs';

// AndroidX's Theme.SplashScreen maps this attribute to the platform one in values-v31.
// Putting android:windowSplashScreenBackground in unqualified values requires API 31.
const SPLASH_THEME_ITEM = '<item name="windowSplashScreenBackground">@color/vis_splash</item>';
const LAUNCH_THEME = /(<style name="AppTheme\.NoActionBarLaunch"[^>]*>)([\s\S]*?)(<\/style>)/;

export const brandLaunchTheme = (styles) => {
  if (!LAUNCH_THEME.test(styles)) throw new Error('values/styles.xml has no AppTheme.NoActionBarLaunch to brand');
  return styles.replace(LAUNCH_THEME, (_, opening, body, closing) => {
    const withoutColour = body.replace(
      /\s*<item name="(?:android:)?windowSplashScreenBackground">[\s\S]*?<\/item>/g,
      '',
    );
    return `${opening}\n        ${SPLASH_THEME_ITEM}${withoutColour}${closing}`;
  });
};

export const prepareAndroidSplash = (stylesPath) => {
  const styles = readFileSync(stylesPath, 'utf8');
  const branded = brandLaunchTheme(styles);
  if (branded !== styles) writeFileSync(stylesPath, branded);
};
