/// <reference types="vite/client" />

/**
 * package.json `version`, injected by vite `define`. ONE source of truth for the
 * release string the compatibility screen shows — never hand-written.
 */
declare const __VIS_APP_VERSION__: string;
declare const __VIS_APP_BUILD_NUMBER__: string;
declare const __VIS_APP_BUILD_COMMIT__: string;
