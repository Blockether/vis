/** Render a user home directory in the same compact form as the TUI. */
export function homeifyPath(value: string | null | undefined): string {
  const path = value?.trim();
  if (!path) return '';

  const unixHome = /^\/(?:Users|home)\/[^/]+(?=\/|$)|^\/root(?=\/|$)/;
  if (unixHome.test(path)) return path.replace(unixHome, '~');

  const windowsPath = path.replaceAll('\\', '/');
  const windowsHome = /^[A-Za-z]:\/Users\/[^/]+(?=\/|$)/i;
  return windowsHome.test(windowsPath) ? windowsPath.replace(windowsHome, '~') : path;
}
