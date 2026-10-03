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

/**
 * Render a tool path workspace-relative, with a compact home-relative fallback.
 *
 * A root arrives absolute (`/Users/ana/vis`), but a label may reach here already
 * abbreviated by whoever produced it (`~/vis/src/a.ts`), so every root is matched
 * in its home form too — otherwise the one path on screen that was already short
 * would be the only one that failed to shorten.
 */
export function workspaceRelativePath(
  value: string | null | undefined,
  roots: readonly (string | null | undefined)[],
): string {
  const path = value?.trim();
  if (!path) return '';

  const normalizedPath = path.replaceAll('\\', '/');
  const candidates = [
    ...new Set(
      roots
        .map((root) => root?.trim().replaceAll('\\', '/'))
        .map((root) =>
          root === '/' || (root != null && /^[A-Za-z]:\/$/.test(root))
            ? root
            : root?.replace(/\/+$/, ''),
        )
        .filter((root): root is string => Boolean(root))
        .flatMap((root) => [root, homeifyPath(root)]),
    ),
  ].sort((left, right) => right.length - left.length);

  for (const root of candidates) {
    const caseInsensitive = /^[A-Za-z]:\//.test(root);
    const comparablePath = caseInsensitive ? normalizedPath.toLowerCase() : normalizedPath;
    const comparableRoot = caseInsensitive ? root.toLowerCase() : root;
    if (comparablePath === comparableRoot) return '.';

    const prefix = comparableRoot.endsWith('/') ? comparableRoot : `${comparableRoot}/`;
    if (comparablePath.startsWith(prefix)) return normalizedPath.slice(prefix.length);
  }

  return homeifyPath(path);
}
