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

/** Render a tool path workspace-relative, with a compact home-relative fallback. */
export function workspaceRelativePath(
  value: string | null | undefined,
  roots: readonly (string | null | undefined)[],
): string {
  const path = value?.trim();
  if (!path) return '';

  const normalizedPath = path.replaceAll('\\', '/');
  const candidates = roots
    .map((root) => root?.trim().replaceAll('\\', '/'))
    .map((root) =>
      root === '/' || (root != null && /^[A-Za-z]:\/$/.test(root))
        ? root
        : root?.replace(/\/+$/, ''),
    )
    .filter((root): root is string => Boolean(root))
    .sort((left, right) => right.length - left.length);

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

/**
 * Label a project root by its useful location rather than repeating its name.
 * The project title already carries the basename, so matching basenames are
 * removed and deep parents keep their nearest two segments.
 */
export function compactProjectPath(
  value: string | null | undefined,
  projectName: string | null | undefined,
): string {
  const path = homeifyPath(value).replaceAll('\\', '/').replace(/\/+$/, '');
  if (!path) return '';

  const project = projectName?.trim().toLowerCase() ?? '';
  const leaf = path.split('/').at(-1)?.toLowerCase() ?? '';
  const withoutLeaf = project && leaf === project
    ? path.slice(0, path.length - leaf.length).replace(/\/+$/, '')
    : path;
  if (!withoutLeaf || withoutLeaf === '~') return path;

  const prefix = withoutLeaf.startsWith('~/')
    ? '~/'
    : withoutLeaf.startsWith('/')
      ? '/'
      : /^[A-Za-z]:\//.test(withoutLeaf)
        ? `${withoutLeaf.slice(0, 2)}/`
        : '';
  const segments = withoutLeaf.slice(prefix.length).split('/').filter(Boolean);
  if (segments.length <= 2) return withoutLeaf;
  return `${prefix}…/${segments.slice(-2).join('/')}`;
}
