import { execFileSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';

const repoRoot = fileURLToPath(new URL('../../../', import.meta.url));

function git(...args: string[]): string {
  try {
    return execFileSync('git', args, {
      cwd: repoRoot,
      encoding: 'utf8',
      stdio: ['ignore', 'pipe', 'ignore'],
    }).trim();
  } catch {
    return '';
  }
}

/** Immutable source identity embedded in both the web bundle and native app. */
export function companionBuildInfo(): { buildNumber: string; commit: string } {
  const suppliedCommit = process.env.VIS_APP_COMMIT?.trim() || process.env.GITHUB_SHA?.trim();
  const baseCommit = suppliedCommit || git('rev-parse', '--short=12', 'HEAD') || 'unknown';
  const dirty = !suppliedCommit && git('status', '--porcelain') !== '';
  return {
    buildNumber:
      process.env.VIS_APP_BUILD_NUMBER?.trim() || git('rev-list', '--count', 'HEAD') || 'dev',
    commit: `${baseCommit.slice(0, 12)}${dirty ? '-dirty' : ''}`,
  };
}
