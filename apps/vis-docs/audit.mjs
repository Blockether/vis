// Run `npm audit` and fail on each high or critical advisory, except an allowed one.
// An allowed advisory still fails when it reaches a runtime package or no longer occurs.
import { spawnSync } from 'node:child_process';
import { readFileSync } from 'node:fs';
import { pathToFileURL } from 'node:url';

// Give each entry a reason. The check fails when the advisory no longer occurs, so remove
// the entry when a fixed release is installed.
export const ALLOWED = {};

const BLOCKING = new Set(['high', 'critical']);

export function findings(report, lock, allowed = ALLOWED) {
  if (report.error) return [`npm audit failed: ${report.error.summary ?? report.error.code}`];
  const problems = [];
  const seen = new Set();
  for (const [name, vulnerability] of Object.entries(report.vulnerabilities ?? {})) {
    for (const via of vulnerability.via) {
      if (typeof via === 'string') continue;
      const id = via.url.split('/').pop();
      seen.add(id);
      if (!BLOCKING.has(via.severity)) continue;
      if (!Object.hasOwn(allowed, id)) {
        problems.push(`${name}: ${id} (${via.severity}) ${via.title}`);
        continue;
      }
      for (const node of vulnerability.nodes) {
        if (!lock.packages[node]?.dev) {
          problems.push(`${name}: ${id} is allowed only in development packages, not in ${node}`);
        }
      }
    }
  }
  for (const id of Object.keys(allowed)) {
    if (!seen.has(id)) problems.push(`${id} no longer occurs. Remove it from ALLOWED in audit.mjs.`);
  }
  return problems;
}

if (import.meta.url === pathToFileURL(process.argv[1]).href) {
  const run = spawnSync('npm', ['audit', '--json'], { encoding: 'utf8', maxBuffer: 64 * 1024 * 1024 });
  const lock = JSON.parse(readFileSync(new URL('./package-lock.json', import.meta.url), 'utf8'));
  const problems = findings(JSON.parse(run.stdout || '{}'), lock);
  for (const problem of problems) console.error(problem);
  if (problems.length) process.exit(1);
  console.log(`npm audit: no blocking advisory. Allowed: ${Object.keys(ALLOWED).join(', ') || 'none'}`);
}
