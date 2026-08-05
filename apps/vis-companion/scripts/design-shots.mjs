#!/usr/bin/env node
/**
 * Screenshots every design proposal in `src/dev/DesignGallery.tsx`.
 *
 * Starts this app's own vite dev server, drives it with the `spel` browser CLI
 * at real phone and desktop viewports, and writes one PNG per variant × state ×
 * viewport. The gallery — not this script — owns the list of proposals: it
 * publishes `window.__designShots`, which is read back from the page, so adding
 * a variant needs no change here.
 *
 *   npm run design:shots
 *   npm run design:shots -- --only b-fleet --out /tmp/vis-ui --viewport phone
 *
 * Requires `spel` on PATH (sibling repo ~/spel). Prints the written paths, one
 * per line, so a caller can attach them straight away.
 */
import { spawn, execFile } from 'node:child_process';
import { mkdir, rm } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { dirname, join, resolve } from 'node:path';
import { promisify } from 'node:util';

const run = promisify(execFile);
const appDir = resolve(dirname(fileURLToPath(import.meta.url)), '..');

const VIEWPORTS = {
  // iPhone 15/16 logical size: the width every phone rule in the app is written for.
  phone: { width: 390, height: 844 },
  // iPad Pro 11" portrait: past `sm:`, but still driven by a finger. The third
  // shot is not optional: the tablet is the only place a desktop-density control
  // (or a phone layout stretched to 834px) shows itself.
  tablet: { width: 834, height: 1194 },
  // Past the `sm:` breakpoint, where the panel detaches and the tab bar moves up.
  desktop: { width: 1280, height: 900 },
};

/** `1728x3720` -> a viewport, so a proposal can name a size the presets do not have. */
function parseViewport(spec) {
  const match = /^(\d+)x(\d+)$/.exec(spec);
  if (!match) {
    throw new Error(`unknown viewport: ${spec} (have ${Object.keys(VIEWPORTS)}, or WIDTHxHEIGHT)`);
  }
  return { width: Number(match[1]), height: Number(match[2]) };
}

/**
 * A proposal that only exists at one size says so in the registry, and that wins
 * over the sweep: a comparison board shot at 390px is a column of clipped cards.
 */
const viewportsOf = (shot, names) =>
  shot.viewport
    ? [[shot.viewport, parseViewport(shot.viewport)]]
    : names.map((name) => [name, VIEWPORTS[name] ?? parseViewport(name)]);

function parseArgs(argv) {
  const args = {
    out: '/tmp/vis-ui',
    only: null,
    viewports: ['phone', 'tablet', 'desktop'],
    themes: ['light', 'dark'],
    keep: false,
  };
  for (let i = 0; i < argv.length; i += 1) {
    const flag = argv[i];
    if (flag === '--out') args.out = argv[(i += 1)];
    else if (flag === '--only') args.only = argv[(i += 1)];
    else if (flag === '--viewport') args.viewports = argv[(i += 1)].split(',');
    else if (flag === '--theme') args.themes = argv[(i += 1)].split(',');
    else if (flag === '--keep') args.keep = true;
    else throw new Error(`unknown flag: ${flag}`);
  }
  for (const name of args.viewports) {
    if (!VIEWPORTS[name]) parseViewport(name);
  }
  for (const theme of args.themes) {
    if (theme !== 'light' && theme !== 'dark') throw new Error(`unknown theme: ${theme}`);
  }
  return args;
}

/** Starts vite and resolves with its local URL plus a stop handle. */
function startDevServer() {
  return new Promise((resolvePromise, reject) => {
    const child = spawn(join(appDir, 'node_modules/.bin/vite'), ['--host', '127.0.0.1'], {
      cwd: appDir,
      stdio: ['ignore', 'pipe', 'pipe'],
    });
    const stop = () => {
      child.kill('SIGTERM');
    };
    let settled = false;
    const fail = (error) => {
      if (settled) return;
      settled = true;
      stop();
      reject(error);
    };
    child.stdout.setEncoding('utf8');
    child.stdout.on('data', (chunk) => {
      const match = /(http:\/\/127\.0\.0\.1:\d+)\//.exec(chunk);
      if (match && !settled) {
        settled = true;
        resolvePromise({ url: match[1], stop });
      }
    });
    child.stderr.setEncoding('utf8');
    child.stderr.on('data', (chunk) => process.stderr.write(`[vite] ${chunk}`));
    child.on('exit', (code) => fail(new Error(`vite exited early (${code})`)));
    setTimeout(() => fail(new Error('vite did not report a local URL within 60s')), 60_000);
  });
}

const spel = (session, ...argv) => run('spel', ['--session', session, ...argv], { cwd: appDir });

/**
 * The gallery is the single source of truth for what exists to photograph, so
 * the matrix is read out of the page rather than restated here.
 */
async function readShotMatrix(session, baseUrl) {
  await spel(session, 'open', `${baseUrl}/#/__design`);
  await spel(session, 'wait', '--fn', 'window.__designReady');
  const { stdout } = await spel(session, 'eval-js', 'JSON.stringify(window.__designShots)');
  const json = /\[.*\]/s.exec(stdout);
  if (!json) throw new Error(`gallery did not publish window.__designShots: ${stdout}`);
  return JSON.parse(json[0]);
}

async function main() {
  const args = parseArgs(process.argv.slice(2));
  const session = `design-shots-${process.pid}`;
  await rm(args.out, { recursive: true, force: true });
  await mkdir(args.out, { recursive: true });

  const server = await startDevServer();
  const written = [];
  try {
    const shots = (await readShotMatrix(session, server.url)).filter(
      (shot) => !args.only || args.only.split(',').includes(shot.id),
    );
    if (shots.length === 0) throw new Error(`no proposals matched --only ${args.only}`);
    for (const shot of shots) {
      for (const theme of args.themes) {
        for (const [name, { width, height }] of viewportsOf(shot, args.viewports)) {
          const path = join(args.out, `${shot.id}-${shot.state}-${theme}-${name}.png`);
          const url = `${server.url}/#/__design?v=${shot.id}&state=${shot.state}&theme=${theme}`;
          // One navigation per shot: the gallery reads the hash at mount, and the
          // ready flag is what makes the capture deterministic instead of timed.
          await spel(session, 'open', '--viewport', `${width}x${height}`, url);
          await spel(session, 'wait', '--fn', 'window.__designReady');
          await spel(session, 'screenshot', path);
          written.push(path);
          console.log(path);
        }
      }
    }
  } finally {
    await spel(session, 'close').catch(() => {});
    if (!args.keep) server.stop();
  }
  if (args.keep) console.error(`dev server left running at ${server.url}`);
  return written;
}

main().catch((error) => {
  console.error(error.message ?? error);
  process.exit(1);
});
