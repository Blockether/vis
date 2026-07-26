#!/usr/bin/env node
// React Compiler as the app's ONLY React linter.
//
// `npm run lint` runs the compiler's full static analysis over every file under
// `src/` — including files the bundle graph never reaches, which a build-time
// pass would silently skip — and fails on any real Rules-of-React diagnostic
// (invalid JS/React, unpreservable manual memoization, bad config).
//
// `Todo`/`Hint` diagnostics are NOT failures: they mean the compiler cannot lower
// syntax yet (e.g. `try/finally`) and simply skips optimizing that function.
import { transformAsync } from '@babel/core';
import { readFile } from 'node:fs/promises';
import { glob } from 'node:fs/promises';
import path from 'node:path';
import process from 'node:process';

const ROOT = path.resolve(import.meta.dirname, '..');
const IGNORED_SEVERITIES = new Set(['Todo', 'Hint']);

/** Diagnostics collected across every file, in discovery order. */
const failures = [];

/** Compiler logger: keep every non-Todo/Hint CompileError as a lint failure. */
const logger = {
  logEvent(filename, event) {
    if (event?.kind !== 'CompileError') return;
    const detail = event.detail;
    const severity = detail?.severity ?? detail?.reason ?? 'Error';
    if (IGNORED_SEVERITIES.has(severity)) return;
    failures.push({
      file: path.relative(ROOT, filename ?? '<unknown>'),
      severity,
      message: detail?.toString?.() ?? String(detail),
    });
  },
};

async function lintFile(file) {
  const source = await readFile(file, 'utf8');
  try {
    await transformAsync(source, {
      filename: file,
      babelrc: false,
      configFile: false,
      parserOpts: { plugins: ['typescript', 'jsx'], sourceType: 'module' },
      plugins: [['babel-plugin-react-compiler', { target: '19', logger }]],
    });
  } catch (cause) {
    failures.push({ file: path.relative(ROOT, file), severity: 'Fatal', message: String(cause?.message ?? cause) });
  }
}

const files = [];
for await (const entry of glob('src/**/*.{ts,tsx}', { cwd: ROOT })) {
  files.push(path.join(ROOT, entry));
}
files.sort();
await Promise.all(files.map(lintFile));

if (failures.length === 0) {
  console.log(`react-compiler: ${files.length} files clean`);
  process.exit(0);
}

for (const failure of failures) {
  console.error(`\n${failure.file} [${failure.severity}]\n${failure.message}`);
}
console.error(`\nreact-compiler: ${failures.length} problem(s) in ${files.length} files`);
process.exit(1);
