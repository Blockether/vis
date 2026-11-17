import { expect, test } from 'vitest';
import { findings } from './audit.mjs';

const allowed = { 'GHSA-fc5q-39xc-vjpf': 'no fixed release' };

function report(id, severity, node = 'node_modules/picomatch') {
  const via = { url: `https://github.com/advisories/${id}`, severity, title: 'ReDoS' };
  return { vulnerabilities: { picomatch: { via: [via], nodes: [node] } } };
}

const lock = {
  packages: {
    'node_modules/picomatch': { dev: true },
    'node_modules/marked': {},
  },
};

test('an allowed advisory in a development package passes', () => {
  expect(findings(report('GHSA-fc5q-39xc-vjpf', 'high'), lock, allowed)).toEqual([]);
});

test('a high advisory that is not allowed fails', () => {
  const audit = report('GHSA-fc5q-39xc-vjpf', 'high');
  audit.vulnerabilities.marked = report('GHSA-aaaa-bbbb-cccc', 'critical').vulnerabilities.picomatch;
  expect(findings(audit, lock, allowed)).toEqual([expect.stringContaining('GHSA-aaaa-bbbb-cccc')]);
});

test('an allowed advisory in a runtime package fails', () => {
  const problems = findings(report('GHSA-fc5q-39xc-vjpf', 'high', 'node_modules/marked'), lock, allowed);
  expect(problems).toEqual([expect.stringContaining('only in development packages')]);
});

test('an allowed advisory that no longer occurs fails, so the exception is removed', () => {
  expect(findings({ vulnerabilities: {} }, lock, allowed)).toEqual([
    expect.stringContaining('no longer occurs'),
  ]);
});

test('a failed audit request fails', () => {
  expect(findings({ error: { code: 'ENOTFOUND' } }, lock, allowed)).toEqual([
    expect.stringContaining('ENOTFOUND'),
  ]);
});
