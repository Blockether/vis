import { expect, test } from 'vitest';
import { execFileSync } from 'node:child_process';
import { readFileSync, readdirSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { JSDOM } from 'jsdom';

const directory = 'dist/python-sdk-api/';

function page(name) {
  return new JSDOM(readFileSync(directory + name, 'utf8'));
}

test('the SDK overview starts with a usable workflow and an API choice guide', () => {
  const dom = page('blockether/vis.html');
  try {
    const document = dom.window.document;
    expect(document.querySelector('main h1').textContent).toBe('Python SDK');
    expect([...document.querySelectorAll('main h2')].map((node) => node.textContent)).toEqual(
      expect.arrayContaining(['First request', 'Choose an API', 'Runtime and versions']),
    );
    expect(document.querySelector('main pre').textContent).toContain('with Agent(');
    expect(document.querySelector('main table').textContent).toContain('Agent');
    expect(document.querySelector('main').textContent).toContain('model calls');
  } finally {
    dom.window.close();
  }
});

test('every module offers task navigation, guide links and a compact symbol tree', () => {
  for (const name of readdirSync(directory, { recursive: true }).filter(
    (name) => name.endsWith('.html') && name !== 'index.html',
  )) {
    const dom = page(name);
    try {
      const document = dom.window.document;
      const navigation = document.querySelector('nav[aria-label="Python SDK"]');
      expect(navigation, name).not.toBeNull();
      const topics = [...navigation.querySelectorAll('.api-modules a')];
      expect(topics.map((link) => link.textContent.trim())).toEqual([
        'Overview',
        'Agents and sessions',
        'Extensions',
        'Activities',
        'Views',
        'Package validation',
      ]);
      expect(topics.filter((link) => link.getAttribute('aria-current') === 'page')).toHaveLength(1);
      expect(navigation.querySelector('a[href="/python-sdk.html"]')).not.toBeNull();
      expect(navigation.querySelector('a[href="/extending.html"]')).not.toBeNull();
      expect(navigation.querySelector('summary').textContent).toContain('Browse Python SDK');
      expect(document.querySelector('.api-skip').getAttribute('href')).toBe('#api-title');
      for (const details of navigation.querySelectorAll('.api-symbols details'))
        expect(details.open).toBe(false);
    } finally {
      dom.window.close();
    }
  }
});

test('public engine methods remain documented when their implementation base is private', () => {
  const methods = JSON.parse(
    execFileSync(
      process.env.PYTHON || 'python3',
      [
        '-c',
        `
import inspect, json
import blockether.vis.engine as engine
methods = []
for name in engine.__all__:
    cls = getattr(engine, name)
    if inspect.isclass(cls):
        for method, value in inspect.getmembers(cls):
            if (not method.startswith('_')
                    and (inspect.isfunction(value) or inspect.ismethod(value))
                    and value.__module__.startswith('blockether.vis')):
                methods.append(name + '.' + method)
print(json.dumps(methods))
`,
      ],
      {
        env: {
          ...process.env,
          PYTHONPATH: fileURLToPath(new URL('../../packages/vis-agent/src/', import.meta.url)),
        },
        encoding: 'utf8',
      },
    ),
  );
  const dom = page('blockether/vis/engine.html');
  try {
    const document = dom.window.document;
    expect(methods).toContain('Events.close');
    expect(methods).toContain('GatewayClient.create_session');
    for (const name of methods) expect(document.getElementById(name), name).not.toBeNull();
    for (const name of [
      'Events.close',
      'GatewayClient.create_session',
      'LocalEngine.create_session',
    ])
      expect(document.getElementById(name).querySelector('.docstring'), name).not.toBeNull();
  } finally {
    dom.window.close();
  }
});
