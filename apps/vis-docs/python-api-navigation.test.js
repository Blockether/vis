import { expect, test } from 'vitest';
import { execFileSync } from 'node:child_process';
import { readFileSync, readdirSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { JSDOM } from 'jsdom';

const directory = 'dist/python-sdk-api/';
const modules = readdirSync(directory, { recursive: true })
  .filter((name) => name.endsWith('.html') && name !== 'index.html')
  .sort();
const moduleNames = modules.map((name) => name.replace(/\.html$/, '').replaceAll('/', '.'));

function page(name, options = {}) {
  return new JSDOM(readFileSync(directory + name, 'utf8'), {
    url: 'https://vis.blockether.com/python-sdk-api/' + name,
    ...options,
  });
}

function loadStyles(document) {
  for (const link of document.querySelectorAll('link[rel="stylesheet"]')) {
    const style = document.createElement('style');
    style.textContent = readFileSync('dist' + new URL(link.href).pathname, 'utf8');
    document.head.append(style);
  }
}

test.each(modules)('%s is an API reference, not a second user guide', (name) => {
  const dom = page(name);
  try {
    const document = dom.window.document;
    const intro = document.querySelector('.module-info');
    expect(document.querySelector('main h1').textContent).toBe(
      name.replace(/\.html$/, '').replaceAll('/', '.'),
    );
    expect(intro.querySelectorAll('h2, h3, h4, pre, table, ol, ul')).toHaveLength(0);
    expect(intro.querySelector('a[href^="https://vis.blockether.com/"]')).not.toBeNull();
    expect([...document.querySelectorAll('nav h2')].map((node) => node.textContent)).not.toContain(
      'Contents',
    );
  } finally {
    dom.window.close();
  }
});

test('every module offers generated module navigation and a compact symbol tree', () => {
  for (const name of modules) {
    const dom = page(name);
    try {
      const document = dom.window.document;
      const navigation = document.querySelector('nav[aria-label="Python SDK"]');
      expect(navigation, name).not.toBeNull();
      const links = [...navigation.querySelectorAll('.api-modules a')];
      expect(links.map((link) => link.textContent.trim())).toEqual(moduleNames);
      expect(links.map((link) => link.getAttribute('href'))).toEqual(
        modules.map((file) => '/python-sdk-api/' + file),
      );
      expect(links.filter((link) => link.getAttribute('aria-current') === 'page')).toHaveLength(1);
      expect(navigation.querySelector('a[href="/"]')).not.toBeNull();
      expect(navigation.querySelector('.api-guides')).toBeNull();
      expect(navigation.querySelector('summary').textContent).toContain('Browse Python SDK');
      expect(document.querySelector('.api-skip').getAttribute('href')).toBe('#api-title');
      for (const details of navigation.querySelectorAll('.api-symbols details'))
        expect(details.open).toBe(false);
    } finally {
      dom.window.close();
    }
  }
});

test.each([false, true])('symbol labels stay beside native chevrons (touch: %s)', (touch) => {
  const dom = page('blockether/vis/activity.html');
  try {
    const document = dom.window.document;
    loadStyles(document);
    if (touch) {
      // JSDOM does not evaluate pointer media features.
      const style = document.createElement('style');
      style.textContent = [...document.styleSheets]
        .flatMap((sheet) => [...sheet.cssRules])
        .filter((rule) => rule.conditionText?.replace(/\s/g, '') === '(pointer:coarse)')
        .flatMap((rule) => [...rule.cssRules])
        .map((rule) => rule.cssText)
        .join('\n');
      document.head.append(style);
    }
    for (const link of document.querySelectorAll('.api-symbols summary > a')) {
      expect(dom.window.getComputedStyle(link).display).toBe('inline-block');
      expect(dom.window.getComputedStyle(link).minHeight).toBe(touch ? '44px' : '28px');
      const summaryStyle = dom.window.getComputedStyle(link.parentElement);
      expect(parseFloat(summaryStyle.paddingTop) || 0).toBe(0);
      expect(parseFloat(summaryStyle.paddingBottom) || 0).toBe(0);
    }
  } finally {
    dom.window.close();
  }
});

test('inherited public methods link to one definition instead of repeating it', () => {
  const dom = page('blockether/vis/engine.html');
  try {
    const document = dom.window.document;
    const description = 'Create a conversation and return a Session handle.';
    expect(document.getElementById('ExecutionLayer.create_session').textContent).toContain(
      description,
    );
    for (const name of ['GatewayClient', 'LocalEngine']) {
      const member = document.getElementById(name + '.create_session');
      expect(member.querySelector('a[href="#ExecutionLayer.create_session"]')).not.toBeNull();
      expect(member.textContent).not.toContain(description);
      expect(member.querySelector('.attr')).toBeNull();
    }
    for (const name of ['Events', 'JobEvents']) {
      expect(
        document.getElementById(name + '.close').querySelector('.docstring').textContent,
      ).toContain('Release this event subscription');
    }
  } finally {
    dom.window.close();
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

function interactivePage(fragment = '', compact = false) {
  const dom = page('blockether/vis/engine.html', {
    url: 'https://vis.blockether.com/python-sdk-api/blockether/vis/engine.html' + fragment,
    runScripts: 'outside-only',
  });
  const media = new dom.window.EventTarget();
  media.matches = compact;
  dom.window.matchMedia = () => media;
  dom.window.eval(readFileSync('pdoc/navigation.js', 'utf8'));
  return { dom, media };
}

test('compact navigation closes after choosing a link and reopens on a wide viewport', () => {
  const { dom, media } = interactivePage('', true);
  try {
    const navigation = dom.window.document.querySelector('.api-navigation');
    expect(navigation.open).toBe(false);
    navigation.open = true;
    navigation.querySelector('a[href="#Agent"]').click();
    expect(navigation.open).toBe(false);
    media.matches = false;
    media.dispatchEvent(new dom.window.Event('change'));
    expect(navigation.open).toBe(true);
    navigation.querySelector('a[href="#Agent"]').click();
    expect(navigation.open).toBe(true);
  } finally {
    dom.window.close();
  }
});

test('deep links reveal inherited documentation on load and hash changes', () => {
  const { dom } = interactivePage('#GatewayClient.create_session', true);
  try {
    const document = dom.window.document;
    const errors = [];
    dom.window.addEventListener('error', (event) => errors.push(event.message));
    const target = document.getElementById('GatewayClient.create_session');
    expect(target.closest('.api-inherited').open).toBe(true);
    expect(document.getElementById('Events.close').closest('.api-inherited').open).toBe(false);
    dom.window.history.replaceState(null, '', '#Events.close');
    dom.window.dispatchEvent(new dom.window.HashChangeEvent('hashchange'));
    expect(document.getElementById('Events.close').closest('.api-inherited').open).toBe(true);
    for (const hash of ['#missing-member', '#%invalid']) {
      dom.window.history.replaceState(null, '', hash);
      expect(() =>
        dom.window.dispatchEvent(new dom.window.HashChangeEvent('hashchange')),
      ).not.toThrow();
    }
    expect(errors).toEqual([]);
  } finally {
    dom.window.close();
  }
});

test('view decoding inherited from private SDK bases remains available on each record', () => {
  const dom = page('blockether/vis/views.html');
  try {
    for (const name of ['InputView', 'LiveView', 'LivePatch', 'LiveResult', 'ViewSnapshot']) {
      for (const method of ['from_wire', 'to_wire']) {
        const member = dom.window.document.getElementById(name + '.' + method);
        expect(member.querySelector('.docstring')).not.toBeNull();
      }
    }
  } finally {
    dom.window.close();
  }
});

test('SDK search has room around its text and readable placeholder and focus states', () => {
  const dom = page('blockether/vis/activity.html');
  try {
    const document = dom.window.document;
    loadStyles(document);
    const input = document.querySelector('input[type="search"]');
    const style = dom.window.getComputedStyle(input);
    expect(input.getAttribute('aria-label')).toBe('Search Python SDK');
    expect(style.paddingLeft).toBe('0.75rem');
    expect(style.paddingRight).toBe('0.75rem');
    expect(style.paddingTop).toBe('0.625rem');
    expect(style.paddingBottom).toBe('0.625rem');
    expect(style.minHeight).toBe('2.75rem');
    const rules = [...document.styleSheets].flatMap((sheet) => [...sheet.cssRules]);
    const searchStyle = (suffix = '') =>
      rules.findLast(
        (rule) =>
          rule.selectorText?.replace(/['"]/g, '') === 'nav.pdoc input[type=search]' + suffix,
      ).style;
    expect(searchStyle().border).toBe('1px solid var(--faint)');
    expect(searchStyle('::placeholder').color).toBe('var(--dim)');
    expect(searchStyle('::placeholder').opacity).toBe('1');
    expect(searchStyle(':hover').borderColor).toBe('var(--dim)');
    expect(searchStyle(':focus').borderColor).toBe('var(--primary)');
  } finally {
    dom.window.close();
  }
});

test('module names wrap within reference headings and navigation', () => {
  const dom = page('blockether/vis/extension_package.html');
  try {
    const document = dom.window.document;
    loadStyles(document);
    for (const node of document.querySelectorAll('main h1, .api-modules a'))
      expect(dom.window.getComputedStyle(node).overflowWrap).toBe('anywhere');
  } finally {
    dom.window.close();
  }
});
