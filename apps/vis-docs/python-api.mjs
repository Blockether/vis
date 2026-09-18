import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { mkdir, readdir, readFile, writeFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { JSDOM } from 'jsdom';
import { metadataHead } from './web/discovery.js';

const repoRoot = fileURLToPath(new URL('../../', import.meta.url));
// pdoc is pinned by the SDK's `docs` extra, so uv installs the same generator this
// repository already documents, instead of a second pin kept here.
const sdkDocsRequirement =
  fileURLToPath(new URL('../../packages/vis-agent', import.meta.url)) + '[docs]';

const canRun = (command, args) => {
  try {
    execFileSync(command, args, { cwd: repoRoot, stdio: 'ignore' });
    return true;
  } catch {
    return false;
  }
};

/** Run pdoc with the interpreter that has it, or borrow a disposable uv environment. */
export function pdocInterpreter(python, hasPdoc, hasUv) {
  if (hasPdoc) return [python];
  if (hasUv) return ['uv', 'run', '--no-project', '--with', sdkDocsRequirement, 'python'];
  throw new Error(
    `${python} cannot import pdoc. Install the pinned generator with ` +
      `python -m pip install './packages/vis-agent[docs]', point PYTHON at an interpreter ` +
      'that has it, or install uv so the build can borrow a disposable environment.',
  );
}

/** The command that generates the SDK reference: interpreter plus any prefix arguments. */
export function sdkPython() {
  const python = process.env.PYTHON || 'python3';
  const hasPdoc = canRun(python, ['-c', 'import pdoc']);
  return pdocInterpreter(python, hasPdoc, canRun('uv', ['--version']));
}

/** Generate the public SDK reference from this checkout, without starting Vis. */
export async function buildPythonApi(dist) {
  const output = new URL('python-sdk-api/', dist);
  const [command, ...pythonArgs] = sdkPython();
  execFileSync(
    command,
    [
      ...pythonArgs,
      '-m',
      'pdoc',
      'blockether.vis',
      '!blockether\\.vis\\._',
      '--output-directory',
      fileURLToPath(output),
      '--docformat',
      'google',
      '--no-show-source',
      '--template-directory',
      fileURLToPath(new URL('pdoc/', import.meta.url)),
      '--footer-text',
      'Vis Python SDK · development API',
      '--favicon',
      '/favicon.ico',
    ],
    {
      cwd: repoRoot,
      env: {
        ...process.env,
        PYTHONPATH: fileURLToPath(new URL('../../packages/vis-agent/src/', import.meta.url)),
      },
      stdio: 'inherit',
    },
  );
  await mkdir(new URL('assets/', output), { recursive: true });
  const pages = [];
  for (const name of (await readdir(output, { recursive: true })).sort()) {
    // pdoc's index is a redirect to the package page, not a second canonical page.
    if (!name.endsWith('.html') || name === 'index.html') continue;
    const file = new URL(name, output);
    const dom = new JSDOM(await readFile(file, 'utf8'));
    try {
      const document = dom.window.document;
      // Keep pdoc's layout and search, but satisfy the site's strict self-only CSP.
      for (const node of document.querySelectorAll('style, script:not([src])')) {
        const css = node.tagName === 'STYLE';
        const content = node.textContent;
        const hash = createHash('sha256').update(content).digest('hex').slice(0, 16);
        const asset = `assets/${hash}.${css ? 'css' : 'js'}`;
        await writeFile(new URL(asset, output), content);
        if (css) {
          const link = document.createElement('link');
          link.rel = 'stylesheet';
          link.href = '/python-sdk-api/' + asset;
          if (node.media) link.media = node.media;
          node.replaceWith(link);
        } else {
          node.textContent = '';
          node.src = '/python-sdk-api/' + asset;
        }
      }
      // Each API owns its prose anchors; repeated headings such as "Arguments" must not collide.
      for (const docstring of document.querySelectorAll('main .docstring')) {
        const owner = docstring.closest('section[id], .classattr[id]');
        if (!owner) continue;
        for (const node of docstring.querySelectorAll('[id]')) {
          const fragment = '#' + node.id;
          node.id = owner.id + '--' + node.id;
          for (const link of docstring.querySelectorAll('a[href]')) {
            if (link.getAttribute('href') === fragment) link.setAttribute('href', '#' + node.id);
          }
        }
      }
      // A signature may name a private base even when its public methods are rendered.
      // Keep the type name without linking to an intentionally hidden implementation.
      for (const link of document.querySelectorAll('a[href^="#_"]')) {
        if (!document.getElementById(link.getAttribute('href').slice(1)))
          link.replaceWith(...link.childNodes);
      }
      const title = document.title.replace(/ API documentation$/, '');
      const path = '/python-sdk-api/' + name;
      document.querySelector('title').remove();
      document.querySelector('link[rel="icon"]').remove();
      document.head.insertAdjacentHTML(
        'beforeend',
        metadataHead({
          title: title + ' · Python SDK API · Vis · Blockether',
          description: `Public Python SDK reference for ${title}: classes, methods, signatures and type annotations generated from the development source.`,
          path,
          type: 'TechArticle',
        }),
      );
      await writeFile(file, dom.serialize());
      pages.push({ title, path });
    } finally {
      dom.window.close();
    }
  }
  return pages;
}
