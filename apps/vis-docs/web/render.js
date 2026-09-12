/** HTML shared by the Worker and browser. Every metadata value is escaped. */
import { catalogMetadata, extensionIcon, extensionName, extensionPath } from './discovery.js';
import { escapeHTML } from './html.js';
import { readmeHTML } from './readme.js';
import { communityHTML } from './community.js';
export const categories = {
  all: 'All extensions',
  tools: 'Tools',
  providers: 'Providers',
  workflows: 'Workflows',
};
export const sorts = {
  stars: 'Most stars',
  updated: 'Recently updated',
  newest: 'Recently added',
  name: 'Name: A–Z',
};
export { escapeHTML } from './html.js';
export function safeURL(value, githubOnly = false) {
  try {
    const url = new URL(value);
    if (
      url.protocol === 'https:' &&
      !url.username &&
      !url.password &&
      (!githubOnly || (url.hostname === 'github.com' && !url.port))
    )
      return url.href;
  } catch {
    /* Metadata is not trusted markup. */
  }
  return '';
}
const link = (text, url, className = '', id = '') =>
  `<a${safeURL(url, true) ? ` href="${escapeHTML(safeURL(url, true))}" target="_blank" rel="noopener noreferrer"` : ''} class="${className}"${id ? ` id="${id}"` : ''}>${escapeHTML(text)}</a>`;
const quote = (text) => "'" + text.replaceAll("'", "'\\''") + "'";
export function installCommand(item) {
  return `vis-agent extension install ${quote(item.repository_url)}${item.subdirectory ? ' --subdirectory ' + quote(item.subdirectory) : ''} --version ${quote(item.version)} --trust`;
}
const dateLabel = (value) =>
  Number.isNaN(Date.parse(value))
    ? 'Date unavailable'
    : new Intl.DateTimeFormat('en', {
        month: 'short',
        day: 'numeric',
        year: 'numeric',
        timeZone: 'UTC',
      }).format(new Date(value));
export function filters(search = '') {
  const query = new URLSearchParams(search);
  return {
    q: query.get('q') || '',
    category: Object.hasOwn(categories, query.get('category')) ? query.get('category') : 'all',
    sort: Object.hasOwn(sorts, query.get('sort')) ? query.get('sort') : 'stars',
  };
}
export function filterURL(state) {
  const query = new URLSearchParams();
  for (const [key, value] of Object.entries(state))
    if (value && !({ category: 'all', sort: 'stars' }[key] === value)) query.set(key, value);
  return '/extensions/' + (query.size ? '?' + query : '');
}
export function visibleItems(items, state) {
  const term = state.q.toLowerCase().trim();
  return items
    .filter(
      (p) =>
        (state.category === 'all' || p.category === state.category) &&
        [p.name, p.description, p.owner, p.repository, ...p.dependencies, ...p.topics]
          .join(' ')
          .toLowerCase()
          .includes(term),
    )
    .sort(
      (a, b) =>
        (state.sort === 'stars'
          ? b.stars - a.stars
          : state.sort === 'name'
            ? 0
            : Date.parse(b[state.sort === 'newest' ? 'added_at' : 'updated_at']) -
              Date.parse(a[state.sort === 'newest' ? 'added_at' : 'updated_at'])) ||
        extensionName(a).localeCompare(extensionName(b)) ||
        a.id.localeCompare(b.id),
    );
}
export function categoriesHTML(items, state) {
  return Object.entries(categories)
    .map(
      ([value, label]) =>
        `<a data-category="${value}" href="${escapeHTML(filterURL({ ...state, category: value }))}"${value === state.category ? ' class="active" aria-current="page"' : ''}><span>${label}</span><span class="count">${items.filter((p) => value === 'all' || p.category === value).length}</span></a>`,
    )
    .join('');
}
export function cardsHTML(items, state) {
  const visible = visibleItems(items, state);
  if (!visible.length)
    return `<div class="empty"><h2>${items.length ? 'No matching extensions' : 'No repositories yet'}</h2><p>${items.length ? 'Try a different search or browse all categories.' : 'Use “Add a repository” above to submit a public GitHub project for moderation.'}</p>${items.length ? '<a id="clear-filters" href="/extensions/">Clear filters</a>' : ''}</div>`;
  return visible
    .map(
      (item) =>
        `<article class="extension-card" data-name="${escapeHTML(extensionName(item))}"><a class="card-main" href="${escapeHTML(extensionPath(item))}${filterURL(state).slice('/extensions/'.length)}"><div class="card-top"><span class="tag">${escapeHTML(categories[item.category])}</span><span class="version">v${escapeHTML(item.version)}</span></div><h3>${escapeHTML(extensionName(item))}</h3><p class="card-description">${escapeHTML(item.description)}</p></a>${link(item.subdirectory || item.repository, item.subdirectory ? item.source_url : item.repository_url, 'repository-link')}<div class="card-meta"><span>${escapeHTML(new Intl.NumberFormat('en', { notation: 'compact', maximumFractionDigits: 1 }).format(item.stars))} stars</span><span>Updated ${dateLabel(item.updated_at)}</span></div></article>`,
    )
    .join('');
}
const fact = (label, value) => `<dt>${label}</dt><dd>${escapeHTML(value)}</dd>`;
export function previewHTML(item) {
  return `<h3>${escapeHTML(extensionName(item))} · v${escapeHTML(item.version)}</h3><p>${escapeHTML(item.description)}</p><dl class="facts">${fact('GitHub owner', item.owner) + fact('Repository', item.repository) + fact('Python package', item.name) + fact('Project folder', item.subdirectory || 'Repository root') + fact('Category', categories[item.category]) + fact('Release', item.release_tag || item.version) + fact('Commit', item.revision)}</dl><h3>Repository checks passed</h3><ul class="repository-checks"><li>Public GitHub repository owner verified and commit pinned</li><li>Published GitHub Release matches the manifest version</li><li><code>pyproject.toml</code> and <code>extension.py</code> found</li><li>Required manifest fields and <code>vis-agent</code> dependency declared</li>${item.source_paths.length ? `<li>${item.source_paths.length} source directories found</li>` : ''}${item.skills?.length ? `<li>${item.skills.length} skill directories contain <code>SKILL.md</code></li>` : ''}</ul><p class="help">These are metadata and file checks, not a code audit. The SDK validates version requirements and runtime compatibility when installing.</p><div class="actions">${link('Review source on GitHub', item.source_url)}${link('pyproject.toml', item.manifest_url)}${item.release_url ? link('GitHub Release', item.release_url) : ''}</div><h3>Dependencies</h3><p class="help">Python ${escapeHTML(item.requires_python)}</p><ul class="dependencies">${item.dependencies.map((dep) => `<li>${escapeHTML(dep)}</li>`).join('')}</ul>`;
}
export function releaseHTML(item) {
  const releases = item.releases || [];
  if (!releases.length) return '<p class="help">No approved release history available.</p>';
  return `<form id="version-form" class="version-picker" method="get" action="${escapeHTML(extensionPath(item))}"><label for="release-version">Version</label><select id="release-version" name="version" aria-describedby="version-help">${releases.map((release) => `<option value="${escapeHTML(release.version)}"${release.version === item.version ? ' selected' : ''}>${escapeHTML(release.version)}${release.prerelease ? ' · Prerelease' : release.version === item.latest_version ? ' · Latest approved' : ''}</option>`).join('')}</select><button type="submit">View release</button></form><p id="version-help" class="help">Only approved releases are listed. Each version keeps its reviewed commit.${item.latest_version && item.version !== item.latest_version ? ' You are viewing a different version from the catalog default.' : ''}</p><details class="release-history"><summary>Approved releases (${releases.length})</summary><ol>${releases.map((release) => `<li><a data-release href="${escapeHTML(extensionPath(item))}?version=${encodeURIComponent(release.version)}">${escapeHTML(release.version)}</a>${release.prerelease ? ' <span class="tag">Prerelease</span>' : ''}<span class="help">${dateLabel(release.release_published_at || release.approved_at)}</span>${link(release.revision.slice(0, 12), release.repository_url + '/commit/' + release.revision)}${release.release_url ? link('Release notes', release.release_url) : ''}</li>`).join('')}</ol></details>`;
}
export function detailHTML(item) {
  return `<div class="detail-heading"><h1 tabindex="-1">${escapeHTML(extensionName(item))}</h1><p>${escapeHTML(item.description)}</p><span class="tag">${escapeHTML(categories[item.category])}</span>${link(item.repository, item.repository_url, 'repository-link')}</div>
    <section class="install-section"><h2 id="install">Install</h2>${releaseHTML(item)}<p>This command selects version ${escapeHTML(item.version)} at its approved commit. It never follows a moving tag or branch.</p><pre id="install-command"><code class="language-bash">${escapeHTML(installCommand(item))}</code></pre><button id="copy-command" type="button" class="primary" aria-live="polite">Copy install command</button><p class="help">Then start Vis or use /reload. Dependencies are prepared automatically.</p><p class="security-note">Review the source first. --trust allows extension code and build backends to run with your permissions.</p><details><summary>Already installed? Check, update or roll back</summary><pre><code class="language-bash">${escapeHTML(`vis-agent extension versions ${quote(item.name)}
vis-agent extension update ${quote(item.name)} --trust
vis-agent extension rollback ${quote(item.name)} --trust`)}</code></pre><p class="help">Add --project for a project installation. Update selects the latest approved stable version; rollback restores the previous pinned source. <a href="/extension-packages.html#check-for-updates-and-roll-back">Choose a specific version and read the lifecycle contract</a>.</p></details></section>
    <div class="project-details"><h2 id="project-details">Project details</h2><dl class="facts">${fact('Version', item.version) + fact('Python package', item.name) + fact('Project folder', item.subdirectory || 'Repository root') + fact('License', item.license && item.license !== 'NOASSERTION' ? item.license : 'Not specified') + fact('GitHub stars', item.stars) + fact('Updated', dateLabel(item.updated_at))}<dt>GitHub owner</dt><dd>${link(item.owner, 'https://github.com/' + item.owner, 'repository-link', 'github-owner')}</dd><dt>Reviewed commit</dt><dd>${link(item.revision.slice(0, 12), item.repository_url + '/commit/' + item.revision)}</dd>${item.checked_at ? fact('Metadata checked', dateLabel(item.checked_at)) : ''}${item.archived ? fact('Repository status', 'Archived on GitHub') : ''}</dl><div class="actions">${link('View source on GitHub', item.source_url, '', 'source-link')}${link('pyproject.toml', item.manifest_url)}${item.readme_url ? link('Read README', item.readme_url) : ''}</div><h2 id="dependencies">Dependencies</h2><p>Python ${escapeHTML(item.requires_python)}</p><ul class="dependencies">${item.dependencies.map((dep) => `<li>${escapeHTML(dep)}</li>`).join('')}</ul><div class="topics">${item.topics.map((topic) => `<span class="tag">${escapeHTML(topic)}</span>`).join('')}</div></div>
    <section class="package-readme" aria-labelledby="readme"><h2 id="readme">README</h2>${readmeHTML(item)}</section>${communityHTML()}`;
}
export function tocHTML(detail = false) {
  const entries = detail
    ? [
        ['install', 'Install'],
        ['project-details', 'Project details'],
        ['dependencies', 'Dependencies'],
        ['readme', 'README'],
        ['feedback', 'Community feedback'],
      ]
    : [
        ['explore', 'Explore extensions'],
        ['share', 'Share a repository'],
      ];
  return (
    '<div class="lbl">On this page</div>' +
    entries.map(([id, label]) => `<a href="#${id}">${label}</a>`).join('')
  );
}
export function shellHTML({
  items = [],
  item = null,
  search = '',
  error = '',
  detailError = false,
  siteKey = '',
} = {}) {
  const state = filters(search),
    detail = !!item || detailError;
  return `
    <a class="skip" href="#search">Skip to search</a>
    <input type="checkbox" id="navtoggle" class="navtoggle" aria-label="Toggle navigation" aria-controls="catalog-navigation" aria-expanded="false">
    <header class="top"><label for="navtoggle" class="hamburger" title="Menu"><span></span><span></span><span></span></label><a class="brand" href="/" title="Vis" aria-label="Vis">Vis</a><a class="center-link" href="/extensions/" aria-current="location" title="Extension Center" aria-label="Extension Center">${extensionIcon}</a><span class="spacer"></span><a class="gh" href="https://github.com/Blockether/vis" title="GitHub" aria-label="GitHub" target="_blank" rel="noopener noreferrer"><svg width="20" height="20" viewBox="0 0 16 16" fill="currentColor" aria-hidden="true"><path d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0 0 16 8c0-4.42-3.58-8-8-8z"/></svg></a></header>
    <label for="navtoggle" class="scrim" aria-hidden="true"></label>
    <div class="shell">
      <aside class="side" id="catalog-navigation" aria-label="Catalog navigation"><div class="tagline">A coding agent for your projects.</div><div class="nav-sec">Extensions</div><nav id="categories" class="nav" aria-label="Extension categories">${categoriesHTML(items, state)}</nav><nav class="nav" aria-label="Documentation"><div class="nav-sec">Documentation</div><a href="/">Getting started</a><a href="/extending.html">Writing extensions</a></nav></aside>
      <main class="main"><article class="content">
        <section id="catalog-page"${detail ? ' hidden' : ''}>
          <h1>Extension Center</h1><p>Find tools, providers and workflows for Vis. Browse public GitHub repositories and install a reviewed commit.</p>
          <div class="intro-actions"><button id="submit-open" class="primary" type="button">Add a repository</button></div>
          <h2 id="explore">Explore extensions</h2>
          <form class="toolbar" id="filters" method="get" action="/extensions/"><input type="hidden" name="category" value="${state.category}"><div class="search-field"><label for="search" class="sr-only">Search extensions</label><input id="search" name="q" value="${escapeHTML(state.q)}" type="search" autocomplete="off" placeholder="Search extensions, owners, topics…"><kbd aria-hidden="true">/</kbd></div><label class="sort-field"><span class="sr-only">Sort extensions</span><select id="sort" name="sort" aria-label="Sort extensions">${Object.entries(
            sorts,
          )
            .map(
              ([value, label]) =>
                `<option value="${value}"${state.sort === value ? ' selected' : ''}>${label}</option>`,
            )
            .join(
              '',
            )}</select></label><noscript><button type="submit">Apply filters</button></noscript></form>
          <p id="catalog-status" role="status">${error ? escapeHTML(error) + ' <a id="retry" href="/extensions/">Retry</a>' : visibleItems(items, state).length + ' extensions'}</p><section id="results" aria-label="Extensions">${error ? '' : cardsHTML(items, state)}</section>
          <section id="share"><h2>Share a repository</h2><p>Keep your code on GitHub. Publish a GitHub Release, choose the folder containing <code>pyproject.toml</code> and <code>extension.py</code>, and submit the link once. New releases are discovered for moderation; no PyPI publication is required.</p><p><a href="/extension-packages.html#publish-and-maintain-releases">Read the release workflow</a>.</p></section>
        </section>
        <section id="detail-page"${detail ? '' : ' hidden'}><a id="back-to-catalog" href="${escapeHTML(filterURL(state))}" class="back-button">Back to extensions</a><div id="detail" aria-live="polite">${item ? detailHTML(item) : detailError ? '<h1>Extension unavailable</h1><p>Only approved repositories appear here. Return to the catalog or try again later.</p>' : ''}</div></section>
        <p id="notice" role="status" class="notice"></p>
        <footer class="foot"><a class="bk" href="https://blockether.com" title="Blockether"><img class="bk-mark" src="/assets/blockether.png" alt="Blockether"></a><span class="spacer"></span><span>Review community code before installing.</span></footer>
      </article></main>
      <aside class="toc" aria-label="On this page">${tocHTML(detail)}</aside>
    </div>
    <dialog id="submit-dialog" class="content" aria-labelledby="submit-title">
      <div class="dialog-head"><div><p class="help" id="submit-step" tabindex="-1">1 of 2 · Repository</p><h2 id="submit-title">Add a repository</h2></div><button id="submit-close" type="button" class="dialog-close" aria-label="Close repository dialog" title="Close"><svg xmlns="http://www.w3.org/2000/svg" width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M18 6 6 18"/><path d="m6 6 12 12"/></svg></button></div>
      <div class="dialog-body"><p>Share a public GitHub project with a published Release. We check its files and metadata before moderation. New releases are discovered after your repository is approved.</p>
        <form id="repository-form"><label for="repository-url">GitHub repository<input id="repository-url" maxlength="240" name="repository_url" type="url" placeholder="https://github.com/owner/repository" autocomplete="url" autocapitalize="none" spellcheck="false" required></label><label for="project-folder">Project folder <span class="optional">Optional</span><input id="project-folder" maxlength="512" name="subdirectory" placeholder="Repository root" autocomplete="off" autocapitalize="none" spellcheck="false" aria-describedby="folder-help"></label><p class="help" id="folder-help">Folder containing <code>pyproject.toml</code> and <code>extension.py</code>. Leave empty if they are at the repository root.</p><label for="release-tag">Release tag <span class="optional">Optional</span><input id="release-tag" name="release_tag" maxlength="200" placeholder="Latest stable GitHub Release" autocomplete="off" autocapitalize="none" spellcheck="false" aria-describedby="release-help"></label><p class="help" id="release-help">Use <code>v1.2.0</code> or a monorepo tag such as <code>vis-greeting/v1.2.0</code>. Its version must match the manifest. A Git tag alone is not a published Release.</p><button id="review-submit" class="primary" type="submit">Review repository</button></form>
        <div id="preview" aria-live="polite"></div><div id="turnstile-widget" data-sitekey="${escapeHTML(siteKey)}"></div><p id="submit-status" role="status"></p><div class="dialog-actions"><button id="edit-submission" type="button" hidden>Edit repository</button><button id="submit-confirm" class="primary" type="button" hidden>Submit for review</button></div><p class="help trust-note">Submissions stay private until approved. The owner is read from GitHub. A listing does not verify the submitter or audit the code.</p>
      </div>
    </dialog>`;
}
export function renderPage(data) {
  const initial = JSON.stringify(data).replace(
    /[<>&\u2028\u2029]/g,
    (c) => '\\u' + c.charCodeAt(0).toString(16).padStart(4, '0'),
  );
  return `<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1, viewport-fit=cover"><meta name="color-scheme" content="light">${catalogMetadata(data)}<link rel="preload" href="/assets/fonts/jetbrains-mono.woff2" as="font" type="font/woff2" crossorigin><link rel="stylesheet" href="/assets/theme.css"><link rel="stylesheet" href="/assets/catalog.css"></head><body><div id="app">${shellHTML(data)}</div><script id="catalog-data" type="application/json">${initial}</script><script type="module" src="/assets/app.js"></script></body></html>`;
}
