let sequence = 0;

/**
 * Manual search: the header box loads the renderer's build-time index once and
 * lists the sections that answer the query, linking straight to their anchors.
 * The box and its `data-index` URL come from the page; without JavaScript the
 * input stays a plain, harmless control. Dispose drops listeners and results.
 */
export function mountSearch(container) {
  const stopped = [...container.querySelectorAll('.search[data-index]')].map((box) =>
    enhance(box, container.ownerDocument || container),
  );
  return {
    dispose() {
      for (const stop of stopped.splice(0)) stop();
    },
  };
}

const MAX_RESULTS = 12;
const SNIPPET_BEFORE = 62;
const SNIPPET_AFTER = 140;

function enhance(box, document) {
  const input = box.querySelector('input');
  const results = box.querySelector('.search-results');
  if (!input || !results) return () => {};
  const window = document.defaultView;
  let entries; // null until the index loads; the box stays inert before that
  let loading = false;
  let failed = false;
  let options = [];
  let active = -1;
  let closing = 0;

  // The list already lives in the page, so choose a free id BEFORE assigning it:
  // testing the element's own id would always find the element itself.
  if (!results.id) {
    let id;
    do {
      id = `search-results-${++sequence}`;
    } while (document.getElementById(id));
    results.id = id;
  }
  input.setAttribute('role', 'combobox');
  input.setAttribute('aria-expanded', 'false');
  input.setAttribute('aria-controls', results.id);
  input.setAttribute('aria-autocomplete', 'list');
  results.setAttribute('role', 'listbox');
  results.setAttribute('aria-label', 'Documentation results');

  async function load() {
    if (entries || loading || failed) return;
    loading = true;
    try {
      const response = await window.fetch(new URL(box.dataset.index, document.baseURI));
      if (!response.ok) throw new Error(`HTTP ${response.status}`);
      const data = await response.json();
      entries = data.pages.map((page) => ({
        ...page,
        hay: `${page.title} ${page.section} ${page.heading} ${page.text}`.toLowerCase(),
      }));
    } catch {
      failed = true;
      input.disabled = true;
      input.placeholder = 'Search unavailable';
      return;
    } finally {
      loading = false;
    }
    render();
  }

  function rank(query) {
    const tokens = query.toLowerCase().split(/\s+/).filter(Boolean);
    if (!tokens.length) return [];
    const scored = [];
    for (const entry of entries || []) {
      const title = entry.title.toLowerCase();
      const heading = entry.heading.toLowerCase();
      let score = 0;
      for (const token of tokens) {
        if (!entry.hay.includes(token)) {
          score = -1;
          break;
        }
        score += title.includes(token) ? 4 : heading.includes(token) ? 2 : 1;
      }
      if (score > 0) scored.push([score, entry]);
    }
    // A stable sort keeps corpus order — sidebar order — between equal scores.
    return scored
      .sort((a, b) => b[0] - a[0])
      .slice(0, MAX_RESULTS)
      .map(([, entry]) => entry);
  }

  function snippet(entry, tokens) {
    const lower = entry.text.toLowerCase();
    let at = -1;
    let token = '';
    for (const candidate of tokens) {
      const found = lower.indexOf(candidate);
      if (found >= 0 && (at < 0 || found < at)) {
        at = found;
        token = candidate;
      }
    }
    if (at < 0) return null;
    const start = Math.max(0, at - SNIPPET_BEFORE);
    const end = Math.min(entry.text.length, at + token.length + SNIPPET_AFTER);
    return {
      before: (start > 0 ? '…' : '') + entry.text.slice(start, at),
      match: entry.text.slice(at, at + token.length),
      after: entry.text.slice(at + token.length, end) + (end < entry.text.length ? '…' : ''),
    };
  }

  function render() {
    const query = input.value.trim();
    options = query ? rank(query) : [];
    active = -1;
    results.replaceChildren();
    for (const entry of options) {
      results.append(option(entry, query));
    }
    if (query && !options.length) {
      const empty = document.createElement('div');
      empty.className = 'search-empty';
      empty.setAttribute('role', 'status');
      empty.textContent = 'No matching sections.';
      results.append(empty);
    }
    results.hidden = !query;
    input.setAttribute('aria-expanded', String(results.children.length > 0));
  }

  function option(entry, query) {
    const node = document.createElement('a');
    node.className = 'search-result';
    node.href = entry.href;
    node.setAttribute('role', 'option');
    node.id = `${results.id}-${results.children.length}`;
    const head = document.createElement('span');
    head.className = 'search-result__head';
    const page = document.createElement('strong');
    page.textContent = entry.title;
    head.append(page);
    if (entry.heading) {
      const heading = document.createElement('span');
      heading.className = 'search-result__heading';
      heading.textContent = entry.heading;
      head.append(heading);
    }
    if (entry.section) {
      const section = document.createElement('span');
      section.className = 'search-result__section';
      section.textContent = entry.section;
      head.append(section);
    }
    node.append(head);
    const parts = snippet(entry, query.toLowerCase().split(/\s+/).filter(Boolean));
    if (parts) {
      const line = document.createElement('span');
      line.className = 'search-result__snippet';
      line.append(parts.before);
      const mark = document.createElement('mark');
      mark.textContent = parts.match;
      line.append(mark, parts.after);
      node.append(line);
    }
    node.addEventListener('mousemove', () => activate([...results.children].indexOf(node)));
    return node;
  }

  function activate(index) {
    active = index;
    for (const [i, node] of [...results.children].entries()) {
      node.toggleAttribute('data-active', i === active);
      if (node.getAttribute('role') === 'option') {
        node.setAttribute('aria-selected', String(i === active));
      }
    }
    results.children[active]?.scrollIntoView?.({ block: 'nearest' });
    if (results.children[active]) input.setAttribute('aria-activedescendant', results.children[active].id);
    else input.removeAttribute('aria-activedescendant');
  }

  function close() {
    options = [];
    active = -1;
    results.replaceChildren();
    results.hidden = true;
    input.setAttribute('aria-expanded', 'false');
    input.removeAttribute('aria-activedescendant');
  }

  function keys(event) {
    if (event.altKey || event.ctrlKey || event.metaKey) return;
    if (event.key === 'ArrowDown' || event.key === 'ArrowUp') {
      if (!results.children.length) return;
      event.preventDefault();
      const count = results.children.length;
      const down = event.key === 'ArrowDown';
      const next =
        active < 0
          ? down
            ? 0
            : count - 1
          : Math.min(count - 1, Math.max(0, active + (down ? 1 : -1)));
      activate(next);
    } else if (event.key === 'Enter') {
      const node =
        results.children[active] ?? [...results.children].find((child) => child.getAttribute('role') === 'option');
      if (node) {
        event.preventDefault();
        node.click();
      }
    } else if (event.key === 'Escape') {
      close();
    }
  }

  // `/` from anywhere jumps to the box; typing `/` in a field must keep working.
  function slash(event) {
    if (event.key !== '/' || event.altKey || event.ctrlKey || event.metaKey) return;
    if (event.target?.closest?.('input, textarea, select, [contenteditable]')) return;
    event.preventDefault();
    input.focus();
    input.select();
  }

  function outsidePress(event) {
    if (!box.contains(event.target)) close();
  }
  function refocus() {
    window.clearTimeout(closing);
  }

  input.addEventListener('focus', refocus);
  input.addEventListener('focus', load);
  input.addEventListener('input', render);
  input.addEventListener('keydown', keys);
  // A result click must survive the focusout that precedes it on browsers that
  // do not focus links on click, so closing is deferred a beat, not instant.
  box.addEventListener('mousedown', refocus);
  box.addEventListener('focusout', () => {
    closing = window.setTimeout(close, 150);
  });
  document.addEventListener('mousedown', outsidePress);
  document.addEventListener('keydown', slash);

  return () => {
    document.removeEventListener('mousedown', outsidePress);
    document.removeEventListener('keydown', slash);
    window.clearTimeout(closing);
    close();
    input.removeAttribute('role');
    input.removeAttribute('aria-expanded');
    input.removeAttribute('aria-controls');
    input.removeAttribute('aria-activedescendant');
    input.removeAttribute('aria-autocomplete');
  };
}
