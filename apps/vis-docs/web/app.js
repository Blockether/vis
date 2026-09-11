/** Enhance the Worker-rendered page; the catalog and detail links also work without JavaScript. */
import {
  shellHTML,
  cardsHTML,
  categoriesHTML,
  detailHTML,
  previewHTML,
  tocHTML,
  filters,
  filterURL,
  visibleItems,
} from './render.js';
export { installCommand } from './render.js';
import { catalogMetadata } from './discovery.js';
import { mountCommunity } from './community.js';
import { loadTurnstile } from './turnstile.js';
function updateMetadata(data = {}) {
  document.head.querySelectorAll('[data-discovery]').forEach((node) => node.remove());
  document.head.insertAdjacentHTML('beforeend', catalogMetadata(data));
}

export function mount(container, request = fetch, initial) {
  const $ = (selector) => container.querySelector(selector);
  if (!$('#catalog-page'))
    container.innerHTML = shellHTML(initial || { search: window.location.search });
  let state = filters(window.location.search),
    items = initial?.items || [],
    disposed = false,
    routeRevision = 0,
    submissionRevision = 0,
    challengeRevision = 0,
    preview = null,
    token = '',
    widget = null,
    cleanupCommunity = null;
  const feedback = (item) => {
    cleanupCommunity = mountCommunity(
      $('#feedback'),
      item.id,
      $('#turnstile-widget').dataset.sitekey,
      request,
    );
  };
  const navigation = $('#navtoggle'),
    mobile = window.matchMedia('(max-width: 820px)'),
    dialog = $('#submit-dialog'),
    form = $('#repository-form');
  async function api(path, options) {
    const response = await request(path, options);
    const data = await response.json();
    if (!response.ok) throw new Error(data.error || 'Request failed. Try again.');
    return data;
  }
  function syncNavigation() {
    const open = mobile.matches && navigation.checked;
    $('#catalog-navigation').inert = mobile.matches && !open;
    $('.main').inert = open;
    $('.toc').inert = open;
    navigation.setAttribute('aria-expanded', String(open));
    navigation.tabIndex = mobile.matches ? 0 : -1;
    document.body.style.overflow = open || dialog.open ? 'hidden' : '';
  }
  function closeNavigation() {
    navigation.checked = false;
    syncNavigation();
  }
  navigation.onchange = syncNavigation;
  mobile.addEventListener('change', syncNavigation);
  syncNavigation();
  function renderList() {
    $('#results').innerHTML = cardsHTML(items, state);
    $('#categories').innerHTML = categoriesHTML(items, state);
    $('#catalog-status').textContent = `${visibleItems(items, state).length} extensions`;
    $('#search').value = state.q;
    $('#sort').value = state.sort;
    $('#filters [name=category]').value = state.category;
  }
  function saveFilters() {
    window.history.replaceState(null, '', filterURL(state));
  }
  async function load() {
    $('#results').setAttribute('aria-busy', 'true');
    $('#catalog-status').textContent = 'Loading extensions…';
    try {
      const data = await api('/api/extensions');
      if (!disposed) {
        items = data.extensions;
        renderList();
      }
    } catch {
      if (!disposed)
        $('#catalog-status').innerHTML =
          `${items.length ? 'Showing saved results. Could not refresh.' : 'Could not load the catalog.'} <a id="retry" href="/extensions/">Retry</a>`;
    } finally {
      $('#results')?.removeAttribute('aria-busy');
    }
  }
  async function route(focus = false) {
    cleanupCommunity?.();
    cleanupCommunity = null;
    const identity = window.location.pathname.match(/^\/extensions\/([0-9a-f]{24})$/)?.[1],
      revision = ++routeRevision;
    $('#catalog-page').hidden = !!identity;
    $('#detail-page').hidden = !identity;
    $('.toc').innerHTML = tocHTML(!!identity);
    $('#back-to-catalog').href = filterURL(state);
    if (!identity) {
      updateMetadata();
      return;
    }
    $('#detail').innerHTML = '<p>Loading extension…</p>';
    try {
      const version = new URLSearchParams(window.location.search).get('version');
      const item = await api(
        '/api/extensions/' +
          identity +
          (version === null ? '' : '?version=' + encodeURIComponent(version)),
      );
      if (disposed || revision !== routeRevision) return;
      $('#detail').innerHTML = detailHTML(item);
      updateMetadata({ item });
      feedback(item);
      if (focus) $('#detail h1').focus({ preventScroll: true });
    } catch (error) {
      if (!disposed && revision === routeRevision) {
        updateMetadata({ detailError: true });
        $('#detail').innerHTML =
          '<h1>Could not load this extension</h1><p></p><button id="retry-detail" type="button">Retry details</button>';
        $('#detail p').textContent = error.message;
      }
    }
  }
  function navigate(href) {
    window.history.pushState(null, '', href);
    route(true);
    window.scrollTo({ top: 0 });
  }
  const pop = () => {
    state = filters(window.location.search);
    renderList();
    route();
  };
  window.addEventListener('popstate', pop);
  function clearFilters() {
    state = { ...state, q: '', category: 'all' };
    renderList();
    saveFilters();
    $('#search').focus();
  }
  $('#search').oninput = () => {
    state.q = $('#search').value;
    renderList();
    saveFilters();
  };
  $('#sort').onchange = () => {
    state.sort = $('#sort').value;
    renderList();
    saveFilters();
  };
  $('#filters').onsubmit = (event) => {
    event.preventDefault();
    renderList();
    saveFilters();
  };
  function removeChallenge() {
    ++challengeRevision;
    token = '';
    if (widget !== null) {
      window.turnstile?.remove(widget);
      widget = null;
    }
  }
  async function challenge(action) {
    removeChallenge();
    const revision = challengeRevision;
    const sitekey = $('#turnstile-widget').dataset.sitekey;
    if (!sitekey) {
      $('#submit-status').textContent = 'Submissions are not configured yet. Try again later.';
      return;
    }
    try {
      if (typeof window.turnstile?.render !== 'function') await loadTurnstile();
      if (disposed || revision !== challengeRevision || !dialog.open) return;
      widget = window.turnstile.render($('#turnstile-widget'), {
        sitekey,
        action,
        theme: 'light',
        size: 'flexible',
        callback: (value) => {
          if (revision === challengeRevision) token = value;
        },
        'expired-callback': () => {
          if (revision === challengeRevision) {
            token = '';
            $('#submit-status').textContent = 'Anti-spam check expired. Please complete it again.';
          }
        },
        'error-callback': () => {
          if (revision === challengeRevision) {
            token = '';
            $('#submit-status').textContent =
              'Anti-spam check failed. Close and reopen the form to retry.';
          }
        },
      });
    } catch (error) {
      if (revision === challengeRevision) $('#submit-status').textContent = error.message;
    }
  }
  function resetPreview() {
    ++submissionRevision;
    preview = null;
    removeChallenge();
    $('#preview').replaceChildren();
    $('#submit-confirm').hidden = true;
    $('#edit-submission').hidden = true;
    $('#submit-status').textContent = '';
    $('#review-submit').disabled = false;
    form.hidden = false;
    $('#submit-step').textContent = '1 of 2 · Repository';
    dialog.querySelector('.dialog-body').scrollTop = 0;
  }
  function openSubmission() {
    closeNavigation();
    resetPreview();
    dialog.showModal();
    syncNavigation();
    (window.matchMedia('(pointer: coarse)').matches
      ? $('#submit-close')
      : form.elements.repository_url
    ).focus({ preventScroll: true });
    challenge('extension-preview');
  }
  $('#submit-close').onclick = () => dialog.close();
  $('#edit-submission').onclick = () => {
    resetPreview();
    form.elements.repository_url.focus();
    challenge('extension-preview');
  };
  dialog.addEventListener('close', () => {
    resetPreview();
    form.reset();
    syncNavigation();
    if (!$('#catalog-page').hidden) $('#submit-open').focus({ preventScroll: true });
  });
  async function submit(confirm) {
    if (confirm && !preview) return;
    if (!token) {
      $('#submit-status').textContent = 'Complete the anti-spam check before continuing.';
      return;
    }
    const source = {
      repository_url: form.elements.repository_url.value.trim(),
      subdirectory: form.elements.subdirectory.value.trim(),
      turnstile_token: token,
    };
    const tag = form.elements.release_tag.value.trim();
    if (tag) source.release_tag = tag;
    if (confirm) {
      source.revision = preview.revision;
      source.release_tag = preview.release_tag;
    }
    const revision = ++submissionRevision;
    token = '';
    $('#review-submit').disabled = true;
    $('#submit-confirm').disabled = true;
    $('#submit-status').textContent = confirm
      ? 'Submitting for review…'
      : 'Reading GitHub metadata…';
    try {
      const data = await api(confirm ? '/api/submissions' : '/api/preview', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(source),
      });
      if (disposed || revision !== submissionRevision) return;
      if (confirm) {
        dialog.close();
        $('#notice').textContent =
          data.status === 'approved'
            ? 'This version is already approved in the catalog.'
            : 'Submitted for moderation. It will appear in the catalog only after approval. Reference: ' +
              data.id;
      } else {
        preview = data;
        form.hidden = true;
        $('#submit-step').textContent = '2 of 2 · Review';
        $('#preview').innerHTML = previewHTML(data);
        $('#submit-status').textContent =
          'Review the checks and linked source before submitting this commit.';
        $('#submit-confirm').hidden = false;
        $('#submit-confirm').disabled = false;
        $('#edit-submission').hidden = false;
        dialog.querySelector('.dialog-body').scrollTop = 0;
        $('#submit-step').focus({ preventScroll: true });
        challenge('extension-submit');
      }
    } catch (error) {
      if (revision === submissionRevision) {
        $('#submit-status').textContent = error.message;
        challenge(confirm ? 'extension-submit' : 'extension-preview');
      }
    } finally {
      if (revision === submissionRevision) {
        $('#review-submit').disabled = false;
        $('#submit-confirm').disabled = false;
      }
    }
  }
  // Input changes invalidate in-flight results without discarding a still-unused anti-spam token.
  form.oninput = () => {
    ++submissionRevision;
    $('#review-submit').disabled = false;
  };
  form.onsubmit = (event) => {
    event.preventDefault();
    submit(false);
  };
  $('#submit-confirm').onclick = () => submit(true);
  const click = async (event) => {
    const node = event.target.closest('a,button');
    if (!node || event.metaKey || event.ctrlKey || event.shiftKey || event.altKey) return;
    if (node.matches('[data-category]')) {
      event.preventDefault();
      state.category = node.dataset.category;
      renderList();
      saveFilters();
      route();
      closeNavigation();
      if (mobile.matches) $('#search').focus({ preventScroll: true });
    } else if (node.matches('.card-main,#back-to-catalog,[data-release]')) {
      event.preventDefault();
      closeNavigation();
      navigate(node.href);
      if (node.id === 'back-to-catalog') $('#search').focus({ preventScroll: true });
    } else if (node.id === 'submit-open') openSubmission();
    else if (node.id === 'clear-filters') {
      event.preventDefault();
      clearFilters();
    } else if (node.id === 'retry') {
      event.preventDefault();
      load();
    } else if (node.id === 'retry-detail') route(true);
    else if (node.id === 'copy-command') {
      try {
        await navigator.clipboard.writeText($('#install-command').textContent);
        node.textContent = 'Copied';
      } catch {
        node.textContent = 'Select and copy the command above';
      }
    } else if (node.closest('.toc')) {
      event.preventDefault();
      $(node.getAttribute('href'))?.scrollIntoView({ block: 'start' });
    }
  };
  const versionSubmit = (event) => {
    if (event.target.id === 'version-form') {
      event.preventDefault();
      navigate(event.target.action + '?version=' + encodeURIComponent($('#release-version').value));
    }
  };
  container.addEventListener('submit', versionSubmit);
  container.addEventListener('click', click);
  const keys = (event) => {
    if (dialog.open) return;
    if (mobile.matches && navigation.checked) {
      if (event.key === 'Escape') {
        event.preventDefault();
        closeNavigation();
        navigation.focus();
        return;
      }
      if (event.key === 'Tab') {
        const links = [navigation, ...container.querySelectorAll('.top a,.side a')];
        if (document.activeElement === (event.shiftKey ? links[0] : links.at(-1))) {
          event.preventDefault();
          (event.shiftKey ? links.at(-1) : links[0]).focus();
        }
      }
    }
    if ($('#catalog-page').hidden) return;
    const editable = ['INPUT', 'TEXTAREA', 'SELECT'].includes(document.activeElement?.tagName);
    if (event.key === '/' && !editable) {
      event.preventDefault();
      closeNavigation();
      $('#search').focus();
    }
    if (event.key === 'Escape' && document.activeElement === $('#search')) {
      clearFilters();
      $('#search').blur();
    }
    if (
      ['ArrowDown', 'ArrowUp'].includes(event.key) &&
      (!editable || document.activeElement === $('#search'))
    ) {
      const links = [...container.querySelectorAll('.card-main')],
        index = links.indexOf(document.activeElement),
        next =
          links[
            Math.max(0, Math.min(links.length - 1, index + (event.key === 'ArrowDown' ? 1 : -1)))
          ];
      if (next) {
        event.preventDefault();
        next.focus();
      }
    }
  };
  document.addEventListener('keydown', keys);
  if (!initial) {
    load();
    route();
  } else if (initial.item) feedback(initial.item);
  return () => {
    disposed = true;
    cleanupCommunity?.();
    ++routeRevision;
    ++submissionRevision;
    removeChallenge();
    window.removeEventListener('popstate', pop);
    mobile.removeEventListener('change', syncNavigation);
    document.removeEventListener('keydown', keys);
    container.removeEventListener('click', click);
    container.removeEventListener('submit', versionSubmit);
    document.body.style.overflow = '';
    container.replaceChildren();
  };
}
