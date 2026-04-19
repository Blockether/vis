// ── DOM refs ────────────────────────────────────────────────────────────
var chat = document.getElementById('chat');
var form = document.getElementById('form');
var input = document.getElementById('input');
var btn = document.getElementById('btn');

// ── Init ────────────────────────────────────────────────────────────────

function init() {
  renderMarkdown();
  initIcons();
  scrollToBottom(true); // instant on page load
  initInput();
}

// ── Markdown rendering ──────────────────────────────────────────────────

function renderMarkdown() {
  if (typeof marked === 'undefined') return;
  marked.setOptions({ breaks: true, gfm: true });
  document.querySelectorAll('.md-content').forEach(function(el) {
    if (el.dataset.r) return;
    el.innerHTML = marked.parse(el.textContent);
    el.dataset.r = '1';
  });
  if (typeof hljs !== 'undefined') hljs.highlightAll();
}

// ── Icons ───────────────────────────────────────────────────────────────

function initIcons() {
  if (typeof lucide !== 'undefined') lucide.createIcons();
}

// ── Scroll ──────────────────────────────────────────────────────────────

function scrollToBottom(instant) {
  setTimeout(function() {
    chat.scrollTo({ top: chat.scrollHeight, behavior: instant ? 'instant' : 'smooth' });
  }, 50);
}

// ── Conversation sheet ─────────────────────────────────────────────────

function openSheet() {
  document.getElementById('sheet').classList.add('open');
}

function closeSheet() {
  document.getElementById('sheet').classList.remove('open');
}

// ── Copy to clipboard (iOS Safari + modern Clipboard API) ──────────────

function copyToClipboard(text) {
  // Primary: Modern Clipboard API (iOS 13.4+, all modern browsers)
  if (navigator.clipboard && navigator.clipboard.writeText) {
    return navigator.clipboard.writeText(text).then(function() {
      return true;
    }).catch(function() {
      return fallbackCopy(text);
    });
  }
  var result = fallbackCopy(text);
  return Promise.resolve(result);
}

function fallbackCopy(text) {
  var ta = document.createElement('textarea');
  ta.value = text;
  ta.setAttribute('readonly', '');
  ta.style.cssText = 'position:fixed;left:-9999px;top:0;font-size:12pt;border:none;outline:none;box-shadow:none;background:transparent';
  document.body.appendChild(ta);
  ta.contentEditable = true;
  ta.readOnly = false;
  var range = document.createRange();
  range.selectNodeContents(ta);
  var sel = window.getSelection();
  sel.removeAllRanges();
  sel.addRange(range);
  ta.setSelectionRange(0, 999999);
  var ok = false;
  try { ok = document.execCommand('copy'); } catch (e) {}
  document.body.removeChild(ta);
  return ok;
}

function closeCopyModal() {
  document.getElementById('copy-modal').classList.remove('open');
}

function extractTraceText(bubble) {
  var text = '';
  bubble.querySelectorAll('.iteration').forEach(function(it) {
    var h = it.querySelector('.iter-header');
    if (h) text += h.textContent + '\n';
    it.querySelectorAll('.exec,.exec-final').forEach(function(ex) {
      var c = ex.querySelector('.exec-code');
      if (c) text += '> ' + c.textContent + '\n';
      var r = ex.querySelector('.exec-result');
      if (r) text += '=> ' + r.textContent + '\n';
      var e = ex.querySelector('.exec-error');
      if (e) text += 'ERROR: ' + e.textContent + '\n';
      var a = ex.querySelector('.exec-answer');
      if (a) text += 'Answer: ' + a.textContent + '\n';
    });
    text += '\n';
  });
  var ans = bubble.querySelector('.answer');
  if (ans) text += 'Answer: ' + ans.textContent + '\n';
  return text;
}

var toastEl = null;
var toastTimer = null;

function showToast(msg) {
  if (!toastEl) {
    toastEl = document.createElement('div');
    toastEl.className = 'toast';
    document.body.appendChild(toastEl);
  }
  clearTimeout(toastTimer);
  toastEl.textContent = '';
  var icon = document.createElement('span');
  icon.textContent = '\u2713';
  icon.style.cssText = 'font-size:16px;line-height:1';
  toastEl.appendChild(icon);
  toastEl.appendChild(document.createTextNode(msg || 'Copied'));
  // Force reflow then show
  toastEl.classList.remove('show');
  void toastEl.offsetWidth;
  toastEl.classList.add('show');
  toastTimer = setTimeout(function() { toastEl.classList.remove('show'); }, 1800);
}

function copyTrace(btn) {
  var bubble = btn.closest('.ai-bubble');
  if (!bubble) return;
  var text = extractTraceText(bubble);
  copyToClipboard(text).then(function(ok) {
    if (ok !== false) {
      showToast('Copied');
    } else {
      document.getElementById('copy-modal-text').textContent = text;
      document.getElementById('copy-modal').classList.add('open');
    }
  });
}

function copyMsg(btn) {
  var bubble = btn.closest('.bubble');
  if (!bubble) return;
  var clone = bubble.cloneNode(true);
  var meta = clone.querySelector('.meta');
  if (meta) meta.remove();
  var copyBtns = clone.querySelectorAll('.copy-btn');
  copyBtns.forEach(function(b) { b.remove(); });
  var text = clone.textContent.trim();
  copyToClipboard(text).then(function(ok) {
    if (ok !== false) showToast('Copied');
  });
}

function copyModalText() {
  var text = document.getElementById('copy-modal-text').textContent;
  copyToClipboard(text).then(function(ok) {
    if (ok !== false) {
      showToast('Copied');
      closeCopyModal();
    }
  });
}

// ── Message selection mode (Apple Messages style) ───────────────────────

var selectMode = false;
var selectedMsgs = new Set();

function toggleSelectMode() {
  if (selectMode) exitSelectMode();
  else enterSelectMode();
}

function enterSelectMode() {
  var msgs = document.querySelectorAll('.msg');
  if (!msgs.length) { showToast('No messages'); return; }
  selectMode = true;
  selectedMsgs.clear();
  document.body.classList.add('select-mode');
  // Add floating action bar
  var bar = document.createElement('div');
  bar.id = 'select-bar';
  bar.className = 'select-bar';
  bar.innerHTML = '<button class="select-cancel" onclick="exitSelectMode()">Cancel</button>' +
    '<span id="select-count">0 selected</span>' +
    '<button class="select-copy" onclick="copySelected()">Copy</button>';
  document.body.appendChild(bar);
  // Make messages tappable
  document.querySelectorAll('.msg').forEach(function(msg, i) {
    msg.dataset.selectIdx = i;
    msg.addEventListener('click', onMsgSelect);
  });
}

function exitSelectMode() {
  selectMode = false;
  selectedMsgs.clear();
  document.body.classList.remove('select-mode');
  var bar = document.getElementById('select-bar');
  if (bar) bar.remove();
  document.querySelectorAll('.msg').forEach(function(msg) {
    msg.classList.remove('msg-selected');
    msg.removeEventListener('click', onMsgSelect);
  });
}

function onMsgSelect(e) {
  if (!selectMode) return;
  e.preventDefault();
  var msg = e.currentTarget;
  var idx = msg.dataset.selectIdx;
  if (selectedMsgs.has(idx)) {
    selectedMsgs.delete(idx);
    msg.classList.remove('msg-selected');
  } else {
    selectedMsgs.add(idx);
    msg.classList.add('msg-selected');
  }
  var countEl = document.getElementById('select-count');
  if (countEl) countEl.textContent = selectedMsgs.size + ' selected';
}

function copySelected() {
  var texts = [];
  document.querySelectorAll('.msg').forEach(function(msg) {
    if (selectedMsgs.has(msg.dataset.selectIdx)) {
      var bubble = msg.querySelector('.bubble');
      if (bubble) {
        var isUser = msg.classList.contains('user-msg');
        var prefix = isUser ? 'User: ' : 'Assistant: ';
        // Get clean text
        var clone = bubble.cloneNode(true);
        var meta = clone.querySelector('.meta');
        if (meta) meta.remove();
        clone.querySelectorAll('.iter-header').forEach(function(el) { el.remove(); });
        var answer = clone.querySelector('.answer');
        var text = answer ? answer.textContent.trim() : clone.textContent.trim();
        texts.push(prefix + text);
      }
    }
  });
  if (texts.length > 0) {
    copyToClipboard(texts.join('\n\n')).then(function(ok) {
      if (ok !== false) showToast('Copied ' + texts.length + ' messages');
    });
  }
  exitSelectMode();
}

// ── Context viewer ──────────────────────────────────────────────────────

function showContext() {
  var sid = chat.dataset.conversation;
  if (!sid) return;
  var sidebar = document.getElementById('context-sidebar');
  if (!sidebar) return;
  sidebar.classList.add('open');
  var bg = document.getElementById('sidebar-bg');
  if (!bg) {
    bg = document.createElement('div');
    bg.id = 'sidebar-bg';
    bg.className = 'sidebar-bg open';
    bg.onclick = closeContext;
    document.body.appendChild(bg);
  } else {
    bg.classList.add('open');
  }
  fetch('/conversations/' + sid + '/context')
    .then(function(r) { return r.json(); })
    .then(function(data) {
      window.__varIndex = {};
      var userVars   = (data.variables && data.variables.length) ? data.variables : [];
      var systemVars = (data['system-variables'] && data['system-variables'].length) ? data['system-variables'] : [];
      userVars.concat(systemVars).forEach(function(v) { window.__varIndex[v.name] = v; });

      function renderVarCard(v) {
        var versionCount = (v.versions && v.versions.length) || v.version || 1;
        var label = v['display-name'] || v.name;
        var s = '';
        s += '<div class="ctx-card ctx-var' + (v['system?'] ? ' ctx-var-system' : '') + '" onclick="openVarHistory(\'' + escHtml(v.name) + '\')">';
        s += '<div class="ctx-var-header">';
        s += '<span class="ctx-var-name">' + escHtml(label) + '</span>';
        s += '<span class="ctx-var-type">' + escHtml(v.type || '?') + '</span>';
        if (versionCount > 1) s += '<span class="ctx-var-versions">v' + versionCount + '</span>';
        s += '</div>';
        s += '<span class="ctx-var-value">' + escHtml((v.value || '').substring(0, 200)) + ((v.value || '').length > 200 ? '…' : '') + '</span>';
        s += '</div>';
        return s;
      }

      var html = '';
      // SYSTEM variables — agent-loop bookkeeping (*query*, *reasoning*, *answer*),
      // always visible at the top, never collapsed.
      if (systemVars.length > 0) {
        html += '<div class="ctx-section ctx-section-system">';
        html += '<div class="ctx-section-header"><i data-lucide="cpu"></i> System <span class="ctx-count">' + systemVars.length + '</span></div>';
        systemVars.forEach(function(v) { html += renderVarCard(v); });
        html += '</div>';
      }
      // User-defined variables — below system vars, always expanded.
      if (userVars.length > 0) {
        html += '<div class="ctx-section">';
        html += '<div class="ctx-section-header"><i data-lucide="variable"></i> Variables <span class="ctx-count">' + userVars.length + '</span></div>';
        userVars.forEach(function(v) { html += renderVarCard(v); });
        html += '</div>';
      }
      if (!html) html = '<div class="ctx-empty">Nothing here yet. Variables will appear as the agent works.</div>';
      if (typeof lucide !== 'undefined') setTimeout(function() { lucide.createIcons(); }, 50);
      document.getElementById('sidebar-content').innerHTML = html;
    })
    .catch(function() { showToast('Failed to load context'); });
}

function openVarHistory(varName) {
  var v = (window.__varIndex || {})[varName];
  if (!v) return;
  var isSystemVar = !!v['system?'];
  var titleLabel = v['display-name'] || v.name;
  var sidebar = document.getElementById('context-sidebar');
  if (!sidebar) return;
  var panel = document.getElementById('var-history-panel');
  if (!panel) {
    panel = document.createElement('div');
    panel.id = 'var-history-panel';
    panel.className = 'var-history-panel';
    sidebar.appendChild(panel);
  }
  var versions = (v.versions && v.versions.length) ? v.versions.slice() : [{
    version: 1, type: v.type, preview: v.value, code: v.code, 'created-at': ''
  }];
  // Newest first in UI.
  versions.reverse();
  var latestVersion = versions[0] && versions[0].version;
  var html = '';
  html += '<div class="var-history-header">';
  html += '<button class="var-history-back" onclick="closeVarHistory()" title="Back to variables"><i data-lucide="arrow-left"></i></button>';
  html += '<span class="var-history-title">' + escHtml(titleLabel) + '</span>';
  html += '<span class="var-history-count">' + versions.length + ' version' + (versions.length === 1 ? '' : 's') + '</span>';
  html += '</div>';
  html += '<div class="var-history-body">';
  versions.forEach(function(ver) {
    var isLatest = ver.version === latestVersion;
    html += '<div class="var-version' + (isLatest ? ' var-version-latest' : '') + '">';
    html += '<div class="var-version-header">';
    html += '<span class="var-version-badge">v' + ver.version + (isLatest ? ' · latest' : '') + '</span>';
    html += '<span class="var-version-time">' + escHtml(ver['created-at'] || '') + '</span>';
    if (ver.type) html += '<span class="var-version-type">' + escHtml(ver.type) + '</span>';
    html += '</div>';
    html += '<div class="var-version-body">';
    if (ver.code && !isSystemVar) {
      html += '<div class="var-version-label">source</div>';
      html += '<pre class="var-version-code">' + escHtml(ver.code) + '</pre>';
    }
    if (!isSystemVar) html += '<div class="var-version-label">value</div>';
    html += '<pre class="var-version-preview">' + escHtml(ver.preview || '') + '</pre>';
    html += '</div></div>';
  });
  html += '</div>';
  panel.innerHTML = html;
  if (typeof lucide !== 'undefined') setTimeout(function() { lucide.createIcons(); }, 30);
  panel.classList.add('open');
}

function closeVarHistory() {
  var panel = document.getElementById('var-history-panel');
  if (panel) panel.classList.remove('open');
}

function closeContext() {
  closeVarHistory();
  var sidebar = document.getElementById('context-sidebar');
  if (sidebar) sidebar.classList.remove('open');
  var bg = document.getElementById('sidebar-bg');
  if (bg) bg.classList.remove('open');
}

function copyCtxItem(btn) {
  var text = btn.parentElement.querySelector('.ctx-text');
  if (text) {
    copyToClipboard(text.textContent).then(function(ok) {
      if (ok !== false) showToast('Copied');
    });
  }
}

// ── Infinite scroll (load older messages) ───────────────────────────────

var loadingMore = false;

function initInfiniteScroll() {
  chat.addEventListener('scroll', function() {
    if (loadingMore || chat.scrollTop > 50) return;
    var lm = document.getElementById('load-more');
    if (!lm) return;
    loadingMore = true;
    var sid = chat.dataset.conversation;
    var pageSize = parseInt(chat.dataset.pageSize || '8');
    var showing = parseInt(chat.dataset.showing || String(pageSize));
    // Next fetch advances by one page. `data-showing` now tracks the ACTUAL
    // number of messages rendered, so after loading we'll have exactly
    // `showing + pageSize` visible and the next scroll-up advances again.
    var newOffset = showing + pageSize;
    var oldH = chat.scrollHeight;
    fetch('/conversations/' + sid + '?offset=' + newOffset)
      .then(function(r) { return r.text(); })
      .then(function(html) {
        var doc = new DOMParser().parseFromString(html, 'text/html');
        var nc = doc.getElementById('chat');
        if (nc) {
          chat.innerHTML = nc.innerHTML;
          chat.dataset.showing = nc.dataset.showing;
          chat.dataset.total = nc.dataset.total;
          chat.scrollTop = chat.scrollHeight - oldH;
          renderMarkdown();
          initIcons();
        }
        // Delay resetting flag so scroll event doesn't re-fire immediately
        setTimeout(function() { loadingMore = false; }, 500);
      })
      .catch(function() { loadingMore = false; });
  });
}

// ── DOM helpers for chat ────────────────────────────────────────────────

function getChatInner() {
  return chat.querySelector('.chat-inner') || chat;
}

function appendUserBubble(text) {
  var d = document.createElement('div');
  d.className = 'msg user-msg';
  d.innerHTML = '<div class="bubble user-bubble">' +
    '<span>' + text.replace(/&/g, '&amp;').replace(/</g, '&lt;') + '</span></div>';
  getChatInner().appendChild(d);
}

function appendThinkingBubble() {
  var d = document.createElement('div');
  d.className = 'msg ai-msg';
  d.id = 'thinking-msg';
  d.innerHTML = '<div class="bubble ai-bubble thinking-bubble" style="color:var(--dim)">' +
    '<div id="live-trace"></div>' +
    '<span class="thinking-dots">Thinking<span>.</span><span>.</span><span>.</span></span></div>';
  getChatInner().appendChild(d);
}

function appendErrorBubble(msg) {
  var el = document.getElementById('thinking-msg');
  if (el) el.remove();
  var d = document.createElement('div');
  d.className = 'msg ai-msg';
  d.innerHTML = '<div class="bubble ai-bubble" style="border-color:#c44">' +
    '<div style="color:#c44;font-weight:600;font-size:13px">' +
    msg.replace(/&/g, '&amp;').replace(/</g, '&lt;') + '</div></div>';
  getChatInner().appendChild(d);
  scrollToBottom();
}

function setButtonLoading(loading) {
  if (loading) {
    btn.disabled = true;
    btn.innerHTML = '<div class="spinner"></div>';
    input.disabled = false;
    input.focus();
  } else {
    btn.disabled = true; // re-enabled by input listener
    btn.innerHTML = '<i data-lucide="arrow-up" style="width:20px;height:20px"></i>';
    initIcons();
    input.disabled = false;
    input.focus();
  }
}

function replaceChat(html) {
  try {
    var doc = new DOMParser().parseFromString(html, 'text/html');
    var nc = doc.getElementById('chat');
    if (nc) {
      chat.style.opacity = '0';
      chat.innerHTML = nc.innerHTML;
      chat.dataset.total = nc.dataset.total;
      // Topbar title AND every sheet row are kept in sync from the same
      // parsed document — so a rename on the server shows up in BOTH
      // the header and the bottom-sheet list on the very next response.
      syncConversationList(doc);
      // Enable select/copy button if messages are now present
      var selectBtn = document.getElementById('select-btn');
      if (selectBtn && nc.children.length > 0) {
        selectBtn.disabled = false;
        selectBtn.classList.remove('topbar-btn-disabled');
      }
      scrollToBottom(true);
      renderMarkdown();
      initIcons();
      requestAnimationFrame(function() {
        chat.style.transition = 'opacity .2s ease';
        chat.style.opacity = '1';
        setTimeout(function() { chat.style.transition = ''; }, 250);
      });
    } else {
      console.error('[vis] replaceChat: no #chat in response');
      appendErrorBubble('Failed to load response. Tap to reload.');
      chat.lastChild.onclick = function() { document.location.reload(); };
    }
  } catch(e) {
    console.error('[vis] replaceChat error:', e);
    appendErrorBubble('Error: ' + e.message + '. Tap to reload.');
    chat.lastChild.onclick = function() { document.location.reload(); };
  }
}

// ── Live trace rendering ────────────────────────────────────────────────

function renderLiveTrace(iterations) {
  // Mirrors the Clojure-side `render-iteration` in
  // adapters/web/presentation/message.clj — iteration header + code/result
  // rows only. The `:thinking` narrative is intentionally NOT rendered; it
  // used to drift turn-to-turn and felt like the system prompt was
  // changing. The main chat shows WHAT the model did, not its inner monologue.
  if (!iterations || !iterations.length) return '';
  var html = '';
  for (var i = 0; i < iterations.length; i++) {
    var it = iterations[i];
    if (it["final?"]) continue; // FINAL answer is shown separately
    html += '<div class="iteration">';
    html += '<div class="iter-header">Iteration ' + (it.iteration + 1) + '</div>';
    if (it.executions) {
      for (var j = 0; j < it.executions.length; j++) {
        var ex = it.executions[j];
        if (ex.error) {
          html += '<div class="exec"><div class="exec-code">' + escHtml(ex.code || '') + '</div>';
          html += '<div class="exec-error">' + escHtml(ex.error) + '</div></div>';
        } else if (ex.code) {
          html += '<div class="exec">';
          html += '<div class="exec-code">' + escHtml(ex.code) + '</div>';
          if (ex.result && ex.result !== 'nil') html += '<div class="exec-result">' + escHtml(ex.result) + '</div>';
          html += '</div>';
        }
      }
    }
    html += '</div>';
  }
  return html;
}

function escHtml(s) {
  return s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
}

function extractBadge(code) {
  if (!code) return null;
  var t = code.trim();
  if (t.charAt(0) === '(') t = t.substring(1);
  var m = t.split(/[\s\)\("']/);
  return m[0] || null;
}

// ── Polling for async query completion ──────────────────────────────────

function pollForResponse(action, expectedCount) {
  var attempts = 0;
  var lastIterCount = 0;
  var poll = setInterval(function() {
    attempts++;
    fetch(action + '?check=' + expectedCount)
      .then(function(r) { return r.json(); })
      .then(function(data) {
        if (data.ready) {
          clearInterval(poll);
          fetch(action)
            .then(function(r) { return r.text(); })
            .then(function(html) {
              replaceChat(html);
              setButtonLoading(false);
              onQueryComplete();
            })
            .catch(function(e) {
              appendErrorBubble('Connection error. Tap to reload.');
              chat.lastChild.onclick = function() { document.location.reload(); };
              setButtonLoading(false);
              onQueryComplete();
            });
        } else {
          // Render live trace iterations
          var liveEl = document.getElementById('live-trace');
          var traceHtml = '';
          if (liveEl && data.iterations && data.iterations.length > 0) {
            traceHtml = renderLiveTrace(data.iterations);
            liveEl.innerHTML = traceHtml;
            renderMarkdown();
            if (data.iterations.length > lastIterCount) {
              scrollToBottom();
              lastIterCount = data.iterations.length;
            }
          }
          // Dots stay visible as the ONLY in-flight indicator until
          // `replaceChat` swaps in the real response. The only time we
          // hide them is when live-trace already shows iteration cards
          // — then the dots would just be noise next to concrete steps.
          var dots = document.querySelector('.thinking-dots');
          if (dots) {
            var hasVisibleTrace = traceHtml && traceHtml.length > 0;
            if (hasVisibleTrace) {
              dots.style.display = 'none';
            } else {
              dots.style.display = '';
            }
          }
        }
      })
      .catch(function() {
        if (attempts > 60) {
          clearInterval(poll);
          appendErrorBubble('Response timed out. Tap to reload.');
          chat.lastChild.onclick = function() { document.location.reload(); };
          setButtonLoading(false);
          onQueryComplete();
        }
      });
  }, 2000);
}

// ── Message queue (localStorage) ────────────────────────────────────────

var MSG_QUEUE_KEY = 'vis_msg_queue_' + (chat ? chat.dataset.conversation : '');
var queryInFlight = false;

function getQueue() {
  try { return JSON.parse(localStorage.getItem(MSG_QUEUE_KEY)) || []; }
  catch(e) { return []; }
}

function saveQueue(q) {
  localStorage.setItem(MSG_QUEUE_KEY, JSON.stringify(q));
}

function clearQueue() {
  localStorage.removeItem(MSG_QUEUE_KEY);
}

function sendMessage(q) {
  queryInFlight = true;
  appendUserBubble(q);
  appendThinkingBubble();
  // Enable select/copy button now that we have messages
  var selectBtn = document.getElementById('select-btn');
  if (selectBtn) { selectBtn.disabled = false; selectBtn.classList.remove('topbar-btn-disabled'); }
  scrollToBottom();
  setButtonLoading(true);

  var msgCount = parseInt(chat.dataset.total || '0') + 1;
  fetch(form.action, {
    method: 'POST',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    body: 'q=' + encodeURIComponent(q)
  });

  pollForResponse(form.action, msgCount);
}

var titlePoll = null;

// Sync the topbar title AND every sheet row's name from a parsed HTML
// document. Called from both `replaceChat` (post-query) and the auto-title
// poll, so any rename / new-chat / deletion that landed server-side
// propagates into BOTH places the title appears in the DOM. Previously
// only the topbar was patched, leaving the bottom-sheet list stuck on
// "New Chat" until the user navigated away.
function syncConversationList(doc) {
  if (!doc) return false;
  var changed = false;
  // Topbar — single element, by class.
  var newTitle = doc.querySelector('.topbar-title');
  var curTitle = document.querySelector('.topbar-title');
  if (newTitle && curTitle && newTitle.textContent !== curTitle.textContent) {
    curTitle.textContent = newTitle.textContent;
    changed = true;
  }
  // Sheet list — one row per conversation, matched by data-id.
  var newItems = doc.querySelectorAll('.sheet-item[data-id]');
  newItems.forEach(function(newItem) {
    var id = newItem.getAttribute('data-id');
    if (!id) return;
    var curItem = document.querySelector('.sheet-item[data-id="' + id + '"]');
    if (!curItem) return;
    var newName = newItem.querySelector('.sheet-item-name');
    var curName = curItem.querySelector('.sheet-item-name');
    if (newName && curName && newName.textContent !== curName.textContent) {
      curName.textContent = newName.textContent;
      changed = true;
    }
  });
  return changed;
}

function refreshTitle() {
  if (titlePoll) clearInterval(titlePoll);
  var curTitle = document.querySelector('.topbar-title');
  if (!curTitle) return;
  var original = curTitle.textContent;
  var attempts = 0;
  titlePoll = setInterval(function() {
    attempts++;
    if (attempts > 15) { clearInterval(titlePoll); titlePoll = null; return; }
    fetch(form.action)
      .then(function(r) { return r.text(); })
      .then(function(html) {
        var doc = new DOMParser().parseFromString(html, 'text/html');
        var newTitle = doc.querySelector('.topbar-title');
        if (newTitle && newTitle.textContent !== original) {
          // syncConversationList patches BOTH the topbar and every sheet
          // row, so the bottom sheet stays in step with the topbar.
          syncConversationList(doc);
          clearInterval(titlePoll);
          titlePoll = null;
        }
      })
      .catch(function() {});
  }, 2000);
}

function onQueryComplete() {
  queryInFlight = false;
  // Poll for title update (name generation is async)
  refreshTitle();
  // Check queue for pending messages
  var queue = getQueue();
  if (queue.length > 0) {
    var next = queue.shift();
    saveQueue(queue);
    // Small delay to let UI settle
    setTimeout(function() { sendMessage(next); }, 500);
  }
}

// ── Form submission ─────────────────────────────────────────────────────

function autoGrowInput() {
  input.style.height = 'auto';
  input.style.height = Math.min(input.scrollHeight, 200) + 'px';
}

function initInput() {
  input.addEventListener('input', function() {
    btn.disabled = !input.value.trim();
    autoGrowInput();
  });
  input.addEventListener('keydown', function(e) {
    if (e.key === 'Enter' && !e.shiftKey && !e.ctrlKey && !e.metaKey && !e.altKey && !e.isComposing) {
      e.preventDefault();
      if (!btn.disabled) {
        if (typeof form.requestSubmit === 'function') form.requestSubmit();
        else form.dispatchEvent(new Event('submit', { cancelable: true, bubbles: true }));
      }
    }
  });
  autoGrowInput();
  input.focus();
}

function initFormSubmit() {
  form.addEventListener('submit', function(e) {
    e.preventDefault();
    var q = input.value.trim();
    if (!q) return;
    input.value = '';
    autoGrowInput();
    btn.disabled = true;

    if (queryInFlight) {
      // Queue the message for later
      var queue = getQueue();
      queue.push(q);
      saveQueue(queue);
      appendUserBubble(q);
      scrollToBottom();
      showToast('Queued (' + queue.length + ')');
    } else {
      sendMessage(q);
    }
  });
}

// ── Check for in-flight query on page load ──────────────────────────────

function checkInFlight() {
  var total = parseInt(chat.dataset.total || '0');
  fetch(form.action + '?check=' + total)
    .then(function(r) { return r.json(); })
    .then(function(data) {
      if (data.ready) {
        // Query finished while page was reloading
        fetch(form.action)
          .then(function(r) { return r.text(); })
          .then(function(html) {
            replaceChat(html);
            onQueryComplete(); // drain queue
          });
      } else if (data.inflight) {
        // Query still running — show thinking and poll
        queryInFlight = true;
        appendThinkingBubble();
        if (data.status) {
          var dots = document.querySelector('.thinking-dots');
          if (dots) dots.textContent = data.status;
        }
        scrollToBottom();
        setButtonLoading(true);
        pollForResponse(form.action, total);
      } else {
        // Nothing in-flight — drain any queued messages from localStorage
        onQueryComplete();
      }
    })
    .catch(function() { onQueryComplete(); });
}

// ── Boot ────────────────────────────────────────────────────────────────

init();
initInfiniteScroll();
initFormSubmit();
checkInFlight();
