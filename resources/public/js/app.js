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
    var t = it.querySelector('.thinking');
    if (t) text += 'Thinking: ' + t.textContent + '\n';
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
        clone.querySelectorAll('.iter-header,.thinking').forEach(function(el) { el.remove(); });
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
      var html = '';
      // Context section
      if (data.context && data.context.length > 0) {
        html += '<div class="ctx-section">';
        html += '<div class="ctx-section-header"><i data-lucide="layers"></i> Context <span class="ctx-count">' + data.context.length + '</span></div>';
        data.context.forEach(function(item, i) {
          html += '<div class="ctx-card"><span class="ctx-idx">' + i + '</span>';
          html += '<span class="ctx-text">' + escHtml(String(item)) + '</span></div>';
        });
        html += '</div>';
      }
      // Learnings section
      if (data.learnings && data.learnings.length > 0) {
        html += '<div class="ctx-section">';
        html += '<div class="ctx-section-header"><i data-lucide="lightbulb"></i> Learnings <span class="ctx-count">' + data.learnings.length + '</span></div>';
        data.learnings.forEach(function(l, i) {
          var p = l.priority || 'medium';
          html += '<div class="ctx-card ctx-learning"><span class="ctx-priority ctx-p-' + escHtml(p) + '">' + escHtml(p) + '</span>';
          html += '<span class="ctx-text">' + escHtml(l.text) + '</span></div>';
        });
        html += '</div>';
      }
      // Variables section
      if (data.variables && data.variables.length > 0) {
        html += '<div class="ctx-section">';
        html += '<div class="ctx-section-header"><i data-lucide="variable"></i> Variables <span class="ctx-count">' + data.variables.length + '</span></div>';
        data.variables.forEach(function(v) {
          html += '<div class="ctx-card ctx-var">';
          html += '<span class="ctx-var-name">' + escHtml(v.name) + '</span>';
          html += '<span class="ctx-var-value">' + escHtml(v.value.substring(0, 200)) + (v.value.length > 200 ? '...' : '') + '</span>';
          html += '</div>';
        });
        html += '</div>';
      }
      if (!html) html = '<div class="ctx-empty">Empty. The agent will populate this as it works.</div>';
      if (typeof lucide !== 'undefined') setTimeout(function() { lucide.createIcons(); }, 50);
      document.getElementById('sidebar-content').innerHTML = html;
    })
    .catch(function() { showToast('Failed to load context'); });
}

function closeContext() {
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
    var showing = parseInt(chat.dataset.showing || '8');
    var newOffset = showing + 8;
    var oldH = chat.scrollHeight;
    fetch('/conversations/' + sid + '?offset=' + newOffset)
      .then(function(r) { return r.text(); })
      .then(function(html) {
        var doc = new DOMParser().parseFromString(html, 'text/html');
        var nc = doc.getElementById('chat');
        if (nc) {
          chat.innerHTML = nc.innerHTML;
          chat.dataset.showing = nc.dataset.showing;
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
    text.replace(/&/g, '&amp;').replace(/</g, '&lt;') + '</div>';
  getChatInner().appendChild(d);
}

function appendThinkingBubble() {
  var d = document.createElement('div');
  d.className = 'msg ai-msg';
  d.id = 'thinking-msg';
  d.innerHTML = '<div class="bubble ai-bubble" style="color:var(--dim)">' +
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
      // Update conversation title from server response
      var newTitle = doc.querySelector('.topbar-title');
      var curTitle = document.querySelector('.topbar-title');
      if (newTitle && curTitle) curTitle.textContent = newTitle.textContent;
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
  if (!iterations || !iterations.length) return '';
  var html = '';
  for (var i = 0; i < iterations.length; i++) {
    var it = iterations[i];
    if (it["final?"]) continue; // FINAL iterations hidden — answer shown separately
    html += '<div class="iteration">';
    html += '<div class="iter-header">Iteration ' + (it.iteration + 1);
    html += '</div>';
    if (it.thinking) {
      html += '<div class="thinking md-content">' + escHtml(it.thinking) + '</div>';
    }
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
          if (liveEl && data.iterations && data.iterations.length > 0) {
            liveEl.innerHTML = renderLiveTrace(data.iterations);
            if (data.iterations.length > lastIterCount) {
              scrollToBottom();
              lastIterCount = data.iterations.length;
            }
          }
          // Update status text — hide when final
          var dots = document.querySelector('.thinking-dots');
          if (dots) {
            var hasFinal = data.iterations && data.iterations.some(function(it) { return it["final?"]; });
            if (hasFinal) {
              dots.style.display = 'none';
            } else if (data.status) {
              dots.textContent = data.status;
            } else if (data.iterations && data.iterations.length > 0) {
              dots.textContent = 'Working…';
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
          curTitle.textContent = newTitle.textContent;
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
