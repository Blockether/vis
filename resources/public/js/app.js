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

// ── Session sheet ───────────────────────────────────────────────────────

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

// ── Infinite scroll (load older messages) ───────────────────────────────

var loadingMore = false;

function initInfiniteScroll() {
  chat.addEventListener('scroll', function() {
    if (loadingMore || chat.scrollTop > 50) return;
    var lm = document.getElementById('load-more');
    if (!lm) return;
    loadingMore = true;
    var sid = chat.dataset.session;
    var showing = parseInt(chat.dataset.showing || '8');
    var newOffset = showing + 8;
    var oldH = chat.scrollHeight;
    fetch('/s/' + sid + '?offset=' + newOffset)
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

function appendUserBubble(text) {
  var d = document.createElement('div');
  d.className = 'msg user-msg';
  d.innerHTML = '<div class="bubble user-bubble">' +
    text.replace(/&/g, '&amp;').replace(/</g, '&lt;') + '</div>';
  chat.appendChild(d);
}

function appendThinkingBubble() {
  var d = document.createElement('div');
  d.className = 'msg ai-msg';
  d.id = 'thinking-msg';
  d.innerHTML = '<div class="bubble ai-bubble" style="color:var(--dim)">' +
    '<div id="live-trace"></div>' +
    '<span class="thinking-dots">Thinking<span>.</span><span>.</span><span>.</span></span></div>';
  chat.appendChild(d);
}

function appendErrorBubble(msg) {
  var el = document.getElementById('thinking-msg');
  if (el) el.remove();
  var d = document.createElement('div');
  d.className = 'msg ai-msg';
  d.innerHTML = '<div class="bubble ai-bubble" style="border-color:#c44">' +
    '<div style="color:#c44;font-weight:600;font-size:13px">' +
    msg.replace(/&/g, '&amp;').replace(/</g, '&lt;') + '</div></div>';
  chat.appendChild(d);
  scrollToBottom();
}

function setButtonLoading(loading) {
  if (loading) {
    btn.disabled = true;
    btn.innerHTML = '<div class="spinner"></div>';
    input.disabled = true;
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
    var cls = it["final?"] ? 'iteration iteration-final' : 'iteration';
    html += '<div class="' + cls + '">';
    html += '<div class="iter-header">Iteration ' + (it.iteration + 1);
    if (it["final?"]) html += '<span class="final"> FINAL</span>';
    html += '</div>';
    if (it.thinking) {
      html += '<div class="thinking">' + escHtml(it.thinking) + '</div>';
    }
    if (it.executions) {
      for (var j = 0; j < it.executions.length; j++) {
        var ex = it.executions[j];
        if (ex.error) {
          html += '<div class="exec"><div class="exec-code">' + escHtml(ex.code || '') + '</div>';
          html += '<div class="exec-error">' + escHtml(ex.error) + '</div></div>';
        } else if (ex.code) {
          var badge = extractBadge(ex.code);
          html += '<div class="exec">';
          if (badge) html += '<span class="exec-badge">' + escHtml(badge) + '</span>';
          html += '<div class="exec-code">' + escHtml(ex.code) + '</div>';
          if (ex.result) html += '<div class="exec-result">' + escHtml(ex.result) + '</div>';
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
            })
            .catch(function(e) {
              appendErrorBubble('Connection error. Tap to reload.');
              chat.lastChild.onclick = function() { document.location.reload(); };
              setButtonLoading(false);
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
          // Update status text
          var dots = document.querySelector('.thinking-dots');
          if (dots) {
            if (data.status) {
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
        }
      });
  }, 2000);
}

// ── Form submission ─────────────────────────────────────────────────────

function initInput() {
  input.addEventListener('input', function() {
    btn.disabled = !input.value.trim();
  });
  input.focus();
}

function initFormSubmit() {
  form.addEventListener('submit', function(e) {
    e.preventDefault();
    var q = input.value.trim();
    if (!q || btn.disabled) return;

    appendUserBubble(q);
    appendThinkingBubble();
    scrollToBottom();
    setButtonLoading(true);
    input.value = '';

    var msgCount = parseInt(chat.dataset.total || '0') + 1;
    fetch(form.action, {
      method: 'POST',
      headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
      body: 'q=' + encodeURIComponent(q)
    });

    pollForResponse(form.action, msgCount);
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
          .then(function(html) { replaceChat(html); });
      } else if (data.inflight) {
        // Query still running — show thinking and poll
        appendThinkingBubble();
        if (data.status) {
          var dots = document.querySelector('.thinking-dots');
          if (dots) dots.textContent = data.status;
        }
        scrollToBottom();
        setButtonLoading(true);
        pollForResponse(form.action, total);
      }
    })
    .catch(function() {});
}

// ── Boot ────────────────────────────────────────────────────────────────

init();
initInfiniteScroll();
initFormSubmit();
checkInFlight();
