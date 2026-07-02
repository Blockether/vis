/* vis web companion - chat interactions (vendored, no externals).
   - prose with data-md re-renders through `marked` (vendored, MIT)
   - code blocks highlight through `Prism` (vendored, MIT; manual mode)
   - composer: autogrow, Enter sends (Shift+Enter breaks)
   - `/` opens slash-command suggestions, `@word` opens the file picker;
     arrows + Enter/Tab select, Esc closes
   - thread follows the newest content while you are near the bottom
   - mic records (AudioWorklet on the audio thread, screen wake lock held
     for the duration), encodes 16-bit PCM WAV, POSTs to /voice (Parakeet) */
(function () {
  function ready(fn) {
    if (document.readyState !== "loading") { fn(); }
    else { document.addEventListener("DOMContentLoaded", fn); }
  }

  /* ── diff fences: marked re-renders a `[data-md]` result body and emits a
     bare `<code class="language-diff">`, but the vendored Prism has NO diff
     grammar — so a re-rendered diff loses the server's df-* colouring. Re-apply
     it CLIENT-SIDE with the same classifier the server uses (render.clj
     `diff-line-kind`), wrapping each line in a df-* span under `.ir-diff`. */
  function diffLineClass(line) {
    if (line.indexOf("+++") === 0 || line.indexOf("---") === 0) { return "df-meta"; }
    if (line.indexOf("@@") === 0) { return "df-hunk"; }
    if (line.length && line.charAt(0) === "+") { return "df-add"; }
    if (line.length && line.charAt(0) === "-") { return "df-del"; }
    return "df-ctx";
  }
  function colorizeDiff(el) {
    var pre = el.closest && el.closest("pre");
    if (pre) { pre.classList.add("ir-diff"); }
    var lines = el.textContent.split("\n");
    var frag = document.createDocumentFragment();
    lines.forEach(function (line) {
      var span = document.createElement("span");
      span.className = diffLineClass(line);
      span.textContent = line === "" ? " " : line;
      frag.appendChild(span);
    });
    el.textContent = "";
    el.appendChild(frag);
  }

  /* ── syntax highlight: every `language-*` code block (trace
     cells, IR fences, marked output) through the vendored Prism */
  function highlightCode(root) {
    /* Diff fences first: colour them ourselves and mark them done so the
       Prism pass below skips them (Prism has no diff grammar). */
    (root || document).querySelectorAll('code.language-diff:not([data-hl-done])')
      .forEach(function (el) {
        el.setAttribute("data-hl-done", "1");
        try { colorizeDiff(el); } catch (e) { /* plain text is fine */ }
      });
    if (typeof Prism === "undefined" || !Prism.highlightElement) { return; }
    (root || document).querySelectorAll('code[class*="language-"]:not([data-hl-done])')
      .forEach(function (el) {
        el.setAttribute("data-hl-done", "1");
        try { Prism.highlightElement(el); } catch (e) { /* plain text is fine */ }
      });
  }

  /* ── markdown: render EVERY [data-md] through marked (bubbles AND
     Context-rail fact contents — the turn_<N> fact is a markdown blob) */


  /* ── fold long pasted code in USER bubbles to a head+tail peek with a
     clickable toggle. Mirrors the TUI's collapsed paste preview: a big wall
     someone pasted stays readable (first FOLD_HEAD + last FOLD_TAIL lines)
     without blowing the transcript open, and one click reveals the full text.
     Assistant / trace code is left untouched (no `.b-user` ancestor). */
  var FOLD_HEAD = 12, FOLD_TAIL = 6, FOLD_MIN = FOLD_HEAD + FOLD_TAIL + 4;
  var FOLD_PROSE_MAX_PX = 360;
  function foldCode(el) {
    if (!el.closest || !el.closest(".b-user")) { return; }
    var foldedAny = false;
    el.querySelectorAll("pre:not([data-folded])").forEach(function (pre) {
      pre.setAttribute("data-folded", "1");
      var code = pre.querySelector("code") || pre;
      var full = code.textContent;
      var lines = full.split("\n");
      if (lines.length < FOLD_MIN) { return; }
      foldedAny = true;
      var hidden = lines.length - FOLD_HEAD - FOLD_TAIL;
      var preview = lines.slice(0, FOLD_HEAD).join("\n") +
        "\n⋯ " + hidden + " more lines ⋯\n" +
        lines.slice(lines.length - FOLD_TAIL).join("\n");
      pre.setAttribute("data-collapsed", "1");
      code.textContent = preview;
      var btn = document.createElement("button");
      btn.type = "button";
      btn.className = "code-fold-toggle";
      var label = function () {
        return pre.getAttribute("data-collapsed") === "1"
          ? ("⋯ show " + hidden + " hidden lines")
          : "▲ collapse";
      };
      btn.textContent = label();
      btn.addEventListener("click", function () {
        var collapsed = pre.getAttribute("data-collapsed") === "1";
        code.textContent = collapsed ? full : preview;
        pre.setAttribute("data-collapsed", collapsed ? "0" : "1");
        btn.textContent = label();
        code.removeAttribute("data-hl-done");
        highlightCode(pre);
      });
      pre.parentNode.insertBefore(btn, pre.nextSibling);
    });
    /* Raw (un-fenced) pastes render as prose, not <pre>. If the bubble is
       still a tall wall and nothing got folded above, cap its height with a
       fade and a one-click reveal so the transcript stays navigable. */
    if (!foldedAny && !el.hasAttribute("data-prose-folded")) {
      el.setAttribute("data-prose-folded", "1");
      if (el.scrollHeight > FOLD_PROSE_MAX_PX + 80) {
        el.classList.add("prose-collapsed");
        var pbtn = document.createElement("button");
        pbtn.type = "button";
        pbtn.className = "code-fold-toggle";
        var plabel = function () {
          return el.classList.contains("prose-collapsed") ? "⋯ show full message" : "▲ collapse";
        };
        pbtn.textContent = plabel();
        pbtn.addEventListener("click", function () {
          el.classList.toggle("prose-collapsed");
          pbtn.textContent = plabel();
        });
        el.parentNode.insertBefore(pbtn, el.nextSibling);
      }
    }
  }



  function renderProse(root) {
    if (typeof marked !== "undefined") {
      (root || document).querySelectorAll("[data-md]:not([data-md-done])")
        .forEach(function (el) {
          el.setAttribute("data-md-done", "1");
          try {
            el.innerHTML = marked.parse(el.getAttribute("data-md"), { breaks: true });
            foldCode(el);

          } catch (e) { /* keep the server-rendered fallback */ }
        });
    }

    highlightCode(root);
  }

  /* ── wav encode for voice ─────────────────────────────────────────── */
  function encodeWav(chunks, sampleRate) {
    var len = 0;
    chunks.forEach(function (c) { len += c.length; });
    var pcm = new Int16Array(len), o = 0;
    chunks.forEach(function (c) {
      for (var i = 0; i < c.length; i++) {
        var s = Math.max(-1, Math.min(1, c[i]));
        pcm[o++] = s < 0 ? s * 0x8000 : s * 0x7FFF;
      }
    });
    var buf = new ArrayBuffer(44 + pcm.length * 2), v = new DataView(buf);
    function ws(off, s) { for (var i = 0; i < s.length; i++) { v.setUint8(off + i, s.charCodeAt(i)); } }
    ws(0, "RIFF"); v.setUint32(4, 36 + pcm.length * 2, true); ws(8, "WAVE");
    ws(12, "fmt "); v.setUint32(16, 16, true); v.setUint16(20, 1, true);
    v.setUint16(22, 1, true); v.setUint32(24, sampleRate, true);
    v.setUint32(28, sampleRate * 2, true); v.setUint16(32, 2, true);
    v.setUint16(34, 16, true); ws(36, "data"); v.setUint32(40, pcm.length * 2, true);
    new Int16Array(buf, 44).set(pcm);
    return new Blob([buf], { type: "audio/wav" });
  }

  ready(function () {
    renderProse(document);

    /* hideable rails (desktop) / off-canvas drawers (mobile, <=68rem) */
    var app = document.querySelector(".app");
    var mobile = window.matchMedia("(max-width: 68rem)");
    function closeDrawers() {
      if (app) { app.classList.remove("open-left", "open-right"); }
    }
    /* On mobile the SAME toggle button opens/closes an overlay drawer (the
       desktop hide-* default is "shown", which on a phone would mean "drawer
       open on load" — so the two need different state classes). */
    function wireToggle(btnSel, deskCls, openCls, key) {
      var btn = document.querySelector(btnSel);
      if (!btn || !app) { return; }
      if (localStorage.getItem(key) === "1") { app.classList.add(deskCls); }
      btn.addEventListener("click", function (e) {
        e.stopPropagation();
        if (mobile.matches) {
          var other = openCls === "open-left" ? "open-right" : "open-left";
          app.classList.remove(other);          /* one drawer at a time */
          app.classList.toggle(openCls);
        } else {
          app.classList.toggle(deskCls);
          localStorage.setItem(key, app.classList.contains(deskCls) ? "1" : "0");
        }
      });
    }
    wireToggle("#toggle-left", "hide-left", "open-left", "vis.hideLeft");
    wireToggle("#toggle-right", "hide-right", "open-right", "vis.hideRight");
    /* explicit closers: the context drawer's X (it covers the scrim full-width),
       and the sidebar's Settings/Providers buttons (close the drawer so the modal
       — which opens into #modal — shows over the chat, not behind the sidebar) */
    document.addEventListener("click", function (e) {
      if (e.target.closest("[data-close-drawer], .side-foot-btn")) { closeDrawers(); }
    });
    /* close an open mobile drawer on a tap outside it (scrim/content) or on Esc */
    document.addEventListener("click", function (e) {
      if (!app) { return; }
      if (!app.classList.contains("open-left") && !app.classList.contains("open-right")) { return; }
      if (e.target.closest(".sidebar, .rail, #toggle-left, #toggle-right")) { return; }
      closeDrawers();
    });
    document.addEventListener("keydown", function (e) { if (e.key === "Escape") { closeDrawers(); } });
    /* a desktop->mobile resize (or back) shouldn't leave a stuck drawer */
    mobile.addEventListener("change", closeDrawers);

    /* modal close: the X button, or a click on the backdrop itself */
    document.body.addEventListener("click", function (e) {
      var closer = e.target.closest("[data-close-modal]");
      var modal = document.querySelector("#modal");
      if (!modal) { return; }
      if (closer && (closer.dataset.closeModal === "x" || closer === e.target)) {
        modal.innerHTML = "";
      }
    });

    var composer = document.querySelector(".composer textarea");
    var suggest = document.querySelector("#suggest");
    var form = document.querySelector("form.composer");
    var slashCache = null;
    /* monotonic token: every keystroke bumps it; async suggest responses only
       render if they're still the latest — kills out-of-order fetch races
       (e.g. a stale /ui/slash landing after the session/file list). */
    var suggestSeq = 0;

    /* ── suggestion popup (slash commands + @files) ─────────────────── */
    var items = [], active = -1, mode = null, wordStart = 0;
    var overlay = document.querySelector(".composer-overlay");
    var pickedFiles = [];
    function escapeHtml(s) {
      return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
    }
    /* mirror the textarea text into the backdrop, wrapping each picked file
       path in a <mark> so it reads as a highlighted token. Picked paths that
       were edited/deleted out of the text drop off automatically. */
    function renderOverlay() {
      if (!overlay || !composer) { return; }
      var text = composer.value;
      pickedFiles = pickedFiles.filter(function (p) { return text.indexOf(p) !== -1; });
      if (!pickedFiles.length) { overlay.innerHTML = ""; return; }
      var sorted = pickedFiles.slice().sort(function (a, b) { return b.length - a.length; });
      var html = "", i = 0;
      while (i < text.length) {
        var hit = null;
        for (var k = 0; k < sorted.length; k++) {
          if (text.substr(i, sorted[k].length) === sorted[k]) { hit = sorted[k]; break; }
        }
        if (hit) {
          html += '<mark class="file-token">' + escapeHtml(hit) + "</mark>";
          i += hit.length;
        } else {
          html += escapeHtml(text.charAt(i));
          i += 1;
        }
      }
      overlay.innerHTML = html;
      overlay.scrollTop = composer.scrollTop;
    }
    function hideSuggest() {
      if (suggest) { suggest.hidden = true; }
      items = []; active = -1; mode = null;
    }
    function showSuggest(list, kind) {
      if (!suggest) { return; }
      items = list.slice(0, 8); mode = kind; active = items.length ? 0 : -1;
      if (!items.length) { hideSuggest(); return; }
      suggest.innerHTML = "";
      items.forEach(function (it, i) {
        var row = document.createElement("div");
        row.className = "suggest-row" + (kind === "file" ? " suggest-file" : "") + (i === active ? " active" : "");
        var name = document.createElement("span");
        name.className = "suggest-name";
        name.textContent = it.name;
        row.appendChild(name);
        if (kind === "file") {
          /* table-style row (like the TUI picker): path on the left, then a
             right-aligned size · age column; git status tints the path. */
          if (it.status && it.status !== "clean") { row.dataset.status = it.status; }
          var meta = [it.size, it.age].filter(Boolean).join("  ");
          if (meta) {
            var m = document.createElement("span");
            m.className = "suggest-meta";
            m.textContent = meta;
            row.appendChild(m);
          }
        } else if (it.doc) {
          var doc = document.createElement("span");
          doc.className = "suggest-doc";
          doc.textContent = it.doc;
          row.appendChild(doc);
        }
        row.addEventListener("mousedown", function (e) {
          e.preventDefault(); pick(i);
        });
        suggest.appendChild(row);
      });
      suggest.hidden = false;
    }
    function highlight() {
      if (!suggest) { return; }
      Array.prototype.forEach.call(suggest.children, function (row, i) {
        row.classList.toggle("active", i === active);
        if (i === active && row.scrollIntoView) { row.scrollIntoView({ block: "nearest" }); }
      });
    }
    function pick(i) {
      var it = items[i];
      if (!it || !composer) { return; }
      if (mode === "session") {
        if (it.nav) { window.location.assign(it.nav); }
        return;
      }
      var caretPos = null;
      if (mode === "slash") {
        composer.value = it.name + " ";
      } else if (mode === "file") {
        composer.value = composer.value.slice(0, wordStart) + it.name + " " +
          composer.value.slice(composer.selectionStart);
        caretPos = wordStart + it.name.length + 1;
        if (pickedFiles.indexOf(it.name) === -1) { pickedFiles.push(it.name); }
      }
      hideSuggest();
      composer.focus();
      if (caretPos !== null) { composer.setSelectionRange(caretPos, caretPos); }
      composer.dispatchEvent(new Event("input"));
    }
    function updateSuggest() {
      if (!composer) { return; }
      var v = composer.value;
      var caret = composer.selectionStart;
      var seq = ++suggestSeq;
      var fresh = function () { return seq === suggestSeq; };
      /* /switch-session [query] → inline session selector (arrow-navigable,
         like the slash + @ pickers); picking one navigates to it. Checked
         BEFORE the bare-slash branch so the trailing space routes here. */
      var sw = v.match(/^\/switch-session(?:\s+(.*))?$/i);
      if (sw) {
        if (mode === "slash") { hideSuggest(); }
        fetch("/ui/sessions/list?q=" + encodeURIComponent(sw[1] || ""))
          .then(function (r) { return r.json(); })
          .then(function (list) { if (fresh()) { showSuggest(list, "session"); } })
          .catch(hideSuggest);
        return;
      }
      if (v.charAt(0) === "/" && v.indexOf(" ") === -1) {
        var q = v.slice(1).toLowerCase();
        var apply = function (cmds) {
          if (!fresh()) { return; }
          showSuggest(cmds.filter(function (c) {
            return c.name.toLowerCase().indexOf("/" + q) === 0 || q === "";
          }), "slash");
        };
        if (slashCache) { apply(slashCache); }
        else {
          fetch("/ui/slash").then(function (r) { return r.json(); })
            .then(function (cmds) { slashCache = cmds; apply(cmds); })
            .catch(hideSuggest);
        }
        return;
      }
      /* @<query> at the caret → shared suggest service (kind=file). The @
         must begin a word (start of input or after whitespace); `@@` escapes
         to a literal @ (never opens the picker). Picking replaces the @token
         with the chosen path. This is the ONLY add-file affordance now. */
      if (form && form.dataset.filesUrl) {
        var head = v.slice(0, caret);
        var m = head.match(/(?:^|\s)@(?!@)(\S*)$/);
        if (m) {
          var query = m[1];
          wordStart = caret - query.length - 1; /* index of the '@' */
          fetch(form.dataset.filesUrl + "?kind=file&q=" + encodeURIComponent(query))
            .then(function (r) { return r.json(); })
            .then(function (rows) {
              /* rows are {name, size, age, status} — the same fuzzy index +
                 metadata the TUI file-picker table shows. */
              if (fresh()) { showSuggest(rows, "file"); }
            })
            .catch(hideSuggest);
          return;
        }
      }
      hideSuggest();
    }

    if (composer) {
      /* per-session draft: a session switch is a full page navigation, so
         without this whatever was typed vanishes. Keyed by the sid in the
         URL; saved on every keystroke, restored on load, cleared on send. */
      var sidMatch = location.pathname.match(/\/ui\/session\/([0-9a-fA-F-]{36})/);
      var draftKey = sidMatch ? "vis.draft." + sidMatch[1] : null;
      var saveDraft = function () {
        if (!draftKey) { return; }
        try {
          if (composer.value) { localStorage.setItem(draftKey, composer.value); }
          else { localStorage.removeItem(draftKey); }
        } catch (e) { /* storage full/blocked - typing still works */ }
      };
      var clearDraft = function () {
        if (draftKey) { try { localStorage.removeItem(draftKey); } catch (e) {} }
      };
      if (draftKey && !composer.value) {
        try {
          var saved = localStorage.getItem(draftKey);
          if (saved) { composer.value = saved; }
        } catch (e) { /* ignore */ }
      }
      /* send is grayed out while the composer is empty — no more
         "request must not be blank" round trips */
      var sendBtn = document.querySelector(".composer .send");
      var syncSend = function () {
        if (sendBtn) { sendBtn.disabled = composer.value.trim().length === 0; }
      };
      var grow = function () {
        composer.style.height = "auto";
        composer.style.height = Math.min(composer.scrollHeight, 200) + "px";
      };
      /* The first grow() (below) can run before the Inter web font has
         loaded; scrollHeight is then measured with fallback-font metrics
         and the baked px height drifts off the 34px button line — the
         "placeholder misaligned until I refresh" bug. Re-measure once the
         fonts settle (and after late reflows) so it self-corrects without
         a refresh. */
      if (document.fonts && document.fonts.ready) {
        document.fonts.ready.then(grow);
      }
      composer.addEventListener("input", function () { grow(); updateSuggest(); syncSend(); saveDraft(); renderOverlay(); });
      composer.addEventListener("scroll", function () { if (overlay) { overlay.scrollTop = composer.scrollTop; } });
      syncSend();
      composer.addEventListener("blur", function () {
        setTimeout(hideSuggest, 150);
      });
      composer.addEventListener("keydown", function (e) {
        if (!suggest || suggest.hidden) {
          if (e.key === "Enter" && !e.shiftKey) {
            e.preventDefault();
            if (composer.value.trim().length > 0) { composer.form.requestSubmit(); }
          }
          return;
        }
        if (e.key === "ArrowDown") { e.preventDefault(); active = (active + 1) % items.length; highlight(); }
        else if (e.key === "ArrowUp") { e.preventDefault(); active = (active - 1 + items.length) % items.length; highlight(); }
        else if (e.key === "Enter" || e.key === "Tab") { e.preventDefault(); pick(active); }
        else if (e.key === "Escape") { hideSuggest(); }
      });
      document.body.addEventListener("htmx:afterRequest", function (e) {
        if (e.detail.successful && e.target.classList.contains("composer")) {
          composer.style.height = "auto";
          composer.focus();
          hideSuggest();
          syncSend();
          clearDraft();
        }
      });
      composer.focus();
      grow();
    }

    /* ── markdown + highlighting on ANY new content (thread bubbles +
       trace cells); observe the whole app root */
    var appRoot = document.querySelector(".app") || document.body;
    new MutationObserver(function () { renderProse(appRoot); })
      .observe(appRoot, { childList: true, subtree: true });

    /* ── lazy trace fallback ─────────────────────────────────────────────
       htmx normally loads a trace on the native <details> toggle. Some
       proxy/browser combinations (notably quick Cloudflare tunnels on
       mobile) miss that non-bubbling event, leaving the placeholder open
       forever. Delegate a capture-phase fallback: when a Work disclosure
       opens and still contains the loading stub, fetch the same fragment and
       replace `.trace-body` ourselves. */
    document.addEventListener("toggle", function (e) {
      var details = e.target;
      if (!details || !details.matches || !details.matches("details.trace") || !details.open) { return; }
      var body = details.querySelector(".trace-body");
      var url = details.getAttribute("hx-get") || details.dataset.traceUrl;
      if (!body || !url || body.dataset.traceLoaded === "1" || body.dataset.traceLoading === "1") { return; }
      if (!body.querySelector(".block-loading")) { return; }
      body.dataset.traceLoading = "1";
      fetch(url, { headers: { "HX-Request": "true" } })
        .then(function (r) { if (!r.ok) { throw new Error("trace fetch failed"); } return r.text(); })
        .then(function (html) {
          var tmp = document.createElement("div");
          tmp.innerHTML = html;
          var next = tmp.querySelector(".trace-body");
          if (next) {
            body.replaceWith(next);
            if (next.querySelector(".block-loading")) {
              delete next.dataset.traceLoading;
            } else {
              next.dataset.traceLoaded = "1";
            }
            renderProse(next);
          }
        })
        .catch(function () { delete body.dataset.traceLoading; });
    }, true);

    /* ── thread follow ───────────────────────────────────────────────── */
    var thread = document.querySelector(".thread");
    if (thread) {
      var jumpBottom = document.querySelector("#jump-bottom");
      var follow = true;
      var setFollow = function (next) {
        follow = next;
        if (jumpBottom) { jumpBottom.hidden = follow; }
      };
      var scrollToBottom = function () { thread.scrollTop = thread.scrollHeight; };
      thread.addEventListener("scroll", function () {
        setFollow(thread.scrollHeight - thread.scrollTop - thread.clientHeight < 160);
      });
      if (jumpBottom) {
        jumpBottom.addEventListener("click", function () {
          setFollow(true);
          scrollToBottom();
          requestAnimationFrame(scrollToBottom);
        });
      }
      /* Auto-follow the bottom, but COALESCE bursts and read scrollHeight
         only AFTER layout (rAF). Reading it synchronously inside the
         mutation — while htmx is mid-swap and an element is added but not
         yet laid out — yanked scrollTop to a transient height and could
         flash a blank viewport during streaming. characterData is dropped:
         streamed content arrives as childList swaps, and force-scrolling on
         every typed character was pure jank. */
      var followPending = false;
      var onMutate = function () {
        if (!follow || followPending) { return; }
        followPending = true;
        requestAnimationFrame(function () {
          followPending = false;
          if (follow) { thread.scrollTop = thread.scrollHeight; }
        });
      };
      new MutationObserver(onMutate).observe(thread, { childList: true, subtree: true });
      onMutate();

      /* Initial landing: open at the NEWEST message. A single rAF scroll
         races font-loading + lazy reflow, so a mid-stream refresh stranded
         the view ABOVE the bottom (you had to scroll way down). Re-snap to
         the bottom after fonts are ready and after window load, but ONLY
         while still following (don't yank a user who already scrolled up). */
      if ("scrollRestoration" in history) { history.scrollRestoration = "manual"; }
      var snapBottom = function () { if (follow) { scrollToBottom(); } };
      requestAnimationFrame(snapBottom);
      window.addEventListener("load", snapBottom);
      window.addEventListener("pageshow", snapBottom);
      setTimeout(snapBottom, 80);
      setTimeout(snapBottom, 250);
      if (document.fonts && document.fonts.ready) {
        document.fonts.ready.then(function () { requestAnimationFrame(snapBottom); });
      }

      /* ── infinite scroll-up: hold the viewport when older turns prepend.
         htmx outerHTML-swaps the top `.load-older` sentinel for a batch of
         older turns + a fresh sentinel; keep the same content under the eye
         by preserving distance-from-bottom across the swap. */
      var olderRem = null;
      document.body.addEventListener("htmx:beforeSwap", function (e) {
        var t = e.detail && e.detail.target;
        if (t && t.classList && t.classList.contains("load-older")) {
          olderRem = thread.scrollHeight - thread.scrollTop;
        }
      });
      document.body.addEventListener("htmx:afterSwap", function () {
        if (olderRem != null) {
          thread.scrollTop = thread.scrollHeight - olderRem;
          olderRem = null;
        }
      });
    }



    /* ── sidebar select-mode: bulk session delete. Delegated on document
       so it SURVIVES the SSE innerHTML re-render of the drawer. */
    document.addEventListener("click", function (e) {
      var t = e.target.closest ? e.target.closest("[data-select-toggle]") : null;
      if (!t) { return; }
      var aside = t.closest(".sidebar");
      if (!aside) { return; }
      aside.classList.toggle("select-mode");
      if (!aside.classList.contains("select-mode")) {
        aside.querySelectorAll(".side-check:checked")
          .forEach(function (c) { c.checked = false; });
        syncSideBulk(aside);
      }
    });
    /* Keep the bulk-delete button's enabled-state AND label in sync with the
       checked count ("Delete 3 selected"); 0 disables it. One helper, called
       from every place that flips a checkbox. */
    function syncSideBulk(aside) {
      if (!aside) { return; }
      var btn = aside.querySelector(".side-bulk-del");
      if (!btn) { return; }
      var n = aside.querySelectorAll(".side-check:checked").length;
      btn.disabled = n === 0;
      btn.textContent = n > 0 ? ("Delete " + n + " selected") : "Delete selected";
    }
    document.addEventListener("change", function (e) {
      if (!e.target.classList || !e.target.classList.contains("side-check")) { return; }
      syncSideBulk(e.target.closest(".sidebar"));
    });
    /* in select-mode, clicking the ROW toggles its checkbox instead of
       navigating - no need to aim for the tiny checkbox itself */
    document.addEventListener("click", function (e) {
      var row = e.target.closest ? e.target.closest(".side-row") : null;
      if (!row) { return; }
      var aside = row.closest(".sidebar.select-mode");
      if (!aside) { return; }
      e.preventDefault();
      var item = row.closest(".side-item");
      var check = item && item.querySelector(".side-check");
      if (!check) { return; }
      check.checked = !check.checked;
      syncSideBulk(aside);
    });

    /* ── click the header title to rename ─────────────────────────────
       Prefill the composer with `/rename <current title>` and focus it;
       submitting routes through the engine's cross-channel /rename slash
       (TUI/web/Telegram share it). Delegated on document so it survives the
       SSE innerHTML re-render of the header bar. */
    document.addEventListener("click", function (e) {
      var name = e.target.closest ? e.target.closest(".bar-name") : null;
      if (!name || !composer) { return; }
      e.preventDefault();
      var cur = name.getAttribute("data-rename") || "";
      composer.value = "/rename " + cur;
      composer.focus();
      var end = composer.value.length;
      try { composer.setSelectionRange(end, end); } catch (_) {}
      composer.dispatchEvent(new Event("input", { bubbles: true }));
    });

    /* ── voice: live gold waveform + timer + cancel(✕)/accept(✓) ─────── */
    var mic = document.querySelector(".composer .mic");
    if (mic && composer) {
      var rec = null;
      var AC = window.AudioContext || window.webkitAudioContext;
      /* title tooltips are invisible on touch — surface mic problems as a
         small inline notice above the composer instead of doing nothing */
      var micNote = function (msg, cls) {
        var old = composer.form.querySelector(".mic-note");
        if (old) { old.parentNode.removeChild(old); }
        var note = document.createElement("div");
        note.className = "mic-note" + (cls ? " " + cls : "");
        note.textContent = msg;
        composer.form.insertBefore(note, composer);
        setTimeout(function () {
          if (note.parentNode) { note.parentNode.removeChild(note); }
        }, 5000);
      };
      var BAR_COUNT = 28;
      function fmtTime(ms) {
        var s = Math.floor(ms / 1000);
        return Math.floor(s / 60) + ":" + String(s % 60).padStart(2, "0");
      }
      function buildRecUi(capturedMs) {
        var wave = document.createElement("div");
        wave.className = "wave";
        var bars = [];
        for (var i = 0; i < BAR_COUNT; i++) {
          var b = document.createElement("span");
          wave.appendChild(b);
          bars.push(b);
        }
        var time = document.createElement("span");
        time.className = "rec-time";
        time.textContent = "0:00";
        var cancel = document.createElement("button");
        cancel.type = "button"; cancel.className = "rec-cancel";
        cancel.setAttribute("aria-label", "Discard recording");
        cancel.innerHTML = '<svg class="icon"><use href="/ui/icons.svg#x"/></svg>';
        var accept = document.createElement("button");
        accept.type = "button"; accept.className = "rec-accept";
        accept.setAttribute("aria-label", "Use recording");
        accept.innerHTML = '<svg class="icon"><use href="/ui/icons.svg#check"/></svg>';
        var form = composer.form;
        form.insertBefore(time, composer);
        form.insertBefore(wave, composer);
        form.appendChild(cancel);
        form.appendChild(accept);
        /* the clock counts CAPTURED AUDIO, not wall time — when the
           browser pauses capture (screen lock, app switch) the timer
           visibly freezes instead of lying about what was recorded */
        var timer = setInterval(function () {
          time.textContent = fmtTime(capturedMs());
        }, 250);
        return { wave: wave, bars: bars, levels: [],
                 time: time, cancel: cancel, accept: accept,
                 remove: function () {
                   clearInterval(timer);
                   [time, wave, cancel, accept].forEach(function (el) {
                     if (el.parentNode) { el.parentNode.removeChild(el); }
                   });
                 } };
      }
      /* The Parakeet voice model (~465MB) is downloaded on first use. Check it
         BEFORE recording: if it's missing, start a background download and show
         progress here, instead of silently blocking the transcription later (or
         crashing on a half-downloaded model). onReady(justFinished) fires when
         the model is installed; justFinished=true means we just downloaded it,
         so we don't auto-start recording — the user taps the mic again. */
      function ensureVoiceModel(onReady, onFail) {
        var modelUrl = mic.dataset.voiceUrl + "/model";
        var poll = null;
        var show = function (pct) { micNote("Downloading voice model… " + (pct || 0) + "%", "info"); };
        var fail = function (msg) {
          if (poll) { clearInterval(poll); }
          mic.classList.remove("model-loading");
          onFail(msg);
        };
        fetch(modelUrl).then(function (r) { return r.json(); }).then(function (d) {
          if (d.status === "ready") { onReady(false); return; }
          if (d.status === "unavailable") { fail(d.error || "not available"); return; }
          mic.classList.add("model-loading");
          show(d.progress);
          fetch(modelUrl, { method: "POST" })
            .then(function (r) { return r.json(); })
            .then(function (s) {
              if (s.status === "failed") { fail(s.error || "download failed"); return; }
              show(s.progress);
              poll = setInterval(function () {
                fetch(modelUrl).then(function (r) { return r.json(); }).then(function (p) {
                  if (p.status === "ready") {
                    clearInterval(poll); mic.classList.remove("model-loading");
                    micNote("Voice model ready — tap the mic to record.", "ok");
                    onReady(true);
                  } else if (p.status === "failed") { fail(p.error || "download failed"); }
                  else { show(p.progress); }
                }).catch(function () { fail("lost connection"); });
              }, 1500);
            }).catch(function () { fail("could not start download"); });
        }).catch(function () { fail("could not reach the server"); });
      }
      /* Capture on the AUDIO RENDERING THREAD via AudioWorklet: the
         deprecated ScriptProcessorNode fires on the MAIN thread, so DOM
         work (SSE swaps, markdown highlight) or a paused page silently
         DROPPED microphone buffers — a 2-minute dictation arrived as
         ~30s of audio. The worklet batches 4096 samples (~85ms at
         48kHz, the old ScriptProcessor cadence). Fallback stays for
         ancient WebViews only. Resolves to {stop}. */
      function startCapture(ac, src, onChunk) {
        if (ac.audioWorklet && window.AudioWorkletNode) {
          return ac.audioWorklet.addModule("/ui/js/rec-worklet.js").then(function () {
            var node = new AudioWorkletNode(ac, "vis-capture", {
              numberOfInputs: 1, numberOfOutputs: 1, outputChannelCount: [1]
            });
            node.port.onmessage = function (ev) { onChunk(ev.data); };
            src.connect(node); node.connect(ac.destination);
            return { stop: function () { node.port.onmessage = null; node.disconnect(); } };
          });
        }
        var proc = ac.createScriptProcessor(4096, 1, 1);
        proc.onaudioprocess = function (e) {
          onChunk(new Float32Array(e.inputBuffer.getChannelData(0)));
        };
        src.connect(proc); proc.connect(ac.destination);
        return Promise.resolve({
          stop: function () { proc.onaudioprocess = null; proc.disconnect(); }
        });
      }
      function startRecording() {
        navigator.mediaDevices.getUserMedia({ audio: true }).then(function (stream) {
          var ac = new AC();
          /* iOS Safari creates AudioContexts suspended — without resume()
             capture never starts and the recording stays empty */
          if (ac.state === "suspended") { ac.resume(); }
          var src = ac.createMediaStreamSource(stream);
          var chunks = [];
          var captured = 0; /* samples actually landed in chunks */
          var startedAt = Date.now();
          var cap = null;
          var wakeLock = null;
          /* Screen Wake Lock: phones auto-lock mid-dictation (iPhone
             default: 30s) and a locked screen SUSPENDS the page — capture
             stops while the speaker keeps talking. Hold a wake lock for
             the whole recording so auto-lock never fires; re-acquire on
             return because the browser releases it whenever the page
             hides. (A page CANNOT capture through an actual lock — this
             prevents the lock instead.) */
          function acquireWakeLock() {
            if (navigator.wakeLock && navigator.wakeLock.request) {
              navigator.wakeLock.request("screen")
                .then(function (l) { wakeLock = l; })
                .catch(function () { /* denied (low battery, iframe): capture still works */ });
            }
          }
          acquireWakeLock();
          var warnedPause = false;
          function capturePaused() {
            if (!warnedPause && rec) {
              warnedPause = true;
              micNote("Recording was paused by the browser (screen lock or app switch) — the paused stretch is not captured.", "info");
            }
          }
          /* a manual lock / app switch still suspends the context: warn
             once and resume the moment the browser allows it */
          ac.onstatechange = function () {
            if (rec && ac.state !== "running") {
              capturePaused();
              if (ac.resume) { ac.resume().catch(function () {}); }
            }
          };
          function onVisibility() {
            if (document.visibilityState === "visible") {
              acquireWakeLock();
              if (ac.state !== "running" && ac.resume) { ac.resume().catch(function () {}); }
            }
          }
          document.addEventListener("visibilitychange", onVisibility);
          var ui = buildRecUi(function () {
            return (captured / ac.sampleRate) * 1000;
          });
          composer.form.classList.add("recording");
          mic.classList.add("recording");
          function onChunk(data) {
            chunks.push(data);
            captured += data.length;
            /* rolling RMS -> the bars ride the voice */
            var sum = 0;
            for (var i = 0; i < data.length; i += 8) { sum += data[i] * data[i]; }
            var rms = Math.sqrt(sum / (data.length / 8));
            ui.levels.push(rms);
            if (ui.levels.length > BAR_COUNT) { ui.levels.shift(); }
            for (var j = 0; j < BAR_COUNT; j++) {
              var lv = ui.levels[ui.levels.length - BAR_COUNT + j] || 0;
              ui.bars[j].style.height =
                Math.max(4, Math.min(30, 4 + lv * 220)) + "px";
            }
          }
          function teardown() {
            if (cap) { cap.stop(); cap = null; }
            document.removeEventListener("visibilitychange", onVisibility);
            ac.onstatechange = null;
            if (wakeLock) { wakeLock.release().catch(function () {}); wakeLock = null; }
            src.disconnect();
            stream.getTracks().forEach(function (t) { t.stop(); });
            var rate = ac.sampleRate;
            ac.close();
            mic.classList.remove("recording");
            composer.form.classList.remove("recording");
            ui.remove();
            rec = null;
            return rate;
          }
          startCapture(ac, src, onChunk).then(function (c) {
            /* cancelled before the worklet module finished loading */
            if (rec) { cap = c; } else { c.stop(); }
          }).catch(function () {
            micNote("Voice capture failed to start — try again.");
            if (rec) { rec.stop(); }
          });
          rec = { stop: teardown };
          ui.cancel.addEventListener("click", function () {
            teardown(); /* discard - no transcription */
            composer.focus();
          });
          ui.accept.addEventListener("click", function () {
            var rate = teardown();
            /* wall clock vs captured audio: a big gap means the browser
               paused capture — say how much is missing instead of
               returning a mysteriously short transcript */
            var lostMs = (Date.now() - startedAt) - (captured / rate) * 1000;
            if (lostMs > 3000) {
              micNote("~" + Math.round(lostMs / 1000) + "s of speech was not captured (the browser paused recording) — transcribing the rest.", "info");
            }
            mic.disabled = true;
            /* the model is already installed by here (ensureVoiceModel gates
               recording), but transcription still takes a moment — say so */
            var busy = document.createElement("div");
            busy.className = "transcribing";
            busy.innerHTML =
              '<svg class="icon spin"><use href="/ui/icons.svg#loader"/></svg>' +
              '<span>transcribing…</span>';
            composer.form.insertBefore(busy, composer);
            composer.form.classList.add("transcribing-on");
            var done = function () {
              mic.disabled = false;
              composer.form.classList.remove("transcribing-on");
              if (busy.parentNode) { busy.parentNode.removeChild(busy); }
            };
            fetch(mic.dataset.voiceUrl, { method: "POST", body: encodeWav(chunks, rate) })
              .then(function (r) { return r.json(); })
              .then(function (d) {
                done();
                if (d.text) {
                  composer.value = (composer.value ? composer.value + " " : "") + d.text;
                  composer.dispatchEvent(new Event("input"));
                  composer.focus();
                } else if (d.error) {
                  mic.title = d.error;
                  mic.classList.add("mic-error");
                }
              })
              .catch(done);
          });
        }).catch(function (err) {
          micNote("Microphone blocked: " +
            (err && err.name === "NotAllowedError" ? "permission denied — allow mic access in the browser settings."
             : err && err.name ? err.name : "unavailable."));
        });
      }
      mic.addEventListener("click", function () {
        if (rec) { return; }
        if (mic.classList.contains("model-loading")) { return; } /* download in flight */
        /* getUserMedia exists only in SECURE contexts — over plain http:// from
           a phone, mediaDevices is undefined; say so instead of doing nothing. */
        if (!navigator.mediaDevices || !navigator.mediaDevices.getUserMedia || !AC) {
          micNote("Microphone needs HTTPS (or localhost) — open the page over a secure URL.");
          return;
        }
        ensureVoiceModel(
          function (justFinished) { if (!justFinished) { startRecording(); } },
          function (err) { micNote("Voice model: " + err); }
        );
      });
    }
  });
})();

/* ── SSE watchdog + polling fallback ─────────────────────────────────
   Some edge proxies (free Cloudflare quick tunnels) accept the SSE
   request (200, text/event-stream) and then BUFFER the body forever —
   the EventSource looks connected but no frame ever arrives, so the
   thread sits dead until a refresh. The stream proves itself: the
   server sends a named "ping" event immediately on connect (and every
   heartbeat) into #ssewatch. If NO message lands within WATCH_MS (capped at 3s), the
   stream is declared buffered and this falls back to pulling
   /ui/session/:sid/poll?from=N — the SAME named HTML fragments the
   stream would have pushed, applied to the same [sse-swap] targets. */
(function () {
  var app = document.querySelector(".app[data-sid]");
  if (!app) { return; }
  var sid = app.dataset.sid;
  var cursor = parseInt(app.dataset.from || "0", 10) || 0;
  var frameCursor = 0;
  var chunkOffset = 0;
  var partialFrame = null;
  var alive = 0;          /* ts of last delivered SSE message */
  var polling = false;
  var WATCH_MS = 3000;    /* max wait for edge proxy buffering */
  var POLL_MS = 2500;

  /* only a DELIVERED message proves life — sseOpen fires even when
     the edge buffers everything after the headers. Once polling owns
     the page, CANCEL any late SSE swap (htmx:sseBeforeMessage is
     cancellable): if the edge buffer ever flushes, those frames were
     already pulled via /poll and would double-apply into the thread.
     Frames carry the gateway seq as the SSE id — track it so a
     RECONNECT rewinds to what was actually applied instead of the
     page-render cursor (which would replay the whole page-life of
     frames into #live again). */
  function liveKeysIn(html) {
    var tpl = document.createElement("template");
    tpl.innerHTML = html || "";
    return Array.prototype.map.call(
      tpl.content.querySelectorAll("[data-live-key]"),
      function (el) { return el.getAttribute("data-live-key"); })
      .filter(Boolean);
  }

  function hasExistingLiveKey(html) {
    return liveKeysIn(html).some(function (key) {
      return Array.prototype.some.call(
        document.querySelectorAll("[data-live-key]"),
        function (el) { return el.getAttribute("data-live-key") === key; });
    });
  }

  document.body.addEventListener("htmx:sseBeforeMessage", function (e) {
    var data = (e.detail && e.detail.data) || "";
    alive = Date.now();
    var seq = parseInt((e.detail && e.detail.lastEventId) || "", 10);
    if (seq > cursor) { cursor = seq; }
    /* htmx-sse swaps chrome with innerHTML. If a replay/reconnect sends the
       exact same sidebar/header fragment, skip it instead of tearing down and
       recreating every SVG <use> icon — that teardown is the visible flicker
       when switching sessions. beforeend live messages still dedupe by key. */
    var mode = e.target && (e.target.getAttribute("hx-swap") || "innerHTML").trim();
    if (polling || hasExistingLiveKey(data) ||
        (mode !== "beforeend" && e.target && e.target.innerHTML === data)) {
      e.preventDefault();
    }
  });

  /* htmx-sse builds reconnects through this hook (documented override
     point) — rewind the ?from= cursor to the highest applied seq. */
  if (window.htmx) {
    window.htmx.createEventSource = function (url) {
      var rewound = url.replace(/([?&]from=)\d+/, "$1" + cursor);
      return new EventSource(rewound, { withCredentials: true });
    };
  }

  function applyFrame(f) {
    var nodes = document.querySelectorAll('[sse-swap~="' + f.event + '"]');
    nodes.forEach(function (el) {
      var mode = (el.getAttribute("hx-swap") || "innerHTML").trim();
      if (mode === "none") { return; }
      if (mode === "beforeend") {
        if (hasExistingLiveKey(f.html)) { return; }
        el.insertAdjacentHTML("beforeend", f.html);
      }
      else { el.innerHTML = f.html; }
      if (window.htmx) { window.htmx.process(el); }
    });
  }

  function poll() {
    if (!polling) { return; }
    if (document.hidden) { setTimeout(poll, POLL_MS); return; }
    fetch("/ui/session/" + sid + "/poll?from=" + cursor +
          "&frame=" + frameCursor + "&offset=" + chunkOffset,
          { headers: { Accept: "application/json" } })
      .then(function (r) { return r.ok ? r.json() : null; })
      .then(function (d) {
        var touched = false;
        if (d && d.frames && d.frames.length) {
          d.frames.forEach(function (f) { applyFrame(f); touched = true; });
        }
        if (d && d.partials && d.partials.length) {
          d.partials.forEach(function (p) {
            if (!partialFrame || partialFrame.event !== p.event) {
              partialFrame = { event: p.event, html: "" };
            }
            partialFrame.html += p.html || "";
            if (p.done) {
              applyFrame(partialFrame);
              partialFrame = null;
              touched = true;
            }
          });
        }
        if (touched) {
          var thread = document.querySelector(".thread");
          if (thread) { thread.scrollTop = thread.scrollHeight; }
        }
        if (d && typeof d.next === "number" && d.next > cursor) {
          cursor = d.next;
          frameCursor = 0;
          chunkOffset = 0;
          partialFrame = null;
        } else if (d) {
          frameCursor = parseInt(d.frame || "0", 10) || 0;
          chunkOffset = parseInt(d.offset || "0", 10) || 0;
        }
        return d && d.more;
      })
      .catch(function (err) { console.warn("vis poll failed (will retry):", err); return false; })
      .then(function (more) { setTimeout(poll, more ? 0 : POLL_MS); });
  }

  setTimeout(function () {
    if (alive) { return; }     /* SSE delivered — stay on the stream */
    polling = true;
    /* close the buffered EventSource so a late edge flush can't
       double-apply frames the poller already pulled. htmx-sse only
       reconnects from its onerror handler — a manual close() fires no
       error, so the stream stays closed for the page's lifetime. */
    try {
      var d = app["htmx-internal-data"];
      if (d && d.sseEventSource) { d.sseEventSource.close(); }
    } catch (err) { console.warn("vis: could not close buffered EventSource:", err); }
    console.warn("vis: SSE silent for " + WATCH_MS +
      "ms — an edge proxy is buffering the stream; falling back to polling.");
    poll();
  }, WATCH_MS);

  /* ── Command palette (Ctrl/Cmd+P) ─────────────────────────────────────
     A searchable overlay over every web command — the SAME set the
     composer's "/" menu uses (/ui/slash: new/fork/switch session, settings,
     providers, and the engine slashes). Type to filter, ↑/↓ to move, Enter
     to run. Running a command drops its slash into the composer and submits,
     so the server's existing slash handling does the work. The TUI twin is
     Ctrl+P. We preventDefault so the browser's Ctrl/Cmd+P print dialog never
     steals the chord. */
  ready(function () {
    var palette, input, list, items = [], filtered = [], active = 0, cmds = null;
    function composerEl() { return document.querySelector(".composer textarea"); }
    function formEl() { return document.querySelector("form.composer"); }

    function build() {
      if (palette) { return; }
      palette = document.createElement("div");
      palette.className = "cmd-palette";
      palette.hidden = true;
      palette.innerHTML =
        '<div class="cmd-palette-backdrop"></div>' +
        '<div class="cmd-palette-box" role="dialog" aria-label="Command palette">' +
        '<input class="cmd-palette-input" type="text" autocomplete="off" ' +
        'spellcheck="false" placeholder="Type a command…">' +
        '<ul class="cmd-palette-list"></ul></div>';
      document.body.appendChild(palette);
      input = palette.querySelector(".cmd-palette-input");
      list = palette.querySelector(".cmd-palette-list");
      palette.querySelector(".cmd-palette-backdrop")
        .addEventListener("mousedown", close);
      input.addEventListener("input", function () { active = 0; render(); });
      input.addEventListener("keydown", onKey);
    }

    function fetchCmds(cb) {
      if (cmds) { cb(cmds); return; }
      fetch("/ui/slash").then(function (r) { return r.json(); })
        .then(function (a) { cmds = a || []; cb(cmds); })
        .catch(function () { cmds = []; cb(cmds); });
    }

    function render() {
      var q = input.value.trim().toLowerCase();
      filtered = items.filter(function (it) {
        return !q || (it.name + " " + (it.doc || "")).toLowerCase().indexOf(q) >= 0;
      });
      if (active >= filtered.length) { active = Math.max(0, filtered.length - 1); }
      list.innerHTML = "";
      filtered.forEach(function (it, i) {
        var li = document.createElement("li");
        li.className = "cmd-palette-row" + (i === active ? " is-active" : "");
        var nm = document.createElement("span");
        nm.className = "cmd-palette-name"; nm.textContent = it.name;
        var dc = document.createElement("span");
        dc.className = "cmd-palette-doc"; dc.textContent = it.doc || "";
        li.appendChild(nm); li.appendChild(dc);
        li.addEventListener("mousedown", function (e) {
          e.preventDefault(); active = i; run();
        });
        list.appendChild(li);
      });
    }

    function open() {
      build();
      fetchCmds(function (a) {
        items = a; active = 0; input.value = "";
        render();
        palette.hidden = false;
        input.focus();
      });
    }
    function close() { if (palette) { palette.hidden = true; } }

    function run() {
      var it = filtered[active];
      var c = composerEl(), f = formEl();
      close();
      if (!it || !c || !f) { return; }
      c.value = it.name;
      c.dispatchEvent(new Event("input"));
      if (typeof f.requestSubmit === "function") { f.requestSubmit(); }
      else { f.submit(); }
    }

    function onKey(e) {
      if (e.key === "Escape") { e.preventDefault(); close(); }
      else if (e.key === "ArrowDown") {
        e.preventDefault(); active = Math.min(active + 1, filtered.length - 1); render();
      } else if (e.key === "ArrowUp") {
        e.preventDefault(); active = Math.max(active - 1, 0); render();
      } else if (e.key === "Enter") { e.preventDefault(); run(); }
    }

    document.addEventListener("keydown", function (e) {
      if ((e.ctrlKey || e.metaKey) && !e.altKey && !e.shiftKey &&
          (e.key === "p" || e.key === "P")) {
        e.preventDefault();
        if (palette && !palette.hidden) { close(); } else { open(); }
      }
    });
  });
})();


