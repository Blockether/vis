/* vis web companion - chat interactions (vendored, no externals).
   - prose with data-md re-renders through `marked` (vendored, MIT)
   - code blocks highlight through `Prism` (vendored, MIT; manual mode)
   - composer: autogrow, Enter sends (Shift+Enter breaks)
   - `/` opens slash-command suggestions, `@word` opens the file picker;
     arrows + Enter/Tab select, Esc closes
   - thread follows the newest content while you are near the bottom
   - mic records, encodes 16-bit PCM WAV, POSTs to /voice (Parakeet) */
(function () {
  function ready(fn) {
    if (document.readyState !== "loading") { fn(); }
    else { document.addEventListener("DOMContentLoaded", fn); }
  }

  /* ── syntax highlight: every `language-*` code block (machinery
     cells, IR fences, marked output) through the vendored Prism */
  function highlightCode(root) {
    if (typeof Prism === "undefined" || !Prism.highlightElement) { return; }
    (root || document).querySelectorAll('code[class*="language-"]:not([data-hl-done])')
      .forEach(function (el) {
        el.setAttribute("data-hl-done", "1");
        try { Prism.highlightElement(el); } catch (e) { /* plain text is fine */ }
      });
  }

  /* ── markdown: render EVERY [data-md] through marked (bubbles AND
     Context-rail fact contents — the turn_<N> fact is a markdown blob) */
  /* ── code folds: every fenced block from marked wraps in a <details>
     COLLAPSED by default (short snippets <=3 lines stay open) — the
     summary shows the language + line count, tap to expand */
  function foldCode(el) {
    el.querySelectorAll("pre:not([data-folded])").forEach(function (pre) {
      var code = pre.querySelector("code");
      if (!code) { return; }
      pre.setAttribute("data-folded", "1");
      var details = document.createElement("details");
      details.className = "code-fold";
      var lang = (code.className.match(/language-([\w-]+)/) || [])[1] || "code";
      var lines = (code.textContent.replace(/\n$/, "").match(/\n/g) || []).length + 1;
      details.open = lines <= 3;
      var sum = document.createElement("summary");
      sum.textContent = lang + " \u00b7 " + lines + (lines === 1 ? " line" : " lines");
      pre.parentNode.insertBefore(details, pre);
      details.appendChild(sum);
      details.appendChild(pre);
    });
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

    /* ── suggestion popup (slash commands + @files) ─────────────────── */
    var items = [], active = -1, mode = null, wordStart = 0;
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
        row.className = "suggest-row" + (i === active ? " active" : "");
        var name = document.createElement("span");
        name.className = "suggest-name";
        name.textContent = it.name;
        row.appendChild(name);
        if (it.doc) {
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
      });
    }
    function pick(i) {
      var it = items[i];
      if (!it || !composer) { return; }
      if (mode === "slash") {
        composer.value = it.name + " ";
      } else if (mode === "file") {
        composer.value = composer.value.slice(0, wordStart) + it.name + " " +
          composer.value.slice(composer.selectionStart);
      }
      hideSuggest();
      composer.focus();
      composer.dispatchEvent(new Event("input"));
    }
    function updateSuggest() {
      if (!composer) { return; }
      var v = composer.value;
      var caret = composer.selectionStart;
      if (v.charAt(0) === "/" && v.indexOf(" ") === -1) {
        var q = v.slice(1).toLowerCase();
        var apply = function (cmds) {
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
      var before = v.slice(0, caret);
      var m = before.match(/(^|\s)@([\w./-]*)$/);
      if (m && form && form.dataset.filesUrl) {
        wordStart = caret - m[2].length - 1;
        fetch(form.dataset.filesUrl + "?q=" + encodeURIComponent(m[2]))
          .then(function (r) { return r.json(); })
          .then(function (paths) {
            showSuggest(paths.map(function (p) { return { name: p }; }), "file");
          })
          .catch(hideSuggest);
        return;
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
      composer.addEventListener("input", function () { grow(); updateSuggest(); syncSend(); saveDraft(); });
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

    /* ── markdown + highlighting on ANY new content (thread bubbles,
       machinery cells, and the Context rail — which lives OUTSIDE
       .thread, so observe the app) */
    var appRoot = document.querySelector(".app") || document.body;
    new MutationObserver(function () { renderProse(appRoot); })
      .observe(appRoot, { childList: true, subtree: true });

    /* ── thread follow ───────────────────────────────────────────────── */
    var thread = document.querySelector(".thread");
    if (thread) {
      var follow = true;
      thread.addEventListener("scroll", function () {
        follow = thread.scrollHeight - thread.scrollTop - thread.clientHeight < 160;
      });
      var onMutate = function () {
        if (follow) { thread.scrollTop = thread.scrollHeight; }
      };
      new MutationObserver(onMutate)
        .observe(thread, { childList: true, subtree: true, characterData: true });
      onMutate();
    }

    /* ── in-thread find bar (Ctrl/Cmd+F) ──────────────────────────────────
       Mirror of the TUI F3 find: input · Aa case chip · cur/total counter ·
       prev/next/close. Client-side, zero deps: every occurrence inside
       .thread text wraps in <mark class="find-hit">; the current one gets
       .current and scrolls into view. Esc / ✕ unwrap everything. Web
       navigates per-OCCURRENCE (cur/total) - finer than the TUI's
       per-message jump, same numbers. */
    if (thread) {
      var fb = document.createElement("div");
      fb.className = "find-bar";
      fb.hidden = true;
      fb.innerHTML =
        '<input type="text" class="find-input" placeholder="Find in chat" />' +
        '<button type="button" class="find-case" title="Match case">Aa</button>' +
        '<span class="find-count">0/0</span>' +
        '<button type="button" class="find-prev" title="Previous (Shift+Enter)">&#9664;</button>' +
        '<button type="button" class="find-next" title="Next (Enter)">&#9654;</button>' +
        '<button type="button" class="find-close" title="Close (Esc)">&#10005;</button>';
      document.body.appendChild(fb);
      var fInput = fb.querySelector(".find-input");
      var fCase = fb.querySelector(".find-case");
      var fCount = fb.querySelector(".find-count");
      var marks = [], cur = -1, caseOn = false;
      function unwrap() {
        marks.forEach(function (m) {
          if (!m.parentNode) { return; } /* bubble was re-rendered - gone */
          var p = m.parentNode;
          p.replaceChild(document.createTextNode(m.textContent), m);
          p.normalize();
        });
        marks = []; cur = -1;
      }
      function syncCount() {
        fCount.textContent = marks.length ? (cur + 1) + "/" + marks.length : "0/0";
        fb.classList.toggle("no-hits", !marks.length && fInput.value.length > 0);
      }
      function setCurrent(i) {
        if (!marks.length) { cur = -1; syncCount(); return; }
        if (cur >= 0 && marks[cur]) { marks[cur].classList.remove("current"); }
        cur = ((i % marks.length) + marks.length) % marks.length;
        marks[cur].classList.add("current");
        marks[cur].scrollIntoView({ block: "center" });
        syncCount();
      }
      function refind() {
        unwrap();
        var q = fInput.value;
        if (!q) { syncCount(); return; }
        var needle = caseOn ? q : q.toLowerCase();
        var walker = document.createTreeWalker(thread, NodeFilter.SHOW_TEXT, {
          acceptNode: function (n) {
            var el = n.parentElement;
            if (!el || el.closest("script, style, textarea, mark.find-hit")) {
              return NodeFilter.FILTER_REJECT;
            }
            var hay = caseOn ? n.nodeValue : n.nodeValue.toLowerCase();
            return hay.indexOf(needle) >= 0 ? NodeFilter.FILTER_ACCEPT
                                            : NodeFilter.FILTER_REJECT;
          }
        });
        var nodes = [];
        while (walker.nextNode()) { nodes.push(walker.currentNode); }
        nodes.forEach(function (node) {
          var text = node.nodeValue;
          var hay = caseOn ? text : text.toLowerCase();
          var frag = document.createDocumentFragment();
          var pos = 0, hit;
          while ((hit = hay.indexOf(needle, pos)) >= 0) {
            if (hit > pos) { frag.appendChild(document.createTextNode(text.slice(pos, hit))); }
            var mk = document.createElement("mark");
            mk.className = "find-hit";
            mk.textContent = text.slice(hit, hit + needle.length);
            frag.appendChild(mk);
            marks.push(mk);
            pos = hit + needle.length;
          }
          if (pos < text.length) { frag.appendChild(document.createTextNode(text.slice(pos))); }
          node.parentNode.replaceChild(frag, node);
        });
        setCurrent(0);
      }
      function openFind() {
        fb.hidden = false;
        fInput.focus();
        fInput.select();
        if (fInput.value) { refind(); }
      }
      function closeFind() {
        unwrap();
        syncCount();
        fb.hidden = true;
      }
      fInput.addEventListener("input", refind);
      fInput.addEventListener("keydown", function (e) {
        if (e.key === "Enter") { e.preventDefault(); setCurrent(e.shiftKey ? cur - 1 : cur + 1); }
        else if (e.key === "Escape") { e.preventDefault(); closeFind(); }
        e.stopPropagation(); /* keep the drawer Esc handler out of the loop */
      });
      fCase.addEventListener("click", function () {
        caseOn = !caseOn;
        fCase.classList.toggle("on", caseOn);
        refind();
        fInput.focus();
      });
      fb.querySelector(".find-prev").addEventListener("click", function () { setCurrent(cur - 1); });
      fb.querySelector(".find-next").addEventListener("click", function () { setCurrent(cur + 1); });
      fb.querySelector(".find-close").addEventListener("click", closeFind);
      document.addEventListener("keydown", function (e) {
        var k = e.key && e.key.toLowerCase();
        if ((e.ctrlKey || e.metaKey) && k === "f") {
          var t = document.activeElement;
          /* native browser find wins while typing in any OTHER field
             (composer, modal inputs) - only a non-typing context opens ours */
          var typing = t && t !== fInput &&
            (t.tagName === "TEXTAREA" || t.tagName === "INPUT" || t.isContentEditable);
          if (typing) { return; }
          e.preventDefault();
          openFind();
        } else if (e.key === "Escape" && !fb.hidden) {
          closeFind();
        }
      });
    }

    /* ── voice: live gold waveform + timer + cancel(✕)/accept(✓) ─────── */
    var mic = document.querySelector(".composer .mic");
    if (mic && composer) {
      var rec = null;
      var AC = window.AudioContext || window.webkitAudioContext;
      /* title tooltips are invisible on touch — surface mic problems as a
         small inline notice above the composer instead of doing nothing */
      var micNote = function (msg) {
        var old = composer.form.querySelector(".mic-note");
        if (old) { old.parentNode.removeChild(old); }
        var note = document.createElement("div");
        note.className = "mic-note";
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
      function buildRecUi() {
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
        var startedAt = Date.now();
        var timer = setInterval(function () {
          time.textContent = fmtTime(Date.now() - startedAt);
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
      mic.addEventListener("click", function () {
        if (rec) { return; }
        /* getUserMedia exists only in SECURE contexts — over plain http://
           from a phone, mediaDevices is undefined and the old code silently
           did NOTHING on tap. Say so instead. */
        if (!navigator.mediaDevices || !navigator.mediaDevices.getUserMedia || !AC) {
          micNote("Microphone needs HTTPS (or localhost) — open the page over a secure URL.");
          return;
        }
        navigator.mediaDevices.getUserMedia({ audio: true }).then(function (stream) {
          var ac = new AC();
          /* iOS Safari creates AudioContexts suspended — without resume()
             onaudioprocess never fires and the recording stays empty */
          if (ac.state === "suspended") { ac.resume(); }
          var src = ac.createMediaStreamSource(stream);
          var proc = ac.createScriptProcessor(4096, 1, 1);
          var chunks = [];
          var ui = buildRecUi();
          composer.form.classList.add("recording");
          mic.classList.add("recording");
          proc.onaudioprocess = function (e) {
            var data = e.inputBuffer.getChannelData(0);
            chunks.push(new Float32Array(data));
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
          };
          src.connect(proc); proc.connect(ac.destination);
          function teardown() {
            proc.disconnect(); src.disconnect();
            stream.getTracks().forEach(function (t) { t.stop(); });
            var rate = ac.sampleRate;
            ac.close();
            mic.classList.remove("recording");
            composer.form.classList.remove("recording");
            ui.remove();
            rec = null;
            return rate;
          }
          rec = { stop: teardown };
          ui.cancel.addEventListener("click", function () {
            teardown(); /* discard - no transcription */
            composer.focus();
          });
          ui.accept.addEventListener("click", function () {
            var rate = teardown();
            mic.disabled = true;
            /* transcription takes a moment (first use downloads the
               model) — say so instead of going silent */
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
      });
    }
  });
})();
