/* vis web companion - chat interactions (vendored, no externals).
   - prose with data-md re-renders through `marked` (vendored, MIT)
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

  /* ── markdown: render EVERY [data-md] through marked (bubbles AND
     Context-rail fact contents — the turn_<N> fact is a markdown blob) */
  function renderProse(root) {
    if (typeof marked === "undefined") { return; }
    (root || document).querySelectorAll("[data-md]:not([data-md-done])")
      .forEach(function (el) {
        el.setAttribute("data-md-done", "1");
        try {
          el.innerHTML = marked.parse(el.getAttribute("data-md"), { breaks: true });
        } catch (e) { /* keep the server-rendered fallback */ }
      });
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

    /* hideable rails */
    var app = document.querySelector(".app");
    function wireToggle(btnSel, cls, key) {
      var btn = document.querySelector(btnSel);
      if (!btn || !app) { return; }
      if (localStorage.getItem(key) === "1") { app.classList.add(cls); }
      btn.addEventListener("click", function () {
        app.classList.toggle(cls);
        localStorage.setItem(key, app.classList.contains(cls) ? "1" : "0");
      });
    }
    wireToggle("#toggle-left", "hide-left", "vis.hideLeft");
    wireToggle("#toggle-right", "hide-right", "vis.hideRight");

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
      composer.addEventListener("input", function () { grow(); updateSuggest(); syncSend(); });
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
        }
      });
      composer.focus();
      grow();
    }

    /* ── markdown on ANY new content (thread bubbles, Work log, and the
       Context rail — which lives OUTSIDE .thread, so observe the app) */
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

    /* ── voice: live gold waveform + timer + cancel(✕)/accept(✓) ─────── */
    var mic = document.querySelector(".composer .mic");
    if (mic && composer && navigator.mediaDevices) {
      var rec = null;
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
        cancel.textContent = "✕";
        var accept = document.createElement("button");
        accept.type = "button"; accept.className = "rec-accept";
        accept.setAttribute("aria-label", "Use recording");
        accept.textContent = "✓";
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
        navigator.mediaDevices.getUserMedia({ audio: true }).then(function (stream) {
          var ac = new AudioContext();
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
            fetch(mic.dataset.voiceUrl, { method: "POST", body: encodeWav(chunks, rate) })
              .then(function (r) { return r.json(); })
              .then(function (d) {
                mic.disabled = false;
                if (d.text) {
                  composer.value = (composer.value ? composer.value + " " : "") + d.text;
                  composer.dispatchEvent(new Event("input"));
                  composer.focus();
                } else if (d.error) {
                  mic.title = d.error;
                  mic.classList.add("mic-error");
                }
              })
              .catch(function () { mic.disabled = false; });
          });
        }).catch(function () { /* mic permission denied - nothing to do */ });
      });
    }
  });
})();
