/* vis web companion - chat interactions (vendored, no externals).
   - composer textarea autogrows up to 8 lines
   - Enter sends, Shift+Enter inserts a newline
   - the thread keeps itself scrolled to the newest content while you
     are near the bottom (reading back disables the follow)
   - mic button records, encodes 16-bit PCM WAV in-browser, POSTs it to
     /ui/session/:sid/voice (local Parakeet ASR) and drops the
     transcript into the composer */
(function () {
  function ready(fn) {
    if (document.readyState !== "loading") { fn(); }
    else { document.addEventListener("DOMContentLoaded", fn); }
  }

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
    // hideable rails: state persists per browser
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
    if (composer) {
      var grow = function () {
        composer.style.height = "auto";
        composer.style.height = Math.min(composer.scrollHeight, 200) + "px";
      };
      composer.addEventListener("input", grow);
      composer.addEventListener("keydown", function (e) {
        if (e.key === "Enter" && !e.shiftKey) {
          e.preventDefault();
          if (composer.value.trim().length > 0) {
            composer.form.requestSubmit();
          }
        }
      });
      document.body.addEventListener("htmx:afterRequest", function (e) {
        if (e.detail.successful && e.target.classList.contains("composer")) {
          composer.style.height = "auto";
          composer.focus();
        }
      });
      composer.focus();
      grow();
    }

    var thread = document.querySelector(".thread");
    if (thread) {
      var follow = true;
      thread.addEventListener("scroll", function () {
        follow = thread.scrollHeight - thread.scrollTop - thread.clientHeight < 160;
      });
      var toBottom = function () {
        if (follow) { thread.scrollTop = thread.scrollHeight; }
      };
      new MutationObserver(toBottom)
        .observe(thread, { childList: true, subtree: true, characterData: true });
      toBottom();
    }

    var mic = document.querySelector(".composer .mic");
    if (mic && composer && navigator.mediaDevices) {
      var rec = null;
      mic.addEventListener("click", function () {
        if (rec) { rec.stop(); return; }
        navigator.mediaDevices.getUserMedia({ audio: true }).then(function (stream) {
          var ac = new AudioContext();
          var src = ac.createMediaStreamSource(stream);
          var proc = ac.createScriptProcessor(4096, 1, 1);
          var chunks = [];
          proc.onaudioprocess = function (e) {
            chunks.push(new Float32Array(e.inputBuffer.getChannelData(0)));
          };
          src.connect(proc); proc.connect(ac.destination);
          mic.classList.add("recording");
          rec = {
            stop: function () {
              proc.disconnect(); src.disconnect();
              stream.getTracks().forEach(function (t) { t.stop(); });
              var rate = ac.sampleRate;
              ac.close();
              mic.classList.remove("recording");
              rec = null;
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
            }
          };
        }).catch(function () { /* mic permission denied - nothing to do */ });
      });
    }
  });
})();
