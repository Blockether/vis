/* vis web companion - chat interactions (vendored, no externals).
   - composer textarea autogrows up to 8 lines
   - Enter sends, Shift+Enter inserts a newline
   - the thread keeps itself scrolled to the newest content while you
     are near the bottom (reading back disables the follow) */
(function () {
  function ready(fn) {
    if (document.readyState !== "loading") { fn(); }
    else { document.addEventListener("DOMContentLoaded", fn); }
  }

  ready(function () {
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
  });
})();
