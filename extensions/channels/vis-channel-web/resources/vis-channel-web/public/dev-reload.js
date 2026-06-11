/* vis web companion auto-reload.
   Listens to /ui/dev-reload (SSE): the server emits `reload` when the
   web channel namespace is reloaded; a successful reconnect after a
   drop means the daemon restarted - refresh then too. */
(function () {
  var es = new EventSource("/ui/dev-reload");
  var dropped = false;
  es.addEventListener("reload", function () { location.reload(); });
  es.onerror = function () { dropped = true; };
  es.onopen = function () { if (dropped) { location.reload(); } };
})();
