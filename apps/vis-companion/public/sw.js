/* Background Web Push worker. Keep this file dependency-free: it runs without the app tab. */
self.addEventListener("push", (event) => {
  const payload = event.data ? event.data.json() : {};
  const data = payload && typeof payload.data === "object" ? payload.data : {};
  const title = typeof payload.title === "string" && payload.title ? payload.title : "Vis";
  const body = typeof payload.body === "string" ? payload.body : "";
  event.waitUntil(
    self.registration.showNotification(title, {
      body,
      tag: typeof payload.tag === "string" ? payload.tag : undefined,
      renotify: true,
      data,
    }),
  );
});

self.addEventListener("notificationclick", (event) => {
  event.notification.close();
  const data = event.notification.data || {};
  const sid = typeof data.session_id === "string" ? data.session_id : "";
  if (!sid) return;
  const gateway = typeof data.gateway_id === "string" ? data.gateway_id : "";
  const hash = `#/s/${encodeURIComponent(sid)}${gateway ? `?gw=${encodeURIComponent(gateway)}` : ""}`;
  const target = new URL("/", self.registration.scope);
  target.hash = hash.slice(1);
  event.waitUntil(
    self.clients.matchAll({ type: "window", includeUncontrolled: true }).then((clients) => {
      const existing = clients.find((client) => "focus" in client);
      if (existing) {
        return existing.navigate(target.href).then((client) => client?.focus());
      }
      return self.clients.openWindow(target.href);
    }),
  );
});
