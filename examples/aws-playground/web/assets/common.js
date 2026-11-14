// Shared parts of the playground pages: DOM builder, API calls, Markdown and Vis activities.
// The pages never put text in innerHTML, so the text of a model cannot become markup.

const ICONS = {
  eye: "M2 12s3.6-7 10-7 10 7 10 7-3.6 7-10 7S2 12 2 12Z M12 9a3 3 0 1 0 0 6 3 3 0 0 0 0-6Z",
  pencil: "M12 20h9 M16.5 3.5a2.1 2.1 0 0 1 3 3L7 19l-4 1 1-4Z",
  check: "M20 6 9 17l-5-5",
  globe: "M12 2a10 10 0 1 0 0 20 10 10 0 0 0 0-20Z M2 12h20 M12 2a15 15 0 0 1 0 20 15 15 0 0 1 0-20Z",
  x: "M18 6 6 18 M6 6l12 12",
  fork: "M6 3v6a6 6 0 0 0 6 6h0a6 6 0 0 0 6-6V3 M12 15v6 M6 3a2 2 0 1 0 0 .01 M18 3a2 2 0 1 0 0 .01 M12 21a2 2 0 1 0 0 .01",
  download: "M12 3v12 M7 10l5 5 5-5 M4 21h16",
  upload: "M12 21V9 M7 14l5-5 5 5 M4 3h16",
  play: "M6 4l14 8-14 8Z",
  plus: "M12 5v14 M5 12h14",
  back: "M15 18l-6-6 6-6",
  file: "M14 3H6a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V9Z M14 3v6h6",
  open: "M14 4h6v6 M20 4l-9 9 M19 14v5a1 1 0 0 1-1 1H5a1 1 0 0 1-1-1V6a1 1 0 0 1 1-1h5",
  trash: "M4 7h16 M9 7V4h6v3 M6 7l1 13h10l1-13",
  save: "M5 3h11l4 4v13a1 1 0 0 1-1 1H5a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1Z M8 3v5h7 M8 21v-7h8v7",
  list: "M8 6h13 M8 12h13 M8 18h13 M3 6h.01 M3 12h.01 M3 18h.01",
  stop: "M6 6h12v12H6Z",
  spark: "M12 3l1.9 5.6L19.5 10l-5.6 1.9L12 17.5l-1.9-5.6L4.5 10l5.6-1.4Z",
};

/** Make an element. `props` sets attributes, `class`, `text`, `data-*` and `on*` handlers. */
export function h(tag, props = {}, ...children) {
  const node = document.createElement(tag);
  for (const [key, value] of Object.entries(props || {})) {
    if (value === null || value === undefined || value === false) continue;
    if (key === "class") node.className = value;
    else if (key === "text") node.textContent = value;
    else if (key.startsWith("on")) node.addEventListener(key.slice(2), value);
    else node.setAttribute(key, value === true ? "" : value);
  }
  node.append(...children.flat().filter((child) => child !== null && child !== undefined && child !== false));
  return node;
}

/** Replace the children of `node`. Like `h`, it skips null, undefined and false. */
export function fill(node, ...children) {
  node.replaceChildren(...children.flat().filter((child) => child !== null && child !== undefined && child !== false));
  return node;
}

export function icon(name) {
  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  for (const [key, value] of Object.entries({ viewBox: "0 0 24 24", fill: "none", stroke: "currentColor", "stroke-width": "2", "stroke-linecap": "round", "stroke-linejoin": "round", "aria-hidden": "true", class: "icon" })) svg.setAttribute(key, value);
  const path = document.createElementNS("http://www.w3.org/2000/svg", "path");
  path.setAttribute("d", ICONS[name] || ICONS.spark);
  svg.append(path);
  return svg;
}

export function button(label, props = {}, glyph) {
  return h("button", { type: "button", ...props, class: `btn ${props.class || ""}` }, glyph && icon(glyph), label && h("span", { text: label }));
}

/** Call the site and return the JSON answer. An error answer becomes an Error with its text. */
export async function call(path, { method = "GET", body, raw = false } = {}) {
  const answer = await fetch(path, {
    method,
    headers: body === undefined ? {} : { "content-type": "application/json" },
    body: body === undefined ? undefined : JSON.stringify(body),
    credentials: "same-origin",
  });
  if (answer.status === 401) {
    location.assign("/play");
    throw new Error("Sign in first.");
  }
  if (raw && answer.ok) return answer;
  const text = await answer.text();
  let data = null;
  try {
    data = text ? JSON.parse(text) : null;
  } catch {
    data = null;
  }
  if (!answer.ok) throw new Error((data && (data.error?.message || data.error || data.message)) || `The call failed (HTTP ${answer.status}).`);
  return data;
}

export const play = (path, options) => call(`/play/api/${path}`, options);
export const vis = (path, options) => call(`/v1/${path}`, options);

export function toast(text, tone = "") {
  let box = document.querySelector(".toasts");
  if (!box) document.body.append((box = h("div", { class: "toasts", role: "status" })));
  const node = h("div", { class: `toast ${tone}`, text });
  box.append(node);
  setTimeout(() => node.remove(), tone === "bad" ? 9000 : 4500);
}

/** Run an action of a button. The button waits, and an error shows as a toast. */
export async function guard(control, action) {
  if (control) control.disabled = true;
  try {
    return await action();
  } catch (error) {
    toast(error.message || String(error), "bad");
    return undefined;
  } finally {
    if (control) control.disabled = false;
  }
}

export function ago(ms) {
  if (!ms) return "";
  const s = Math.max(0, (Date.now() - ms) / 1000);
  if (s < 50) return "now";
  if (s < 3600) return `${Math.round(s / 60)} min ago`;
  if (s < 86400) return `${Math.round(s / 3600)} h ago`;
  if (s < 86400 * 14) return `${Math.round(s / 86400)} d ago`;
  return new Date(ms).toLocaleDateString("en-GB", { day: "numeric", month: "short", year: "numeric" });
}

export function clock(ms) {
  return ms ? new Date(ms).toLocaleString("en-GB", { day: "numeric", month: "short", hour: "2-digit", minute: "2-digit", second: "2-digit" }) : "";
}

export function span(ms) {
  if (ms === null || ms === undefined) return "";
  if (ms < 1000) return `${Math.round(ms)} ms`;
  const s = ms / 1000;
  if (s < 60) return `${s.toFixed(s < 10 ? 1 : 0)} s`;
  return `${Math.floor(s / 60)} min ${Math.round(s % 60)} s`;
}

export function size(bytes) {
  if (bytes < 1024) return `${bytes} B`;
  if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)} kB`;
  return `${(bytes / 1024 / 1024).toFixed(1)} MB`;
}

export const count = (n) => (n >= 1000 ? `${(n / 1000).toFixed(n >= 10000 ? 0 : 1)}k` : String(n ?? 0));

// -- Markdown ---------------------------------------------------------------------

function inline(text) {
  const out = [];
  const pattern = /(`[^`\n]+`)|(\*\*[^*\n]+\*\*)|(\[[^\]\n]+\]\((https?:\/\/[^)\s]+)\))|(https?:\/\/[^\s<>)\]]+)/g;
  let at = 0;
  for (const found of text.matchAll(pattern)) {
    if (found.index > at) out.push(text.slice(at, found.index));
    const token = found[0];
    if (found[1]) out.push(h("code", { text: token.slice(1, -1) }));
    else if (found[2]) out.push(h("strong", {}, inline(token.slice(2, -2))));
    else if (found[3]) out.push(h("a", { href: found[4], target: "_blank", rel: "noopener noreferrer", text: token.slice(1, token.indexOf("](")) }));
    else out.push(h("a", { href: token, target: "_blank", rel: "noopener noreferrer", text: token }));
    at = found.index + token.length;
  }
  if (at < text.length) out.push(text.slice(at));
  return out;
}

/** Return Markdown as elements. It knows headings, lists, code, quotes, tables and links. */
export function markdown(source) {
  const root = h("div", { class: "md" });
  const rows = String(source ?? "").replace(/\r/g, "").split("\n");
  let index = 0;
  const cells = (row) => row.trim().replace(/^\||\|$/g, "").split("|").map((cell) => cell.trim());
  while (index < rows.length) {
    const row = rows[index];
    const fence = row.match(/^\s*(```|~~~)/);
    if (fence) {
      const code = [];
      index += 1;
      while (index < rows.length && !rows[index].trim().startsWith(fence[1])) code.push(rows[index++]);
      index += 1;
      root.append(h("pre", {}, h("code", { text: code.join("\n") })));
    } else if (/^#{1,6}\s/.test(row)) {
      const level = Math.min(3, row.match(/^#+/)[0].length);
      root.append(h(`h${level}`, {}, inline(row.replace(/^#+\s*/, ""))));
      index += 1;
    } else if (/^\s*([-*+]|\d+[.)])\s/.test(row)) {
      const ordered = /^\s*\d/.test(row);
      const list = h(ordered ? "ol" : "ul");
      while (index < rows.length && /^\s*([-*+]|\d+[.)])\s/.test(rows[index])) {
        let item = rows[index++].replace(/^\s*([-*+]|\d+[.)])\s+/, "");
        while (index < rows.length && /^\s{2,}\S/.test(rows[index]) && !/^\s*([-*+]|\d+[.)])\s/.test(rows[index])) item += ` ${rows[index++].trim()}`;
        list.append(h("li", {}, inline(item.replace(/^\[( |x)\]\s*/, (_, mark) => (mark === "x" ? "☑ " : "☐ ")))));
      }
      root.append(list);
    } else if (/^\s*>/.test(row)) {
      const quote = [];
      while (index < rows.length && /^\s*>/.test(rows[index])) quote.push(rows[index++].replace(/^\s*>\s?/, ""));
      root.append(h("blockquote", {}, inline(quote.join(" "))));
    } else if (row.includes("|") && /^\s*\|?\s*:?-{2,}/.test(rows[index + 1] || "")) {
      const table = h("table", {}, h("tr", {}, cells(row).map((cell) => h("th", {}, inline(cell)))));
      index += 2;
      while (index < rows.length && rows[index].includes("|") && rows[index].trim()) table.append(h("tr", {}, cells(rows[index++]).map((cell) => h("td", {}, inline(cell)))));
      root.append(table);
    } else if (/^\s*(-{3,}|\*{3,})\s*$/.test(row)) {
      root.append(h("hr"));
      index += 1;
    } else if (!row.trim()) {
      index += 1;
    } else {
      const text = [];
      while (index < rows.length && rows[index].trim() && !/^(\s*(```|~~~|>|[-*+]\s|\d+[.)]\s)|#{1,6}\s)/.test(rows[index])) text.push(rows[index++]);
      root.append(h("p", {}, inline(text.join(" "))));
    }
  }
  return root;
}

// -- Vis activities -----------------------------------------------------------------

const SIGNAL = { observation: "eye", mutation: "pencil", verification: "check", external: "globe" };

function block(part) {
  if (part.type === "table") {
    const head = h("tr", {}, (part.columns || []).map((name) => h("th", { text: name })));
    const body = (part.rows || []).slice(0, 200).map((row) => h("tr", {}, row.map((cell) => h("td", { text: String(cell ?? "") }))));
    return h("div", { class: "block scroll-x" }, h("table", {}, head, body));
  }
  if (part.type === "terminal") {
    const command = Array.isArray(part.command) ? part.command.join(" ") : String(part.command || "");
    const lines = (part.lines || []).map((line) => h("span", { class: line.stream === "stderr" ? "err" : "", text: `${line.text}\n` }));
    return h("div", { class: "block terminal" }, h("div", { class: "cmd", text: `$ ${command}` }), lines.length ? h("pre", {}, lines) : null);
  }
  if (part.type === "markdown") return h("div", { class: "block" }, h("div", { style: "padding:10px 12px" }, markdown(part.markdown || part.text || "")));
  if (part.type === "code" || part.type === "diff") return h("div", { class: "block" }, h("pre", { text: part.code || part.text || part.diff || "" }));
  if (part.type === "text") return h("div", { class: "block" }, h("pre", { text: part.text || "" }));
  return h("div", { class: "block" }, h("pre", { text: JSON.stringify(part, null, 2) }));
}

/** Return one row of a Vis activity: the headline, the summary and the content that Vis shows. */
export function activity(row) {
  const shown = row.presentation || {};
  const failed = row.state === "failed";
  const parts = (shown.content || []).map(block);
  if (failed && row.error_summary && !parts.length) parts.push(h("div", { class: "block" }, h("pre", { text: row.error_summary })));
  const summary = h(
    "summary",
    {},
    h("span", { class: "sig", title: row.signal || "" }, icon(failed ? "x" : SIGNAL[row.signal] || "spark")),
    h("span", { class: "line" }, h("span", { class: "headline", text: shown.headline || row.summary || row.operation || "Step" }), h("span", { class: "sum", text: shown.summary || "" })),
    h("span", { class: "ms", text: span(row.duration_ms) }),
  );
  return h("details", { class: `act ${row.signal || ""} ${failed ? "failed" : ""}` }, summary, h("div", { class: "act-body" }, shown.summary && h("div", { class: "small muted" }, inline(shown.summary)), parts));
}

/** Return the Vis activities of one turn of the JSON transcript, in the order of the work. */
export function activities(turn) {
  const rows = [];
  for (const step of turn.iterations || []) for (const form of step.forms || []) rows.push(...((form.activity && form.activity.rows) || []));
  return rows;
}

// -- page parts -----------------------------------------------------------------------

/** Show a value of a workflow. A short text, a number or a flag is a decision. */
export function value(data) {
  if (data === null || data === undefined) return h("span", { class: "muted", text: "none" });
  if (typeof data === "boolean" || typeof data === "number" || (typeof data === "string" && data.length <= 24 && !data.includes(" "))) return h("span", { class: "decision", text: String(data) });
  if (typeof data === "string") return data.includes("\n") ? markdown(data) : h("span", { text: data });
  if (Array.isArray(data) && data.every((item) => typeof item === "string")) return h("ul", { style: "margin:0;padding-left:1.1em" }, data.map((item) => h("li", { text: item })));
  if (typeof data === "object" && !Array.isArray(data)) return h("dl", { class: "kv" }, Object.entries(data).flatMap(([key, item]) => [h("dt", { text: key }), h("dd", {}, value(item))]));
  return h("pre", { text: JSON.stringify(data, null, 2) });
}

export const plural = (n, word, many = `${word}s`) => `${n} ${n === 1 ? word : many}`;

/** True for a value that fits on one row: a flag, a number or a short text. */
function brief(data) {
  return data === null || typeof data === "boolean" || typeof data === "number" || (typeof data === "string" && data.length <= 48 && !data.includes("\n"));
}

/** Show a structured result. The short fields are the decisions. A long text stacks and a list folds. */
export function structured(data) {
  if (typeof data !== "object" || data === null || Array.isArray(data)) return value(data);
  const fields = Object.entries(data);
  const short = fields.filter(([, item]) => brief(item));
  return h(
    "div",
    { class: "result" },
    short.length ? h("dl", { class: "kv" }, short.flatMap(([key, item]) => [h("dt", { text: key }), h("dd", {}, value(item))])) : null,
    fields
      .filter(([, item]) => !brief(item))
      .map(([key, item]) =>
        typeof item === "string" && item.length <= 400
          ? h("div", { class: "long" }, h("span", { class: "label", text: key }), value(item))
          : h("details", { class: "fold" }, h("summary", {}, h("span", { class: "label", text: key }), h("span", { class: "small muted", text: Array.isArray(item) ? plural(item.length, "item") : typeof item === "string" ? "text" : plural(Object.keys(item).length, "field") })), value(item)),
      ),
  );
}

export function dialog(title, body, actions = []) {
  const box = h("dialog", {});
  const close = () => {
    box.close();
    box.remove();
  };
  box.append(h("div", { class: "dialog-head" }, h("h3", { class: "clip", text: title }), button("", { class: "quiet small right", "aria-label": "Close", onclick: close }, "x")), h("div", { class: "dialog-body" }, body));
  if (actions.length) box.append(h("div", { class: "dialog-foot" }, actions));
  box.addEventListener("click", (event) => event.target === box && close());
  document.body.append(box);
  box.showModal();
  return { box, close };
}

/** Save an answer of the site as a file of the browser. */
export async function download(path, name) {
  const answer = await call(path, { raw: true });
  const link = h("a", { href: URL.createObjectURL(await answer.blob()), download: name });
  document.body.append(link);
  link.click();
  setTimeout(() => {
    URL.revokeObjectURL(link.href);
    link.remove();
  }, 1000);
}

/** Show a stored text file in a dialog, with a download button. */
export async function preview(path, name) {
  const text = await (await call(path, { raw: true })).text();
  const body = /\.(md|markdown)$/i.test(name) ? markdown(text) : h("div", { class: "block" }, h("pre", { text }));
  dialog(name, body, [button("Download", { onclick: (event) => guard(event.currentTarget, () => download(path, name.split("/").pop())) }, "download")]);
}

/** Fill the top bar and keep the release label current. */
export function bar(active) {
  const links = [["/play", "Sessions"], ["/play/workflows", "Workflows"], ["/play/architecture", "Architecture"]];
  const release = h("span", { class: "release", title: "The release of the repository that this worker uses" }, h("i"), h("span", { text: "starting worker…" }));
  document.querySelector("header.bar").append(
    h("a", { class: "brand", href: "/play" }, h("img", { src: "/play/assets/logo.svg", alt: "" }), h("span", {}, "Vis Playground ", h("small", { text: "on AWS Lambda" }))),
    h("nav", { class: "nav", "aria-label": "Pages" }, links.map(([href, text]) => h("a", { href, text, "aria-current": href === active ? "page" : null }))),
    h("span", { class: "grow" }),
    release,
    h("a", { class: "btn dark small", href: "/", title: "The Vis web app, on the same gateway" }, icon("open"), h("span", { text: "Open Vis" })),
    h("form", { class: "only-wide", method: "post", action: "/logout" }, h("button", { class: "btn quiet small", type: "submit", text: "Sign out" })),
  );
  const refresh = async () => {
    try {
      const status = await play("status");
      release.classList.add("live");
      release.lastChild.textContent = `${status.repository.split("/").pop()}@${status.release.slice(0, 7)}`;
      release.dataset.release = status.release;
      document.dispatchEvent(new CustomEvent("playground-status", { detail: status }));
    } catch {
      release.classList.remove("live");
    }
  };
  refresh();
  setInterval(refresh, 30000);
}
