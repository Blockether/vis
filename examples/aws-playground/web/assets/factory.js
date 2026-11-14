// The factory page: the workflow `software_factory` as a production line.
// An order is one run. A README goes in at the first station. SPEC.md comes out and goes to GitHub.
import { OPEN, TONE, ago, bar, button, cancelRun, chip, download, drop, empty, filePath, fileRow, fill, giveFile, github, h, icon, item, loading, markdown, play, press, read, size, span, stepBars, structured, textOf, versionOf, waiting } from "/play/assets/common.js";

const WORKFLOW = "software_factory";
// The glyph of each station, by the name of its workflow activity.
const GLYPHS = { intake: "upload", specify: "pencil", inspect: "eye", rework: "fork", ship: "globe" };
const WORDS = { idle: "Idle", skipped: "Not needed", running: "At work", waiting: "Waits for a file", completed: "Done", failed: "Failed", cancelled: "Stopped" };

const orders = document.getElementById("orders");
const floor = document.getElementById("floor");
// The fragment goes into request paths: accept only a run id.
const orderId = () => /^#([0-9a-f]{32})$/.exec(location.hash)?.[1] || null;
// flow: the workflow. runs: its runs. id, doc: the open order. file: a README that waits for the first station of its new order.
// spec: the text of the shown document, with the key of its version. drawn: the data of the last draw.
const state = { flow: null, runs: [], id: orderId(), doc: null, file: null, spec: { key: "", text: null }, drawn: "" };

/** Return the stations of an order: each workflow activity with the state of its step. */
function stations(doc) {
  const steps = doc?.activities || [];
  const found = state.flow.activities.map((activity) => ({ ...activity, step: steps.findLast((step) => step.name === activity.name) }));
  const last = found.findLastIndex((station) => station.step);
  // A station before the last used one that has no step: the workflow did not need it.
  return found.map((station, index) => ({ ...station, status: station.step?.status || (index < last ? "skipped" : "idle") }));
}

/** Give a file to the station that waits for it. The run then starts again. */
async function give(doc, given) {
  await giveFile(doc, given);
  await refresh();
}

/** Make a new order. Its first station gets the file as soon as it waits. */
async function order(file, product) {
  const given = await textOf(file);
  const run = await play("runs", { method: "POST", body: { workflow: WORKFLOW, input: product ? { product } : {} } });
  state.file = { id: run.id, given };
  select(run.id);
}

function select(id) {
  Object.assign(state, { id, doc: null, drawn: "" });
  history.replaceState(null, "", id ? `#${id}` : location.pathname);
  drawOrders();
  if (id) fill(floor, loading("Loading the order…"));
  return refresh();
}

/** Return the line: the belt, the product on it and the stations. */
function line(doc) {
  const all = stations(doc);
  const active = all.findIndex((station) => station.status === "running" || station.status === "waiting");
  const at = doc?.status === "completed" ? all.length - 1 : active >= 0 ? active : Math.max(0, all.findLastIndex((station) => station.step));
  const moving = active >= 0 && all[active].status === "running";
  return h(
    "div",
    { class: `conveyor ${moving ? "moving" : ""}`, style: `--n:${all.length};--at:${at}`, role: "list", "aria-label": "Stations" },
    h("div", { class: "belt" }, h("i")),
    doc && h("div", { class: "crate", title: "The product of this order" }, icon("file")),
    all.map(({ name, title, status, step }, index) => {
      const took = step?.finished ? ` · ${span((step.finished - step.started) * 1000)}` : "";
      const glyph = status === "completed" ? "check" : status === "failed" ? "x" : GLYPHS[name];
      return h(
        "div",
        { class: `station ${status}`, role: "listitem" },
        h("div", { class: "hub" }, icon(glyph)),
        h("div", { class: "what" }, h("span", { class: "no", text: String(index + 1).padStart(2, "0") }), h("b", { text: title }), h("span", { class: "small muted", text: WORDS[status] + took })),
      );
    }),
  );
}

/** Return a bar for a score from 0 to 100. */
const gauge = (score) => h("div", { class: `gauge ${score >= 80 ? "good" : ""}`, role: "img", "aria-label": `Score ${score} of 100` }, h("i", { style: `width:${Math.max(0, Math.min(100, score))}%` }));

/** Return the card of a station that has something to show: a wait, work, an error or a result. */
function card(doc, { title, status, step }) {
  const result = step?.result;
  const body =
    status === "waiting"
      ? drop(doc.waiting.title, doc.waiting.accept, async (file) => give(doc, await textOf(file)))
      : status === "running"
        ? waiting("The session works at this station.")
        : status === "failed"
          ? h("p", { class: "small bad-text", text: step.error || "The station failed." })
          : result?.kind === "upload"
            ? fileRow(result.value.name, result.value.size)
            : result?.kind === "document"
              ? fileRow(result.value.name, result.value.size, versionOf(result.value))
              : result?.kind === "model"
                ? [typeof result.value.score === "number" && gauge(result.value.score), structured(result.value), result.value.url && h("p", { class: "small" }, github(result.value.url, "Open the commit on GitHub"))]
                : result && structured(result.value);
  if (!body) return null;
  return h("section", { class: `card bench-card ${status}` }, h("div", { class: "row" }, h("h3", { text: title }), chip(WORDS[status], TONE[status], status === "running")), body, (step.notes || []).map((note) => h("p", { class: "small muted", text: note.text })));
}

/** Return the product: the newest Markdown document that the session attached. */
function product(doc) {
  const file = (doc.files || []).findLast((entry) => /\.md$/i.test(entry.name));
  if (!file) return h("section", { class: "card product" }, empty("No product yet", "The specification shows here when a station attaches it."));
  const path = filePath(doc.session_id, file.name);
  const key = `${path}:${file.version || 0}:${file.size}`;
  if (state.spec.key !== key) {
    state.spec = { key, text: null };
    read(path)
      .then((text) => state.spec.key === key && ((state.spec.text = text), draw()))
      .catch(() => {});
  }
  const head = h(
    "div",
    { class: "row product-head" },
    icon("file"),
    h("b", { class: "mono", text: file.name }),
    versionOf(file) && chip(versionOf(file), "amber"),
    h("span", { class: "small muted", text: `${size(file.size)} · ${ago(file.at * 1000)}` }),
    button("Download", { class: "small right", onclick: press(() => download(path, file.name)) }, "download"),
  );
  return h("section", { class: "card product" }, head, h("div", { class: "product-body" }, state.spec.text === null ? loading("Loading the document…") : markdown(state.spec.text)));
}

/** Return the floor without an order: what the factory does, and the zone that starts an order. */
function welcome() {
  const name = h("input", { class: "input", value: state.flow.inputs[0]?.default || "", "aria-label": "Product name" });
  return [
    h("div", { class: "hero" }, h("span", { class: "eyebrow", text: "Software factory" }), h("h1", { text: "A README goes in. A checked specification comes out." }), h("p", { class: "muted", text: state.flow.description })),
    line(null),
    h("section", { class: "card start" }, h("label", { class: "field" }, h("span", { text: "Product" }), name), drop("Give the README of the product", ".md,.txt", (file) => order(file, name.value.trim()))),
  ];
}

function draw() {
  const doc = state.doc;
  const data = JSON.stringify([state.id, doc, state.spec.key, state.spec.text?.length]);
  if (data === state.drawn) return; // a new draw would take the file dialog and the scroll position away
  state.drawn = data;
  if (!state.id) return fill(floor, welcome());
  if (!doc) return;
  const shipped = doc.status === "completed" && doc.result?.value;
  const head = h(
    "div",
    { class: "order-head" },
    h("div", {}, h("span", { class: "eyebrow", text: `Order ${doc.id.slice(0, 8)} · ${ago(doc.created * 1000)}` }), h("h1", { text: doc.input.product || state.flow.inputs[0]?.default || doc.title })),
    chip(doc.status, TONE[doc.status], doc.status === "running"),
    h("span", { class: "grow" }),
    doc.session_id && h("a", { class: "btn small", href: `/play#${doc.session_id}` }, icon("open"), h("span", { text: "Open the session" })),
    OPEN.has(doc.status) && button("Cancel", { class: "small", onclick: press(() => cancelRun(doc.id).then(refresh)) }, "stop"),
  );
  const done = shipped?.url && h("div", { class: "shipped" }, icon("check"), h("span", {}, "Shipped. ", h("code", { text: (shipped.files || []).join(", ") }), " is in the repository at ", github(shipped.url, shipped.commit || "the commit"), "."));
  const cards = stations(doc).map((station) => card(doc, station));
  fill(floor, head, done, doc.error && h("div", { class: "problem", text: doc.error }), line(doc), h("div", { class: "bench" }, product(doc), h("div", { class: "bench-cards" }, cards)));
}

function drawOrders() {
  const rows = state.runs.map((run) =>
    item(
      run.id === state.id,
      () => select(run.id),
      h("span", { class: "row" }, h("span", { class: "title", text: `Order ${run.id.slice(0, 8)}` }), h("span", { class: "right" }, chip(run.status, TONE[run.status], run.status === "running"))),
      stepBars(run),
      h("span", { class: "small muted", text: ago(run.created * 1000) }),
    ),
  );
  fill(orders, rows.length ? rows : empty("No orders", "Give a README to start the first order."));
}

/** Read the orders and the open order, then draw what changed. */
async function refresh() {
  state.runs = (await play("runs")).runs.filter((run) => run.workflow === WORKFLOW);
  drawOrders();
  if (state.id) {
    const doc = await play(`runs/${state.id}`);
    if (doc.id !== state.id) return;
    state.doc = doc;
    if (state.file?.id === doc.id && doc.status === "waiting") {
      const { given } = state.file;
      state.file = null;
      return give(doc, given);
    }
  }
  draw();
}

bar();
document.getElementById("new").addEventListener("click", () => select(null));
// A link to another order on the open page.
addEventListener("hashchange", () => orderId() !== state.id && select(orderId()));
fill(floor, loading("Loading the factory…"));
try {
  state.flow = await play(`workflows/${WORKFLOW}`);
  await refresh();
  // The page reads again while a run can change.
  setInterval(() => !document.hidden && (OPEN.has(state.doc?.status) || state.runs.some((run) => OPEN.has(run.status))) && refresh().catch(() => {}), 3000);
} catch (error) {
  fill(floor, empty("The factory has no workflow", `${error.message} Save the workflow ${WORKFLOW} on the Workflows page.`));
}
