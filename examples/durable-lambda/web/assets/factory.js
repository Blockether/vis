// The factory page: the workflow `song_factory` as a production line.
// An order is one run. A prompt goes in. A person approves the song sheet and then the song. The song goes to GitHub as code.
import { OPEN, TONE, ago, bar, button, cancelRun, chip, decided, download, empty, filePath, fileRow, fill, gate, github, guard, h, icon, item, loading, markdown, play, press, read, retryRun, size, span, stepBars, structured, versionOf, waiting } from "/play/assets/common.js";

const WORKFLOW = "song_factory";
// The glyph of each station, by the name of its workflow activity.
const GLYPHS = { specify: "pencil", approve_sheet: "eye", rework: "fork", record: "play", approve_song: "eye", ship: "globe" };
const WORDS = { idle: "Idle", skipped: "Not needed", running: "At work", waiting: "Waits for a decision", completed: "Done", failed: "Failed", cancelled: "Stopped" };
// The longest brief, in characters. A brief is a part of the first request to the model.
const BRIEF_LIMIT = 20000;

const orders = document.getElementById("orders");
const floor = document.getElementById("floor");
// The fragment goes into request paths: accept only a run id.
const orderId = () => /^#([0-9a-f]{32})$/.exec(location.hash)?.[1] || null;
// flow: the workflow. runs: its runs. id, doc: the open order.
// spec: the text of the shown document, with the key of its version. drawn: the data of the last draw.
const state = { flow: null, runs: [], id: orderId(), doc: null, spec: { key: "", text: null }, drawn: "" };

/** Return the stations of an order: each workflow activity with the state of its step. */
function stations(doc) {
  const steps = doc?.activities || [];
  const found = state.flow.activities.map((activity) => ({ ...activity, step: steps.findLast((step) => step.name === activity.name) }));
  const last = found.findLastIndex((station) => station.step);
  // A station before the last used one that has no step: the workflow did not need it.
  return found.map((station, index) => ({ ...station, status: station.step?.status || (index < last ? "skipped" : "idle") }));
}

/** Make a new order from a prompt. Without a prompt, the workflow uses its own. */
async function order(prompt) {
  const run = await play("runs", { method: "POST", body: { workflow: WORKFLOW, input: prompt ? { prompt } : {} } });
  await select(run.id);
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

/** Return the card of a station that has something to show: a gate, work, an error or a result. */
function card(doc, { title, status, step }) {
  const result = step?.result;
  const body =
    status === "waiting"
      ? gate(doc, refresh)
      : status === "running"
        ? waiting("The session works at this station.")
        : status === "failed"
          ? h("p", { class: "small bad-text", text: step.error || "The station failed." })
          : result?.kind === "decision"
            ? decided(result.value)
            : result?.kind === "document"
              ? fileRow(result.value.name, result.value.size, versionOf(result.value))
              : result?.kind === "model"
                ? [structured(result.value), result.value.url && h("p", { class: "small" }, github(result.value.url, "Open the commit on GitHub"))]
                : result && structured(result.value);
  if (!body) return null;
  return h("section", { class: `card bench-card ${status}` }, h("div", { class: "row" }, h("h3", { text: title }), chip(WORDS[status], TONE[status], status === "running")), body, (step.notes || []).map((note) => h("p", { class: "small muted", text: note.text })));
}

/** Return the product: the newest Markdown document that the session attached. */
function product(doc) {
  const file = (doc.files || []).findLast((entry) => /\.md$/i.test(entry.name));
  if (!file) return h("section", { class: "card product" }, empty("No product yet", "The song sheet shows here when a station attaches it."));
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

/** Put the text of a brief file in the box. The person can change it before the order starts. */
async function useFile(box, file) {
  if (!file) return;
  if (file.size > BRIEF_LIMIT) throw new Error(`The file is larger than ${BRIEF_LIMIT / 1000} kB.`);
  const text = (await file.text()).trim();
  if (!text) throw new Error("The file is empty.");
  box.value = text;
  box.focus();
}

/** Return the floor without an order: what the factory does, and the card that starts an order. */
function welcome() {
  const brief = h("textarea", { class: "input brief", rows: 10, maxlength: BRIEF_LIMIT, "aria-label": "Brief", text: state.flow.inputs[0]?.default || "" });
  const file = h("input", { type: "file", accept: ".md,.txt", hidden: true, "aria-label": "Brief file" });
  const choose = button("Use a file", { onclick: () => file.click() }, "upload");
  const take = (given) => guard(choose, () => useFile(brief, given));
  // An empty value lets the person choose the same file again.
  file.addEventListener("change", () => take(file.files[0]).then(() => (file.value = "")));
  const start = h(
    "section",
    { class: "card start" },
    h("label", { class: "field" }, h("span", { text: "The song that you want" }), brief),
    h("div", { class: "row wrap" }, button("Start the order", { class: "primary", onclick: press(() => order(brief.value.trim())) }, "play"), choose, file, h("span", { class: "small muted", text: "Write the brief, or drop a .md or .txt file on it." })),
  );
  start.addEventListener("dragover", (event) => {
    event.preventDefault();
    start.classList.add("over");
  });
  start.addEventListener("dragleave", () => start.classList.remove("over"));
  start.addEventListener("drop", (event) => {
    event.preventDefault();
    start.classList.remove("over");
    take(event.dataTransfer.files[0]);
  });
  return [
    h("div", { class: "hero" }, h("span", { class: "eyebrow", text: "Song factory" }), h("h1", { text: "A prompt goes in. You approve the song sheet and the song." }), h("p", { class: "muted", text: state.flow.description })),
    line(null),
    start,
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
    h("div", {}, h("span", { class: "eyebrow", text: `Order ${doc.id.slice(0, 8)} · ${ago(doc.created * 1000)}` }), h("h1", { class: "clip", text: shipped?.song || doc.input.prompt || state.flow.inputs[0]?.default || doc.title })),
    chip(doc.status, TONE[doc.status], doc.status === "running"),
    h("span", { class: "grow" }),
    doc.session_id && h("a", { class: "btn small", href: `/play#${doc.session_id}` }, icon("open"), h("span", { text: "Open the session" })),
    OPEN.has(doc.status) && button("Cancel", { class: "small", onclick: press(() => cancelRun(doc.id).then(refresh)) }, "stop"),
    doc.status === "failed" && button("Retry in a fork", { class: "small", onclick: press(() => retryRun(doc.id).then(refresh)) }, "fork"),
  );
  const done = shipped?.url && h("div", { class: "shipped" }, icon("check"), h("span", {}, "Shipped. The song ", h("code", { text: shipped.song }), " is in the repository at ", github(shipped.url, shipped.commit || "the commit"), "."));
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
  fill(orders, rows.length ? rows : empty("No orders", "Write a prompt to start the first order."));
}

/** Read the orders and the open order, then draw what changed. */
async function refresh() {
  state.runs = (await play("runs")).runs.filter((run) => run.workflow === WORKFLOW);
  drawOrders();
  if (state.id) {
    const doc = await play(`runs/${state.id}`);
    if (doc.id !== state.id) return;
    state.doc = doc;
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
