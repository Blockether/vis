// Workflow source, inputs and triggers.
import {
  api, back, bar, button, chip, date, dialog, empty, field, fill, h, item, loadError, loading,
  panel, panes, poll, press, selection, setSelection, structured, toast, update,
} from "./common.js";

const state = { name: selection("workflow"), list: [], triggers: [], about: null, saved: "", source: null, listRead: 0, detailRead: 0 };
const view = panes("Workflows", "Edit workflow code, check its triggers or start a run.", h("h2", { text: "Workflow files" }));
const failure = h("p", { class: "status-line bad-text", role: "status" });
const body = h("div");
view.detail.append(failure, body);
const dirty = () => state.source && state.source.value !== state.saved;

window.addEventListener("beforeunload", (event) => {
  if (!dirty()) return;
  event.preventDefault();
  event.returnValue = true;
});

function open(name) {
  if (name === state.name && state.about) return;
  if (dirty() && !confirm("Discard the source changes?")) return;
  state.name = name;
  state.about = state.source = null;
  state.saved = "";
  failure.textContent = "";
  setSelection("workflow", name);
  view.reveal(Boolean(name));
  fill(body, name ? [back(() => open("")), loading("Loading the workflow…")] : empty("Select a workflow", "Open a file to see and edit its source."));
  drawList();
  if (name) void readDetail();
}

function drawList() {
  update(view.list, [state.list, state.name], () => state.list.length ? state.list.map((row) => item(state.name === row.name, () => open(row.name),
    h("span", { class: "row wrap" }, h("b", { class: "title", text: row.title || row.name }), row.error && chip("Invalid", "bad")),
    h("span", { class: "meta", text: `${row.name}.py` }),
  )) : empty("No workflows", "Add a workflow file to the project."));
}

async function readList() {
  const number = ++state.listRead;
  try {
    const result = await api("workflows");
    if (number !== state.listRead) return;
    state.list = result.workflows || [];
    view.message.textContent = "";
    drawList();
  } catch (error) {
    if (number !== state.listRead) return;
    loadError(view.message, error);
    if (view.list.querySelector(".spin")) fill(view.list, empty("Workflows did not load", "The page will try again."));
  }
}

async function readTriggers() {
  try {
    state.triggers = (await api("triggers")).triggers || [];
    if (state.about) drawFacts();
  } catch (error) {
    const status = body.querySelector(".trigger-status");
    if (status) loadError(status, error);
  }
}

async function readDetail() {
  const name = state.name;
  const number = ++state.detailRead;
  try {
    const about = await api(`workflows/${encodeURIComponent(name)}`);
    if (name !== state.name || number !== state.detailRead) return;
    failure.textContent = "";
    if (!state.about) return draw(about);
    // Another writer can update the file. Keep a draft based on the old source.
    if (!dirty() && state.saved !== about.source) {
      state.saved = about.source;
      state.source.value = about.source;
    }
    state.about = about;
    drawFacts();
  } catch (error) {
    if (name !== state.name || number !== state.detailRead) return;
    loadError(failure, error);
    if (!state.source) fill(body, back(() => open("")), empty("The workflow did not load", "The page will try again."));
  }
}

function draw(about) {
  state.about = about;
  state.saved = about.source;
  const source = h("textarea", { class: "input source", wrap: "off", spellcheck: "false" });
  state.source = source;
  source.value = about.source;
  fill(body, panel(
    [h("div", { class: "row" }, back(() => open("")), h("h2", { id: "workflow-title" })),
      h("span", { class: "meta", text: `${state.name}.py` }),
      h("div", { class: "row actions" }, button("Save", { onclick: press(save) }, "save"), button("Run", { class: "primary", onclick: press(run) }, "play")),
    ],
    field("Source", source),
    h("div", { class: "stack", id: "workflow-facts" }),
    h("div", { class: "stack" }, h("h3", { text: "Triggers" }), h("p", { class: "trigger-status bad-text", role: "status" }), h("div", { class: "stack", id: "workflow-triggers" })),
  ));
  source.addEventListener("input", drawFacts);
  drawFacts();
}

function triggerCard(trigger) {
  return h("div", { class: "trigger stack" },
    h("div", { class: "row wrap" }, chip(trigger.kind === "cron" ? "Cron · UTC" : "Event"), h("b", { text: trigger.cron || trigger.event })),
    h("div", { class: "meta" }, `Last: ${date(trigger.last_fired) || "not yet"} · Next: ${date(trigger.next_due) || (trigger.kind === "on" ? "on event" : "not set")}`),
    trigger.last_run && h("a", { href: `/factory#run=${encodeURIComponent(trigger.last_run)}`, text: "Open last run" }),
    Object.keys(trigger.input || {}).length > 0 && structured(trigger.input),
  );
}

function drawFacts() {
  const about = state.about;
  if (!about) return;
  body.querySelector("#workflow-title").textContent = about.title || state.name;
  update(body.querySelector("#workflow-facts"), [about.title, about.description, about.inputs, dirty()], () => [
    dirty() && h("p", { class: "status-line", text: "Unsaved source changes." }),
    about.description && h("p", { text: about.description }),
    h("h3", { text: "Inputs" }),
    about.inputs?.length ? h("div", { class: "row wrap" }, about.inputs.map((input) => chip(input.name, "mono"))) : h("p", { class: "muted", text: "This workflow has no inputs." }),
  ]);
  const timing = new Map(state.triggers.filter((trigger) => trigger.workflow === state.name).map((row) => [row.id, row]));
  const triggers = (about.triggers || []).map((trigger) => ({ ...timing.get(trigger.id), ...trigger }));
  body.querySelector(".trigger-status").textContent = "";
  update(body.querySelector("#workflow-triggers"), triggers, () => triggers.length ? triggers.map(triggerCard) : h("p", { class: "muted", text: "This workflow has no triggers." }));
}

async function save() {
  const name = state.name;
  const source = state.source.value;
  const saved = await api(`workflows/${encodeURIComponent(name)}`, { method: "PUT", body: { source, base: state.saved } });
  if (name === state.name) {
    ++state.detailRead;
    state.saved = source;
    state.about = { ...state.about, ...saved, source };
    drawFacts();
  }
  toast("Workflow saved.", "good");
  await Promise.all([readList(), readTriggers()]);
}

async function run() {
  if (dirty()) await save();
  const { name, about } = state;
  if (!about || dirty()) return;
  const inputs = (about.inputs || []).map((input) => {
    // A typed input other than text is JSON, so a number stays a number.
    const text = ["int", "float", "bool", "list", "dict"].includes(input.type) ? false : typeof input.default === "string" || input.default == null;
    const control = h("textarea", { class: "input", rows: "2" });
    control.value = text ? input.default || "" : JSON.stringify(input.default);
    return { name: input.name, control, text };
  });
  const fields = inputs.map(({ name, control }) => field(name, control, "mono"));
  const start = async () => {
    const input = Object.fromEntries(inputs.map(({ name, control, text }) => [name, text ? control.value : JSON.parse(control.value)]));
    const created = await api("runs", { method: "POST", body: { workflow: name, input } });
    location.assign(`/factory#run=${encodeURIComponent(created.id)}`);
  };
  dialog(`Run · ${about.title}`, fields.length ? fields : h("p", { text: "This workflow has no inputs." }), [button("Start run", { class: "primary", onclick: press(start) }, "play")]);
}

async function refresh() {
  await Promise.all([readList(), readTriggers(), state.name && readDetail()]);
}

view.reveal(Boolean(state.name));
fill(body, state.name ? [back(() => open("")), loading("Loading the workflow…")] : empty("Select a workflow", "Open a file to see and edit its source."));
window.addEventListener("hashchange", () => open(selection("workflow")));
bar();
poll(refresh);
