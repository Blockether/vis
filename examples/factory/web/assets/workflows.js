// Workflow versions, inputs and triggers. The code changes in the Vis project of the workflows, not on this page.
import {
  api, back, bar, button, chip, date, dialog, empty, field, fill, h, item, loadError, loading,
  panel, panes, poll, press, selection, setSelection, structured, toast, update,
} from "./common.js";

// `version` is the version on the page. Without it, the page follows the current version.
const state = { name: selection("workflow"), version: null, project: "Workflows", list: [], triggers: [], about: null, listRead: 0, detailRead: 0 };
const view = panes("Workflows", "See the code of a workflow and its versions, check its triggers or start a run.", h("h2", { text: "Workflows" }));
const failure = h("p", { class: "status-line bad-text", role: "status" });
const body = h("div");
const waiting = h("p", { class: "status-line", role: "status" });
view.detail.append(failure, body);
view.message.after(waiting);
const nothing = () => empty("Select a workflow", `Open a workflow to see its code. To change the code, open a session in the Vis project ${state.project}.`);
const origins = { seed: () => "first version", project: () => "changed in the project", restore: (row) => `restored from version ${row.from}`, delete: () => "removed" };
const origin = (row) => (origins[row.origin] || (() => row.origin))(row);

function open(name) {
  if (name === state.name && state.about) return;
  state.name = name;
  state.about = state.version = null;
  failure.textContent = "";
  setSelection("workflow", name);
  view.reveal(Boolean(name));
  fill(body, name ? [back(() => open("")), loading("Loading the workflow…")] : nothing());
  drawList();
  if (name) void readDetail();
}

function drawList() {
  update(view.list, [state.list, state.name], () => state.list.length ? state.list.map((row) => item(state.name === row.name, () => open(row.name),
    h("span", { class: "row wrap" }, h("b", { class: "title", text: row.title || row.name }),
      row.deleted && chip("Removed"), row.error && chip("Invalid", "bad"), row.draft && chip("File error", "bad")),
    h("span", { class: "meta", text: row.version ? `${row.name}.py · version ${row.version}` : `${row.name}.py · no version` }),
  )) : empty("No workflows", `Add a file <name>.py in the Vis project ${state.project}.`));
}

async function readList() {
  const number = ++state.listRead;
  try {
    const result = await api("workflows");
    if (number !== state.listRead) return;
    state.list = result.workflows || [];
    state.project = result.project || state.project;
    view.message.textContent = "";
    waiting.textContent = result.editing ? `A session of the project ${state.project} has a turn in progress. The factory reads the files after the turn.` : "";
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
    if (state.about) draw();
  } catch (error) {
    const status = body.querySelector(".trigger-status");
    if (status) loadError(status, error);
  }
}

async function readDetail() {
  const name = state.name;
  const number = ++state.detailRead;
  // A new file that does not load has no version: show its error from the list.
  const row = state.list.find((row) => row.name === name);
  if (row && !row.version) {
    state.about = null;
    return update(body, [name, row.draft], () => [back(() => open("")), empty(`${name}.py does not load`, `${row.draft}. Correct the file in the Vis project ${state.project}.`)]);
  }
  try {
    const about = await api(`workflows/${encodeURIComponent(name)}${state.version ? `?version=${state.version}` : ""}`);
    if (name !== state.name || number !== state.detailRead) return;
    failure.textContent = "";
    state.about = about;
    draw();
  } catch (error) {
    if (name !== state.name || number !== state.detailRead) return;
    loadError(failure, error);
    if (!state.about) update(body, [name, "error"], () => [back(() => open("")), empty("The workflow did not load", "The page will try again.")]);
  }
}

function triggerCard(trigger) {
  return h("div", { class: "trigger stack" },
    h("div", { class: "row wrap" }, chip(trigger.kind === "cron" ? "Cron · UTC" : "Event"), h("b", { text: trigger.cron || trigger.event })),
    h("div", { class: "meta" }, `Last: ${date(trigger.last_fired) || "not yet"} · Next: ${date(trigger.next_due) || (trigger.kind === "on" ? "on event" : "not set")}`),
    trigger.last_run && h("a", { href: `/factory#run=${encodeURIComponent(trigger.last_run)}`, text: "Open last run" }),
    Object.keys(trigger.input || {}).length > 0 && structured(trigger.input),
  );
}

function versions(about) {
  const select = h("select", { class: "input versions", "aria-label": "Version", onchange: () => show(Number(select.value)) },
    about.versions.map((row) => h("option", { value: row.version, text: `Version ${row.version}${row.version === about.current ? " · current" : ""} · ${date(row.at)} · ${origin(row)}` })));
  select.value = about.version;
  return select;
}

function show(version) {
  state.version = version === state.about.current ? null : version;
  void readDetail();
}

function draw() {
  const { about, name } = state;
  const old = about.version !== about.current;
  const live = !old && !about.deleted && !about.error;
  const timing = new Map(state.triggers.filter((trigger) => trigger.workflow === name).map((row) => [row.id, row]));
  const triggers = (about.triggers || []).map((trigger) => ({ ...(live ? timing.get(trigger.id) : {}), ...trigger }));
  update(body, [about, triggers], () => panel(
    [h("div", { class: "row" }, back(() => open("")), h("h2", { text: about.title || name })),
      h("span", { class: "meta", text: `${name}.py` }),
      h("div", { class: "row actions" },
        versions(about),
        about.origin !== "delete" && (old || about.deleted) && button("Restore", { onclick: press(restore) }, "upload"),
        live && button("Run", { class: "primary", onclick: press(run) }, "play"),
        !about.deleted && button("Remove", { class: "quiet", onclick: press(remove) }, "trash")),
    ],
    h("p", { class: "status-line" }, `This page does not change the code. Open a session in the Vis project ${state.project} and change ${name}.py there. The factory keeps each change as a new version. `, h("a", { href: "/factory/vis", text: "Open Vis" })),
    about.draft && h("p", { class: "status-line bad-text", role: "status", text: `The file ${name}.py of the project does not load: ${about.draft}. ${about.deleted ? "The workflow stays removed." : `Runs use version ${about.current}.`}` }),
    about.error && h("p", { class: "status-line bad-text", role: "status", text: `This version does not load: ${about.error}.` }),
    about.deleted && h("p", { class: "status-line", text: old ? "This workflow is removed. Restore this version to use it again." : "This workflow is removed. Select an earlier version and restore it to use the workflow again." }),
    old && !about.deleted && h("p", { class: "status-line", text: `This is version ${about.version}. Runs use version ${about.current}.` }),
    about.origin !== "delete" && field(`Source · version ${about.version}`, h("pre", { class: "input source", tabindex: "0", text: about.source })),
    about.origin !== "delete" && h("div", { class: "stack" },
      about.description && h("p", { text: about.description }),
      h("h3", { text: "Inputs" }),
      about.inputs?.length ? h("div", { class: "row wrap" }, about.inputs.map((input) => chip(input.name, "mono"))) : h("p", { class: "muted", text: "This workflow has no inputs." })),
    about.origin !== "delete" && h("div", { class: "stack" }, h("h3", { text: "Triggers" }), h("p", { class: "trigger-status bad-text", role: "status" }),
      h("div", { class: "stack" }, triggers.length ? triggers.map(triggerCard) : h("p", { class: "muted", text: "This workflow has no triggers." }))),
  ));
}

async function restore() {
  const { name, about } = state;
  await api(`workflows/${encodeURIComponent(name)}/restore`, { method: "POST", body: { version: about.version } });
  if (name === state.name) state.version = null;
  toast(`Version ${about.version} is the current code again, as a new version.`, "good");
  await refresh();
}

async function remove() {
  const name = state.name;
  if (!confirm(`Remove the workflow ${name}? Its versions stay, and you can restore one.`)) return;
  await api(`workflows/${encodeURIComponent(name)}`, { method: "DELETE" });
  if (name === state.name) state.version = null;
  toast("Workflow removed.", "good");
  await refresh();
}

async function run() {
  const { name, about } = state;
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
  dialog(`Run · ${about.title} · version ${about.version}`, fields.length ? fields : h("p", { text: "This workflow has no inputs." }), [button("Start run", { class: "primary", onclick: press(start) }, "play")]);
}

async function refresh() {
  await Promise.all([readList(), readTriggers()]);
  if (state.name) await readDetail();
}

view.reveal(Boolean(state.name));
fill(body, state.name ? [back(() => open("")), loading("Loading the workflow…")] : nothing());
window.addEventListener("hashchange", () => open(selection("workflow")));
bar();
poll(refresh);
