// Run history, details and decisions.
import {
  OPEN, TONE, activities, activity, ago, api, back, bar, button, chip, date, decided, empty, field,
  filePath, fileRow, fill, fold, gate, h, icon, item, loadError, loading, padded, panel, panes, plural, poll,
  press, preview, redraw, select, selection, sessionPath, setSelection, structured, update, value, versionOf,
} from "./common.js";

const statuses = ["queued", "running", "waiting", "completed", "failed", "cancelled"];
const state = { id: selection("run"), workflow: "", status: "", listRead: 0, detailRead: 0 };
const status = (name) => chip(name, TONE[name], name === "running");
const flow = select("Workflow", [["", "All workflows"]], (name) => { state.workflow = name; void readList(); });
const filter = select("Status", [["", "All statuses"], ...statuses.map((name) => [name, name])], (name) => { state.status = name; void readList(); });
const view = panes("Runs", "See current runs and past runs. Open a run to see its work.", h("div", { class: "filters" }, field("Workflow", flow), field("Status", filter)));
const failure = h("p", { class: "status-line bad-text", role: "status" });
const body = h("div");
view.detail.append(failure, body);

function open(id) {
  state.id = id;
  setSelection("run", id);
  failure.textContent = "";
  view.reveal(Boolean(id));
  fill(body, id ? [back(() => open("")), loading("Loading the run…")] : empty("Select a run", "Its steps, files and status show here."));
  void readList();
  if (id) void readDetail();
}

async function readList() {
  const number = ++state.listRead;
  const filters = new URLSearchParams();
  if (state.workflow) filters.set("workflow", state.workflow);
  if (state.status) filters.set("status", state.status);
  try {
    const [result, workflows] = await Promise.all([api(`runs?${filters}`), api("workflows").catch(() => null)]);
    if (number !== state.listRead) return;
    view.message.textContent = "";
    const rows = result.runs || [];
    const names = new Map((workflows?.workflows || []).map(({ name, title }) => [name, title]));
    // A workflow can have runs after its source was removed.
    for (const option of flow.options) if (option.value && !names.has(option.value)) names.set(option.value, option.textContent);
    for (const row of rows) if (!names.has(row.workflow)) names.set(row.workflow, row.workflow);
    update(flow, [...names], () => [h("option", { value: "", text: "All workflows" }), [...names].map(([name, title]) => h("option", { value: name, text: title }))]);
    flow.value = state.workflow;
    update(view.list, [rows, state.id, Math.floor(Date.now() / 60000)], () => rows.length ? rows.map((run) => item(state.id === run.id, () => open(run.id),
      h("span", { class: "row" }, h("b", { class: "title", text: run.title || run.workflow }), h("span", { class: "right" }, status(run.status))),
      h("span", { class: "row meta" }, h("span", { text: run.workflow }), h("span", { class: "right", text: ago(run.created * 1000) })),
    )) : empty("No runs", filters.size ? "No run matches these filters." : "Start a run on the Workflows page."));
  } catch (error) {
    if (number !== state.listRead) return;
    loadError(view.message, error);
    if (view.list.querySelector(".spin")) fill(view.list, empty("Runs did not load", "The page will try again."));
  }
}

function documentRow(session, file) {
  if (!session || !file?.name) return null;
  const path = filePath(session, file.name);
  const row = fileRow(file.name, file.size, versionOf(file));
  row.className = "row card file-row";
  row.append(button("View", { class: "small", onclick: press(() => preview(path, file.name)) }, "eye"));
  return row;
}

function stepCard(run, step, index) {
  const result = step.result;
  const marker = { completed: "check", failed: "x", waiting: "eye" }[step.status];
  return h("div", { class: `step ${step.status}` },
    h("span", { class: "mark" }, marker ? icon(marker) : String(index + 1)),
    h("div", { class: "step-body" },
      h("div", { class: "row wrap" }, h("b", { text: step.title || step.name }), status(step.status), h("span", { class: "meta", text: date(step.started) })),
      step.error && h("pre", { class: "problem", text: step.error }),
      (step.notes || []).map((note) => h("p", { class: "note", text: note.text })),
      result?.kind === "document" ? documentRow(run.session_id, result.value) :
        result?.kind === "decision" ? decided(result.value) : result && padded(structured(result.value ?? result)),
    ),
  );
}

function draw(run) {
  const steps = [...(run.activities || run.steps || [])].sort((a, b) => a.seq - b.seq);
  const rows = (run.turns || []).flatMap(activities);
  const session = run.session_id;
  const waiting = run.waiting;
  const current = body.querySelector(".gate textarea");
  const active = current && current === document.activeElement;
  const caret = active ? [current.selectionStart, current.selectionEnd] : null;
  const draft = body.dataset.gate === `${run.id}/${waiting?.key}` ? current?.value : "";
  redraw(body, view.detail, "details", () => update(body, run, () => panel(
    [h("div", { class: "row wrap" }, back(() => open("")), status(run.status)), h("h2", { text: run.title || run.workflow }),
      h("span", { class: "meta", text: [run.workflow, run.version && `version ${run.version}`, run.uses?.model && `model ${run.uses.model}`, date(run.created), run.trigger?.id].filter(Boolean).join(" · ") }),
      h("div", { class: "row actions" },
        session && h("a", { href: sessionPath(session), class: "btn" }, icon("open"), "Open session"),
        OPEN.has(run.status) && button("Cancel", { class: "danger", onclick: press(() => action(run.id, "cancel")) }, "stop"),
        run.status === "failed" && button("Retry", { onclick: press(() => action(run.id, "retry")) }, "fork"),
      ),
    ],
    waiting && gate(run, refresh),
    run.error && h("pre", { class: "problem", text: run.error }),
    Object.keys(run.input || {}).length > 0 && fold("Input", "", padded(value(run.input))),
    h("h3", { text: "Steps" }),
    steps.length ? h("div", { class: "timeline" }, steps.map((step, index) => stepCard(run, step, index))) : h("p", { class: "muted", text: "This run has no recorded steps." }),
    (run.notes || []).map((note) => h("p", { class: "note", text: note.text })),
    run.result && [h("h3", { text: "Result" }), padded(structured(run.result.value ?? run.result))],
    run.files?.length > 0 && [h("h3", { text: "Documents" }), run.files.map((file) => documentRow(session, file))],
    rows.length > 0 && fold("Vis activities", plural(rows.length, "step"), h("div", { class: "acts" }, rows.map(activity))),
    run.sessions?.length > 0 && fold("Related sessions", "", h("div", { class: "stack" }, run.sessions.map((related) => {
      const id = typeof related === "string" ? related : related.id || related.session_id;
      return id && id !== session && h("a", { href: sessionPath(id), text: related.title || id });
    }))),
  )));
  if (waiting) {
    body.dataset.gate = `${run.id}/${waiting.key}`;
    const next = body.querySelector(".gate textarea");
    if (next && current !== next && draft !== undefined) {
      next.value = draft;
      if (active && caret) {
        next.focus({ preventScroll: true });
        next.setSelectionRange(...caret);
      }
    }
  } else {
    delete body.dataset.gate;
  }
}

async function action(id, name) {
  if (name === "cancel" && !confirm("Cancel this run?")) return;
  const result = await api(`runs/${encodeURIComponent(id)}/${name}`, { method: "POST", body: {} });
  if (name === "retry" && result.id && result.id !== id && state.id === id) open(result.id);
  else await refresh();
}

async function readDetail() {
  const id = state.id;
  if (!id) return;
  const number = ++state.detailRead;
  try {
    const run = await api(`runs/${encodeURIComponent(id)}`);
    if (id !== state.id || number !== state.detailRead) return;
    failure.textContent = "";
    draw(run);
  } catch (error) {
    if (id !== state.id || number !== state.detailRead) return;
    loadError(failure, error);
    if (body.querySelector(".empty .spin")) fill(body, back(() => open("")), empty("The run did not load", "The page will try again."));
  }
}

async function refresh() {
  await Promise.all([readList(), readDetail()]);
}

view.reveal(Boolean(state.id));
fill(body, state.id ? [back(() => open("")), loading("Loading the run…")] : empty("Select a run", "Its steps, files and status show here."));
window.addEventListener("hashchange", () => open(selection("run")));
bar();
poll(refresh);
