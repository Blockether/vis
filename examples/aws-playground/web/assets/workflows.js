// The workflows page: write a workflow in Python, add activities, run it and watch the run.
/* global CodeMirror */
import { OPEN, TONE, activities, activity, ago, artifactPath, bar, button, cancelRun, chip, clock, dialog, drop, empty, field, fileRow, fill, giveFile, guard, h, icon, item, loading, padded, play, plural, press, preview, redraw, span, stepBars, structured, textOf, toast, turnsOf, value, versionOf, waiting } from "/play/assets/common.js";

const studio = document.querySelector("#studio");
const list = document.querySelector("#list");
const head = document.querySelector("#editor-head");
const problem = document.querySelector("#problem");
const outline = document.querySelector("#outline");
const runsHead = document.querySelector("#runs-head");
const runsBody = document.querySelector("#runs");
const state = { workflows: [], name: "", saved: "", about: null, runs: [], run: null, doc: null, turns: [], all: false };
const status = (name) => chip(name, `right ${TONE[name] || ""}`, OPEN.has(name) && name !== "waiting");
/** Return a Python name for a title: lower case, with `_` between the words and a letter first. */
const pythonName = (title) => title.toLowerCase().replace(/[^a-z0-9]+/g, "_").replace(/^[^a-z]+|_+$/g, "");
/** Return a part that opens when a person clicks its label. */
const more = (label, ...body) => h("details", {}, h("summary", { class: "small muted", style: "cursor:pointer", text: label }), ...body);
const notes = (rows) => (rows || []).map((note) => h("div", { class: "note small", text: note }));

const TEMPLATE = (name, title) => `"""${title}."""

from typing import Literal

from pydantic import BaseModel, Field

from playground.workflows import activity, workflow


class Decision(BaseModel):
    decision: Literal["go", "stop"] = Field(description="go: the request is clear and useful. stop: it is not")
    reason: str


@activity(title="Decide")
def decide(ctx, request: str) -> Decision:
    return ctx.ask(f"Read this request and decide. Do not write code.\\n\\n{request}", Decision)


@activity(title="Write the report")
def report(ctx):
    return ctx.document("REPORT.md", "Write a short report about the request and your decision.")


@workflow(title="${title}", description="Decide about a request, then write a report.")
def ${name}(ctx, request: str = "Describe the request here."):
    made = decide(ctx, request)
    if made.decision == "stop":
        ctx.note(f"Stopped: {made.reason}")
        return {"decision": "stop", "reason": made.reason}
    return {"decision": "go", "artifacts": [report(ctx).name]}
`;

const SNIPPETS = {
  decision: (name, title, prompt, artifact) => `class ${artifact}(BaseModel):
    decision: Literal["yes", "no"] = Field(description="yes: ... no: ...")
    reason: str


@activity(title=${JSON.stringify(title)})
def ${name}(ctx) -> ${artifact}:
    return ctx.ask(${JSON.stringify(prompt)}, ${artifact})
`,
  document: (name, title, prompt, artifact) => `@activity(title=${JSON.stringify(title)})
def ${name}(ctx):
    return ctx.document(${JSON.stringify(artifact)}, ${JSON.stringify(prompt)})
`,
  answer: (name, title, prompt) => `@activity(title=${JSON.stringify(title)})
def ${name}(ctx) -> str:
    return ctx.ask(${JSON.stringify(prompt)})
`,
};

// -- the editor ------------------------------------------------------------------------------

// CodeMirror 5 comes from plain scripts of the page, before this module.
const editor = CodeMirror(document.querySelector("#editor"), {
  mode: "python",
  lineNumbers: true,
  indentUnit: 4,
  tabSize: 4,
  indentWithTabs: false,
  matchBrackets: true,
  autoCloseBrackets: true,
  styleActiveLine: true,
  lineWrapping: window.innerWidth < 820,
  extraKeys: {
    Tab: (cm) => (cm.somethingSelected() ? cm.indentSelection("add") : cm.replaceSelection("    ", "end")),
    "Shift-Tab": (cm) => cm.indentSelection("subtract"),
    "Cmd-/": "toggleComment",
    "Ctrl-/": "toggleComment",
    "Cmd-S": () => save(),
    "Ctrl-S": () => save(),
    "Cmd-Enter": () => run(),
    "Ctrl-Enter": () => run(),
  },
});
// The line that shows the error of the last check, and the timer of the next check.
let marked = null;
let timer = 0;
const dirty = () => editor.getValue() !== state.saved;

editor.on("change", () => {
  clearTimeout(timer);
  timer = setTimeout(check, 450);
  drawHead();
});

/** Ask the worker about the source: the activities and the inputs, or the error with its line. */
async function check() {
  if (marked !== null) editor.removeLineClass(marked, "background", "cm-error-line");
  marked = null;
  try {
    state.about = await play("workflows/check", { method: "POST", body: { source: editor.getValue() } });
    problem.hidden = true;
  } catch (error) {
    state.about = null;
    problem.hidden = false;
    problem.textContent = error.message;
    const line = /^line (\d+)/.exec(error.message);
    if (line) marked = editor.addLineClass(Number(line[1]) - 1, "background", "cm-error-line");
  }
  drawOutline();
  drawHead();
}

function drawOutline() {
  const about = state.about;
  if (!about) return fill(outline, h("span", { class: "small muted", text: "Correct the source to see the activities." }));
  const none = (text) => h("span", { class: "small muted", text });
  fill(
    outline,
    h("h4", { text: "Activities" }),
    about.activities.map((step) => h("button", { class: "chip amber", type: "button", title: `Go to ${step.name}`, text: step.title, onclick: () => jump(`def ${step.name}(`) })),
    !about.activities.length && none("none yet"),
    h("h4", { style: "margin-left:10px", text: "Inputs" }),
    about.inputs.map((input) => chip(input.name, "mono")),
    !about.inputs.length && none("none"),
  );
}

/** Put the cursor on the first row that has the text. */
function jump(text) {
  const at = editor.getValue().split("\n").findIndex((row) => row.includes(text));
  if (at < 0) return;
  editor.setCursor(at, 0);
  editor.scrollIntoView({ line: at, ch: 0 }, 120);
  editor.focus();
}

function drawHead() {
  const about = state.about;
  fill(head,
    h("span", { class: "name clip", text: state.name ? `${state.name}.py` : "no workflow" }),
    dirty() && chip("not saved", "amber"),
    about && h("span", { class: "small muted clip only-wide", text: about.title }),
    h("span", { class: "right row wrap" },
      button("Add activity", { class: "small", onclick: addActivity, disabled: !state.name }, "plus"),
      button("Save", { class: "small", id: "save", onclick: () => save(), disabled: !state.name || !dirty() }, "save"),
      button("Run", { class: "small primary", onclick: () => run(), disabled: !state.name }, "play"),
      button("", { class: "small quiet danger", title: "Delete the workflow", "aria-label": "Delete the workflow", onclick: remove, disabled: !state.name }, "trash"),
      // Only a middle width has this button: there the runs are an overlay. A phone has tabs.
      button("Runs", { class: "small only-narrow only-wide", onclick: () => studio.classList.toggle("show-runs") }, "list"),
    ),
  );
}

// -- workflows -------------------------------------------------------------------------------

async function loadList() {
  state.workflows = (await play("workflows")).workflows;
  drawList();
}

function drawList() {
  fill(
    list,
    state.workflows.map((flow) =>
      item(
        flow.name === state.name,
        () => open(flow.name),
        h("span", { class: "title clip", text: flow.title }),
        h("span", { class: "small muted", style: "display:-webkit-box;-webkit-line-clamp:2;-webkit-box-orient:vertical;overflow:hidden", text: flow.error || flow.description }),
        h("span", { class: "row small muted" }, chip(plural(flow.activities.length, "activity", "activities"), "mono"), h("span", { class: "right", text: ago(flow.updated * 1000) })),
      ),
    ),
  );
}

async function open(name) {
  if (dirty() && state.name && !confirm(`Discard the changes of ${state.name}.py?`)) return;
  const found = await play(`workflows/${name}`);
  state.name = name;
  state.about = found;
  state.saved = found.source;
  editor.setValue(found.source);
  editor.clearHistory();
  history.replaceState(null, "", `#wf=${name}`);
  pane("editor");
  drawList();
  drawHead();
  drawRuns();
}

async function save() {
  if (!state.name) return false;
  const control = document.querySelector("#save");
  return guard(control, async () => {
    const source = editor.getValue();
    await play(`workflows/${state.name}`, { method: "PUT", body: { source } });
    state.saved = source;
    toast(`Saved ${state.name}.py in S3.`, "good");
    drawHead();
    await loadList();
    return true;
  });
}

async function remove() {
  if (!confirm(`Delete ${state.name}.py? The runs stay.`)) return;
  await guard(null, async () => {
    await play(`workflows/${state.name}`, { method: "DELETE" });
    state.name = state.saved = "";
    editor.setValue("");
    await loadList();
    if (state.workflows[0]) await open(state.workflows[0].name);
  });
}

function create() {
  const title = h("input", { class: "input", placeholder: "Refund decision", maxlength: "60" });
  const name = h("input", { class: "input mono", placeholder: "refund_decision", maxlength: "40", pattern: "[a-z][a-z0-9_]*" });
  title.addEventListener("input", () => (name.value = pythonName(title.value)));
  const make = async () => {
    if (!/^[a-z][a-z0-9_]*$/.test(name.value)) throw new Error("The file name must be a Python name in lower case.");
    if (state.workflows.some((flow) => flow.name === name.value)) throw new Error("A workflow with this name exists.");
    await play(`workflows/${name.value}`, { method: "PUT", body: { source: TEMPLATE(name.value, title.value || name.value) } });
    box.close();
    // The editor counts as saved, so `open` does not ask about the changes of the old file.
    state.saved = editor.getValue();
    await loadList();
    await open(name.value);
  };
  const about = h("p", { class: "small muted", text: "The new file has one structured decision, one branch and one document. Change it as you like." });
  const box = dialog("New workflow", [field("Title", title), field("File name (Python name)", name), about], [button("Create", { class: "primary", onclick: press(make) }, "plus")]);
}

function addActivity() {
  const title = h("input", { class: "input", placeholder: "Analysis of the story", maxlength: "60" });
  const kind = h("select", { class: "input" }, h("option", { value: "decision", text: "Structured decision — a Pydantic model that the workflow branches on" }), h("option", { value: "document", text: "Document — an artifact in S3 and in the transcript" }), h("option", { value: "answer", text: "Plain answer — text" }));
  const prompt = h("textarea", { class: "input", placeholder: "What Vis does in this activity" });
  const add = () => {
    const name = pythonName(title.value) || "new_activity";
    // A document has a file name. A decision has the name of its Pydantic model.
    const model = name.split("_").map((word) => word.charAt(0).toUpperCase() + word.slice(1)).join("");
    const artifact = kind.value === "document" ? `${name.toUpperCase()}.md` : model;
    const snippet = SNIPPETS[kind.value](name, title.value || name, prompt.value || "Describe the work here.", artifact);
    const rows = editor.getValue().split("\n");
    const entry = rows.findIndex((row) => row.startsWith("@workflow"));
    if (entry < 0) return toast("The file has no @workflow function.", "bad");
    let last = -1;
    rows.forEach((row, index) => index > entry && /^ {4}return\b/.test(row) && (last = index));
    const callRow = `    ${name}_result = ${name}(ctx)`;
    editor.operation(() => {
      if (last >= 0) editor.replaceRange(`${callRow}\n`, { line: last, ch: 0 });
      else editor.replaceRange(`\n${callRow}\n`, { line: editor.lastLine() + 1, ch: 0 });
      editor.replaceRange(`${snippet}\n\n`, { line: entry, ch: 0 });
    });
    box.close();
    jump(`def ${name}(`);
    toast(`Added the activity ${name}.`, "good");
  };
  const about = h("p", { class: "small muted", text: "The activity goes above the workflow function, and a call goes before its last return. Each activity is one journal step: a run that starts again does not repeat a completed step." });
  const box = dialog("Add an activity", [field("Title", title), field("Kind", kind), field("Request to Vis", prompt), about], [button("Add to the workflow", { class: "primary", onclick: add }, "plus")]);
}

// -- runs --------------------------------------------------------------------------------------

async function run() {
  if (!state.name) return;
  if (dirty() && !(await save())) return;
  const about = state.about || (await play(`workflows/${state.name}`));
  // An input with a text default, or with no default, is text. Each other input is JSON.
  const isText = (input) => typeof input.default === "string" || input.default === null;
  const areas = about.inputs.map((input) => {
    const area = h("textarea", { class: "input", "data-name": input.name });
    area.value = isText(input) ? input.default || "" : JSON.stringify(input.default);
    return [input, area];
  });
  const start = async () => {
    const input = Object.fromEntries(areas.map(([given, area]) => [given.name, isText(given) ? area.value : JSON.parse(area.value)]));
    const made = await play("runs", { method: "POST", body: { workflow: state.name, input } });
    box.close();
    toast("The run started.", "good");
    await loadRuns();
    await openRun(made.id);
  };
  const fields = areas.map(([input, area]) => field(input.name, area, "mono"));
  const box = dialog(`Run · ${about.title}`, [h("p", { class: "muted", text: about.description }), fields, !fields.length && h("p", { class: "small muted", text: "This workflow has no inputs." })], [button("Start the run", { class: "primary", onclick: press(start) }, "play")]);
}

async function loadRuns() {
  state.runs = (await play("runs")).runs;
  if (!state.run) drawRuns();
}

function drawRuns() {
  if (state.run) return drawRun();
  const shown = state.runs.filter((run) => state.all || !state.name || run.workflow === state.name);
  fill(runsHead, h("div", { class: "row" }, h("h2", { text: "Runs" }), chip(String(shown.length), "mono"), button(state.all ? "All workflows" : "This workflow", { class: "small right", title: "Change the filter", onclick: () => ((state.all = !state.all), drawRuns()) })));
  fill(
    runsBody,
    shown.map((run) =>
      h(
        "button",
        { class: "card run", type: "button", onclick: () => openRun(run.id) },
        h("span", { class: "row" }, h("b", { class: "clip", text: run.title }), status(run.status)),
        stepBars(run),
        h("span", { class: "row small muted" }, h("span", { class: "mono", text: run.id.slice(0, 8) }), h("span", { class: "right", text: ago(run.created * 1000) })),
      ),
    ),
    !shown.length && empty("No runs yet", "Start a run with the Run button."),
  );
}

async function openRun(id) {
  state.run = id;
  state.doc = null;
  state.turns = [];
  history.replaceState(null, "", `#run=${id}`);
  pane("runs");
  studio.classList.add("show-runs");
  // The head of the run before has buttons for that run.
  fill(runsHead);
  fill(runsBody, loading("Loading the run…"));
  await refreshRun().catch((error) => {
    closeRun();
    throw error;
  });
}

async function refreshRun() {
  const id = state.run;
  if (!id) return;
  const doc = await play(`runs/${id}`);
  // When the transcript does not load, the page keeps the turns that it has.
  const turns = doc.session_id ? await turnsOf(doc.session_id).catch(() => state.turns) : state.turns;
  if (state.run !== id) return;
  // A run that still waits did not change. A new draw would drop the file that a person chooses.
  if (doc.status === "waiting" && state.doc?.id === id && state.doc.status === "waiting") return;
  state.doc = doc;
  state.turns = turns;
  drawRun();
}

function closeRun() {
  state.run = state.doc = null;
  history.replaceState(null, "", state.name ? `#wf=${state.name}` : location.pathname);
  drawRuns();
}

/** Return the row of an artifact of a session, with a button that shows its text. */
function artifactRow(session, file) {
  const path = artifactPath(session, file.name);
  const row = fileRow(file.name, file.size, versionOf(file));
  row.className = "row card file-row";
  row.append(button("View", { class: "small", onclick: press(() => preview(path, file.name)) }, "eye"));
  return row;
}

/** Return the action that gives a file to a waiting run. The file starts the run again. */
const giver = (doc) => async (file) => {
  await giveFile(doc, await textOf(file));
  await refreshRun();
};

function stepCard(step, doc) {
  const from = step.started * 1000;
  const to = (step.finished || Date.now() / 1000) * 1000;
  // An activity is one or more turns of the session. The turns of its time are its Vis work.
  const rows = state.turns.filter((turn) => turn.created_at >= from - 500 && turn.created_at <= to).flatMap(activities);
  const result = step.result;
  const kind = result?.kind;
  const mark = step.status === "completed" ? icon("check") : step.status === "failed" ? icon("x") : step.status === "running" ? h("i", { class: "spin" }) : step.status === "waiting" ? icon("upload") : String(step.seq + 1);
  return h(
    "div",
    { class: `step ${step.status}` },
    h("span", { class: "mark" }, mark),
    h(
      "div",
      { class: "step-body" },
      h("div", { class: "row wrap" }, h("b", { text: step.title }), h("span", { class: "small muted", text: span(to - from) }), kind === "model" && chip(result.type, "violet")),
      step.error && h("div", { class: "problem", style: "border-radius:8px;border:0", text: step.error }),
      (kind === "model" || kind === "json") && padded(structured(result.value)),
      step.status === "waiting" && drop(doc.waiting.title, doc.waiting.accept, giver(doc)),
      kind === "artifact" && artifactRow(doc.session_id, result.value),
      kind === "upload" && padded(fileRow(result.value.name, result.value.size)),
      notes(step.notes),
      rows.length > 0 && more(plural(rows.length, "Vis activity", "Vis activities"), h("div", { class: "acts", style: "margin-top:6px" }, rows.map(activity))),
    ),
  );
}

function drawRun() {
  const doc = state.doc;
  if (!doc) return;
  const busy = OPEN.has(doc.status);
  const cancel = async () => {
    await cancelRun(doc.id);
    await refreshRun();
  };
  const facts = [`run ${doc.id.slice(0, 8)}`, clock(doc.created * 1000), doc.started && span(((doc.finished || Date.now() / 1000) - doc.started) * 1000), doc.starts > 1 && `${doc.starts} starts`];
  fill(
    runsHead,
    h("div", { class: "row" }, button("Runs", { class: "quiet small", onclick: closeRun }, "back"), status(doc.status)),
    h("h2", { text: doc.title }),
    h("div", { class: "small muted", text: facts.filter(Boolean).join(" · ") }),
    h(
      "div",
      { class: "row wrap" },
      doc.session_id && h("a", { class: "btn small", href: `/play#${doc.session_id}` }, h("span", { text: "Session and transcript" })),
      doc.session_id && h("a", { class: "btn small", href: `/#/s/${doc.session_id}` }, h("span", { text: "Open in Vis" })),
      busy && button("Cancel", { class: "small danger", onclick: press(cancel) }, "stop"),
    ),
  );
  const steps = [...doc.activities].sort((a, b) => a.seq - b.seq);
  redraw(runsBody, runsBody, "details", () =>
    fill(
      runsBody,
      Object.keys(doc.input || {}).length > 0 && more("Input", h("div", { class: "block pad", style: "margin-top:6px" }, value(doc.input))),
      steps.length ? h("div", { class: "timeline" }, steps.map((step) => stepCard(step, doc))) : busy && !doc.waiting && waiting(doc.status === "queued" ? "The run waits for a free place…" : "The run starts its Vis session…"),
      // A workflow can wait for a file outside an activity. Then no step shows the place for the file.
      doc.waiting && !steps.some((step) => step.status === "waiting") && drop(doc.waiting.title, doc.waiting.accept, giver(doc)),
      notes(doc.notes),
      // A failed step shows its error itself.
      doc.error && !steps.some((step) => step.error === doc.error) && h("div", { class: "problem", style: "border-radius:10px;border:0;white-space:pre-wrap", text: doc.error }),
      doc.result && h("div", {}, h("h4", { style: "margin-bottom:6px", text: "Result" }), padded(structured(doc.result.value))),
      doc.artifacts?.length > 0 && h("div", { style: "display:grid;gap:6px" }, h("h4", { text: "Artifacts in S3" }), doc.artifacts.map((file) => artifactRow(doc.session_id, file))),
    ),
  );
}

// -- start ---------------------------------------------------------------------------------------

/** Show one pane on a phone: "list", "editor" or "runs". A wide screen shows all three. */
function pane(name) {
  studio.dataset.pane = name;
  document.querySelectorAll("#phone-tabs button").forEach((tab) => tab.setAttribute("aria-selected", String(tab.dataset.pane === name)));
  if (name === "editor") setTimeout(() => editor.refresh(), 0);
}

bar();
document.querySelector("#new").addEventListener("click", create);
document.querySelectorAll("#phone-tabs button").forEach((tab) => tab.addEventListener("click", () => pane(tab.dataset.pane)));
window.addEventListener("beforeunload", (event) => dirty() && event.preventDefault());
drawHead();
/** Open what the fragment names: a workflow (#wf=name) or a run (#run=id). Return false when it names none. */
async function route() {
  const wanted = /^#(wf|run)=([\w-]+)/.exec(location.hash);
  const flow = wanted?.[1] === "run" ? state.runs.find((run) => run.id === wanted[2])?.workflow : wanted?.[2];
  const known = state.workflows.some((row) => row.name === flow);
  if (known && flow !== state.name) await open(flow);
  if (wanted?.[1] === "run") await openRun(wanted[2]);
  return known || wanted?.[1] === "run";
}

guard(null, async () => {
  await Promise.all([loadList(), loadRuns()]);
  const routed = await route();
  if (!routed && state.workflows[0]) await open(state.workflows[0].name);
  if (window.innerWidth < 820 && !routed) pane("list");
});
window.addEventListener("hashchange", () => guard(null, route));
setInterval(() => {
  if (document.hidden) return;
  loadRuns().catch(() => {});
  if (state.doc && (OPEN.has(state.doc.status) || !state.doc.finished) && !document.querySelector("dialog[open]")) refreshRun().catch(() => {});
}, 3000);
