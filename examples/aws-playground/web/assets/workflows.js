// The workflows page: write a workflow in Python, add activities, run it and watch the run.
/* global CodeMirror */
import { activities, activity, ago, bar, button, clock, count, dialog, fill, guard, h, icon, markdown, play, plural, preview, size, span, structured, toast, value, vis } from "/play/assets/common.js";

const studio = document.querySelector("#studio");
const list = document.querySelector("#list");
const head = document.querySelector("#editor-head");
const problem = document.querySelector("#problem");
const outline = document.querySelector("#outline");
const runsHead = document.querySelector("#runs-head");
const runsBody = document.querySelector("#runs");
const state = { workflows: [], name: "", saved: "", about: null, runs: [], run: null, doc: null, turns: [], all: false };
const LIVE = new Set(["queued", "running"]);
const TONE = { completed: "good", failed: "bad", running: "amber", queued: "", cancelled: "" };

const TEMPLATE = (name, title) => `"""${title}."""

from typing import Literal

from playground.workflows import activity, workflow
from pydantic import BaseModel, Field


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
let marked = null;
let timer = 0;
const dirty = () => editor.getValue() !== state.saved;

editor.on("change", () => {
  clearTimeout(timer);
  timer = setTimeout(check, 450);
  drawHead();
});

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
  fill(outline,
    h("h4", { text: "Activities" }),
    ...about.activities.map((item) => h("button", { class: "chip amber", type: "button", title: `Go to ${item.name}`, text: item.title, onclick: () => jump(`def ${item.name}(`) })),
    !about.activities.length && h("span", { class: "small muted", text: "none yet" }),
    h("h4", { style: "margin-left:10px", text: "Inputs" }),
    ...about.inputs.map((item) => h("span", { class: "chip mono", text: item.name })),
    !about.inputs.length && h("span", { class: "small muted", text: "none" }),
  );
}

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
    dirty() && h("span", { class: "chip amber", text: "not saved" }),
    about && h("span", { class: "small muted clip only-wide", text: about.title }),
    h("span", { class: "right row wrap" },
      button("Add activity", { class: "small", onclick: addActivity, disabled: !state.name }, "plus"),
      button("Save", { class: "small", id: "save", onclick: () => save(), disabled: !state.name || !dirty() }, "save"),
      button("Run", { class: "small primary", onclick: () => run(), disabled: !state.name }, "play"),
      button("", { class: "small quiet danger", title: "Delete the workflow", "aria-label": "Delete the workflow", onclick: remove, disabled: !state.name }, "trash"),
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
  fill(list,
    ...state.workflows.map((item) =>
      h(
        "button",
        { class: "item", type: "button", "aria-selected": String(item.name === state.name), onclick: () => open(item.name) },
        h("span", { class: "title clip", text: item.title }),
        h("span", { class: "small muted", style: "display:-webkit-box;-webkit-line-clamp:2;-webkit-box-orient:vertical;overflow:hidden", text: item.error || item.description }),
        h("span", { class: "row small muted" }, h("span", { class: "chip mono", text: plural(item.activities.length, "activity", "activities") }), h("span", { class: "right", text: ago(item.updated * 1000) })),
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
  title.addEventListener("input", () => (name.value = title.value.toLowerCase().replace(/[^a-z0-9]+/g, "_").replace(/^_+|_+$|^(?=\d)/g, "")));
  const make = button("Create", { class: "primary" }, "plus");
  const box = dialog("New workflow", [h("label", { class: "field" }, h("span", { text: "Title" }), title), h("label", { class: "field" }, h("span", { text: "File name (Python name)" }), name), h("p", { class: "small muted", text: "The new file has one structured decision, one branch and one document. Change it as you like." })], [make]);
  make.addEventListener("click", () =>
    guard(make, async () => {
      if (!/^[a-z][a-z0-9_]*$/.test(name.value)) throw new Error("The file name must be a Python name in lower case.");
      if (state.workflows.some((item) => item.name === name.value)) throw new Error("A workflow with this name exists.");
      await play(`workflows/${name.value}`, { method: "PUT", body: { source: TEMPLATE(name.value, title.value || name.value) } });
      box.close();
      state.saved = editor.getValue();
      await loadList();
      await open(name.value);
    }),
  );
}

function addActivity() {
  const title = h("input", { class: "input", placeholder: "Analysis of the story", maxlength: "60" });
  const kind = h("select", { class: "input" }, h("option", { value: "decision", text: "Structured decision — a Pydantic model that the workflow branches on" }), h("option", { value: "document", text: "Document — an artifact in S3 and in the transcript" }), h("option", { value: "answer", text: "Plain answer — text" }));
  const prompt = h("textarea", { class: "input", placeholder: "What Vis does in this activity" });
  const add = button("Add to the workflow", { class: "primary" }, "plus");
  const box = dialog("Add an activity", [h("label", { class: "field" }, h("span", { text: "Title" }), title), h("label", { class: "field" }, h("span", { text: "Kind" }), kind), h("label", { class: "field" }, h("span", { text: "Request to Vis" }), prompt), h("p", { class: "small muted", text: "The activity goes above the workflow function, and a call goes before its last return. Each activity is one journal step: a run that starts again does not repeat a completed step." })], [add]);
  add.addEventListener("click", () => {
    const name = (title.value || "new activity").toLowerCase().replace(/[^a-z0-9]+/g, "_").replace(/^_+|_+$|^(?=\d)/g, "") || "new_activity";
    const words = name.split("_").map((word) => word.charAt(0).toUpperCase() + word.slice(1)).join("");
    const artifact = kind.value === "document" ? `${name.toUpperCase()}.md` : words;
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
  });
}

// -- runs --------------------------------------------------------------------------------------

async function run() {
  if (!state.name) return;
  if (dirty() && !(await save())) return;
  const about = state.about || (await play(`workflows/${state.name}`));
  const fields = about.inputs.map((item) => {
    const box = h("textarea", { class: "input", "data-name": item.name });
    box.value = typeof item.default === "string" ? item.default : item.default === null ? "" : JSON.stringify(item.default);
    return h("label", { class: "field" }, h("span", { class: "mono", text: item.name }), box);
  });
  const start = button("Start the run", { class: "primary" }, "play");
  const box = dialog(`Run · ${about.title}`, [h("p", { class: "muted", text: about.description }), ...fields, !fields.length && h("p", { class: "small muted", text: "This workflow has no inputs." })], [start]);
  start.addEventListener("click", () =>
    guard(start, async () => {
      const input = {};
      for (const field of box.box.querySelectorAll("textarea[data-name]")) {
        const given = about.inputs.find((item) => item.name === field.dataset.name);
        if (typeof given.default === "string" || given.default === null) input[field.dataset.name] = field.value;
        else input[field.dataset.name] = JSON.parse(field.value);
      }
      const made = await play("runs", { method: "POST", body: { workflow: state.name, input } });
      box.close();
      toast("The run started.", "good");
      await loadRuns();
      await openRun(made.id);
    }),
  );
}

async function loadRuns() {
  state.runs = (await play("runs")).runs;
  if (!state.run) drawRuns();
}

function steps(run) {
  return h("span", { class: "steps" }, (run.activities.length ? run.activities : [["", "queued"]]).map(([title, status]) => h("i", { class: status, title })));
}

function drawRuns() {
  if (state.run) return drawRun();
  const shown = state.runs.filter((item) => state.all || !state.name || item.workflow === state.name);
  fill(runsHead,
    h("div", { class: "row" }, h("h2", { text: "Runs" }), h("span", { class: "chip mono", text: String(shown.length) }), button(state.all ? "All workflows" : "This workflow", { class: "small right", title: "Change the filter", onclick: () => ((state.all = !state.all), drawRuns()) })),
  );
  fill(runsBody,
    ...shown.map((item) =>
      h(
        "button",
        { class: "card run", type: "button", onclick: () => openRun(item.id) },
        h("span", { class: "row" }, h("b", { class: "clip", text: item.title }), h("span", { class: `chip right ${TONE[item.status]}` }, LIVE.has(item.status) && h("i", { class: "dot running" }), item.status)),
        steps(item),
        h("span", { class: "row small muted" }, h("span", { class: "mono", text: item.id.slice(0, 8) }), h("span", { class: "right", text: ago(item.created * 1000) })),
      ),
    ),
  );
  if (!shown.length) runsBody.append(h("div", { class: "empty" }, h("b", { text: "No runs yet" }), "Start a run with the Run button."));
}

async function openRun(id) {
  state.run = id;
  state.doc = null;
  state.turns = [];
  history.replaceState(null, "", `#run=${id}`);
  pane("runs");
  studio.classList.add("show-runs");
  fill(runsBody, h("div", { class: "empty" }, h("div", { class: "spin", style: "margin:0 auto 12px" }), "Loading the run…"));
  await refreshRun();
}

async function refreshRun() {
  const id = state.run;
  if (!id) return;
  const doc = await play(`runs/${id}`);
  let turns = state.turns;
  if (doc.session_id) turns = ((await vis(`sessions/${doc.session_id}/transcript?limit=200`).catch(() => null)) || { turns }).turns;
  if (state.run !== id) return;
  state.doc = doc;
  state.turns = turns;
  drawRun();
}

function closeRun() {
  state.run = state.doc = null;
  history.replaceState(null, "", state.name ? `#wf=${state.name}` : location.pathname);
  drawRuns();
}

function artifactRow(session, name, bytes) {
  const path = `/play/api/artifacts/${session}/${encodeURIComponent(name)}`;
  return h("div", { class: "row card", style: "padding:8px 10px;box-shadow:none" }, icon("file"), h("span", { class: "mono small clip", text: name }), bytes !== undefined && h("span", { class: "small muted", text: size(bytes) }), button("View", { class: "small right", onclick: (event) => guard(event.currentTarget, () => preview(path, name)) }, "eye"));
}

function stepCard(step, doc) {
  const from = step.started * 1000;
  const to = (step.finished || Date.now() / 1000) * 1000;
  // An activity is one or more turns of the session. The turns of its time are its Vis work.
  const rows = state.turns.filter((turn) => turn.created_at >= from - 500 && turn.created_at <= to).flatMap(activities);
  const result = step.result;
  const mark = step.status === "completed" ? icon("check") : step.status === "failed" ? icon("x") : step.status === "running" ? h("i", { class: "spin" }) : String(step.seq + 1);
  return h(
    "div",
    { class: `step ${step.status}` },
    h("span", { class: "mark" }, mark),
    h(
      "div",
      { class: "step-body" },
      h("div", { class: "row wrap" }, h("b", { text: step.title }), h("span", { class: "small muted", text: span(to - from) }), result?.kind === "model" && h("span", { class: "chip violet", text: result.type })),
      step.error && h("div", { class: "problem", style: "border-radius:8px;border:0", text: step.error }),
      result?.kind === "model" || result?.kind === "json" ? h("div", { class: "block", style: "padding:10px 12px" }, structured(result.value)) : null,
      result?.kind === "artifact" ? artifactRow(doc.session_id, result.value.name, result.value.size) : null,
      result?.kind === "text" ? h("div", { class: "block", style: "padding:10px 12px" }, markdown(result.value)) : null,
      ...(step.notes || []).map((note) => h("div", { class: "note small", text: note })),
      rows.length ? h("details", {}, h("summary", { class: "small muted", style: "cursor:pointer", text: `${rows.length} Vis ${rows.length === 1 ? "activity" : "activities"}` }), h("div", { class: "acts", style: "margin-top:6px" }, rows.map(activity))) : null,
    ),
  );
}

function drawRun() {
  const doc = state.doc;
  if (!doc) return;
  const busy = LIVE.has(doc.status);
  const opened = new Set([...runsBody.querySelectorAll("details")].map((node, index) => (node.open ? index : -1)));
  const scroll = runsBody.scrollTop;
  fill(runsHead,
    h("div", { class: "row" }, button("Runs", { class: "quiet small", onclick: closeRun }, "back"), h("span", { class: `chip right ${TONE[doc.status]}` }, busy && h("i", { class: "dot running" }), doc.status)),
    h("h2", { text: doc.title }),
    h("div", { class: "small muted", text: [`run ${doc.id.slice(0, 8)}`, clock(doc.created * 1000), doc.started && span(((doc.finished || Date.now() / 1000) - doc.started) * 1000), doc.starts > 1 && `${doc.starts} starts`].filter(Boolean).join(" · ") }),
    h(
      "div",
      { class: "row wrap" },
      doc.session_id && h("a", { class: "btn small", href: `/play#${doc.session_id}` }, h("span", { text: "Session and transcript" })),
      doc.session_id && h("a", { class: "btn small", href: `/#/s/${doc.session_id}` }, h("span", { text: "Open in Vis" })),
      busy && button("Cancel", { class: "small danger", onclick: (event) => guard(event.currentTarget, async () => (await play(`runs/${doc.id}/cancel`, { method: "POST", body: {} }), refreshRun())) }, "stop"),
    ),
  );
  const input = Object.keys(doc.input || {}).length ? h("details", {}, h("summary", { class: "small muted", style: "cursor:pointer", text: "Input" }), h("div", { class: "block", style: "padding:10px 12px;margin-top:6px" }, value(doc.input))) : null;
  const steps = [...doc.activities].sort((a, b) => a.seq - b.seq);
  fill(runsBody,
    input,
    steps.length ? h("div", { class: "timeline" }, steps.map((step) => stepCard(step, doc))) : h("div", { class: "row small muted" }, h("i", { class: "spin" }), doc.status === "queued" ? "The run waits for a free place…" : "The run starts its Vis session…"),
    ...(doc.notes || []).map((note) => h("div", { class: "note small", text: note })),
    doc.error && h("div", { class: "problem", style: "border-radius:10px;border:0;white-space:pre-wrap", text: doc.error }),
    doc.result && h("div", {}, h("h4", { style: "margin-bottom:6px", text: "Result" }), h("div", { class: "block", style: "padding:10px 12px" }, structured(doc.result.value))),
    doc.artifacts?.length ? h("div", { style: "display:grid;gap:6px" }, h("h4", { text: "Artifacts in S3" }), doc.artifacts.map((item) => artifactRow(doc.session_id, item.name, item.size))) : null,
  );
  runsBody.querySelectorAll("details").forEach((node, index) => (node.open = opened.has(index)));
  runsBody.scrollTop = scroll;
}

// -- start ---------------------------------------------------------------------------------------

function pane(name) {
  studio.dataset.pane = name;
  document.querySelectorAll("#phone-tabs button").forEach((tab) => tab.setAttribute("aria-selected", String(tab.dataset.pane === name)));
  if (name === "editor") setTimeout(() => editor.refresh(), 0);
}

bar("/play/workflows");
document.querySelector("#new").addEventListener("click", create);
document.querySelectorAll("#phone-tabs button").forEach((tab) => tab.addEventListener("click", () => pane(tab.dataset.pane)));
window.addEventListener("beforeunload", (event) => dirty() && event.preventDefault());
drawHead();
guard(null, async () => {
  await Promise.all([loadList(), loadRuns()]);
  const wanted = /^#(wf|run)=([\w-]+)/.exec(location.hash);
  if (wanted?.[1] === "run") {
    const found = state.runs.find((item) => item.id === wanted[2]);
    if (found && state.workflows.some((item) => item.name === found.workflow)) await open(found.workflow);
    await openRun(wanted[2]);
  } else if (wanted && state.workflows.some((item) => item.name === wanted[2])) await open(wanted[2]);
  else if (state.workflows[0]) await open(state.workflows[0].name);
  if (window.innerWidth < 820 && !wanted) pane("list");
});
setInterval(() => {
  if (document.hidden) return;
  loadRuns().catch(() => {});
  if (state.doc && (LIVE.has(state.doc.status) || !state.doc.finished) && !document.querySelector("dialog[open]")) refreshRun().catch(() => {});
}, 3000);
