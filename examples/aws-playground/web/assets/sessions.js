// The sessions page: each session of the gateway, its Vis activities, artifacts and files.
import { activities, activity, ago, bar, button, clock, count, dialog, download, fill, guard, h, markdown, play, plural, preview, size, span, structured, toast, vis } from "/play/assets/common.js";

const panes = document.querySelector("#panes");
const list = document.querySelector("#list");
const detail = document.querySelector("#detail");
const filter = document.querySelector("#filter");
const state = { sessions: [], runs: {}, selected: sessionId(location.hash), tab: "activity", loaded: null, draft: "" };
// The SDK adds the JSON schema to a structured request. The page shows the request without it.
const SCHEMA = "\n\nReturn the final answer as exactly one JSON value";
const LIVE = new Set(["running", "streaming", "queued", "pending"]);

// The fragment goes into request paths: accept only a session id.
function sessionId(hash) {
  const id = decodeURIComponent(hash.slice(1));
  return /^[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}$/.test(id) ? id : "";
}

const tone = (status) => (LIVE.has(status) ? "running" : status === "failed" || status === "error" ? "bad" : status === "completed" || status === "done" ? "good" : "");
const live = (session) => LIVE.has(session.status) || Boolean(session.current_turn_id);

async function load() {
  const [sessions, runs] = await Promise.all([vis("sessions?limit=100"), play("runs").catch(() => ({ runs: [] }))]);
  const rows = new Map();
  for (const row of [...(sessions.awaiting || []), ...(sessions.sessions || [])]) rows.set(row.id, row);
  state.sessions = [...rows.values()].sort((a, b) => b.modified_at - a.modified_at);
  state.runs = Object.fromEntries(runs.runs.filter((run) => run.session_id).map((run) => [run.session_id, run]));
  document.querySelector("#total").textContent = `${state.sessions.length}`;
  drawList();
}

function drawList() {
  const words = filter.value.trim().toLowerCase();
  const shown = state.sessions.filter((row) => !words || `${row.title} ${row.model} ${row.id}`.toLowerCase().includes(words));
  fill(list,
    ...shown.map((row) =>
      h(
        "button",
        { class: "item", type: "button", "aria-selected": String(row.id === state.selected), onclick: () => select(row.id) },
        h("span", { class: "row" }, h("i", { class: `dot ${live(row) ? "running" : "good"}` }), h("span", { class: "title clip", text: row.title || "Untitled session" })),
        h(
          "span",
          { class: "row small muted" },
          h("span", { class: "clip", text: `${plural(row.turn_count || 0, "turn")} · ${row.model || "no model"}` }),
          state.runs[row.id] && h("span", { class: "chip amber", text: "workflow" }),
          h("span", { class: "right", style: "white-space:nowrap", text: ago(row.modified_at) }),
        ),
      ),
    ),
  );
  if (!shown.length) list.append(h("div", { class: "empty" }, h("b", { text: state.sessions.length ? "No session matches" : "No sessions yet" }), "Start one in Vis or run a workflow."));
}

async function select(id, quiet = false) {
  state.selected = id;
  history.replaceState(null, "", id ? `#${encodeURIComponent(id)}` : location.pathname);
  panes.classList.toggle("detail", Boolean(id));
  drawList();
  if (!id) return drawEmpty();
  if (!quiet) fill(detail, h("div", { class: "empty" }, h("div", { class: "spin", style: "margin:0 auto 12px" }), "Loading the session…"));
  try {
    const [transcript, artifacts, files] = await Promise.all([vis(`sessions/${id}/transcript?limit=200`), play(`artifacts?session=${id}`), play(`tmp?session=${id}`)]);
    if (state.selected !== id) return;
    state.loaded = { id, turns: transcript.turns || [], artifacts: artifacts.artifacts, files: files.files };
    drawDetail();
  } catch (error) {
    if (!quiet) fill(detail, h("div", { class: "empty" }, h("b", { text: "The session did not load" }), error.message));
  }
}

function drawEmpty() {
  fill(detail, h("div", { class: "empty" }, h("b", { text: "Select a session" }), "You see its Vis activities, its artifacts and the files that it wrote."));
}

// -- actions --------------------------------------------------------------------------

async function fork(turn) {
  const body = turn ? { through_turn_id: turn.turn_id } : {};
  const made = await vis(`sessions/${state.selected}/forks`, { method: "POST", body });
  toast(turn ? `Forked after turn ${turn.position}.` : "Forked the session.", "good");
  await load();
  state.tab = "activity";
  await select(made.session.id);
}

/** Return the turn that was at work when a stored file was written. */
function turnAt(seconds) {
  const at = seconds * 1000;
  const turns = state.loaded.turns;
  return turns.find((turn, index) => at >= turn.created_at && at <= (turns[index + 1]?.created_at ?? Infinity)) || turns[turns.length - 1];
}

function transcripts(session) {
  const name = (session.title || "session").replace(/[^\w.-]+/g, "-").slice(0, 60);
  const kinds = [["Markdown", "transcript.md", "md", "To read and to keep in a repository"], ["HTML", "transcript.html", "html", "One page that opens in a browser"], ["JSON", "transcript", "json", "Each turn, iteration and activity as data"]];
  dialog(
    "Download the transcript",
    kinds.map(([label, path, suffix, about]) =>
      h("div", { class: "row" }, h("div", {}, h("b", { text: label }), h("div", { class: "small muted", text: about })), button("Download", { class: "right", onclick: (event) => guard(event.currentTarget, () => download(`/v1/sessions/${session.id}/${path}`, `${name}.${suffix}`)) }, "download")),
    ),
  );
}

function publish(session) {
  const message = h("input", { class: "input", value: `Work of the session ${session.title || session.id.slice(0, 8)}`.slice(0, 72), maxlength: "200" });
  const result = h("div", {});
  const send = button("Commit and push", { class: "primary" }, "upload");
  send.addEventListener("click", () =>
    guard(send, async () => {
      const done = await play("publish", { method: "POST", body: { root: session.workspace.root, message: message.value } });
      const short = done.sha.slice(0, 12);
      const link = String(done.url).startsWith("https://github.com/") ? h("a", { href: done.url, target: "_blank", rel: "noopener noreferrer", text: short }) : short;
      const next = done.merged ? "The next worker starts on this commit." : "The main branch moved, so the commits are on a new branch.";
      fill(result, h("div", { class: "note" }, h("b", { text: `Pushed to ${done.branch}. ` }), link, h("div", { class: "small muted", text: next })));
    }),
  );
  dialog("Publish to GitHub", [h("p", { class: "muted" }, "The worker commits the changes of ", h("code", { text: session.workspace.root }), " and gives them to the repo function. Only that function can push, and only to the playground repository."), h("label", { class: "field" }, h("span", { text: "Commit message" }), message), result], [send]);
}

async function send(session, text, control) {
  await guard(control, async () => {
    await vis(`sessions/${session.id}/turns`, { method: "POST", body: { request: text, idempotency_key: crypto.randomUUID() } });
    state.draft = "";
    toast("The request is with Vis.", "good");
    await load();
    await select(session.id, true);
  });
}

// -- the detail of a session ---------------------------------------------------------------

function answer(turn) {
  const text = (turn.content || []).filter((part) => part.type === "prose").map((part) => part.markdown).join("\n\n");
  if (!text) return null;
  try {
    const value = JSON.parse(text);
    if (value && typeof value === "object") return h("div", { class: "answer" }, h("div", { class: "row small muted", style: "margin-bottom:6px" }, h("span", { class: "chip violet", text: "structured output" })), h("div", { class: "block", style: "padding:12px 14px" }, structured(value)));
  } catch {
    // The answer is prose.
  }
  return h("div", { class: "answer" }, markdown(text));
}

function turnCard(turn) {
  const cut = (turn.request || "").indexOf(SCHEMA);
  const request = h("div", { class: "request", title: "Click to open", text: cut < 0 ? turn.request || "" : turn.request.slice(0, cut), onclick: (event) => event.currentTarget.classList.toggle("open") });
  const rows = activities(turn);
  const busy = LIVE.has(turn.status);
  return h(
    "article",
    { class: "card turn" },
    h(
      "div",
      { class: "turn-head" },
      h("span", { class: "turn-no", text: `TURN ${turn.position}` }),
      busy ? h("span", { class: "chip amber" }, h("i", { class: "dot running" }), "working") : h("span", { class: `chip ${tone(turn.status)}`, text: turn.status }),
      cut >= 0 && h("span", { class: "chip violet", text: "structured" }),
      h("span", { class: "small muted", text: [clock(turn.created_at), span(turn.duration_ms), plural(turn.iteration_count || 0, "iteration"), turn.input_tokens ? `${count(turn.input_tokens)} → ${count(turn.output_tokens)} tokens` : "", turn.total_cost ? `$${turn.total_cost.toFixed(3)}` : ""].filter(Boolean).join(" · ") }),
      !busy && button("Fork from here", { class: "small right", title: "Make a new session that holds the turns through this turn", onclick: (event) => guard(event.currentTarget, () => fork(turn)) }, "fork"),
    ),
    h("div", { class: "turn-body" }, request, rows.length ? h("div", { class: "acts" }, rows.map(activity)) : null, busy && !rows.length ? h("div", { class: "row small muted" }, h("i", { class: "spin" }), "Vis works on this turn…") : null, answer(turn)),
  );
}

function fileRows(kind, rows, id) {
  if (!rows.length) return h("div", { class: "card empty" }, h("b", { text: kind === "artifacts" ? "No artifacts" : "No files" }), kind === "artifacts" ? "A session saves a document with playground.save_artifact(name, content)." : "Each file that a tool writes in the project or in /tmp is copied here.");
  return h(
    "div",
    { class: "card files" },
    rows.map((row) => {
      const path = `/play/api/${kind}/${id}/${row.name.split("/").map(encodeURIComponent).join("/")}`;
      return h(
        "div",
        { class: "file" },
        h("div", {}, h("div", { class: "name", text: row.name }), h("div", { class: "small muted", text: `${size(row.size)} · ${clock(row.at * 1000)}` })),
        h(
          "div",
          { class: "row" },
          button("View", { class: "small", onclick: (event) => guard(event.currentTarget, () => preview(path, row.name)) }, "eye"),
          button("", { class: "small", title: "Download", "aria-label": `Download ${row.name}`, onclick: (event) => guard(event.currentTarget, () => download(path, row.name.split("/").pop())) }, "download"),
          kind === "artifacts" && button("Fork", { class: "small", title: "Make a new session from the moment that wrote this artifact", onclick: (event) => guard(event.currentTarget, () => fork(turnAt(row.at))) }, "fork"),
        ),
      );
    }),
  );
}

function drawDetail() {
  const { id, turns, artifacts, files } = state.loaded;
  const session = state.sessions.find((row) => row.id === id) || { id, workspace: {} };
  const run = state.runs[id];
  const tokens = turns.reduce((sum, turn) => sum + (turn.input_tokens || 0) + (turn.output_tokens || 0), 0);
  const cost = turns.reduce((sum, turn) => sum + (turn.total_cost || 0), 0);
  const steps = turns.reduce((sum, turn) => sum + activities(turn).length, 0);
  const inProject = /\/work\/[^/]+$/.test(session.workspace?.root || "");
  const tab = (key, label, total) => h("button", { type: "button", role: "tab", "aria-selected": String(state.tab === key), onclick: () => ((state.tab = key), drawDetail()) }, label, h("span", { class: "count", text: String(total) }));
  const body = state.tab === "activity" ? (turns.length ? turns.map(turnCard) : h("div", { class: "card empty" }, h("b", { text: "No turns yet" }), "Send the first request below.")) : state.tab === "artifacts" ? fileRows("artifacts", artifacts, id) : fileRows("tmp", files, id);
  const draft = h("textarea", { class: "input", placeholder: "Ask Vis to continue this session…", "aria-label": "A new request for this session" });
  draft.value = state.draft;
  draft.addEventListener("input", () => (state.draft = draft.value));
  const sendButton = button("Send", { class: "primary", onclick: () => draft.value.trim() && send(session, draft.value.trim(), sendButton) }, "play");
  const scroll = document.querySelector(".main").scrollTop;
  const opened = new Set([...detail.querySelectorAll("details.act")].map((node, index) => (node.open ? index : -1)));
  fill(detail,
    h(
      "div",
      { class: "head" },
      button("Sessions", { class: "back quiet small", style: "justify-self:start", onclick: () => select("") }, "back"),
      h("div", { class: "row wrap" }, h("h1", { style: "min-width:0;overflow-wrap:anywhere", text: session.title || "Untitled session" })),
      h(
        "div",
        { class: "meta" },
        live(session) ? h("span", { class: "chip amber" }, h("i", { class: "dot running" }), "working") : h("span", { class: "chip good", text: "idle" }),
        session.model && h("span", { class: "chip mono", text: session.model }),
        h("span", { class: "chip", text: plural(turns.length, "turn") }),
        h("span", { class: "chip", text: plural(steps, "activity", "activities") }),
        tokens > 0 && h("span", { class: "chip", text: `${count(tokens)} tokens` }),
        cost > 0 && h("span", { class: "chip", text: `$${cost.toFixed(2)}` }),
        run && h("a", { class: "chip amber", href: `/play/workflows#run=${run.id}`, text: `workflow · ${run.title}` }),
        h("span", { class: "chip mono", title: session.workspace?.root || "", text: (session.workspace?.root || "").split("/").slice(-2).join("/") }),
      ),
      h(
        "div",
        { class: "row wrap" },
        h("a", { class: "btn dark", href: `/#/s/${encodeURIComponent(id)}` }, h("span", { text: "Open in Vis" })),
        button("Transcript", { onclick: () => transcripts(session) }, "download"),
        button("Fork", { title: "Make a new session with each turn of this session", onclick: (event) => guard(event.currentTarget, () => fork(null)) }, "fork"),
        inProject && button("Publish", { onclick: () => publish(session) }, "upload"),
      ),
    ),
    h("div", { class: "tabs", role: "tablist" }, tab("activity", "Activity", turns.length), tab("artifacts", "Artifacts", artifacts.length), tab("files", "Files", files.length)),
    body,
    state.tab === "activity" && h("div", { class: "card", style: "padding:12px;display:grid;gap:8px" }, draft, h("div", { class: "row" }, h("span", { class: "small muted", text: "The request runs in this session, with the same project directory." }), h("span", { class: "right" }, sendButton))),
  );
  detail.querySelectorAll("details.act").forEach((node, index) => (node.open = opened.has(index)));
  document.querySelector(".main").scrollTop = scroll;
}

// -- start -------------------------------------------------------------------------------

bar("/play");
filter.addEventListener("input", drawList);
window.addEventListener("hashchange", () => {
  const id = sessionId(location.hash);
  if (id !== state.selected) select(id);
});
drawEmpty();
guard(null, async () => {
  await load();
  if (state.selected) await select(state.selected);
});
setInterval(async () => {
  if (document.hidden) return;
  try {
    await load();
    const session = state.sessions.find((row) => row.id === state.selected);
    const busy = state.loaded && state.loaded.turns.some((turn) => LIVE.has(turn.status));
    if (session && state.loaded && (live(session) || busy) && !document.querySelector("dialog[open]") && document.activeElement?.tagName !== "TEXTAREA") await select(session.id, true);
  } catch {
    // The next round tries again.
  }
}, 6000);
