// The sessions page: each session of the gateway, its Vis activities and its files.
import { activities, activity, ago, bar, button, chip, clock, count, dialog, download, empty, field, fill, github, guard, h, item, loading, markdown, play, plural, press, preview, redraw, size, span, structured, toast, turnsOf, versionOf, vis, waiting } from "/play/assets/common.js";

const panes = document.querySelector("#panes");
const list = document.querySelector("#list");
const main = document.querySelector(".main");
const detail = document.querySelector("#detail");
const filter = document.querySelector("#filter");
const state = { sessions: [], runs: {}, selected: sessionId(location.hash), tab: "activity", loaded: null, draft: "" };
// The SDK adds the JSON schema to a structured request. The page shows the request without it.
const SCHEMA = "\n\nReturn the final answer as exactly one JSON value";
// The statuses of a session or a turn that has work to do.
const LIVE = new Set(["running", "streaming", "queued", "pending"]);

// The fragment goes into request paths: accept only a session id.
function sessionId(hash) {
  const id = decodeURIComponent(hash.slice(1));
  return /^[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}$/.test(id) ? id : "";
}

// The colour of the chip of a turn that ended, by status.
const TONE = { completed: "good", done: "good", failed: "bad", error: "bad" };
const live = (session) => LIVE.has(session.status) || Boolean(session.current_turn_id);
const working = () => chip("working", "amber", true);
// The two lists of the Files tab, by their API name: the title, and the text of a list without rows.
const LISTS = {
  attached: ["Attached files", "No attached files", "A session saves a document with attach(content, name) of Vis. The site copies each attached file to S3."],
  tmp: ["Written files", "No written files", "Each file that a tool writes in the project or in /tmp is copied here."],
};

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
  fill(
    list,
    shown.map((row) =>
      item(
        row.id === state.selected,
        () => select(row.id),
        h("span", { class: "row" }, h("i", { class: `dot ${live(row) ? "running" : "good"}` }), h("span", { class: "title clip", text: row.title || "Untitled session" })),
        h(
          "span",
          { class: "row small muted" },
          h("span", { class: "clip", text: `${plural(row.turn_count || 0, "turn")} · ${row.model || "no model"}` }),
          state.runs[row.id] && chip("workflow", "amber"),
          h("span", { class: "right", style: "white-space:nowrap", text: ago(row.modified_at) }),
        ),
      ),
    ),
    !shown.length && empty(state.sessions.length ? "No session matches" : "No sessions yet", "Start one in Vis or run a workflow."),
  );
}

async function select(id, quiet = false) {
  state.selected = id;
  history.replaceState(null, "", id ? `#${encodeURIComponent(id)}` : location.pathname);
  panes.classList.toggle("detail", Boolean(id));
  drawList();
  if (!id) return drawEmpty();
  if (!quiet) fill(detail, loading("Loading the session…"));
  try {
    const [turns, attached, written] = await Promise.all([turnsOf(id), play(`attached?session=${id}`), play(`tmp?session=${id}`)]);
    if (state.selected !== id) return;
    state.loaded = { id, turns, attached: attached.files, written: written.files };
    drawDetail();
  } catch (error) {
    if (!quiet) fill(detail, empty("The session did not load", error.message));
  }
}

function drawEmpty() {
  fill(detail, empty("Select a session", "You see its Vis activities and its files."));
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

/** Return the turn that attached a file. A row without the turn id gives the turn that was at work at its time. */
function turnOf(row) {
  const at = row.at * 1000;
  const turns = state.loaded.turns;
  return turns.find((turn) => turn.turn_id === row.turn) || turns.find((turn, index) => at >= turn.created_at && at <= (turns[index + 1]?.created_at ?? Infinity)) || turns[turns.length - 1];
}

function transcripts(session) {
  const name = (session.title || "session").replace(/[^\w.-]+/g, "-").slice(0, 60);
  const kinds = [["Markdown", "transcript.md", "md", "To read and to keep in a repository"], ["HTML", "transcript.html", "html", "One page that opens in a browser"], ["JSON", "transcript", "json", "Each turn, iteration and activity as data"]];
  dialog(
    "Download the transcript",
    kinds.map(([label, path, suffix, about]) =>
      h("div", { class: "row" }, h("div", {}, h("b", { text: label }), h("div", { class: "small muted", text: about })), button("Download", { class: "right", onclick: press(() => download(`/v1/sessions/${session.id}/${path}`, `${name}.${suffix}`)) }, "download")),
    ),
  );
}

function publish(session) {
  const message = h("input", { class: "input", value: `Work of the session ${session.title || session.id.slice(0, 8)}`.slice(0, 72), maxlength: "200" });
  const result = h("div", {});
  const push = async () => {
    const done = await play("publish", { method: "POST", body: { root: session.workspace.root, message: message.value } });
    const short = done.sha.slice(0, 12);
    const link = github(done.url, short);
    const next = done.merged ? "The next worker starts on this commit." : "The main branch moved, so the commits are on a branch.";
    fill(result, h("div", { class: "note" }, h("b", { text: done.pushed ? `Pushed to ${done.branch}. ` : `Nothing to push: ${done.branch} has the commits. ` }), link, h("div", { class: "small muted", text: next })));
  };
  const about = h("p", { class: "muted" }, "The worker commits the changes of ", h("code", { text: session.workspace.root }), " and gives them to the repo function. Only that function can push, and only to the playground repository.");
  dialog("Publish to GitHub", [about, field("Commit message", message), result], [button("Commit and push", { class: "primary", onclick: press(push) }, "upload")]);
}

async function send(session, text) {
  if (!text) return;
  await vis(`sessions/${session.id}/turns`, { method: "POST", body: { request: text, idempotency_key: crypto.randomUUID() } });
  state.draft = "";
  toast("The request is with Vis.", "good");
  await load();
  await select(session.id, true);
}

// -- the detail of a session ---------------------------------------------------------------

function answer(turn) {
  const text = (turn.content || []).filter((part) => part.type === "prose").map((part) => part.markdown).join("\n\n");
  if (!text) return null;
  try {
    const value = JSON.parse(text);
    if (value && typeof value === "object") return h("div", { class: "answer" }, h("div", { class: "row small muted", style: "margin-bottom:6px" }, chip("structured output", "violet")), h("div", { class: "block", style: "padding:12px 14px" }, structured(value)));
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
  const facts = [clock(turn.created_at), busy ? "" : span(turn.duration_ms), busy ? "" : plural(turn.iteration_count || 0, "iteration"), turn.input_tokens ? `${count(turn.input_tokens)} → ${count(turn.output_tokens)} tokens` : "", turn.total_cost ? `$${turn.total_cost.toFixed(3)}` : ""];
  return h(
    "article",
    { class: "card turn" },
    h(
      "div",
      { class: "turn-head" },
      h("span", { class: "turn-no", text: `TURN ${turn.position}` }),
      busy ? working() : chip(turn.status, TONE[turn.status]),
      cut >= 0 && chip("structured", "violet"),
      h("span", { class: "small muted", text: facts.filter(Boolean).join(" · ") }),
      !busy && button("Fork from here", { class: "small right", title: "Make a new session that holds the turns through this turn", onclick: press(() => fork(turn)) }, "fork"),
    ),
    h("div", { class: "turn-body" }, request, rows.length > 0 && h("div", { class: "acts" }, rows.map(activity)), busy && !rows.length && waiting("Vis works on this turn…"), answer(turn)),
  );
}

/** Return one list of the Files tab: its title, then its rows. */
function fileRows(kind, rows, id) {
  const [title, ...none] = LISTS[kind];
  const heading = h("h4", { class: "section", text: title });
  if (!rows.length) return [heading, empty(...none, "card")];
  const list = h(
    "div",
    { class: "card files" },
    rows.map((row) => {
      const path = `/play/api/${kind}/${id}/${row.name.split("/").map(encodeURIComponent).join("/")}`;
      return h(
        "div",
        { class: "file" },
        h("div", {}, h("div", { class: "name", text: row.name }), h("div", { class: "small muted", text: [versionOf(row), size(row.size), clock(row.at * 1000)].filter(Boolean).join(" · ") })),
        h(
          "div",
          { class: "row" },
          button("View", { class: "small", onclick: press(() => preview(path, row.name)) }, "eye"),
          button("", { class: "small", title: "Download", "aria-label": `Download ${row.name}`, onclick: press(() => download(path, row.name)) }, "download"),
          kind === "attached" && button("Fork", { class: "small", title: "Make a new session from the moment that attached this file", onclick: press(() => fork(turnOf(row))) }, "fork"),
        ),
      );
    }),
  );
  return [heading, list];
}

/** Return the box that sends a new request to the session. The draft stays when the page draws again. */
function composer(session) {
  const draft = h("textarea", { class: "input", placeholder: "Ask Vis to continue this session…", "aria-label": "A new request for this session" });
  draft.value = state.draft;
  draft.addEventListener("input", () => (state.draft = draft.value));
  const sendButton = button("Send", { class: "primary", onclick: press(() => send(session, draft.value.trim())) }, "play");
  return h("div", { class: "card", style: "padding:12px;display:grid;gap:8px" }, draft, h("div", { class: "row" }, h("span", { class: "small muted", text: "The request runs in this session, with the same project directory." }), h("span", { class: "right" }, sendButton)));
}

function drawDetail() {
  const { id, turns, attached, written } = state.loaded;
  const session = state.sessions.find((row) => row.id === id) || { id, workspace: {} };
  const run = state.runs[id];
  const root = session.workspace?.root || "";
  const total = (amount) => turns.reduce((sum, turn) => sum + amount(turn), 0);
  const tokens = total((turn) => (turn.input_tokens || 0) + (turn.output_tokens || 0));
  const cost = total((turn) => turn.total_cost || 0);
  // Only a clone of the repository can go to GitHub.
  const inProject = /\/work\/[^/]+$/.test(root);
  const tab = (key, label, rows) => h("button", { type: "button", role: "tab", "aria-selected": String(state.tab === key), onclick: () => ((state.tab = key), drawDetail()) }, label, h("span", { class: "count", text: String(rows.length) }));
  const bodies = {
    activity: () => (turns.length ? turns.map(turnCard) : empty("No turns yet", "Send the first request below.", "card")),
    files: () => [...fileRows("attached", attached, id), ...fileRows("tmp", written, id)],
  };
  const head = h(
    "div",
    { class: "head" },
    button("Sessions", { class: "back quiet small", style: "justify-self:start", onclick: () => select("") }, "back"),
    h("div", { class: "row wrap" }, h("h1", { style: "min-width:0;overflow-wrap:anywhere", text: session.title || "Untitled session" })),
    h(
      "div",
      { class: "meta" },
      live(session) ? working() : chip("idle", "good"),
      session.model && chip(session.model, "mono"),
      chip(plural(turns.length, "turn")),
      chip(plural(total((turn) => activities(turn).length), "activity", "activities")),
      tokens > 0 && chip(`${count(tokens)} tokens`),
      cost > 0 && chip(`$${cost.toFixed(2)}`),
      run && h("a", { class: "chip amber", href: `/play/workflows#run=${run.id}`, text: `workflow · ${run.title}` }),
      h("span", { class: "chip mono", title: root, text: root.split("/").slice(-2).join("/") }),
    ),
    h(
      "div",
      { class: "row wrap" },
      h("a", { class: "btn dark", href: `/#/s/${encodeURIComponent(id)}` }, h("span", { text: "Open in Vis" })),
      button("Transcript", { onclick: () => transcripts(session) }, "download"),
      button("Fork", { title: "Make a new session with each turn of this session", onclick: press(() => fork(null)) }, "fork"),
      inProject && button("Publish", { onclick: () => publish(session) }, "upload"),
    ),
  );
  const tabs = h("div", { class: "tabs", role: "tablist" }, tab("activity", "Activity", turns), tab("files", "Files", [...attached, ...written]));
  redraw(detail, main, "details.act", () => fill(detail, head, tabs, bodies[state.tab](), state.tab === "activity" && composer(session)));
}

// -- start -------------------------------------------------------------------------------

bar();
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
