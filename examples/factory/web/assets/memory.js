// The newest entries of the team and personal memories.
import {
  api, back, bar, button, chip, empty, field, fill, h, item, loadError, loading, panel,
  panes, plural, poll, selection, setSelection, update,
} from "./common.js";

const state = { name: selection("memory"), query: "", pattern: "", memories: [], detailRead: 0 };
const title = (name) => name === "team" ? "Team memory" : name === "personal" ? "Personal memory" : name;
const view = panes("Memory", "Read the newest entries or find entries by text and pattern.", h("h2", { text: "Memories" }));
const head = h("h2");
const facts = h("div", { class: "row wrap" });
const search = h("input", { class: "input", type: "search", placeholder: "Words to find" });
const pattern = h("input", { class: "input", type: "search", placeholder: "Example: release|deploy" });
const errorLine = h("p", { class: "status-line bad-text", role: "status" });
const entries = h("div", { class: "stack" }, loading("Loading entries…"));
const form = h("form", { class: "stack", onsubmit: (event) => {
  event.preventDefault();
  state.query = search.value.trim();
  state.pattern = pattern.value.trim();
  loadEntries();
} },
  h("div", { class: "filters" }, field("Search text", search), field("Pattern (regular expression)", pattern)),
  h("div", { class: "row" }, button("Search", { class: "primary", type: "submit" }), button("Clear", { onclick: () => {
    search.value = pattern.value = "";
    form.requestSubmit();
  } })),
);
const detail = h("div", {}, panel(h("div", { class: "row wrap" }, back(() => open("")), head), facts, form, errorLine, entries));
const blank = empty("Select a memory", "Choose the team memory or the personal memory.");
view.detail.append(blank, detail);

function open(name) {
  state.name = name;
  setSelection("memory", name);
  view.reveal(Boolean(name));
  detail.hidden = !name;
  blank.hidden = Boolean(name);
  head.textContent = title(name);
  errorLine.textContent = "";
  drawList();
  if (name) loadEntries();
}

function drawList() {
  update(view.list, [state.memories, state.name], () => state.memories.length ? state.memories.map((memory) => item(memory.name === state.name, () => open(memory.name),
    h("span", { class: "row" }, h("b", { class: "title", text: title(memory.name) }), h("span", { class: "right" }, chip(String(memory.count)))),
    memory.location && h("span", { class: "meta wrap-word", text: memory.location }),
  )) : empty("No memories", "No memory is available."));
  const memory = state.memories.find((row) => row.name === state.name);
  update(facts, memory, () => memory ? [chip(plural(memory.count, "entry", "entries")), h("span", { class: "meta", text: memory.location })] : []);
}

async function readList() {
  try {
    const data = await api("memory");
    state.memories = data.memories || [];
    view.message.textContent = "";
    drawList();
  } catch (error) {
    loadError(view.message, error);
    if (view.list.querySelector(".spin")) fill(view.list, empty("Memories did not load", "The page will try again."));
  }
}

function loadEntries() {
  errorLine.textContent = "";
  fill(entries, loading("Loading entries…"));
  void readEntries();
}

async function readEntries() {
  const { name, query, pattern } = state;
  if (!name) return;
  const number = ++state.detailRead;
  const filters = new URLSearchParams({ limit: "50" });
  if (query) filters.set("query", query);
  if (pattern) filters.set("pattern", pattern);
  try {
    const result = await api(`memory/${encodeURIComponent(name)}?${filters}`);
    if (name !== state.name || number !== state.detailRead) return;
    errorLine.textContent = "";
    const rows = result.entries || [];
    update(entries, rows, () => rows.length ? rows.map((row) => h("article", { class: "entry" },
      h("div", { class: "row wrap meta" }, h("span", { text: row.date }), h("span", { class: "right", text: row.id })),
      h("pre", { text: row.text ?? row.line }),
    )) : empty("No entries", query || pattern ? "No entry matches this search." : "This memory has no entries yet."));
  } catch (error) {
    if (name !== state.name || number !== state.detailRead) return;
    loadError(errorLine, error);
    if (entries.querySelector(".spin")) fill(entries, empty("Entries did not load", "Check the search or try again."));
  }
}

view.reveal(Boolean(state.name));
head.textContent = title(state.name);
detail.hidden = !state.name;
blank.hidden = Boolean(state.name);
window.addEventListener("hashchange", () => open(selection("memory")));
bar();
poll(() => Promise.all([readList(), readEntries()]));
