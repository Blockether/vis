// Suggestion review, comments and Vis discussions.
import {
  api, back, bar, button, chip, date, empty, field, fill, h, icon, item, loadError, loading,
  markdown, panel, panes, poll, press, select, selection, sessionPath, setSelection, update,
} from "./common.js";

const state = { id: selection("suggestion"), status: "", listRead: 0, detailRead: 0 };
const tone = { accepted: "good", declined: "bad", open: "amber" };
const filter = select("Status", [["", "All statuses"], ["open", "Open"], ["accepted", "Accepted"], ["declined", "Declined"]], (status) => {
  state.status = status;
  void readList();
});
const view = panes("Suggestions", "Review ideas from sessions and decide what to do next.", field("Status", filter));
const failure = h("p", { class: "status-line bad-text", role: "status" });
const body = h("div");
view.detail.append(failure, body);

function open(id) {
  state.id = id;
  setSelection("suggestion", id);
  view.reveal(Boolean(id));
  failure.textContent = "";
  fill(body, id ? [back(() => open("")), loading("Loading the suggestion…")] : empty("Select a suggestion", "Its details and comments show here."));
  void refresh();
}

async function readList() {
  const number = ++state.listRead;
  try {
    const result = await api(`suggestions?${new URLSearchParams(state.status ? { status: state.status } : {})}`);
    if (number !== state.listRead) return;
    view.message.textContent = "";
    const rows = result.suggestions || [];
    update(view.list, [rows, state.id], () => rows.length ? rows.map((row) => item(state.id === row.id, () => open(row.id),
      h("span", { class: "row" }, h("b", { class: "title", text: row.title }), h("span", { class: "right" }, chip(row.status, tone[row.status]))),
      h("span", { class: "meta", text: [row.kind, row.effort, date(row.created)].filter(Boolean).join(" · ") }),
    )) : empty("No suggestions", state.status ? "No suggestion has this status." : "Suggestions from workflows show here."));
  } catch (error) {
    if (number !== state.listRead) return;
    loadError(view.message, error);
    if (view.list.querySelector(".spin")) fill(view.list, empty("Suggestions did not load", "The page will try again."));
  }
}

function comments(rows) {
  return rows?.length ? rows.map((row) => h("div", { class: "comment" },
    h("span", { class: "meta", text: `${row.author === "factory" ? "Factory" : "Person"} · ${date(row.at)}` }), h("p", { text: row.text }),
  )) : h("p", { class: "muted", text: "No comments yet." });
}

async function act(id, action, payload) {
  const result = await api(`suggestions/${encodeURIComponent(id)}/${action}`, { method: "POST", body: payload });
  if (action === "discuss") location.assign(sessionPath(result.session_id));
  else if (id === state.id) {
    body.querySelector("textarea").value = "";
    await refresh();
  }
}

function draw(row) {
  const current = body.querySelector("textarea");
  const active = current === document.activeElement;
  const caret = active ? [current.selectionStart, current.selectionEnd] : null;
  const draft = current?.value || "";
  update(body, row, () => {
    const comment = h("textarea", { class: "input", maxlength: "4000", rows: "3", placeholder: "Write a comment." });
    comment.value = draft;
    const decide = (action) => press(() => act(row.id, action, { comment: comment.value.trim() }));
    const add = press(() => {
      const text = comment.value.trim();
      if (!text) throw new Error("Write a comment first.");
      return act(row.id, "comments", { text, author: "person" });
    });
    return panel([
      h("div", { class: "row wrap" }, back(() => open("")), chip(row.status, tone[row.status]), chip(row.kind), chip(`${row.effort} effort`)),
      h("h2", { text: row.title }),
      h("span", { class: "meta", text: `Created ${date(row.created)}${row.decided ? ` · Decided ${date(row.decided)}` : ""}` }),
      h("div", { class: "row actions" },
        row.session && h("a", { class: "btn", href: sessionPath(row.session), text: "Source session" }),
        row.run && h("a", { class: "btn", href: `/factory#run=${encodeURIComponent(row.run)}`, text: "Source run" }),
        row.talk ? h("a", { class: "btn primary", href: sessionPath(row.talk) }, icon("open"), "Open discussion") :
          button("Discuss", { class: "primary", onclick: press(() => act(row.id, "discuss", {})) }, "open"),
      ),
    ], markdown(row.body || ""),
    h("h3", { text: "Comments" }), comments(row.comments), field(row.status === "open" ? "Comment (optional for a decision)" : "Add a comment", comment),
      h("div", { class: "row actions" }, row.status === "open" && [button("Accept", { class: "primary", onclick: decide("accept") }, "check"),
        button("Decline", { class: "danger", onclick: decide("decline") }, "x")], button("Post comment", { onclick: add })),
    );
  });
  const next = body.querySelector("textarea");
  if (caret && current !== next) {
    next.focus({ preventScroll: true });
    next.setSelectionRange(...caret);
  }
}

async function readDetail() {
  const id = state.id;
  if (!id) return;
  const number = ++state.detailRead;
  try {
    const row = await api(`suggestions/${encodeURIComponent(id)}`);
    if (id !== state.id || number !== state.detailRead) return;
    failure.textContent = "";
    draw(row);
  } catch (error) {
    if (id !== state.id || number !== state.detailRead) return;
    loadError(failure, error);
    if (body.querySelector(".spin")) fill(body, back(() => open("")), empty("The suggestion did not load", "The page will try again."));
  }
}

async function refresh() {
  await Promise.all([readList(), readDetail()]);
}

view.reveal(Boolean(state.id));
fill(body, state.id ? [back(() => open("")), loading("Loading the suggestion…")] : empty("Select a suggestion", "Its details and comments show here."));
window.addEventListener("hashchange", () => open(selection("suggestion")));
bar();
poll(refresh);
