// The Studio page: the web app of the project, from the app function, in a frame.
// The frame has its own origin, so the app cannot read this page or call the API of Vis.
// The page loads the frame again when a session changes a source file.
import { ago, app, bar, button, chip, empty, fill, h, item, loading } from "/play/assets/common.js";

const list = document.getElementById("sources");
const stage = document.getElementById("stage");
// A source goes into request paths: accept only `release` or the name of a work directory.
const NAME = /^(release|[0-9a-f]{12}(-[0-9a-f]{8})?)$/;
const named = () => (NAME.test(location.hash.slice(1)) ? location.hash.slice(1) : null);
// found: the sources of the app. source: the shown source. version: the stamp of its files.
// expires: the end of the frame token, in seconds. changed: the time of the last live change.
const state = { found: null, source: named(), version: null, expires: 0, changed: 0 };
const POLL_MS = 2500;

/** Return the title and the detail of a source. */
function label(name) {
  if (name === "release") return ["Published", `main · ${(state.found?.release || "").slice(0, 7) || "no release"}`];
  const [commit, run] = name.split("-");
  return run ? [`Workflow run ${run}`, `work/${name}`] : ["Vis sessions", `work/${commit}`];
}

function drawList() {
  const rows = [{ name: "release" }, ...(state.found?.work || [])];
  fill(list, rows.map((row) => {
    const [title, detail] = label(row.name);
    return item(row.name === state.source, () => choose(row.name), h("span", { class: "title", text: title }), h("span", { class: "small muted mono clip", text: detail }), row.modified && h("span", { class: "small muted", text: `changed ${ago(row.modified * 1000)}` }));
  }));
}

/** Put the app of the source in a new frame, with a new token. */
async function show() {
  const source = state.source;
  const { path, expires } = await app(`token?source=${source}`);
  if (source !== state.source) return;
  state.expires = expires;
  const [title, detail] = label(source);
  const frame = h("iframe", { src: path, sandbox: "allow-scripts allow-downloads", referrerpolicy: "no-referrer", title: "The app of the project" });
  const note = state.changed ? `updated ${new Date(state.changed).toLocaleTimeString("en-GB")}` : "waits for a change";
  fill(
    stage,
    h("div", { class: "stage-head" }, h("b", { text: title }), h("span", { class: "small muted mono clip", text: detail }), source !== "release" && chip("live", "good", true), source !== "release" && h("span", { class: "small muted", text: note }), h("span", { class: "grow" }), button("Reload", { class: "small", onclick: () => show().catch(failed) }, "play")),
    frame,
  );
}

const failed = (error) => fill(stage, empty("The app is not available", error.message, "bad"));

function choose(name) {
  state.source = name;
  state.version = null;
  state.changed = 0;
  history.replaceState(null, "", `#${name}`);
  drawList();
  fill(stage, loading("The app function starts the studio…"));
  show().catch(failed);
}

/** Load the frame again when the files of the source changed, and before the token ends. */
async function watch() {
  const source = state.source;
  try {
    if (!document.hidden && source && state.found) {
      const { version } = await app(`version?source=${source}`);
      if (source === state.source) {
        const first = state.version === null;
        const edited = !first && version !== state.version;
        state.version = version;
        if (edited) state.changed = Date.now();
        if (!version && first) fill(stage, empty("No such source", "The directory is not on the file system. Choose another source."));
        else if (edited || Date.now() / 1000 > state.expires - 300) await show();
      }
    }
  } catch {
    // The next poll tries again.
  }
  setTimeout(watch, POLL_MS);
}

async function sources() {
  try {
    const first = !state.found;
    state.found = await app("sources");
    // Without a choice, show the copy that the sessions change, when it exists.
    if (!state.source) {
      const sessions = state.found.release.slice(0, 12);
      choose(state.found.work.some((row) => row.name === sessions) ? sessions : "release");
    } else if (first) {
      // The head of the stage names the release, so the frame waits for the list.
      show().catch(failed);
    }
    drawList();
  } catch (error) {
    if (!state.found) failed(error);
  }
  setTimeout(sources, 15000);
}

bar();
fill(stage, loading("The app function starts the studio…"));
sources();
watch();
