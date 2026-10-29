# Live apps examples

## When to use

- You want to see what a live app can do before you build one: see [The apps](#the-apps).
- You want to open the apps in a session from the TUI or the Companion app: see [Load the extension](#load-the-extension).
- You want a program outside Vis to own the apps: see [Run the SDK program](#run-the-sdk-program).
- You want to write your own app with the same model: see [Write an app](#write-an-app).
- You want to check the apps after a change: see [Run the tests](#run-the-tests).

For the components and the rules of live apps, read the *Live views* page of the manual.

## The apps

Each app is one pure model in `src/live_apps/`. The same model runs in an extension and in an SDK program.

| App | Key | What it shows |
|---|---|---|
| Counter | `counter` | Buttons, stats and keys: `+`, `-`, `ArrowUp`, `ArrowDown`, `s` and `0`. |
| Todo | `todo` | An input, a selectable table, filters and a refusal of an empty task. Select rows, then press `x` to mark them done. |
| Kanban | `kanban` | Three columns with a limit on work in progress. Select a card, then press `ArrowRight` or `ArrowLeft` to move it. |
| 2048 | `2048` | A game board in a table. Play with the arrow keys or with `h`, `j`, `k` and `l`. |
| Asteroids | `asteroids` | A real-time game in a 3D `scene` node. The model ticks 10 times a second and sends only the objects that moved. The Companion app moves them smoothly with WebGL, and the TUI draws them with `▀` half blocks. Turn with `ArrowLeft` and `ArrowRight`, thrust with `ArrowUp`, fire with `Space` and jump with `ArrowDown`. The letters `a`, `d`, `w`, `f` and `h` do the same. Press `p` to pause and `n` to start a new game. Play it with a keyboard. |
| Incident dashboard | `incident` | Live alerts from a background producer, stats, a status, a log and a notes thread. Select an alert and press `a` to acknowledge it. A Send of a note gets a receipt and an answer in the thread. |
| Deploy wizard | `wizard` | Steps of inputs: select, multiselect, range, password and checkbox, each with a check. The deploy runs in stages. The password value never shows in the view or in the receipt. |
| Gallery | `gallery` | A card for each app. Activate a card, or press its number key, to open its app. |

## Load the extension

1. Add the example to the `vis.yml` file of your project:

   ```yaml
   extensions:
     vis-live-apps:
       source: /path/to/vis/examples/live-apps
   ```

2. Open a session in that project.
3. Type `/apps` to open the gallery, or `/apps todo kanban` to open each named app.

In the TUI, the extension adds keys after `C-x` and changes one built-in key:

- Press `C-x e L` to open the gallery.
- Press `C-x a i` to open the incident dashboard.
- Press `C-x a e` to focus the attachments. The extension moves this built-in key from `C-x a i` to `C-x a e`, so that `C-x a i` is free for the incident dashboard.

After a gateway restart, the apps stay in the session but they have no owner. Type `/apps` with their names again. The extension adopts each app by its key, with the same id and its saved state. Asteroids starts a new game with its best score.

## Run the SDK program

`sdk_demo.py` is a program outside Vis that owns the apps. It uses `GatewayClient` and `Session.attach` from `blockether.vis.engine`.

Run it from this directory with the Python of the Vis SDK:

```bash
python sdk_demo.py --url http://127.0.0.1:8090 counter todo
```

- Without `--session`, the program creates a session.
- Without app names, it opens every app except the gallery.
- If the gateway requires a token, set `VIS_GATEWAY_TOKEN` before you run the program.
- The program serves events until you press Ctrl+C, or for `--seconds` seconds.
- The app states go to the JSON file that `--state` names, by default `live-apps-state.json`.

Run it again with the same `--session` and `--state` values. The program adopts the open apps by key and paints their saved state.

The gateway records the owner as `sdk/live-apps-demo`. Use `--owner` to give another name. An app key is unique for each owner, so the extension and the SDK program can each open their own `counter` in one session.

## Write an app

An app model is a Python module with these names:

- `KEY`, `TITLE` and `DESCRIPTION`: the identity and the texts of the app.
- `initial()`: the first state, as plain JSON data.
- `reduce(state, event)`: the next state after one event. It must not change anything outside the state.
- `render(state)`: the list of view nodes for that state.
- `shortcuts(state)`: optional, the keys of the app for that state.
- `effects(old, new)`: optional, the side effects that the runner does after a step, such as a reply in a thread.
- `READS`: optional, the ids of the tables whose selection the reducer reads.
- `TICK_SECONDS`: optional, the interval of a `tick` event for an app that changes without a person.
- `save(state)` and `load(saved)`: optional, the data that outlives a restart and the state that it gives back. Without them, the runner stores the whole state.

`runtime.Runner` drives a model through one app handle. After each event, it compares the old picture with the new one and sends only the changes. It writes the store only when the saved data changes. Asteroids saves only its best score, so a tick writes nothing. Add the model to `apps()` in `src/live_apps/__init__.py` to show it in the gallery and in `/apps`.

Keep a view under 64 nodes. Put a long collection in the rows of a table, not in one node for each item.

## Run the tests

Run the tests from the root of the Vis repository:

```bash
packages/vis-agent/.venv/bin/python -m pytest examples/live-apps/tests
```

The tests drive each app through a test host: events go in, and the tests check the nodes and the patches that come out.
