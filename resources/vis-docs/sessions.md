# Sessions

Vis saves each conversation as a session, so you can close it and continue later.
This page shows how to control a session while Vis works, and how to find, fork,
organize and export saved sessions. It covers the terminal, the desktop and phone
apps and the command line.

## When to use

- **You think of a follow-up while Vis is still working.** Send it now. It [waits in
  the queue](#queue-a-message) and runs when the current turn finishes. You can edit
  it until then.
- **Vis is going in the wrong direction and you want to stop it.** [Cancel the
  turn](#cancel-a-turn). Your queued messages return to the composer so you can
  send them again.
- **You want to leave the session.** See [Quit](#quit) for what **Ctrl+C** does in
  each state.
- **You need a conversation from last week.** [Find the
  session](#find-a-saved-session) by its title or by something said in it.
- **You write a gateway client that must find sessions.** Use the [gateway search
  route](#search-through-the-gateway-api).
- **You want to try another approach without losing this conversation.** [Fork
  the session](#fork-a-session).
- **The last few turns went wrong and you want to go back.** [Fork from an earlier
  turn](#fork-from-an-earlier-turn).
- **Your project has more sessions than you can scan.** [Put them into
  groups](#organize-sessions-into-groups), star the ones you use often and
  [archive](#rename-star-archive-or-delete-a-session) the ones you have finished.
- **You want to share a conversation or keep a readable transcript.** [Export the
  session](#export-a-session) as an HTML page, or as Markdown with every tool call.
- **A bug report needs the conversation that shows the problem.** Export it, then
  remove private details as described in [Reporting a
  bug](reporting-bugs.md#sharing-a-transcript).
- **You want another session to help or to review the work.** Read
  [Council](council.md) instead.

## Start a new session

In the terminal, press **Ctrl+X n**. In the desktop or phone app, choose **New session**. To start
it inside a group, use the **+** on the group's band in the app. You can also choose **＋ New session
here** in the group's **g** menu in [Projects](#find-a-saved-session). The session then opens at the
top of that group.

## Control a running session

You can keep typing while Vis works. A message that you send during a running turn
waits in a queue. You can also cancel the running turn or quit the session.

### Follow progress

Vis adds short notes while it works. Under each note, one digest row gives a summary of the steps after the note.
The row shows the number of steps and what their calls did, for example `2 steps · 1 mutation · 3 observations`.
It also shows any running, failed or cancelled calls and the time that the steps took.
When you stop a turn, the stopped step counts as a failure, so the row turns red.

A mutation changes something, for example a file. An observation only reads.
A verification checks the work, for example with tests. An external action reaches outside the computer that runs Vis.
The mutation count always shows, also when it is 0, so you can see at once if the steps changed anything.

Open the digest to see the thinking, code and Activity of these steps. Failures, files and images stay visible when the digest is closed.
The Interrupted message of a stopped step shows only in the open digest.

When the steps open live views, a closed row also shows a live button. Select it to open the newest running live view, or else the newest recording.

To see each step as a separate Activity, turn off **Compact mode** in Settings, under **Responses**.
The terminal and each app keep their own choice.

### Queue a message

Press **Enter** to send. If no turn is running, the message starts one.
Otherwise it is added to the queue below the progress display.

Queued messages run in submission order. The queue pauses after a failed turn.
Resume it manually to send the next message.

To edit a queued message:

- **Terminal:** press **↑** to move the newest queued message back into the
  composer.
- **Desktop or phone app:** choose the message to edit it, or choose **×** to
  remove it.

The queue is stored in memory and cleared when the gateway restarts.

### Cancel a turn

Press **Esc** or **Ctrl+G** to cancel the running turn.

The cancelled message stays in the conversation. Vis does not put it back in the
composer. To send it again in the terminal, press **↑** to recall it.

Cancellation stops the turn and returns queued messages to the composer as a
draft. To run them, submit the draft again.

### Quit

**Ctrl+C** depends on the current state:

| State | Ctrl+C |
|---|---|
| Nothing typed, nothing running | Quits |
| A draft in the composer | Clears the draft. A second press quits |
| A turn is running | Cancels the turn |
| A cancel is in progress | Quits immediately |

## Find a saved session

Each result names its **Project** and **Group**. Search rows do not use project or
group colors. Status labels use uppercase, such as `IDLE`, `NEW` and `LIVE`.

Project and group filters apply to recent sessions and text searches. Selecting several
groups searches any of them. A project and group selection narrows both together.

### In the terminal

Press **Ctrl+X s** to open the session switcher. Before you type, it lists your
recent sessions. Type to search session titles and conversation text, choose a
session with **↑** and **↓**, and press **Enter** to open it.

A border always divides the switcher, also before you type. The left side lists the
recent sessions or the sessions that match. The right side shows the matching messages
of the selected session. Each message shows who wrote it, **You** or **Vis**, and when.
Your search words are highlighted. The messages stay beside the list in a narrow terminal.

The switcher also has these keys:

| Keys | What they do |
|---|---|
| Ctrl+P | Choose all projects or one project. All projects also clears the groups |
| Ctrl+G | Choose groups in the chosen project, or select none for all groups. It works only if the project has groups |
| Ctrl+N | Start a new session |
| Ctrl+F | Fork the selected session |
| Ctrl+S | Star or unstar the selected session |
| Ctrl+D | Delete the selected session after you confirm |
| Esc | Close the switcher |

To browse by project, press **Ctrl+X w** to open **Projects**. It lists saved
sessions from the connected gateway, including ones you have not opened in this
terminal. Use **↑** and **↓** to choose a project, group or session, and press
**Enter** to expand it or to resume a session. Press **g** on a row for its menu.
If your terminal reports mouse clicks, you can click rows and menu items instead.
**Tab** does not switch sessions.

Each project shows only its name. A new project takes the name of its folder. To rename a project
or change its folder, press **g** on the project row. Then choose **Rename project…** or
**Change folder…**. For a new folder, type its path and press **Enter**. The project keeps its
sessions, and Vis does not move your files.

To search saved session titles and conversation text, press **/** while Projects has focus. The
search also finds sessions outside the pages on screen. Result-page rows show more matches. Press
**Esc** to go back to your previous folds and page.

Projects shows only part of your sessions and groups at a time. Use the page controls to see more
sessions or groups. New sessions and groups appear automatically. If your selected row remains in
the list, it keeps focus.

### In the desktop or phone app

The app lists sessions under their projects, with groups inside each project.
Choose a session to open it.

Each project shows only its name. A new project takes the name of its folder. To add a project,
choose the folder icon next to the **+** button. Then choose the project folder.

To change a project, right-click it or choose the three-dot button at its right. The menu has
**Rename project**, **Change folder**, **Settings** and **Delete project**. When you change the
folder, the project keeps its sessions. Vis does not move your files.

To search your sessions, choose the search icon in the app bar. On a phone, you can also
pull the session list down. The search opens in its own dialog. The session list behind
the dialog does not change.

The dialog opens on your recent sessions, with the most recent at the top. Type to
search session titles and conversation text. When you type, the search also finds
sessions that you archived.

Menus below the search field control where the search looks. If you added more than one
machine, use **Machine** to choose the machine to search. You cannot choose a machine that
does not answer.

Use **Project** to choose **All projects** or one named project.
If the project you choose has groups, the **Groups** menu appears.
Use **Groups** to select several groups. The list stays open while you select them.
Choose **All groups** to clear the group selection.
To search all projects and groups again, choose **All projects**.

When you type, the line below the menus shows the number of matches. When you scroll to
the end of the results, more results load automatically. If they cannot load, choose
**Try again** below the last result.

With a keyboard, press **Ctrl+/** to open the search. This shortcut also works while you
type in a text box. When you are not typing, you can also press **/**.

A border always divides the dialog in the same way as the terminal switcher. The sessions
are on the left. The matching messages of one session are on the right, with your search
words highlighted. Before you type, the right side names one session and asks you to type.
On a phone or in a narrow window, the messages are below the sessions.

The messages of the first session in the results show first. To see the messages of a
different session, choose that session. To open the session, choose it again. You
can also choose **Open** or one of its messages. Before you type, choose a recent session
once to open it. To close the search, press **Esc** or choose the close button.

### Search through the gateway API

The terminal and the apps search sessions through one gateway route. To use the
same search in your own program, send this request through an authenticated gateway
client:

```text
GET /v1/sessions/actions/search?q=release%20notes&limit=20
```

The answer lists session rows with the same fields as `GET /v1/sessions` rows. The
rows are in order of recent activity, with the most recent first.

- With an empty `q`, the answer lists your recent sessions.
- With words in `q`, the answer lists the sessions whose title or conversation text
  matches. Each of these rows also has a `match` object. It tells where the words
  matched and gives short text around each match.
- `project_id` restricts results to one saved project.
- `root` restricts results to one project directory. An empty `root` selects sessions without a project.
- `group_ids` selects any group in a comma-separated list. An empty value selects no groups.
- Without these parameters, the search includes every project and group.
- Scopes apply before `total`, the page window and its cursor are calculated.
- `limit` sets the page size, from 1 to 1000. The default is 50.
- `total` is the number of sessions in all pages.
- To read the next page, send the `next_cursor` value as `after`. When `has_more`
  is `false`, there are no more pages.
- `archived=exclude` lists active sessions, `include` lists all sessions and `only`
  lists archived sessions. The default is `exclude`.

## Fork a session

A fork is a new session that starts with a copy of another session's
conversation. The two sessions then continue separately, and the original does
not change. You can fork a session once it has a turn.

### Fork the whole conversation

- **Terminal:** press **Ctrl+X y**. Vis opens the fork and switches to it. To fork
  another session, select it in the session switcher and press **Ctrl+F**.
- **Desktop or phone app:** open the session's actions in the session list and
  choose **Fork**. The app opens the fork.

### Fork from an earlier turn

A turn is one message you sent and the work Vis did to answer it. The fork keeps
the turn you choose and every turn before it.

- **Terminal:** press **Ctrl+X t** to open **Fork session at…**, which lists the
  message that started each turn. Type to filter the list, choose the last turn to
  keep and press **Enter**.
- **Desktop or phone app, or a terminal that reports mouse clicks:** choose **Fork
  from this turn** in the header of a finished answer. In a narrow terminal
  window, the button reads **Fork**.

### What a fork keeps

- **Conversation:** the copied turns, with the work Vis did and the files attached
  to them.
- **Title:** the original title followed by `(fork)`. Rename the fork to tell the
  two sessions apart.
- **Project and group:** the same as the original session.
- **Settings:** your global, project and group
  [settings](configuration.md#project-group-and-session-settings), but not the
  settings you changed for the original session only.

A fork does not copy or restore your project files. Changes Vis made in later
turns stay in place, so forking from an earlier turn does not undo them.

## Organize sessions into groups

A group collects related sessions under a project, such as "Release apps" or
"Bug triage". It shows its own name, colour and session count, and opens and
closes like any other fold. A group also narrows which sessions talk to each
other in [Council](council.md#groups-and-settings).

- **Desktop or phone app:** open the **⋮** menu in a project's header and choose
  **New group**. The same menu renames a group, changes its colour, deletes it and
  files the sessions listed on that page into a group. To move one session, choose
  **Move to...** in its actions or drag its row onto a group's band.
- **Terminal:** in Projects, choose **＋ New group…** in the **g** menu of a
  project, **Groups** or **Sessions** row. A group's own **g** menu renames it,
  changes its colour, archives it or deletes it. To file sessions, use **g** on a
  session row, or mark several with **Space** and use the group's **g** menu.
  **Ctrl+X d** moves the session you are in.

Deleting a group asks whether to keep its sessions, which return to the project
ungrouped, or to delete them with the group.

## Rename, star, archive or delete a session

- **Desktop or phone app:** a session's actions include **Rename**, **Star**,
  **Archive** and **Delete**.
- **Terminal:** in Projects, the **g** menu on a session row has its details and
  the same actions. The **g** menus on **Sessions** and **Groups** show archived
  sessions and groups.

Stars are shared, so a session you star in the terminal is also starred in the
apps. Archived sessions leave the usual lists until you show them. Removing a
project or deleting a group or session asks for confirmation, and deleting saved
sessions cannot be undone.

## Manage sessions from the command line

The `vis-agent sessions` commands read the sessions saved on this computer, even
when you connect the terminal to a remote gateway. A session id can be the full id
or any unambiguous prefix from `vis-agent sessions list`.

```bash
vis-agent sessions list                   # saved sessions and their ids
vis-agent sessions search "release notes" # search conversation text
vis-agent sessions show 3a7b2c1d          # one session's details and turns
vis-agent sessions delete 3a7b2c1d        # permanent
```

`vis-agent sessions fork` does not create a second session: it records a branch
point in the same session, which `vis-agent sessions show` lists. To get a
separate copy, fork in the terminal or an app.

## Export a session

Export a saved session to share a conversation, review its tool calls or keep a
readable transcript. Use a session id from `vis-agent sessions list`:

```bash
vis-agent sessions export <SESSION-ID> [--md | --html PATH]
```

Vis does not remove private data from an export. Read an export before you share
it. To remove private details, follow [Reporting a
bug](reporting-bugs.md#sharing-a-transcript).

### Markdown

Markdown is the default format. The export prints the transcript, including tool calls, to stdout:

```bash
vis-agent sessions export 3a7b2c1d > session.md
```

### HTML

Use `--html` for a self-contained page you can open in a browser. Vis creates
missing directories and adds `.html` if the output path has no extension:

```bash
vis-agent sessions export 3a7b2c1d --html report.html
```

## See also

- [Keyboard shortcuts](keyboard-shortcuts.md) — every terminal key, including the session commands on this page.
- [Desktop and mobile setup](index.md#connect-an-app) — follow and control the same session from another device.
- [Council](council.md) — ask another session for help or a second review.
- [Reporting a bug](reporting-bugs.md) — remove private information before you share an export.
- [Project instructions](context-and-prompts.md) — slash commands and shell shortcuts you can queue.
