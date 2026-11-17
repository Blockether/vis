# Sessions

Vis saves each conversation as a session, so you can close it and continue later. This page shows
how to control a session while Vis works, and how to find, fork, organize, export, and move saved
sessions. It covers the terminal, the desktop and phone apps, and the command line.

## When to use

- **You think of a follow-up while Vis is still working.** Send it now. It
  [waits in the queue](#queue-a-message).
- **Vis must read your correction before the turn ends.**
  [Send the queued message now](#send-a-queued-message-now).
- **Vis is going in the wrong direction and you want to stop it.**
  [Cancel the turn](#cancel-a-turn). Your queued messages return to the composer.
- **You want to leave the session.** [Quit](#quit) lists what **Ctrl+C** does in each state.
- **The session must work in another directory.**
  [Change the directory of the session](#change-the-directory-of-a-session).
- **You need a conversation from last week.** [Find the session](#find-a-saved-session) by its
  title or by something said in it.
- **Your program must create sessions, send messages, or find saved sessions.** Read
  [Sessions API](sessions-api.md).
- **You want to try another approach without losing this conversation.**
  [Fork the session](#fork-a-session).
- **The last few turns went wrong and you want to go back.**
  [Fork from an earlier turn](#fork-from-an-earlier-turn).
- **Your project has more sessions than you can scan.**
  [Put them into groups](#organize-sessions-into-groups), star the ones that you use most, and
  [archive](#rename-star-archive-or-delete-a-session) the finished ones.
- **You want to share a conversation or keep a readable transcript.**
  [Export the session](#export-a-session) as an HTML page, or as Markdown with every tool call.
- **You want to continue a conversation on another computer.**
  [Move the session](#move-a-session-to-another-computer) with a snapshot file.
- **A bug report needs the conversation that shows the problem.** Export it, then remove private
  details as [Reporting a bug](reporting-bugs.md#sharing-a-transcript) describes.
- **You want another session to help or to review the work.** Read [Council](council.md).

## Start a new session

- In the terminal, press **Ctrl+X s n**. To start in a group, select the group in
  [Projects](#find-a-saved-session) and press **n**.
- In the desktop or phone app, choose **New session**. To start in a group, use the **+** on the
  band of the group.

A session that starts in a group opens at the top of that group.

## Change the directory of a session

Shell commands, file tools, and search start in the one directory of the session. To see that
directory, send `/cd`. To move the session, send `/cd` with a path:

```text
/cd ~/projects/api
```

The path must be a directory that exists. A relative path starts at the current directory of the
session. The change applies from the next turn. The files in the old directory stay unchanged.

A session that belongs to a project moves to the project of the new directory. If no project has
that directory, the session leaves its project and its group.

## Control a running session

While Vis works, you can queue a message, cancel the turn, or quit the session.

### Follow progress

Vis adds short notes while it works. Under each note, one digest row sums up the steps after the
note, for example `1 mutation · 3 observations`. The row counts the calls by kind. It also shows
running, failed, and cancelled calls, and the time that the steps took. A step that you stop
counts as a failure, so the row turns red.

| Kind of call | Meaning |
|---|---|
| Mutation | Changes something, for example a file. The count always shows, also when it is 0 |
| Observation | Only reads |
| Verification | Checks the work, for example with tests |
| External action | Reaches outside the computer that runs Vis |

Open the digest to see the thinking, the code, and the Activity of its steps. A closed digest
still shows failures, files, and images. Only the open digest shows the Interrupted message of a
stopped step.

When the steps open live views, a closed row ends its counts with **live**, for example
`1 mutation · 1 running · live`. Select **live** to open the newest running live view, or else the
newest recording.

To see each step as a separate Activity, turn off **Compact mode** in Settings. The terminal and
each app keep their own choice.

In the desktop or phone app, a large turn first shows only its newest steps, so the session opens
faster. The app loads the older steps when you scroll near them, and then shows every step, as the
terminal does. If the load fails, the app shows the error and a **Try again** button.

### Queue a message

Press **Enter** to send. If no turn runs, the message starts one. If a turn runs, the message goes
to the queue below the progress display.

Queued messages run in the order that you sent them. After a failed turn, the queue pauses until
you resume it. The queue stays when the gateway restarts.

To edit a queued message:

- In the terminal, press **↑** to move the newest queued message back into the composer.
- In the desktop or phone app, choose the message to edit it, or choose **×** to remove it.

### Send a queued message now

A queued message waits for the end of the turn. Send it now when the running turn must read it
first, for example a correction.

- In the terminal, press **Ctrl+X q s** to send the whole queue. To send one message, click
  **Send it now** at the end of its row, or press **Ctrl+X q** and its row number.
- In the desktop or phone app, choose **Send all now** in the queue header. To send one message,
  choose **Send it now** on its row.

A sent message shows **next iter** on its row, and its button changes to **Unsend**. **Unsend**
makes the message wait for the end of the turn again.

The running turn reads the sent messages, in queue order, when it starts its next step. The
conversation then shows each one as a normal **You** message under a
**Queued · sent now · iter N** header. N is the iteration that the message reached.

Images attached to a sent message go to the running turn with it. Vis keeps them with that turn,
so the desktop and phone apps show them, also after a reload.

Two cases have a different order:

- If the turn ends before its next step, the sent messages start their own turns, before the rest
  of the queue.
- A queued command (`/` or `!`) cannot go into a running turn. It stays in the queue and runs as
  its own turn.

### Cancel a turn

Press **Esc** or **Ctrl+G** to cancel the running turn.

Your queued messages return to the composer as one draft. Submit the draft to run them. The
cancelled message stays in the conversation. To send it again in the terminal, press **↑**.

### Quit

The effect of **Ctrl+C** depends on the state:

| State | Ctrl+C |
|---|---|
| Nothing typed, nothing running | Quits |
| A draft in the composer | Clears the draft. A second press quits |
| A turn is running | Cancels the turn |
| A cancel is in progress | Quits immediately |

## Find a saved session

Search matches session titles and conversation text. Each result shows its project, its group,
and its status, such as `IDLE`, `NEW`, or `LIVE`.

Project and group filters apply to recent sessions and to text searches. A selection of several
groups searches any of them. A project and a group selection narrow the search together.

The terminal switcher and the search dialog of the apps always have two sides with a border
between them. One side lists the recent sessions or the sessions that match. The other side shows
messages of the selected session, so you always see a part of its conversation. Before you type,
and when no message matches, you see the newest messages. When messages match, you see them with
your search words highlighted.

### In the terminal

Press **Ctrl+X s s** to open the session switcher. Type to search, choose a session with **↑** and
**↓**, and press **Enter** to open it.

The list is on the left and the messages are on the right, also in a narrow terminal. The newest
messages show newest turn first. If no message matches, a note above them says so. Each message
shows who wrote it, **You** or **Vis**, and when.

The switcher also has these keys:

| Keys | What they do |
|---|---|
| Ctrl+P | Choose all projects or one project. All projects also clears the groups |
| Ctrl+G | Choose groups in the chosen project, or select none for all groups. It works only if the project has groups |
| Ctrl+N | Start a new session |
| Ctrl+F | Fork the selected session |
| Ctrl+S | Star or unstar the selected session |
| Ctrl+D | Delete the selected session after you confirm |
| Ctrl+X | Export the selected session to a file |
| Esc | Close the switcher |

To browse by project, press **Ctrl+X b w** to open **Projects**. It lists the saved sessions of
the connected gateway, also sessions that you did not open in this terminal. Use **↑** and **↓**
to choose a project, a group, or a session. Press **Enter** to expand the row or to resume the
session.

Press **?** for the sidebar menu. It lists every key of Projects and runs your choice on the
selected row. If your terminal reports mouse clicks, click rows and menu items. Right-click a row
to open its menu.

To search, press **/** while Projects has focus. The search also finds sessions outside the pages
on screen. Result-page rows show more matches. Press **Esc** to go back to your previous folds and
page.

Projects shows only a part of your sessions and groups at a time. Use the page controls to see
more. New sessions and groups appear without a refresh. If your selected row stays in the list, it
keeps focus.

### In the desktop or phone app

The app lists sessions under their projects, with groups inside each project. Choose a session to
open it.

To search, choose the search icon in the app bar or press **Ctrl+/**. The shortcut also works
while you type in a text box. When you do not type, **/** works too. On a phone, you can also pull
the session list down. The search opens in its own dialog, and the session list behind it does not
change.

The dialog opens on your recent sessions, the most recent first. A typed search also finds
archived sessions, and the line below the menus shows the number of matches. More results load
when you scroll to the end. If they cannot load, choose **Try again** below the last result.

The menus below the search field set where the search looks:

- **Machine** chooses the machine to search, if you added more than one. You cannot choose a
  machine that does not answer.
- **Project** chooses **All projects** or one named project. **All projects** also clears the
  groups.
- **Groups** appears if the chosen project has groups. Select several groups. The list stays open
  while you select. **All groups** clears the selection.

The sessions are on the left and the messages are on the right. On a phone or in a narrow window,
the messages are below the sessions. The messages of the first result show first. Choose another
session to see its messages.

To open a session, choose it again, choose **Open**, or choose one of its messages. Before you
type, one choice opens a recent session. To close the search, press **Esc** or choose the close
button.

## Add, rename, or move a project

Each project shows only its name. A new project takes the name of its folder. When you change the
folder, the project keeps its sessions, and Vis does not move your files.

- In the terminal, select the project row in Projects. Press **R** to rename the project. Press
  **F** to change its folder, then type the path and press **Enter**.
- In the desktop or phone app, choose the folder icon next to the **+** button to add a project,
  then choose its folder. To change a project, right-click it or choose the three-dot button at
  its right. The menu has **Rename project**, **Change folder**, **Settings**, and
  **Delete project**.

## Fork a session

A fork is a new session that starts with a copy of the conversation of another session. The
original does not change. You can fork a session after its first turn.

A fork does not copy or restore your project files. The changes that Vis made in later turns stay
in place, so a fork from an earlier turn does not undo them.

### Fork the whole conversation

- In the terminal, press **Ctrl+X s f**. Vis opens the fork and switches to it. To fork another
  session, select it in the session switcher and press **Ctrl+F**.
- In the desktop or phone app, open the actions of the session in the session list and choose
  **Fork**. The app opens the fork.

### Fork from an earlier turn

A turn is one message that you sent and the work that Vis did to answer it. The fork keeps the
turn that you choose and every turn before it.

- In the terminal, press **Ctrl+X s t** to open **Fork session at…**. It lists the message that
  started each turn. Type to filter the list, choose the last turn to keep, and press **Enter**.
- In an app, or in a terminal that reports mouse clicks, choose **Fork from this turn** in the
  header of a finished answer. In a narrow terminal window, the button reads **Fork**.

### What a fork keeps

| Part | In the fork |
|---|---|
| Conversation | The copied turns, with the work that Vis did and the files attached to them |
| Title | The original title, then `(fork)`. Rename the fork to tell the two sessions apart |
| Project and group | The same as the original session |
| Settings | Your global, project, and group [settings](configuration.md#project-group-and-session-settings), without the settings that you changed for the original session only |
| A turn that still runs | The turn as it is at the moment of the fork. The copy shows **Forked from this point.** The turn continues in the original session |

## Organize sessions into groups

A group collects related sessions under a project, such as "Release apps" or "Bug triage". It
shows its own name, colour, and session count, and it opens and closes like any other fold. A
group also narrows which sessions talk to each other in [Council](council.md#groups-and-settings).

- In the desktop or phone app, open the **⋮** menu in the header of a project and choose
  **New group**. The same menu renames a group, changes its colour, and deletes it. It also files
  the sessions listed on that page into a group. To move one session, choose **Move to...** in its
  actions, or drag its row onto the band of a group.
- In the terminal, open Projects. Press **G** on a project, **Groups**, or **Sessions** row to
  create a group. On a group row, **R** renames the group, **c** changes its colour, **A**
  archives it, and **D** deletes it. To file sessions, press **m** on a session row, or mark
  several with **Space** and press **m** on the group row. **Ctrl+X s m** moves the session that
  you are in.

When you delete a group, Vis asks if it must also delete the sessions. Kept sessions return to
the project without a group.

## Rename, star, archive, or delete a session

You cannot undo the deletion of saved sessions. Vis asks you to confirm before it removes a
project or deletes a group or a session.

- In the desktop or phone app, the actions of a session include **Rename**, **Star**,
  **Archive**, and **Delete**.
- In the terminal, select a session row in Projects. The keys are **d** for its details, **R** to
  rename it, **f** to star it, **A** to archive it, and **D** to delete it. Press **v** to show
  archived sessions and **V** to show archived groups.

Stars are shared, so a session that you star in the terminal is also starred in the apps.
Archived sessions leave the usual lists until you show them.

## Manage sessions from the command line

The `vis-agent sessions` commands read the sessions saved on this computer, also when your
terminal connects to a remote gateway. A session id is the full id or any unambiguous prefix from
`vis-agent sessions list`.

```bash
vis-agent sessions list                   # saved sessions and their ids
vis-agent sessions search "release notes" # search conversation text
vis-agent sessions show 3a7b2c1d          # one session's details and turns
vis-agent sessions delete 3a7b2c1d        # permanent
```

`vis-agent sessions fork` does not create a second session. It records a branch point in the same
session, and `vis-agent sessions show` lists that point. For a separate copy, fork in the terminal
or in an app.

## Export a session

Vis does not remove private data from an export. Read an export before you share it. To remove
private details, follow [Reporting a bug](reporting-bugs.md#sharing-a-transcript).

You can export a session after its first turn. An export is one file in one of these formats:

- **HTML** is a self-contained page that opens in a browser.
- **Markdown** is the text of each turn, tool calls included.
- **Snapshot** is a `.vissnap` file that Vis imports as a new session. To use it, read
  [Move a session to another computer](#move-a-session-to-another-computer).

To export a session:

- In the terminal, press **Ctrl+X s e**, then choose the format. Vis saves the file in
  **Vis Artifacts** in your downloads folder and shows a notice with the file name. Click the file
  name to open its folder. To export another session, select it in the session switcher and press
  **Ctrl+X**, or press **e** on its row in **Projects**.
- In the desktop or phone app, open the actions of the session in the session list. Choose
  **Export**, then the format. The desktop app saves the file in a new folder in **Vis Artifacts**
  in your downloads folder and shows it in the file manager. Click the path in the notice to show
  the file again. The phone app opens the share sheet.
- In a browser, the file goes to your downloads.

If the desktop app or the browser can share a file, Vis asks you to keep the file or to share it.

### Export from the command line

Use a session id from `vis-agent sessions list`:

```bash
vis-agent sessions export <SESSION-ID> [--md | --html PATH | --snapshot PATH]
```

### Markdown

Markdown is the default format. The command prints the transcript, tool calls included, to stdout:

```bash
vis-agent sessions export 3a7b2c1d > session.md
```

### HTML

`--html` writes a self-contained page that also holds the conversation of each subagent. Select a
subagent row to open its conversation. Use the path at the top of the page to go back. Vis creates
missing directories and adds `.html` if the output path has no extension:

```bash
vis-agent sessions export 3a7b2c1d --html report.html
```

## Move a session to another computer

A snapshot is one `.vissnap` file with a session, its subagent sessions, and their Council
messages. An import gives you a new session that you can continue. A snapshot is also a full
backup.

A snapshot holds the full conversation, also tool output and attached files. Vis does not remove
private data from it. Share it only with people who may read the whole session.

1. On the first computer, write the snapshot. Vis adds `.vissnap` if the path has no
   extension:

   ```bash
   vis-agent sessions export 3a7b2c1d --snapshot work.vissnap
   ```

2. Copy `work.vissnap` to the other computer.
3. On the other computer, go to the project folder and import the file:

   ```bash
   cd ~/code/shop
   vis-agent sessions import work.vissnap
   ```

4. Continue the session with the id that the import prints:

   ```bash
   vis-agent tui --session-id <NEW-SESSION-ID>
   ```

You can also import the file without the command line:

- In the desktop app, the phone app, or a browser, open the actions of **Sessions** in a project
  in the session list. Choose **Import snapshot…**, then choose the file. Vis opens the new
  session. To put the new session in a group, use the actions of that group.
- In the terminal, press **Ctrl+X s i** and type the path of the file. If the project has groups,
  choose its sessions or one group. Vis opens the new session. In **Projects**, press **i** on a
  row to import into the project or the group of that row.

The imported session uses the folder where you run the command. These options change that:

| Option | Effect |
|---|---|
| `--root DIR` | Use `DIR` as the project folder. |
| `--group ID` | Put the session in this session group. Without `--root`, use the project folder of the group. |
| `--key KEY` | Import only once. A second import with the same key prints the first session and creates nothing. |

The permissions on the new computer come from its own policy, not from the snapshot. The
**Session snapshot limit** setting sets the largest snapshot, and its default is 1 GiB. A program
can do the same steps with the
[Sessions API](sessions-api.md#save-and-restore-a-session-snapshot).

## See also

- For every terminal key, the session commands on this page included, read
  [Keyboard shortcuts](keyboard-shortcuts.md).
- To follow and control the same session from another device, read
  [Desktop and mobile setup](index.md#connect-an-app).
- To ask another session for help or a second review, read [Council](council.md).
- To remove private information before you share an export, read
  [Reporting a bug](reporting-bugs.md).
- For the slash commands and shell shortcuts that you can queue, read
  [Project instructions](project-instructions.md).
- To create, follow, and manage sessions from your own program, read
  [Sessions API](sessions-api.md).
