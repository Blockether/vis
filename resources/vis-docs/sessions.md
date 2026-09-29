# Managing sessions

Vis saves each conversation as a session, so you can close it and continue later.
This page shows how to start, find, fork and organize sessions in the terminal, in
the desktop and phone apps and from the command line.

## When to use

- **You want to try another approach without losing this conversation.** [Fork
  the session](#fork-a-session).
- **The last few turns went wrong and you want to go back.** [Fork from an earlier
  turn](#fork-from-an-earlier-turn).
- **You need a conversation from last week.** [Find the
  session](#find-a-saved-session) by its title or by something said in it.
- **Your project has more sessions than you can scan.** [Put them into
  groups](#organize-sessions-into-groups), star the ones you use often and
  [archive](#rename-star-archive-or-delete-a-session) the ones you have finished.
- **You want to send a follow-up or stop Vis while it works.** Read [Controlling a
  session](queue-and-cancel.md) instead.
- **You want to save or share a conversation as a file.** Read [Exporting
  sessions](exporting-sessions.md) instead.

## Start a new session

In the terminal, press **Ctrl+X n**. In the desktop or phone app, choose **New session**. To start
it inside a group, use the **+** on the group's band in the app. You can also choose **＋ New session
here** in the group's **g** menu in [Projects](#find-a-saved-session). The session then opens at the
top of that group.

## Find a saved session

### In the terminal

Press **Ctrl+X s** to open the session switcher. Type to search session titles
and conversation text, choose a session with **↑** and **↓**, and press **Enter**
to open it. The switcher also has these keys:

| Keys | What they do |
|---|---|
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

To search saved session titles and conversation text, press **/** while Projects has focus. The
search also finds sessions outside the pages on screen. Result-page rows show more matches. Press
**Esc** to go back to your previous folds and page.

Projects shows only part of your sessions and groups at a time. To see the next page of a list,
choose **More sessions** or **More groups**. To show rows that arrived while you were reading,
choose **new updates**. The list you were reading does not move.

### In the desktop or phone app

The app lists sessions under their projects, with groups inside each project.
Choose a session to open it. On a phone, pull the session list down to search it.

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

## See also

- [Keyboard shortcuts](keyboard-shortcuts.md) — every terminal key, including the session commands on this page.
- [Exporting sessions](exporting-sessions.md) — save a conversation as Markdown or HTML and check it before you share it.
- [Controlling a session](queue-and-cancel.md) — queue follow-ups, cancel a turn and quit while Vis works.
