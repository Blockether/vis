# Managing sessions

Vis saves each conversation as a session, so you can close it and continue later.
This page shows how to start a session, find an earlier one, fork a conversation
to try another approach and keep a busy project organized, in the terminal and in
the desktop and phone apps.

## When to use

- **You want to try another approach without losing this conversation.** [Fork
  the session](#fork-a-session). The fork is a separate copy, and the original
  stays as it is.
- **The last few turns went wrong and you want to go back.** [Fork from an earlier
  turn](#fork-from-an-earlier-turn) and continue from the last turn you want to keep.
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

In the terminal, press **Ctrl+X n**. In the desktop or phone app, choose **New
session**, or use the **+** on a group's band to start the session in that group.

A new session uses the global, project and group settings that apply to it. See
[Project, group and session settings](configuration.md#project-group-and-session-settings).

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
terminal. Use **↑** and **↓** to choose a project, group or session. Press
**Enter** to expand a project or group, or to resume a session. **Tab** does not
switch sessions.

Press **/** while Projects has focus to search saved session titles and
conversation text, even outside the pages on screen. Use the result-page rows to
see more matches; press **Esc** to return to your previous folds and page.
Projects keeps a window of sessions and groups at a time. Choose **More
sessions** or **More groups** to page each list separately. When new rows arrive,
choose **new updates** to show them without moving the list you were reading.

### In the desktop or phone app

The app lists sessions under their projects, with groups inside each project.
Choose a session to open it. On a phone, pull the session list down to search it.

## Fork a session

A fork is a new session that starts with a copy of another session's
conversation. Use it to try a different approach, or to return to an earlier
point, without changing the original. After you fork, the two sessions continue
separately.

### Fork the whole conversation

- **Terminal:** press **Ctrl+X y**. Vis opens the fork in a new tab and switches
  to it. To fork a different session, press **Ctrl+X s**, select that session and
  press **Ctrl+F**.
- **Desktop or phone app:** open the session's actions in the session list and
  choose **Fork**. The app opens the fork.

### Fork from an earlier turn

A turn is one message you sent and the work Vis did to answer it. When you fork
from a turn, the fork keeps that turn and every turn before it, and leaves out the
turns after it.

- **Terminal:** press **Ctrl+X t** to open **Fork session at…**, a list of the
  messages that started each turn. Type to filter the list, choose the last turn
  you want to keep and press **Enter**. If your terminal reports mouse clicks, you
  can instead click **Fork from this turn** in the header of a finished answer; in
  a narrow window, the button reads **Fork**.
- **Desktop or phone app:** choose **Fork from this turn** in the header of a
  finished answer.

### What a fork keeps

- **Conversation:** every copied turn, with the work Vis did and the files
  attached to it.
- **Title:** the original title followed by `(fork)`. Rename the fork to tell the
  two sessions apart.
- **Project and group:** the same as the original session.
- **Settings:** your global, project and group settings. Settings you changed for
  the original session only are not copied.

A fork copies the conversation, not your files. Changes Vis made to your project
in later turns stay in place, so forking from an earlier turn does not undo them.
You can fork a session once it has a turn, and the fork buttons appear only on
finished answers.

## Organize sessions into groups

A busy project collects more sessions than one screen holds. Put the ones that
belong together into a group, such as "Release apps" or "Bug triage". Each group
gets its own name, colour and session count under the project, and opens and
closes like any other fold.

In the desktop or phone app, open the **⋮** menu in a project's header and choose
**New group**. The same menu renames a group, changes its colour, deletes it and
files the sessions listed on that page into a group. A session row also carries
**Move to...** in its own actions, and you can drag a row onto a group's band to
file it there. In the terminal, press **Ctrl+X w** for Projects and **g** on a
project, group or session row for its menu. **Ctrl+X d** moves the session you
are in.

To start a conversation straight inside a group, use the **+** on the group's band
in the app, or **＋ New session here** in the terminal's **g** menu on that group
row. The new session is filed as it is created, so it opens at the top of that
group instead of loose in the project.

Deleting a group asks what becomes of the sessions filed under it: keep them, and
they return to the project ungrouped, or delete them with the group. Deleting the
sessions cannot be undone. A group also narrows who a session talks to in
[Council](council.md#groups-and-settings).

## Rename, star, archive or delete a session

In the terminal, press **Ctrl+X w** for Projects and **g** on a session row for
its details, star, rename, move, archive and delete actions. Press **Space** to
mark several sessions, then use the group or **Sessions** menu to move them
together. Use **g** on **Groups** to create a group or show archived groups; the
**Sessions** menu starts a new session or shows archived sessions. You can use the
same row and menu actions with a mouse when your terminal reports clicks.

In the desktop or phone app, a session's actions include **Rename**, **Star**,
**Move to...**, **Archive** and **Delete**.

Stars are shared, so a session you star in the terminal is also starred in the
apps. Archived sessions leave the usual lists until you show archived sessions.
Removing a project or deleting a group or session asks for confirmation, and
deleting saved sessions cannot be undone.

## Manage sessions from the command line

The `vis-agent sessions` commands read the sessions saved on this computer, even
when you connect the terminal to a remote gateway. A session id can be the full id
or any unambiguous prefix from `vis-agent sessions list`.

```bash
vis-agent sessions list                   # saved sessions and their ids
vis-agent sessions search "release notes" # search conversation text
vis-agent sessions show 3a7b2c1d          # one session's details and turns
vis-agent sessions export 3a7b2c1d > session.md
vis-agent sessions delete 3a7b2c1d        # permanent
```

[Exporting sessions](exporting-sessions.md) explains the export formats.

`vis-agent sessions fork` is not the fork described above. It keeps the same
session and id and records a branch point that `vis-agent sessions show` lists,
so it does not give you a second session to compare with the original. Fork in the
terminal or in the desktop or phone app when you want a separate copy.

## See also

- [Controlling a session](queue-and-cancel.md) — queue follow-ups, cancel a turn and quit while Vis works.
- [Exporting sessions](exporting-sessions.md) — save a conversation as Markdown or HTML and check it before you share it.
- [Keyboard shortcuts](keyboard-shortcuts.md) — every terminal key, including the session commands on this page.
- [Configuration](configuration.md#project-group-and-session-settings) — how global, project, group and session settings combine for new sessions and forks.
- [Council](council.md#groups-and-settings) — how groups narrow which sessions talk to each other.
