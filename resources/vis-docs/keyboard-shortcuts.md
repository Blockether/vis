# Keyboard shortcuts

This page lists the keyboard shortcuts, also called keybindings, of the Vis terminal app. The
desktop and phone app uses [the same Ctrl+X keys](#in-the-desktop-or-phone-app) for the commands
that it has.

To see the most common shortcuts while you work, press **Ctrl+X h**.

## When to use

- **Shift+Enter sends your message and does not start a new line.** Read
  [New lines in your terminal](#new-lines-in-your-terminal).
- **You want a command but do not know its key.** Press **Ctrl+X p** and type part of its name, or
  find it in [Run a command](#run-a-command).
- **You want to read an earlier part of a long session.** Read
  [Move through a session](#move-through-a-session).
- **You do not know which area gets your keys, or you want to go to Projects.** Read
  [Move between areas](#move-between-areas).
- **You want to fork a session, go back to an earlier turn, or move a session to a group.** Use
  the keys in [Sessions](#sessions) and [In Projects](#in-projects).
- **You want to dictate a message, or voice recording fails.** Read
  [Use voice input](#use-voice-input).
- **You want to see the pictures of a message larger.** Read [View pictures](#view-pictures).
- **You want to use an app, such as a review, a form, or a game.** Read [Use an app](#use-an-app).
- **An extension added a key, or a key does something new.** Read
  [Keys from extensions](#keys-from-extensions).
- **You want to send an earlier message again, or run a command by key, in the desktop or phone
  app.** Read [In the desktop or phone app](#in-the-desktop-or-phone-app).
- **You want to know what a cancel or a quit does to your queued messages.** Read
  [Control a running session](sessions.md#control-a-running-session).

## Send a message or start a new line

| Keys | What they do |
|---|---|
| Enter | Send the message, or queue it while Vis works |
| Shift+Enter or Alt+Enter | Start a new line |

### New lines in your terminal

Ghostty, kitty, iTerm2, WezTerm, Alacritty, foot, xterm, Konsole, and Windows Terminal report
Shift+Enter with no setup. Windows Terminal does so with WSL too. In Windows Terminal, Alt+Enter
switches to full screen, so use Shift+Enter there.

Other terminals need a setting or a different key.

- **GNOME Terminal and other VTE-based terminals.** Shift+Enter sends the same code as Enter. Use
  Alt+Enter.
- **Terminal on macOS.** Turn on **Use Option as Meta key** in **Settings** > **Profiles** >
  **Keyboard**. Then use Option+Enter.
- **tmux.** Add `set -g extended-keys on` to `~/.tmux.conf`.
- **Windows Terminal over SSH.** This applies when Vis runs on a computer that you connect to with
  `ssh`. Open **Settings** and select **Open JSON file**. Then add this entry to the `actions` list:

  ```json
  { "command": { "action": "sendInput", "input": "\u001b[13;2u" }, "keys": "shift+enter" }
  ```

## Run a command

Most commands use **Ctrl+X**, a group key, and a command key. For example, press **Ctrl+X**, then
**s** for the session group, then **n** to start a new session. After **Ctrl+X**, Vis shows the
groups, and after the group key, the commands of that group. A command that needs something first,
such as a turn to fork, appears when it can act.

| Keys | Group or command |
|---|---|
| Ctrl+X s | Sessions: new, switch, delete, archive, fork, group, export, and metrics |
| Ctrl+X g | Group of this session: new, rename, colour, settings, archive, and delete |
| Ctrl+X m | Models and answers: model, thinking level, answer length, and providers |
| Ctrl+X b | View: search, jump, folds, zen mode, side panel, **Projects**, and agent details |
| Ctrl+X a | Attachments: attach a file, review attachments, view pictures, and play recordings |
| Ctrl+X v | Voice: recording and voice conversation |
| Ctrl+X q | Queue: send or edit queued messages |
| Ctrl+X t | Agents: open a subagent, list the subagents, or go back to the parent session. This group appears only when it can act |
| Ctrl+X e | Keys from extensions. This group appears only when an extension adds a key |
| Ctrl+X p | Open the command palette with every command, then type to filter |
| Ctrl+X h | Show or hide the keyboard shortcuts |

To close the group list without a command, press **Esc** or **Ctrl+G**.

### Keys from extensions

An extension can add its own keys, or move a command of this page to a different key. The added
keys appear under **Ctrl+X e** and in the help of **Ctrl+X h**. An extension can also put a key in
a group of the table above, or make its own group under **Ctrl+X e**. For example, **Ctrl+X e r**
can open a `Release` group, and the next key runs one of its commands.

A key from an extension does one of two things:

- It runs a command as if you typed the command and pressed Enter. Your draft stays as it is.
- It runs a function of the extension. The key adds no message to the transcript. When the
  function ends, Vis shows its result as a notice.

Vis applies these rules to the keys:

- A key that an extension moves replaces the key on this page. The old key then does nothing. The
  command palette still has the command.
- An extension cannot use **g** or a digit as a key in a group.
- When two keys conflict, Vis keeps the first key and shows one notice.

To move a command, the extension names its verb id.

| Verb id | Default key | What it does |
|---|---|---|
| `new-session` | Ctrl+X s n | Start a new session |
| `show-sessions` | Ctrl+X s s | Switch to another session |
| `delete-session` | Ctrl+X s d | Delete this session |
| `archive-session` | Ctrl+X s a | Archive or restore this session |
| `session-metrics` | Ctrl+X s u | Show session metrics |
| `export-session` | Ctrl+X s e | Export this session to a file |
| `import-session` | Ctrl+X s i | Import a snapshot as a new session |
| `queue-send-all-now` | Ctrl+X q s | Send the queued messages now |
| `queue-edit` | Ctrl+X q e | Edit, send, remove, or merge queued messages |
| `toggle-agent-dock` | Ctrl+X b a | Show or hide the agent team details |
| `agent-team` | Ctrl+X t t | List the subagents, then open one |
| `agent-parent` | Ctrl+X t b | Open the session that started this subagent |
| `fork-session` | Ctrl+X s f | Fork this session |
| `fork-at-turn` | Ctrl+X s t | Fork from an earlier turn |
| `switch-project` | Ctrl+X b w | Open **Projects** |
| `session-group` | Ctrl+X s m | Move this session to a group |
| `group-new` | Ctrl+X g n | Create a group in the project of this session |
| `group-rename` | Ctrl+X g r | Rename the group of this session |
| `group-recolour` | Ctrl+X g c | Change the colour of the group of this session |
| `group-settings` | Ctrl+X g s | Open the settings of the group of this session |
| `group-archive` | Ctrl+X g a | Archive or restore the group of this session |
| `group-delete` | Ctrl+X g d | Delete the group of this session |
| `cycle-model` | Ctrl+X m m | Use the next model |
| `pick-model` | Ctrl+X m c | Choose a model |
| `pick-reasoning` | Ctrl+X m r | Choose the reasoning level |
| `toggle-codex-fast` | Ctrl+X m f | Turn fast mode on or off |
| `cycle-verbosity` | Ctrl+X m l | Change the answer length |
| `toggle-thinking-summary` | Ctrl+X m t | Show or hide the thinking summary |
| `search-open` | Ctrl+X b f | Search the conversation |
| `recenter` | Ctrl+X b j | Jump to the bottom |
| `live-panel` | Ctrl+X b p | Show or hide the side panel |
| `toggle-detail-labels` | Ctrl+X b z | Show or hide the fold labels |
| `toggle-zen-mode` | Ctrl+X b Z (Shift+Z) | Show or hide the header and the footer |
| `toggle-help` | Ctrl+X h | Show or hide the keyboard shortcuts |
| `pick-file` | Ctrl+X a a | Attach a file |
| `focus-attachments` | Ctrl+X a i | Open the attachments and apps |
| `view-images` | Ctrl+X a v | View the pictures |
| `open-newest-image` | Ctrl+X a o | Open the newest picture |
| `play-recording` | Ctrl+X a p | Play or stop a recording |
| `toggle-voice-recording` | Ctrl+X v v | Start or stop voice input |
| `toggle-voice-conversation` | Ctrl+X v c | Start or stop a voice conversation |
| `providers` | Ctrl+X m p | Open the providers |

### Sessions

| Keys | What they do |
|---|---|
| Ctrl+X s n | Start a new session |
| Ctrl+X s s | Switch to another session. In the list, **Ctrl+A** shows only archived sessions. Press it again to show the others |
| Ctrl+X s d, Ctrl+X s Delete, or Ctrl+X s Backspace | Delete this session permanently. Vis asks you to confirm first |
| Ctrl+X s a | Archive this session, or restore it if it is archived. You cannot archive a session while it works |
| Ctrl+X b w | Open **Projects**, the list of saved sessions |
| Ctrl+X s f | Fork this session. The new session has a copy of the whole conversation |
| Ctrl+X s t | Fork from an earlier turn. Choose the last turn that the new session keeps |
| Ctrl+X s m | Move this session to a group |
| Ctrl+X g n, r, c, s, a, or d | Create a group. Or rename, colour, open the settings of, archive, or delete the group of this session. A group command appears only when this session is in a group |
| Ctrl+X s u | Show the session metrics: context health, totals, and cache |
| Ctrl+X s e | Export this session as HTML, Markdown, or a snapshot. Vis saves the file in **Vis Artifacts** in your downloads folder |
| Ctrl+X s i | Import a `.vissnap` snapshot as a new session. Type the path of the file. If the project has groups, choose its sessions or one group |
| Ctrl+X q s | Send the queued messages now. The running turn reads them at its next step |
| Ctrl+X q 1 to 9 | Send that queued message at the next step. Press again to keep it for the end of the turn |
| Ctrl+X q e | Select queued messages to edit, send, remove, or merge them. Read [Queued messages](#queued-messages) |
| Ctrl+X b a | Show or hide the agent team table under the one-line **Agents** summary. A click on the summary does the same |
| Ctrl+X t 1 to 9 | Open the subagent with that number. The **#** column of the agent team table shows the numbers |
| Ctrl+X t t | List the subagents of this session, then open one. A click on a row of the agent team table also opens that subagent |
| Ctrl+X t b | In a subagent session, go back to the session that started the subagent |

### Queued messages

After **Ctrl+X q e**, these keys act on the queued messages until you press **Esc**. The line under
the queue shows them. A different key returns to the prompt and does its usual work.

| Keys | What they do |
|---|---|
| Up, Down, or 1 to 9 | Select a queued message |
| Space | Mark or unmark the selected message |
| Enter | Put the marked messages, or the selected one, back in the prompt below your draft. Vis merges them oldest first |
| s | Send the marked messages, or the selected one, at the next step of the running turn |
| l | Keep the marked messages, or the selected one, for the end of the turn |
| Delete or Backspace | Remove the marked messages, or the selected one, from the queue |
| u | Put the removed messages back at the end of the queue. This key works for 8 seconds |
| Esc | Return to the prompt |

### Models and answers

| Keys | What they do |
|---|---|
| Ctrl+X m p | Open **Providers** to add a provider and sign in |
| Ctrl+X m c | Choose a model from a list that you can search |
| Ctrl+X m m | Switch to the next model |
| Ctrl+X m r | Change the thinking level |
| Ctrl+X m l | Change the answer length |
| Ctrl+X m f | Turn fast mode on or off for OpenAI Codex models |
| Ctrl+X m t | Show or hide the thinking summary for Claude models |

The thinking label in the footer and **Thinking Summary** in the command palette do the same as
Ctrl+X m t. With the summary off, Claude still thinks, but Vis shows no thinking text. These
controls appear only when the current model supports them.

With **Simplified thinking modes** on, Ctrl+X m r switches to the next of quick, balanced, and
deep. With the setting off, Ctrl+X m r lists every thinking level of the current model. The
setting is in the top section of **Settings**. A click on the reasoning label in the footer does
the same as the key.

### Files, voice and search

| Keys | What they do |
|---|---|
| Ctrl+X b f | Search in the session |
| Ctrl+X a a | Attach a file |
| Ctrl+X a i | Review your attached files, the files that this session produced, and its apps. Enter opens the selected app |
| Ctrl+X a v | Show the pictures of this session one at a time. Only terminals that show pictures have this key. Read [View pictures](#view-pictures) |
| Ctrl+X a o | Open the newest picture of this session in the picture app of your computer |
| Ctrl+X a p | Play a recording of this session, or stop the recording that plays. With more than one recording, choose one from the list. The status line shows the time. A file that is not WAV or AIFF needs `ffmpeg` |
| Ctrl+X v v | Start or stop a voice recording |
| Ctrl+X v c | Turn voice conversation on or off. Vis reads each answer aloud, and sends each recording when its transcript is ready |
| Ctrl+X b p | Show the live view panel at the side, or move the focus to it. In the panel, Up and Down select a view, Left folds it, and Right unfolds it. Tab shows the next extension, and Esc closes the panel |

## View pictures

Vis shows the pictures of a message in one table. Each row has a small preview, the name, the type,
the size, and an **Open externally** button. The button opens the picture in the picture app of
your computer. kitty, Ghostty, WezTerm, Warp, and iTerm2 show the previews. Other terminals show
the same table without previews.

- To see one picture larger, click its preview.
- To see every picture of the session, press **Ctrl+X a v**. The picture viewer opens and fills
  most of the window.

A terminal that cannot show pictures has no picture viewer. There, use **Ctrl+X a o** or the
**Open externally** buttons.

To zoom at the pointer, turn the mouse wheel over the picture. The header shows the zoom, for
example `250%`. To move a zoomed picture, drag it or press the arrow keys.

| Keys | What they do |
|---|---|
| Left or Up | Show the previous picture. On a zoomed picture, move it |
| Right or Down | Show the next picture. On a zoomed picture, move it |
| PgUp or PgDn | Show the previous or the next picture |
| Home or End | Show the first or the last picture |
| 1 to 9 | Show the picture with that number |
| + or - | Zoom in or out |
| 0 | Show the whole picture |
| Enter or o | Open the picture in the picture app of your computer |
| Esc or q | Close the picture viewer |

## Use an app

An app is a screen that the agent or an extension attaches to a session, such as a diff to review.
To open an app, click its row in the transcript. Or press **Ctrl+X a i**, select the app, and press
Enter. In a review app, you write comments on lines and in threads, save them as drafts, and send
them together.

| Keys | What they do |
|---|---|
| Up or Down | Select the previous or the next row. PgUp, PgDn, Home, and End move further |
| Left or Right | Fold or unfold the selected file or group |
| < or > | Scroll long lines to the side |
| Enter | Press the selected button, card, or table row. On a field, edit its value. On a line, write a comment. On a file row, fold or unfold the file |
| Ctrl+S | In the comment editor, save the comment as a draft |
| s | Send every saved draft and the values of the fields |
| r | Load the app again |
| Esc | In the comment editor, cancel the comment. In other places, close the app |

### Keys of an app

An app can define its own keys, for example arrow keys in a game. The row above the status line
shows each key of the app and its label. When you press one, the status line shows the label. A
check mark follows when the app has its answer. A closed app does not use its own keys.

An app key can be a key from the table above, for example **s** or an arrow. To use the key from
the table then, press **Ctrl+X** first. For example, **Ctrl+X s** still sends the app. The status
line shows `C-x s` for each such key.

An app that defines no keys gets keys from the terminal. Each tab gets a digit, and each other
button gets a letter of its label. The hint bar at the bottom of the app shows these keys with
their labels, and `•` marks the open tab. A button that is not available keeps its row.

After Ctrl+S, the app shows `Saved · state N` when the gateway has saved your draft. After s, it
shows `Sent` and the id of the receipt. A closed or archived app is read only. There, Enter and s
show `Closed: read only`. For developers, [Live views](live-views.md#apps) explains apps.

## Use voice input

Connect a microphone before you start the terminal app. On macOS, permit microphone access for
your terminal or Vis when macOS asks.

Press **Ctrl+X v v** to start a recording. Press it again to transcribe the recording into your
message.

Vis first tries Java Sound. If Java Sound cannot capture, macOS can use SoX or FFmpeg. Vis looks
for them on `PATH` and in both standard Homebrew locations, `/opt/homebrew/bin` and
`/usr/local/bin`. If neither is installed, install one:

```bash
brew install sox
```

For FFmpeg, run `brew install ffmpeg`.

On Linux, the fallback recorders are `pw-record` and `parec`. WSL2 also needs a WSLg audio server
that Vis can reach.

To list the audio devices and the recorder paths that Vis finds, run:

```bash
vis-agent tui --check-audio
```

This check does not test the microphone permission, and it captures no audio.

If a recorder is installed but capture fails on macOS, check **System Settings > Sound > Input**.
If macOS reports denied access, permit it under **Privacy & Security > Microphone**. After you
connect or select a different microphone, start the terminal app again.

## Cancel or quit

| Keys | What they do |
|---|---|
| Esc or Ctrl+G | Cancel the running turn, close a dialog, or clear your draft |
| Ctrl+C | Clear your draft, cancel the running turn, or quit Vis |

[Control a running session](sessions.md#control-a-running-session) explains which of these effects
a key has, and what it does to your queued messages.

## Move through a session

| Keys | What they do |
|---|---|
| Alt+>, Ctrl+X b j, Ctrl+L, or Ctrl+End | Jump to the latest message |
| Alt+< | Jump to the start of the session |
| Ctrl+V or Page Down | Scroll down one screen |
| Alt+V or Page Up | Scroll up one screen |
| Ctrl+X Tab or Ctrl+X Shift+Tab | Fold or unfold every block that folds, such as thinking and tool calls |
| Ctrl+X b z | Label every fold. Then press the letter of a label to fold or unfold it |

A click on **↓ messages**, when it shows, also jumps to the latest message.

On macOS, Alt is the Option key. Many macOS terminals need a setting before Option works as Alt,
such as **Use Option as Meta key** in Terminal.

## Move between areas

When **Projects** or the side panel is open, the screen has more than one area. Only one area gets
your keys. Vis shows the other areas in faded colors.

Press **Ctrl+O** to go to the next area. The order is Projects, the chat, the side panel, and
then Projects again. When only the chat is open, Ctrl+O does nothing.

## In Projects

Press **Ctrl+X b w** to open **Projects**. While it has the focus, these keys act on the selected
row.

Press **?** or **Ctrl+H** to open the sidebar menu. It lists every key below, and the list is the
same on every row. Choose an entry to run it on the selected row. Read
[In the terminal](sessions.md#in-the-terminal) for mouse controls.

| Keys | What they do |
|---|---|
| Up or Down, Ctrl+P or Ctrl+N | Move between rows |
| Left or Right | Fold or unfold the row |
| Enter | Open the row |
| ? or Ctrl+H | Open the sidebar menu |
| n | Start a session here |
| G (Shift+G) | Create a group |
| a | Add a project |
| p | Use the project. Open its current view, a saved session, or a new one |
| F (Shift+F) | Change the folder of the project |
| + | Add a group, session, or project here |
| Space | Select or clear a session |
| m or u | Move sessions to a group, or take them out of it |
| f | Star or unstar a session |
| e | Export a session to a file |
| i | Import a session from a snapshot into the project or the group of the row |
| R (Shift+R) | Rename the row |
| A (Shift+A) | Archive or restore the row |
| v | Show or hide the archived sessions of the project |
| V (Shift+V) | Show or hide the archived groups of the project |
| N, S, H, or X (Shift) | On a session in a group: rename, open the settings of, archive, or delete its group |
| D (Shift+D) | Delete the row. Vis asks you to confirm first |
| / | Search sessions |
| Esc or Tab | Return to the chat |
| Ctrl+O | Go to the next area |

## Edit your message

| Keys | What they do |
|---|---|
| Ctrl+A | Go to the start of the line |
| Ctrl+E | Go to the end of the line |
| Ctrl+B | Move back one character |
| Ctrl+F | Move forward one character |
| Ctrl+P | Go to the previous line |
| Ctrl+N | Go to the next line |
| Alt+Left or Alt+Right | Move one word back or forward, where your terminal supports it |
| Up or Down | Move the cursor up or down, or step through earlier messages |
| Ctrl+T | Swap the two characters next to the cursor |
| Ctrl+K | Delete to the end of the line |
| Ctrl+U | Delete to the start of the line |
| Ctrl+W | Delete the word before the cursor |
| Ctrl+D | Delete the character after the cursor |

To copy, select text in your terminal. To paste, use the paste key of your terminal.

## In the desktop or phone app

These keys work in the message box of the Companion app on a computer, and on a tablet with a
keyboard.

| Keys | What they do |
|---|---|
| Enter | Send the message |
| Shift+Enter | Start a new line |
| Up | Show your previous message in this session. Press again to go further back |
| Down | Go forward to a newer message, and then back to the text that you typed |

Up and Down show an earlier message only when the cursor is on the first or the last line. In a
message with more lines, they first move the cursor. The app keeps your 20 newest messages. It
keeps `/goal`, but no other command such as `/reload`. It does not keep messages that other
sessions send through Council. On a phone with no keyboard, Return starts a new line, and the send
button sends the message.

The app uses the same **Ctrl+X** keys as the terminal app, also when the cursor is in the message
box. After **Ctrl+X**, a band at the bottom of the window shows the keys that you can press next.

| Keys | What they do |
|---|---|
| Ctrl+X p | Open the command palette. Type part of a command name, then press Enter |
| Ctrl+X h | Show all keys. A key that the app cannot use now is dimmed |
| Ctrl+X s s | Search your sessions |
| Esc or Ctrl+G | Cancel the keys that you started |
| / | Search your sessions when the cursor is not in a text field |

The app shows and runs only the commands that it can use now. For example, **Ctrl+X a a** attaches
a file only when the message box is open. Some terminal commands are not in the app, such as
groups, session metrics, and the picture keys. When you select text in a field, **Ctrl+X** cuts
the text and does not start a key.

To see a picture larger in the app, click or tap it. If the message has more pictures, use these
keys in the picture viewer. On a phone, swipe left or right.

| Keys | What they do |
|---|---|
| Left or Right | Show the previous or the next picture |
| Esc | Close the picture viewer |

## See also

- To learn what Enter, Esc, and Ctrl+C do while Vis works, and how to find, fork, and organize
  sessions, read [Sessions](sessions.md).
- To start Vis in your terminal and send a first task, read
  [Quickstart](quickstart.md#in-the-terminal).
- To report a shortcut that does not work in your terminal, read
  [Reporting a bug](reporting-bugs.md).
