# Keyboard shortcuts

Use these keyboard shortcuts, also called keybindings, in the Vis terminal app. With them you can
send a message, start a new line, run commands, move through a session and edit your text. The
desktop and phone app has [a smaller set of keys](#in-the-desktop-or-phone-app).

To see the most common shortcuts while you work, press **Ctrl+X h** in Vis.

## When to use

- **Shift+Enter sends your message instead of starting a new line.** Your terminal
  needs a setting, or you need another key. See [New lines in your
  terminal](#new-lines-in-your-terminal).
- **You want a command but do not know its key.** Press **Ctrl+X p** and type part of
  its name, or look it up in [Run a command](#run-a-command).
- **You want to read an earlier part of a long session.** Use the keys in [Move
  through a session](#move-through-a-session) to scroll, jump and fold.
- **You do not know which area your keys go to, or you want to go to Projects quickly.**
  See [Move between areas](#move-between-areas).
- **You want to fork a session, go back to an earlier turn or move a session to a
  group.** Use the keys under [Sessions](#sessions) and [In Projects](#in-projects). The [Sessions guide](sessions.md)
  explains each task.
- **You want to dictate a message, or voice recording fails.** See [Use voice input](#use-voice-input).
- **You want to see the pictures in a message larger.** See [View pictures](#view-pictures).
- **You want to use an app, such as a review, a form or a game.** See [Use an app](#use-an-app).
- **An extension added a key, or a key does something new.** See [Keys from extensions](#keys-from-extensions).
- **You want to send an earlier message again from the desktop or phone app.** See [In the
  desktop or phone app](#in-the-desktop-or-phone-app).
- **You want to know what cancelling or quitting does to your queued messages.** Read
  [Control a running session](sessions.md#control-a-running-session) instead.

## Send a message or start a new line

| Keys | What they do |
|---|---|
| Enter | Send the message, or queue it while Vis is working |
| Shift+Enter or Alt+Enter | Start a new line |

### New lines in your terminal

Ghostty, kitty, iTerm2, WezTerm, Alacritty, foot, xterm, Konsole and Windows
Terminal, also with WSL, report Shift+Enter without extra setup. In Windows
Terminal, Alt+Enter switches to full screen, so use Shift+Enter there.

Other terminals need a setting or another key. These are limits of the terminal,
not something Vis can change:

- **GNOME Terminal and other VTE-based terminals:** Shift+Enter sends the same code
  as Enter, so use Alt+Enter.
- **Terminal on macOS:** turn on **Use Option as Meta key** in **Settings** >
  **Profiles** > **Keyboard**, then use Option+Enter.
- **tmux:** add `set -g extended-keys on` to `~/.tmux.conf`.
- **Windows Terminal over SSH:** use this when Vis runs on a computer that you connect to with
  `ssh`. Open **Settings** and select **Open JSON file**. Then add this entry to the `actions` list:

  ```json
  { "command": { "action": "sendInput", "input": "\u001b[13;2u" }, "keys": "shift+enter" }
  ```

## Run a command

Most commands use **Ctrl+X** and a letter: press **Ctrl+X**, then the letter. After
**Ctrl+X**, Vis shows the most common choices. The other commands on this page work
the same way. A command that needs something first, such as a turn to fork, appears
once it can act.

| Keys | What they do |
|---|---|
| Ctrl+X p | Open the command palette with every command, then type to filter |
| Ctrl+X h | Show or hide the keyboard shortcuts |

### Keys from extensions

An extension can add its own **Ctrl+X** keys, or move a command of this page to another key. The
keys that extensions add appear under **Extensions** after **Ctrl+X**, and in the help of **Ctrl+X h**.
Such a key runs its command as if you typed it and pressed Enter. Your draft stays as it is.

- A key that an extension moves replaces the key on this page. The old key then does nothing, and
  the command palette still has the command.
- An extension cannot use **Ctrl+X p**, **Ctrl+X g** or **Ctrl+X** with a digit.
- When two keys conflict, Vis keeps the first one and shows one notice.

To move a command, the extension names its verb id. These are the verb ids and their default keys:

| Verb id | Default key | What it does |
|---|---|---|
| `new-session` | Ctrl+X n | Start a new session |
| `show-sessions` | Ctrl+X s | Switch to another session |
| `delete-session` | Ctrl+X D | Delete this session |
| `archive-session` | Ctrl+X A | Archive or restore this session |
| `session-metrics` | Ctrl+X u | Show session metrics |
| `queue-send-all-now` | Ctrl+X k | Send the queued messages now |
| `queue-edit` | Ctrl+X e | Edit, send, remove or merge queued messages |
| `toggle-agent-dock` | Ctrl+X T (Shift+T) | Show or hide the subagent details |
| `fork-session` | Ctrl+X y | Fork this session |
| `fork-at-turn` | Ctrl+X t | Fork from an earlier turn |
| `switch-project` | Ctrl+X w | Open **Projects** |
| `session-group` | Ctrl+X d | Move this session to a group |
| `cycle-model` | Ctrl+X m | Use the next model |
| `pick-model` | Ctrl+X c | Choose a model |
| `pick-reasoning` | Ctrl+X r | Choose the reasoning level |
| `toggle-codex-fast` | Ctrl+X q | Turn fast mode on or off |
| `cycle-verbosity` | Ctrl+X l | Change the answer length |
| `toggle-thinking-summary` | Ctrl+X x | Show or hide the thinking summary |
| `search-open` | Ctrl+X f | Search the conversation |
| `recenter` | Ctrl+X j | Jump to the bottom |
| `live-panel` | Ctrl+X V | Show or hide the side panel |
| `toggle-detail-labels` | Ctrl+X z | Show or hide the fold labels |
| `toggle-help` | Ctrl+X h | Show or hide the keyboard shortcuts |
| `pick-file` | Ctrl+X a | Attach a file |
| `focus-attachments` | Ctrl+X i | Open the attachments and apps |
| `view-images` | Ctrl+X I (Shift+I) | View the pictures |
| `open-newest-image` | Ctrl+X O (Shift+O) | Open the newest picture |
| `toggle-voice-recording` | Ctrl+X v | Start or stop voice input |
| `toggle-voice-conversation` | Ctrl+X b | Start or stop a voice conversation |
| `providers` | Ctrl+X o | Open the providers |

### Sessions

| Keys | What they do |
|---|---|
| Ctrl+X n | Start a new session |
| Ctrl+X s | Switch to another session |
| Ctrl+X D (Shift+D), Ctrl+X Delete or Ctrl+X Backspace | Delete this session permanently. Vis asks you to confirm first |
| Ctrl+X A (Shift+A) | Archive this session. If the session is archived, restore it. You cannot archive a session while it works |
| Ctrl+X w | Open **Projects**, the list of saved sessions |
| Ctrl+X y | Fork this session: open a new session with a copy of the whole conversation |
| Ctrl+X t | Fork from an earlier turn: choose the last turn the new session keeps |
| Ctrl+X d | Move this session to a group |
| Ctrl+X u | Show session metrics: context health, totals and cache |
| Ctrl+X k | Send the queued messages now: the running turn reads them at its next step |
| Ctrl+X 1 to 9 | Send that queued message now, or press again to keep it for the turn end |
| Ctrl+X e | Select the queued messages to edit, send, remove or merge them. See [Queued messages](#queued-messages) |
| Ctrl+X T (Shift+T) | Show or hide the subagent table under the one-line **Agents** summary. A click on the summary does the same |

### Queued messages

After **Ctrl+X e**, these keys act on the queued messages until you press **Esc**. The line under
the queue shows them. Any other key returns to the prompt and does its usual work.

| Keys | What they do |
|---|---|
| Up, Down or 1 to 9 | Select a queued message |
| Space | Mark or unmark the selected message |
| Enter | Put the marked messages, or the selected one, back in the prompt below your draft. Vis merges them oldest first |
| s | Send the marked messages, or the selected one, at the next step of the running turn |
| l | Keep the marked messages, or the selected one, for the end of the turn |
| Delete or Backspace | Remove the marked messages, or the selected one, from the queue |
| u | Queue the removed messages again, at the end of the queue. This works for 8 seconds |
| Esc | Return to the prompt |

### Models and answers

| Keys | What they do |
|---|---|
| Ctrl+X o | Open **Providers** to add a provider and sign in |
| Ctrl+X c | Choose a model from a searchable list |
| Ctrl+X m | Switch to the next model |
| Ctrl+X r | Change the thinking level |
| Ctrl+X l | Change the answer length |
| Ctrl+X q | Turn fast mode on or off for OpenAI Codex models |
| Ctrl+X x | Show or hide the thinking summary for Claude models |

To turn the thinking summary on or off for Claude models, press Ctrl+X x or click the thinking
label in the footer. You can also choose **Thinking Summary** in the command palette. With the
summary off, Claude still thinks, but Vis shows no thinking text. These controls appear only when
the current model supports them.

With **Simplified thinking modes** on, Ctrl+X r switches to the next of quick, balanced and deep.
With the setting off, Ctrl+X r opens a list of every thinking level that the current model offers.
The setting is in the top section of **Settings**. You can also click the reasoning label in the
footer.

### Files, voice and search

| Keys | What they do |
|---|---|
| Ctrl+X f | Search in the session |
| Ctrl+X a | Attach a file |
| Ctrl+X i | Review your attached files, the files this session produced and its apps. Enter opens the selected app |
| Ctrl+X I (Shift+I) | Show the pictures in this session one at a time. Only terminals that show pictures have this key. See [View pictures](#view-pictures) |
| Ctrl+X O (Shift+O) | Open the newest picture of this session in the picture app of your computer |
| Ctrl+X v | Start or stop a voice recording |
| Ctrl+X b | Turn voice conversation on or off: Vis reads each answer aloud and sends each recording once it is transcribed |
| Ctrl+X V (Shift+V) | Show the live view panel at the side, or move the focus to it. In the panel, ↑ and ↓ select a view, ← folds it, → unfolds it, Tab shows the next extension and Esc closes the panel |

## View pictures

Vis shows the pictures of a message in one table, like a list of attached files. Each row has a
small preview of the picture, its name, type and size, and an **Open externally** button. That
button opens the picture in your system's picture app. kitty, Ghostty, WezTerm, Warp and iTerm2 can
show the previews. Other terminals show the same table without previews.

To open the newest picture in your picture app, press **Ctrl+X Shift+O**. This key works in every
terminal. **Ctrl+X o** without Shift opens the providers.

To see one picture larger, click its preview. To see every picture in the session, press
**Ctrl+X Shift+I**. The picture viewer opens and fills most of the window. A terminal that cannot
show pictures has no picture viewer. There, use **Ctrl+X Shift+O** or the **Open externally**
buttons. **Ctrl+X i** without Shift lists the attachments of the session.

To see a detail, zoom in. Turn the mouse wheel over the picture to zoom at the pointer, or press
**+** and **-**. The header shows the zoom, for example `250%`. Then drag the picture with the mouse
or use the arrow keys to move it. Press **0** to see the whole picture again.

| Keys | What they do |
|---|---|
| ← or ↑ | Show the previous picture. On a zoomed picture, move it |
| → or ↓ | Show the next picture. On a zoomed picture, move it |
| PgUp or PgDn | Show the previous or the next picture |
| Home or End | Show the first or the last picture |
| 1 to 9 | Show the picture with that number |
| + or - | Zoom in or out |
| 0 | Show the whole picture |
| Enter or o | Open the picture in the image viewer of your computer |
| Esc or q | Close the picture viewer |

## Use an app

An app is a screen that the agent or an extension attaches to a session. Examples are a diff to
review, a form and a game. Click its row in the transcript, or press **Ctrl+X i**, select the app and
press Enter. You can press its buttons, fill in its fields and select its rows. In a review app, you
write comments on lines and in threads, save them as drafts and send them together.

| Keys | What they do |
|---|---|
| ↑ or ↓ | Select the previous or the next row. PgUp, PgDn, Home and End move further |
| ← or → | Fold or unfold the selected file or group |
| < or > | Scroll long lines to the side |
| Enter | Press the selected button, card or table row. On a field, edit its value. On a line, write a comment. On a file row, fold or unfold the file |
| Ctrl+S | In the comment editor, save the comment as a draft |
| s | Send every saved draft and the values of the fields |
| r | Load the app again |
| Esc | In the comment editor, cancel the comment. Else close the app |

### Keys of an app

An app can give its own keys, for example arrow keys in a game. The row above the status line shows
each key of the app and its label. When you press one of these keys, the status line shows the label
and then a check mark when the app has its answer.

An app key can use a key from the table above, for example **s** or an arrow. Then press **Ctrl+X**
first to use the key from the table: **Ctrl+X s** still sends the app. The status line shows `C-x s`
for each such key. A closed app does not use its own keys.

After Ctrl+S, the app shows `Saved · state N` when the gateway has saved your draft. After s, it
shows `Sent` and the id of the receipt. A closed or archived app is read only. There, Enter and s
show `Closed: read only`. [Live views](live-views.md#apps) explains apps for developers.

## Use voice input

Connect a microphone before you start the TUI. On macOS, allow microphone access for your terminal
or Vis when macOS asks.

Press **Ctrl+X v** to start recording. Press it again to transcribe the recording into your message.

Vis first tries Java Sound. If Java Sound cannot capture, macOS can use SoX or FFmpeg.
Vis checks PATH and both standard Homebrew locations: `/opt/homebrew/bin` and `/usr/local/bin`.

If neither recorder is installed, install one:

```bash
brew install sox
```

Use `brew install ffmpeg` if you prefer FFmpeg. Vis does not install these programs automatically.

On Linux, the fallback recorders are `pw-record` and `parec`.
WSL2 also requires a reachable WSLg audio server.

To check discovered audio devices and recorder paths, run:

```bash
vis-agent tui --check-audio
```

The check lists devices and executable paths. It does not test microphone permissions or capture audio.

If a recorder is installed but capture fails, check **System Settings > Sound > Input** on macOS.
If macOS reports denied access, allow it under **Privacy & Security > Microphone**.
Restart the TUI after connecting or selecting a different microphone.

## Cancel or quit

| Keys | What they do |
|---|---|
| Esc or Ctrl+G | Cancel the running turn, close a dialog or clear your draft |
| Ctrl+C | Clear your draft, cancel the running turn or quit Vis |

[Control a running session](sessions.md#control-a-running-session) explains which of these happens when,
and what happens to your queued messages.

## Move through a session

| Keys | What they do |
|---|---|
| Alt+>, Ctrl+X j, Ctrl+L or Ctrl+End | Jump to the latest message |
| Alt+< | Jump to the start of the session |
| Ctrl+V or Page Down | Scroll down one screen |
| Alt+V or Page Up | Scroll up one screen |
| Ctrl+X Tab or Ctrl+X Shift+Tab | Fold or unfold every foldable block, such as thinking and tool calls |
| Ctrl+X z | Label every fold, then press a label's letter to fold or unfold it |

You can also click **↓ messages** when it appears to jump to the latest message.

On macOS, Alt is the Option key. Many macOS terminals need a setting before Option
works as Alt, such as **Use Option as Meta key** in Terminal.

## Move between areas

When **Projects** or the side panel is open, the screen has more than one area. Only one area
gets your keys. Vis shows the other areas with faded colors.

Press **Ctrl+O** to go to the next area: Projects, then the chat, then the side panel. After the
last area, Vis goes back to the first one. When only the chat is open, Ctrl+O does nothing.

## In Projects

Press **Ctrl+X w** to open **Projects**. While it has focus, these keys act on the selected row.
Press **?** to see them in Vis.

| Keys | What they do |
|---|---|
| ↑ or ↓, Ctrl+P or Ctrl+N | Move between rows |
| ← or → | Fold or unfold the row |
| Enter | Open the row |
| g | Open the row menu |
| n | Start a session here |
| G (Shift+G) | Create a group |
| a | Add a project |
| + | Add a group, session or project here |
| Space | Select or clear a session |
| m or u | Move sessions to a group, or take them out of it |
| f | Star or unstar a session |
| R (Shift+R) | Rename the row |
| A (Shift+A) | Archive or restore the row |
| v | Show or hide archived rows |
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
| Alt+← or Alt+→ | Move one word back or forward, where your terminal supports it |
| ↑ or ↓ | Move the cursor up or down, or step through earlier messages |
| Ctrl+T | Swap two characters next to the cursor |
| Ctrl+K | Delete to the end of the line |
| Ctrl+U | Delete to the start of the line |
| Ctrl+W | Delete the word before the cursor |
| Ctrl+D | Delete the character after the cursor |

To copy and paste, use your terminal: select text to copy it, then paste with your
terminal's paste key.

## In the desktop or phone app

Use these keys in the message box of the Companion app, on a computer or with a keyboard on a
tablet.

| Keys | What they do |
|---|---|
| Enter | Send the message |
| Shift+Enter | Start a new line |
| ↑ | Show your previous message in this session. Press again to go further back |
| ↓ | Go forward to a newer message, and then back to the text that you typed |

↑ and ↓ show an earlier message only when the cursor is on the first or the last line. In a
message with more lines, they first move the cursor. The app keeps your 20 newest messages. It
does not keep commands such as `/reload`, but it keeps `/goal`. Messages from other sessions
through Council are not included. On a phone with no keyboard, Return starts a new line, and the
send button sends the message.

To see a picture larger in the app, click or tap it. If the message has more pictures, use these
keys in the picture viewer. On a phone, swipe left or right instead.

| Keys | What they do |
|---|---|
| ← or → | Show the previous or the next picture |
| Esc | Close the picture viewer |

## See also

- [Sessions](sessions.md) — what Enter, Esc and Ctrl+C do while Vis works, and how to find, fork and organize sessions with the keys on this page.
- [Getting started](index.md#in-the-terminal) — start Vis in your terminal and send a first task.
- [Reporting a bug](reporting-bugs.md) — report a shortcut that does not work in your terminal, with details that let someone reproduce it.
