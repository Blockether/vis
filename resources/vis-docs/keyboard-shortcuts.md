# Keyboard shortcuts

Use these keyboard shortcuts, also called keybindings, in the Vis terminal app. With them you can
send a message, start a new line, run commands, move through a session and edit your text.

To see the most common shortcuts while you work, press **Ctrl+X h** in Vis.

## When to use

- **Shift+Enter sends your message instead of starting a new line.** Your terminal
  needs a setting, or you need another key. See [New lines in your
  terminal](#new-lines-in-your-terminal).
- **You want a command but do not know its key.** Press **Ctrl+X p** and type part of
  its name, or look it up in [Run a command](#run-a-command).
- **You want to read an earlier part of a long session.** Use the keys in [Move
  through a session](#move-through-a-session) to scroll, jump and fold.
- **You want to fork a session, go back to an earlier turn or move a session to a
  group.** Use the keys under [Sessions](#sessions). The [Sessions guide](sessions.md)
  explains each task.
- **You want to dictate a message, or voice recording fails.** See [Use voice input](#use-voice-input).
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

### Sessions

| Keys | What they do |
|---|---|
| Ctrl+X n | Start a new session |
| Ctrl+X s | Switch to another session |
| Ctrl+X Delete or Ctrl+X Backspace | Delete this session permanently. Vis asks you to confirm first |
| Ctrl+X w | Open **Projects**, the list of saved sessions |
| Ctrl+X y | Fork this session: open a new session with a copy of the whole conversation |
| Ctrl+X t | Fork from an earlier turn: choose the last turn the new session keeps |
| Ctrl+X d | Move this session to a group |
| Ctrl+X u | Show session metrics: context health, totals and cache |
| Ctrl+X k | Send the queued messages now: the running turn reads them at its next step |
| Ctrl+X 1 to 9 | Send that queued message now, or press again to keep it for the turn end |

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
| Ctrl+X i | Review your attached files and the files this session produced |
| Ctrl+X v | Start or stop a voice recording |
| Ctrl+X b | Turn voice conversation on or off: Vis reads each answer aloud and sends each recording once it is transcribed |

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

## See also

- [Sessions](sessions.md) — what Enter, Esc and Ctrl+C do while Vis works, and how to find, fork and organize sessions with the keys on this page.
- [Getting started](index.md#in-the-terminal) — start Vis in your terminal and send a first task.
- [Reporting a bug](reporting-bugs.md) — report a shortcut that does not work in your terminal, with details that let someone reproduce it.
