# Sessions

Vis saves each conversation as a session, so you can close it and continue later. This page shows
how to control a session while Vis works, and how to find, fork, organize, export, and move saved
sessions.

## When to use

- **You want to talk without pressing a button for each request.** Try [hands-free voice](#use-hands-free-voice).
- **You think of a follow-up while Vis is still working.** Send it now. It
  [waits in the queue](#queue-a-message).
- **Vis must read your correction before the turn ends.**
  [Send the queued message now](#send-a-queued-message-now).
- **Vis is going in the wrong direction and you want to stop it.**
  [Cancel the turn](#cancel-a-turn).
- **You want to stop one operation without stopping the turn.**
  [Find and cancel a Task](#find-and-cancel-a-task).
- **You want to leave the session.** [Quit](#quit) lists what **Ctrl+C** does in each state.
- **The session must work in another directory.**
  [Change the directory of the session](#change-the-directory-of-a-session).
- **You need a conversation from last week.** [Find the session](#find-a-saved-session).
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
  [Export the session](#export-a-session).
- **You want to continue a conversation on another computer.**
  [Move the session](#move-a-session-to-another-computer).
- **A bug report needs the conversation that shows the problem.** Export it, then remove private
  details as [Reporting a bug](reporting-bugs.md#sharing-a-transcript) describes.
- **You want another session to help or to review the work.** Read [Council](council.md).

## Use hands-free voice

Hands-free voice is an experimental, English-only mode. It listens while answers play and submits each finished utterance automatically.
While a voice task runs, a spoken follow-up reaches that task at its next step.
Other requests use the normal session queue. Partial transcripts never start work.
Vis waits for 1,100 milliseconds of silence before it ends an utterance.

### Start a conversation

1. Open the session in the Companion in a desktop browser.
2. Choose **Start hands-free voice** and allow microphone access.
3. Wait for **Listening**, then speak your request.

The desktop browser requires the microphone track to report echo cancellation as enabled.
If that setting is false or unknown, capture stops. Use dictation or text instead.
This check does not establish acoustic quality.
The TUI has no hands-free voice. Use dictation or text there.

Only one live voice session can run on the gateway. The gateway needs the local VibeVoice streaming ASR and Pocket speech models.
If local speech is unavailable, use text or dictation. This mode does not switch to cloud speech or operating-system speech.

### Test voice on a phone

The native iOS app and Android 12 or newer include an unverified mobile mode. Mobile browsers do not support this mode.
Each start requires explicit consent to test. Simulator checks do not establish physical microphone, speaker or echo performance.

1. Keep the native Companion app open and set a low speaker volume.
2. Select **Enable mobile audio testing**, then choose **Start mobile voice test**.
3. Allow microphone access and wait for **Listening**.

The iOS app uses system voice processing. Android uses its communication audio path and an enabled acoustic echo canceller.
If the device cannot enable echo cancellation, voice stops. It does not switch to an unprocessed microphone.
Recognition and synthesis still run on the gateway, not through phone speech services.

Before regular use, check these behaviors on each device and audio route:

- Check that speaker playback does not submit another request through the microphone.
- Speak during playback and check that interruption works without losing your request.
- Mute the microphone and check that Vis hears nothing while new answers can still play.
  In the iPhone app, the system microphone indicator stays on until you end voice mode.
- Change the audio route or leave the app and check that voice stops.

Changing microphone state can interrupt current speech because the app rebuilds its audio path.
Voice stops on backgrounding, screen lock, audio interruption or route changes. Return to the app and start another test explicitly.
Bluetooth and speakerphone behavior still require human verification. Enabling testing does not mark a device as qualified.

### Prepare local models

Review the model and voice terms before downloading. These commands prepare assets on your configured gateway, not necessarily the client computer.
The downloads need about 2.84 GB, plus runtime files and working space.

```bash
vis-agent speech models licenses
vis-agent speech models download --vibevoice-asr --pocket-tts
vis-agent speech models status
```

The Pocket download includes its reference clips. Live readiness checks require both models, those clips and the native speech runtime.
Starting hands-free voice does not download missing assets. The general status command does not prove live readiness or acoustic quality.

### Model and voice terms

Code, weights and voice recordings have separate terms. Keep attribution when redistributing modified weights or clips.

| Component | Terms and source |
|---|---|
| VibeVoice ASR weights | [Pinned model card](https://huggingface.co/microsoft/VibeVoice-ASR-Streaming-1.5B/blob/4262d23d8a539a6530cf64fbd0b1751ef9a30853/README.md): MIT. |
| Pocket weights | [Kyutai Pocket TTS](https://huggingface.co/kyutai/pocket-tts): CC BY 4.0. Vis exports checkpoint `b6369a24` itself. |
| Reference clips | [Pinned voice catalogue](https://huggingface.co/kyutai/tts-voices/blob/323332d33f997de8394f24a193e1a76df720e01a/README.md): Alba and VCTK clips are CC BY 4.0. Marius and Javert are CC0. |
| Model and runtime code | Pocket, VibeVoice and ONNX Runtime use MIT. sherpa-onnx uses Apache 2.0. These code licenses do not replace weight or voice terms. |

The [VibeVoice guide](https://github.com/microsoft/VibeVoice#-risks-and-limitations) recommends further testing before commercial or real-world use.
Its research-use warning is separate from the model card's MIT label. This pilot is not production qualification.

Pocket's upstream access notice prohibits unlawful or deceptive use and voice cloning without explicit lawful consent.
Its raw model card requires authenticated access. The published Vis asset pack needs no Hugging Face login, but does not remove upstream notices.
The shipped clips exclude the noncommercial EARS and Expresso voices. Imported recordings need their own rights review.

### Read and hear the answer

Conversation appears on screen and is spoken from the same text. Diagrams, code, links and documents remain visible without being read aloud.
You can ask for a visual explanation while continuing the conversation.

Each completed reply needs a conversational result or question. Earlier progress does not replace that outcome.
If you explicitly request a silent reply, Vis can show the result without speech.

### Control capture, speech and tasks

| Action | Companion | Effect |
|---|---|---|
| Mute input | **Mute microphone** | Stop capture. Speech from before the mute still becomes a request. |
| Resume input | **Unmute microphone** | Start capture again. |
| Stop output | **Stop speech** | Stop old speech, not the agent task. |
| End conversation | **End voice mode** | Close voice devices, not agent tasks. |

A muted microphone cannot hear an unmute command. Use the interface to resume.
In the Companion, **Mute speech** controls output separately. **Cancel voice task** uses normal task cancellation.

Say “Stop speaking” to stop output, or “End voice mode” to close voice mode without cancelling tasks.
Cancellation does not undo completed actions.

**Send now** delivers queued messages into the running task at its next step. It does not start a separate task.
When a voice follow-up reaches that task, new replies can speak again. Interrupted speech does not replay.
Tasks started outside voice mode can still answer as text.

A misrecognized control can become an ordinary request.
If speech recognition changes a command, use the interface controls.
Synthetic tests missed some “End voice mode” commands.

Existing approvals still control consequential actions. An interruption does not grant approval or cancel tools.
When Vis needs approval or an answer, use the authenticated form in the interface. Voice does not answer these forms.

### Recover from a voice failure

If speech is unavailable, check that the gateway has both local models and Pocket voice assets.
If another voice session is active, end it before starting a new one.
If the connection or audio device fails, return to text and check the task history before repeating a request.
A request can continue after its voice connection ends. Do not assume that silence means the task failed.

If the response format is invalid, Vis tries one formatting repair without repeating completed tools.
If that repair fails, a visible notice and a short spoken notice report the presentation failure.
Completed work keeps its task status. Check the visible results before repeating the request.

If speech input falls behind, capture becomes muted. Unfinished utterances receive a rejection receipt and do not start tasks.
Already admitted tasks continue. When you are ready, unmute and repeat only the rejected request.
The session keeps queue counts and processing times for diagnosis, without recording audio.

Confirmed admission receipts survive loss of the gateway's live voice state. Recovery does not restore audio or repeat requests.
Older receipts can exist while the latest admission remains unknown.
A missing receipt does not prove failure. Check ordinary session history before repeating a request.

### Privacy and experimental limits

Speech recognition and synthesis stay on the gateway. A hosted agent can still receive admitted text.
Raw PCM is transient. Admitted text, speech metadata and playback receipts remain in session history.
Playback positions estimate rendered audio conservatively. They do not prove that you heard it.

Keep the client visible and active. Native mobile audio remains test-only. Background and locked-screen capture are not supported.
Stopping speech clears queued output. The model can continue work on the current text chunk before it stops. This can delay the next spoken answer.

Speaker echo, simultaneous speech, headsets and Bluetooth still need physical testing. Automated tests do not establish acoustic quality or latency percentiles.
The current ASR needs an initial 3.467-second window, then advances in 2.933-second steps.
There is no two-second response or 200-millisecond interruption guarantee.

To stop, end voice mode. Existing dictation, manual replay and text remain available.
For lifecycle calls and admission recovery, read [Live voice sessions](sessions-api.md#live-voice-sessions).

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

## Keep temporary files separate

Each session has a scratch directory in the Vis state directory. Shell commands and sandbox Python get its path in `TMPDIR` and `VIS_SESSION_TMPDIR`. An extension can read these variables with `vis.call_env()` and pass them to a process it starts. For Java, pass `-Djava.io.tmpdir` with that path.

Move files that you want to keep into your project. Vis removes scratch files when you delete the session and its managed processes stop. A cleanup check also removes scratch when the session had no turn in the past 7 days and nothing claims it. Open environments, managed shells and managed REPLs hold claims. Detached processes that Vis does not track do not hold claims.

Vis warns the agent when the directory exceeds 10 GiB. The warning does not remove files.

Before cleanup, remove a temporary Git worktree with `git worktree remove`. This command also removes its record from the Git repository.

## Control a running session

### Follow progress

Vis adds short notes while it works. Under each note, one digest row sums up the steps after the
note, for example `1 mutation · 3 observations`. The row counts the calls by kind, and it shows
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

In the desktop or phone app, a large turn first shows only its newest steps. The app
loads the older steps when you scroll near them. If the load fails, the app shows the error and a **Try again** button.

### Find and cancel a Task

A Task is one operation, such as a shell command or a tool call. **Background** shows the running
Tasks for the open session. Cancel a Task to request a stop of that operation, not the whole turn.

- In the terminal, find **Background** above the message box. Click the Task name to select the
  next Task, or use the [Background keys](keyboard-shortcuts.md#use-background-in-the-terminal).
  Select **View** if the Task has a live view. Select **Cancel** to request a stop.
- In the desktop or phone app, find **Background** above the message box. Select the title of a
  live view to open it. Select **Cancel** next to the Task you want to stop.

**Cancelling…** means that the Task has not stopped yet. The Task leaves **Background** when it ends.
Cancellation does not undo changes that the Task already made.

### Queue a message

Press **Enter** to send. If no turn runs, the message starts one. If a turn runs, the message goes
to the queue below the progress display.

Queued messages run in the order that you sent them. After a failed turn, the queue pauses until
you resume it. The queue stays when the gateway restarts.

To edit a queued message:

- In the terminal, press **↑** to move the newest queued message back into the composer.
- In the desktop or phone app, choose the message to edit it, or choose **×** to remove it.

### Send a queued message now

Send a queued message now when the running turn must read it first.

- In the terminal, press **Ctrl+X q s** to send the whole queue. To send one message, click
  **Send it now** at the end of its row, or press **Ctrl+X q** and its row number.
- In the desktop or phone app, choose **Send all now** in the queue header. To send one message,
  choose **Send it now** on its row.

A sent message shows **next iter** on its row, and its button changes to **Unsend**. **Unsend**
makes the message wait for the end of the turn again.

The running turn reads the sent messages, in queue order, when it starts its next step. The
conversation then shows each one as a normal **You** message under a
**Queued · sent now · iter N** header. N is the iteration that the message reached.

Images attached to a sent message go to the running turn with it. The desktop and phone apps show
them, also after a reload.

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

The terminal switcher and the search dialog of the apps have two sides. The left side lists the
recent sessions or the sessions that match. The right side shows messages of the selected session.
On a phone or in a narrow app window, the messages are below the list.

Before you type, and when no message matches, you see the newest messages. When messages match,
you see them with your search words highlighted.

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

A fork does not copy or restore your project files, so a fork from an earlier turn does not undo
the changes of later turns.

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

A session that you star in the terminal is also starred in the apps.
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

```bash
vis-agent sessions export <SESSION-ID> [--md | --atif | --html PATH | --snapshot PATH]
```

### Markdown

Markdown is the default format. The command prints the transcript to stdout:

```bash
vis-agent sessions export 3a7b2c1d > session.md
```

### ATIF JSON

Use `--atif` to read a session in another program. The command prints one ATIF v1.8 JSON document
with the turns, tool calls, token counts, and subagent conversations:

```bash
vis-agent sessions export 3a7b2c1d --atif > session.json
```

The export lists attachment names, media types, and sizes. It does not include file contents or
attachments for the model only. Vis replaces known credential values and cuts long text after
20,000 characters. Known values are provider keys and tokens, the gateway token, secret settings,
and credentials in the environment, the project `.env` file, and `environment:`.

Vis also replaces credentials that their form shows: private keys, passwords in URLs,
`Authorization` and `Cookie` header values, and well-known API key and token formats. Base64 text
that holds one of these credentials also becomes `[REDACTED]`. Review the JSON for other private
data before you share it.

To read an ATIF trajectory in Vis, import the JSON file:

```bash
vis-agent sessions import session.json
```

Vis creates a new session with the conversation and its subagent conversations. The import
keeps tool call records but does not run them. Use a snapshot to move attachments and saved
session state.

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

A snapshot holds the full conversation, also tool output and attached files. Vis replaces known
credential values with `[REDACTED]` in the text, the tool records, and text files. Known values
are provider keys and tokens, the gateway token, secret settings, and credentials in the
environment, the project `.env` file, and `environment:`. Vis also replaces private keys,
passwords in URLs, credential header values, and well-known API key and token formats. Base64
text that holds one of these credentials also becomes `[REDACTED]`.

The values of secret settings also become `[REDACTED]`: after an import, set them again. Binary
files, such as images, stay unchanged. Review a snapshot for other private data before you share
it.

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
  in the session list. Choose **Import snapshot**, then choose the file. Vis opens the new
  session. To put the new session in a group, use the actions of that group.
- In the terminal, press **Ctrl+X s i** and type the path of the file. If the project has groups,
  choose its sessions or one group. Vis opens the new session. In **Projects**, press **i** on a
  row to import into the project or the group of that row.

The imported session uses the folder where you run the command. These options change that:

| Option | Effect |
|---|---|
| `--root DIR` | Use `DIR` as the project folder. |
| `--group ID` | Put the session in this session group. Without `--root`, use the project folder of the group. |
| `--key KEY` | Import only once. A second import with the same key prints the first session. |

The permissions on the new computer come from its own policy, not from the snapshot. The
**Session snapshot limit** setting sets the largest snapshot, and its default is 1 GiB. A program
can do the same steps with the
[Sessions API](sessions-api.md#save-and-restore-a-session-snapshot).

## See also

- For every terminal key, read [Keyboard shortcuts](keyboard-shortcuts.md).
- To follow and control the same session from another device, read
  [Desktop and mobile setup](quickstart.md#connect-an-app).
- To ask another session for help or a second review, read [Council](council.md).
- To remove private information before you share an export, read
  [Reporting a bug](reporting-bugs.md).
- For the slash commands and shell shortcuts that you can queue, read
  [Project instructions](project-instructions.md).
- To create, follow, and manage sessions from your own program, read
  [Sessions API](sessions-api.md).
