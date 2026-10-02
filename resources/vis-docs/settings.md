# Settings

Settings is where you change how Vis works for a gateway, a project, a group or one session.
You change values in a draft, review the draft and then apply all changes together. This page
shows how to do this in the app and in the terminal UI.

## When to use

- **You want to change several settings and save them together.** Follow
  [Edit, review and apply](#edit-review-and-apply).
- **A change has no effect, or a row shows a warning.** Read
  [Inherited values and overrides](#inherited-values-and-overrides).
- **Settings tells you that another client changed the values.** Read
  [When another client changes settings](#when-another-client-changes-settings).
- **A session needs access to more folders or hosts.** Use the
  [access editors](#workspace-paths-and-network-access).
- **You want the same settings in a different project or on a different machine.** Use
  [profiles](#save-and-share-profiles).
- **You want to add a provider, choose models or edit YAML files.** Read
  [Configuration](configuration.md) instead.

## Open Settings

Each setting has a scope: **global**, **project**, **group** or **session**. Global settings
apply to all clients of one gateway. The other scopes change only the work inside them.

### In the app

| Goal | Steps |
| --- | --- |
| Change global settings | Open **Settings** and select a machine under **Machines**. |
| Change app preferences, such as the theme | Open **Settings** and select **This device**. |
| Change one session | Open the session **…** menu and select **Settings**. |
| Change a project or a group | Open the actions of the project or group and select **Settings**. |

On a wide screen, the pages of Settings show in a column at the side. On a narrow screen,
select the page from a list at the top.

### In the terminal UI

Press **Ctrl+X p** to open the command palette. Then type `settings` and select one of these
commands:

- **Settings** opens global settings, providers, MCP servers and terminal preferences.
- **Session settings** opens the settings of the current session.
- **Group settings** and **Project settings** open the settings of the group or the project of
  the current session.

In Settings, press **F6** to change to a different scope.

## Find a setting

Type in the search field to find a setting by its name, ID or description. Search also finds
skills and MCP servers. Select a category to show only that part of the catalog.

Select **Set here only** to show only the settings that the open scope sets. Each row shows
**Set here** or the scope that it inherits from. Each row also tells you when a change applies.
Most settings apply on the next turn. Settings for skills, MCP servers and tool extensions apply
on the next call.

## Edit, review and apply

When you change a value, Vis adds the change to a draft. Nothing is saved until you apply the
draft. The status line shows how many changes are not yet applied.

1. Change one or more values.
2. Examine the old and the new value of each change. In the app, open the list of changes, for
   example **Review 2 changes**. In the terminal UI, press **F2**.
3. In the app, if the draft changes access, select the check box that confirms your review.
4. Select **Apply changes** in the app, or confirm the review in the terminal UI.

The gateway saves all changes together, or it saves none of them. If one value is not
valid, the row shows the error and no setting changes. Correct the value and apply again.

To remove the draft, select **Discard changes**. If you close Settings with a draft, Vis asks
you to select **Keep editing** or **Discard and leave**.

Access changes apply on the next turn. A call that runs now keeps its current permissions.
Response options, such as reasoning effort, apply to the next message that you send.

### Keys in the terminal UI

| Key | Action |
| --- | --- |
| Enter | Edit the selected setting |
| F1 | Show details about the selected setting |
| F2 or Ctrl+S | Review and apply the draft |
| F3 | Discard the draft |
| F4 | Open profiles, import and export |
| F5 | Load the latest values and keep your draft |
| F6 | Change the scope |
| Esc | Close Settings |

## Inherited values and overrides

Vis reads each setting in this order: **global**, **project**, **group** and then **session**.
The most specific value wins. A scope without a value uses the value of the scope before it.

A value that you set in a scope is an override. Under an override, the row shows the value
that applies without it. Select **Use inherited value** to remove the override from the open
scope. Select **Set value here** to add an override with the current value. In the terminal UI,
press **F1** on the row and then press **i** to use the inherited value.

Sometimes a more specific scope decides a setting for the open session. The row then shows a
warning with that scope and its value. You can still edit the row. Your change applies to all
work that does not have a more specific value.

For example, your project turns off **Automatic provider fallback**. You turn it on in global
settings. Sessions in that project still do not change to a different provider. To change
these sessions, select **Open winning scope** in the app. In the terminal UI, press **F6** and
select the scope that the warning names.

## When another client changes settings

Each catalog has a revision. If another client applies changes after you open Settings, your
apply stops and nothing is saved. Vis keeps your draft.

1. In the app, select **Review latest and keep draft**. In the terminal UI, press **F5**.
2. Compare the latest values with your draft. In the app, open **Changed settings** to see
   what the other client changed.
3. Apply your draft again.

## Workspace paths and network access

Access settings have editors with separate fields. You do not have to write JSON for them.

- **Workspace roots** adds folders that the session can use. The terminal UI calls them
  **Workspace paths**. Give each folder a name and an absolute path or a `~/` path. The Python
  name and the description are optional.
- **Host rules** sets the hosts, methods and ports that a request can use. Add allowed
  requests with a method and an optional path.
- **Inbound ports** lists the ports where a server in the jail can accept connections.
- **Private network access** lets requests reach addresses in your private network.

To edit the full value, select **Advanced JSON**. Vis checks the JSON before it adds it to
the draft. Local settings cannot give more access than the global policy of the host. For the
rules that these settings control, read [Process jail and network policy](jail.md).

## Save and share profiles

A profile is a named set of setting changes. Use it to repeat the same settings in a
different project, group or session. Profiles stay on this device in the app, and in your
terminal settings in the terminal UI.

Open **Profiles, import and export** in the app. In the terminal UI, press **F4**.

| Task | What happens |
| --- | --- |
| Save a profile | Vis saves the overrides of the open scope with the name that you enter. |
| Export a profile | Vis shows the overrides of the open scope as JSON that you can copy. |
| Load or import a profile | Vis adds the changes to your draft. |
| Use inherited defaults | Vis adds a change that removes each override of the open scope. |
| Remove a saved profile | Vis removes the profile from this device. Gateway settings do not change. |

Apply or discard your draft before you save or export a profile. A loaded or imported
profile changes only the settings that it includes. Review the draft and apply it as usual.

Profiles contain configuration, not provider tokens, passwords or MCP credentials. Paths and
domains can identify your workspace. Examine them before you share a profile.

A profile has a name of 1 to 64 characters and up to 256 changes. Its JSON can be up to
128 KiB. You can save up to 20 profiles. This example turns off refusal fallback and removes
the override for provider fallback:

```json
{
  "version": 1,
  "name": "Quick fixes",
  "changes": [
    {"id": "refusal_fallback", "action": "value", "value": false},
    {"id": "provider_fallback", "action": "inherit"}
  ]
}
```

## See also

- [Configuration](configuration.md) explains configuration files, providers and the settings
  HTTP API.
- [Sessions](sessions.md) shows how to put sessions into groups and projects.
- [Keyboard shortcuts](keyboard-shortcuts.md) lists the other keys of the terminal UI.
- [Process jail and network policy](jail.md) explains what access settings permit.
