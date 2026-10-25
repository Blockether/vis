/**
 * The Swarm Forge board, as `.vis/extensions/swarm_forge.py` paints it after a scripted
 * two-pack run with four tasks. One task landed, one waits for approval, one has a
 * question and one waits for the coder.
 */
export const SWARM_FORGE_BOARD: Record<string, unknown> = {
  "id": "swarm-board",
  "title": "Swarm Forge",
  "extension": "swarm_forge",
  "placement": "panel",
  "nodes": [
    {
      "stats": [
        {
          "id": "open",
          "value_text": "3",
          "label": "Open",
          "tone": "idle"
        },
        {
          "id": "working",
          "value_text": "0",
          "label": "Working",
          "tone": "idle"
        },
        {
          "id": "attention",
          "value_text": "2",
          "label": "Need you",
          "tone": "warn"
        },
        {
          "id": "landed",
          "value_text": "1",
          "label": "Landed",
          "tone": "ok"
        }
      ],
      "type": "stat",
      "id": "summary"
    },
    {
      "type": "group",
      "direction": "column",
      "id": "attention",
      "children": [
        {
          "type": "group",
          "direction": "column",
          "id": "att:J5",
          "children": [
            {
              "text": "Updated call sites and docs",
              "tone": "warn",
              "meta": "cde3456",
              "title": "T2 Rename greet() to hello()",
              "type": "card",
              "id": "att-card:J5"
            },
            {
              "type": "group",
              "direction": "row",
              "id": "att-actions:J5",
              "children": [
                {
                  "label": "Approve",
                  "is_disabled": false,
                  "type": "button",
                  "id": "approve:J5",
                  "clicks": 0
                },
                {
                  "label": "Drop",
                  "is_disabled": false,
                  "type": "button",
                  "id": "drop:J5",
                  "clicks": 0
                }
              ]
            },
            {
              "type": "group",
              "direction": "column",
              "id": "att-reject:J5",
              "children": [
                {
                  "label": "Comment",
                  "type": "multiline",
                  "name": "comment:J5",
                  "id": "comment:J5",
                  "is_required": false,
                  "is_secret": false
                },
                {
                  "action": "submit",
                  "label": "Reject",
                  "is_disabled": false,
                  "type": "button",
                  "id": "reject:J5",
                  "clicks": 0
                }
              ]
            }
          ]
        },
        {
          "type": "group",
          "direction": "column",
          "id": "att:J6",
          "children": [
            {
              "text": "Which languages should the greeting support?",
              "tone": "warn",
              "meta": "Coder",
              "title": "T3 Translate the greeting",
              "type": "card",
              "id": "att-card:J6"
            },
            {
              "label": "Answer",
              "is_required": true,
              "type": "multiline",
              "name": "answer:J6",
              "id": "answer:J6",
              "is_secret": false
            },
            {
              "action": "submit",
              "label": "Send answer",
              "is_disabled": false,
              "type": "button",
              "id": "reply:J6",
              "clicks": 0
            },
            {
              "label": "Drop",
              "is_disabled": false,
              "type": "button",
              "id": "drop:J6",
              "clicks": 0
            }
          ]
        }
      ],
      "label": "Attention"
    },
    {
      "type": "group",
      "direction": "row",
      "id": "lanes",
      "children": [
        {
          "type": "group",
          "direction": "column",
          "id": "lane:coder",
          "children": [
            {
              "text": "Has a question",
              "tone": "warn",
              "title": "T3 Translate the greeting",
              "type": "card",
              "id": "card:coder:T3"
            },
            {
              "text": "Waiting",
              "tone": "idle",
              "title": "T4 Add a --quiet flag",
              "type": "card",
              "id": "card:coder:T4"
            }
          ],
          "label": "Coder"
        },
        {
          "type": "group",
          "direction": "column",
          "id": "lane:cleaner",
          "children": [
            {
              "text": "Landed",
              "tone": "ok",
              "meta": "0aa1234",
              "title": "T1 Add farewell()",
              "type": "card",
              "id": "card:cleaner:T1"
            },
            {
              "text": "Needs approval",
              "tone": "warn",
              "meta": "cde3456",
              "title": "T2 Rename greet() to hello()",
              "type": "card",
              "id": "card:cleaner:T2"
            }
          ],
          "label": "Cleaner"
        }
      ],
      "label": "Lanes"
    },
    {
      "rows": [
        {
          "id": "T1",
          "tone": "ok",
          "cells": [
            "T1 Add farewell()",
            "Landed"
          ]
        },
        {
          "id": "T2",
          "tone": "warn",
          "cells": [
            "T2 Rename greet() to hello()",
            "Needs approval"
          ]
        },
        {
          "id": "T3",
          "tone": "warn",
          "cells": [
            "T3 Translate the greeting",
            "Has a question"
          ]
        },
        {
          "id": "T4",
          "tone": "idle",
          "cells": [
            "T4 Add a --quiet flag",
            "Waiting"
          ]
        }
      ],
      "label": "Work queue",
      "columns": [
        {
          "id": "task",
          "label": "Task"
        },
        {
          "id": "state",
          "label": "State"
        }
      ],
      "type": "table",
      "id": "queue",
      "max_rows": 5000,
      "order": "insertion"
    },
    {
      "type": "group",
      "direction": "column",
      "id": "new-task",
      "children": [
        {
          "label": "Title",
          "is_required": true,
          "type": "plaintext",
          "name": "task-title",
          "id": "task-title",
          "is_secret": false
        },
        {
          "label": "Details",
          "type": "multiline",
          "name": "task-text",
          "id": "task-text",
          "is_required": false,
          "is_secret": false
        },
        {
          "action": "submit",
          "label": "Add task",
          "is_disabled": false,
          "type": "button",
          "id": "add-task",
          "clicks": 4
        }
      ],
      "label": "New task"
    },
    {
      "type": "group",
      "direction": "column",
      "id": "roles",
      "children": [
        {
          "type": "group",
          "direction": "row",
          "id": "role:coder",
          "children": [
            {
              "label": "Provider",
              "value": "default",
              "options": [
                {
                  "value": "default",
                  "label": "Session default"
                },
                {
                  "value": "cheap"
                }
              ],
              "type": "select",
              "name": "provider:coder",
              "id": "provider:coder",
              "is_required": false,
              "is_secret": false
            },
            {
              "label": "Model",
              "value": "default",
              "options": [
                {
                  "value": "default",
                  "label": "Session default"
                }
              ],
              "type": "select",
              "name": "model:coder",
              "id": "model:coder",
              "is_required": false,
              "is_secret": false
            }
          ],
          "label": "Coder"
        },
        {
          "type": "group",
          "direction": "row",
          "id": "role:cleaner",
          "children": [
            {
              "label": "Provider",
              "value": "default",
              "options": [
                {
                  "value": "default",
                  "label": "Session default"
                },
                {
                  "value": "cheap"
                }
              ],
              "type": "select",
              "name": "provider:cleaner",
              "id": "provider:cleaner",
              "is_required": false,
              "is_secret": false
            },
            {
              "label": "Model",
              "value": "default",
              "options": [
                {
                  "value": "default",
                  "label": "Session default"
                }
              ],
              "type": "select",
              "name": "model:cleaner",
              "id": "model:cleaner",
              "is_required": false,
              "is_secret": false
            }
          ],
          "label": "Cleaner"
        }
      ],
      "label": "Roles"
    },
    {
      "type": "group",
      "direction": "column",
      "id": "end",
      "children": [
        {
          "label": "Teardown",
          "is_disabled": false,
          "type": "button",
          "id": "teardown",
          "clicks": 0
        }
      ]
    }
  ]
};
