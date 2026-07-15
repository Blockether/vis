"""Todo — model-facing tools + a /todos slash command over durable state.

Demonstrates:
  * tools with durable state (`vis.state` — a database-backed store
    that survives /reload and process restarts)
  * a user-facing slash command reading the same state
  * a prompt fragment teaching the model the surface

The model tracks multi-step work in a list the user can inspect with
/todos at any time.
"""

import vis


def _todos():
    return vis.state.get("todos", [])


def _save(todos):
    vis.state["todos"] = todos


def todo_add(text):
    """await todo_add(text) -> {"id", "text", "done"} — add one todo."""
    todos = _todos()
    next_id = max((t["id"] for t in todos), default=0) + 1
    todo = {"id": next_id, "text": str(text), "done": False}
    _save(todos + [todo])
    return todo


def todo_toggle(id):
    """await todo_toggle(id) -> {"id", "done"} — flip one todo's done flag."""
    todos = _todos()
    for t in todos:
        if t["id"] == id:
            t["done"] = not t["done"]
            _save(todos)
            return {"id": id, "done": t["done"]}
    # raising = failure envelope; the message surfaces to the model
    raise ValueError(f"no todo with id {id}; call todo_list() to see ids")


def todo_list():
    """await todo_list() -> {"todos": [...], "open": N, "done": N}."""
    todos = _todos()
    done = sum(1 for t in todos if t["done"])
    return {"todos": todos, "open": len(todos) - done, "done": done}


def todo_clear():
    """await todo_clear() -> {"cleared": N} — drop every todo."""
    n = len(_todos())
    _save([])
    return {"cleared": n}


def _slash_todos(ctx):
    todos = _todos()
    if not todos:
        return vis.ok("No todos yet — ask the agent to add some.")
    lines = [
        ("[x] " if t["done"] else "[ ] ") + f'#{t["id"]} {t["text"]}'
        for t in todos
    ]
    done = sum(1 for t in todos if t["done"])
    return vis.ok(
        f"{done}/{len(todos)} completed",
        body="\n".join(lines),
        data={"todos": todos},
    )


PROMPT = """todo_ surface active — a durable todo list the user can see with /todos.
  todo_add(text)   todo_toggle(id)   todo_list()   todo_clear()
Track multi-step work here instead of prose plans; keep entries short."""


vis.extension(
    name="todo",
    description="Durable todo list: model tools + /todos for the user.",
    version="0.1.0",
    kind="integration",
    alias="todo",
    symbols=[
        # sandbox name = f"{alias}_{name}"; name defaults to fn.__name__
        # with a leading "<alias>_" stripped — so todo_add binds as todo_add.
        vis.symbol(todo_add, tag="mutation"),
        vis.symbol(todo_toggle, tag="mutation"),
        vis.symbol(todo_list, tag="observation"),
        vis.symbol(todo_clear, tag="mutation"),
    ],
    prompt=PROMPT,  # constant string, or a callable (env) -> str | None
    slash_commands=[
        vis.slash("todos", _slash_todos, doc="Show the todo list."),
    ],
)
