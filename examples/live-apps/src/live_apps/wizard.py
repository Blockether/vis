"""Deploy wizard: four steps with every input type, checks and a live deploy run.

Next and Deploy are submit buttons in the group of their step, so the engine
collects the inputs of that step. The engine checks the required inputs and the
range. Then it asks the owner: the `validate=` functions of the inputs judge
each value before the reducer sees the submit. The reducer checks only the rule
that crosses steps (production needs 2 replicas). The token is a secret: the app
sees only a handle. After a valid Deploy, ticks move the deploy through its stages.
"""

from live_apps import ui

KEY = "wizard"
TITLE = "Deploy wizard"
DESCRIPTION = "Plan a deploy step by step, check it and watch it run."
TICK_SECONDS = 1.0
STEPS = (
    ("basics", "Basics"),
    ("targets", "Targets"),
    ("secrets", "Secrets"),
    ("review", "Review"),
)
ENVS = (("dev", "Development"), ("staging", "Staging"), ("prod", "Production"))
REGIONS = (("eu-west", "EU West"), ("us-east", "US East"), ("ap-south", "Asia South"))
STAGES = ("Build", "Upload", "Migrate", "Roll out", "Verify")
NAME_CHARS = set("abcdefghijklmnopqrstuvwxyz0123456789-")


def initial():
    return {
        "step": 0,
        "form": {
            "name": "",
            "env": "",
            "regions": [],
            "replicas": 2,
            "strategy": "rolling",
        },
        "token_set": False,
        "errors": {},
        "run": None,
        "runs": 0,
    }


def check_name(value):
    """The `validate=` function of the name input: None, or the problem."""
    name = str(value).strip()
    if not set(name) <= NAME_CHARS or name.startswith("-"):
        return "Use lowercase letters, digits and dashes."
    if len(name) > 40:
        return "Use at most 40 characters."
    return None


def check_confirm(value):
    """The `validate=` function of the confirm checkbox."""
    return None if value is True else "Confirm the deploy."


def check(step, form):
    """Field errors of one step; an empty dict when the step is valid.

    A submit reaches the reducer only after the engine and the `validate=`
    functions accept it. The `n` key presses Next as a plain press, so this
    check runs the same functions for that path.
    """
    errors = {}
    if step == 0:
        name = form["name"].strip()
        if not name:
            errors["name"] = "Give the service a name."
        elif check_name(name):
            errors["name"] = check_name(name)
        if form["env"] not in {e for e, _ in ENVS}:
            errors["env"] = "Choose an environment."
    if step == 1:
        if not form["regions"]:
            errors["regions"] = "Choose at least one region."
        replicas = form["replicas"]
        if not isinstance(replicas, (int, float)) or not 1 <= replicas <= 10:
            errors["replicas"] = "Use 1 to 10 replicas."
        elif form["env"] == "prod" and replicas < 2:
            errors["replicas"] = "Production needs at least 2 replicas."
    return errors


def _learn(state, event):
    form = state["form"]
    for key, value in (event.get("read") or {}).items():
        if key in form and value is not None:
            form[key] = value
    if event.get("type") == "submit":
        for key, value in (event.get("values") or {}).items():
            if key in form and value is not None:
                form[key] = value
    if event.get("type") == "change":
        key = event.get("node_id")
        if key == "token":
            state["token_set"] = bool(event.get("is_set"))
        elif key in form:
            form[key] = (
                event.get("value")
                if event.get("value") is not None
                else initial()["form"][key]
            )
    if isinstance(form["replicas"], float) and form["replicas"].is_integer():
        form["replicas"] = int(form["replicas"])


def reduce(state, event):
    if event.get("type") == "tick":
        run = state["run"]
        if run and run["stage"] < len(STAGES):
            run["stage"] += 1
            if run["stage"] == len(STAGES):
                run["status"] = "done"
        return state
    _learn(state, event)
    what = ui.intent(event)
    if event.get("type") == "submit" and event.get("node_id") == "next":
        what = "next"
    if what == "next":
        state["errors"] = check(state["step"], state["form"])
        if not state["errors"]:
            state["step"] = min(state["step"] + 1, len(STEPS) - 1)
    elif what == "back":
        state["errors"] = {}
        state["step"] = max(state["step"] - 1, 0)
    elif what == "restart":
        return dict(initial(), runs=state["runs"])
    elif event.get("type") == "submit" and event.get("node_id") == "deploy":
        # The engine checked the token (required) and the confirmation (its
        # validate= function). The form of the earlier steps can be stale after
        # Back, so check it again here.
        values = event.get("values") or {}
        errors = {}
        for step in range(len(STEPS) - 1):
            errors.update(check(step, state["form"]))
        state["errors"] = errors
        state["token_set"] = bool(values.get("token"))
        if not errors:
            state["runs"] += 1
            state["run"] = {"stage": 0, "status": "running", "number": state["runs"]}
            state["step"] = len(STEPS) - 1
    return state


def _error(state, name):
    return state["errors"].get(name)


def _input(state, kind, name, label, **spec):
    node = ui.field(kind, name, label, **spec)
    error = _error(state, name)
    if error:
        node["description"] = f"\u26a0 {error}"
    return node


def _step_nodes(state):
    form = state["form"]
    step = state["step"]
    if step == 0:
        return [
            _input(
                state,
                "plaintext",
                "name",
                "Service name",
                placeholder="payments-api",
                is_required=True,
                validate=check_name,
                value=form["name"] or None,
            ),
            _input(
                state,
                "select",
                "env",
                "Environment",
                options=[ui.option(v, label) for v, label in ENVS],
                is_required=True,
                value=form["env"] or None,
            ),
            ui.button("next", "Next", action="submit"),
        ]
    if step == 1:
        return [
            _input(
                state,
                "multiselect",
                "regions",
                "Regions",
                options=[ui.option(v, label) for v, label in REGIONS],
                is_required=True,
                value=form["regions"] or None,
            ),
            _input(
                state,
                "range",
                "replicas",
                "Replicas",
                min=1,
                max=10,
                step=1,
                value=form["replicas"],
            ),
            _input(
                state,
                "select",
                "strategy",
                "Strategy",
                options=[
                    ui.option("rolling", "Rolling"),
                    ui.option("blue-green", "Blue and green"),
                ],
                value=form["strategy"],
            ),
            ui.button("next", "Next", action="submit"),
        ]
    if step == 2:
        return [
            _input(
                state,
                "password",
                "token",
                "Deploy token",
                is_required=True,
                placeholder="Paste the token",
            ),
            _input(
                state,
                "checkbox",
                "confirm",
                f"Deploy {form['name'] or 'the service'} to {form['env'] or '?'}",
                validate=check_confirm,
                value=False,
            ),
            ui.button(
                "deploy",
                "Deploy",
                action="submit",
                is_disabled=state["run"] is not None
                and state["run"]["status"] == "running",
            ),
        ]
    return []


def _summary(state):
    form = state["form"]
    regions = ", ".join(form["regions"]) or "none"
    return (
        f"**{form['name'] or '?'}** to **{form['env'] or '?'}** in {regions}, "
        f"{form['replicas']} replica(s), {form['strategy']} strategy. "
        + (
            "The deploy token is set."
            if state["token_set"]
            else "The deploy token is still missing."
        )
    )


def render(state):
    step = state["step"]
    run = state["run"]
    plan = []
    for index, (step_id, label) in enumerate(STEPS):
        tone = "ok" if index < step else "running" if index == step else "idle"
        plan.append({"id": step_id, "label": label, "tone": tone})
    nodes = [
        ui.heading("title", "Deploy wizard"),
        ui.steps("plan", plan),
        ui.group(
            f"step-{STEPS[step][0]}",
            *(_step_nodes(state) or [ui.paragraph("summary", _summary(state))]),
            label=STEPS[step][1],
        ),
    ]
    if state["errors"]:
        nodes.append(
            ui.status(
                "check",
                f"{len(state['errors'])} problem(s) to fix",
                "error",
                detail="; ".join(state["errors"].values()),
            )
        )
    else:
        nodes.append(
            ui.status(
                "check",
                f"Step {step + 1} of {len(STEPS)}",
                "idle",
                detail=_summary(state),
            )
        )
    nodes.append(
        ui.wrap(
            "nav",
            ui.button("back", "Back", is_disabled=step == 0),
            ui.button("restart", "Start again"),
        )
    )
    if run:
        stages = [
            {
                "id": f"s{index}",
                "label": name,
                "tone": "ok"
                if index < run["stage"]
                else "running"
                if index == run["stage"]
                else "idle",
            }
            for index, name in enumerate(STAGES)
        ]
        nodes += [
            ui.progress(
                "deploy-progress",
                run["stage"],
                len(STAGES),
                label=f"Deploy {run['number']}",
            ),
            ui.steps("stages", stages),
        ]
    return nodes


def shortcuts(state):
    keys = [ui.shortcut("r", "Start again", node_id="restart")]
    if state["step"] > 0:
        keys.append(ui.shortcut("b", "Back", node_id="back"))
    if state["step"] < 2:
        keys.append(ui.shortcut("n", "Next", node_id="next"))
    return keys
