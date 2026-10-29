"""Asteroids: turn and thrust the ship, shoot the rocks and dodge the saucer.

The field is one `scene` node: the model sends only the world, a list of
objects with a mesh, a place, a turn and a colour. The Companion draws it with
WebGL and eases each object between two ticks; the terminal draws the same
objects as truecolor half blocks. Every object keeps one id while it lives, so
a tick sends only the keys that changed. Keys are the only controls.

The runner gives a `tick` event every `TICK_SECONDS`. A key press only queues a
control, and the next tick applies it. So the view gets at most one patch for
each tick, also while a key repeats. A game that is not playing changes nothing
on a tick, so a ready, paused or finished game sends no patches at all. A game
pauses itself after `IDLE_SECONDS` without a key.
"""

import copy
import math

from live_apps import ui

KEY = "asteroids"
TITLE = "Asteroids"
DESCRIPTION = "Fly the ship, shoot the rocks and dodge the saucer."
TICK_SECONDS = 0.1
IDLE_SECONDS = 45

WIDTH = 48
HEIGHT = 12
# A terminal cell is about twice as tall as it is wide. Distances and speeds are
# in units of one cell width, so a rock flies as fast up as it does sideways.
ASPECT = 2.0
HEADINGS = 16
LIVES = 3
EXTRA_LIFE = 10_000

MAX_TURN = 2  # heading steps that one tick applies
MAX_THRUST = 2
MAX_FIRE = 2
THRUST = 0.12
MAX_SPEED = 1.0
DRAG = 0.98
SHIP_RADIUS = 0.8

BULLET_SPEED = 1.6
BULLET_LIFE = 12
MAX_BULLETS = 4

ROCK_RADIUS = {3: 2.6, 2: 1.8, 1: 0.9}
ROCK_SPEED = {3: (0.15, 0.3), 2: (0.25, 0.45), 1: (0.35, 0.6)}
ROCK_POINTS = {3: 20, 2: 50, 1: 100}
ROCK_COLORS = {3: "#9a8f80", 2: "#b5a58f", 1: "#d2c2a8"}

SAUCER_EVERY = 250
SAUCER_SPEED = 0.35
SAUCER_RADIUS = 1.5
SAUCER_POINTS = 200
SAUCER_FIRE_EVERY = 15
SAUCER_BULLET_SPEED = 1.0
SAUCER_BULLET_LIFE = 16

RESPAWN_TICKS = 15
SAFE_TICKS = 25
WAVE_TICKS = 20
HYPER_COOLDOWN = 20

CONTROLS = ("left", "right", "thrust", "fire", "hyper")


# -- randomness and geometry ------------------------------------------------


def _random(state):
    """A small LCG, so a game is a pure function of its seed."""
    state["seed"] = (state["seed"] * 1103515245 + 12345) % (2**31)
    return state["seed"]


def _uniform(state, low, high):
    return low + (high - low) * (_random(state) / 2**31)


def _direction(heading):
    """The unit vector of a heading in cell units: (dx, dy)."""
    angle = heading * 2 * math.pi / HEADINGS
    return math.sin(angle), -math.cos(angle) / ASPECT


def _velocity(angle, speed):
    return speed * math.sin(angle), -speed * math.cos(angle) / ASPECT


def _next_id(state, prefix):
    """A fresh object id. An object keeps its id while it lives, so the scene
    eases it from tick to tick and never into the place of another one."""
    state["serial"] = state.get("serial", 0) + 1
    return f"{prefix}{state['serial']}"


def _wrap(thing):
    thing["x"] = round(thing["x"] % WIDTH, 4)
    thing["y"] = round(thing["y"] % HEIGHT, 4)


def _move(thing):
    thing["x"] += thing["vx"]
    thing["y"] += thing["vy"]
    _wrap(thing)


def distance(a, b):
    """The shortest distance on the wrapped field, in cell widths."""
    dx = (a["x"] - b["x"] + WIDTH / 2) % WIDTH - WIDTH / 2
    dy = (a["y"] - b["y"] + HEIGHT / 2) % HEIGHT - HEIGHT / 2
    return math.hypot(dx, dy * ASPECT)


# -- the game ---------------------------------------------------------------


def _ship(state):
    return {
        "id": _next_id(state, "ship"),
        "x": WIDTH / 2,
        "y": HEIGHT / 2,
        "vx": 0.0,
        "vy": 0.0,
        "heading": 0,
        "safe": SAFE_TICKS,
        "is_thrusting": False,
    }


def _rock(state, x, y, size):
    low, high = ROCK_SPEED[size]
    speed = _uniform(state, low, high) * min(1.5, 1 + 0.05 * (state["wave"] - 1))
    vx, vy = _velocity(_uniform(state, 0, 2 * math.pi), speed)
    return {
        "id": _next_id(state, "rock"),
        "x": x,
        "y": y,
        "vx": vx,
        "vy": vy,
        "size": size,
    }


def _spawn_wave(state):
    centre = {"x": WIDTH / 2, "y": HEIGHT / 2}
    for _ in range(min(2 + state["wave"], 6)):
        spot = centre
        for _ in range(20):
            spot = {
                "x": _uniform(state, 0, WIDTH),
                "y": _uniform(state, 0, HEIGHT),
            }
            if distance(spot, state["ship"] or centre) >= 12:
                break
        state["rocks"].append(_rock(state, spot["x"], spot["y"], 3))


def new_game(seed=1979, best=0, status="playing"):
    state = {
        "status": status,
        "paused_by": None,
        "seed": seed,
        "score": 0,
        "best": best,
        "lives": LIVES,
        "next_life": EXTRA_LIFE,
        "wave": 1,
        "tick": 0,
        "idle": 0,
        "serial": 0,
        "ship": None,
        "respawn": 0,
        "hyper": 0,
        "wave_timer": 0,
        "rocks": [],
        "bullets": [],
        "saucer": None,
        "saucer_in": SAUCER_EVERY,
        "saucer_bullets": [],
        "particles": [],
        "input": {"turn": 0, "thrust": 0, "fire": 0, "hyper": False},
    }
    state["ship"] = _ship(state)
    _spawn_wave(state)
    return state


def initial():
    return new_game(status="ready")


def save(state):
    """The data that outlives the owner: the best score. A game in flight is a
    picture, so a restart starts a new game."""
    return {"best": state["best"]}


def load(saved):
    return new_game(best=int(saved.get("best") or 0), status="ready")


def _burst(state, at, count):
    for _ in range(count):
        vx, vy = _velocity(_uniform(state, 0, 2 * math.pi), _uniform(state, 0.2, 0.7))
        state["particles"].append(
            {
                "id": _next_id(state, "spark"),
                "x": at["x"],
                "y": at["y"],
                "vx": vx,
                "vy": vy,
                "life": int(_uniform(state, 3, 7)),
            }
        )


def _score(state, points):
    state["score"] += points
    state["best"] = max(state["best"], state["score"])
    while state["score"] >= state["next_life"]:
        state["lives"] += 1
        state["next_life"] += EXTRA_LIFE


def _break(state, rock, *, points=True):
    """Split one rock into two smaller ones, or remove the smallest."""
    state["rocks"].remove(rock)
    if points:
        _score(state, ROCK_POINTS[rock["size"]])
    _burst(state, rock, 2 + rock["size"] * 2)
    if rock["size"] > 1:
        for _ in range(2):
            state["rocks"].append(_rock(state, rock["x"], rock["y"], rock["size"] - 1))


def _lose_ship(state):
    _burst(state, state["ship"], 10)
    state["ship"] = None
    state["lives"] -= 1
    state["respawn"] = RESPAWN_TICKS


def _queue(state, what):
    queued = state["input"]
    if what == "left":
        queued["turn"] = max(-MAX_TURN, queued["turn"] - 1)
    elif what == "right":
        queued["turn"] = min(MAX_TURN, queued["turn"] + 1)
    elif what == "thrust":
        queued["thrust"] = min(MAX_THRUST, queued["thrust"] + 1)
    elif what == "fire":
        queued["fire"] = min(MAX_FIRE, queued["fire"] + 1)
    elif what == "hyper":
        queued["hyper"] = True


def _steer(state):
    """Apply the queued controls to the ship, then clear the queue."""
    queued = state["input"]
    state["input"] = {"turn": 0, "thrust": 0, "fire": 0, "hyper": False}
    ship = state["ship"]
    if ship is None:
        return
    ship["heading"] = (ship["heading"] + queued["turn"]) % HEADINGS
    dx, dy = _direction(ship["heading"])
    ship["is_thrusting"] = queued["thrust"] > 0
    if queued["thrust"]:
        ship["vx"] += dx * THRUST * queued["thrust"]
        ship["vy"] += dy * THRUST * queued["thrust"]
    speed = math.hypot(ship["vx"], ship["vy"] * ASPECT)
    if speed > MAX_SPEED:
        ship["vx"] *= MAX_SPEED / speed
        ship["vy"] *= MAX_SPEED / speed
    for _ in range(queued["fire"]):
        if len(state["bullets"]) >= MAX_BULLETS:
            break
        state["bullets"].append(
            {
                "id": _next_id(state, "shot"),
                "x": ship["x"] + dx,
                "y": ship["y"] + dy,
                "vx": ship["vx"] + dx * BULLET_SPEED,
                "vy": ship["vy"] + dy * BULLET_SPEED,
                "life": BULLET_LIFE,
            }
        )
    if queued["hyper"] and state["hyper"] == 0:
        ship["x"] = _uniform(state, 0, WIDTH)
        ship["y"] = _uniform(state, 0, HEIGHT)
        ship["vx"] = ship["vy"] = 0.0
        # A jump is not a flight: a new id makes the ship appear, not glide.
        ship["id"] = _next_id(state, "ship")
        state["hyper"] = HYPER_COOLDOWN
        _burst(state, ship, 4)


def _fly(state):
    ship = state["ship"]
    if ship is not None:
        ship["vx"] *= DRAG
        ship["vy"] *= DRAG
        _move(ship)
        ship["safe"] = max(0, ship["safe"] - 1)
    for rock in state["rocks"]:
        _move(rock)
    for key in ("bullets", "saucer_bullets", "particles"):
        for thing in state[key]:
            _move(thing)
            thing["life"] -= 1
        state[key] = [thing for thing in state[key] if thing["life"] > 0]
    saucer = state["saucer"]
    if saucer is not None:
        _move(saucer)
        saucer["life"] -= 1
        if saucer["life"] <= 0:
            state["saucer"] = None


def _saucer(state):
    """Send a saucer now and then; it turns and fires at the ship."""
    saucer = state["saucer"]
    if saucer is None:
        state["saucer_in"] -= 1
        if state["saucer_in"] <= 0 and state["ship"] is not None:
            state["saucer_in"] = SAUCER_EVERY
            side = _random(state) % 2
            state["saucer"] = {
                "id": _next_id(state, "saucer"),
                "x": 0.0 if side == 0 else WIDTH - 0.01,
                "y": _uniform(state, 1, HEIGHT - 1),
                "vx": SAUCER_SPEED if side == 0 else -SAUCER_SPEED,
                "vy": 0.0,
                "life": int(WIDTH / SAUCER_SPEED),
                "fire_in": SAUCER_FIRE_EVERY,
            }
        return
    if saucer["life"] % 20 == 0:
        saucer["vy"] = (_random(state) % 3 - 1) * 0.15
    saucer["fire_in"] -= 1
    ship = state["ship"]
    if saucer["fire_in"] <= 0 and ship is not None:
        saucer["fire_in"] = SAUCER_FIRE_EVERY
        dx = (ship["x"] - saucer["x"] + WIDTH / 2) % WIDTH - WIDTH / 2
        dy = ((ship["y"] - saucer["y"] + HEIGHT / 2) % HEIGHT - HEIGHT / 2) * ASPECT
        angle = math.atan2(dx, -dy) + _uniform(state, -0.5, 0.5)
        vx, vy = _velocity(angle, SAUCER_BULLET_SPEED)
        state["saucer_bullets"].append(
            {
                "id": _next_id(state, "bolt"),
                "x": saucer["x"],
                "y": saucer["y"],
                "vx": vx,
                "vy": vy,
                "life": SAUCER_BULLET_LIFE,
            }
        )


def _collide(state):
    for bullet in list(state["bullets"]):
        hit = next(
            (r for r in state["rocks"] if distance(bullet, r) < ROCK_RADIUS[r["size"]]),
            None,
        )
        if hit is not None:
            state["bullets"].remove(bullet)
            _break(state, hit)
            continue
        saucer = state["saucer"]
        if saucer is not None and distance(bullet, saucer) < SAUCER_RADIUS:
            state["bullets"].remove(bullet)
            _score(state, SAUCER_POINTS)
            _burst(state, saucer, 8)
            state["saucer"] = None
    saucer = state["saucer"]
    if saucer is not None:
        hit = next(
            (
                r
                for r in state["rocks"]
                if distance(saucer, r) < ROCK_RADIUS[r["size"]] + SAUCER_RADIUS
            ),
            None,
        )
        if hit is not None:
            _break(state, hit, points=False)
            _burst(state, saucer, 8)
            state["saucer"] = None
    ship = state["ship"]
    if ship is None or ship["safe"] > 0:
        return
    rock = next(
        (
            r
            for r in state["rocks"]
            if distance(ship, r) < ROCK_RADIUS[r["size"]] + SHIP_RADIUS
        ),
        None,
    )
    if rock is not None:
        _break(state, rock)
        _lose_ship(state)
        return
    shot = next(
        (b for b in state["saucer_bullets"] if distance(ship, b) < SHIP_RADIUS + 0.5),
        None,
    )
    saucer = state["saucer"]
    if shot is not None:
        state["saucer_bullets"].remove(shot)
        _lose_ship(state)
    elif saucer is not None and distance(ship, saucer) < SAUCER_RADIUS + SHIP_RADIUS:
        _score(state, SAUCER_POINTS)
        _burst(state, saucer, 8)
        state["saucer"] = None
        _lose_ship(state)


def _timers(state):
    state["hyper"] = max(0, state["hyper"] - 1)
    if state["ship"] is None and state["lives"] > 0:
        state["respawn"] -= 1
        if state["respawn"] <= 0:
            state["ship"] = _ship(state)
    if state["ship"] is None and state["lives"] <= 0 and not state["particles"]:
        state["status"] = "over"
    if not state["rocks"]:
        if state["wave_timer"] == 0:
            state["wave_timer"] = WAVE_TICKS
        state["wave_timer"] -= 1
        if state["wave_timer"] == 0:
            state["wave"] += 1
            _spawn_wave(state)


def _tick(state):
    if state["status"] != "playing":
        return state
    state["idle"] += 1
    if state["idle"] >= IDLE_SECONDS / TICK_SECONDS:
        state["status"] = "paused"
        state["paused_by"] = "idle"
        return state
    _steer(state)
    _fly(state)
    _saucer(state)
    _collide(state)
    _timers(state)
    state["tick"] += 1
    return state


def reduce(state, event):
    state = copy.deepcopy(state)
    if event.get("type") == "tick":
        return _tick(state)
    what = ui.intent(event)
    status = state["status"]
    if what == "new":
        return new_game(seed=_random(state), best=state["best"])
    if what == "fire" and status == "over":
        return new_game(seed=_random(state), best=state["best"])
    if (what == "pause" and status != "over") or (
        what == "fire" and status in ("ready", "paused")
    ):
        state["status"] = "paused" if status == "playing" else "playing"
        state["paused_by"] = "you" if status == "playing" else None
        state["idle"] = 0
        return state
    if what in CONTROLS and status == "playing":
        _queue(state, what)
        state["idle"] = 0
    return state


# -- the picture ------------------------------------------------------------


# Scene units are cell widths and y points up, so the field is WIDTH wide and
# HEIGHT * ASPECT high, and a rock looks as round as it collides.
SCENE_HEIGHT = HEIGHT * ASPECT


def _at(thing, prefix, index):
    """The id, x and y of one thing in scene units."""
    return {
        "id": thing.get("id") or f"{prefix}{index}",
        "x": round(thing["x"], 2),
        "y": round((HEIGHT - thing["y"]) * ASPECT, 2),
    }


def _spin(object_id, tick):
    """A steady tumble for each id, so a rock turns without any state."""
    rate = 4 + sum(map(ord, object_id)) % 7
    return {"rx": tick * rate % 360, "ry": tick * rate * 2 % 360}


def _ship_objects(state):
    ship = state["ship"]
    if ship is None:
        return []
    blink = ship["safe"] > 0 and state["tick"] % 2 == 1
    turn = -ship["heading"] * 360 / HEADINGS
    body = _at(ship, "ship", 0)
    objects = [
        ui.scene_object(
            body["id"],
            "cone",
            x=body["x"],
            y=body["y"],
            rx=30,
            rz=turn,
            scale=0.9,
            color="#35505e" if blink else "#7fd4ff",
        )
    ]
    if ship["is_thrusting"]:
        dx, dy = _direction(ship["heading"])
        flame = _at({"x": ship["x"] - dx * 1.4, "y": ship["y"] - dy * 1.4}, "", 0)
        objects.append(
            ui.scene_object(
                f"{body['id']}-flame",
                "cone",
                x=flame["x"],
                y=flame["y"],
                rz=turn + 180,
                scale=0.45,
                color="#ff9f1c",
                is_emissive=True,
            )
        )
    return objects


def scene(state):
    """The field as a scene: the world only, never pixels."""
    tick = state["tick"]
    objects = []
    for index, particle in enumerate(state["particles"]):
        life = particle["life"]
        color = "#ffd166" if life >= 4 else "#ff8c42" if life >= 2 else "#c0392b"
        spark = _at(particle, "spark", index)
        objects.append(
            ui.scene_object(
                spark.pop("id"),
                "tetrahedron",
                **spark,
                scale=0.3,
                color=color,
                is_emissive=True,
            )
        )
    for index, rock in enumerate(state["rocks"]):
        at = _at(rock, "rock", index)
        rock_id = at.pop("id")
        objects.append(
            ui.scene_object(
                rock_id,
                "icosahedron",
                **at,
                **_spin(rock_id, tick),
                scale=ROCK_RADIUS[rock["size"]],
                color=ROCK_COLORS[rock["size"]],
            )
        )
    if state["saucer"] is not None:
        at = _at(state["saucer"], "saucer", 0)
        saucer_id = at.pop("id")
        objects.append(
            ui.scene_object(
                saucer_id,
                "octahedron",
                **at,
                rx=70,
                ry=tick * 12 % 360,
                scale=SAUCER_RADIUS,
                color="#c792ea",
            )
        )
    for key, prefix, color in (
        ("saucer_bullets", "bolt", "#ff6b6b"),
        ("bullets", "shot", "#ffe066"),
    ):
        for index, bullet in enumerate(state[key]):
            at = _at(bullet, prefix, index)
            objects.append(
                ui.scene_object(
                    at.pop("id"),
                    "sphere",
                    **at,
                    scale=0.3,
                    color=color,
                    is_emissive=True,
                )
            )
    objects.extend(_ship_objects(state))
    return ui.scene(
        "field",
        WIDTH,
        SCENE_HEIGHT,
        objects,
        label="Field",
        fov=30,
        is_wrapping=True,
        tick_ms=round(TICK_SECONDS * 1000),
    )


def _status(state):
    status = state["status"]
    if status == "ready":
        return "Press Space to start. Shoot every rock and dodge the saucer.", "idle"
    if status == "paused" and state["paused_by"] == "idle":
        return f"Paused after {IDLE_SECONDS} s without a key. Press p to play.", "warn"
    if status == "paused":
        return "Paused. Press p to play.", "warn"
    if status == "over":
        return (
            f"Game over with {state['score']} points. Press Space for a new game.",
            "error",
        )
    if state["ship"] is None:
        return "Your ship is lost. The next one comes in the centre.", "warn"
    if state["wave_timer"] > 0:
        return f"Wave {state['wave'] + 1} is coming.", "running"
    return f"Wave {state['wave']}: {len(state['rocks'])} rocks left.", "running"


def render(state):
    text, tone = _status(state)
    return [
        ui.heading("title", "Asteroids"),
        ui.stat(
            "hud",
            [
                ui.one_stat("score", "Score", state["score"], "running"),
                ui.one_stat("best", "Best", state["best"], "ok"),
                ui.one_stat(
                    "lives",
                    "Lives",
                    state["lives"],
                    "error" if state["lives"] <= 1 else None,
                ),
                ui.one_stat("wave", "Wave", state["wave"]),
                ui.one_stat("rocks", "Rocks", len(state["rocks"])),
            ],
        ),
        scene(state),
        ui.status("state", text, tone),
        ui.paragraph("help", ui.key_help(shortcuts(state))),
    ]


def shortcuts(state):
    keys = []
    for named, letter, action, label in (
        ("ArrowLeft", "a", "left", "Turn left"),
        ("ArrowRight", "d", "right", "Turn right"),
        ("ArrowUp", "w", "thrust", "Thrust"),
        ("Space", "f", "fire", "Fire"),
        ("ArrowDown", "h", "hyper", "Hyperspace"),
    ):
        keys.append(ui.shortcut(named, label, action=action))
        keys.append(ui.shortcut(letter, label, action=action))
    keys.append(ui.shortcut("p", "Pause", action="pause"))
    keys.append(ui.shortcut("n", "New game", action="new"))
    return keys
