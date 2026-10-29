"""Asteroids: turn and thrust the ship, shoot the rocks and dodge the saucer.

The field is one code node of plain ASCII, so the terminal and the Companion
paint the same monospace picture. It has 14 lines, the rows that a Companion
code box shows without a scroll bar.

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
ROCK_SPRITES = {
    3: (" .--. ", "(    )", " '--' "),
    2: (".--.", "'--'"),
    1: ("()",),
}

SAUCER_EVERY = 250
SAUCER_SPEED = 0.35
SAUCER_RADIUS = 1.5
SAUCER_POINTS = 200
SAUCER_FIRE_EVERY = 15
SAUCER_BULLET_SPEED = 1.0
SAUCER_BULLET_LIFE = 16
SAUCER_SPRITE = "<=>"

RESPAWN_TICKS = 15
SAFE_TICKS = 25
WAVE_TICKS = 20
HYPER_COOLDOWN = 20

# The nose of the ship for each of 8 sectors, clockwise from up: (dx, dy, glyph).
NOSES = (
    (0, -1, "|"),
    (1, -1, "/"),
    (1, 0, "-"),
    (1, 1, "\\"),
    (0, 1, "|"),
    (-1, 1, "/"),
    (-1, 0, "-"),
    (-1, -1, "\\"),
)
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


def _ship():
    return {
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
    return {"x": x, "y": y, "vx": vx, "vy": vy, "size": size}


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
        "ship": _ship(),
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
    _spawn_wave(state)
    return state


def initial():
    return new_game(status="ready")


def _burst(state, at, count):
    for _ in range(count):
        vx, vy = _velocity(_uniform(state, 0, 2 * math.pi), _uniform(state, 0.2, 0.7))
        state["particles"].append(
            {
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
            state["ship"] = _ship()
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


def _put(grid, x, y, glyph):
    grid[round(y) % HEIGHT][round(x) % WIDTH] = glyph


def _sprite(grid, thing, rows):
    left = round(thing["x"]) - len(rows[0]) // 2
    top = round(thing["y"]) - len(rows) // 2
    for dy, line in enumerate(rows):
        for dx, glyph in enumerate(line):
            if glyph != " ":
                _put(grid, left + dx, top + dy, glyph)


def _overlay(grid, lines):
    """Write centred text lines with one blank cell around them."""
    top = HEIGHT // 2 - len(lines) // 2
    for index, text in enumerate(lines):
        padded = f" {text} "
        left = (WIDTH - len(padded)) // 2
        for dx, glyph in enumerate(padded):
            _put(grid, left + dx, top + index, glyph)


def field(state):
    """The field as text: a frame of `WIDTH` + 2 columns and `HEIGHT` + 2 lines."""
    grid = [[" "] * WIDTH for _ in range(HEIGHT)]
    for particle in state["particles"]:
        glyph = "x" if particle["life"] >= 4 else "+" if particle["life"] >= 2 else "."
        _put(grid, particle["x"], particle["y"], glyph)
    for rock in state["rocks"]:
        _sprite(grid, rock, ROCK_SPRITES[rock["size"]])
    if state["saucer"] is not None:
        _sprite(grid, state["saucer"], (SAUCER_SPRITE,))
    for bullet in state["saucer_bullets"]:
        _put(grid, bullet["x"], bullet["y"], "o")
    for bullet in state["bullets"]:
        _put(grid, bullet["x"], bullet["y"], "*")
    ship = state["ship"]
    if ship is not None and (ship["safe"] == 0 or state["tick"] % 2 == 0):
        x, y = round(ship["x"]), round(ship["y"])
        # Two headings share each nose; an odd heading takes the nose before it.
        dx, dy, nose = NOSES[(ship["heading"] // (HEADINGS // 8)) % 8]
        if ship["is_thrusting"]:
            _put(grid, x - dx, y - dy, "~")
        _put(grid, x + dx, y + dy, nose)
        _put(grid, x, y, "@")
    message = _message(state)
    if message:
        _overlay(grid, message)
    edge = "+" + "-" * WIDTH + "+"
    return "\n".join([edge, *("|" + "".join(row) + "|" for row in grid), edge])


def _message(state):
    status = state["status"]
    if status == "ready":
        return ["A S T E R O I D S", "", "Press Space to start"]
    if status == "paused":
        return ["PAUSED", "Press p to play"]
    if status == "over":
        return ["GAME OVER", f"{state['score']} points", "Press Space to play again"]
    if state["wave_timer"] > 0:
        return [f"WAVE {state['wave'] + 1}"]
    return None


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
    return f"Wave {state['wave']}: {len(state['rocks'])} rocks left.", "running"


def render(state):
    playing = state["status"] == "playing"
    text, tone = _status(state)
    pause_label = {"ready": "Start", "playing": "Pause", "paused": "Resume"}
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
        {"type": "code", "id": "field", "label": "Field", "text": field(state)},
        ui.wrap(
            "controls",
            ui.button("left", "\u2190 Turn", is_disabled=not playing),
            ui.button("thrust", "Thrust", is_disabled=not playing),
            ui.button("right", "Turn \u2192", is_disabled=not playing),
            ui.button("fire", "Fire", is_disabled=not playing),
            ui.button("hyper", "Hyperspace", is_disabled=not playing),
            ui.button(
                "pause",
                pause_label.get(state["status"], "Pause"),
                is_disabled=state["status"] == "over",
            ),
            ui.button("new", "New game"),
        ),
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
    keys.append(ui.shortcut("n", "New game", node_id="new"))
    return keys
