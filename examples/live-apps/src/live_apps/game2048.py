"""2048: a game on a 4 x 4 board, played with the arrow keys or the buttons."""

from live_apps import ui

KEY = "2048"
TITLE = "2048"
DESCRIPTION = "Slide the tiles and join equal numbers. Reach 2048 to win."
SIZE = 4
GOAL = 2048
MOVES = ("up", "down", "left", "right")


def _random(state):
    """A small LCG, so a game is a pure function of its seed."""
    state["seed"] = (state["seed"] * 1103515245 + 12345) % (2**31)
    return state["seed"]


def _spawn(state):
    empty = [
        (r, c) for r in range(SIZE) for c in range(SIZE) if state["board"][r][c] == 0
    ]
    if not empty:
        return
    r, c = empty[_random(state) % len(empty)]
    state["board"][r][c] = 4 if _random(state) % 10 == 0 else 2


def new_game(seed=2048, best=0):
    state = {
        "board": [[0] * SIZE for _ in range(SIZE)],
        "score": 0,
        "best": best,
        "moves": 0,
        "seed": seed,
    }
    state["status"] = "playing"
    _spawn(state)
    _spawn(state)
    return state


def initial():
    return new_game()


def slide_row(row):
    """One row slid to the left: (new row, points). Each tile joins once."""
    tiles = [v for v in row if v]
    out, points, index = [], 0, 0
    while index < len(tiles):
        if index + 1 < len(tiles) and tiles[index] == tiles[index + 1]:
            out.append(tiles[index] * 2)
            points += tiles[index] * 2
            index += 2
        else:
            out.append(tiles[index])
            index += 1
    return out + [0] * (len(row) - len(out)), points


def _rotate(board):
    return [list(row) for row in zip(*board[::-1], strict=True)]


def move(board, direction):
    """The board after one move: (board, points). Turns so every move is a left slide."""
    turns = {"left": 0, "down": 1, "right": 2, "up": 3}[direction]
    for _ in range(turns):
        board = _rotate(board)
    slid, points = [], 0
    for row in board:
        new_row, gained = slide_row(row)
        slid.append(new_row)
        points += gained
    for _ in range((4 - turns) % 4):
        slid = _rotate(slid)
    return slid, points


def can_move(board):
    return any(move(board, d)[0] != board for d in MOVES)


def reduce(state, event):
    what = ui.intent(event)
    if what == "new":
        return new_game(seed=_random(state), best=state["best"])
    if what in MOVES and state["status"] == "playing":
        board, points = move(state["board"], what)
        if board == state["board"]:
            return state
        state["board"] = board
        state["score"] += points
        state["best"] = max(state["best"], state["score"])
        state["moves"] += 1
        _spawn(state)
        if any(v >= GOAL for row in board for v in row):
            state["status"] = "won"
        elif not can_move(state["board"]):
            state["status"] = "lost"
    if what == "continue" and state["status"] == "won":
        state["status"] = "playing"
    return state


STATUS = {
    "playing": ("Use the arrow keys or the buttons.", "running"),
    "won": ("You reached 2048!", "ok"),
    "lost": ("No moves left. Press n for a new game.", "error"),
}


def render(state):
    rows = [
        ui.table_row(f"r{r}", [str(v) if v else "\u00b7" for v in state["board"][r]])
        for r in range(SIZE)
    ]
    text, tone = STATUS[state["status"]]
    playing = state["status"] == "playing"
    top = max(v for row in state["board"] for v in row)
    controls = [
        ui.button("up", "\u2191", is_disabled=not playing),
        ui.button("left", "\u2190", is_disabled=not playing),
        ui.button("down", "\u2193", is_disabled=not playing),
        ui.button("right", "\u2192", is_disabled=not playing),
        ui.button("new", "New game"),
    ]
    if state["status"] == "won":
        controls.append(ui.button("continue", "Keep playing"))
    return [
        ui.heading("title", "2048"),
        ui.stat(
            "score",
            [
                ui.one_stat("score", "Score", state["score"], "running"),
                ui.one_stat("best", "Best", state["best"], "ok"),
                ui.one_stat("moves", "Moves", state["moves"]),
                ui.one_stat("top", "Top tile", top, "ok" if top >= GOAL else None),
            ],
        ),
        ui.table(
            "board", [(f"c{c}", "ABCD"[c]) for c in range(SIZE)], rows, label="Board"
        ),
        ui.wrap("controls", *controls),
        ui.status("state", text, tone),
    ]


def shortcuts(state):
    keys = [ui.shortcut("n", "New game", node_id="new")]
    if state["status"] == "playing":
        keys += [ui.shortcut(f"Arrow{d.title()}", d.title(), action=d) for d in MOVES]
        keys += [
            ui.shortcut(k, d.title(), action=d)
            for k, d in zip("kjhl", ("up", "down", "left", "right"), strict=True)
        ]
    if state["status"] == "won":
        keys.append(ui.shortcut("c", "Keep playing", node_id="continue"))
    return keys
