"""The settings of the factory. This is the one module that reads the environment.

A factory is one process next to one Vis gateway. Its records and its memories are in
one database, which `FACTORY_DATABASE` selects.
"""

from __future__ import annotations

import os

ENV = os.environ

# -- the place -----------------------------------------------------------------------

# The name of the factory on its pages.
NAME = ENV.get("FACTORY_NAME", "Factory")
# The directory of the factory: its database file, its logs and the files of its runs.
HOME = os.path.expanduser(ENV.get("FACTORY_HOME", "~/.factory"))
# The database: `sqlite:///<file>`, `postgres://...` or `d1://<account>/<database>` (see `tables.py`).
DATABASE = ENV.get("FACTORY_DATABASE") or f"sqlite:///{HOME}/factory.db"
# The token of Cloudflare, for a `d1://` database only.
D1_TOKEN = ENV.get("CLOUDFLARE_API_TOKEN", "")
# The project directory. Each session and each run works in it.
PROJECT = os.path.expanduser(ENV.get("FACTORY_PROJECT") or f"{HOME}/project")
# The directory of the workflow files, `<name>.py`. A file is the one source of a workflow and of its triggers.
WORKFLOWS = os.path.expanduser(ENV.get("FACTORY_WORKFLOWS") or f"{PROJECT}/workflows")
# The author of each commit that the factory makes for a saved workflow.
GIT_NAME, GIT_EMAIL = "Factory", "factory@blockether.com"

# -- the server ----------------------------------------------------------------------

# The address where the factory listens, and the address that people use.
HOST, PORT = ENV.get("FACTORY_HOST", "127.0.0.1"), int(ENV.get("FACTORY_PORT", "8090"))
PUBLIC_URL = ENV.get("FACTORY_PUBLIC_URL", "").rstrip("/")
# The address of the factory for the processes on this machine: the runs, the extension and the memory store of a session.
LOCAL_URL = ENV.get("FACTORY_URL") or f"http://127.0.0.1:{PORT}"
# The one secret of the factory. A person signs in with it, and a process sends it as `Authorization: Bearer`.
TOKEN = ENV.get("FACTORY_TOKEN", "")


def extension_connection() -> tuple[str, str]:
    """The address and the token of the factory for the extension. Vis gives an extension its environment after this module loads, so read them at each call."""
    return ENV.get("FACTORY_URL") or LOCAL_URL, ENV.get("FACTORY_TOKEN", TOKEN)


# The path below which the factory serves its own pages, files and API. Each other path goes to the Vis gateway.
BASE = "/factory"
# The seconds that a sign-in stays valid.
LOGIN_S = 30 * 86400

# -- the Vis gateway -----------------------------------------------------------------

# The gateway of the factory and the file with its token.
GATEWAY_URL = ENV.get("VIS_GATEWAY_URL", "http://127.0.0.1:7890").rstrip("/")
TOKEN_FILE = os.path.expanduser(ENV.get("VIS_GATEWAY_TOKEN_FILE", "~/.vis/gateway.token"))
# The provider and the model of a run whose workflow names a model. Without `@workflow(model=)`, a run uses the model of the gateway.
PROVIDER = ENV.get("FACTORY_PROVIDER", "")
# The session group of the runs, and the group of the sessions that the factory starts for a talk about a suggestion.
RUNS_GROUP, TALKS_GROUP = "Workflows", "Suggestions"

# -- the workflow runs ---------------------------------------------------------------

# The run processes that work at the same time. The other runs wait.
PARALLEL = int(ENV.get("FACTORY_PARALLEL", "3"))
# The activities that one run can have, and the processes that one run can start.
STEPS, STARTS = 1000, 50
# The time that one turn of a run can take, and one call of the factory API.
TURN_S, API_S = 900, 120
# The correction requests that a session gets for an answer that does not fit its model.
CORRECTIONS = 2
# The runs that the API lists.
RUNS_LISTED = 100
# The largest source of a workflow, in bytes, and the largest comment of a person, in characters.
WORKFLOW_LIMIT, COMMENT_LIMIT = 200_000, 4_000

# -- the triggers --------------------------------------------------------------------

# The seconds between two looks of the scheduler at the triggers and at the sessions.
TICK_S = int(ENV.get("FACTORY_TICK_S", "20"))
# A session without a new turn for IDLE_S is idle: the event `session.idle` goes out one time for each number of turns.
IDLE_S = int(ENV.get("FACTORY_IDLE_S", "600"))
# The events that the API lists.
EVENTS_LISTED = 200

# -- the memory ----------------------------------------------------------------------

# The memories that the factory keeps. A session reads and writes them with the extension vis-optmem.
MEMORIES = ("personal", "team")
