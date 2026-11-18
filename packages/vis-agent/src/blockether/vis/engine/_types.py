"""Generated gateway response types. Do not edit.

The `response_schema` of each route in `vis-contract/schema/gateway.json` gives
one type here. Run `python tests/test_contracts.py` in `packages/vis-agent` to
write this file again.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Literal, NotRequired, TypeAlias, TypedDict

JSONValue: TypeAlias = (
    str | int | float | bool | None | list["JSONValue"] | dict[str, "JSONValue"]
)


EmptyObject: TypeAlias = dict[str, JSONValue]


class ClientCallsCallsItem(TypedDict):
    id: str
    name: str
    args: list[JSONValue]
    kwargs: dict[str, JSONValue]


class ClientCalls(TypedDict):
    calls: list[ClientCallsCallsItem]


class Handshake(TypedDict):
    protocol: int
    min_client: int
    min_gateway: int
    version: str | None
    build: str | None


ConfigMachineName: TypeAlias = str


class Health(TypedDict):
    """The answer of /healthz and /readyz: the stable id of the gateway, its protocol
    and the machine name that its operator set. A gateway without a name omits
    machine_name. The answer also carries process counters that have no contract.
    """

    status: Literal["ok"]
    id: str
    protocol: Handshake
    machine_name: NotRequired[ConfigMachineName]
    require_token: NotRequired[bool]
    secret_match: NotRequired[bool]


class OpenapiDocument(TypedDict):
    openapi: str
    info: NotRequired[dict[str, JSONValue]]
    paths: dict[str, JSONValue]


class ProtocolInfo(TypedDict):
    """The protocol numbers and build of the gateway."""

    build: str | None
    min_client: int
    min_gateway: int
    protocol: int
    version: str


class GatewayStatus(TypedDict):
    """The gateway status map. The engine adds fields."""

    id: NotRequired[str | None]
    pid: NotRequired[int | None]
    port: NotRequired[int | None]
    protocol: ProtocolInfo
    running_turns: NotRequired[int]
    sse_clients: NotRequired[int]
    managed: NotRequired[bool]


class GatewayStop(TypedDict):
    stopping: bool
    status: GatewayStatus


class GatewayDrain(TypedDict):
    """Counts of the turns that a drain parked, forced and left running."""

    parked: int
    forced: int
    running: int


class AppList(TypedDict):
    views: list[dict[str, JSONValue]]


AppDoorAnswer: TypeAlias = dict[str, JSONValue]


class AppViewVariant1(TypedDict):
    view: dict[str, JSONValue]


CommonNonBlankString: TypeAlias = str


class ViewViewDeltaPatchOpsItem(TypedDict):
    op: CommonNonBlankString


class ViewViewDeltaPatch(TypedDict):
    """A `live_patch`. An operation comes without each key that a splice gives."""

    view_id: CommonNonBlankString
    seq: int
    ops: list[ViewViewDeltaPatchOpsItem]


class ViewStringEdit(TypedDict):
    """One change of a string. At `start`, `delete` UTF-16 code units of the base
    string go out and `insert` comes in. `start` counts in the base string.
    """

    start: int
    delete: int
    insert: str


class ViewPatchSplice(TypedDict):
    """A long string of one `set` or `add-node` operation, as edits of a base string.
    The operation comes without the key. The base is the string that the node has
    for the key before the operation. For a node that an earlier operation of the
    frame removed, the base is the string that the node had when the frame started.
    """

    op_index: int
    node_id: NotRequired[CommonNonBlankString]
    key: CommonNonBlankString
    base_hash: int
    edits: list[ViewStringEdit]


class ViewViewDelta(TypedDict):
    """One published frame of an app, as a read with `since` gives it. A string with
    more characters than `x-vis-splice-above-chars` comes as a `patch_splice` when
    the node had a string of that length before. The gateway keeps the newest frames
    of an app while their strings hold at most `x-vis-chars-kept` characters. It
    answers the whole view when the frames hold more characters than the view.
    """

    first_seq: int
    patch: ViewViewDeltaPatch
    splices: list[ViewPatchSplice]


class AppViewVariant2(TypedDict):
    patches: list[ViewViewDelta]


AppView: TypeAlias = AppViewVariant1 | AppViewVariant2


class ViewOutcome(TypedDict):
    is_accepted: bool
    reason: NotRequired[str | None]


class CapabilitiesCompatibility(TypedDict):
    behind: int | None
    client_min_gateway: int | None
    client_name: str
    client_protocol: int | None
    client_version: str | None
    gateway_min_client: int
    gateway_protocol: int
    gateway_version: str
    is_compatible: bool
    reason: str
    upgrade: str | None


class Capabilities(TypedDict):
    version: int
    protocol: ProtocolInfo
    addresses: NotRequired[list[str]]
    compatibility: NotRequired[CapabilitiesCompatibility]
    features: dict[str, JSONValue]


class ClientLeaseExtensions(TypedDict):
    packages: list[JSONValue]
    stage: str


class ClientLease(TypedDict):
    client_id: str
    status: GatewayStatus
    extensions: NotRequired[ClientLeaseExtensions]


class ClientRelease(TypedDict):
    released: bool
    status: NotRequired[GatewayStatus]


class Devices(TypedDict):
    devices: list[dict[str, JSONValue]]
    push: dict[str, JSONValue]


class DeviceRegistered(TypedDict):
    device: dict[str, JSONValue]
    push: NotRequired[dict[str, JSONValue]]


class Removed(TypedDict):
    is_removed: bool


class DeviceTest(TypedDict):
    results: list[dict[str, JSONValue]]


class FsEntry(TypedDict):
    name: str
    path: str
    entry_count: NotRequired[int | None]
    is_repo: NotRequired[bool]
    branch: NotRequired[str | None]


class FsListing(TypedDict):
    path: str
    parent: NotRequired[str | None]
    home: NotRequired[str]
    is_truncated: NotRequired[bool]
    entries: list[FsEntry]


class MachineOrder(TypedDict):
    machine_ids: list[str]


class McpServer(TypedDict):
    """One sanitized MCP inventory row. The row can also have command, args, cwd, url,
    timeout_ms, source and is_override.
    """

    name: str
    transport: Literal["stdio", "sse", "streamable_http"]
    enabled: bool
    is_connected: bool
    status: Literal[
        "connected",
        "disconnected",
        "connecting",
        "unhealthy",
        "disabled",
        "killed",
    ]
    is_managed: bool
    is_killed: NotRequired[bool]
    tools: NotRequired[int | None]


class McpServers(TypedDict):
    servers: list[McpServer]


class McpDeleted(TypedDict):
    name: str
    is_deleted: JSONValue


class McpAuthCancelled(TypedDict):
    flow_id: str
    is_cancelled: JSONValue


class McpAuthFlow(TypedDict):
    """The wire view of one MCP sign-in flow."""

    flow_id: str
    server: str
    kind: NotRequired[str | None]
    url: NotRequired[str | None]
    redirect_uri: NotRequired[str | None]
    callback_mode: NotRequired[str | None]
    expires_at_ms: NotRequired[int | None]
    status: str | None
    error: NotRequired[str]


class McpTokenStatus(TypedDict):
    """The non-secret view of the stored OAuth tokens of one MCP server."""

    server: str
    is_authorized: bool
    is_expired: bool
    has_refresh_token: bool
    expires_at_ms: int | None
    scope: str | None


class McpTestToolsItem(TypedDict):
    name: str
    description: NotRequired[str | None]


class McpTest(TypedDict):
    name: str
    is_connected: bool
    tools: list[McpTestToolsItem]


class ModelsCatalogProvidersItem(TypedDict):
    id: str
    doc: NotRequired[str | None]


class ModelsCatalog(TypedDict):
    providers: list[ModelsCatalogProvidersItem]
    models: list[dict[str, JSONValue]]


class Project(TypedDict):
    id: str
    owner_id: NotRequired[str]
    name: str
    color: NotRequired[str | None]
    position: NotRequired[int]
    session_count: NotRequired[int]
    workspace_root: NotRequired[str | None]
    created_at: int
    archived_at: NotRequired[int | None]


class ProjectList(TypedDict):
    projects: list[Project]


class ProjectSessionsMoved(TypedDict):
    project_id: str
    count: int


class SessionGroupRecord(TypedDict):
    id: str
    project_id: NotRequired[str | None]
    name: str
    color: NotRequired[str | None]
    active_at: NotRequired[int | None]
    session_count: NotRequired[int]
    created_at: NotRequired[int]
    archived_at: NotRequired[int | None]


class SessionGroupPage(TypedDict):
    project_id: NotRequired[str | None]
    groups: list[SessionGroupRecord]
    total: NotRequired[int]
    session_total: NotRequired[int]
    limit: NotRequired[int | None]
    offset: NotRequired[int]
    has_more: NotRequired[bool]


class SessionGroupDeleted(TypedDict):
    group_id: str
    scattered_session_ids: list[str]
    deleted_session_ids: list[str]
    session_count: int


class SessionGoal(TypedDict):
    id: str
    objective: str
    status: Literal[
        "active",
        "paused",
        "blocked",
        "budget_limited",
        "complete",
        "cancelled",
        "cleared",
    ]
    iteration_budget: int | None
    tokens_used: int
    time_used_ms: int
    revision: int
    version: int
    created_at: int
    updated_at: int
    iterations_used: int


class SessionOriginVariant2(TypedDict):
    """The program that created the session."""

    kind: Literal["extension"]
    extension: str
    session_id: str


SessionOrigin: TypeAlias = None | SessionOriginVariant2


class SessionTeam(TypedDict):
    """The subagents of a leader session. active counts the queued and running
    children; total counts all children. A subagent session has zero of each.
    """

    active: int
    total: int


class AgentsModelPair(TypedDict):
    provider: str
    model: str


class SessionAgent(TypedDict):
    """The agent context of the session: a leader, or a subagent with its task, budget
    and file grant.
    """

    role: Literal["leader", "subagent"]
    session_id: str
    leader_id: str
    interactive: bool
    routing_locked: bool
    parent_id: NotRequired[str]
    team_id: NotRequired[str]
    task: NotRequired[str]
    iteration_budget: NotRequired[int]
    iterations_used: NotRequired[int]
    status: NotRequired[JSONValue]
    allowed_models: NotRequired[None | list[AgentsModelPair]]
    files: NotRequired[dict[str, JSONValue]]
    budget_notice: NotRequired[str]


class SessionModelPref2(TypedDict):
    provider: str
    model: str


class SessionWorkspace(TypedDict):
    root: str
    label: str | None


class TurnTokens(TypedDict):
    cache_created: int
    cached: int
    input: int
    input_regular: int
    output: int
    reasoning: int


class TurnCost(TypedDict):
    total_cost: float


class TurnRecord(TypedDict):
    """One turn of a session as the turn routes answer it. The engine can add fields."""

    turn_id: str
    session_id: str
    status: str
    request: NotRequired[str | None]
    request_kind: str
    role: NotRequired[str]
    content: NotRequired[list[dict[str, JSONValue]]]
    created_at: int
    position: NotRequired[int]
    completed_at: NotRequired[int | None]
    duration_ms: NotRequired[int | None]
    iteration_count: NotRequired[int]
    council: NotRequired[dict[str, JSONValue] | None]
    tokens: NotRequired[TurnTokens]
    cost: NotRequired[TurnCost]


class QueuePausedMarker(TypedDict):
    """The live hold of the turn queue: held turns wait for reason."""

    reason: str
    held: int
    gen: NotRequired[int]
    at: NotRequired[int]


class SessionRecord(TypedDict):
    """The wire soul of one session: its stored record and its live gateway status.
    goal is null until an explicit /goal or SDK request creates it. List rows also
    carry unread_answers and is_unread. Search rows also carry match. GET with
    include=queued adds queued_turns and queue_paused.
    """

    id: str
    channel: str | None
    agent_name: str
    title: str | None
    goal: None | SessionGoal
    model: str | None
    created_at: int | None
    project_id: str | None
    project_name: str | None
    project_position: int | None
    group_id: str | None
    favorite_rank: float | None
    archived_at: int | None
    origin: SessionOrigin
    status: Literal["idle", "running", "suspended"]
    live: bool
    is_awaiting_input: bool
    awaiting_input_count: int
    current_turn_id: str | None
    turn_count: int
    answer_count: int
    was_interrupted: bool
    was_failed: bool
    team: SessionTeam
    agent: SessionAgent
    server_time_ms: int
    model_pref: NotRequired[None | SessionModelPref2]
    workspace: NotRequired[SessionWorkspace]
    modified_at: NotRequired[int]
    running_request: NotRequired[str]
    running_request_kind: NotRequired[str]
    running_council: NotRequired[JSONValue]
    running_started_at: NotRequired[int]
    running_position: NotRequired[int]
    running_created_at: NotRequired[int]
    queued_turns: NotRequired[list[TurnRecord]]
    queue_paused: NotRequired[None | QueuePausedMarker]
    unread_answers: NotRequired[int]
    is_unread: NotRequired[bool]
    match: NotRequired[dict[str, JSONValue]]


class SessionRead(TypedDict):
    session_id: str
    reader: str
    seen_answers: int
    is_unread: bool


class ProjectOverviewProjectsItem(TypedDict):
    awaiting_count: int
    last_activity_ms: int
    live_count: int
    name: str
    project_id: str | None
    root: str
    session_count: int
    stopped_count: int
    unread_count: int


class ProjectOverview(TypedDict):
    project_count: NotRequired[int]
    session_count: NotRequired[int]
    live_count: NotRequired[int]
    awaiting_count: NotRequired[int]
    unread_count: NotRequired[int]
    stopped_count: NotRequired[int]
    projects: list[ProjectOverviewProjectsItem]
    server_time_ms: NotRequired[int]


class ProviderPresets(TypedDict):
    presets: list[dict[str, JSONValue]]


class RouterFleet(TypedDict):
    """The provider fleet. A provider removal also gives is_removed. A selection change
    gives the default and fallback fields.
    """

    providers: list[dict[str, JSONValue]]


class ProviderAuthVariant1(TypedDict):
    """The flow that `auth/start` starts. `kind` is pkce, device or api-key."""

    flow_id: str
    provider_id: str
    kind: Literal["pkce", "device", "api-key"]
    expires_at: int
    url: NotRequired[str]
    redirect_uri: NotRequired[str]
    callback_mode: NotRequired[str]
    user_code: NotRequired[str]
    verification_uri: NotRequired[str]
    interval_ms: NotRequired[int]
    instructions: NotRequired[list[str] | None]


class ProviderAuthVariant2(TypedDict):
    """The status of a sign-in step, with the poll message and the sign-in lines."""

    status: str
    message: NotRequired[str | None]
    instructions: NotRequired[list[str] | None]


ProviderAuth: TypeAlias = ProviderAuthVariant1 | ProviderAuthVariant2


class ProviderReportStatic(TypedDict):
    rpm: NotRequired[int]
    tpm: NotRequired[int]


class ProviderWindow(TypedDict):
    kind: Literal["calendar", "lifetime", "rolling"]
    unit: NotRequired[Literal["day", "hour", "minute", "month", "week", "year"]]
    size: NotRequired[int]
    resets_at_ms: NotRequired[int]


class ProviderLimitRow(TypedDict):
    id: CommonNonBlankString
    label: CommonNonBlankString
    scope: Literal["account", "model", "plan", "workspace"]
    subject: NotRequired[dict[str, JSONValue]]
    kind: Literal["credits", "rate", "requests", "sessions", "tokens", "usd"]
    precision: Literal["derived", "estimate", "exact", "unknown"]
    source: Literal["derived", "local", "provider-api", "static"]
    used: NotRequired[float]
    limit: NotRequired[float]
    remaining: NotRequired[float]
    is_unlimited: bool
    note: NotRequired[str]
    window: NotRequired[ProviderWindow]


class ProviderResetCredits(TypedDict):
    """Account-scoped allowance for explicitly confirmed limit resets. Missing
    availability is never zero. Credentials must not appear here.
    """

    status: Literal["error", "ok", "unsupported"]
    available_count: NotRequired[int]
    account_id: NotRequired[CommonNonBlankString]
    message: NotRequired[str]


class ProviderReportDynamic(TypedDict):
    limits: list[ProviderLimitRow]
    reset_credits: NotRequired[ProviderResetCredits]
    note: NotRequired[str]


class ProviderReportError(TypedDict):
    type: CommonNonBlankString
    message: CommonNonBlankString
    data: NotRequired[dict[str, JSONValue]]


class ProviderReport(TypedDict):
    provider_id: CommonNonBlankString
    status: Literal["error", "ok", "unauthenticated", "unknown-provider", "unsupported"]
    fetched_at_ms: int
    static: ProviderReportStatic
    dynamic: ProviderReportDynamic
    error: NotRequired[ProviderReportError]


class ProviderLimits(TypedDict):
    report: ProviderReport


class ProviderModels(TypedDict):
    models: list[str]
    hidden_count: NotRequired[int]


class ProviderResetOutcome(TypedDict):
    outcome: Literal["reset", "nothing_to_reset", "no_credit", "already_redeemed"]


class ProviderStatusStatus(TypedDict):
    auth_state: str
    is_authenticated: bool


class ProviderStatus(TypedDict):
    status: ProviderStatusStatus


class RouterSelection(TypedDict):
    """Both router tags. A role is null where it has no tag."""

    default_provider: str | None
    default_model: str | None
    fallback_provider: str | None
    fallback_model: str | None


RouterPatch: TypeAlias = RouterSelection | RouterFleet


class SessionList(TypedDict):
    sessions: list[SessionRecord]


class SessionCreated(TypedDict):
    """The answer of POST /v1/sessions: the new session id and the values that made it.
    Read GET /v1/sessions/:sid for the full session.
    """

    id: str
    channel: str
    title: str | None
    goal: None | SessionGoal
    external_id: str | None
    workspace_id: str | None
    group_id: str | None
    origin: SessionOrigin


ActivityPage: TypeAlias = dict[str, JSONValue]


class SessionArtifacts(TypedDict):
    artifacts: list[dict[str, JSONValue]]


class Upload(TypedDict):
    upload_id: str
    size: int


class SessionBlock(TypedDict):
    """The result of one block: the text that the agent sees. `is_error` is true when
    the block failed.
    """

    result: str
    is_error: bool


class TurnCancel(TypedDict):
    status: Literal["cancelling", "deleted"]


SessionContext: TypeAlias = dict[str, JSONValue]


class CouncilBinding(TypedDict):
    default_group_id: str
    activation_id: str | None


AgentsRole: TypeAlias = Literal["research", "implement", "verify"]


class AgentsAgentBudgetRequest(TypedDict):
    iterations: NotRequired[int]
    reason: NotRequired[str]


AgentsFileRules: TypeAlias = list[str]


class AgentsFiles(TypedDict):
    write: NotRequired[AgentsFileRules]
    read_only: NotRequired[AgentsFileRules]
    deny: NotRequired[AgentsFileRules]


class AgentsAgentFilesRequest(TypedDict):
    files: NotRequired[AgentsFiles]
    reason: NotRequired[str]


class AgentsAgentRunsItem(TypedDict):
    start: int
    end: int
    status: Literal["completed", "failed", "cancelled", "budget_limited"]
    outcome: NotRequired[JSONValue]


class AgentsAgent(TypedDict):
    id: NotRequired[str]
    session_id: str
    parent_id: str
    leader_id: str
    team_id: str
    task: str
    title: NotRequired[JSONValue]
    role: NotRequired[AgentsRole]
    status: Literal[
        "waiting",
        "queued",
        "running",
        "completed",
        "failed",
        "cancelled",
        "budget_limited",
        "blocked",
    ]
    after: NotRequired[list[JSONValue]]
    iteration_budget: JSONValue
    iterations_used: int
    provider: NotRequired[str | None]
    model: NotRequired[str | None]
    fallback_model: NotRequired[AgentsModelPair]
    routing_locked: NotRequired[bool]
    pending_input: NotRequired[bool]
    pending_input_count: NotRequired[int]
    question: NotRequired[str]
    question_entry_id: NotRequired[int]
    budget_request: NotRequired[AgentsAgentBudgetRequest]
    files_request: NotRequired[AgentsAgentFilesRequest]
    activity: NotRequired[str]
    note: NotRequired[str]
    result: NotRequired[str]
    outcome: NotRequired[JSONValue]
    usage: NotRequired[dict[str, JSONValue] | None]
    files: NotRequired[AgentsFiles]
    created_at: NotRequired[int]
    resumed_at: NotRequired[int | None]
    finished_at: NotRequired[int]
    runs: NotRequired[list[AgentsAgentRunsItem]]


AgentSpawned: TypeAlias = AgentsAgent


class AgentsRecordBudgetRequest(TypedDict):
    iterations: int
    reason: NotRequired[str]


class AgentsRecordFilesRequest(TypedDict):
    files: AgentsFiles
    reason: NotRequired[str]


class AgentsRecordMessagesItem(TypedDict):
    entry_id: int
    content: str


class AgentsRecordError(TypedDict):
    """Why a failed child stopped. Code rate_limited means a 429 from its provider.
    Code not_started means that its turn never started. Code interrupted means that
    its turn stopped before its end. Code dependency_failed means that a task in
    after did not complete, so a blocked child never started.
    """

    code: Literal["rate_limited", "not_started", "interrupted", "dependency_failed"]
    message: str
    provider: NotRequired[str]
    model: NotRequired[str]


class AgentsRecord(TypedDict):
    session_id: str
    id: NotRequired[JSONValue]
    status: Literal[
        "waiting",
        "queued",
        "running",
        "asking",
        "held",
        "completed",
        "failed",
        "cancelled",
        "budget_limited",
        "blocked",
        "unknown",
    ]
    after: NotRequired[list[dict[str, JSONValue]] | None]
    answer: NotRequired[str | None]
    question: NotRequired[str | None]
    question_entry_id: NotRequired[int | None]
    budget_request: NotRequired[AgentsRecordBudgetRequest | None]
    files_request: NotRequired[AgentsRecordFilesRequest | None]
    files: NotRequired[AgentsFiles | None]
    iteration_budget: NotRequired[JSONValue]
    iterations_used: NotRequired[int | None]
    activity: NotRequired[str | None]
    notes: NotRequired[list[str] | None]
    messages: NotRequired[list[AgentsRecordMessagesItem] | None]
    step_secs: NotRequired[int | None]
    elapsed_secs: NotRequired[int | None]
    alerts: NotRequired[list[Literal["slow_step", "errors", "budget"]] | None]
    warning: NotRequired[str | None]
    error: NotRequired[AgentsRecordError | None]
    provider: NotRequired[str | None]
    model: NotRequired[str | None]
    alternatives: NotRequired[list[AgentsModelPair] | None]


AgentsRecords: TypeAlias = list[AgentsRecord]


AgentRecords: TypeAlias = AgentsRecords


class AgentsRoute(TypedDict):
    """The model that a subagent uses from its next request."""

    provider: str
    model: str
    session_id: str
    effective: Literal["next_request"]


class AgentsTimelineItemsItem(TypedDict):
    at: int
    kind: Literal["started", "continued", "note", "message", "finished"]
    text: NotRequired[str]
    status: NotRequired[JSONValue]
    outcome: NotRequired[JSONValue]


class AgentsTimeline(TypedDict):
    session_id: str
    items: list[AgentsTimelineItemsItem]


AgentTimeline: TypeAlias = AgentsTimeline


class SessionAlert(TypedDict):
    title: str
    body: str


CouncilEntryId: TypeAlias = int


class CouncilSourceRefScope(TypedDict):
    turn: int
    iter: int
    next_form: int


class CouncilSourceRef(TypedDict):
    session_id: str
    scope: CouncilSourceRefScope | None
    operation_id: NotRequired[str]
    session_state_id: NotRequired[str]
    session_turn_soul_id: NotRequired[str]
    session_turn_state_id: NotRequired[str]
    session_turn_iteration_id: NotRequired[str]
    tool_call_id: NotRequired[str]


class CouncilReplyState(TypedDict):
    session_id: str
    state: Literal["pending", "delivered", "replied", "unavailable", "interrupted"]
    reply_entry_id: NotRequired[CouncilEntryId]


class CouncilEntryRecord(TypedDict):
    thread_id: CouncilEntryId
    group_id: str | None
    content: str
    author_session_id: str
    created_at: int
    source: Literal["host", "sdk"]
    ping: list[str]
    title: NotRequired[str]
    source_ref: NotRequired[CouncilSourceRef]
    reply_required: NotRequired[bool]
    reply_to: NotRequired[CouncilEntryId]
    replies: NotRequired[list[CouncilReplyState]]
    entry_id: CouncilEntryId


CouncilCursor: TypeAlias = int


class CouncilEntryPage(TypedDict):
    entries: list[CouncilEntryRecord]
    after: CouncilCursor
    has_more: bool


class CouncilMemberRecord(TypedDict):
    session_id: str
    title: str
    state: Literal["running", "queued", "held"]


CouncilMembers: TypeAlias = list[CouncilMemberRecord]


class CouncilThreadRecord(TypedDict):
    thread_id: CouncilEntryId
    title: str
    author_session_id: str
    created_at: int


class CouncilThreadPage(TypedDict):
    entries: list[CouncilThreadRecord]
    after: CouncilCursor
    has_more: bool


class TurnQueueStep(TypedDict):
    turn: TurnRecord | None


SessionEventType: TypeAlias = Literal[
    "block.activity",
    "block.output",
    "block.preview",
    "block.started",
    "content.block.completed",
    "content.block.delta",
    "content.block.started",
    "context.updated",
    "extensions.reloaded",
    "iteration.completed",
    "iteration.error",
    "notification",
    "provider.limits_changed",
    "provider.retry",
    "queue.paused",
    "queue.resumed",
    "session.agent_name_updated",
    "session.created",
    "session.deleted",
    "session.goal_updated",
    "session.model_updated",
    "session.status",
    "session.team_updated",
    "session.title_updated",
    "settings.changed",
    "speech.committed",
    "speech.invalidated",
    "subscription.ready",
    "turn.cancelled",
    "turn.completed",
    "turn.failed",
    "turn.input",
    "turn.progress",
    "turn.queued",
    "turn.queued.deleted",
    "turn.queued.drained",
    "turn.queued.sent",
    "turn.queued.updated",
    "turn.started",
    "view.close",
    "view.open",
    "view.patch",
    "voice.admission",
    "voice.input_overflow",
    "voice.playback",
    "voice.speech",
    "voice.utterance",
]


class SessionEvent(TypedDict):
    schema: int
    seq: int
    ts: int
    session_id: str
    type: SessionEventType


class SessionEvents(TypedDict):
    events: list[SessionEvent]


class SessionForkTurnsTurnsItem(TypedDict):
    turn_id: str
    request: NotRequired[str | None]
    request_kind: NotRequired[str]
    council: NotRequired[dict[str, JSONValue] | None]
    status: str
    created_at: int
    answer: NotRequired[str | None]
    is_forked: NotRequired[bool]


class SessionForkTurns(TypedDict):
    turns: list[SessionForkTurnsTurnsItem]


class SessionWrapped(TypedDict):
    session: SessionRecord


class SessionFsOpen(TypedDict):
    path: str
    is_open: bool


class FilePreviewVariant1(TypedDict):
    """One window of clipped lines around the line that the client named."""

    path: str
    kind: Literal["text"]
    line: int
    first_line: int
    lines: list[str]
    is_truncated: bool
    size_bytes: int


PreviewImageBytes: TypeAlias = int


class FilePreviewVariant2(TypedDict):
    """A picture that a browser draws, typed by its first bytes, not by its file name."""

    path: str
    kind: Literal["image"]
    media_type: Literal[
        "image/png",
        "image/jpeg",
        "image/gif",
        "image/webp",
        "image/avif",
    ]
    size_bytes: PreviewImageBytes
    data: str


class FilePreviewVariant3(TypedDict):
    """Another file with a NUL byte. The gateway does not read it."""

    path: str
    kind: Literal["binary"]
    size_bytes: int


class FilePreviewVariant4(TypedDict):
    """A recording or a clip that a browser plays, typed by its first bytes. A player
    reads it from content_url, which streams the file and answers Range requests.
    """

    path: str
    kind: Literal["media"]
    media_type: str
    size_bytes: int
    content_url: str
    expires_at: int


FilePreview: TypeAlias = (
    FilePreviewVariant1
    | FilePreviewVariant2
    | FilePreviewVariant3
    | FilePreviewVariant4
)


class AttachmentLink(TypedDict):
    """The answer of GET /v1/sessions/:sid/iterations/:iid/attachments/:idx/link for an
    attachment that keeps a reference to a file on the gateway machine. A frame, a
    download link or another app cannot send the gateway token, so they open
    content_url. The route answers 409 attachment-not-reference for an attachment
    that has its bytes in the session database, 404 attachment-not-found for an
    unknown attachment and 404 attachment-file-missing when the file is gone.
    """

    content_url: str
    expires_at: int


class AgentNote(TypedDict):
    """The page note that the model sees, or null when the note is blank."""

    id: str
    agent_note: str | None


class SessionModelPairVariant2(TypedDict):
    provider: str
    model: str


SessionModelPair: TypeAlias = None | SessionModelPairVariant2


class SessionModelRecord(TypedDict):
    model: SessionModelPair
    default: SessionModelPair


class QueueSendNowSkippedItem(TypedDict):
    turn_id: str
    reason: Literal["not_steerable", "held_by_cancel"]


class QueueSendNow(TypedDict):
    marked: list[str]
    skipped: list[QueueSendNowSkippedItem]


class SessionResources(TypedDict):
    resources: list[dict[str, JSONValue]]


class SessionResourceLogs(TypedDict):
    lines: list[str] | None


class SessionResourceStop(TypedDict):
    result: str
    id: NotRequired[str]
    message: NotRequired[str]


class SessionSeq(TypedDict):
    seq: int


class SessionSlashesCommandsItem(TypedDict):
    name: str
    doc: str


class SessionSlashes(TypedDict):
    commands: list[SessionSlashesCommandsItem]


class Keybinding(TypedDict):
    """One TUI key binding that an extension declares: the key after C-x runs a slash
    command, a built-in TUI verb or an extension function.
    """

    key: str
    command: NotRequired[str]
    action: NotRequired[str]
    run: NotRequired[JSONValue]
    label: NotRequired[str]
    group: NotRequired[str]
    extension: NotRequired[str]


class KeybindingList(TypedDict):
    """The TUI key bindings of the extensions of one session, in extension order, then
    declaration order.
    """

    keybindings: list[Keybinding]


class KeybindingRunResult(TypedDict):
    """The result of one run binding. The function returns it, or the gateway reports
    its failure.
    """

    status: Literal["ok", "error"]
    title: NotRequired[str]
    body: NotRequired[str]


class VoiceJob(TypedDict):
    id: str
    direction: NotRequired[str]
    engine: NotRequired[str]
    phase: NotRequired[str]
    progress: NotRequired[float]
    is_done: bool
    created_at: NotRequired[int]
    updated_at: NotRequired[int]


class Forgotten(TypedDict):
    is_forgotten: bool


SessionSuggestions: TypeAlias = list[JSONValue]


class TranscriptRecord(TypedDict):
    turns: list[dict[str, JSONValue]]
    total: int
    offset: int
    has_more: bool


class TurnList(TypedDict):
    turns: list[TurnRecord]
    queue_paused: NotRequired[None | QueuePausedMarker]


class TurnAttachments(TypedDict):
    attachments: list[dict[str, JSONValue]]


class TurnTrace(TypedDict):
    iterations: list[dict[str, JSONValue]]


class SessionUsageUsage(TypedDict):
    """The usage rollup, or null when the session has no turns."""

    cost_usd: float
    duration_ms: int
    first_turn_at: int
    fold_count: int
    input_cache_read_tokens: int
    input_cache_write_tokens: int
    input_regular_tokens: int
    input_tokens: int
    iteration_count: int
    last_turn_at: int
    output_reasoning_tokens: int
    output_tokens: int
    prompt_cache_estimated_sample_count: int
    prompt_cache_expired_count: int
    prompt_cache_rebuild_count: int
    prompt_cache_reusable_tokens: int
    prompt_cache_reused_tokens: int
    prompt_cache_sample_count: int
    reusable_prefix_estimated: bool
    tool_call_count: int
    tool_failure_count: int
    total_cost_usd: float
    turn_count: int


class SessionUsage(TypedDict):
    usage: SessionUsageUsage | None


class ViewRequests(TypedDict):
    requests: list[dict[str, JSONValue]]


ViewLog: TypeAlias = dict[str, JSONValue]


class VoiceState(TypedDict):
    voice_session_id: str
    capture_generation: int
    output_epoch: int
    capture: Literal["off", "muted", "live", "device_lost"]
    utterance: Literal["idle", "open", "provisional", "final", "admitted", "rejected"]
    agent: Literal[
        "idle",
        "queued",
        "running",
        "waiting_for_input",
        "cancelling",
        "terminal",
    ]
    output: Literal[
        "idle",
        "committed",
        "synthesizing",
        "playing",
        "interrupted",
        "drained",
        "muted",
    ]


class VoiceLiveLimits(TypedDict):
    """The bounds of a live voice session. Each client reads them from the gateway."""

    sample_rate: int
    frame_ms: int
    frame_samples: int
    header_bytes: int
    input_buffer_ms: int
    output_credit_ms: int
    preroll_ms: int
    max_utterance_ms: int
    endpoint_silence_ms: int
    min_speech_ms: int
    lease_ms: int
    heartbeat_ms: int
    ticket_ms: int
    max_control_bytes: int
    max_sessions: int
    max_pending_utterances: int
    max_pending_segments: int
    max_projection_chars: int
    max_speech_segment_chars: int
    max_speech_segments: int
    max_admissions: int
    max_retained_sessions: int
    retention_ms: int
    max_pending_inputs: int


class VoiceLiveCapabilitiesEnginesAsr(TypedDict):
    available: bool
    engine: NotRequired[str]
    sample_rate: NotRequired[int]
    max_samples: NotRequired[int]
    languages: NotRequired[list[str]]
    concurrency: NotRequired[int]
    abort_bound_ms: NotRequired[int | None]


class VoiceLiveCapabilitiesEnginesTts(TypedDict):
    available: bool
    engine: NotRequired[str]
    sample_rate: NotRequired[int]
    max_characters: NotRequired[int]
    concurrency: NotRequired[int]
    abort_bound_ms: NotRequired[int | None]


class VoiceLiveCapabilitiesEngines(TypedDict):
    available: bool
    reason: NotRequired[str]
    asr: NotRequired[VoiceLiveCapabilitiesEnginesAsr]
    tts: NotRequired[VoiceLiveCapabilitiesEnginesTts]


class VoiceLiveCapabilitiesProfile(TypedDict):
    language: str
    privacy: str
    concurrency: int
    clients: list[str]
    mobile_qualified: bool
    playback_precision: str


class VoiceLiveCapabilities(TypedDict):
    """The answer of the live voice capabilities route. reason says why voice is not
    available. enabled is always true.
    """

    enabled: bool
    available: bool
    reason: str | None
    limits: VoiceLiveLimits
    engines: VoiceLiveCapabilitiesEngines | None
    profile: VoiceLiveCapabilitiesProfile


class VoiceLiveStart(TypedDict):
    """The answer to a live voice start. ticket opens media_path one time."""

    voice_session_id: str
    client_id: str
    media_path: str
    ticket: str
    state: VoiceState
    limits: VoiceLiveLimits
    capabilities: VoiceLiveCapabilities


class VoiceLiveClosed(TypedDict):
    """The answer to a live voice close."""

    closed: JSONValue


class VoiceLiveReceipt(TypedDict):
    """The result of one spoken request. turn_id and status name the admitted turn.
    error says why there is no turn.
    """

    voice_session_id: str
    utterance_id: int
    idempotency_key: str
    text: str
    revision: int
    turn_id: NotRequired[str]
    status: NotRequired[str]
    error: NotRequired[str]


class VoiceLiveSnapshot(TypedDict):
    """The state of one live voice session and the result of each spoken request."""

    state: VoiceState
    receipts: list[VoiceLiveReceipt]


class SessionArchive(TypedDict):
    archived: list[str]
    working: list[str]
    dry_run: NotRequired[bool]


class SessionSearch(TypedDict):
    query: NotRequired[str]
    sessions: list[SessionRecord]
    total: NotRequired[int]
    limit: NotRequired[int]
    has_more: bool
    next_cursor: NotRequired[str | None]


ToggleScope: TypeAlias = Literal["global", "project", "group", "session"]


class SettingsProjectExtensions(TypedDict):
    """Project extensions of the context session. Only Machine settings show them."""

    target_id: str
    label: NotRequired[str]
    count: int
    names: list[str]


class SettingsExtension(TypedDict):
    name: str
    origin: Literal["built_in", "global", "project"]
    path: NotRequired[str]
    status: Literal["loaded", "stale", "failed"]
    error: NotRequired[str]


class SettingDecidedIn(TypedDict):
    """The closest wider scope that decides this inherited row. The row is read-only
    here.
    """

    scope: ToggleScope
    target_id: NotRequired[str]
    label: NotRequired[str]


SettingValue: TypeAlias = bool | str | float | list[JSONValue] | dict[str, JSONValue]


class SettingOverriddenBy(TypedDict):
    scope: ToggleScope
    enabled: NotRequired[bool]
    value: NotRequired[SettingValue]


class Setting(TypedDict):
    id: str
    label: str
    description: NotRequired[str]
    type: Literal["boolean", "enum", "string", "number", "array", "object"]
    scopes: NotRequired[list[ToggleScope]]
    scope: NotRequired[ToggleScope]
    source: NotRequired[Literal["default", "global", "project", "group", "session"]]
    is_override: NotRequired[bool]
    decided_in: NotRequired[SettingDecidedIn]
    overridden_by: NotRequired[SettingOverriddenBy]
    enabled: NotRequired[bool]
    value: NotRequired[SettingValue]
    choices: NotRequired[list[str]]
    is_experimental: NotRequired[bool]
    multiline: NotRequired[bool]
    editor: NotRequired[
        Literal[
            "switch",
            "select",
            "text",
            "secret",
            "multiline",
            "number",
            "paths",
            "filesystem",
            "network",
            "list",
            "tools",
            "presets",
            "models",
        ]
    ]
    is_set: NotRequired[bool]
    suggestions: NotRequired[list[str]]
    schema: NotRequired[str]
    applies: NotRequired[
        Literal[
            "immediate",
            "next_call",
            "next_turn",
            "reload",
            "restart",
        ]
    ]
    own_value: NotRequired[JSONValue]
    inherited_value: NotRequired[SettingValue]
    inherited_source: NotRequired[
        Literal[
            "default",
            "global",
            "project",
            "group",
            "session",
        ]
    ]
    max_length: NotRequired[int]
    is_optional: NotRequired[bool]
    lock_reason: NotRequired[str]
    inheritance: NotRequired[JSONValue]
    choice_labels: NotRequired[dict[str, JSONValue]]
    change_error: NotRequired[str]
    parent: NotRequired[str]
    children: NotRequired[list[Setting]]
    editor_options: NotRequired[dict[str, JSONValue]]


class SettingsGroupsItem(TypedDict):
    id: str
    title: str
    extension: NotRequired[SettingsExtension]
    toggles: list[Setting]


class Settings(TypedDict):
    scope: ToggleScope
    target_id: str | None
    label: NotRequired[str]
    revision: str
    lineage: NotRequired[list[dict[str, JSONValue]]]
    project_extensions: NotRequired[SettingsProjectExtensions]
    groups: list[SettingsGroupsItem]


class SettingPreviewResult(TypedDict):
    is_played: bool


class ExtensionReloadScopesItem(TypedDict):
    scope: Literal["global", "project"]
    dirs: list[str]
    loaded: int
    failed: int
    extensions: list[str]


class ExtensionReload(TypedDict):
    loaded: int
    reload_id: str
    failed: int
    scopes: list[ExtensionReloadScopesItem]


class VoiceModel(TypedDict):
    status: str
    engine: str
    progress: NotRequired[float]
    phase: NotRequired[str]
    error: NotRequired[str | None]


class SpeechVoicesEngine(TypedDict):
    id: str
    label: NotRequired[str]


class SpeechVoicesVoicesItem(TypedDict):
    id: str
    label: NotRequired[str]


class SpeechVoices(TypedDict):
    engine: SpeechVoicesEngine
    voices: list[SpeechVoicesVoicesItem]


class SpeechVoice(TypedDict):
    voice: dict[str, JSONValue]


class DecisionModelsModelsItem(TypedDict):
    model_ref: str


class DecisionModels(TypedDict):
    models: list[DecisionModelsModelsItem]


class DecisionModel(TypedDict):
    model_ref: str
    revision: NotRequired[str | None]


class DecisionAlias(TypedDict):
    alias: str
    model_ref: str


class DecisionJob(TypedDict):
    job_id: str
    status: Literal[
        "running",
        "registering",
        "cancelling",
        "completed",
        "failed",
        "cancelled",
        "deleted",
    ]
    stage: NotRequired[str]
    model_id: NotRequired[str]


class DecisionAnswerRouting(TypedDict):
    model: str
    model_ref: str


class DecisionAnswer(TypedDict):
    model: str
    routing: DecisionAnswerRouting
    answers: dict[str, JSONValue]
    usage: NotRequired[dict[str, JSONValue]]


class WebLaunch(TypedDict):
    """A one-time code that connects a browser to this gateway. `vis-agent web` puts it
    in the URL fragment.
    """

    code: str
    expires_at_ms: int
    gateway_id: str


class WebLaunchGrant(TypedDict):
    """What a redeemed launch code gives the browser: the gateway it reached and the
    token for its API.
    """

    gateway_id: str
    token: str
    require_token: bool
    machine_name: NotRequired[ConfigMachineName]


WebBrandHref: TypeAlias = str


ViewAppIcon: TypeAlias = Literal[
    "app",
    "review",
    "code",
    "terminal",
    "files",
    "search",
    "git",
    "branch",
    "checklist",
    "chart",
    "dashboard",
    "activity",
    "calendar",
    "clock",
    "mail",
    "chat",
    "users",
    "globe",
    "database",
    "cloud",
    "server",
    "shield",
    "lock",
    "key",
    "settings",
    "tools",
    "rocket",
    "flask",
    "music",
    "image",
    "book",
    "lightning",
]


class WebBrandLink(TypedDict):
    """One icon link of the header. It opens in a new tab."""

    icon: ViewAppIcon
    label: str
    href: WebBrandHref


class WebBrandTheme(TypedDict):
    """The only theme of the web app. While a brand has one, the app shows no theme
    choice. `base` names a theme that the app ships. `colors` replaces CSS variables
    of that theme.
    """

    label: NotRequired[str]
    base: str
    colors: NotRequired[dict[str, JSONValue]]


class WebBrand(TypedDict):
    """The white label of the web app that this gateway serves. An absent key keeps the
    Vis default. A GET needs no token, because the app shows the brand before it
    connects.
    """

    name: NotRequired[str]
    logo: NotRequired[str]
    href: NotRequired[WebBrandHref]
    links: NotRequired[list[WebBrandLink]]
    theme: NotRequired[WebBrandTheme]


AutomationsAutomationId: TypeAlias = str


AutomationsName: TypeAlias = JSONValue


class AutomationsCronTrigger(TypedDict):
    """Five cron fields or a macro such as @daily. Without a time zone, the gateway
    uses its system time zone.
    """

    kind: Literal["cron"]
    expression: str
    timezone: NotRequired[str]


class AutomationsEveryTrigger(TypedDict):
    """A fixed interval from the creation time of the automation."""

    kind: Literal["every"]
    seconds: int


AutomationsTimestamp: TypeAlias = int


class AutomationsOnceTrigger(TypedDict):
    """One run at a time. A run that the gateway missed starts late, but not more than
    one day late.
    """

    kind: Literal["once"]
    at: AutomationsTimestamp


AutomationsWebhookFilter = TypedDict(
    "AutomationsWebhookFilter",
    {
        "field": "str",
        "equals": "NotRequired[str | float | bool | None]",
        "contains": "NotRequired[str]",
        "in": "NotRequired[list[str | float | bool]]",
    },
)


class AutomationsWebhookTrigger(TypedDict):
    """A signed POST to /v1/hooks/{automation_id}. github checks X-Hub-Signature-256.
    standard checks webhook-id, webhook-timestamp and webhook-signature. generic
    checks X-Webhook-Timestamp and X-Webhook-Signature-V2. token compares X-Gitlab-
    Token, X-Webhook-Token or a bearer token. An empty events list accepts every
    event.
    """

    kind: Literal["webhook"]
    signature: Literal["github", "standard", "generic", "token"]
    events: NotRequired[list[str]]
    filters: NotRequired[list[AutomationsWebhookFilter]]


AutomationsTrigger: TypeAlias = (
    AutomationsCronTrigger
    | AutomationsEveryTrigger
    | AutomationsOnceTrigger
    | AutomationsWebhookTrigger
)


AutomationsPrompt: TypeAlias = JSONValue


class AutomationsSessionTarget(TypedDict):
    """Run in one existing session."""

    mode: Literal["session"]
    session_id: str


class AutomationsNewTarget(TypedDict):
    """Start a new session for each run and keep it."""

    mode: Literal["new"]
    root: NotRequired[str]
    group_id: NotRequired[str]


class AutomationsTemporaryTarget(TypedDict):
    """Start a new session for each run and delete it after the run. The run keeps the
    answer.
    """

    mode: Literal["temporary"]
    root: NotRequired[str]


class AutomationsPersistentTarget(TypedDict):
    """Run in one session that Vis starts at the first run and keeps for the next runs.
    When that session is gone, the next run starts a new one.
    """

    mode: Literal["persistent"]
    root: NotRequired[str]


AutomationsTarget: TypeAlias = (
    AutomationsSessionTarget
    | AutomationsNewTarget
    | AutomationsTemporaryTarget
    | AutomationsPersistentTarget
)


AutomationsRunEventType: TypeAlias = Literal[
    "run.completed",
    "run.failed",
    "run.cancelled",
    "run.skipped",
    "run.unknown",
]


class AutomationsCallback(TypedDict):
    """POST each run event to this URL, signed as Standard Webhooks with the callback
    secret. Without events, all events are sent.
    """

    url: str
    events: NotRequired[list[AutomationsRunEventType]]


class AutomationsDelivery(TypedDict):
    """Push sends a phone alert for each run. An answer that starts with [SILENT]
    suppresses the alert and the callback for a completed run.
    """

    push: NotRequired[bool]
    callback: NotRequired[AutomationsCallback | None]


class AutomationsModelPin(TypedDict):
    provider: NotRequired[str]
    model: str


class AutomationsWebhookEndpoint(TypedDict):
    """The gateway path that receives webhook POSTs for this automation."""

    path: str


class AutomationsSecretsState(TypedDict):
    webhook: bool
    callback: bool


AutomationsExtensionAutomationKey: TypeAlias = str


class AutomationsAutomationOwner(TypedDict):
    """The extension that declares this automation. A run calls the run callback of the
    extension and no model. The extension owns the definition: a client can only
    pause, resume, run or delete it. The extension, the key and the root identify
    one automation.
    """

    extension: JSONValue
    key: AutomationsExtensionAutomationKey
    root: str


AutomationsRunTriggerKind: TypeAlias = Literal[
    "cron",
    "every",
    "once",
    "webhook",
    "manual",
]


AutomationsRunStatus: TypeAlias = Literal[
    "queued",
    "running",
    "completed",
    "failed",
    "cancelled",
    "skipped",
    "unknown",
]


class AutomationsRun(TypedDict):
    """One claimed trigger. A run that was queued or running when its gateway stopped
    becomes unknown and never runs again.
    """

    id: AutomationsAutomationId
    automation_id: AutomationsAutomationId
    automation_name: AutomationsName
    trigger: AutomationsRunTriggerKind
    status: AutomationsRunStatus
    reason: str | None
    scheduled_at: AutomationsTimestamp | None
    created_at: AutomationsTimestamp
    started_at: AutomationsTimestamp | None
    finished_at: AutomationsTimestamp | None
    session_id: str | None
    turn_id: str | None
    answer: str | None
    error: str | None
    is_silent: bool


class AutomationsAutomation(TypedDict):
    id: AutomationsAutomationId
    name: AutomationsName
    enabled: bool
    triggers: list[AutomationsTrigger]
    prompt: AutomationsPrompt
    target: AutomationsTarget
    delivery: AutomationsDelivery
    model: AutomationsModelPin | None
    deliver_only: bool
    created_at: AutomationsTimestamp
    updated_at: AutomationsTimestamp
    next_run_at: AutomationsTimestamp | None
    webhook: AutomationsWebhookEndpoint | None
    secrets: AutomationsSecretsState
    owner: AutomationsAutomationOwner | None
    last_run: AutomationsRun | None


class AutomationsAutomationList(TypedDict):
    automations: list[AutomationsAutomation]


AutomationsList: TypeAlias = AutomationsAutomationList


class AutomationsRunList(TypedDict):
    runs: list[AutomationsRun]


class AutomationsAutomationDeleted(TypedDict):
    id: AutomationsAutomationId
    is_deleted: JSONValue


AutomationsDeleted: TypeAlias = AutomationsAutomationDeleted


class AutomationsSecretCreated(TypedDict):
    kind: Literal["webhook", "callback"]
    secret: str


class EvalsAgreement(TypedDict):
    """The agreement of automatic results with human labels, with Cohen kappa,
    Krippendorff alpha and advice.
    """

    labelled: int
    agreement_percent: float | None
    kappa: float | None
    alpha: float | None
    disagreements: list[str]
    advice: str


class EvalsRouteTest(TypedDict):
    """The exact McNemar test of the paired cases of one route."""

    route: str
    paired_cases: int
    worse: int
    better: int
    p_value: float
    q_value: float


class EvalsCaseChange(TypedDict):
    """One case whose pass rate changed, as passed/runs on each side."""

    case: str
    baseline: str
    candidate: str


class EvalsCaseComparison(TypedDict):
    """A paired comparison of the cases that both sides ran. Differences are in points."""

    paired_cases: int
    baseline_rate_percent: float
    candidate_rate_percent: float
    difference_points: float
    difference_ci95: list[float]
    verdict: Literal[
        "better",
        "worse",
        "no measurable difference",
        "equivalent",
        "not worse",
        "not better",
        "inconclusive",
    ]
    detectable_change_points: float
    p_value: float
    routes: list[EvalsRouteTest]
    worse: list[EvalsCaseChange]
    better: list[EvalsCaseChange]
    unpaired: int
    caveats: list[str]
    margin_points: float | None


class EvalsTaskNote(TypedDict):
    """One task of a finding. budget_used is the used share of the agent time limit."""

    task: str
    outcome: str
    checks: str
    pass_ratio: float | None
    budget_used: float | None
    minutes: float | None
    iterations: int | None


class EvalsFinding(TypedDict):
    """One failure cause with its share of the scored tasks and its tasks."""

    kind: str
    title: str
    task_count: int
    share: float
    detail: str
    tasks: list[EvalsTaskNote]


class EvalsDiagnosis(TypedDict):
    """The failure causes of one round and the check of the human labels."""

    scored_tasks: int
    solved_tasks: int
    findings: list[EvalsFinding]
    estimate: str
    label_check: EvalsAgreement


class EvalsLeaderboardRow(TypedDict):
    """One row of a Harbor Hub leaderboard."""

    rank: int
    agent: str
    model: str
    reasoning_effort: str
    accuracy_percent: float
    ci95_half_width: float | None
    trials: int
    successes: int
    total_cost_usd: float | None
    cost_per_trial_usd: float | None
    cost_per_success_usd: float | None
    tokens_per_trial: int | None
    avg_trial_minutes: float | None
    date: str


class EvalsVisStanding(TypedDict):
    """The place of the local Vis round on the leaderboard."""

    model: str
    round_name: str | None
    scored_tasks: int
    total_tasks: int
    attempts_per_task: int
    pass_rate_percent: float
    pass_rate_ci95: list[float] | None
    strict_pass_rate_percent: float
    strict_pass_rate_ci95: list[float] | None
    rank_by_pass_rate: int
    rank_range: list[int] | None
    rank_by_strict_rate: int
    strict_rank_range: list[int] | None
    cost_per_attempt_usd: float | None
    cost_per_solved_usd: float | None
    tokens_per_attempt: int | None
    avg_attempt_minutes: float | None
    caveats: list[str]


class EvalsLeaderboard(TypedDict):
    """A Harbor Hub leaderboard. vis is null without a local result for its dataset."""

    package: str
    name: str
    title: str
    rows: list[EvalsLeaderboardRow]
    vis: EvalsVisStanding | None


class EvalsOutcomeCount(TypedDict):
    """The tasks of one outcome, such as solved or failed_tests."""

    outcome: str
    count: int
    tasks: list[str]


class EvalsBenchReport(TypedDict):
    """The score of one benchmark round. Rates are in percent. Intervals are 95%
    intervals.
    """

    dataset: str
    round_name: str | None
    model: str
    total_tasks: int
    scored_tasks: int
    solved_tasks: int
    pass_rate_percent: float
    pass_rate_ci95: list[float] | None
    strict_pass_rate_percent: float
    strict_pass_rate_ci95: list[float] | None
    attempts_per_task: int
    scored_attempts: int
    solved_attempts: int
    attempt_pass_rate_percent: float
    attempt_pass_rate_ci95: list[float] | None
    detectable_change_points: float | None
    pass_at_k_percent: float | None
    pass_all_k_percent: float | None
    unstable_tasks: list[str]
    outcomes: list[EvalsOutcomeCount]
    unscored_tasks: list[str]
    gpu_tasks: list[str]
    unattempted_tasks: int
    attempts: int
    model_attempts: int
    agent_hours: float
    input_tokens: int
    cached_input_percent: float
    output_tokens: int
    estimated_metered_cost_usd: float
    cost_per_solved_usd: float | None
    repeated_final_errors: list[str]


class AutomationsWebhookResult(TypedDict):
    status: Literal["accepted", "duplicate", "ignored"]
    run_id: AutomationsAutomationId | None
    reason: str | None


class BlockActivityData(TypedDict):
    """The `data` of an event of type `block.activity`."""

    position: NotRequired[int]
    created_at: NotRequired[int]
    form_index: int
    iteration: NotRequired[int]
    schema: int
    ts: int


class BlockOutputData(TypedDict):
    """The `data` of an event of type `block.output`."""

    position: NotRequired[int]
    created_at: NotRequired[int]
    form_index: int
    iteration: NotRequired[int]
    code: NotRequired[str | None]
    display_code: NotRequired[str]
    display_language: NotRequired[str]
    comment: NotRequired[str]
    scope: NotRequired[str]
    started_at_ms: NotRequired[int]
    op: NotRequired[str]
    render_segments: NotRequired[JSONValue]
    result_kind: NotRequired[str]
    result_detail: NotRequired[JSONValue]
    tag: NotRequired[JSONValue]
    tool_call_id: NotRequired[str]
    llm_model: NotRequired[str]
    is_timeout: NotRequired[bool]
    is_repaired: NotRequired[bool]
    silent: NotRequired[bool]
    stdout: NotRequired[str]
    error: NotRequired[str]
    duration_ms: NotRequired[int]
    schema: int
    ts: int


class BlockPreviewData(TypedDict):
    """The `data` of an event of type `block.preview`."""

    position: NotRequired[int]
    created_at: NotRequired[int]
    form_index: int
    iteration: NotRequired[int]
    code: NotRequired[str | None]
    display_code: NotRequired[str]
    display_language: NotRequired[str]
    comment: NotRequired[str]
    scope: NotRequired[str]
    started_at_ms: NotRequired[int]
    op: NotRequired[str]
    render_segments: NotRequired[JSONValue]
    result_kind: NotRequired[str]
    result_detail: NotRequired[JSONValue]
    tag: NotRequired[JSONValue]
    tool_call_id: NotRequired[str]
    llm_model: NotRequired[str]
    is_timeout: NotRequired[bool]
    is_repaired: NotRequired[bool]
    schema: int
    ts: int


class BlockStartedData(TypedDict):
    """The `data` of an event of type `block.started`."""

    position: NotRequired[int]
    created_at: NotRequired[int]
    form_index: int
    iteration: NotRequired[int]
    code: NotRequired[str | None]
    display_code: NotRequired[str]
    display_language: NotRequired[str]
    comment: NotRequired[str]
    scope: NotRequired[str]
    started_at_ms: NotRequired[int]
    op: NotRequired[str]
    render_segments: NotRequired[JSONValue]
    result_kind: NotRequired[str]
    result_detail: NotRequired[JSONValue]
    tag: NotRequired[JSONValue]
    tool_call_id: NotRequired[str]
    llm_model: NotRequired[str]
    is_timeout: NotRequired[bool]
    is_repaired: NotRequired[bool]
    schema: int
    ts: int


class ContentBlockCompletedData(TypedDict):
    """The `data` of an event of type `content.block.completed`."""

    position: NotRequired[int]
    created_at: NotRequired[int]
    block_id: str
    iteration: NotRequired[int]
    schema: int
    ts: int


class ContentBlockDeltaData(TypedDict):
    """The `data` of an event of type `content.block.delta`."""

    position: NotRequired[int]
    created_at: NotRequired[int]
    block_id: str
    field: Literal["text", "markdown"]
    text: str
    offset: int
    iteration: NotRequired[int]
    schema: int
    ts: int


VoiceSpeechText: TypeAlias = str


class ContentBlockVariant1(TypedDict):
    id: CommonNonBlankString
    type: Literal["conversation"]
    phase: Literal["progress", "answer"]
    text: VoiceSpeechText


class ContentBlockVariant2(TypedDict):
    id: CommonNonBlankString
    type: Literal["visual"]
    markdown: str


class ContentBlockVariant3(TypedDict):
    id: CommonNonBlankString
    type: Literal["silence"]
    reason: Literal["user_requested"]


class ContentBlockVariant4(TypedDict):
    id: CommonNonBlankString
    type: Literal["prose"]
    markdown: str


class ContentBlockVariant5(TypedDict):
    id: CommonNonBlankString
    type: Literal["speech"]
    text: CommonNonBlankString


class ContentBlockVariant6(TypedDict):
    id: CommonNonBlankString
    type: Literal["code"]
    text: str
    language: NotRequired[CommonNonBlankString]


class ContentBlockVariant7(TypedDict):
    id: CommonNonBlankString
    type: Literal["tool"]
    tool: CommonNonBlankString
    status: Literal["cancelled", "completed", "failed", "pending", "running"]


class ContentBlockVariant8(TypedDict):
    id: CommonNonBlankString
    type: Literal["reasoning"]
    text: str
    visibility: NotRequired[Literal["private", "visible"]]


CommonSnakeCaseId: TypeAlias = str


class ContentBlockVariant9(TypedDict):
    id: CommonNonBlankString
    type: Literal["error"]
    code: CommonSnakeCaseId
    message: CommonNonBlankString
    retryable: NotRequired[bool]


class ContentBlockVariant10(TypedDict):
    id: CommonNonBlankString
    type: Literal["attachment"]
    attachment_id: CommonNonBlankString
    name: CommonNonBlankString
    media_type: CommonNonBlankString


class ContentBlockVariant11(TypedDict):
    id: CommonNonBlankString
    type: Literal["notice"]
    code: CommonSnakeCaseId
    message: CommonNonBlankString


ContentBlock: TypeAlias = (
    ContentBlockVariant1
    | ContentBlockVariant2
    | ContentBlockVariant3
    | ContentBlockVariant4
    | ContentBlockVariant5
    | ContentBlockVariant6
    | ContentBlockVariant7
    | ContentBlockVariant8
    | ContentBlockVariant9
    | ContentBlockVariant10
    | ContentBlockVariant11
)


class ContentBlockStartedData(TypedDict):
    """The `data` of an event of type `content.block.started`."""

    position: NotRequired[int]
    created_at: NotRequired[int]
    block: ContentBlock
    iteration: NotRequired[int]
    schema: int
    ts: int


class ContextUpdatedData(TypedDict):
    """The `data` of an event of type `context.updated`."""

    utilization: dict[str, JSONValue]
    schema: int
    ts: int


class ExtensionsReloadedData(TypedDict):
    """The `data` of an event of type `extensions.reloaded`."""

    reload_id: str
    schema: int
    ts: int


ContentContent: TypeAlias = list[ContentBlock]


class IterationCompletedData(TypedDict):
    """The `data` of an event of type `iteration.completed`."""

    position: NotRequired[int]
    created_at: NotRequired[int]
    done: NotRequired[bool]
    thinking: NotRequired[str | None]
    assistant_prose: NotRequired[str]
    attachments: NotRequired[list[dict[str, JSONValue]]]
    iteration: NotRequired[int]
    presentation_content: NotRequired[ContentContent]
    schema: int
    ts: int


class IterationErrorData(TypedDict):
    """The `data` of an event of type `iteration.error`."""

    position: NotRequired[int]
    created_at: NotRequired[int]
    error: NotRequired[str | None]
    thinking: NotRequired[str | None]
    error_data: NotRequired[dict[str, JSONValue]]
    provider_error_data: NotRequired[dict[str, JSONValue]]
    iteration: NotRequired[int]
    schema: int
    ts: int


class NotificationData(TypedDict):
    """The `data` of an event of type `notification`."""

    text: str
    level: Literal["info", "success", "warn", "error"]
    extension: NotRequired[str]
    notice_id: NotRequired[str]
    title: NotRequired[str]
    duration_ms: NotRequired[int]
    scope: Literal["session", "global"]
    schema: int
    ts: int


class ProviderLimitsChangedData(TypedDict):
    """The `data` of an event of type `provider.limits_changed`."""

    provider_id: str
    schema: int
    ts: int


class ProviderRetryData(TypedDict):
    """The `data` of an event of type `provider.retry`."""

    position: NotRequired[int]
    created_at: NotRequired[int]
    attempt: NotRequired[int]
    max_retries: NotRequired[int]
    delay_ms: NotRequired[int]
    error: NotRequired[dict[str, JSONValue]]
    event: NotRequired[dict[str, JSONValue]]
    iteration: NotRequired[int]
    schema: int
    ts: int


class QueuePausedData(TypedDict):
    """The `data` of an event of type `queue.paused`."""

    reason: str
    held: int
    schema: int
    ts: int


class QueueResumedData(TypedDict):
    """The `data` of an event of type `queue.resumed`."""

    is_auto: bool
    schema: int
    ts: int


class SessionAgentNameUpdatedData(TypedDict):
    """The `data` of an event of type `session.agent_name_updated`."""

    agent_name: str | None
    schema: int
    ts: int


class SessionCreatedData(TypedDict):
    """The `data` of an event of type `session.created`."""

    root: NotRequired[str]
    project_id: NotRequired[str]
    group_id: NotRequired[str]
    schema: int
    ts: int


class SessionDeletedData(TypedDict):
    """The `data` of an event of type `session.deleted`."""

    schema: int
    ts: int


class SessionGoalUpdatedData(TypedDict):
    """The `data` of an event of type `session.goal_updated`."""

    goal: None | SessionGoal
    schema: int
    ts: int


class SessionModelUpdatedData(TypedDict):
    """The `data` of an event of type `session.model_updated`."""

    provider: str | None
    model: str | None
    reason: NotRequired[str]
    schema: int
    ts: int


class SessionStatusData(TypedDict):
    """The `data` of an event of type `session.status`."""

    is_live: bool
    is_awaiting_input: bool
    awaiting_input_count: int
    current_turn_id: str | None
    schema: int
    ts: int


class TeamChange(TypedDict):
    """The change of one team row since the last row that the gateway pushed for that
    child. Each key other than session_id has a value of the agent row shape, or
    null when the key went away. Null and a missing key mean the same: the gateway
    sends a key that becomes null or goes away once as null, and a key that is null
    or missing in both rows not at all. A client removes a key with a null value
    from the row. A lifecycle or request change never comes as a change: it comes as
    a whole row in agents.
    """

    session_id: JSONValue
    id: NotRequired[JSONValue]
    parent_id: NotRequired[JSONValue]
    leader_id: NotRequired[JSONValue]
    team_id: NotRequired[JSONValue]
    task: NotRequired[JSONValue]
    title: NotRequired[JSONValue]
    role: NotRequired[JSONValue]
    status: NotRequired[JSONValue]
    after: NotRequired[list[dict[str, JSONValue]] | None]
    iteration_budget: NotRequired[JSONValue]
    iterations_used: NotRequired[JSONValue]
    provider: NotRequired[JSONValue]
    model: NotRequired[JSONValue]
    fallback_model: NotRequired[JSONValue]
    routing_locked: NotRequired[JSONValue]
    pending_input: NotRequired[JSONValue]
    pending_input_count: NotRequired[JSONValue]
    question: NotRequired[JSONValue]
    question_entry_id: NotRequired[JSONValue]
    budget_request: NotRequired[dict[str, JSONValue] | None]
    files_request: NotRequired[dict[str, JSONValue] | None]
    activity: NotRequired[JSONValue]
    note: NotRequired[JSONValue]
    result: NotRequired[JSONValue]
    outcome: NotRequired[JSONValue]
    usage: NotRequired[JSONValue]
    files: NotRequired[JSONValue]
    created_at: NotRequired[JSONValue]
    resumed_at: NotRequired[JSONValue]
    finished_at: NotRequired[JSONValue]
    runs: NotRequired[list[dict[str, JSONValue]] | None]


class SessionTeamUpdatedData(TypedDict):
    """The `data` of an event of type `session.team_updated`."""

    team: SessionTeam
    child_id: NotRequired[str]
    agents: list[AgentsAgent]
    changes: NotRequired[list[TeamChange]]
    schema: int
    ts: int


class SessionTitleUpdatedData(TypedDict):
    """The `data` of an event of type `session.title_updated`."""

    title: str
    titled_session_id: NotRequired[str]
    schema: int
    ts: int


class SettingsTarget(TypedDict):
    scope: ToggleScope
    target_id: NotRequired[str]


class SettingsChangedData(TypedDict):
    """The `data` of an event of type `settings.changed`."""

    target: SettingsTarget
    id: NotRequired[str]
    schema: int
    ts: int


class SpeechCommittedData(TypedDict):
    """The `data` of an event of type `speech.committed`."""

    position: NotRequired[int]
    created_at: NotRequired[int]
    iteration: int
    segment_index: int
    source: str
    block_id: str
    phase: NotRequired[str | None]
    text: str
    source_start: int
    source_end: int
    schema: int
    ts: int


class SpeechInvalidatedData(TypedDict):
    """The `data` of an event of type `speech.invalidated`."""

    position: NotRequired[int]
    created_at: NotRequired[int]
    reason: str
    schema: int
    ts: int


class SubscriptionReadyData(TypedDict):
    """The `data` of an event of type `subscription.ready`."""

    current_turn_id: str | None
    is_live: bool
    server_time_ms: int
    goal: None | SessionGoal
    agent_name: NotRequired[str]
    latest_iteration: NotRequired[int]
    agents: NotRequired[list[AgentsAgent]]


class TurnCancelledData(TypedDict):
    """The `data` of an event of type `turn.cancelled`."""

    position: NotRequired[int]
    created_at: NotRequired[int]
    status: str
    request_kind: NotRequired[str]
    subagent: NotRequired[bool]
    idempotency_key: NotRequired[str]
    content: NotRequired[list[ContentBlock]]
    error: NotRequired[str]
    schema: int
    ts: int


class TurnCompletedData(TypedDict):
    """The `data` of an event of type `turn.completed`."""

    position: NotRequired[int]
    created_at: NotRequired[int]
    status: str
    request_kind: NotRequired[str]
    subagent: NotRequired[bool]
    idempotency_key: NotRequired[str]
    content: NotRequired[list[ContentBlock]]
    error: NotRequired[str]
    schema: int
    ts: int


class TurnFailedData(TypedDict):
    """The `data` of an event of type `turn.failed`."""

    position: NotRequired[int]
    created_at: NotRequired[int]
    status: str
    request_kind: NotRequired[str]
    subagent: NotRequired[bool]
    idempotency_key: NotRequired[str]
    content: NotRequired[list[ContentBlock]]
    error: NotRequired[str]
    schema: int
    ts: int


class TurnInputDataMessagesItem(TypedDict):
    queued_turn_id: str
    request: str
    display_request: NotRequired[str]
    attachment_previews: NotRequired[list[dict[str, JSONValue]]]


class TurnInputData(TypedDict):
    """The `data` of an event of type `turn.input`."""

    position: NotRequired[int]
    created_at: NotRequired[int]
    iteration: NotRequired[int]
    messages: list[TurnInputDataMessagesItem]
    schema: int
    ts: int


class TurnProgressData(TypedDict):
    """The `data` of an event of type `turn.progress`."""

    position: NotRequired[int]
    created_at: NotRequired[int]
    progress: str
    iteration: NotRequired[int]
    reason: NotRequired[str]
    model: NotRequired[str]
    cmd: NotRequired[str]
    op: NotRequired[str]
    phrase: NotRequired[str]
    label: NotRequired[str]
    busy_ms: NotRequired[int]
    deadline_in_ms: NotRequired[int]
    cpu_percent: NotRequired[float]
    silent_ms: NotRequired[int]
    connection: NotRequired[Literal["alive", "silent"]]
    awaiting_output: NotRequired[bool]
    deadline_action: NotRequired[Literal["retry", "timeout"]]
    schema: int
    ts: int


class TurnQueuedData(TypedDict):
    """The `data` of an event of type `turn.queued`."""

    position: NotRequired[int]
    created_at: NotRequired[int]
    request: str
    deliver: Literal["turn_end", "next_iteration"]
    idempotency_key: NotRequired[str]
    request_preview: NotRequired[str]
    attachment_previews: NotRequired[list[dict[str, JSONValue]]]
    display_request: NotRequired[str]
    schema: int
    ts: int


class TurnQueuedDeletedData(TypedDict):
    """The `data` of an event of type `turn.queued.deleted`."""

    position: NotRequired[int]
    created_at: NotRequired[int]
    request: NotRequired[str]
    reason: NotRequired[str]
    attachments: NotRequired[list[dict[str, JSONValue]]]
    schema: int
    ts: int


class TurnQueuedDrainedData(TypedDict):
    """The `data` of an event of type `turn.queued.drained`."""

    position: NotRequired[int]
    created_at: NotRequired[int]
    schema: int
    ts: int


class TurnQueuedSentData(TypedDict):
    """The `data` of an event of type `turn.queued.sent`."""

    position: NotRequired[int]
    created_at: NotRequired[int]
    into_turn_id: str
    iteration: NotRequired[int]
    schema: int
    ts: int


class TurnQueuedUpdatedData(TypedDict):
    """The `data` of an event of type `turn.queued.updated`."""

    position: NotRequired[int]
    created_at: NotRequired[int]
    request: str
    deliver: Literal["turn_end", "next_iteration"]
    request_preview: NotRequired[str]
    attachment_previews: NotRequired[list[dict[str, JSONValue]]]
    schema: int
    ts: int


class TurnStartedData(TypedDict):
    """The `data` of an event of type `turn.started`."""

    position: NotRequired[int]
    created_at: NotRequired[int]
    request: NotRequired[str | None]
    display_request: NotRequired[str | None]
    request_kind: NotRequired[str]
    council: NotRequired[dict[str, JSONValue] | None]
    started_at: NotRequired[int]
    is_queued: NotRequired[bool]
    resumed: NotRequired[bool]
    idempotency_key: NotRequired[str]
    attachments: NotRequired[list[dict[str, JSONValue]]]
    schema: int
    ts: int


class ViewCloseData(TypedDict):
    """The `data` of an event of type `view.close`."""

    schema: int
    ts: int


class ViewOpenData(TypedDict):
    """The `data` of an event of type `view.open`."""

    schema: int
    ts: int


class ViewPatchData(TypedDict):
    """The `data` of an event of type `view.patch`."""

    schema: int
    ts: int


class VoiceAdmissionData(TypedDict):
    """The `data` of an event of type `voice.admission`."""

    voice_session_id: str
    utterance_id: int
    idempotency_key: str
    text: str
    revision: int
    status: NotRequired[str]
    error: NotRequired[str]
    position: NotRequired[int]
    created_at: NotRequired[int]
    schema: int
    ts: int


class VoiceInputOverflowData(TypedDict):
    """The `data` of an event of type `voice.input_overflow`."""

    voice_session_id: str
    capture_generation: int
    operation: str
    active_operation: str | None
    active_duration_ms: int
    queue_depth: int
    buffered_ms: int
    audio_frames: int
    schema: int
    ts: int


class VoicePlaybackData(TypedDict):
    """The `data` of an event of type `voice.playback`."""

    voice_session_id: str
    segment_id: int
    position: NotRequired[int]
    created_at: NotRequired[int]
    output_epoch: int
    generated: int
    played: int
    reason: str
    schema: int
    ts: int


class VoiceSpeechData(TypedDict):
    """The `data` of an event of type `voice.speech`."""

    id: int
    epoch: int
    position: NotRequired[int]
    created_at: NotRequired[int]
    source: str
    generated: int
    sent: int
    played: int
    finished: bool
    schema: int
    ts: int


class VoiceUtteranceData(TypedDict):
    """The `data` of an event of type `voice.utterance`."""

    voice_session_id: str
    utterance_id: int
    idempotency_key: str
    text: str
    revision: int
    schema: int
    ts: int


RESPONSE_TYPES: dict[str, str] = {
    "DELETE /v1/automations/:automation-id": "AutomationsDeleted",
    "DELETE /v1/clients/:cid": "ClientRelease",
    "DELETE /v1/decisions/training/jobs/:job-id": "DecisionJob",
    "DELETE /v1/devices/:token": "Removed",
    "DELETE /v1/mcp/servers/:name": "McpDeleted",
    "DELETE /v1/providers/:provider-id": "RouterFleet",
    "DELETE /v1/session-groups/:gid": "SessionGroupDeleted",
    "DELETE /v1/sessions/:sid/client-extensions": "EmptyObject",
    "DELETE /v1/sessions/:sid/speech/jobs/:job-id": "Forgotten",
    "DELETE /v1/sessions/:sid/turns/:tid": "TurnCancel",
    "DELETE /v1/sessions/:sid/voice/jobs/:job-id": "Forgotten",
    "DELETE /v1/sessions/:sid/voice/live/:vid": "VoiceLiveClosed",
    "DELETE /v1/speech/jobs/:job-id": "Forgotten",
    "DELETE /v1/speech/voices/:voice-id": "Forgotten",
    "DELETE /v1/voice/jobs/:job-id": "Forgotten",
    "GET /healthz": "Health",
    "GET /openapi.json": "OpenapiDocument",
    "GET /readyz": "Health",
    "GET /v1/admin/status": "GatewayStatus",
    "GET /v1/apps": "AppList",
    "GET /v1/apps/:view-id": "AppView",
    "GET /v1/automations": "AutomationsList",
    "GET /v1/automations/:automation-id": "AutomationsAutomation",
    "GET /v1/automations/runs": "AutomationsRunList",
    "GET /v1/automations/runs/:run-id": "AutomationsRun",
    "GET /v1/capabilities": "Capabilities",
    "GET /v1/decisions/aliases/:alias": "DecisionAlias",
    "GET /v1/decisions/models": "DecisionModels",
    "GET /v1/decisions/models/:model-ref": "DecisionModel",
    "GET /v1/decisions/training/jobs/:job-id": "DecisionJob",
    "GET /v1/devices": "Devices",
    "GET /v1/fs": "FsListing",
    "GET /v1/mcp/servers": "McpServers",
    "GET /v1/models": "ModelsCatalog",
    "GET /v1/projects": "ProjectList",
    "GET /v1/projects/:pid": "Project",
    "GET /v1/projects/overview": "ProjectOverview",
    "GET /v1/provider-presets": "ProviderPresets",
    "GET /v1/providers/:provider-id/limits": "ProviderLimits",
    "GET /v1/providers/:provider-id/models": "ProviderModels",
    "GET /v1/providers/:provider-id/status": "ProviderStatus",
    "GET /v1/router": "RouterFleet",
    "GET /v1/session-groups": "SessionGroupPage",
    "GET /v1/sessions": "SessionList",
    "GET /v1/sessions/:sid": "SessionRecord",
    "GET /v1/sessions/:sid/activity/:aid": "ActivityPage",
    "GET /v1/sessions/:sid/agents/:child-id/timeline": "AgentTimeline",
    "GET /v1/sessions/:sid/alert": "SessionAlert",
    "GET /v1/sessions/:sid/artifacts": "SessionArtifacts",
    "GET /v1/sessions/:sid/client-calls": "ClientCalls",
    "GET /v1/sessions/:sid/context": "SessionContext",
    "GET /v1/sessions/:sid/council": "CouncilBinding",
    "GET /v1/sessions/:sid/council/entries": "CouncilEntryPage",
    "GET /v1/sessions/:sid/council/entries/:entry-id": "CouncilEntryRecord",
    "GET /v1/sessions/:sid/council/members": "CouncilMembers",
    "GET /v1/sessions/:sid/council/threads": "CouncilThreadPage",
    "GET /v1/sessions/:sid/events-since": "SessionEvents",
    "GET /v1/sessions/:sid/forks": "SessionForkTurns",
    "GET /v1/sessions/:sid/fs/file": "FilePreview",
    "GET /v1/sessions/:sid/iterations/:iid/attachments/:idx/link": "AttachmentLink",
    "GET /v1/sessions/:sid/keybindings": "KeybindingList",
    "GET /v1/sessions/:sid/model": "SessionModelRecord",
    "GET /v1/sessions/:sid/resources": "SessionResources",
    "GET /v1/sessions/:sid/resources/logs": "SessionResourceLogs",
    "GET /v1/sessions/:sid/seq": "SessionSeq",
    "GET /v1/sessions/:sid/slashes": "SessionSlashes",
    "GET /v1/sessions/:sid/speech/jobs/:job-id": "VoiceJob",
    "GET /v1/sessions/:sid/suggest": "SessionSuggestions",
    "GET /v1/sessions/:sid/transcript": "TranscriptRecord",
    "GET /v1/sessions/:sid/turns": "TurnList",
    "GET /v1/sessions/:sid/turns/:tid": "TurnRecord",
    "GET /v1/sessions/:sid/turns/:tid/attachments": "TurnAttachments",
    "GET /v1/sessions/:sid/turns/:tid/trace": "TurnTrace",
    "GET /v1/sessions/:sid/usage": "SessionUsage",
    "GET /v1/sessions/:sid/views/input": "ViewRequests",
    "GET /v1/sessions/:sid/views/live": "AppList",
    "GET /v1/sessions/:sid/views/live/:view-id": "AppView",
    "GET /v1/sessions/:sid/views/live/:view-id/log": "ViewLog",
    "GET /v1/sessions/:sid/voice/jobs/:job-id": "VoiceJob",
    "GET /v1/sessions/:sid/voice/live/:vid": "VoiceLiveSnapshot",
    "GET /v1/sessions/:sid/workspace": "SessionWorkspace",
    "GET /v1/sessions/actions/search": "SessionSearch",
    "GET /v1/settings": "Settings",
    "GET /v1/settings/:id": "Setting",
    "GET /v1/speech/jobs/:job-id": "VoiceJob",
    "GET /v1/speech/model": "VoiceModel",
    "GET /v1/speech/voices": "SpeechVoices",
    "GET /v1/voice/jobs/:job-id": "VoiceJob",
    "GET /v1/voice/live/capabilities": "VoiceLiveCapabilities",
    "GET /v1/voice/model": "VoiceModel",
    "GET /v1/web/brand": "WebBrand",
    "PATCH /v1/automations/:automation-id": "AutomationsAutomation",
    "PATCH /v1/projects/:pid": "Project",
    "PATCH /v1/projects/:pid/sessions": "ProjectSessionsMoved",
    "PATCH /v1/router": "RouterPatch",
    "PATCH /v1/session-groups/:gid": "SessionGroupRecord",
    "PATCH /v1/sessions/:sid": "SessionRecord",
    "PATCH /v1/sessions/:sid/model": "SessionModelRecord",
    "PATCH /v1/sessions/:sid/turns/:tid": "TurnRecord",
    "PATCH /v1/sessions/:sid/workspace/root": "SessionWorkspace",
    "PATCH /v1/settings": "Settings",
    "PATCH /v1/speech/voices/:voice-id": "SpeechVoice",
    "POST /v1/admin/drain": "GatewayDrain",
    "POST /v1/admin/stop": "GatewayStop",
    "POST /v1/apps": "AppDoorAnswer",
    "POST /v1/apps/:view-id/actions": "ViewOutcome",
    "POST /v1/automations": "AutomationsAutomation",
    "POST /v1/automations/:automation-id/run": "AutomationsRun",
    "POST /v1/automations/:automation-id/secrets": "AutomationsSecretCreated",
    "POST /v1/clients": "ClientLease",
    "POST /v1/decisions/models": "DecisionModel",
    "POST /v1/decisions/training/jobs": "DecisionJob",
    "POST /v1/devices": "DeviceRegistered",
    "POST /v1/devices/actions/test": "DeviceTest",
    "POST /v1/evals/agreement": "EvalsAgreement",
    "POST /v1/evals/compare": "EvalsCaseComparison",
    "POST /v1/evals/diagnose": "EvalsDiagnosis",
    "POST /v1/evals/labels": "EvalsAgreement",
    "POST /v1/evals/leaderboard": "EvalsLeaderboard",
    "POST /v1/evals/report": "EvalsBenchReport",
    "POST /v1/extensions/reload": "ExtensionReload",
    "POST /v1/fs/actions/mkdir": "FsEntry",
    "POST /v1/hooks/:automation-id": "AutomationsWebhookResult",
    "POST /v1/machines/order": "MachineOrder",
    "POST /v1/mcp/servers": "McpServer",
    "POST /v1/mcp/servers/:name/actions/enable": "McpServer",
    "POST /v1/mcp/servers/:name/actions/kill": "McpServer",
    "POST /v1/mcp/servers/:name/actions/start": "McpServer",
    "POST /v1/mcp/servers/:name/auth/cancel": "McpAuthCancelled",
    "POST /v1/mcp/servers/:name/auth/complete": "McpAuthFlow",
    "POST /v1/mcp/servers/:name/auth/logout": "McpTokenStatus",
    "POST /v1/mcp/servers/:name/auth/poll": "McpAuthFlow",
    "POST /v1/mcp/servers/:name/auth/start": "McpAuthFlow",
    "POST /v1/mcp/servers/actions/test": "McpTest",
    "POST /v1/projects": "Project",
    "POST /v1/projects/actions/ensure": "Project",
    "POST /v1/providers": "RouterFleet",
    "POST /v1/providers/:provider-id/auth/cancel": "ProviderAuth",
    "POST /v1/providers/:provider-id/auth/complete": "ProviderAuth",
    "POST /v1/providers/:provider-id/auth/poll": "ProviderAuth",
    "POST /v1/providers/:provider-id/auth/start": "ProviderAuth",
    "POST /v1/providers/:provider-id/logout": "ProviderAuth",
    "POST /v1/providers/:provider-id/reset-credits/consume": "ProviderResetOutcome",
    "POST /v1/session-groups": "SessionGroupRecord",
    "POST /v1/sessions": "SessionCreated",
    "POST /v1/sessions/:sid/agents": "AgentSpawned",
    "POST /v1/sessions/:sid/agents/answer": "AgentRecords",
    "POST /v1/sessions/:sid/agents/cancel": "AgentRecords",
    "POST /v1/sessions/:sid/agents/deny": "AgentRecords",
    "POST /v1/sessions/:sid/agents/extend": "AgentRecords",
    "POST /v1/sessions/:sid/agents/grant": "AgentRecords",
    "POST /v1/sessions/:sid/agents/route": "AgentsRoute",
    "POST /v1/sessions/:sid/agents/run": "AgentRecords",
    "POST /v1/sessions/:sid/agents/wait": "AgentRecords",
    "POST /v1/sessions/:sid/apps": "AppDoorAnswer",
    "POST /v1/sessions/:sid/attachments": "Upload",
    "POST /v1/sessions/:sid/blocks": "SessionBlock",
    "POST /v1/sessions/:sid/cancel-current": "TurnCancel",
    "POST /v1/sessions/:sid/client-calls/:call_id/activity": "EmptyObject",
    "POST /v1/sessions/:sid/client-calls/:call_id/result": "EmptyObject",
    "POST /v1/sessions/:sid/council/entries": "CouncilEntryRecord",
    "POST /v1/sessions/:sid/council/wake": "CouncilEntryRecord",
    "POST /v1/sessions/:sid/drain-queue": "TurnQueueStep",
    "POST /v1/sessions/:sid/forks": "SessionWrapped",
    "POST /v1/sessions/:sid/fs/actions/open": "SessionFsOpen",
    "POST /v1/sessions/:sid/keybindings/run": "KeybindingRunResult",
    "POST /v1/sessions/:sid/queue/send-now": "QueueSendNow",
    "POST /v1/sessions/:sid/resources/stop": "SessionResourceStop",
    "POST /v1/sessions/:sid/resume-queue": "TurnQueueStep",
    "POST /v1/sessions/:sid/speech": "VoiceJob",
    "POST /v1/sessions/:sid/turns": "TurnRecord",
    "POST /v1/sessions/:sid/turns/:tid/cancel": "TurnCancel",
    "POST /v1/sessions/:sid/views/:view-id/actions": "ViewOutcome",
    "POST /v1/sessions/:sid/voice": "VoiceJob",
    "POST /v1/sessions/:sid/voice/live": "VoiceLiveStart",
    "POST /v1/sessions/actions/archive": "SessionArchive",
    "POST /v1/sessions/actions/import": "SessionRecord",
    "POST /v1/settings": "Setting",
    "POST /v1/settings/:id/preview": "SettingPreviewResult",
    "POST /v1/speech": "VoiceJob",
    "POST /v1/speech/model": "VoiceModel",
    "POST /v1/speech/voices": "SpeechVoice",
    "POST /v1/systemone": "DecisionAnswer",
    "POST /v1/voice": "VoiceJob",
    "POST /v1/voice/model": "VoiceModel",
    "POST /v1/web/launches": "WebLaunch",
    "POST /v1/web/launches/redeem": "WebLaunchGrant",
    "PUT /v1/decisions/aliases/:alias": "DecisionAlias",
    "PUT /v1/mcp/servers/:name": "McpServer",
    "PUT /v1/sessions/:sid/client-extensions": "EmptyObject",
    "PUT /v1/sessions/:sid/group": "SessionRecord",
    "PUT /v1/sessions/:sid/iterations/:iid/attachments/:idx/agent-note": "AgentNote",
    "PUT /v1/sessions/:sid/read": "SessionRead",
    "PUT /v1/web/brand": "WebBrand",
}
"""The response type name of each `METHOD path`."""


if TYPE_CHECKING:
    from blockether.vis.activity import ActivityProjection
    from blockether.vis.engine._client import Event
    from blockether.vis.views import ViewEvent

    class BlockActivityEvent(Event):
        """An event of type `block.activity`."""

        type: Literal["block.activity"]
        seq: int
        turn_id: str
        data: BlockActivityData
        activity: ActivityProjection

    class BlockOutputEvent(Event):
        """An event of type `block.output`."""

        type: Literal["block.output"]
        seq: int
        turn_id: str
        data: BlockOutputData

    class BlockPreviewEvent(Event):
        """An event of type `block.preview`."""

        type: Literal["block.preview"]
        seq: int
        turn_id: str
        data: BlockPreviewData

    class BlockStartedEvent(Event):
        """An event of type `block.started`."""

        type: Literal["block.started"]
        seq: int
        turn_id: str
        data: BlockStartedData

    class ContentBlockCompletedEvent(Event):
        """An event of type `content.block.completed`."""

        type: Literal["content.block.completed"]
        seq: int
        turn_id: str
        data: ContentBlockCompletedData

    class ContentBlockDeltaEvent(Event):
        """An event of type `content.block.delta`."""

        type: Literal["content.block.delta"]
        seq: int
        turn_id: str
        data: ContentBlockDeltaData

    class ContentBlockStartedEvent(Event):
        """An event of type `content.block.started`."""

        type: Literal["content.block.started"]
        seq: int
        turn_id: str
        data: ContentBlockStartedData

    class ContextUpdatedEvent(Event):
        """An event of type `context.updated`."""

        type: Literal["context.updated"]
        seq: int
        data: ContextUpdatedData

    class ExtensionsReloadedEvent(Event):
        """An event of type `extensions.reloaded`."""

        type: Literal["extensions.reloaded"]
        seq: int
        data: ExtensionsReloadedData

    class IterationCompletedEvent(Event):
        """An event of type `iteration.completed`."""

        type: Literal["iteration.completed"]
        seq: int
        turn_id: str
        data: IterationCompletedData

    class IterationErrorEvent(Event):
        """An event of type `iteration.error`."""

        type: Literal["iteration.error"]
        seq: int
        turn_id: str
        data: IterationErrorData

    class NotificationEvent(Event):
        """An event of type `notification`."""

        type: Literal["notification"]
        seq: int
        data: NotificationData

    class ProviderLimitsChangedEvent(Event):
        """An event of type `provider.limits_changed`."""

        type: Literal["provider.limits_changed"]
        seq: int
        data: ProviderLimitsChangedData

    class ProviderRetryEvent(Event):
        """An event of type `provider.retry`."""

        type: Literal["provider.retry"]
        seq: int
        turn_id: str
        data: ProviderRetryData

    class QueuePausedEvent(Event):
        """An event of type `queue.paused`."""

        type: Literal["queue.paused"]
        seq: int
        data: QueuePausedData

    class QueueResumedEvent(Event):
        """An event of type `queue.resumed`."""

        type: Literal["queue.resumed"]
        seq: int
        data: QueueResumedData

    class SessionAgentNameUpdatedEvent(Event):
        """An event of type `session.agent_name_updated`."""

        type: Literal["session.agent_name_updated"]
        seq: int
        data: SessionAgentNameUpdatedData

    class SessionCreatedEvent(Event):
        """An event of type `session.created`."""

        type: Literal["session.created"]
        seq: int
        data: SessionCreatedData

    class SessionDeletedEvent(Event):
        """An event of type `session.deleted`."""

        type: Literal["session.deleted"]
        seq: int
        data: SessionDeletedData

    class SessionGoalUpdatedEvent(Event):
        """An event of type `session.goal_updated`."""

        type: Literal["session.goal_updated"]
        seq: int
        data: SessionGoalUpdatedData

    class SessionModelUpdatedEvent(Event):
        """An event of type `session.model_updated`."""

        type: Literal["session.model_updated"]
        seq: int
        data: SessionModelUpdatedData

    class SessionStatusEvent(Event):
        """An event of type `session.status`."""

        type: Literal["session.status"]
        seq: int
        data: SessionStatusData

    class SessionTeamUpdatedEvent(Event):
        """An event of type `session.team_updated`."""

        type: Literal["session.team_updated"]
        seq: int
        data: SessionTeamUpdatedData

    class SessionTitleUpdatedEvent(Event):
        """An event of type `session.title_updated`."""

        type: Literal["session.title_updated"]
        seq: int
        data: SessionTitleUpdatedData

    class SettingsChangedEvent(Event):
        """An event of type `settings.changed`."""

        type: Literal["settings.changed"]
        seq: int
        data: SettingsChangedData

    class SpeechCommittedEvent(Event):
        """An event of type `speech.committed`."""

        type: Literal["speech.committed"]
        seq: int
        turn_id: str
        data: SpeechCommittedData

    class SpeechInvalidatedEvent(Event):
        """An event of type `speech.invalidated`."""

        type: Literal["speech.invalidated"]
        seq: int
        turn_id: str
        data: SpeechInvalidatedData

    class SubscriptionReadyEvent(Event):
        """An event of type `subscription.ready`."""

        type: Literal["subscription.ready"]
        cursor: int
        data: SubscriptionReadyData

    class TurnCancelledEvent(Event):
        """An event of type `turn.cancelled`."""

        type: Literal["turn.cancelled"]
        seq: int
        turn_id: str
        data: TurnCancelledData

    class TurnCompletedEvent(Event):
        """An event of type `turn.completed`."""

        type: Literal["turn.completed"]
        seq: int
        turn_id: str
        data: TurnCompletedData

    class TurnFailedEvent(Event):
        """An event of type `turn.failed`."""

        type: Literal["turn.failed"]
        seq: int
        turn_id: str
        data: TurnFailedData

    class TurnInputEvent(Event):
        """An event of type `turn.input`."""

        type: Literal["turn.input"]
        seq: int
        turn_id: str
        data: TurnInputData

    class TurnProgressEvent(Event):
        """An event of type `turn.progress`."""

        type: Literal["turn.progress"]
        seq: int
        turn_id: str
        data: TurnProgressData

    class TurnQueuedEvent(Event):
        """An event of type `turn.queued`."""

        type: Literal["turn.queued"]
        seq: int
        turn_id: str
        data: TurnQueuedData

    class TurnQueuedDeletedEvent(Event):
        """An event of type `turn.queued.deleted`."""

        type: Literal["turn.queued.deleted"]
        seq: int
        turn_id: str
        data: TurnQueuedDeletedData

    class TurnQueuedDrainedEvent(Event):
        """An event of type `turn.queued.drained`."""

        type: Literal["turn.queued.drained"]
        seq: int
        turn_id: str
        data: TurnQueuedDrainedData

    class TurnQueuedSentEvent(Event):
        """An event of type `turn.queued.sent`."""

        type: Literal["turn.queued.sent"]
        seq: int
        turn_id: str
        data: TurnQueuedSentData

    class TurnQueuedUpdatedEvent(Event):
        """An event of type `turn.queued.updated`."""

        type: Literal["turn.queued.updated"]
        seq: int
        turn_id: str
        data: TurnQueuedUpdatedData

    class TurnStartedEvent(Event):
        """An event of type `turn.started`."""

        type: Literal["turn.started"]
        seq: int
        turn_id: str
        data: TurnStartedData

    class ViewCloseEvent(Event):
        """An event of type `view.close`."""

        type: Literal["view.close"]
        seq: int
        data: ViewCloseData
        view: ViewEvent

    class ViewOpenEvent(Event):
        """An event of type `view.open`."""

        type: Literal["view.open"]
        seq: int
        data: ViewOpenData
        view: ViewEvent

    class ViewPatchEvent(Event):
        """An event of type `view.patch`."""

        type: Literal["view.patch"]
        seq: int
        data: ViewPatchData
        view: ViewEvent

    class VoiceAdmissionEvent(Event):
        """An event of type `voice.admission`."""

        type: Literal["voice.admission"]
        seq: int
        data: VoiceAdmissionData

    class VoiceInputOverflowEvent(Event):
        """An event of type `voice.input_overflow`."""

        type: Literal["voice.input_overflow"]
        seq: int
        data: VoiceInputOverflowData

    class VoicePlaybackEvent(Event):
        """An event of type `voice.playback`."""

        type: Literal["voice.playback"]
        seq: int
        turn_id: str
        data: VoicePlaybackData

    class VoiceSpeechEvent(Event):
        """An event of type `voice.speech`."""

        type: Literal["voice.speech"]
        seq: int
        turn_id: str
        data: VoiceSpeechData

    class VoiceUtteranceEvent(Event):
        """An event of type `voice.utterance`."""

        type: Literal["voice.utterance"]
        seq: int
        data: VoiceUtteranceData

    StreamEvent: TypeAlias = (
        BlockActivityEvent
        | BlockOutputEvent
        | BlockPreviewEvent
        | BlockStartedEvent
        | ContentBlockCompletedEvent
        | ContentBlockDeltaEvent
        | ContentBlockStartedEvent
        | ContextUpdatedEvent
        | ExtensionsReloadedEvent
        | IterationCompletedEvent
        | IterationErrorEvent
        | NotificationEvent
        | ProviderLimitsChangedEvent
        | ProviderRetryEvent
        | QueuePausedEvent
        | QueueResumedEvent
        | SessionAgentNameUpdatedEvent
        | SessionCreatedEvent
        | SessionDeletedEvent
        | SessionGoalUpdatedEvent
        | SessionModelUpdatedEvent
        | SessionStatusEvent
        | SessionTeamUpdatedEvent
        | SessionTitleUpdatedEvent
        | SettingsChangedEvent
        | SpeechCommittedEvent
        | SpeechInvalidatedEvent
        | SubscriptionReadyEvent
        | TurnCancelledEvent
        | TurnCompletedEvent
        | TurnFailedEvent
        | TurnInputEvent
        | TurnProgressEvent
        | TurnQueuedEvent
        | TurnQueuedDeletedEvent
        | TurnQueuedDrainedEvent
        | TurnQueuedSentEvent
        | TurnQueuedUpdatedEvent
        | TurnStartedEvent
        | ViewCloseEvent
        | ViewOpenEvent
        | ViewPatchEvent
        | VoiceAdmissionEvent
        | VoiceInputOverflowEvent
        | VoicePlaybackEvent
        | VoiceSpeechEvent
        | VoiceUtteranceEvent
    )
    """An event of a session stream. Check `type` to narrow `data`."""
