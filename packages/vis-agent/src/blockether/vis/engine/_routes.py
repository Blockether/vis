"""The gateway routes of the SDK as methods, one for each route operation.

`EngineClient.routes` gives the `Routes` object of a client. `EngineClient` and
its session handles hold the workflow API and call these methods.
"""

from __future__ import annotations

from collections.abc import Mapping
from typing import TYPE_CHECKING, TypeAlias

from blockether.vis.engine import _types
from blockether.vis.engine._types import JSONValue

if TYPE_CHECKING:
    from ._client import EngineClient, Response

Query: TypeAlias = Mapping[str, str | int | float | bool | None]


class Routes:
    """One method for each gateway route that the SDK can call.

    Get it from `EngineClient.routes`. Each method sends one request and returns
    the parsed answer. The method name is the HTTP method and the route. `body`
    and `query` use the wire schema of the gateway, not Python objects. For
    ordinary workflows, use `EngineClient`, `Session` and `Turn` methods.

    A route failure raises `GatewayError`. An IO failure or a malformed answer
    raises `TransportError` or a subclass. A mutation is sent one time only.
    The streamed model upload is `GatewayClient.post_decision_model`, because
    only HTTP can stream a request body.

    ```python
    from blockether.vis.engine import GatewayClient

    with GatewayClient("http://127.0.0.1:7890") as client:
        usage = client.routes.get_session_usage("session-id")
        print(usage)
    ```
    """

    def __init__(self, client: EngineClient):
        self._client = client

    def put_session_client_extensions(
        self, sid: str, *, body: JSONValue, timeout: float | None = None
    ) -> _types.EmptyObject:
        """PUT /v1/sessions/:sid/client-extensions — bind client-owned declarations."""
        return self._client._request(
            "PUT",
            "/v1/sessions/:sid/client-extensions",
            path={"sid": sid},
            body=body,
            timeout=timeout,
        ).json()

    def delete_session_client_extensions(
        self, sid: str, *, timeout: float | None = None
    ) -> _types.EmptyObject:
        """DELETE /v1/sessions/:sid/client-extensions — detach owned callbacks."""
        return self._client._request(
            "DELETE",
            "/v1/sessions/:sid/client-extensions",
            path={"sid": sid},
            timeout=timeout,
        ).json()

    def get_session_client_calls(
        self, sid: str, *, timeout: float | None = None
    ) -> _types.ClientCalls:
        """GET /v1/sessions/:sid/client-calls — retrieve pending owned calls."""
        return self._client._request(
            "GET",
            "/v1/sessions/:sid/client-calls",
            path={"sid": sid},
            timeout=timeout,
        ).json()

    def post_session_client_call_result(
        self, sid: str, call_id: str, *, body: JSONValue, timeout: float | None = None
    ) -> _types.EmptyObject:
        """POST /v1/sessions/:sid/client-calls/:call_id/result — acknowledge one call."""
        return self._client._request(
            "POST",
            "/v1/sessions/:sid/client-calls/:call_id/result",
            path={"sid": sid, "call_id": call_id},
            body=body,
            timeout=timeout,
        ).json()

    def post_session_client_call_activity(
        self, sid: str, call_id: str, *, body: JSONValue, timeout: float | None = None
    ) -> _types.EmptyObject:
        """POST /v1/sessions/:sid/client-calls/:call_id/activity — publish presentation."""
        return self._client._request(
            "POST",
            "/v1/sessions/:sid/client-calls/:call_id/activity",
            path={"sid": sid, "call_id": call_id},
            body=body,
            timeout=timeout,
        ).json()

    def get_capabilities(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.Capabilities:
        """GET /v1/capabilities — json response."""
        response = self._client._request(
            "GET", "/v1/capabilities", path={}, query=query, timeout=timeout
        )
        return response.json()

    def get_devices(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.Devices:
        """GET /v1/devices — json response."""
        response = self._client._request(
            "GET", "/v1/devices", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_devices(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.DeviceRegistered:
        """POST /v1/devices — json response."""
        response = self._client._request(
            "POST", "/v1/devices", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def delete_device(
        self, token: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.Removed:
        """DELETE /v1/devices/:token — json response."""
        response = self._client._request(
            "DELETE",
            "/v1/devices/:token",
            path={"token": token},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_devices_test(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.DeviceTest:
        """POST /v1/devices/actions/test — json response."""
        response = self._client._request(
            "POST", "/v1/devices/actions/test", path={}, query=query, timeout=timeout
        )
        return response.json()

    def get_fs(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.FsListing:
        """GET /v1/fs — json response."""
        response = self._client._request(
            "GET", "/v1/fs", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_fs_mkdir(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.FsEntry:
        """POST /v1/fs/actions/mkdir — json response."""
        response = self._client._request(
            "POST",
            "/v1/fs/actions/mkdir",
            path={},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_mcp_servers(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.McpServers:
        """GET /v1/mcp/servers — json response."""
        response = self._client._request(
            "GET", "/v1/mcp/servers", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_mcp_servers(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.McpServer:
        """POST /v1/mcp/servers — json response."""
        response = self._client._request(
            "POST", "/v1/mcp/servers", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def delete_mcp_server(
        self, name: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.McpDeleted:
        """DELETE /v1/mcp/servers/:name — json response."""
        response = self._client._request(
            "DELETE",
            "/v1/mcp/servers/:name",
            path={"name": name},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def put_mcp_server(
        self,
        name: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.McpServer:
        """PUT /v1/mcp/servers/:name — json response."""
        response = self._client._request(
            "PUT",
            "/v1/mcp/servers/:name",
            path={"name": name},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_mcp_server_enable(
        self,
        name: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.McpServer:
        """POST /v1/mcp/servers/:name/actions/enable — json response."""
        response = self._client._request(
            "POST",
            "/v1/mcp/servers/:name/actions/enable",
            path={"name": name},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_mcp_server_kill(
        self, name: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.McpServer:
        """POST /v1/mcp/servers/:name/actions/kill — json response."""
        response = self._client._request(
            "POST",
            "/v1/mcp/servers/:name/actions/kill",
            path={"name": name},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_mcp_server_start(
        self, name: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.McpServer:
        """POST /v1/mcp/servers/:name/actions/start — json response."""
        response = self._client._request(
            "POST",
            "/v1/mcp/servers/:name/actions/start",
            path={"name": name},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_mcp_server_auth_cancel(
        self,
        name: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.McpAuthCancelled:
        """POST /v1/mcp/servers/:name/auth/cancel — json response."""
        response = self._client._request(
            "POST",
            "/v1/mcp/servers/:name/auth/cancel",
            path={"name": name},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_mcp_server_auth_complete(
        self,
        name: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.McpAuthFlow:
        """POST /v1/mcp/servers/:name/auth/complete — json response."""
        response = self._client._request(
            "POST",
            "/v1/mcp/servers/:name/auth/complete",
            path={"name": name},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_mcp_server_auth_logout(
        self, name: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.McpTokenStatus:
        """POST /v1/mcp/servers/:name/auth/logout — json response."""
        response = self._client._request(
            "POST",
            "/v1/mcp/servers/:name/auth/logout",
            path={"name": name},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_mcp_server_auth_poll(
        self,
        name: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.McpAuthFlow:
        """POST /v1/mcp/servers/:name/auth/poll — json response."""
        response = self._client._request(
            "POST",
            "/v1/mcp/servers/:name/auth/poll",
            path={"name": name},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_mcp_server_auth_start(
        self, name: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.McpAuthFlow:
        """POST /v1/mcp/servers/:name/auth/start — json response."""
        response = self._client._request(
            "POST",
            "/v1/mcp/servers/:name/auth/start",
            path={"name": name},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_mcp_servers_test(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.McpTest:
        """POST /v1/mcp/servers/actions/test — json response."""
        response = self._client._request(
            "POST",
            "/v1/mcp/servers/actions/test",
            path={},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_models(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.ModelsCatalog:
        """GET /v1/models — json response."""
        response = self._client._request(
            "GET", "/v1/models", path={}, query=query, timeout=timeout
        )
        return response.json()

    def get_projects(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.ProjectList:
        """GET /v1/projects — json response."""
        response = self._client._request(
            "GET", "/v1/projects", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_projects(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.Project:
        """POST /v1/projects — json response."""
        response = self._client._request(
            "POST", "/v1/projects", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def delete_project(
        self, pid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> None:
        """DELETE /v1/projects/:pid — empty response."""
        self._client._request(
            "DELETE",
            "/v1/projects/:pid",
            path={"pid": pid},
            query=query,
            timeout=timeout,
        )
        return None

    def get_project(
        self, pid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.Project:
        """GET /v1/projects/:pid — json response."""
        response = self._client._request(
            "GET", "/v1/projects/:pid", path={"pid": pid}, query=query, timeout=timeout
        )
        return response.json()

    def patch_project(
        self,
        pid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.Project:
        """PATCH /v1/projects/:pid — json response."""
        response = self._client._request(
            "PATCH",
            "/v1/projects/:pid",
            path={"pid": pid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def patch_project_sessions(
        self,
        pid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.ProjectSessionsMoved:
        """PATCH /v1/projects/:pid/sessions — json response."""
        response = self._client._request(
            "PATCH",
            "/v1/projects/:pid/sessions",
            path={"pid": pid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_projects_ensure(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.Project:
        """POST /v1/projects/actions/ensure — json response."""
        response = self._client._request(
            "POST",
            "/v1/projects/actions/ensure",
            path={},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_projects_overview(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.ProjectOverview:
        """GET /v1/projects/overview — json response."""
        response = self._client._request(
            "GET", "/v1/projects/overview", path={}, query=query, timeout=timeout
        )
        return response.json()

    def get_provider_presets(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.ProviderPresets:
        """GET /v1/provider-presets — json response."""
        response = self._client._request(
            "GET", "/v1/provider-presets", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_providers(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.RouterFleet:
        """POST /v1/providers — json response."""
        response = self._client._request(
            "POST", "/v1/providers", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def delete_provider(
        self,
        provider_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.RouterFleet:
        """DELETE /v1/providers/:provider-id — json response."""
        response = self._client._request(
            "DELETE",
            "/v1/providers/:provider-id",
            path={"provider-id": provider_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_provider_auth_cancel(
        self,
        provider_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.ProviderAuth:
        """POST /v1/providers/:provider-id/auth/cancel — json response."""
        response = self._client._request(
            "POST",
            "/v1/providers/:provider-id/auth/cancel",
            path={"provider-id": provider_id},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_provider_auth_complete(
        self,
        provider_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.ProviderAuth:
        """POST /v1/providers/:provider-id/auth/complete — json response."""
        response = self._client._request(
            "POST",
            "/v1/providers/:provider-id/auth/complete",
            path={"provider-id": provider_id},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_provider_auth_poll(
        self,
        provider_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.ProviderAuth:
        """POST /v1/providers/:provider-id/auth/poll — json response."""
        response = self._client._request(
            "POST",
            "/v1/providers/:provider-id/auth/poll",
            path={"provider-id": provider_id},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_provider_auth_start(
        self,
        provider_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.ProviderAuth:
        """POST /v1/providers/:provider-id/auth/start — json response."""
        response = self._client._request(
            "POST",
            "/v1/providers/:provider-id/auth/start",
            path={"provider-id": provider_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_provider_limits(
        self,
        provider_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.ProviderLimits:
        """GET /v1/providers/:provider-id/limits — json response."""
        response = self._client._request(
            "GET",
            "/v1/providers/:provider-id/limits",
            path={"provider-id": provider_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_provider_reset_credits_consume(
        self,
        provider_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.ProviderResetOutcome:
        """POST /v1/providers/:provider-id/reset-credits/consume — json response."""
        response = self._client._request(
            "POST",
            "/v1/providers/:provider-id/reset-credits/consume",
            path={"provider-id": provider_id},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_provider_logout(
        self,
        provider_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.ProviderAuth:
        """POST /v1/providers/:provider-id/logout — json response."""
        response = self._client._request(
            "POST",
            "/v1/providers/:provider-id/logout",
            path={"provider-id": provider_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_provider_models(
        self,
        provider_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.ProviderModels:
        """GET /v1/providers/:provider-id/models — json response."""
        response = self._client._request(
            "GET",
            "/v1/providers/:provider-id/models",
            path={"provider-id": provider_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_provider_status(
        self,
        provider_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.ProviderStatus:
        """GET /v1/providers/:provider-id/status — json response."""
        response = self._client._request(
            "GET",
            "/v1/providers/:provider-id/status",
            path={"provider-id": provider_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_router(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.RouterFleet:
        """GET /v1/router — json response."""
        response = self._client._request(
            "GET", "/v1/router", path={}, query=query, timeout=timeout
        )
        return response.json()

    def patch_router(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.RouterFleet:
        """PATCH /v1/router — json response."""
        response = self._client._request(
            "PATCH", "/v1/router", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def get_session_groups(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionGroupPage:
        """GET /v1/session-groups — the groups of one project, in the human's order."""
        response = self._client._request(
            "GET", "/v1/session-groups", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_session_groups(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.SessionGroupRecord:
        """POST /v1/session-groups — create one group inside a project."""
        response = self._client._request(
            "POST",
            "/v1/session-groups",
            path={},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def delete_session_group(
        self, gid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionGroupDeleted:
        """DELETE /v1/session-groups/:gid — drop a group and say what becomes of it."""
        response = self._client._request(
            "DELETE",
            "/v1/session-groups/:gid",
            path={"gid": gid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def patch_session_group(
        self,
        gid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.SessionGroupRecord:
        """PATCH /v1/session-groups/:gid — rename, recolour or reorder a group."""
        response = self._client._request(
            "PATCH",
            "/v1/session-groups/:gid",
            path={"gid": gid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_sessions(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionList:
        """GET /v1/sessions — json response."""
        response = self._client._request(
            "GET", "/v1/sessions", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_sessions(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.SessionRecord:
        """POST /v1/sessions — json response."""
        response = self._client._request(
            "POST", "/v1/sessions", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def delete_session(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> None:
        """DELETE /v1/sessions/:sid — empty response."""
        self._client._request(
            "DELETE",
            "/v1/sessions/:sid",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return None

    def get_session(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionRecord:
        """GET /v1/sessions/:sid — json response."""
        response = self._client._request(
            "GET", "/v1/sessions/:sid", path={"sid": sid}, query=query, timeout=timeout
        )
        return response.json()

    def patch_session(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.SessionRecord:
        """PATCH /v1/sessions/:sid — json response."""
        response = self._client._request(
            "PATCH",
            "/v1/sessions/:sid",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_session_activity(
        self,
        sid: str,
        aid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.ActivityPage:
        """GET /v1/sessions/:sid/activity/:aid — a bounded page of complete Activity records.

        Query with after, limit and q. Follow history.next_after until it is
        null. When you combine pages into one copy, reject a changed revision.
        """
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/activity/:aid",
            path={"sid": sid, "aid": aid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_activity_export(
        self,
        sid: str,
        aid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> Response:
        """GET /v1/sessions/:sid/activity/:aid/export — complete unfiltered Activity text.

        The gateway streams this response, and the returned Response buffers its
        bytes. Supply revision to reject a changed history before the export
        starts. Raises ProtocolError if a concurrent change marks the export
        incomplete.
        """
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/activity/:aid/export",
            path={"sid": sid, "aid": aid},
            query=query,
            timeout=timeout,
        )
        if response.content.endswith(
            b"\n\nINCOMPLETE EXPORT: Activity changed. Reload and retry.\n"
        ):
            from ._client import ProtocolError

            raise ProtocolError("Activity export is incomplete; reload and retry")
        return response

    def get_session_alert(
        self, sid: str, *, timeout: float | None = None
    ) -> _types.SessionAlert:
        """GET /v1/sessions/:sid/alert — the banner text for the latest turn."""
        return self._client._request(
            "GET",
            "/v1/sessions/:sid/alert",
            path={"sid": sid},
            timeout=timeout,
        ).json()

    def get_session_artifacts(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionArtifacts:
        """GET /v1/sessions/:sid/artifacts — json response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/artifacts",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_session_attachments(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        content: bytes | None,
    ) -> _types.Upload:
        """POST /v1/sessions/:sid/attachments — json response."""
        response = self._client._request(
            "POST",
            "/v1/sessions/:sid/attachments",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            content=content,
        )
        return response.json()

    def post_session_run(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.SessionBlock:
        """POST /v1/sessions/:sid/run — json response."""
        response = self._client._request(
            "POST",
            "/v1/sessions/:sid/run",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_session_cancel_current(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.TurnCancel:
        """POST /v1/sessions/:sid/cancel-current — json response."""
        response = self._client._request(
            "POST",
            "/v1/sessions/:sid/cancel-current",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_session_context(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionContext:
        """GET /v1/sessions/:sid/context — json response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/context",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_session_drain_queue(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.TurnQueueStep:
        """POST /v1/sessions/:sid/drain-queue — json response."""
        response = self._client._request(
            "POST",
            "/v1/sessions/:sid/drain-queue",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_events_since(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionEvents:
        """GET /v1/sessions/:sid/events-since — json response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/events-since",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_forks(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionForkTurns:
        """GET /v1/sessions/:sid/forks — json response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/forks",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_session_forks(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.SessionWrapped:
        """POST /v1/sessions/:sid/forks — json response."""
        response = self._client._request(
            "POST",
            "/v1/sessions/:sid/forks",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_session_fs_actions_open(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.SessionFsOpen:
        """POST /v1/sessions/:sid/fs/actions/open — open one file in the editor."""
        response = self._client._request(
            "POST",
            "/v1/sessions/:sid/fs/actions/open",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_session_fs_file(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.FilePreview:
        """GET /v1/sessions/:sid/fs/file — preview one workspace file by its kind."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/fs/file",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_fs_file_content(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> Response:
        """GET /v1/sessions/:sid/fs/file/content — binary response.

        Stream one recording or clip. Pass the path, expires and ticket of the
        content_url that the media preview of get_session_fs_file gives.
        """
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/fs/file/content",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response

    def put_session_group(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.SessionRecord:
        """PUT /v1/sessions/:sid/group — file this session under a group."""
        response = self._client._request(
            "PUT",
            "/v1/sessions/:sid/group",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_session_iteration_attachments(
        self,
        sid: str,
        iid: str,
        idx: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> Response:
        """GET /v1/sessions/:sid/iterations/:iid/attachments/:idx — binary response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/iterations/:iid/attachments/:idx",
            path={"sid": sid, "iid": iid, "idx": idx},
            query=query,
            timeout=timeout,
        )
        return response

    def get_session_iteration_attachment_link(
        self,
        sid: str,
        iid: str,
        idx: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.AttachmentLink:
        """GET /v1/sessions/:sid/iterations/:iid/attachments/:idx/link — JSON response.

        Get a signed content_url for an attachment that keeps a reference to a
        file on the gateway machine. A frame, a download link or another app
        opens that address without the gateway token.
        """
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/iterations/:iid/attachments/:idx/link",
            path={"sid": sid, "iid": iid, "idx": idx},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def put_session_iteration_attachment_agent_note(
        self,
        sid: str,
        iid: str,
        idx: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.AgentNote:
        """PUT /v1/sessions/:sid/iterations/:iid/attachments/:idx/agent-note — set the note of an HTML page for the model."""
        response = self._client._request(
            "PUT",
            "/v1/sessions/:sid/iterations/:iid/attachments/:idx/agent-note",
            path={"sid": sid, "iid": iid, "idx": idx},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_session_model(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionModelRecord:
        """GET /v1/sessions/:sid/model — json response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/model",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def patch_session_model(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.SessionModelRecord:
        """PATCH /v1/sessions/:sid/model — json response."""
        response = self._client._request(
            "PATCH",
            "/v1/sessions/:sid/model",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_session_queue_send_now(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.QueueSendNow:
        """POST /v1/sessions/:sid/queue/send-now — json response."""
        response = self._client._request(
            "POST",
            "/v1/sessions/:sid/queue/send-now",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def put_session_read(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.SessionRead:
        """PUT /v1/sessions/:sid/read — how far this reader has read the session."""
        response = self._client._request(
            "PUT",
            "/v1/sessions/:sid/read",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_session_release(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> None:
        """POST /v1/sessions/:sid/release — empty response."""
        self._client._request(
            "POST",
            "/v1/sessions/:sid/release",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return None

    def get_session_resources(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionResources:
        """GET /v1/sessions/:sid/resources — json response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/resources",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_resources_logs(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionResourceLogs:
        """GET /v1/sessions/:sid/resources/logs — json response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/resources/logs",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_session_resources_stop(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionResourceStop:
        """POST /v1/sessions/:sid/resources/stop — json response."""
        response = self._client._request(
            "POST",
            "/v1/sessions/:sid/resources/stop",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_session_resume_queue(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.TurnQueueStep:
        """POST /v1/sessions/:sid/resume-queue — json response."""
        response = self._client._request(
            "POST",
            "/v1/sessions/:sid/resume-queue",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_seq(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionSeq:
        """GET /v1/sessions/:sid/seq — json response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/seq",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_slashes(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionSlashes:
        """GET /v1/sessions/:sid/slashes — json response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/slashes",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_keybindings(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.KeybindingList:
        """GET /v1/sessions/:sid/keybindings — json response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/keybindings",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_session_keybindings_run(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.KeybindingRunResult:
        """POST /v1/sessions/:sid/keybindings/run — json response."""
        response = self._client._request(
            "POST",
            "/v1/sessions/:sid/keybindings/run",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_session_snapshot(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> Response:
        """GET /v1/sessions/:sid/snapshot — binary response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/snapshot",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response

    def post_session_speech(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.VoiceJob:
        """POST /v1/sessions/:sid/speech — json response."""
        response = self._client._request(
            "POST",
            "/v1/sessions/:sid/speech",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def delete_session_speech_job(
        self,
        sid: str,
        job_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.Forgotten:
        """DELETE /v1/sessions/:sid/speech/jobs/:job-id — json response."""
        response = self._client._request(
            "DELETE",
            "/v1/sessions/:sid/speech/jobs/:job-id",
            path={"sid": sid, "job-id": job_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_speech_job(
        self,
        sid: str,
        job_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.VoiceJob:
        """GET /v1/sessions/:sid/speech/jobs/:job-id — json response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/speech/jobs/:job-id",
            path={"sid": sid, "job-id": job_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_speech_job_audio(
        self,
        sid: str,
        job_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> Response:
        """GET /v1/sessions/:sid/speech/jobs/:job-id/audio — binary response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/speech/jobs/:job-id/audio",
            path={"sid": sid, "job-id": job_id},
            query=query,
            timeout=timeout,
        )
        return response

    def get_session_suggest(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionSuggestions:
        """GET /v1/sessions/:sid/suggest — json response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/suggest",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_transcript(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.TranscriptRecord:
        """GET /v1/sessions/:sid/transcript — json response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/transcript",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_transcript_html(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> Response:
        """GET /v1/sessions/:sid/transcript.html — html response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/transcript.html",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response

    def get_session_transcript_md(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> Response:
        """GET /v1/sessions/:sid/transcript.md — markdown response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/transcript.md",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response

    def get_session_turns(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.TurnList:
        """GET /v1/sessions/:sid/turns — json response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/turns",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_session_turns(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.TurnRecord:
        """POST /v1/sessions/:sid/turns — json response."""
        response = self._client._request(
            "POST",
            "/v1/sessions/:sid/turns",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def delete_session_turn(
        self,
        sid: str,
        tid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.TurnCancel:
        """DELETE /v1/sessions/:sid/turns/:tid — json response."""
        response = self._client._request(
            "DELETE",
            "/v1/sessions/:sid/turns/:tid",
            path={"sid": sid, "tid": tid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_turn(
        self,
        sid: str,
        tid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.TurnRecord:
        """GET /v1/sessions/:sid/turns/:tid — json response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/turns/:tid",
            path={"sid": sid, "tid": tid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def patch_session_turn(
        self,
        sid: str,
        tid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.TurnRecord:
        """PATCH /v1/sessions/:sid/turns/:tid — json response."""
        response = self._client._request(
            "PATCH",
            "/v1/sessions/:sid/turns/:tid",
            path={"sid": sid, "tid": tid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_session_turn_attachments(
        self,
        sid: str,
        tid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.TurnAttachments:
        """GET /v1/sessions/:sid/turns/:tid/attachments — json response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/turns/:tid/attachments",
            path={"sid": sid, "tid": tid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_session_turn_cancel(
        self,
        sid: str,
        tid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.TurnCancel:
        """POST /v1/sessions/:sid/turns/:tid/cancel — json response."""
        response = self._client._request(
            "POST",
            "/v1/sessions/:sid/turns/:tid/cancel",
            path={"sid": sid, "tid": tid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_turn_trace(
        self,
        sid: str,
        tid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.TurnTrace:
        """GET /v1/sessions/:sid/turns/:tid/trace — json response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/turns/:tid/trace",
            path={"sid": sid, "tid": tid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_usage(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionUsage:
        """GET /v1/sessions/:sid/usage — json response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/usage",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_session_view(
        self,
        sid: str,
        view_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.ViewOutcome:
        """POST /v1/sessions/:sid/views/:view-id/actions — json response."""
        response = self._client._request(
            "POST",
            "/v1/sessions/:sid/views/:view-id/actions",
            path={"sid": sid, "view-id": view_id},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_session_views_input(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.ViewRequests:
        """GET /v1/sessions/:sid/views/input — json response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/views/input",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_views_live(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.AppList:
        """GET /v1/sessions/:sid/views/live — json response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/views/live",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_views_live_view(
        self,
        sid: str,
        view_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.AppView:
        """GET /v1/sessions/:sid/views/live/:view-id — json response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/views/live/:view-id",
            path={"sid": sid, "view-id": view_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_views_live_log(
        self,
        sid: str,
        view_id: str,
        node_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.ViewLog:
        """GET /v1/sessions/:sid/views/live/:view-id/log — json response.

        The node rides the query string: node ids are free text a surface chose
        and may hold ``/``, which a path segment cannot carry.
        """
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/views/live/:view-id/log",
            path={"sid": sid, "view-id": view_id},
            query={"node": node_id, **(query or {})},
            timeout=timeout,
        )
        return response.json()

    def post_session_apps(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.AppDoorAnswer:
        """POST /v1/sessions/:sid/apps — json response."""
        response = self._client._request(
            "POST",
            "/v1/sessions/:sid/apps",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_apps(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.AppList:
        """GET /v1/apps — json response."""
        response = self._client._request(
            "GET", "/v1/apps", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_apps(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.AppDoorAnswer:
        """POST /v1/apps — json response."""
        response = self._client._request(
            "POST", "/v1/apps", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def get_app(
        self, view_id: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.AppView:
        """GET /v1/apps/:view-id — json response."""
        response = self._client._request(
            "GET",
            "/v1/apps/:view-id",
            path={"view-id": view_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_app_actions(
        self,
        view_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.ViewOutcome:
        """POST /v1/apps/:view-id/actions — json response."""
        response = self._client._request(
            "POST",
            "/v1/apps/:view-id/actions",
            path={"view-id": view_id},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_web_brand(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.WebBrand:
        """GET /v1/web/brand — json response."""
        response = self._client._request(
            "GET", "/v1/web/brand", path={}, query=query, timeout=timeout
        )
        return response.json()

    def put_web_brand(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.WebBrand:
        """PUT /v1/web/brand — json response."""
        response = self._client._request(
            "PUT", "/v1/web/brand", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def delete_web_brand(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> None:
        """DELETE /v1/web/brand — empty response."""
        self._client._request(
            "DELETE", "/v1/web/brand", path={}, query=query, timeout=timeout
        )
        return None

    def post_session_voice(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        content: bytes | None,
    ) -> _types.VoiceJob:
        """POST /v1/sessions/:sid/voice — json response."""
        response = self._client._request(
            "POST",
            "/v1/sessions/:sid/voice",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            content=content,
        )
        return response.json()

    def delete_session_voice_job(
        self,
        sid: str,
        job_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.Forgotten:
        """DELETE /v1/sessions/:sid/voice/jobs/:job-id — json response."""
        response = self._client._request(
            "DELETE",
            "/v1/sessions/:sid/voice/jobs/:job-id",
            path={"sid": sid, "job-id": job_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_voice_job(
        self,
        sid: str,
        job_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.VoiceJob:
        """GET /v1/sessions/:sid/voice/jobs/:job-id — json response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/voice/jobs/:job-id",
            path={"sid": sid, "job-id": job_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_workspace(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionWorkspace:
        """GET /v1/sessions/:sid/workspace — json response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/workspace",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def patch_session_workspace_root(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.SessionWorkspace:
        """PATCH /v1/sessions/:sid/workspace/root — json response."""
        response = self._client._request(
            "PATCH",
            "/v1/sessions/:sid/workspace/root",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_sessions_archive(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.SessionArchive:
        """POST /v1/sessions/actions/archive — archive many sessions; skip working ones."""
        response = self._client._request(
            "POST",
            "/v1/sessions/actions/archive",
            path={},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_sessions_import(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        content: bytes | None,
    ) -> _types.SessionRecord:
        """POST /v1/sessions/actions/import — restore a session snapshot."""
        response = self._client._request(
            "POST",
            "/v1/sessions/actions/import",
            path={},
            query=query,
            timeout=timeout,
            content=content,
        )
        return response.json()

    def get_sessions_search(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionSearch:
        """GET /v1/sessions/actions/search — json response."""
        response = self._client._request(
            "GET", "/v1/sessions/actions/search", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_machines_order(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.MachineOrder:
        """POST /v1/machines/order — register gateway IDs and return their durable order."""
        response = self._client._request(
            "POST",
            "/v1/machines/order",
            path={},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_automations(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.AutomationsList:
        """GET /v1/automations — json response."""
        response = self._client._request(
            "GET", "/v1/automations", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_automations(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.AutomationsAutomation:
        """POST /v1/automations — json response."""
        response = self._client._request(
            "POST", "/v1/automations", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def get_automation_runs(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.AutomationsRunList:
        """GET /v1/automations/runs — json response."""
        response = self._client._request(
            "GET", "/v1/automations/runs", path={}, query=query, timeout=timeout
        )
        return response.json()

    def get_automation_run(
        self, run_id: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.AutomationsRun:
        """GET /v1/automations/runs/:run-id — json response."""
        response = self._client._request(
            "GET",
            "/v1/automations/runs/:run-id",
            path={"run-id": run_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_automation(
        self,
        automation_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.AutomationsAutomation:
        """GET /v1/automations/:automation-id — json response."""
        response = self._client._request(
            "GET",
            "/v1/automations/:automation-id",
            path={"automation-id": automation_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def patch_automation(
        self,
        automation_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.AutomationsAutomation:
        """PATCH /v1/automations/:automation-id — json response."""
        response = self._client._request(
            "PATCH",
            "/v1/automations/:automation-id",
            path={"automation-id": automation_id},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def delete_automation(
        self,
        automation_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.AutomationsDeleted:
        """DELETE /v1/automations/:automation-id — json response."""
        response = self._client._request(
            "DELETE",
            "/v1/automations/:automation-id",
            path={"automation-id": automation_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_automation_run(
        self,
        automation_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.AutomationsRun:
        """POST /v1/automations/:automation-id/run — json response."""
        response = self._client._request(
            "POST",
            "/v1/automations/:automation-id/run",
            path={"automation-id": automation_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_automation_secrets(
        self,
        automation_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.AutomationsSecretCreated:
        """POST /v1/automations/:automation-id/secrets — json response."""
        response = self._client._request(
            "POST",
            "/v1/automations/:automation-id/secrets",
            path={"automation-id": automation_id},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_evals_agreement(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.EvalsAgreement:
        """POST /v1/evals/agreement — json response."""
        response = self._client._request(
            "POST",
            "/v1/evals/agreement",
            path={},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_evals_compare(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.EvalsCaseComparison:
        """POST /v1/evals/compare — json response."""
        response = self._client._request(
            "POST",
            "/v1/evals/compare",
            path={},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_evals_diagnose(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.EvalsDiagnosis:
        """POST /v1/evals/diagnose — json response."""
        response = self._client._request(
            "POST",
            "/v1/evals/diagnose",
            path={},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_evals_labels(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.EvalsAgreement:
        """POST /v1/evals/labels — json response."""
        response = self._client._request(
            "POST", "/v1/evals/labels", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def post_evals_leaderboard(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.EvalsLeaderboard:
        """POST /v1/evals/leaderboard — json response."""
        response = self._client._request(
            "POST",
            "/v1/evals/leaderboard",
            path={},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_evals_report(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.EvalsBenchReport:
        """POST /v1/evals/report — json response."""
        response = self._client._request(
            "POST", "/v1/evals/report", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def get_settings(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.Settings:
        """GET /v1/settings — json response."""
        response = self._client._request(
            "GET", "/v1/settings", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_settings(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.Setting:
        """POST /v1/settings — json response."""
        response = self._client._request(
            "POST", "/v1/settings", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def patch_settings(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.Settings:
        """PATCH /v1/settings — json response."""
        response = self._client._request(
            "PATCH", "/v1/settings", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def get_setting(
        self, id: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.Setting:
        """GET /v1/settings/:id — json response."""
        response = self._client._request(
            "GET", "/v1/settings/:id", path={"id": id}, query=query, timeout=timeout
        )
        return response.json()

    def post_setting_preview(
        self,
        id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.SettingPreviewResult:
        """POST /v1/settings/:id/preview — json response."""
        response = self._client._request(
            "POST",
            "/v1/settings/:id/preview",
            path={"id": id},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_extensions_reload(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.ExtensionReload:
        """POST /v1/extensions/reload — json response."""
        response = self._client._request(
            "POST",
            "/v1/extensions/reload",
            path={},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_speech(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.VoiceJob:
        """POST /v1/speech — json response."""
        response = self._client._request(
            "POST", "/v1/speech", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def delete_speech_job(
        self, job_id: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.Forgotten:
        """DELETE /v1/speech/jobs/:job-id — json response."""
        response = self._client._request(
            "DELETE",
            "/v1/speech/jobs/:job-id",
            path={"job-id": job_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_speech_job(
        self, job_id: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.VoiceJob:
        """GET /v1/speech/jobs/:job-id — json response."""
        response = self._client._request(
            "GET",
            "/v1/speech/jobs/:job-id",
            path={"job-id": job_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_speech_job_audio(
        self, job_id: str, *, query: Query | None = None, timeout: float | None = None
    ) -> Response:
        """GET /v1/speech/jobs/:job-id/audio — binary response."""
        response = self._client._request(
            "GET",
            "/v1/speech/jobs/:job-id/audio",
            path={"job-id": job_id},
            query=query,
            timeout=timeout,
        )
        return response

    def get_speech_model(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.VoiceModel:
        """GET /v1/speech/model — json response."""
        response = self._client._request(
            "GET", "/v1/speech/model", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_speech_model(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.VoiceModel:
        """POST /v1/speech/model — json response."""
        response = self._client._request(
            "POST", "/v1/speech/model", path={}, query=query, timeout=timeout
        )
        return response.json()

    def get_speech_voices(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SpeechVoices:
        """GET /v1/speech/voices — json response."""
        response = self._client._request(
            "GET", "/v1/speech/voices", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_speech_voices(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        content: bytes | None,
    ) -> _types.SpeechVoice:
        """POST /v1/speech/voices — json response."""
        response = self._client._request(
            "POST",
            "/v1/speech/voices",
            path={},
            query=query,
            timeout=timeout,
            content=content,
        )
        return response.json()

    def delete_speech_voice(
        self, voice_id: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.Forgotten:
        """DELETE /v1/speech/voices/:voice-id — json response."""
        response = self._client._request(
            "DELETE",
            "/v1/speech/voices/:voice-id",
            path={"voice-id": voice_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def patch_speech_voice(
        self,
        voice_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.SpeechVoice:
        """PATCH /v1/speech/voices/:voice-id — json response."""
        response = self._client._request(
            "PATCH",
            "/v1/speech/voices/:voice-id",
            path={"voice-id": voice_id},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_speech_voice_sample(
        self, voice_id: str, *, query: Query | None = None, timeout: float | None = None
    ) -> Response:
        """GET /v1/speech/voices/:voice-id/sample — binary response."""
        response = self._client._request(
            "GET",
            "/v1/speech/voices/:voice-id/sample",
            path={"voice-id": voice_id},
            query=query,
            timeout=timeout,
        )
        return response

    def get_decision_models(
        self, *, timeout: float | None = None
    ) -> _types.DecisionModels:
        """GET /v1/decisions/models — installed and resident decision versions."""
        response = self._client._request(
            "GET", "/v1/decisions/models", path={}, timeout=timeout
        )
        return response.json()

    def get_decision_model(
        self, model_ref: str, *, timeout: float | None = None
    ) -> _types.DecisionModel:
        """GET /v1/decisions/models/:model-ref — immutable status after upload."""
        return self._client._request(
            "GET",
            "/v1/decisions/models/:model-ref",
            path={"model-ref": model_ref},
            timeout=timeout,
        ).json()

    def get_decision_alias(
        self, alias: str, *, timeout: float | None = None
    ) -> _types.DecisionAlias:
        """GET /v1/decisions/aliases/:alias — current ref before a CAS update."""
        return self._client._request(
            "GET",
            "/v1/decisions/aliases/:alias",
            path={"alias": alias},
            timeout=timeout,
        ).json()

    def put_decision_alias(
        self,
        alias: str,
        *,
        model_ref: str,
        expected_current: str | None = None,
        timeout: float | None = None,
    ) -> _types.DecisionAlias:
        """PUT /v1/decisions/aliases/:alias — explicit compare-and-swap update."""
        return self._client._request(
            "PUT",
            "/v1/decisions/aliases/:alias",
            path={"alias": alias},
            body={"model_ref": model_ref, "expected_current": expected_current},
            timeout=timeout,
        ).json()

    def post_decision_training_job(
        self, *, body: JSONValue, timeout: float | None = None
    ) -> _types.DecisionJob:
        """POST /v1/decisions/training/jobs — train on approved local data."""
        return self._client._request(
            "POST", "/v1/decisions/training/jobs", path={}, body=body, timeout=timeout
        ).json()

    def get_decision_training_job(
        self, job_id: str, *, timeout: float | None = None
    ) -> _types.DecisionJob:
        """GET /v1/decisions/training/jobs/:job-id — durable progress."""
        return self._client._request(
            "GET",
            "/v1/decisions/training/jobs/:job-id",
            path={"job-id": job_id},
            timeout=timeout,
        ).json()

    def delete_decision_training_job(
        self, job_id: str, *, timeout: float | None = None
    ) -> _types.DecisionJob:
        """DELETE /v1/decisions/training/jobs/:job-id — cancel or discard."""
        return self._client._request(
            "DELETE",
            "/v1/decisions/training/jobs/:job-id",
            path={"job-id": job_id},
            timeout=timeout,
        ).json()

    def post_systemone(
        self, *, body: JSONValue, timeout: float | None = None
    ) -> _types.DecisionAnswer:
        """POST /v1/systemone — typed decisions from an explicitly named model."""
        response = self._client._request(
            "POST", "/v1/systemone", path={}, timeout=timeout, body=body
        )
        return response.json()

    def post_voice(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        content: bytes | None,
    ) -> _types.VoiceJob:
        """POST /v1/voice — json response."""
        response = self._client._request(
            "POST", "/v1/voice", path={}, query=query, timeout=timeout, content=content
        )
        return response.json()

    def delete_voice_job(
        self, job_id: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.Forgotten:
        """DELETE /v1/voice/jobs/:job-id — json response."""
        response = self._client._request(
            "DELETE",
            "/v1/voice/jobs/:job-id",
            path={"job-id": job_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_voice_live_capabilities(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.VoiceLiveCapabilities:
        """GET /v1/voice/live/capabilities — json response."""
        response = self._client._request(
            "GET", "/v1/voice/live/capabilities", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_session_voice_live(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.VoiceLiveStart:
        """POST /v1/sessions/:sid/voice/live — json response."""
        response = self._client._request(
            "POST",
            "/v1/sessions/:sid/voice/live",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_session_voice_live(
        self,
        sid: str,
        vid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.VoiceLiveSnapshot:
        """GET /v1/sessions/:sid/voice/live/:vid — json response."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/voice/live/:vid",
            path={"sid": sid, "vid": vid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def delete_session_voice_live(
        self,
        sid: str,
        vid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.VoiceLiveClosed:
        """DELETE /v1/sessions/:sid/voice/live/:vid — json response."""
        response = self._client._request(
            "DELETE",
            "/v1/sessions/:sid/voice/live/:vid",
            path={"sid": sid, "vid": vid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_voice_live_media(
        self,
        sid: str,
        vid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> Response:
        """GET /v1/sessions/:sid/voice/live/:vid/media — WebSocket media is not supported by this SDK."""
        response = self._client._request(
            "GET",
            "/v1/sessions/:sid/voice/live/:vid/media",
            path={"sid": sid, "vid": vid},
            query=query,
            timeout=timeout,
        )
        return response

    def get_voice_job(
        self, job_id: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.VoiceJob:
        """GET /v1/voice/jobs/:job-id — json response."""
        response = self._client._request(
            "GET",
            "/v1/voice/jobs/:job-id",
            path={"job-id": job_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_voice_model(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.VoiceModel:
        """GET /v1/voice/model — json response."""
        response = self._client._request(
            "GET", "/v1/voice/model", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_voice_model(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.VoiceModel:
        """POST /v1/voice/model — json response."""
        response = self._client._request(
            "POST", "/v1/voice/model", path={}, query=query, timeout=timeout
        )
        return response.json()
