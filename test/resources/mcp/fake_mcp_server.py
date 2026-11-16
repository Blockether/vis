#!/usr/bin/env python3
# Minimal fake MCP server for the vis MCP client tests: newline-delimited
# JSON-RPC 2.0 over stdin/stdout. Handles initialize, tools/list, tools/call.
# The tool "ping" sends a ping to the client and reports its answer. The tool
# "grow" adds a tool and sends notifications/tools/list_changed. The tool "ask"
# sends a form elicitation and reports the answer of the client. The tools
# "roots" and "sample" do the same with roots/list and sampling/createMessage.
# With the argument "stateless" it is a server of revision 2026-07-28: it has no
# handshake, each request must name its protocol version, and it says that its
# tools changed only on a subscription.
# More arguments: "dual" also answers the handshake of an older revision, "mute"
# gives no answer to a request that it does not know, and "slow" starts late.
import json
import sys
import time

modes = set(sys.argv[1:])
grown = False
dual = "dual" in modes
stateless = dual or "stateless" in modes
subscription = None

if "slow" in modes:
    time.sleep(0.6)


def send(obj):
    sys.stdout.write(json.dumps(obj) + "\n")
    sys.stdout.flush()


def text_result(mid, text):
    send(
        {
            "jsonrpc": "2.0",
            "id": mid,
            "result": {"content": [{"type": "text", "text": text}], "isError": False},
        }
    )


for line in sys.stdin:
    line = line.strip()
    if not line:
        continue
    try:
        msg = json.loads(line)
    except Exception:
        continue
    mid = msg.get("id")
    method = msg.get("method")
    meta = (msg.get("params") or {}).get("_meta") or {}
    if (
        stateless
        and not dual
        and mid is not None
        and (
            method == "initialize"
            or "io.modelcontextprotocol/protocolVersion" not in meta
        )
    ):
        send(
            {
                "jsonrpc": "2.0",
                "id": mid,
                "error": {"code": -32602, "message": "no protocol version"},
            }
        )
    elif stateless and method == "server/discover":
        send(
            {
                "jsonrpc": "2.0",
                "id": mid,
                "result": {
                    "resultType": "complete",
                    "supportedVersions": ["2026-07-28"],
                    "capabilities": {"tools": {"listChanged": True}},
                    "_meta": {
                        "io.modelcontextprotocol/serverInfo": {
                            "name": "fake",
                            "version": "1.0",
                        }
                    },
                },
            }
        )
    elif stateless and method == "subscriptions/listen":
        subscription = mid
        send(
            {
                "jsonrpc": "2.0",
                "method": "notifications/subscriptions/acknowledged",
                "params": {
                    "_meta": {"io.modelcontextprotocol/subscriptionId": mid},
                    "notifications": {"toolsListChanged": True},
                },
            }
        )
    elif method == "initialize":
        send(
            {
                "jsonrpc": "2.0",
                "id": mid,
                "result": {
                    "protocolVersion": "2025-06-18",
                    "serverInfo": {"name": "fake", "version": "1.0"},
                    "capabilities": {"tools": {}},
                },
            }
        )
    elif method == "notifications/initialized":
        pass  # notification — no reply
    elif method == "tools/list":
        send(
            {
                "jsonrpc": "2.0",
                "id": mid,
                "result": {
                    "tools": [
                        {
                            "name": "echo",
                            "description": "echo back the msg",
                            "inputSchema": {
                                "type": "object",
                                "properties": {"msg": {"type": "string"}},
                            },
                        }
                    ]
                    + ([{"name": "grown"}] if grown else [])
                },
            }
        )
    elif method == "tools/call":
        args = (msg.get("params") or {}).get("arguments") or {}
        name = (msg.get("params") or {}).get("name")
        if name == "echo":
            send(
                {
                    "jsonrpc": "2.0",
                    "id": mid,
                    "result": {
                        "content": [
                            {
                                "type": "text",
                                "text": "echo: " + str(args.get("msg", "")),
                            }
                        ],
                        "isError": False,
                    },
                }
            )
        elif name == "ping":
            send({"jsonrpc": "2.0", "id": "server-1", "method": "ping"})
            answer = json.loads(sys.stdin.readline() or "{}")
            is_pong = answer.get("id") == "server-1" and answer.get("result") == {}
            text_result(mid, "pong" if is_pong else "silence")
        elif name == "ask":
            send(
                {
                    "jsonrpc": "2.0",
                    "id": "server-2",
                    "method": "elicitation/create",
                    "params": {
                        "message": "Who are you?",
                        "requestedSchema": {
                            "type": "object",
                            "properties": {
                                "zone": {"type": "string", "default": "UTC"},
                                "age": {"type": "integer", "default": 30},
                                "name": {"type": "string"},
                            },
                            "required": ["name"],
                        },
                    },
                }
            )
            answer = json.loads(sys.stdin.readline() or "{}")
            text_result(mid, json.dumps(answer.get("result"), sort_keys=True))
        elif name in ("roots", "sample"):
            send(
                {"jsonrpc": "2.0", "id": "server-3", "method": "roots/list"}
                if name == "roots"
                else {
                    "jsonrpc": "2.0",
                    "id": "server-3",
                    "method": "sampling/createMessage",
                    "params": {
                        "messages": [
                            {
                                "role": "user",
                                "content": {
                                    "type": "text",
                                    "text": "Capital of France?",
                                },
                            }
                        ],
                        "maxTokens": 50,
                    },
                }
            )
            answer = json.loads(sys.stdin.readline() or "{}")
            text_result(
                mid,
                json.dumps(answer.get("result") or answer.get("error"), sort_keys=True),
            )
        elif name == "grow":
            grown = True
            if not stateless:
                send({"jsonrpc": "2.0", "method": "notifications/tools/list_changed"})
            elif subscription is not None:
                send(
                    {
                        "jsonrpc": "2.0",
                        "method": "notifications/tools/list_changed",
                        "params": {
                            "_meta": {
                                "io.modelcontextprotocol/subscriptionId": subscription
                            }
                        },
                    }
                )
            text_result(mid, "grown")
        else:
            send(
                {
                    "jsonrpc": "2.0",
                    "id": mid,
                    "result": {
                        "content": [{"type": "text", "text": "no such tool"}],
                        "isError": True,
                    },
                }
            )
    elif mid is not None and "mute" not in modes:
        send(
            {
                "jsonrpc": "2.0",
                "id": mid,
                "error": {
                    "code": -32601,
                    "message": "method not found: " + str(method),
                },
            }
        )
