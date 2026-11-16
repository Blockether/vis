#!/usr/bin/env python3
# Minimal fake MCP server for the vis MCP client tests: newline-delimited
# JSON-RPC 2.0 over stdin/stdout. Handles initialize, tools/list, tools/call.
# The tool "ping" sends a ping to the client and reports its answer. The tool
# "grow" adds a tool and sends notifications/tools/list_changed. The tool "ask"
# sends a form elicitation and reports the answer of the client.
import json
import sys

grown = False


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
    if method == "initialize":
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
        elif name == "grow":
            grown = True
            send({"jsonrpc": "2.0", "method": "notifications/tools/list_changed"})
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
    elif mid is not None:
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
