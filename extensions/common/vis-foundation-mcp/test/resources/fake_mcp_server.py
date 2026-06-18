#!/usr/bin/env python3
# Minimal fake MCP server for the vis MCP client tests: newline-delimited
# JSON-RPC 2.0 over stdin/stdout. Handles initialize, tools/list, tools/call.
import sys, json


def send(obj):
    sys.stdout.write(json.dumps(obj) + "\n")
    sys.stdout.flush()


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
        send({"jsonrpc": "2.0", "id": mid, "result": {
            "protocolVersion": "2025-06-18",
            "serverInfo": {"name": "fake", "version": "1.0"},
            "capabilities": {"tools": {}}}})
    elif method == "notifications/initialized":
        pass  # notification — no reply
    elif method == "tools/list":
        send({"jsonrpc": "2.0", "id": mid, "result": {"tools": [
            {"name": "echo", "description": "echo back the msg",
             "inputSchema": {"type": "object",
                             "properties": {"msg": {"type": "string"}}}}]}})
    elif method == "tools/call":
        args = (msg.get("params") or {}).get("arguments") or {}
        name = (msg.get("params") or {}).get("name")
        if name == "echo":
            send({"jsonrpc": "2.0", "id": mid, "result": {
                "content": [{"type": "text", "text": "echo: " + str(args.get("msg", ""))}],
                "isError": False}})
        else:
            send({"jsonrpc": "2.0", "id": mid, "result": {
                "content": [{"type": "text", "text": "no such tool"}], "isError": True}})
    elif mid is not None:
        send({"jsonrpc": "2.0", "id": mid,
              "error": {"code": -32601, "message": "method not found: " + str(method)}})
