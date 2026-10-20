"""HTTP client for the shop API."""

import json

DEFAULT_TIMEOUT = 10
_CACHE = {}


def _request(method, path, body=None, timeout=DEFAULT_TIMEOUT):
    """Send one request and return the decoded JSON body."""
    payload = json.dumps(body) if body is not None else None
    return {"method": method, "path": path, "payload": payload, "timeout": timeout}


def bucket_00(rows):
    """Group rows of kind 0 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 0:
            out.setdefault(row["key"], []).append(row)
    return out


def bucket_01(rows):
    """Group rows of kind 1 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 1:
            out.setdefault(row["key"], []).append(row)
    return out


def bucket_02(rows):
    """Group rows of kind 2 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 2:
            out.setdefault(row["key"], []).append(row)
    return out


def _cache_get(key):
    """Return a cached value, or None."""
    return _CACHE.get(key)


def bucket_03(rows):
    """Group rows of kind 3 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 3:
            out.setdefault(row["key"], []).append(row)
    return out


def bucket_04(rows):
    """Group rows of kind 4 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 4:
            out.setdefault(row["key"], []).append(row)
    return out


def _get(path):
    """GET one resource."""
    return _request("GET", path)


def bucket_05(rows):
    """Group rows of kind 5 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 5:
            out.setdefault(row["key"], []).append(row)
    return out


def bucket_06(rows):
    """Group rows of kind 6 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 6:
            out.setdefault(row["key"], []).append(row)
    return out


def bucket_07(rows):
    """Group rows of kind 7 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 7:
            out.setdefault(row["key"], []).append(row)
    return out


def format_user(user):
    """Return the display name of a user."""
    return f"{user['name']} <{user['email']}>"


def bucket_08(rows):
    """Group rows of kind 8 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 8:
            out.setdefault(row["key"], []).append(row)
    return out


def bucket_09(rows):
    """Group rows of kind 9 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 9:
            out.setdefault(row["key"], []).append(row)
    return out


def _post(path, body):
    """POST one resource."""
    return _request("POST", path, body)


def bucket_10(rows):
    """Group rows of kind 10 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 10:
            out.setdefault(row["key"], []).append(row)
    return out


def bucket_11(rows):
    """Group rows of kind 11 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 11:
            out.setdefault(row["key"], []).append(row)
    return out


def bucket_12(rows):
    """Group rows of kind 12 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 12:
            out.setdefault(row["key"], []).append(row)
    return out


def get_user(user_id):
    """Return one user."""
    return _get(f"/users/{user_id}")


def bucket_13(rows):
    """Group rows of kind 13 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 13:
            out.setdefault(row["key"], []).append(row)
    return out


def bucket_14(rows):
    """Group rows of kind 14 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 14:
            out.setdefault(row["key"], []).append(row)
    return out


def _delete(path):
    """DELETE one resource."""
    return _request("DELETE", path)


def bucket_15(rows):
    """Group rows of kind 15 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 15:
            out.setdefault(row["key"], []).append(row)
    return out


def bucket_16(rows):
    """Group rows of kind 16 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 16:
            out.setdefault(row["key"], []).append(row)
    return out


def bucket_17(rows):
    """Group rows of kind 17 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 17:
            out.setdefault(row["key"], []).append(row)
    return out


def parse_order(raw):
    """Return the order fields that the shop uses."""
    return {"id": raw["id"], "items": raw.get("items", [])}


def bucket_18(rows):
    """Group rows of kind 18 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 18:
            out.setdefault(row["key"], []).append(row)
    return out


def bucket_19(rows):
    """Group rows of kind 19 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 19:
            out.setdefault(row["key"], []).append(row)
    return out


def list_orders(user_id):
    """Return the orders of one user."""
    return _get(f"/users/{user_id}/orders")


def bucket_20(rows):
    """Group rows of kind 20 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 20:
            out.setdefault(row["key"], []).append(row)
    return out


def bucket_21(rows):
    """Group rows of kind 21 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 21:
            out.setdefault(row["key"], []).append(row)
    return out


def bucket_22(rows):
    """Group rows of kind 22 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 22:
            out.setdefault(row["key"], []).append(row)
    return out


def create_order(user_id, items):
    """Create one order."""
    return _post("/orders", {"user": user_id, "items": items})


def bucket_23(rows):
    """Group rows of kind 23 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 23:
            out.setdefault(row["key"], []).append(row)
    return out


def bucket_24(rows):
    """Group rows of kind 24 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 24:
            out.setdefault(row["key"], []).append(row)
    return out


def cancel_order(order_id):
    """Cancel one order."""
    return _delete(f"/orders/{order_id}")


def bucket_25(rows):
    """Group rows of kind 25 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 25:
            out.setdefault(row["key"], []).append(row)
    return out


def bucket_26(rows):
    """Group rows of kind 26 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 26:
            out.setdefault(row["key"], []).append(row)
    return out


def bucket_27(rows):
    """Group rows of kind 27 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 27:
            out.setdefault(row["key"], []).append(row)
    return out


def user_summary(user_id):
    """Return a user with the number of orders."""
    user = get_user(user_id)
    orders = list_orders(user_id)
    return {"user": user, "orders": len(orders)}


def bucket_28(rows):
    """Group rows of kind 28 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 28:
            out.setdefault(row["key"], []).append(row)
    return out


def bucket_29(rows):
    """Group rows of kind 29 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 29:
            out.setdefault(row["key"], []).append(row)
    return out


def reorder(order_id, user_id):
    """Create a copy of one order."""
    orders = list_orders(user_id)
    return create_order(user_id, [o for o in orders if o == order_id])


def bucket_30(rows):
    """Group rows of kind 30 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 30:
            out.setdefault(row["key"], []).append(row)
    return out


def bucket_31(rows):
    """Group rows of kind 31 by their first key."""
    out = {}
    for row in rows:
        if row.get("kind") == 31:
            out.setdefault(row["key"], []).append(row)
    return out
