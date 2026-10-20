"""Service layer that the web handlers call."""

from client import cancel_order, format_user, reorder, user_summary


def profile_page(user_id):
    """Return the data of the profile page."""
    summary = user_summary(user_id)
    return {"title": "Profile", "summary": summary}


def display_name(user):
    """Return the name shown in the header."""
    return format_user(user)


def repeat_order(order_id, user_id):
    """Repeat one order for a user."""
    return reorder(order_id, user_id)


def cancel(order_id):
    """Cancel one order for a user."""
    return cancel_order(order_id)
