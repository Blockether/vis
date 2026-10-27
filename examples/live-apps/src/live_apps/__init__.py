"""Proof apps for Vis live apps: one pure model per app, shared by every runner."""


def apps():
    """Every app model by its key, in gallery order, the gallery last."""
    from live_apps import counter, gallery, game2048, incident, kanban, todo, wizard

    return {
        app.KEY: app
        for app in (counter, todo, kanban, game2048, incident, wizard, gallery)
    }
