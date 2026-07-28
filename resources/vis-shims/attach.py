# vis sandbox attachment shim: vis_attach / vis_attach_bytes.
#
# A tool that PRODUCES an artifact (image/csv/json/pdf/wav/...) persists it as a
# durable iteration attachment (a session_iteration_attachment DB row) so it
# survives a restart and, for image media-types, replays to a vision model. The
# bytes are read through the sandbox's own CONFINED open, so a path outside the
# filesystem roots raises the normal sandbox error.

def __vis_install_attach__():
    import os as _os
    import base64 as _b64

    def __vis_kind_for(mt):
        return 'image' if str(mt or '').startswith('image/') else 'file'

    def __vis_guess_media_type(name, data):
        head = bytes(data[:16])

        def starts(sig):
            s = bytes(sig)
            return head[:len(s)] == s

        if starts([0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]):
            return 'image/png'
        if starts([0xFF, 0xD8, 0xFF]):
            return 'image/jpeg'
        if starts([0x47, 0x49, 0x46, 0x38]):
            return 'image/gif'
        if starts([0x42, 0x4D]):
            return 'image/bmp'
        if starts([0x25, 0x50, 0x44, 0x46]):
            return 'application/pdf'
        if starts([0x50, 0x4B, 0x03, 0x04]) or starts([0x50, 0x4B, 0x05, 0x06]):
            return 'application/zip'
        if starts([0x1F, 0x8B]):
            return 'application/gzip'
        if starts([0x52, 0x49, 0x46, 0x46]) and head[8:12] == bytes([0x57, 0x45, 0x42, 0x50]):
            return 'image/webp'
        if starts([0x52, 0x49, 0x46, 0x46]) and head[8:12] == bytes([0x57, 0x41, 0x56, 0x45]):
            return 'audio/wav'
        if starts([0x4F, 0x67, 0x67, 0x53]):
            return 'audio/ogg'
        if starts([0x49, 0x44, 0x33]) or starts([0xFF, 0xFB]):
            return 'audio/mpeg'
        import mimetypes
        mt = mimetypes.guess_type(str(name))[0]
        if mt:
            return mt
        try:
            bytes(data).decode('utf-8')
            return 'text/plain'
        except Exception:
            return 'application/octet-stream'

    def __vis_emit_image_fence(disp, name, mt, nbytes):
        # A `vis-image` fence (the same shape plt.show() emits): 5 header lines
        # (summary / host path / mime / WxH / size) a graphical TUI/web reads to
        # paint the picture inline, with the closing fence. No backslash escapes
        # in this shim, so the lines are joined with chr(10).
        try:
            path = str(disp[0])
            w = int(disp[1])
            h = int(disp[2])
        except Exception:
            return
        def _human(n):
            n = float(n)
            for unit in ('B', 'KB', 'MB'):
                if n < 1024.0 or unit == 'MB':
                    return (str(int(n)) + ' B') if unit == 'B' else ('%.1f %s' % (n, unit))
                n = n / 1024.0
        size = _human(nbytes)
        summary = '[Image: ' + str(name) + ' ' + str(w) + '×' + str(h) + ', ' + size + ']'
        fence = '`' * 4
        lines = [fence + 'vis-image', summary, path, str(mt),
                 str(w) + 'x' + str(h), size, fence]
        print(chr(10).join(lines))

    def vis_attach_bytes(data, filename, kind=None, media_type=None):
        if isinstance(data, str):
            data = data.encode('utf-8')
        data = bytes(data)
        name = str(filename) if filename else 'artifact'
        mt = media_type or __vis_guess_media_type(name, data)
        knd = kind or __vis_kind_for(mt)
        b64 = _b64.b64encode(data).decode('ascii')
        rec = globals().get('__vis_record_attachment__')
        if rec is None:
            raise RuntimeError('vis_attach: capture bridge not bound in this sandbox')
        env = rec(knd, mt, b64, name, len(data))
        if not env[0]:
            raise RuntimeError('vis_attach: ' + str(env[1]))
        disp = env[1] if len(env) > 1 else None
        if disp:
            __vis_emit_image_fence(disp, name, mt, len(data))
        return None

    def vis_attach(path, kind=None, media_type=None, filename=None):
        with open(path, 'rb') as f:
            data = f.read()
        name = filename or _os.path.basename(str(path)) or 'artifact'
        return vis_attach_bytes(data, name, kind=kind, media_type=media_type)

    def vis_attachments():
        lst = globals().get('__vis_list_attachments__')
        if lst is None:
            raise RuntimeError('vis_attachments: reader bridge not bound in this sandbox')
        env = lst()
        if not env[0]:
            raise RuntimeError('vis_attachments: ' + str(env[1]))
        import json as _json
        rows = _json.loads(env[1])

        return [{str(k).replace('-', '_'): v for k, v in r.items()} for r in rows]

    def vis_reinspect_attachment(attachment_id, detail='auto'):
        if detail not in ('auto', 'low', 'high'):
            raise ValueError('detail must be auto, low, or high')
        reinsp = globals().get('__vis_reinspect_attachment__')
        if reinsp is None:
            raise RuntimeError('vis_reinspect_attachment: reader bridge not bound in this sandbox')
        env = reinsp(str(attachment_id), detail)
        if not env[0]:
            raise RuntimeError('vis_reinspect_attachment: ' + str(env[1]))
        row = env[1]
        return {'id': row[0], 'filename': row[1], 'media_type': row[2], 'size': row[3]}

    def vis_read_attachment(attachment_id):
        rd = globals().get('__vis_read_attachment__')
        if rd is None:
            raise RuntimeError('vis_read_attachment: reader bridge not bound in this sandbox')
        env = rd(str(attachment_id))
        if not env[0]:
            raise RuntimeError('vis_read_attachment: ' + str(env[1]))
        row = env[1]
        b64 = row[0]
        data = _b64.b64decode(b64) if b64 else None
        return {'bytes': data, 'media_type': row[1], 'filename': row[2],
                'kind': row[3], 'size': row[4], 'id': row[5], 'storage_uri': row[6]}

    vis_attach.__doc__ = (
        'Persist a file this tool produced as a durable iteration attachment. '
        'Reads path through the sandbox-confined filesystem (a path outside the '
        'roots raises), sniffs the media-type (magic bytes / extension / utf-8 '
        'probe), and hands the bytes to the engine so they land in the DB as a '
        'session_iteration_attachment row - surviving a web/TUI restart and, for '
        'image/* media-types, replayable to a vision model on later turns. '
        'kind / media_type / filename override the guesses. '
        'Returns None so a bare call produces no result display; do not print the '
        'call. Use vis_attachments() when attachment metadata is needed. '
        'Use vis_attach_bytes(data, filename, ...) for in-memory bytes/str.'
    )
    vis_attach_bytes.__doc__ = (
        'Persist in-memory bytes (or a str, utf-8 encoded) as a durable iteration '
        'attachment - the no-temp-file twin of vis_attach. filename gives the '
        'artifact its name and drives extension-based media-type guessing. '
        'Returns None so a bare call produces no result display; do not print the '
        'call. Use vis_attachments() when attachment metadata is needed.'
    )

    g = globals()
    g['vis_attach'] = vis_attach
    g['vis_attach_bytes'] = vis_attach_bytes
    g['vis_attachments'] = vis_attachments
    g['vis_read_attachment'] = vis_read_attachment
    g['vis_reinspect_attachment'] = vis_reinspect_attachment
    g['__vis_guess_media_type'] = __vis_guess_media_type
    g['__vis_kind_for'] = __vis_kind_for

    docs = g.setdefault('__vis_docs__', {})
    docs['vis_attach'] = (
        'vis_attach(path, kind=None, media_type=None, filename=None): persist a '
        'produced file as a durable DB iteration attachment (survives restart; '
        'image/* replays to vision models). Returns None; call directly, do not print. '
        'Use vis_attachments() when metadata is needed.'
    )
    docs['vis_attach_bytes'] = (
        'vis_attach_bytes(data, filename, kind=None, media_type=None): persist '
        'in-memory bytes/str as a durable DB iteration attachment. Returns None; '
        'call directly, do not print. Use vis_attachments() when metadata is needed.'
    )
    docs['vis_reinspect_attachment'] = (
        'vis_reinspect_attachment(id, detail=auto): queue a persisted image from this '
        'session for exactly the NEXT provider request, then return it to stored-only state. '
        'Use this when pixels from an earlier user upload or tool artifact need another look. '
        'detail is auto, low, or high. Non-image and unknown ids raise RuntimeError.'
    )
    docs['vis_attachments'] = (
        'vis_attachments(): list artifacts already persisted in THIS session '
        '(id, filename, media_type, kind, size, position, tool_call_id, '
        'iteration_id) - metadata only, newest turns included. Pick an id and '
        'read the bytes with vis_read_attachment(id).'
    )
    docs['vis_read_attachment'] = (
        'vis_read_attachment(id): fetch one persisted artifact by id as '
        '{bytes, media_type, filename, kind, size, id, storage_uri} - the '
        'read-back twin of vis_attach, so a tool can reuse an artifact it (or '
        'an earlier turn) produced.'
    )

__vis_install_attach__()
del __vis_install_attach__
