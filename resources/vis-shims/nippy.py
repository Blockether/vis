def __vis_install_nippy__():
    import base64, sys, types

    _bi = sys.modules["builtins"]
    _decode = __vis_nippy_decode__
    _encode = __vis_nippy_encode__

    class NippyError(Exception):
        """Raised when nippy_encode/nippy_decode cannot encode or decode a value."""

        pass

    def _realize(value):
        is_foreign = globals().get("__vis_is_foreign__")
        if is_foreign is None or not is_foreign(value):
            return value
        if hasattr(value, "keys"):
            try:
                return {key: _realize(item) for key, item in value.items()}
            except Exception:
                return value
        try:
            return [_realize(item) for item in value]
        except Exception:
            return value

    def _call(fn, arg):
        result = fn(arg)
        if not result[0]:
            raise NippyError(result[1])
        return _realize(result[1])

    def decode(data):
        """Read trusted Vis Nippy bytes (or their base64 text) back into plain Python data.

        Vectorz vectors arrive as lists and Clojure's exact types are not preserved.
        Raises TypeError on a non-bytes argument, NippyError on malformed input.
        Never decode bytes from an untrusted source.
        """
        if not isinstance(data, (bytes, bytearray, memoryview)):
            raise TypeError("nippy_decode() requires bytes-like input")
        encoded = base64.b64encode(bytes(data)).decode("ascii")
        return _call(_decode, encoded)

    def encode(value):
        """Serialize plain Python data to Vis Nippy bytes the Clojure host reads back.

        Takes what JSON takes plus bytes, and answers `bytes`. Raises NippyError on a
        value the encoder cannot represent.
        """
        encoded = _call(_encode, value)
        return base64.b64decode(encoded)

    mod = types.ModuleType("nippy")
    mod.__doc__ = (
        "`nippy_decode`/`nippy_encode` round-trip trusted Vis Nippy bytes and plain Python "
        "data; Vectorz vectors decode as lists. Not supported: exact Clojure type "
        "preservation, Java Serializable fallback, encryption, untrusted input."
    )
    mod.__version__ = "vis"
    mod.NippyError = NippyError
    mod.decode = decode
    mod.encode = encode
    mod.loads = decode
    mod.dumps = encode
    sys.modules["nippy"] = mod
    _bi.nippy = mod
    _bi.nippy_decode = decode
    _bi.nippy_encode = encode


__vis_install_nippy__()
del __vis_install_nippy__
