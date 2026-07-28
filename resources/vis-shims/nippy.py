def __vis_install_nippy__():
    import base64, sys, types
    _bi = sys.modules['builtins']
    _decode = __vis_nippy_decode__
    _encode = __vis_nippy_encode__

    class NippyError(Exception):
        pass

    def _realize(value):
        is_foreign = globals().get('__vis_is_foreign__')
        if is_foreign is None or not is_foreign(value):
            return value
        if hasattr(value, 'keys'):
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
        if not isinstance(data, (bytes, bytearray, memoryview)):
            raise TypeError('nippy_decode() requires bytes-like input')
        encoded = base64.b64encode(bytes(data)).decode('ascii')
        return _call(_decode, encoded)

    def encode(value):
        encoded = _call(_encode, value)
        return base64.b64decode(encoded)

    mod = types.ModuleType('nippy')
    mod.__doc__ = 'Vis Nippy codec for trusted persistence BLOBs and Python plain data.'
    mod.__version__ = 'vis'
    mod.NippyError = NippyError
    mod.decode = decode
    mod.encode = encode
    mod.loads = decode
    mod.dumps = encode
    sys.modules['nippy'] = mod
    _bi.nippy = mod
    _bi.nippy_decode = decode
    _bi.nippy_encode = encode

__vis_install_nippy__()
del __vis_install_nippy__
