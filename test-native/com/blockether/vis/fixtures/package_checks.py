"""Native-package compatibility checks; a blocked import is not a successful check."""

import os
import traceback
from pathlib import Path


def _numpy():
    import numpy as np

    values = np.array([2.0, 3.0])
    assert np.allclose(np.fft.ifft(np.fft.fft(values)), values)
    assert np.allclose(np.linalg.solve([[3.0, 1.0], [1.0, 2.0]], [9.0, 8.0]), values)
    return np


def _scipy():
    import numpy as np
    import scipy
    from scipy.integrate import quad
    from scipy.linalg import solve
    from scipy.optimize import minimize_scalar

    assert np.allclose(solve([[3.0, 1.0], [1.0, 2.0]], [9.0, 8.0]), [2.0, 3.0])
    assert abs(quad(lambda x: x * x, 0, 1)[0] - 1 / 3) < 1e-9
    assert abs(minimize_scalar(lambda x: (x - 3) ** 2).x - 3) < 1e-6
    return scipy


def _pydantic():
    import pydantic

    class Payload(pydantic.BaseModel):
        value: int

    assert Payload.model_validate_json('{"value":"42"}').value == 42
    try:
        Payload.model_validate_json('{"value":"invalid"}')
    except pydantic.ValidationError:
        pass
    else:
        raise AssertionError("invalid payload was accepted")
    return pydantic


def _cryptography():
    import cryptography
    from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PrivateKey
    from cryptography.hazmat.primitives.ciphers.aead import AESGCM

    cipher = AESGCM(AESGCM.generate_key(bit_length=128))
    nonce = bytes(range(12))
    encrypted = cipher.encrypt(nonce, b"package-check", b"context")
    assert cipher.decrypt(nonce, encrypted, b"context") == b"package-check"
    key = Ed25519PrivateKey.generate()
    key.public_key().verify(key.sign(b"package-check"), b"package-check")
    return cryptography


def packages_check() -> dict:
    """Run each library independently and distinguish compatibility from refusal."""
    results = {}
    for name, check in (
        ("numpy", _numpy),
        ("scipy", _scipy),
        ("pydantic", _pydantic),
        ("cryptography", _cryptography),
    ):
        try:
            module = check()
            results[name] = {
                "status": "ok",
                "version": module.__version__,
                "path": str(Path(module.__file__).resolve()),
            }
        except Exception as error:
            results[name] = {
                "status": "blocked" if isinstance(error, PermissionError) else "error",
                "error_type": type(error).__name__,
                "error": str(error),
                "traceback": "".join(traceback.format_exception(error)),
            }
    return {
        "packages": results,
        "pid": os.getpid(),
    }
