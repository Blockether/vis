"""Small helpers for the AWS calls of the stack."""

from __future__ import annotations

import functools
import json
import time
from typing import Any

import boto3
from botocore.config import Config
from botocore.exceptions import BotoCoreError, ClientError

from playground.env import BUCKET, LEASE

__all__ = ["BotoCoreError", "ClientError", "alive", "client", "delete", "get", "invoke", "keys", "lease", "metadata", "parameter", "put", "start"]


@functools.cache
def client(name: str) -> Any:
    """Return the shared client of one AWS service."""
    # A model answer can pause for minutes, and a repeated call costs money.
    slow = name == "lambda"
    retries = {"max_attempts": 2 if slow else 5, "mode": "standard"}
    return boto3.client(name, config=Config(retries=retries, read_timeout=900 if slow else 60))


def get(key: str) -> tuple[bytes | None, str | None]:
    """Return the bytes and the ETag of an S3 object, or (None, None)."""
    try:
        answer = client("s3").get_object(Bucket=BUCKET, Key=key)
        return answer["Body"].read(), answer["ETag"]
    except ClientError as error:
        if error.response["Error"]["Code"] in ("NoSuchKey", "404"):
            return None, None
        raise


def metadata(key: str) -> dict[str, str] | None:
    """Return the metadata of an S3 object, or None when there is no object."""
    try:
        return client("s3").head_object(Bucket=BUCKET, Key=key)["Metadata"]
    except ClientError as error:
        if error.response["Error"]["Code"] in ("NoSuchKey", "404"):
            return None
        raise


def put(key: str, body: bytes, **options: Any) -> str:
    """Write an S3 object and return its ETag. `IfMatch` and `IfNoneMatch` make the write conditional."""
    return client("s3").put_object(Bucket=BUCKET, Key=key, Body=body, **options)["ETag"]


def delete(key: str) -> None:
    client("s3").delete_object(Bucket=BUCKET, Key=key)


def keys(prefix: str) -> list[dict[str, Any]]:
    """Return every object below a prefix, with its key, size and time."""
    found: list[dict[str, Any]] = []
    for page in client("s3").get_paginator("list_objects_v2").paginate(Bucket=BUCKET, Prefix=prefix):
        found += page.get("Contents", [])
    return found


def parameter(name: str) -> str:
    """Return the decrypted value of an SSM parameter."""
    return client("ssm").get_parameter(Name=name, WithDecryption=True)["Parameter"]["Value"]


def invoke(function: str, **event: Any) -> tuple[bytes, bool]:
    """Run a function with an event and wait. Return its answer, and True when the function failed."""
    answer = client("lambda").invoke(FunctionName=function, Payload=json.dumps(event).encode())
    return answer["Payload"].read(), bool(answer.get("FunctionError"))


def start(function: str, **event: Any) -> None:
    """Start a function with an event. Do not wait for its end."""
    client("lambda").invoke(FunctionName=function, InvocationType="Event", Payload=json.dumps(event).encode())


def lease() -> tuple[dict[str, Any], str | None]:
    """Return the lease of the worker and its ETag."""
    raw, etag = get(LEASE)
    return (json.loads(raw) if raw else {}), etag


def alive(data: dict[str, Any]) -> bool:
    """True while a worker holds the lease."""
    return data.get("expires", 0) > time.time()
