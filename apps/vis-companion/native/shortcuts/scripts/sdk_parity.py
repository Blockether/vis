"""Write the SDK parity fixture for the Swift Shortcuts core.

Run `uv run python ../../apps/vis-companion/native/shortcuts/scripts/sdk_parity.py`
in `packages/vis-agent`, so that the script uses the SDK of this checkout.
The fixture holds the prompts, schema text and problems that the Python SDK's
response contract gives. `VisShortcutsCoreTests` checks the Swift contract against it.
"""

import json
from pathlib import Path
from typing import Literal, Optional

from pydantic import BaseModel, Field

from blockether.vis.engine._agent import _problem_lines, _ResponseContract


class Item(BaseModel):
    name: str = Field(min_length=1)
    count: int = Field(ge=0)


class Order(BaseModel):
    id: int
    status: Literal["open", "closed"]
    items: list[Item]
    note: Optional[str] = None
    tags: list[str] = Field(default_factory=list, max_length=3)
    price: float = Field(gt=0)


class Quote(BaseModel):
    cents: int
    express: bool


ANSWERS = {
    "Quote": [
        '{"cents": 1200, "express": false}',
        '{"cents": "1200", "express": false}',
        '{"express": true}',
        '```json\n{"cents": 1, "express": true}\n```',
        'Here you go: {"cents": 1}',
        '{"cents": 1, "express": true} trailing',
        '{"cents": 1, "cents": 2, "express": true}',
        '{"cents": 1, "express": tru}',
        '{"cents": NaN, "express": true}',
        '{"cents": 1e400, "express": true}',
        '{"cents": 1.0, "express": true}',
        '{"cents": 1.5, "express": true}',
        '[1, 2]',
        '{"cents": 1,\n "express": }',
        "",
        None,
    ],
    "Order": [
        '{"id": 1, "status": "open", "items": [{"name": "a", "count": 1}], "price": 2.5}',
        '{"id": 1, "status": "pending", "items": [{"name": "", "count": -1}], "price": 0}',
        '{"id": 1, "status": "open", "items": [{"count": 1}], "note": 5, "price": 1, '
        '"tags": ["a", "b", "c", "d"]}',
        '{"id": true, "status": "open", "items": "none", "price": 1}',
        '{}',
    ],
}


def main():
    cases = []
    for model in (Quote, Order):
        contract = _ResponseContract(model)
        rows = []
        for answer in ANSWERS[model.__name__]:
            value, problems = contract.validate(answer)
            rows.append(
                {
                    "answer": answer,
                    "is_valid": value is not None,
                    "problems": problems,
                    "correction": contract.correction(problems) if problems else None,
                }
            )
        cases.append(
            {
                "model": model.__name__,
                "schema": model.model_json_schema(mode="validation"),
                "schema_text": contract.schema_text,
                "request": contract.request("Quote a 1000 gram parcel."),
                "answers": rows,
            }
        )
    many = [{"path": f"$.f{n}", "message": "bad", "source": "schema"} for n in range(12)]
    fixture = {"cases": cases, "problem_lines": {"problems": many, "text": _problem_lines(many)}}
    target = Path(__file__).resolve().parents[1] / "Tests/VisShortcutsCoreTests/Fixtures/sdk-parity.json"
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(json.dumps(fixture, ensure_ascii=False, indent=2) + "\n")
    print(f"wrote {target}")


if __name__ == "__main__":
    main()
