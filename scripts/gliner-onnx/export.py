"""Prepare a verified local FP32 decision bundle from a GLiNER checkpoint."""

import argparse
import json

from blockether.vis.decisions._gliner import ARCHITECTURES, prepare_fp32


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--checkpoint", required=True, help="Complete local training checkpoint"
    )
    parser.add_argument("--model", choices=ARCHITECTURES, required=True)
    parser.add_argument("--output", required=True, help="New output directory")
    parser.add_argument(
        "--license", required=True, help="Local Apache-2.0 license text"
    )
    parser.add_argument(
        "--revision", help="Pinned source revision; defaults to the weight digest"
    )
    arguments = parser.parse_args()
    report = prepare_fp32(
        arguments.checkpoint,
        arguments.output,
        model_id=arguments.model,
        license_file=arguments.license,
        revision=arguments.revision,
    )
    print(json.dumps(report))


if __name__ == "__main__":
    main()
