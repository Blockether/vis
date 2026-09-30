"""Model identity shared by the optional GLiNER trainer and lightweight publisher."""

ARCHITECTURES = {
    "gliner2.5-base": "boundary",
    "gliner2.5-small": "boundary",
    "gliner2.5-multi": "boundary",
    "gliner2.5-decide": "span",
    "gliner2.5-multi-decide": "boundary",
    "gliner2.5-decide-1b": "span",
}

ENCODERS = {
    "gliner2.5-base": "deberta-v2",
    "gliner2.5-small": "deberta-v2",
    "gliner2.5-multi": "deberta-v2",
    "gliner2.5-decide": "deberta-v2",
    "gliner2.5-multi-decide": "deberta-v2",
    "gliner2.5-decide-1b": "modernbert",
}

_ROPE_LAYERS = {
    "full_attention": "global_rope_theta",
    "sliding_attention": "local_rope_theta",
}


def pinned_settings(name: str, value: dict) -> dict:
    """Translate Transformers 5 checkpoint settings for the pinned version 4.57.6.

    Version 4 ignores ModernBERT ``rope_parameters`` and silently uses another local
    RoPE theta. It also cannot load the version 5 tokenizer class. Other files and
    settings are returned unchanged; an unknown or conflicting form raises ValueError.
    """
    if name == "encoder_config/config.json" and "rope_parameters" in value:
        rope = value["rope_parameters"]
        if (
            value.get("model_type") != "modernbert"
            or not isinstance(rope, dict)
            or set(rope) != set(_ROPE_LAYERS)
            or any(
                not isinstance(layer, dict)
                or set(layer) - {"rope_theta", "rope_type"}
                or layer.get("rope_type", "default") != "default"
                or type(layer.get("rope_theta")) not in (int, float)
                for layer in rope.values()
            )
        ):
            raise ValueError("Unsupported Transformers 5 RoPE settings")
        legacy = {
            key: float(rope[layer]["rope_theta"]) for layer, key in _ROPE_LAYERS.items()
        }
        if any(key in value and value[key] != theta for key, theta in legacy.items()):
            raise ValueError("Encoder RoPE settings conflict")
        return value | legacy
    if name == "tokenizer_config.json":
        pinned = dict(value)
        if pinned.get("tokenizer_class") == "TokenizersBackend":
            pinned["tokenizer_class"] = "PreTrainedTokenizerFast"
        extra = pinned.get("extra_special_tokens")
        if isinstance(extra, list):
            if pinned.get("additional_special_tokens", extra) != extra:
                raise ValueError("Tokenizer special tokens conflict")
            del pinned["extra_special_tokens"]
            pinned["additional_special_tokens"] = extra
        return pinned
    return value
