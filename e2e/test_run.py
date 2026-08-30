import unittest

from e2e.run import cache_metric_failures, decode_usage_body, usage_percent


class CacheMetricValidationTest(unittest.TestCase):
    def usage(self):
        return {
            "input_tokens": 30_000,
            "input_cache_read_tokens": 19_000,
            "prompt_cache_reusable_tokens": 20_000,
            "prompt_cache_reused_tokens": 18_500,
            "prompt_cache_sample_count": 3,
            "prompt_cache_estimated_sample_count": 1,
            "prompt_cache_rebuild_count": 1,
            "prompt_cache_expired_count": 0,
            "cache_read_share_percent": 63,
            "reusable_prefix_coverage_percent": 93,
        }

    def test_accepts_cross_layer_totals_and_one_fold(self):
        self.assertEqual(
            [],
            cache_metric_failures(
                self.usage(),
                {"input": 30_000, "cached": 19_000},
                provider_call_count=4,
                folded_prefix=True,
            ),
        )

    def test_rejects_provider_percentage_and_fold_drift(self):
        usage = self.usage()
        usage.update(
            {
                "input_tokens": 29_999,
                "cache_read_share_percent": 62,
                "prompt_cache_estimated_sample_count": 0,
                "prompt_cache_rebuild_count": 0,
            }
        )
        failures = cache_metric_failures(
            usage,
            {"input": 30_000, "cached": 19_000},
            provider_call_count=4,
            folded_prefix=True,
        )
        self.assertTrue(any("usage input" in failure for failure in failures))
        self.assertTrue(any("cache-read share" in failure for failure in failures))
        self.assertTrue(any("estimated samples" in failure for failure in failures))
        self.assertTrue(any("rebuilds" in failure for failure in failures))

    def test_percentage_rounds_half_up_like_gateway(self):
        self.assertEqual(13, usage_percent(1, 8))
        self.assertEqual(0, usage_percent(0, 0))
        self.assertEqual(100, usage_percent(9, 8))

    def test_decodes_public_usage_envelope(self):
        self.assertEqual(
            {"input_tokens": 12}, decode_usage_body('{"usage":{"input_tokens":12}}')
        )
        with self.assertRaisesRegex(ValueError, "no usage object"):
            decode_usage_body('{"usage":null}')


if __name__ == "__main__":
    unittest.main()
