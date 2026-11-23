import unittest

from src.invoice import discount, subtotal, total


class InvoiceTest(unittest.TestCase):
    def test_subtotal(self):
        self.assertEqual(subtotal([(2, 500), (1, 250)]), 1250)

    def test_total_with_tax(self):
        self.assertEqual(total([(2, 500)]), 1230)

    def test_discount_applies(self):
        self.assertEqual(total([(1, 1000)], discount_bps=1000), 1107)

    def test_discount_caps_at_fifty(self):
        self.assertEqual(discount(1000, 8000), 500)

    def test_empty_invoice(self):
        self.assertEqual(total([]), 0)
