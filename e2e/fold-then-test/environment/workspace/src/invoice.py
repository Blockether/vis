"""Invoice totals in cents."""

from src.rates import MAX_DISCOUNT_BPS, TAX_RATE_BPS


def subtotal(lines):
    return sum(quantity * price for quantity, price in lines)


def discount(amount, bps):
    return amount * min(bps, MAX_DISCOUNT_BPS) // 10000


def total(lines, discount_bps=0):
    gross = subtotal(lines)
    net = gross - discount(gross, discount_bps)
    return net + net * TAX_RATE_BPS // 10000
