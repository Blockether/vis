"""Stock bookkeeping for one warehouse."""

DEFAULT_TIMEOUT = 30
LOW_STOCK = 5


def shelf_00(stock):
    """Return the units on shelf 0."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S00"):
            units += qty
    return units


def shelf_01(stock):
    """Return the units on shelf 1."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S01"):
            units += qty
    return units


def shelf_02(stock):
    """Return the units on shelf 2."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S02"):
            units += qty
    return units


def shelf_03(stock):
    """Return the units on shelf 3."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S03"):
            units += qty
    return units


def reserve(stock, sku, qty):
    """Take qty units of sku out of stock."""
    stock[sku] = stock.get(sku, 0) - qty
    return stock[sku]


def shelf_04(stock):
    """Return the units on shelf 4."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S04"):
            units += qty
    return units


def shelf_05(stock):
    """Return the units on shelf 5."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S05"):
            units += qty
    return units


def shelf_06(stock):
    """Return the units on shelf 6."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S06"):
            units += qty
    return units


def shelf_07(stock):
    """Return the units on shelf 7."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S07"):
            units += qty
    return units


def shelf_08(stock):
    """Return the units on shelf 8."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S08"):
            units += qty
    return units


def release(stock, sku, qty):
    """Put qty units of sku back into stock."""
    stock[sku] = stock.get(sku, 0) + qty
    return stock[sku]


def shelf_09(stock):
    """Return the units on shelf 9."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S09"):
            units += qty
    return units


def shelf_10(stock):
    """Return the units on shelf 10."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S10"):
            units += qty
    return units


def shelf_11(stock):
    """Return the units on shelf 11."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S11"):
            units += qty
    return units


def shelf_12(stock):
    """Return the units on shelf 12."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S12"):
            units += qty
    return units


def report(stock):
    """Summarize the stock."""
    return {"count": sum(stock.values()), "skus": len(stock)}


def shelf_13(stock):
    """Return the units on shelf 13."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S13"):
            units += qty
    return units


def shelf_14(stock):
    """Return the units on shelf 14."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S14"):
            units += qty
    return units


def shelf_15(stock):
    """Return the units on shelf 15."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S15"):
            units += qty
    return units


def summary_line(stock):
    """Return one line for the dashboard."""
    data = report(stock)
    return f"{data['skus']} SKUs, {data['count']} units"


def shelf_16(stock):
    """Return the units on shelf 16."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S16"):
            units += qty
    return units


def shelf_17(stock):
    """Return the units on shelf 17."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S17"):
            units += qty
    return units


def shelf_18(stock):
    """Return the units on shelf 18."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S18"):
            units += qty
    return units


def batch_stats(batch):
    """Count the rows of one import batch."""
    stats = {"count": len(batch)}
    return stats


def shelf_19(stock):
    """Return the units on shelf 19."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S19"):
            units += qty
    return units


def shelf_20(stock):
    """Return the units on shelf 20."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S20"):
            units += qty
    return units


def shelf_21(stock):
    """Return the units on shelf 21."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S21"):
            units += qty
    return units


def low_stock_alert(stock):
    """Return True when the warehouse runs low."""
    return report(stock)["count"] < LOW_STOCK


def shelf_22(stock):
    """Return the units on shelf 22."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S22"):
            units += qty
    return units


def shelf_23(stock):
    """Return the units on shelf 23."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S23"):
            units += qty
    return units


def shelf_24(stock):
    """Return the units on shelf 24."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S24"):
            units += qty
    return units


def legacy_export(stock):
    """Old export format. Nothing calls it."""
    return ";".join(f"{k}={v}" for k, v in stock.items())


def shelf_25(stock):
    """Return the units on shelf 25."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S25"):
            units += qty
    return units


def shelf_26(stock):
    """Return the units on shelf 26."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S26"):
            units += qty
    return units


def shelf_27(stock):
    """Return the units on shelf 27."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S27"):
            units += qty
    return units


def export_csv(stock):
    """Return the stock as CSV text with a total line."""
    rows = [f"{sku},{qty}" for sku, qty in sorted(stock.items())]
    rows.append(f"total,{report(stock)['count']}")
    return "\n".join(rows)


def shelf_28(stock):
    """Return the units on shelf 28."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S28"):
            units += qty
    return units


def shelf_29(stock):
    """Return the units on shelf 29."""
    units = 0
    for sku, qty in stock.items():
        if sku.startswith("S29"):
            units += qty
    return units
