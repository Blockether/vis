Update the warehouse code:

1. In inventory.py, set DEFAULT_TIMEOUT to 10, and change the README line about the timeout to match.
2. reserve and release must raise ValueError("qty must be positive") when qty is 0 or less, before they change the stock.
3. Rename the `count` key that report returns to `total`. Update every reader of that key in inventory.py and the README. Keep other `count` keys.
4. Delete legacy_export.
