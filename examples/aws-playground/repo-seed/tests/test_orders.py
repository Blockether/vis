import pytest

from shop.orders import Line, discount, subtotal, to_csv


def test_subtotal_sums_the_lines():
    assert subtotal([Line("pen", 150, 2), Line("book", 1200)]) == 1500


def test_discount_rounds_down():
    assert discount(999, 10) == 899


def test_discount_refuses_a_bad_percent():
    with pytest.raises(ValueError):
        discount(100, 101)


def test_csv_has_a_header_and_one_row_for_each_line():
    assert to_csv([Line("pen", 150, 2)]) == "product,unit_cents,quantity,total_cents\npen,150,2,300\n"
