empty_list = []
empty_dict = {}
bare_tuple = (1,)
single_tuple = (1,)
numbers = [1, 2, 3]
magic = [
    1,
    2,
    3,
]
config = {
    "name": "vis",
    "version": 1,
    "enabled": True,
    "paths": ["src", "resources", "test"],
    "extra": None,
}
matrix = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9],
    [10, 11, 12],
    [13, 14, 15],
    [16, 17, 18],
    [19, 20],
]
squares = [
    value * value
    for value in range(100)
    if value % 2 == 0 and value % 3 == 0 and value > 10
]
mapping = {
    key: value
    for key, value in zip(keys_list, values_list)
    if value is not None and key
}
unique = {
    item.identifier
    for item in collection_of_items
    if item.is_valid() and not item.is_deleted
}
first, *rest = sequence
a, b = b, a
for index, (key, value) in enumerate(
    sorted(mapping.items(), key=lambda pair: pair[1], reverse=True)
):
    pass
