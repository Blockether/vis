match (command.split()):
    case [ action ]:
        pass
    case [action,obj]:
        pass
    case Point(x = 0, y = 0):
        print("Origin")
    case {"type":"click", "position":(x,y)}:
        handle_click(x,y)
    case str()|bytes():
        pass
    case [first, *others] if len(others)>2:
        pass
    case {"kind": "long", "first_value": first_value, "second_value": second_value, **rest}:
        pass
    case _: pass
