Callers need to choose the request timeout. `_request` in client.py already takes `timeout`.

Give every function in client.py and service.py that reaches `_request`, directly or through other functions, a last parameter `timeout=DEFAULT_TIMEOUT`. Pass `timeout=timeout` as the last argument of each call on that path. In service.py, import DEFAULT_TIMEOUT from client. Do not change functions that do not reach `_request`.
