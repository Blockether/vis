def add(a, b): return a + b
def sub(a, b): return a - b
def mul(a, b): return a * b
def div(a, b): return a / b
def mean(xs): return sum(xs) / len(xs)
def compute_total(items, weights):
    return sum(x * weights[i] for i, x in enumerate(items))
def maxv(xs): return max(xs)
def minv(xs): return min(xs)
def clamp(x, lo, hi): return max(lo, min(hi, x))
def square(x): return x * x
def cube(x): return x * x * x
def negate(x): return -x
