import numpy as np

def f(x):
    return 1/(1+x*x)

a = -6
b = 6
lst = np.arange(a, b+1, 1.5)
print(f'# {len(lst)-1}')
print(f'{a} {b}')
for i in lst:
    print(round(f(i), 5))