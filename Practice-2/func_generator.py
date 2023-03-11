import numpy as np

def f(x):
    return 1+x**2

for i in np.arange(-6, 7, 1):
    print(round(f(i), 5))