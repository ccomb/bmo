#!/usr/bin/env python3

from math import sqrt
import numpy as np
from scipy.optimize import minimize

# current situation
P = 16  # NBPROD
S = 7  # NBSUPP
B = 39000  # BRUT
N = 290  # NDF
C = 120000  # CHARF
T = 600  # TJM
F = 0.45  # %FACT

# optimization method
methods = [
    'Nelder-Mead',
    'Powell',
    'CG',
    'BFGS',
#    'Newton-CG',
    'L-BFGS-B',
    'TNC',
    'COBYLA',
    'SLSQP',
    'trust-constr',
#    'dogleg',
#    'trust-ncg',
#    'trust-exact',
#    'trust-krylov'
    ]


# value of F depending on other variables
# (profitability formula)
def f(p, s, b, n, c, t):
    return ((p+s)*(b*1.38+12*n)+c)/(218*t*p)


# value of F to be profitable
Fi = f(P, S, B, N, C, T)


# function to minimize
def D(x):
    p, s, b, n, c, t = x
    return sqrt(
            ((F-f(p, s, b, n, c, t))/F)**2
            + ((P-p)/P)**2
            + ((S-s)/S)**2
            + ((B-b)/B)**2
            + ((N-n)/N)**2
            + ((C-c)/C)**2
            + ((T-t)/T)**2
            )

# initial guess
Pi = 10  # NBPROD
Si = 0  # NBSUPP
Bi = 20000  # BRUT
Ni = 290  # NDF
Ci = 120000  # CHARF
Ti = 600  # TJM
x0 = np.array([Pi, Si, Bi, Ni, Ci, Ti])

for method in methods:
    res = minimize(D, x0, method=method)

    Px, Sx, Bx, Nx, Cx, Tx = res.x
    Fx = f(Px, Sx, Bx, Nx, Cx, Tx)
    print('=== METHOD = %s ===' % method)
    print('NBPROD : %s' % Px)
    print('NBSUPP : %s' % Sx)
    print('BRUT : %s' % Bx)
    print('NDF : %s' % Nx)
    print('CHARF : %s' % Cx)
    print('TJM : %s' % Tx)
    print('%%FACT : %s' % Fx)

