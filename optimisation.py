#!/usr/bin/env python3

from math import sqrt
from scipy.optimize import minimize
from sympy import Eq, solve, symbols, lambdify, implemented_function
import ast
import numpy as np
import sympy


#formula = '218*t*p*f - (p+s)*(b*1.38+12*n)+c'

def function_to_minimize(formula, objective):
    """take :
        - a function of N variables as a string,
          representing a surface in N-1 dimensions in a space of dimension N
          such as : '218*t*p*f - (p+s)*(b*1.38+12*n)+c'
        - an objective the the function value should be equal to

    returns a tuple (f, vars, initials) where
        - f is a function representing the distance from an initial point to the surface
        - vars is the list of variables of the function,
        - initials is the list of variables representing the initial point
    """
    variables = list({node.id for node in ast.walk(ast.parse(formula)) if isinstance(node, ast.Name)})
    initial_vars = ['initial_' + v for v in variables]
    pivot = variables[0]
    
    if any(forbidden in variables for forbidden in dir() + ['symformula', 'objective', 'solution', 'pivot']):
        raise Exception("forbidden variable")
    
    symformula = eval(formula, {v: symbols(v) for v in variables})
    objective = 0
    solution = solve(Eq(symformula, objective), symbols(pivot))[0]
    distance = sympy.sqrt((symbols('initial_' + pivot)-solution)**2 + sum((symbols('initial_' + v) - symbols(v))**2 for v in variables))
    allvars = variables + initial_vars
    return lambdify(allvars, distance), variables, initial_vars



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
x0 = np.array([P, S, B, N, C, T])

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
