#!/usr/bin/env python3

from fastapi import FastAPI, Request
from fastapi.responses import HTMLResponse, FileResponse, JSONResponse
from fastapi.staticfiles import StaticFiles
from math import sqrt
from scipy.optimize import minimize
from sympy import Eq, solve, symbols, lambdify
import ast
import numpy as np
import sympy


api = FastAPI()
api.mount("/static", StaticFiles(directory="static"), name="static")


@api.get("/", response_class=HTMLResponse)
async def standalone():
    return FileResponse("index.html")


@api.get("/optimize", response_class=JSONResponse)
async def optimize(request: Request, formula: str = "", objective: float = 0.0):
    params = request.query_params.keys()
    if set(["formula", "objective"]) == set(params):
        # we're just querying the variables
        try:
            f, variables, initial_vars = function_to_minimize(formula, objective)
            return {"vars": variables, "initial": initial_vars}
        except:
            return {"status": "error", "error": "Invalid formula"}
    else:
        try:
            initial_values = [
                float(i[1])
                for i in sorted(request.query_params.items())
                if i[0].startswith("initial_")
            ]
            # we run the optimization
            f, variables, initial_vars = function_to_minimize(
                formula, objective, initial_values
            )
            x0 = np.array(initial_values)
            minval = minimize(f, x0, method="Powell")
            return {
                "status": "success",
                "result": {k: v for k, v in zip(initial_vars, minval.x)},
            }
        except:
            return {"status": "error", "error": "Could not find an optimal solution"}


# formula = '218*t*p*f - (p+s)*(b*1.38+12*n)+c'


def function_to_minimize(formula, objective, initial_values):
    """take :
        - a formula with N variables as a string,
          representing a surface of dimension N-1 in a space of dimension N
          such as : '218*t*p*f - (p+s)*(b*1.38+12*n)+c'
        - an objective the the function value should be equal to

    returns a tuple (f, vars, initial_vars) where
        - f is a function representing the distance from an initial point to the surface
        - vars is the list of variables of the function,
        - initial_vars is the list of variable names representing the initial point
    """
    variables = sorted(
        list(
            {
                node.id
                for node in ast.walk(ast.parse(formula))
                if isinstance(node, ast.Name)
            }
        )
    )
    initial_vars = ["initial_" + v for v in variables]
    pivot = variables[0]  # TODO try without explicit pivot

    if any(
        forbidden in variables
        for forbidden in dir() + ["symformula", "objective", "solution", "pivot"]
    ):
        raise Exception("forbidden variable")

    symformula = eval(formula, {v: symbols(v) for v in variables})
    objective = 0
    solution = solve(Eq(symformula, objective), symbols(pivot))[0]
    distance = sympy.sqrt(
        (symbols("initial_" + pivot) - solution) ** 2
        + sum((symbols("initial_" + v) - symbols(v)) ** 2 for v in variables)
    )
    allvars = variables + initial_vars

    f = lambdify(allvars, distance)

    def g(x):
        kw = {z[0]: z[1] for z in zip(initial_vars, initial_values)}
        for k, w in zip(variables, x):
            kw[k] = w
        return f(**kw)

    return g, variables, initial_vars
