#!/usr/bin/env python3

from fastapi import FastAPI, Request
from fastapi.responses import HTMLResponse, JSONResponse
from fastapi.staticfiles import StaticFiles
from fastapi.templating import Jinja2Templates
from scipy.optimize import minimize
from sympy import Eq, solve, symbols, lambdify
import ast
import numpy as np
import os
import sympy

api = FastAPI()
api.mount("/static", StaticFiles(directory="static"), name="static")
templates = Jinja2Templates(directory=".")


@api.get("/", response_class=HTMLResponse)
async def home(request: Request):
    return templates.TemplateResponse(
        "index.html",
        {"request": request, "host": os.environ.get("HOST", "http://localhost:8000")},
    )


@api.get("/optimize", response_class=JSONResponse)
async def optimize(request: Request, formula: str = ""):
    try:
        fdistance, fpivot, pivot, vars_wo_pivot = function_to_minimize(formula)
    except Exception:
        return {"status": "Error : Invalid formula"}

    # just query the formula
    if len(set(request.query_params.keys())) < len(
        set(["formula", pivot] + vars_wo_pivot)
    ):
        return {
            "status": "success",
            "result": {k: None for k in [pivot] + vars_wo_pivot},
        }

    try:
        initialpoint_values = [
            float(i[1])
            for i in sorted(request.query_params.items())
            if i[0] in [pivot] + vars_wo_pivot
        ]
        # we take the initial point as the initial guess.
        # The pivot variable will be recomputed anyway
        # so that the initial guess in on the surface
        initialguess_values = [
            float(i[1])
            for i in sorted(request.query_params.items())
            if i[0] in vars_wo_pivot
        ]
        # compute the distance formula
        fdistance, fpivot, pivot, vars_wo_pivot = function_to_minimize(
            formula, initialpoint_values
        )
        # find the shortest distance
        x0 = np.array(initialguess_values)  # without pivot value
        minval = minimize(fdistance, x0, method="Powell")
        pivotval = fpivot(*minval.x)
        return {
            "status": "success",
            "result": {
                k: v
                for k, v in zip([pivot] + vars_wo_pivot, [pivotval] + list(minval.x))
            },
        }
    except Exception:
        return {
            "status": "Error : Could not find an optimal solution",
        }


# formula = '218*t*p*f - (p+s)*(b*1.38+12*n)+c = 0'


def function_to_minimize(formula, initialpoint_values=[]):
    """take :
        - a formula with N variables as a string,
          representing a surface of dimension N-1 in a space of dimension N
          such as : '218*t*p*f - (p+s)*(b*1.38+12*n)+c=0'
        - an initial point

    returns a tuple (fdistance, fpivot, pivot, varsd where
        - fdistance is a function representing the distance
          from an initial point to the surface
        - fpivot is the function to compute the pivot from the other variables
        - pivot is the variable extracted from the formula
        - vars is the list of variables of the function without the pivot
    """
    left, right = [x.strip() for x in formula.split("=")]
    leftvars = sorted(
        list(
            {
                node.id
                for node in ast.walk(ast.parse(left))
                if isinstance(node, ast.Name)
            }
        )
    )
    rightvars = sorted(
        list(
            {
                node.id
                for node in ast.walk(ast.parse(right))
                if isinstance(node, ast.Name)
            }
        )
    )
    variables = leftvars + rightvars
    initial_vars = ["initial_" + v for v in variables]
    pivot = variables[0]  # TODO try without explicit pivot
    vars_wo_pivot = [v for v in variables if v != pivot]

    # symbolic representation of the formula
    symleft = eval(left, {v: symbols(v) for v in leftvars})
    symright = eval(right, {v: symbols(v) for v in rightvars})
    # formula of the pivot depending on the other variables
    sympivot = solve(Eq(symleft, symright), symbols(pivot))[0]
    fpivot = lambdify(vars_wo_pivot, sympivot)
    symdistance = sympy.sqrt(
        (((symbols("initial_" + pivot) - sympivot)) / symbols("initial_" + pivot)) ** 2
        + sum(
            ((symbols("initial_" + v) - symbols(v)) / symbols("initial_" + v)) ** 2
            for v in vars_wo_pivot
        )
    )

    f = lambdify(vars_wo_pivot + initial_vars, symdistance)

    def fdistance(x):
        kw = {z[0]: z[1] for z in zip(initial_vars, initialpoint_values)}
        for k, w in zip(vars_wo_pivot, x):
            kw[k] = w
        return f(**kw)

    return fdistance, fpivot, pivot, vars_wo_pivot
