#!/usr/bin/env python3

from collections import OrderedDict
from fastapi import FastAPI, Request
from fastapi.responses import HTMLResponse, JSONResponse
from fastapi.staticfiles import StaticFiles
from fastapi.templating import Jinja2Templates
from hashlib import sha1
from pymongo import MongoClient
from scipy.optimize import minimize
from sympy import Eq, solve, symbols, lambdify
import ast
import json
import math
import numpy as np
import os
import sympy

api = FastAPI()
api.mount("/static", StaticFiles(directory="static"), name="static")
templates = Jinja2Templates(directory=".")

MONGOPASSWORD = os.environ.get("MONGO_INITDB_ROOT_PASSWORD", "root")
CLIENT = MongoClient("/tmp/mongodb-27017.sock", username="root", password=MONGOPASSWORD)


@api.get("/optimize", response_class=JSONResponse)
async def optimize(request: Request, formula: str = ""):
    try:
        fdistance, fpivot, pivot, vars_wo_pivot = function_to_minimize(formula)
        if "formula" in [pivot] + vars_wo_pivot:
            return {"status": "Error : Invalid variable name 'formula' in the formula"}
    except Exception:
        return {"status": "Error : Invalid formula"}

    # just query the formula
    if len(set(request.query_params.keys())) < len(
        set(["formula", pivot] + vars_wo_pivot)
    ):
        return {
            "status": "success",
            "id": None,
            "point": {k: None for k in [pivot] + vars_wo_pivot},
        }
    try:
        initial_point = {
            k: float(v)
            for k, v in sorted(request.query_params.items())
            if k in [pivot] + vars_wo_pivot
        }
        coef_point = {"coef_" + k: 1.0 for k in [pivot] + vars_wo_pivot}
        coef_point.update(
            {
                k: float(v)
                for k, v in sorted(request.query_params.items())
                if k in ["coef_" + i for i in vars_wo_pivot]
            }
        )
    except Exception:
        return {"status": "Error : One initial value cannot be converted to a float"}

    # we take the initial point as the initial guess.
    # The pivot variable will be recomputed anyway
    # so that the initial guess in on the surface
    initialguess = initial_point.copy()
    initialguess.pop(pivot)

    try:
        # compute the distance formula
        fdistance, fpivot, pivot, vars_wo_pivot = function_to_minimize(
            formula, initial_point, coef_point
        )
    except Exception:
        return {"status": "Error : I cannot use the provided input"}

    try:
        # find the shortest distance
        x0 = np.array(list(OrderedDict(initialguess).values()))  # without pivot value
        minval = minimize(fdistance, x0, method="Powell")
        pivotval = fpivot(*minval.x)
    except Exception:
        return {"status": "Error : Optimization failed"}

    closest_solution = {
        k: v for k, v in zip([pivot] + vars_wo_pivot, [pivotval] + list(minval.x))
    }

    # store the result
    try:
        _id = store(
            formula=formula,
            initial_point=initial_point,
            coef_point=coef_point,
            closest_solution=closest_solution,
        )
    except Exception:
        return {"status": "Error : Could not insert data to the database"}

    return {
        "id": _id,
        "status": "success",
        "point": closest_solution,
    }


# Redirect everything else to the frontend
@api.get("/{rest_of_path:path}", response_class=HTMLResponse)
async def home(request: Request):
    flags = {
        "host": os.environ.get("HOST", "http://localhost:8000"),
        "formula": "",
        "initial_point": {},
        "coef_point": {},
        "closest_solution": {},
    }
    try:
        _id = request.path_params.get("rest_of_path", "")
        doc = CLIENT.bmo.optim.find_one({"_id": _id})
        flags["formula"] = doc.get("formula", "")
        flags["initial_point"] = doc.get("initial_point")
        flags["coef_point"] = doc.get("initial_point")
        flags["closest_solution"] = doc.get("closest_solution")
    except Exception:
        pass
    return templates.TemplateResponse(
        "index.html", {"request": request, "flags": flags}
    )


# formula = '218*t*p*f - (p+s)*(b*1.38+12*n)+c = 0'


def function_to_minimize(formula, initial_point=dict(), coef_point=dict()):
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
    # variables in the formula
    variables = sorted(leftvars + rightvars)

    # variables for initial values
    initial_vars = ["initial_" + v for v in variables]

    # variables for the difficulty to change variables
    coef_vars = ["coef_" + v for v in variables]

    # arbitrarily choose the 1st variable as the pivot to extract
    # TODO try without explicit pivot
    pivot = variables[0]
    vars_wo_pivot = [v for v in variables if v != pivot]

    # symbolic representation of the formula
    symleft = eval(left, {v: symbols(v) for v in leftvars})
    symright = eval(right, {v: symbols(v) for v in rightvars})

    # compute the formula and function to get the pivot from other variables
    sympivot = solve(Eq(symleft, symright), symbols(pivot))[0]
    fpivot = lambdify(vars_wo_pivot, sympivot)

    # formula and function to compute the distance from the initial point to the curve
    # FIXME the coef is not active on the pivot
    symdistance = sympy.sqrt(
        (
            symbols("coef_" + pivot)
            * (symbols("initial_" + pivot) - sympivot)
            / symbols("initial_" + pivot)
        )
        ** 2
        + sum(
            (
                symbols("coef_" + v)
                * (symbols("initial_" + v) - symbols(v))
                / symbols("initial_" + v)
            )
            ** 2
            for v in vars_wo_pivot
        )
    )
    print(symdistance)
    f = lambdify(sorted(vars_wo_pivot + initial_vars + coef_vars), symdistance)

    def fdistance(x):
        kw = {"initial_" + k: v for k, v in initial_point.items()}
        kw.update(coef_point)
        for k, w in zip(vars_wo_pivot, x):
            kw[k] = w
        return f(**kw)

    return fdistance, fpivot, pivot, vars_wo_pivot


def store(formula, initial_point, coef_point, closest_solution):
    """cache the formula and variables to mongo and return the id generated by mongo"""
    doc = {
        "formula": formula,
        "initial_point": initial_point,
        "closest_point": closest_solution,
    }
    # trunk the sha1 depending on the number of record to keep short urls
    hashtrunksize = 6 + len(
        str(int(math.sqrt(CLIENT.bmo.optim.estimated_document_count())))
    )
    doc["_id"] = sha1(
        (
            formula
            + json.dumps(initial_point, sort_keys=True)
            + json.dumps(coef_point, sort_keys=True)
            + json.dumps(closest_solution, sort_keys=True)
        ).encode("utf-8")
    ).hexdigest()[:hashtrunksize]
    CLIENT.bmo.optim.replace_one({"_id": doc["_id"]}, doc, upsert=True)
    return doc["_id"]
