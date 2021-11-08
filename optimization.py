#!/usr/bin/env python3

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
        initial_point = {
            k: v for k, v in zip([pivot] + vars_wo_pivot, initialpoint_values)
        }

        closest_solution = {
            k: v for k, v in zip([pivot] + vars_wo_pivot, [pivotval] + list(minval.x))
        }
        # store the result
        try:
            _id = store(
                formula=formula,
                initial_point=initial_point,
                closest_solution=closest_solution,
            )
        except Exception:
            return {"status": "Error : Could not insert data to the database"}

        return {
            "id": _id,
            "status": "success",
            "point": closest_solution,
        }
    except Exception:
        return {
            "status": "Error : Could not find an optimal solution",
        }


# Redirect everything else to the frontend
@api.get("/{rest_of_path:path}", response_class=HTMLResponse)
async def home(request: Request):
    flags = {
        "host": os.environ.get("HOST", "http://localhost:8000"),
        "formula": "",
        "initial_point": {},
        "closest_solution": {},
    }
    try:
        _id = request.path_params.get("rest_of_path", "")
        doc = CLIENT.bmo.optim.find_one({"_id": _id})
        flags["formula"] = doc.get("formula", "")
        flags["initial_point"] = doc.get("initial_point")
        flags["closest_solution"] = doc.get("closest_solution")
    except Exception:
        pass
    return templates.TemplateResponse(
        "index.html", {"request": request, "flags": flags}
    )


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


def store(formula, initial_point, closest_solution):
    """store the dict and return the id generated by mongo"""
    doc = {
        "formula": formula,
        "initial_point": initial_point,
        "closest_point": closest_solution,
    }
    doc["_id"] = sha1(
        (
            formula
            + json.dumps(initial_point, sort_keys=True)
            + json.dumps(closest_solution, sort_keys=True)
        ).encode("utf-8")
    ).hexdigest()
    CLIENT.bmo.optim.replace_one({"_id": doc["_id"]}, doc, upsert=True)
    return doc["_id"]
