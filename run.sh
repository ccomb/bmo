#!/bin/bash
if [ "$1" == "--reload" ]; then
    uvicorn --host 0.0.0.0 optimization:api --reload
else
    uvicorn --host 0.0.0.0 optimization:api
fi
