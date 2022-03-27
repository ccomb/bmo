#!/bin/bash
if [ "$1" == "--reload" ]; then
    pushd src
    uvicorn --host 0.0.0.0 server:api --reload
    popd
else
    pushd src
    uvicorn --host 0.0.0.0 server:api
    popd
fi
