#!/bin/bash
if [ "$1" == "--reload" ]; then
    pushd src
    uvicorn --host 0.0.0.0 server:api --reload
    popd
elif [ "$1" == "--website" ]; then
    npx elm-spa server
else
    pushd src
    uvicorn --host 0.0.0.0 server:api
    popd
fi
