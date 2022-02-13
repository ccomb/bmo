#!/bin/bash

if [ "$1" == "-o" ]; then
    npx elm-spa build
else
    npx elm-spa gen
    elm make --output public/dist/elm.js .elm-spa/defaults/Main.elm
fi
