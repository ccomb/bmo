#!/bin/bash

pushd $( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

if [ "$1" == "-c" ]; then
    while true; do
        inotifywait -e modify -e create -e delete src/*elm src/*/*elm src/static/* || killall less &
        sleep 0.25
        rsync -r --delete --exclude "eventstore.txt" public/ build/
        unbuffer elm make --output build/app.js src/Main.elm 2>&1 | grep -v Compiling... | less -R
    done
    export APP=app
elif [ "$1" == "-o" ]; then
    rsync -r --delete public/ build/
    elm make --optimize --output build/tmp.js src/Main.elm \
        && uglifyjs build/tmp.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
            | uglifyjs --mangle --output build/app.js \
        && rm build/tmp.js
    export APP=app.$( md5sum build/app.js | cut -d' ' -f1 )
    mv build/app.js build/${APP}.js
else
    rsync -r --delete public/ build/
    elm make --output build/app.js src/Main.elm
    export APP=app
fi
envsubst < public/index.html > build/index.html
popd
