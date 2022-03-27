FROM debian:11 AS build

WORKDIR /srv
RUN apt-get update \
    && apt-get install -y --no-install-recommends rsync curl gzip npm uglifyjs \
    && curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz \
    && gunzip elm.gz \
    && chmod +x elm \
    && mv elm /usr/local/bin/

COPY . /srv/
RUN ./build.sh -o


FROM debian:11

ENV DEBIAN_FRONTEND noninteractive
ENV LASTBUILD 2021100601
ENV LANG C.UTF-8

RUN set -x; \
    apt-get update \
    && apt-get install -y --no-install-recommends \
        python3-aiofiles \
        python3-jinja2 \
        python3-pymongo \
        python3-uvicorn \
        uvicorn \
        python3-fastapi \
        python3-numpy \
        python3-scipy \
        python3-sympy \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

COPY src/server.py /srv/src/server.py
COPY public /srv/public
COPY content /srv/content
COPY --from=build /srv/build/ /srv/build/
WORKDIR /srv/src
EXPOSE 8000
CMD ["uvicorn", "--host", "0.0.0.0", "server:api"]
