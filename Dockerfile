from debian:11

ENV DEBIAN_FRONTEND noninteractive
ENV LASTBUILD 2021100601
ENV LANG C.UTF-8

RUN set -x; \
    apt-get update \
    && apt-get install -y --no-install-recommends \
	python3-uvicorn \
        python3-fastapi \
        python3-numpy \
        python3-scipy \
        python3-sympy \
	uvicorn \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

COPY optimization.py /srv/

EXPOSE 8000
CMD ["uvicorn optimization:api"]
