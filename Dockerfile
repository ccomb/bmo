from debian:11

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

COPY optimization.py /srv/
COPY index.html /srv/
COPY static /srv/static
WORKDIR /srv
EXPOSE 8000
CMD ["uvicorn", "--host", "0.0.0.0", "optimization:api"]
