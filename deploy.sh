#!/bin/bash

./build.sh -o
ssh $1 "cd services/bmo && git pull && /opt/bin/docker-compose up -d --build"