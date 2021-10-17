#!/bin/bash

ssh $1 "cd services/bmo && git pull && /opt/bin/docker-compose up -d --build"
