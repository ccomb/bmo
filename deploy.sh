#!/bin/bash

if [ "$1" == "" ]; then echo "Need the target host as 1st argument"; exit 1;fi
if [ "$2" == "" ]; then echo "Need the root mongo password as 2nd argument"; exit 1;fi
ssh $1 "ssh-keyscan git.anybox.cloud >> ~/.ssh/known_hosts && cd services/bmo && git pull && MONGO_INITDB_ROOT_PASSWORD=$2 /opt/bin/docker-compose -f docker-compose.yml -f docker-compose.prod.yml  up -d --build"
