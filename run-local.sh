#!/bin/sh
# Usage:
# ./run-local.sh
# ./run-local.sh tidyverse

# Kill child process on exit
trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT

# Start dummy server
rm -Rf ".dummydata-prod"
mkdir ".dummydata-prod"
DEBUG=cranlike:* mongod --bind_ip_all --dbpath ".dummydata-prod" --logpath mongo.log --logappend & sleep 1

cd ~/workspace/r-universe/express-frontend
NODE_ENV=production npm start
