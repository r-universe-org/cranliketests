#!/bin/sh
# Usage:
# ./run-local.sh
# ./run-local.sh tidyverse

# Kill child process on exit
trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT

# Start dummy server
rm -Rf ".dummydata-prod"
mkdir ".dummydata-prod"
DEBUG=cranlike:* mongod --port 3993 --bind_ip_all --dbpath ".dummydata-prod" --logpath mongo.log --logappend & sleep 1

cd ~/workspace/r-universe/express-frontend
CRANLIKE_MONGODB_PORT=3993 NODE_ENV=production npm start
