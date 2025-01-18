#!/usr/bin/env bash

mkdir -p build

cp -r node_modules/bootstrap-icons/font/ ./build/

npx concurrently --kill-others \
  "nodemon -w index.html -x 'cp index.html build'" \
  "nodemon -w src -e purs -x 'npx spago bundle-app -m Main --to ./build/index.js'" \
  "nodemon -w src -e purs -x 'npx spago bundle-app -m Worker.Simulation --to ./build/worker.js'" \
  "live-server build"

