#!/usr/bin/env bash

mkdir -p build

npx concurrently --kill-others \
  "nodemon -w index.html -x 'cp index.html build'" \
  "nodemon -w src -e purs,js -x 'npx spago bundle-app --source-maps --minify -m Main --to ./build/index.js'" \
  "nodemon -w src -e purs -x 'npx spago bundle-app --source-maps --minify -m Worker.Simulation --to ./build/worker.js'" \
  "live-server --middleware=\"$(pwd)/headers-middleware.js\" build"

