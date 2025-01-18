#!/usr/bin/env bash

rm -rf build
mkdir build
cp index.html build
npx spago bundle-app -m Main --to ./build/index.js
npx spago bundle-app -m Worker.Simulation --to ./build/worker.js
cp -r node_modules/bootstrap-icons/font/ ./build/