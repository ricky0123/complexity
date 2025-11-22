#!/usr/bin/env bash

rm -rf build
mkdir build
cp index.html build
npx spago bundle-app --source-maps -m Main --to ./build/index.js
npx spago bundle-app --source-maps -m Worker.Simulation --to ./build/worker.js
