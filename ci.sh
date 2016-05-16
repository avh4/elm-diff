#!/bin/bash

set -ex

elm-make

cd tests
elm-make TestRunner.elm --output tests.js
node tests.js
