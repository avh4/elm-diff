#!/bin/bash

set -e
set -x

elm-make
(cd tests; elm-test TestRunner.elm)
