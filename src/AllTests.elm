module AllTests where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import DiffTest

all = Suite "avh4/elm-diff"
  [ DiffTest.suite
  ]
