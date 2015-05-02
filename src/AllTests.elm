module AllTests where

import ElmTest.Assertion exposing (..)
import ElmTest.Test exposing (..)

import DiffTest

all = Suite "avh4/elm-diff"
  [ DiffTest.suite
  ]
