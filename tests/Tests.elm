module Tests exposing (..)

import Test exposing (..)
import DiffTest


all : Test
all =
    describe "avh4/elm-diff"
        [ DiffTest.all
        ]
