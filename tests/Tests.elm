module Tests exposing (..)

import ElmTest exposing (..)
import DiffTest


all : Test
all =
    suite "avh4/elm-diff"
        [ DiffTest.all
        ]
