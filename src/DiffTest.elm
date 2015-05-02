module DiffTest where

import ElmTest.Assertion exposing (..)
import ElmTest.Test exposing (..)

import Diff exposing (..)

type TestType
  = Foo Int
  | Bar String

original = """Brian
Sohie
Oscar
Stella
Takis
"""

changed = """BRIAN
Stella
Frosty
Takis
"""

suite = Suite "Foo"
  [ test "diffLines" <|
      diffLines "a\nb\nc" "a\nb1\nxxx\n"
      `assertEqual`
      [ NoChange "a\n"
      , Changed "b\nc" "b1\nxxx\n"
      ]
  , test "diffChars" <|
      diffChars "a\nb\nc" "a\nb1\nxxx\n"
      `assertEqual`
      [ NoChange "a\nb"
      , Added "1"
      , NoChange "\n"
      , Changed "c" "xxx\n"
      ]
  , test "empty strings" <|
      diffChars "" ""
      `assertEqual`
      []
  , test "single char is added" <|
      diffChars "" "b"
      `assertEqual`
      [ Added "b" ]
  , test "single char is removed" <|
      diffChars "a" ""
      `assertEqual`
      [ Removed "a" ]
  , test "single char that is equal" <|
      diffChars "a" "a"
      `assertEqual`
      [ NoChange "a" ]
  , test "single char that is changed" <|
      diffChars "a" "b"
      `assertEqual`
      [ Changed "a" "b" ]
  , test "second char is added" <|
      diffChars "a" "ab"
      `assertEqual`
      [ NoChange "a"
      , Added "b"
      ]
  , test "second char is equal" <|
      diffChars "ab" "1b"
      `assertEqual`
      [ Changed "a" "1"
      , NoChange "b"
      ]
  , test "two chars are equal" <|
      diffChars "ab" "ab"
      `assertEqual`
      [ NoChange "ab" ]
  , test "two chars are changed" <|
      diffChars "ab" "12"
      `assertEqual`
      [ Changed "ab" "12" ]
  , test "two chars are added" <|
      diffChars "" "ab"
      `assertEqual`
      [ Added "ab" ]
  , test "two chars are removed" <|
      diffChars "ab" ""
      `assertEqual`
      [ Removed "ab" ]
  , test "first char is removed" <|
      diffChars "ab" "b"
      `assertEqual`
      [ Removed "a"
      , NoChange "b"
      ]
  -- , test "andThen" <|
  --     (diffLines original changed |> andThen diffChars)
  --     `assertEqual`
  --     [ Changed "Brian\nSohie\nOscar" "BRIAN"
  --     , NoChange "\nStella\n"
  --     , Added "Frosty\n"
  --     , NoChange "Takis\n"
  --     ]
  , test "performance of diffLines" <|
      diffLines original changed
      `assertEqual`
      [ Changed "Brian\nSohie\nOscar\n" "BRIAN\n"
      , NoChange "Stella\n"
      , Added "Frosty\n"
      , NoChange "Takis\n"
      ]
  , test "performance of diffChars" <|
      diffChars original changed
      `assertEqual`
      [ NoChange "B"
      , Changed "rian\nSohie\nOscar" "RIAN"
      , NoChange "\nStella"
      , Added "\nFrosty"
      , NoChange "\nTakis\n"
      ]
  , test "performance with no matches" <|
      diffChars "abcdefghijklmnopqrstuvwxyz" "1234567890"
      `assertEqual`
      [ Changed "abcdefghijklmnopqrstuvwxyz" "1234567890"
      ]
  , test "performance of DiffChars" <|
    diffChars
      "Expected Fail (Diff (\"Expected Foo { x = 1 } to equal Bar (Just \"hi\")\")"
      "Expected Foo { x = 1 } to equal Bar (Just \"hi\")"
      `assertEqual`
      [ Removed ("Expected Fail (Diff (\"")
      , NoChange ("Expected Foo { x = 1 } to equal Bar (Just \"hi")
      , Removed "\")" -- TODO: why aren't the last two changes flipped?
      , NoChange "\")"
      ]
  ]
