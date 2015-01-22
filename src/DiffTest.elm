module DiffTest where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Diff (..)

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
      [ NoChange "a\n"
      , Changed "b\nc" "b1\nxxx\n"
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
      [ Changed "Brian\nSohie\nOscar" "BRIAN"
      , NoChange "\nStella\n"
      , Added "Frosty\n"
      , NoChange "Takis\n"
      ]
  ]
