module DiffTest where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Diff (..)

type TestType
  = Foo Int
  | Bar String

suite = Suite "Foo"
  -- [ test "diffLines" <|
  --     diffLines "a\nb\nc" "a\nb1\nxxx\n"
  --     `assertEqual`
  --     [ NoChange "a\n"
  --     , Added "b1\nxxx\n"
  --     , Removed "b\nc"
  --     ]
  [ test "diffChars" <|
      diffChars "a\nb\nc" "a\nb1\nxxx\n"
      `assertEqual`
      [ NoChange "a\nb"
      , Added "1"
      , NoChange "\n"
      , Added "xxx\n"
      , Removed "c"
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
  -- , test "diffWords" <|
  --     diffWords "b\nc" "b1 \nxxx\n"
  --     `assertEqual`
  --     [ Added "b1"
  --     , Removed "b"
  --     , NoChange " \n"
  --     , Added "xxx\n"
  --     , Removed "c"
  --     ]
  -- , test "diffWordsWithSpace" <|
  --     diffWordsWithSpace "b\nc" "b1 \nxxx\n"
  --     `assertEqual`
  --     [ Added "b1 \nxxx"
  --     , Removed "b"
  --     , NoChange "\n"
  --     , Removed "c"
  --     ]
  -- , test "diffSentences" <|
  --     diffSentences "b c. a." "b1. a."
  --     `assertEqual`
  --     [ Added "b1."
  --     , Removed "b c."
  --     , NoChange " a."
  --     ]
  -- , test "diffInternalStructure" <|
  --     diffInternalStructure (Foo 7) (Bar "baz")
  --     `assertEqual`
  --     [ NoChange "{\n"
  --     , Added "  \"_0\": \"baz\",\n  \"ctor\": \"Bar\"\n"
  --     , Removed "  \"_0\": 7,\n  \"ctor\": \"Foo\"\n"
  --     , NoChange "}"
  --     ]
  ]
