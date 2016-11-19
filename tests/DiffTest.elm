module DiffTest exposing (all)

import Test exposing (..)
import Diff exposing (..)
import Expect


type TestType
    = Foo Int
    | Bar String


original : String
original =
    """Brian
Sohie
Oscar
Stella
Takis
"""


changed : String
changed =
    """BRIAN
Stella
Frosty
Takis
"""


all : Test
all =
    describe "Foo"
        [ test "diffLines" <|
            \() ->
                diffLines "a\nb\nc" "a\nb1\nxxx\n"
                    |> Expect.equal
                        [ NoChange "a\n"
                        , Changed "b\nc" "b1\nxxx\n"
                        ]
        , test "diffChars" <|
            \() ->
                diffChars "a\nb\nc" "a\nb1\nxxx\n"
                    |> Expect.equal
                        [ NoChange "a\nb"
                        , Added "1"
                        , NoChange "\n"
                        , Changed "c" "xxx\n"
                        ]
        , test "empty strings" <|
            \() ->
                diffChars "" ""
                    |> Expect.equal []
        , test "single char is added" <|
            \() -> diffChars "" "b" |> Expect.equal [ Added "b" ]
        , test "single char is removed" <|
            \() -> diffChars "a" "" |> Expect.equal [ Removed "a" ]
        , test "single char that is equal" <|
            \() -> diffChars "a" "a" |> Expect.equal [ NoChange "a" ]
        , test "single char that is changed" <|
            \() -> diffChars "a" "b" |> Expect.equal [ Changed "a" "b" ]
        , test "second char is added" <|
            \() ->
                diffChars "a" "ab"
                    |> Expect.equal
                        [ NoChange "a"
                        , Added "b"
                        ]
        , test "second char is equal" <|
            \() ->
                diffChars "ab" "1b"
                    |> Expect.equal
                        [ Changed "a" "1"
                        , NoChange "b"
                        ]
        , test "two chars are equal" <|
            \() -> diffChars "ab" "ab" |> Expect.equal [ NoChange "ab" ]
        , test "two chars are changed" <|
            \() -> diffChars "ab" "12" |> Expect.equal [ Changed "ab" "12" ]
        , test "two chars are added" <|
            \() -> diffChars "" "ab" |> Expect.equal [ Added "ab" ]
        , test "two chars are removed" <|
            \() -> diffChars "ab" "" |> Expect.equal [ Removed "ab" ]
        , test "first char is removed" <|
            \() ->
                diffChars "ab" "b"
                    |> Expect.equal
                        [ Removed "a"
                        , NoChange "b"
                        ]
          -- , test "andThen" <|
          --     (diffLines original changed |> andThen diffChars)
          --     `          --     [ Changed "Brian\nSohie\nOscar" "BRIAN"
          --     , NoChange "\nStella\n"
          --     , Added "Frosty\n"
          --     , NoChange "Takis\n"
          --     ]
        , test "performance of diffLines" <|
            \() ->
                diffLines original changed
                    |> Expect.equal
                        [ Changed "Brian\nSohie\nOscar\n" "BRIAN\n"
                        , NoChange "Stella\n"
                        , Added "Frosty\n"
                        , NoChange "Takis\n"
                        ]
        , test "performance of diffChars" <|
            \() ->
                diffChars original changed
                    |> Expect.equal
                        [ NoChange "B"
                        , Changed "rian\nSohie\nOscar" "RIAN"
                        , NoChange "\nStella"
                        , Added "\nFrosty"
                        , NoChange "\nTakis\n"
                        ]
        , test "performance with no matches" <|
            \() ->
                diffChars "abcdefghijklmnopqrstuvwxyz" "1234567890"
                    |> Expect.equal
                        [ Changed "abcdefghijklmnopqrstuvwxyz" "1234567890"
                        ]
        , test "performance of DiffChars" <|
            \() ->
                diffChars "Expected Fail (Diff (\"Expected Foo { x = 1 } to equal Bar (Just \"hi\")\")"
                    "Expected Foo { x = 1 } to equal Bar (Just \"hi\")"
                    |> Expect.equal
                        [ Removed ("Expected Fail (Diff (\"")
                        , NoChange ("Expected Foo { x = 1 } to equal Bar (Just \"hi")
                        , Removed "\")"
                          -- TODO: why aren't the last two changes flipped?
                        , NoChange "\")"
                        ]
        ]
