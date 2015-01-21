module Diff
  ( diffChars
  -- , diffWords, diffWordsWithSpace, diffLines, diffSentences
  -- , diffInternalStructure
  , Change(..)
  ) where

{-| Provides and Elm interface to the [npm 'diff' module](https://www.npmjs.com/package/diff).

# Types and Constructors
@docs Change

# Diffing text
@docs diffChars, diffWords, diffLines

-}

import List
import String

type Change
  = NoChange String
  | Changed String String
  | Added String
  | Removed String

merge : List Change -> List Change
merge list = case list of
  (Added a :: Added b :: rest) -> Added (a++b) :: rest
  (Removed a :: Removed b :: rest) -> Removed (a++b) :: rest
  (NoChange a :: NoChange b :: rest) -> NoChange (a++b) :: rest
  (Changed a1 a2 :: Changed b1 b2 :: rest)  -> Changed (a1++b1) (a2++b2) :: rest
  _ -> list

step : (List String) -> (List String) -> List Change -> List Change
step aa bb acc = case (aa,bb) of
  ([], []) -> []
  ([], b::bb') -> merge (Added b :: step aa bb' [])
  (a::aa', []) -> merge (Removed a :: step aa' bb [])
  (a::aa', b::bb') -> if
    | a == b -> merge (NoChange a :: step aa' bb' [])
    | otherwise -> merge (Changed a b :: step aa' bb' [])

{-| Diffs two blocks of text, comparing character by character.
-}
diffChars : String -> String -> List Change
diffChars a b = step (String.split "" a) (String.split "" b) []

-- {-| Diffs two blocks of text, comparing comparing word by word, ignoring whitespace.
-- -}
-- diffWords : String -> String -> List Change
-- diffWords = Native.JsDiff.diffWords
--
-- {-| Diffs two blocks of text, comparing comparing word by word, treating whitespace as significant.
-- -}
-- diffWordsWithSpace : String -> String -> List Change
-- diffWordsWithSpace = Native.JsDiff.diffWordsWithSpace
--
-- {-| Diffs two blocks of text, comparing line by line.
-- -}
-- diffLines : String -> String -> List Change
-- diffLines = Native.JsDiff.diffLines
--
-- {-| Diffs two blocks of text, comparing sentence by sentence.
-- -}
-- diffSentences : String -> String -> List Change
-- diffSentences = Native.JsDiff.diffSentences
--
-- {-| Diffs two Elm objects, comparing their internal structure.
-- -}
-- diffInternalStructure : a -> a -> List Change
-- diffInternalStructure = Native.JsDiff.diffInternalStructure
