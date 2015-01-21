module Diff
  ( diffChars, diffLines
  , Change(..)
  ) where

{-| Functions to compare strings to produce a list of changes.  This is an
implementation of the [Hunt-McIlroy](http://en.wikipedia.org/wiki/Hunt%E2%80%93McIlroy_algorithm)
diff algorithm.

# Types and Constructors
@docs Change

# Diffing strings
@docs diffChars, diffLines

-}

import List
import String

type Change
  = NoChange String
  | Changed String String
  | Added String
  | Removed String

merge : Int -> Change -> (Int,List Change) -> (Int,List Change)
merge add next (score,list) = (add+score, case (next, list) of
  (Added a, Added b :: rest) -> Added (a++b) :: rest
  -- TODO: Added, Removed? (probably not)
  -- TODO: Added, Changed?
  (Removed a, Removed b :: rest) -> Removed (a++b) :: rest
  (Removed a, Added b :: rest) -> Changed a b :: rest
  (Removed a, Changed b1 b2 :: rest) -> Changed (a++b1) b2 :: rest
  (NoChange a, NoChange b :: rest) -> NoChange (a++b) :: rest
  -- (Changed a1 a2, Changed b1 b2 :: rest)  -> Changed (a1++b1) (a2++b2) :: rest
  _ -> (next::list))

step : (List String) -> (List String) -> (Int,List Change)
step aa bb = case (aa,bb) of
  ([], []) -> (0,[])
  ([], b::bb') -> merge 0 (Added b) (step aa bb')
  (a::aa', []) -> merge 0 (Removed a) (step aa' bb)
  (a::aa', b::bb') -> if
    | a == b -> merge 1 (NoChange a) (step aa' bb')
    | otherwise ->
      let
          (ls,l) = merge 0 (Added b) (step aa bb')
          (rs,r) = merge 0 (Removed a) (step aa' bb)
      in
        if
          | ls > rs -> (ls,l)
          | otherwise -> (rs,r)

diff : (String -> List String) -> String -> String -> List Change
diff tokenize a b = step (tokenize a) (tokenize b) |> snd

{-| Diffs two strings, comparing character by character.
-}
diffChars : String -> String -> List Change
diffChars = diff (String.split "")

tokenizeLines s =
  let
      tokens = String.split "\n" s
      n = List.length tokens
  in tokens
    |> List.indexedMap (\i s -> if i < n-1 then s ++ "\n" else s)

{-| Diffs two strings, comparing line by line.
-}
diffLines : String -> String -> List Change
diffLines = diff tokenizeLines
