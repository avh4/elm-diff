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

mergeChanges : Change -> List Change -> List Change
mergeChanges next list = case (next, list) of
  (Removed a, Added b :: rest) -> Changed a b :: rest
  _ -> (next::list)

mergeAll : Change -> List Change -> List Change
mergeAll next list = case (next, list) of
  (Added a, Added b :: rest) -> Added (a++b) :: rest
  -- TODO: Added, Removed? (probably not)
  -- TODO: Added, Changed?
  (Removed a, Removed b :: rest) -> Removed (a++b) :: rest
  (Removed a, Added b :: rest) -> Changed a b :: rest
  (Removed a, Changed b1 b2 :: rest) -> Changed (a++b1) b2 :: rest
  (NoChange a, NoChange b :: rest) -> NoChange (a++b) :: rest
  (Changed a1 a2, Changed b1 b2 :: rest)  -> Changed (a1++b1) (a2++b2) :: rest
  _ -> (next::list)

sum : Int -> Change -> (Int,List Change) -> (Int,List Change)
sum add next (score,list) = (add+score, (next::list))

step : (List String) -> (List String) -> (Int,List Change)
step aa bb = case (aa,bb) of
  ([], []) -> (0,[])
  ([], b::bb') -> sum 0 (Added b) (step aa bb')
  (a::aa', []) -> sum 0 (Removed a) (step aa' bb)
  (a::aa', b::bb') -> if
    | a == b -> sum 1 (NoChange a) (step aa' bb')
    | otherwise ->
      let
          (ls,l) = sum 0 (Added b) (step aa bb')
          (rs,r) = sum 0 (Removed a) (step aa' bb)
      in
        if
          | ls > rs -> (ls,l)
          | otherwise -> (rs,r)

diff : (String -> List String) -> String -> String -> List Change
diff tokenize a b = step (tokenize a) (tokenize b) |> snd

rediff1 : (String -> List String) -> Change -> List Change
rediff1 tokenize change = case change of
  Changed a b -> diff tokenize a b
  _ -> [change]

rediff : (String -> List String) -> List Change -> List Change
rediff tokenize input = input |> List.map (rediff1 tokenize) |> List.concat

{-| Diffs two strings, first comparing line by line, then comparing character by
charater within the changed lines.

    diffChars "abc" "aBcd"
      == [ NoChange "a", Changed "b" "B", NoChange "c", Added "d" ]
-}
diffChars : String -> String -> List Change
diffChars a b = diff tokenizeLines a b
  |> List.foldr mergeChanges []
  |> rediff (String.split "")
  |> List.foldr mergeAll []

tokenizeLines s =
  let
      tokens = String.split "\n" s
      n = List.length tokens
  in if
    | s == "" -> []
    | otherwise -> tokens
      |> List.indexedMap (\i s -> if i < n-1 then s ++ "\n" else s)

{-| Diffs two strings, comparing line by line.

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

    diffLines original changed
      == [ Changed "Brian\nSohie\nOscar\n" "BRIAN\n"
          , NoChange "Stella\n"
          , Added "Frosty\n"
          , NoChange "Takis\n"
          ]
-}
diffLines : String -> String -> List Change
diffLines a b = diff tokenizeLines a b
    |> List.foldr mergeAll []
