{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Intelligence where
import SudokuObject
import Data.Function (on)
import Data.Vector ((!), toList)
import Data.List ((\\), minimumBy, find, delete)
import Data.Maybe (isNothing, isJust, mapMaybe)
import Language.Haskell.TH.Syntax (lift)

import qualified Data.List as L

import Debug.Trace

relatedCached = $(lift $ map (related . posToCoord) [0..81])

allCoords = [(x,y) | x <- [0..8], y <- [0..8]]

reduce board (pos, me) =
  let related' = relatedCached !! coordToPos pos
      specifiedRelated = mapMaybe (\pos -> case boardBoard board ! coordToPos pos
                                             of Specified c -> Just c
                                                Unspecified _ -> Nothing) related'
      possible = removeFromUnspecified me specifiedRelated
      newMe = fieldFromUnspecified possible
    in updatePos board pos newMe

reduceAll board =
  let unspecifiedPositions = mapMaybe (\x -> case boardBoard board ! coordToPos x
                                               of Unspecified l -> Just (x, l)
                                                  _ -> Nothing)
                                      allCoords
    in foldr (flip reduce) board unspecifiedPositions

permute board pos =
  let Unspecified me = boardBoard board ! coordToPos pos
      alternatives = map Specified $ fromUnspecified me
      alternativeBoards = map (updatePos board pos) alternatives
    in alternativeBoards

bestPermute board =
  let allUnspecified = flip mapMaybe allCoords
                                     $ \pos -> case boardBoard board ! coordToPos pos
                                                 of Unspecified l -> Just (pos, unspecifiedLength l)
                                                    _ -> Nothing

      (bestPos,_) = minimumBy (compare `on` snd)
                              allUnspecified
    in bestPos

data BoardStatus = Solvable | Solved | Unsolvable

boardStatus board =
  let filteredBoard = filter (\case Unspecified _ -> True ; _ -> False) $ toList $ boardBoard board
      solvable = isNothing $ find emptyUnspecified filteredBoard
      solved = L.null filteredBoard
    in if solved then Solved else if solvable then Solvable else Unsolvable

solve board =
  let reduced = reduceAll board
      status = boardStatus reduced
      alternatives = permute reduced $ bestPermute reduced
      solvedAlternatives = filter isJust $ map solve alternatives
    in case status
         of Solved -> Just reduced
            Unsolvable -> Nothing
            _ -> case solvedAlternatives of (var:_) -> var
                                            [] -> Nothing
