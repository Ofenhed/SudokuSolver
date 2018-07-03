{-# LANGUAGE FlexibleContexts #-}

module Intelligence where
import SudokuObject
import Data.Vector ((!), toList)
import Data.List ((\\), minimumBy, find, delete)
import Data.Maybe (isNothing, isJust, mapMaybe)

import qualified Data.List as L

import Debug.Trace

related (x,y) = let boxX = quot x 3
                    boxY = quot y 3
                    vertical = [(x,y') | y' <- [0..8], y' /= y]
                    horizontal = [(x',y) | x' <- [0..8], x' /= x]
                    othersInBox = [(x',y') | x' <- map ((+)$boxX*3) [0..2]
                                           , y' <- map ((+)$boxY*3) [0..2]
                                           , x /= x'
                                           , y /= y']
                  in vertical ++ horizontal ++ othersInBox

allCoords = [(x,y) | x <- [0..8], y <- [0..8]]

reduce board (pos, me) =
  let related' = related pos
      specifiedRelated = mapMaybe (\pos -> case boardBoard board ! coordToPos pos
                                             of Specified c -> Just c
                                                Unspecified _ -> Nothing) related'
      possible = me \\ specifiedRelated
      newMe = case possible
                of [val] -> Specified val
                   vals  -> Unspecified vals
    in updatePos board pos newMe

reduceAll board =
  let unspecifiedPositions = mapMaybe (\x -> case boardBoard board ! coordToPos x
                                               of Unspecified l -> Just (x, l)
                                                  _ -> Nothing)
                                      allCoords
    in foldr (\pos b -> reduce b pos) board unspecifiedPositions

permute board pos =
  let Unspecified me = boardBoard board ! coordToPos pos
      alternatives = map Specified me
      alternativeBoards = map (updatePos board pos) alternatives
    in alternativeBoards

bestPermute board =
  let allUnspecified = mapMaybe (\pos -> case (boardBoard board ! coordToPos pos)
                                           of v@(Unspecified _) -> Just (pos, v)
                                              _ -> Nothing)
                                allCoords
      (bestPos,_) = minimumBy (\(_, Unspecified x) (_, Unspecified y) -> compare (length x) (length y)) allUnspecified
    in bestPos

data BoardStatus = Solvable | Solved | Unsolvable

boardStatus board =
  let filteredBoard = filter (\x -> case x of Unspecified _ -> True ; _ -> False) $ toList $ boardBoard board
      solvable = isNothing $ find (\x -> case x of Unspecified [] -> True ; _ -> False) filteredBoard
      solved = L.null filteredBoard
    in if solved then Solved else if solvable then Solvable else Unsolvable

solve board =
  let reduced = reduceAll board
      status = boardStatus reduced
      alternatives = permute reduced $ bestPermute reduced
      solvedAlternatives = filter (isJust) $ map solve alternatives
    in case status
         of Solved -> Just reduced
            Unsolvable -> Nothing
            _ -> case solvedAlternatives of (var:_) -> var
                                            [] -> Nothing
