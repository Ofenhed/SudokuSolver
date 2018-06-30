{-# LANGUAGE FlexibleContexts #-}

module Intelligence where
import SudokuObject
import Data.Vector ((!), (!?), find)
import Data.List ((\\), sortBy)
import Data.Maybe (isNothing,isJust)

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

reduce board pos =
  let Unspecified me = boardBoard board ! coordToPos pos
      related' = related pos
      others = map (((!) $ boardBoard board) . coordToPos) related'
      specified = concat $ map (\x -> case x of Specified c -> [c] ; Unspecified _ -> []) others
      possible = me \\ specified
      newMe = case possible
                of [val] -> Specified val
                   vals  -> Unspecified vals
    in updatePos board pos newMe

reduceAll board =
  let poi = filter (\x -> case boardBoard board ! coordToPos x of Unspecified _ -> True ; _ -> False) allCoords
    in foldr (\pos b -> reduce b pos) board poi

permute board pos =
  let Unspecified me = boardBoard board ! coordToPos pos
      alternatives = map Specified me
      alternativeBoards = map (updatePos board pos) alternatives
    in alternativeBoards

bestPermute board =
  let allFields = map (\pos -> (pos, boardBoard board ! coordToPos pos)) allCoords
      allUnspecified = filter (\(_, field) -> case field of Unspecified _ -> True ; _ -> False) allFields
      (bestPos,_):_ = sortBy (\(_, Unspecified x) (_, Unspecified y) -> compare (length x) (length y)) allUnspecified
    in bestPos

data BoardStatus = Solvable | Solved | Unsolvable

boardStatus board = checkStatus 0 Solved
  where
  b = boardBoard board
  checkStatus i s = case b !? i
                      of Nothing -> s
                         Just (Unspecified []) -> Unsolvable
                         Just (Unspecified _) -> checkStatus (i+1) Solvable
                         _ -> checkStatus (i+1) s

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
