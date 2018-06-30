{-# LANGUAGE FlexibleContexts #-}

module Intelligence where
import SudokuObject
import Data.Vector ((!), find)
import Data.List ((\\), sortBy)
import Data.Maybe (isNothing,isJust)

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
      specified = concat $ map (\x -> case x of Specified c -> [c] ; Unspecified c -> []) others
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
      allUnspecified = filter (\(_, field) -> case field of Unspecified x -> True ; _ -> False) allFields
      (bestPos,_):_ = sortBy (\(_, Unspecified x) (_, Unspecified y) -> compare (length x) (length y)) allUnspecified
    in bestPos

solvable board = isNothing $ find (\x -> case x of Unspecified [] -> True ; _ -> False) (boardBoard board)

solved board = isNothing $ find (\x -> case x of Unspecified _ -> True ; _ -> False) (boardBoard board)

solve board =
  let reduced = reduceAll board
      reducedIsSolved = solved reduced
      alternatives = permute reduced $ bestPermute reduced
      reducedAlternatives = map reduceAll alternatives
      solvableAlternatives = filter solvable reducedAlternatives
      solvedAlternatives = filter (isJust) $ map solve solvableAlternatives
    in if reducedIsSolved
         then Just reduced
         else case solvedAlternatives of (var:_) -> var
                                         [] -> Nothing
