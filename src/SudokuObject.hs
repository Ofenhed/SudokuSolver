module SudokuObject where

import Data.Vector (Vector(..), (!), fromList, modify, toList)
import Data.Vector.Mutable (write)
import Data.List (find, concat, filter, sortBy)
import Data.Maybe (isJust, isNothing)
import Data.Int (Int8)

boardMax = 9

data Field = Specified Int8
           | Unspecified [Int8] deriving (Show, Eq)

data Board = Board {boardBoard :: (Vector (Field))}

related (x,y) = let boxX = quot x 3
                    boxY = quot y 3
                    vertical = [(x,y') | y' <- [0..8], y' /= y]
                    horizontal = [(x',y) | x' <- [0..8], x' /= x]
                    othersInBox = [(x',y') | x' <- map ((+)(boxX*3)) [0..2]
                                           , y' <- map ((+)(boxY*3)) [0..2]
                                           , x /= x'
                                           , y /= y']
                  in vertical ++ horizontal ++ othersInBox

coordToPos (x, y) = x + y * boardMax
posToCoord pos = (mod pos boardMax, quot pos boardMax)

instance Show Board where
  show b = print' "" [(x, y) |
                               y <- [0..boardMax-1],
                               x <- [0..boardMax-1]
                     ]
    where
    printList (Unspecified l) = concat $ map show l
    unspecifiedList = filter (\x -> case x of Unspecified _ -> True ; _ -> False) $ toList $ boardBoard b
    longestUnspecifiedList = sortBy (\(Unspecified x) (Unspecified y) -> compare (length y) (length x)) unspecifiedList
    (Unspecified longestUnspecified):_ = longestUnspecifiedList
    maxLen = if length unspecifiedList == 0
                then 1
                else length $ longestUnspecified
    print' prev [] = prev
    print' prev ((pos@(x,y)):xs) = do
      let field = boardBoard b ! coordToPos pos
          delimiter = if x == boardMax - 1
                         then if mod y 3 == 2 then "\n\n" else "\n"
                         else if mod x 3 == 2 then "   " else " "
          d = case field of
                   Specified t -> show t
                   list@(Unspecified _) -> printList list
        in print' (prev ++ d ++ replicate (maxLen - length d) ' ' ++ delimiter) xs

readBoard d =
  let unspec = Unspecified [1..fromIntegral boardMax]
      b = map (\x -> if x == '.'
                       then unspec
                       else if x > '0' && x <= '9'
                              then Specified (read [x] :: Int8)
                              else error "Unreadable config"
                  ) $ filter ((/=)'\n') d
    in Board {boardBoard = fromList b}

updatePos board pos newVal = let board' = boardBoard board
                              in Board {boardBoard = modify (\v -> write v (coordToPos pos) newVal) board'}
