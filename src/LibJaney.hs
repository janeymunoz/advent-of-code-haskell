module LibJaney where

import Protolude
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-----------------------
-- General helper funcs
-----------------------

textToIntM :: Text -> Maybe Int
textToIntM = readMaybe . Text.unpack

textToInts :: Text -> [Int]
textToInts = catMaybes . map textToIntM . Text.lines

textCsvToInts :: Text -> [Int]
textCsvToInts = catMaybes . map textToIntM . Text.splitOn (Text.singleton ',')

-- int-int
textToIntTuple :: Text -> (Int, Int)
textToIntTuple t = (i, j)
  where
    (i : j : []) = catMaybes . map textToIntM $ Text.splitOn (Text.singleton '-') t

-- Returns a list of ints between two ints, inclusive, regarless of sign of ints.
getRange :: (Int, Int) -> [Int]
getRange (i, j) = if j < i then [j..i] else [i..j]

--------
-- Day 1
--------

d1p1 :: [Int] -> Int
d1p1 is = foldr accum 0 is
  where accum :: Int -> Int -> Int
        accum mass fuel = (+) fuel $ fuelReq mass

d1p2 :: [Int] -> Int
d1p2 = sum . map fuelReqCum
  where fuelReqCum :: Int -> Int
        fuelReqCum m = sum $ takeWhile (> 0) ms
          where (_:ms) = iterate fuelReq m

fuelReq :: Int -> Int
fuelReq = flip (-) 2 . flip div 3

--------
-- Day 2
--------

data Opcode = One | Two | NineNine | Invalid

intToOpcode :: Int -> Opcode
intToOpcode i =
  case i of
    1  -> One
    2  -> Two
    99 -> NineNine
    _  -> Invalid

d2p1 :: [Int] -> Maybe Int
d2p1 is = d2General is (12, 2)

d2p2 :: [Int] -> (Int, Int)
d2p2 is = foldr accum (0, 0) outputs
  where
    accum :: ((Int, Int), Maybe Int) -> (Int, Int) -> (Int, Int)
    accum ((s1Poss, s2Poss), vM) (s1', s2') =
      case vM of
        Just 19690720 -> (s1Poss, s2Poss)
        _             -> (s1', s2')
    outputs :: [((Int, Int), Maybe Int)]
    outputs = zip seeds $ map (d2General is) seeds
    seeds = [ (x, y) | x <- [0..99], y <- [0..99] ]

d2General :: [Int] -> (Int, Int) -> Maybe Int
d2General is (address1, address2) =
  case doOps (startIntMap is (address1, address2)) [0,1..] of
    Just m  -> Map.lookup 0 m
    Nothing -> Nothing

doOps :: Map Int Int -> [Int] -> Maybe (Map Int Int)
doOps m ps =
  case processIntcodes m psNow of
    Just (One, v1, v2, p3)   ->
      doOps (Map.insert p3 (v1 + v2) m) psRest
    Just (Two, v1, v2, p3)   ->
      doOps (Map.insert p3 (v1 * v2) m) psRest
    Just (NineNine, _, _, _) -> Just m
    _                        -> Nothing
  where (psNow,psRest) = splitAt 4 ps
    
processIntcodes :: Map Int Int -> [Int] -> Maybe (Opcode, Int, Int, Int)
processIntcodes m ps =
  case ps of
    p0 : p1 : p2 : p3 : [] -> do
      case Map.lookup p0 m of
        Just 99 -> Just (NineNine, 0, 0, 0)
        Just oc -> do
          v1  <- flip Map.lookup m =<< Map.lookup p1 m
          v2  <- flip Map.lookup m =<< Map.lookup p2 m
          v3  <- Map.lookup p3 m
          pure (intToOpcode oc, v1, v2, v3) 
        Nothing -> Nothing
    _ -> Nothing
    
startIntMap :: [Int] -> (Int, Int) -> Map Int Int
startIntMap is (j, k) = edit . Map.fromList $ zip [0 .. ] is
  where
    edit :: Map Int Int -> Map Int Int
    edit m = Map.insert 2 k $ Map.insert 1 j m

--------
-- Day 3
--------

newtype X = X Int
  deriving (Eq, Ord, Show)
newtype Y = Y Int
  deriving (Eq, Ord, Show)
data Coord = Coord X Y
  deriving (Eq, Ord, Show)
data Direction = U | D | L | R
  deriving (Eq, Read, Show)
data Move = Move Direction Int
  deriving (Eq, Read, Show)


parseInputD3 :: Text -> ([Move], [Move])
parseInputD3 t = ( catMaybes $ map (toMove . toS) input1
                 , catMaybes $ map (toMove . toS) input2 )
    where 
      [input1,input2] =
        map (Text.splitOn (Text.singleton ',')) $ Text.lines t

      toMove :: [Char] -> Maybe Move
      toMove (d:i) = do
        dir <- readMaybe [d]
        dis <- readMaybe i
        pure $ Move dir dis

d3Both :: ([Move], [Move]) -> (Maybe Int, Maybe Int)
d3Both (moves1, moves2) =
  ( head . sort . map (manhattanDist coordOrigin) $ Set.toList coordsIntersect 
  , shortestPath)
  where
    shortestPath :: Maybe Int
    shortestPath = head . sort $ foldr accum mempty coordsIntersect
      where accum :: Coord -> [Int] -> [Int]
            accum c is =
              case Map.toList $ Map.filter (c ==) coordsMap1 of
                [(k1,_)] ->
                  case Map.toList $ Map.filter (c ==) coordsMap2 of
                    [(k2,_)] -> (k1 + k2) : is
                    _        -> is
                _ -> is

    move :: Coord -> Move -> Coord
    move (Coord (X x) (Y y)) (Move dir mag) =
      case dir of
        U -> Coord (X x) (Y $ y + mag)
        D -> Coord (X x) (Y $ y - mag)
        L -> Coord (X $ x - mag) (Y y)
        R -> Coord (X $ x + mag) (Y y)

    coordsIntersect =
      Set.delete coordOrigin
        $ Set.intersection (Set.fromList coordsList2) (Set.fromList coordsList1)
    coordsList1 = snd . unzip $ Map.toList coordsMap1
    coordsList2 = snd . unzip $ Map.toList coordsMap2
    coordsMap1 = getPath moves1
    coordsMap2 = getPath moves2
    
    -- All the coordinates through which the path passes given a list of moves.
    getPath :: [Move] -> Map Int Coord
    getPath moves = Map.fromList $ zip [0,1..] . reverse $ foldl accum [coordOrigin] moves
        where
          accum :: [Coord] -> Move -> [Coord]
          accum (cCur@(Coord (X xCur) (Y yCur)) : cs) moveCur =
            -- Returns a list of coordinates passed through, in reverse order (most
            -- recent coordinate first, oldest coordinate last).
            coordsNewOrd <> cs
            where
              cNew@(Coord (X xNew) (Y yNew)) = move cCur moveCur
              coordsNewOrd =
                case xNew >= xCur && yNew >= yCur of
                  True  -> reverse coordsNew
                  False -> coordsNew
                where
                  coordsNew = [ Coord (X x) (Y y) |
                                x <- (getRange (xCur, xNew)),
                                y <- (getRange (yCur,yNew)) ]
    coordOrigin = Coord (X 0) (Y 0)

    manhattanDist :: Coord -> Coord -> Int
    manhattanDist (Coord (X x1) (Y y1)) (Coord (X x2) (Y y2)) =
      (abs $ (-) x1 x2) + (abs $ (-) y1 y2)

--------
-- Day 4
--------

-- Given an inclusive range of integers, return all the values within that range
-- that satisfy the following
--   1: six digits
--   2: at least two adjacent digits are the same (e.g., 22 in 122345)
--   3: L to R, each new digit is greater than or equal to the previous
d4p1 :: (Int, Int) -> Int
d4p1 (startInt, endInt) = Set.size $ foldr accum Set.empty [ startInt .. endInt ]
  where accum :: Int -> Set Int -> Set Int
        accum i s =
          case iChars of
            i0 : i1 : i2 : i3 : i4 : i5 : [] -> -- checking for length
              case anyAdjEqual of
                True ->
                  case all (True ==) . fst $ foldl isInc ([True], 0) iInts of
                    True -> Set.insert i s
                    False -> s
                  where
                    isInc :: ([Bool],Int) -> Int -> ([Bool], Int)
                    isInc (bs, iPrev) iNext =
                      if iNext >= iPrev
                         then (True:bs, iNext)
                         else (False:bs, iNext)
                False -> s
              where
                anyAdjEqual :: Bool
                anyAdjEqual
                  | i0 == i1 || i1 == i2 || i2 == i3 || i3 == i4 || i4 == i5 = True
                  | otherwise = False
          where iChars = show i :: [Char]
                iInts = catMaybes $ map (textToIntM . Text.singleton) iChars

