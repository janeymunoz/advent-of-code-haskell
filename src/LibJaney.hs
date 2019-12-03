module LibJaney where

import Protolude
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map

-----------------------
-- General helper funcs
-----------------------

textToIntM :: Text -> Maybe Int
textToIntM = readMaybe . Text.unpack

textToInts :: Text -> [Int]
textToInts = catMaybes . map textToIntM . Text.lines

textCsvToInts :: Text -> [Int]
textCsvToInts = catMaybes . map textToIntM . Text.splitOn (Text.singleton ',')

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


