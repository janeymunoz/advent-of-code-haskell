module LibJaney where

import Protolude
import qualified Data.Text as Text

-----------------------
-- General helper funcs
-----------------------

textToIntM :: Text -> Maybe Int
textToIntM = readMaybe . Text.unpack

textToInts :: Text -> [Int]
textToInts = catMaybes . map textToIntM . Text.lines

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

