module Main where

import qualified LibJaney as J

import Protolude
import qualified System.FilePath as FP
import qualified System.Environment as SysEnv
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Set as Set

main :: IO ()
main = do
  args <- getArgs
  problemID <- processArgs $ Protolude.map toS args
  input <- readFile $ inputFP problemID
  let answer = doProblem problemID input
  putText answer
  
  where
    inputFP :: ProblemID -> FilePath
    inputFP (ProblemID u (Day d) p) =
      "inputs" FP.</>
        (show u) FP.</>
          "day" <> (show d) <> "-part" <> (if p == One then "1" else "2")

    doProblem :: ProblemID -> Text -> Text
    doProblem pID input =
      case Map.lookup pID problems of
        Nothing -> Text.pack "[ERROR] ProblemID is not in Problems map"
        Just f -> f input

    processArgs :: [Text] -> IO ProblemID
    processArgs ts =
      case ts of
        (user' : day' : part' : _) ->
          case parseArgs (user', day', part') of
            Left err -> panic err
            Right udp@(user, day, part) -> pure $ ProblemID user day part
        _ -> panic $ Text.pack "[ERROR] Expected format: <user> <day> <part>"
    
    parseArgs :: (Text, Text, Text) -> Either Text (User, Day, Part)
    parseArgs (user', day', part') =
      (,,) <$> parseUser user' <*> parseDay day' <*> parsePart part'
      where
        parseUser :: Text -> Either Text User
        parseUser u' =
          case readMaybe $ toS u' of
            Nothing -> Left $ toS "Failed to parse 'User' argument"
            Just u  -> Right u
        parseDay :: Text -> Either Text Day
        parseDay d' =
          case readMaybe $ toS d' of
            Nothing -> Left $ toS "Failed to parse 'Day' argument"
            Just d  -> if d <= 25 && d > 0
                          then Right (Day d)
                          else Left $ toS "'Day' must be between 1 & 25"
        parsePart :: Text -> Either Text Part
        parsePart p' =
          case readMaybe $ toS p' of
            Nothing -> Left $ toS "Failed to parse 'Part' argument"
            Just p  -> case p of
                         1 -> Right One
                         2 -> Right Two
                         _ -> Left $ toS "'Part' can be 1 or 2"

data Part = One | Two
  deriving (Eq, Ord, Show)

newtype Day = Day Int
  deriving (Eq, Ord, Show)

data User = Janey | Thomas
  deriving (Eq, Ord, Read, Show)

data ProblemID = ProblemID User Day Part
  deriving (Eq, Ord, Show)

-- A map of problemIDs to a function to solve that problem
problems :: Map ProblemID (Text -> Text)
problems =
  Map.fromList [ (ProblemID Janey (Day 1) One, show . J.d1p1 . J.textToInts)
               , (ProblemID Janey (Day 1) Two, show . J.d1p2 . J.textToInts)
               ]
