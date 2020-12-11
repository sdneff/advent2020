module Utils where

import Data.Char
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

data Passport = Passport
    { birthYr   :: Maybe String --byr (Birth Year)
    , issueYr   :: Maybe String --iyr (Issue Year)
    , expireYr  :: Maybe String --eyr (Expiration Year)
    , height    :: Maybe String --hgt (Height)
    , hairColor :: Maybe String --hcl (Hair Color)
    , eyeColor  :: Maybe String --ecl (Eye Color)
    , pId       :: Maybe String --pid (Passport ID)
    , countryId :: Maybe String --cid (Country ID)
    } deriving (Show)

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen _ [] = []
splitWhen p xs = case dropWhile p xs of
                      [] -> []
                      xs' -> x : splitWhen p xs''
                        where (x, xs'') = break p xs'

readPassportStrings :: FilePath -> IO [T.Text]
readPassportStrings path = do
    rawLines <- fmap T.lines (TIO.readFile path)
    return (fmap (T.intercalate (T.pack " ")) (splitWhen (\t -> (T.length t) == 0) rawLines))

lookupField :: [(String, String)] -> String -> Maybe String
lookupField [] _ = Nothing
lookupField fs k = case filter (\(k', _) -> k' == k) fs of
                    [] -> Nothing
                    ((k'', v) : _) -> Just v

parsePassport :: T.Text -> Passport
parsePassport s = Passport
                        { birthYr   = (lookupField fs "byr")
                        , issueYr   = (lookupField fs "iyr")
                        , expireYr  = (lookupField fs "eyr")
                        , height    = (lookupField fs "hgt")
                        , hairColor = (lookupField fs "hcl")
                        , eyeColor  = (lookupField fs "ecl")
                        , pId       = (lookupField fs "pid")
                        , countryId = (lookupField fs "cid")
                        }
  where
    break' :: String -> (String, String)
    break' s = let (k, v) = (break (== ':') s) in (k, drop 1 v)

    fs = map break' (words (T.unpack s))

isNumeric :: String -> Bool
isNumeric s = all isDigit s

isWithin :: Int -> Int -> Int -> Bool
isWithin min max val = val >= min && val <= max

isLength :: Int -> String -> Bool
isLength l s = length s == l
