import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import Utils

isTotallyValid :: Passport -> Bool
isTotallyValid Passport{ birthYr   = Just _
                       , issueYr   = Just _
                       , expireYr  = Just _
                       , height    = Just _
                       , hairColor = Just _
                       , eyeColor  = Just _
                       , pId       = Just _
                       , countryId = Just _
                       } = True
isTotallyValid _ = False

isMostlyValid :: Passport -> Bool
isMostlyValid p = isTotallyValid (p { countryId=Just "" })

main = do
  passportStrings <- readPassportStrings "passports.txt"
  let passports = fmap parsePassport passportStrings
  let validPassports = filter isMostlyValid passports
  putStrLn ("passport count: " ++ show (length passports))
  putStrLn ("totally valid passport count: " ++ show (length (filter isTotallyValid passports)))
  putStrLn ("mostly valid passport count: " ++ show (length (filter isMostlyValid passports)))
