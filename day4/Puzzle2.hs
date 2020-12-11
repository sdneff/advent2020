import Data.Char

import Utils

isYearInRange :: Int -> Int -> String -> Bool
isYearInRange min max s = isLength 4 s && isNumeric s && isWithin min max (read s :: Int)

check :: (String -> Bool) -> Maybe String -> Bool
check p (Just s) = p s
check _ Nothing = False

-- byr (Birth Year) - four digits; at least 1920 and at most 2002.
isValidBirthYear :: String -> Bool
isValidBirthYear = isYearInRange 1920 2002

-- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
isValidIssueYear :: String -> Bool
isValidIssueYear = isYearInRange 2010 2020

-- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
isValidExpireYear :: String -> Bool
isValidExpireYear = isYearInRange 2020 2030

-- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
isValidEyeColor :: String -> Bool
isValidEyeColor s = elem s ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

-- hgt (Height) - a number followed by either cm or in:
--   If cm, the number must be at least 150 and at most 193.
--   If in, the number must be at least 59 and at most 76.
isValidHeight :: String -> Bool
isValidHeight s = case splitAt (length s - 2) s of
                    (n, "cm") -> isLength 3 n && isNumeric n && isWithin 150 193 (read n :: Int)
                    (n, "in") -> isLength 2 n && isNumeric n && isWithin 59 76 (read n :: Int)
                    otherwise -> False

-- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
isValidHairColor :: String -> Bool
isValidHairColor ('#' : s) = isLength 6 s && all (\c -> elem c "0123456789abcdef") s
isValidHairColor _ = False

-- pid (Passport ID) - a nine-digit number, including leading zeroes.
isValidPassportId :: String -> Bool
isValidPassportId s = isLength 9 s && isNumeric s

isValidPassport :: Passport -> Bool
isValidPassport passport = (check isValidBirthYear (birthYr passport))
                        && (check isValidIssueYear (issueYr passport))
                        && (check isValidExpireYear (expireYr passport))
                        && (check isValidHeight (height passport))
                        && (check isValidHairColor (hairColor passport))
                        && (check isValidEyeColor (eyeColor passport))
                        && (check isValidPassportId (pId passport))

main :: IO ()
main = do
  passportStrings <- readPassportStrings "passports.txt"
  let passports = fmap parsePassport passportStrings
  let validPassports = filter isValidPassport passports
  putStrLn ("passport count: " ++ show (length passports))
  putStrLn ("valid passport count: " ++ show (length validPassports))
