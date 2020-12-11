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

main :: IO ()
main = do
  passportStrings <- readPassportStrings "passports.txt"
  let passports = fmap parsePassport passportStrings
  let validPassports = filter isMostlyValid passports
  putStrLn ("passport count: " ++ show (length passports))
  putStrLn ("valid passport count: " ++ show (length validPassports))
