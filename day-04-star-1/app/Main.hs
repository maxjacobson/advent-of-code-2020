module Main where

import Data.List (elemIndex)

data PassportValue
  = BirthYear Int
  | IssueYear Int
  | ExpirationYear Int
  | Height String
  | HairColor String
  | EyeColor String
  | PassportID Int
  | CountryID Int

data Passport = Passport
  { birthYear :: Maybe Int,
    issueYear :: Maybe Int,
    expirationYear :: Maybe Int,
    height :: Maybe String,
    hairColor :: Maybe String,
    eyeColor :: Maybe String,
    passportID :: Maybe Int,
    countryID :: Maybe Int
  }

parsePassportValue :: String -> Maybe PassportValue
parsePassportValue rawValue =
  case elemIndex ':' rawValue of
    Just colonIndex ->
      let (key, value) = splitAt colonIndex rawValue
       in case key of
            "byr" -> Just (BirthYear (read value :: Int))
            "iyr" -> Just (IssueYear (read value :: Int))
            "eyr" -> Just (ExpirationYear (read value :: Int))
            "hgt" -> Just (Height value)
            "hcl" -> Just (HairColor value)
            "ecl" -> Just (EyeColor value)
            "pid" -> Just (PassportID (read value :: Int))
            "cid" -> Just (CountryID (read value :: Int))
            _ -> Nothing
    Nothing -> Nothing

buildPassport :: Passport -> Maybe PassportValue -> Passport
buildPassport passport maybeValue =
  case maybeValue of
    Just value ->
      case value of
        BirthYear year -> passport {birthYear = Just year}
        IssueYear year -> passport {issueYear = Just year}
        Height height -> passport {height = Just height}
        HairColor color -> passport {hairColor = Just color}
        EyeColor color -> passport {eyeColor = Just color}
        PassportID id -> passport {passportID = Just id}
        CountryID id -> passport {countryID = Just id}
        ExpirationYear year -> passport {expirationYear = Just year}
    Nothing -> passport

passportFrom :: String -> Maybe Passport
passportFrom rawPassport =
  let pairs = words rawPassport
      values = map parsePassportValue pairs
      passport =
        foldl
          buildPassport
          Passport
            { birthYear = Nothing,
              expirationYear = Nothing,
              height = Nothing,
              hairColor = Nothing,
              eyeColor = Nothing,
              passportID = Nothing,
              countryID = Nothing,
              issueYear = Nothing
            }
          values
   in case birthYear passport of
        Just _ ->
          case expirationYear passport of
            Just _ ->
              case height passport of
                Just _ ->
                  case hairColor passport of
                    Just _ ->
                      case eyeColor passport of
                        Just _ ->
                          case passportID passport of
                            Just _ ->
                              case issueYear passport of
                                Just _ ->
                                  Just passport
                                Nothing -> Nothing
                            Nothing -> Nothing
                        Nothing -> Nothing
                    Nothing -> Nothing
                Nothing -> Nothing
            Nothing -> Nothing
        Nothing -> Nothing

passportsFrom :: [String] -> [Passport] -> [Passport]
passportsFrom inputLines passportsList =
  let trimmedInputLines = dropWhile (== "") inputLines
      (passportParagraph, remainingLines) = span (/= "") trimmedInputLines
   in case passportParagraph of
        [] -> passportsList
        _ -> case passportFrom (unlines passportParagraph) of
          Just passport -> passportsFrom remainingLines (passportsList ++ [passport])
          Nothing -> passportsFrom remainingLines passportsList

solve :: String -> Int
solve input =
  length (passportsFrom (lines input) [])

main :: IO ()
main = do
  input <- readFile "input.txt"

  putStr (show (solve input))
