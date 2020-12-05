module Lib
  ( solve,
  )
where

import Data.Char (isHexDigit, isNumber)
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
  deriving (Show)

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

numberBetween :: String -> Int -> Int -> Maybe Int
numberBetween value min max =
  let num = (read value :: Int)
   in if num >= min && num <= max
        then Just num
        else Nothing

validatedBirthYear :: String -> Maybe PassportValue
validatedBirthYear value =
  case numberBetween value 1920 2002 of
    Just year -> Just (BirthYear year)
    _ -> Nothing

validatedIssueYear :: String -> Maybe PassportValue
validatedIssueYear value =
  case numberBetween value 2010 2020 of
    Just year -> Just (IssueYear year)
    _ -> Nothing

validatedExpirationYear :: String -> Maybe PassportValue
validatedExpirationYear value =
  case numberBetween value 2020 2030 of
    Just year -> Just (ExpirationYear year)
    _ -> Nothing

validatedEyeColor :: String -> Maybe PassportValue
validatedEyeColor value =
  if value == "amb"
    || value == "blu"
    || value == "brn"
    || value == "gry"
    || value == "grn"
    || value == "hzl"
    || value == "oth"
    then Just (EyeColor value)
    else Nothing

validatedHeight :: String -> Maybe PassportValue
validatedHeight value =
  let (numbers, unit) = span isNumber value
   in case unit of
        "cm" -> case numberBetween numbers 150 193 of
          Just _ -> Just (Height value)
          _ -> Nothing
        "in" -> case numberBetween numbers 59 76 of
          Just _ -> Just (Height value)
          _ -> Nothing
        _ -> Nothing

validatedHairColor :: String -> Maybe PassportValue
validatedHairColor value =
  if head value == '#'
    then
      let numbers = drop 1 value
       in if length numbers == 6 && all isHexDigit numbers
            then Just (HairColor value)
            else Nothing
    else Nothing

validatedPassportId :: String -> Maybe PassportValue
validatedPassportId value =
  if length value == 9 && all isNumber value
    then Just (PassportID (read value :: Int))
    else Nothing

parsePassportValue :: String -> Maybe PassportValue
parsePassportValue rawValue =
  case elemIndex ':' rawValue of
    Just colonIndex ->
      let (key, valueWithColon) = splitAt colonIndex rawValue
          value = dropWhile (== ':') valueWithColon
       in case key of
            "byr" -> validatedBirthYear value
            "iyr" -> validatedIssueYear value
            "eyr" -> validatedExpirationYear value
            "hgt" -> validatedHeight value
            "hcl" -> validatedHairColor value
            "ecl" -> validatedEyeColor value
            "pid" -> validatedPassportId value
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
