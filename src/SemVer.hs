module SemVer where

import Control.Applicative
import Text.Trifecta

data NumberOrString =
         NOSS String
       | NOSI Integer
      deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Show, Eq)

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  _     <- char '.'
  minor <- integer
  _     <- char '.'
  patch <- integer

  release  <- option [] (char '-' *> parseRelease)
  metadata <- option [] (char '+' *> parseMetadata)
  return (SemVer major minor patch release metadata)

parseRelease :: Parser Release
parseRelease = sepBy parseNumOrString (char '.')

parseMetadata :: Parser Metadata
parseMetadata = sepBy parseNumOrString (char '.')

parseNumOrString :: Parser NumberOrString
parseNumOrString =
      (NOSI <$> integer)
  <|> (NOSS <$> (many alphaNum))
